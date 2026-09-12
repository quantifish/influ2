gamma_implied_fixture <- function() {
  set.seed(821)
  d <- expand.grid(year = factor(2011:2015), area = factor(c("A", "B")), record = 1:30)
  d$x <- runif(nrow(d), -1, 1)
  d$month <- sample(1:12, nrow(d), replace = TRUE)
  d$vessel <- factor(sample(1:12, nrow(d), replace = TRUE))
  d$log_effort <- runif(nrow(d), -.5, .5)
  re <- rnorm(12, sd = .45)
  eta <- 1 + .12 * as.integer(d$year) + .2 * as.integer(d$area) +
    sin(2 * pi * d$month / 12) + re[d$vessel] + .4 * d$x + 1.3 * d$log_effort
  d$response <- rgamma(nrow(d), shape = 1.4, scale = exp(eta) / 1.4)
  rownames(d) <- paste0("gamma", seq_len(nrow(d)))
  d
}

check_gamma_implied <- function(fit, d, phi) {
  x <- implied_effects(fit, data = d, groups = "area")
  eta <- if (inherits(fit, "glmmTMB")) predict(fit, type = "link", re.form = NULL) else fit$linear.predictors
  phi <- rep_len(phi, nrow(d))
  expect_equal(x$metadata$family, "Gamma")
  expect_match(x$metadata$dispersion, "shape = 1/phi", fixed = TRUE)
  for (j in seq_len(nrow(x$table))) {
    cell <- x$table[j, ]
    i <- d$year == cell$level & d$area == cell$group
    ll <- function(delta) sum(dgamma(d$response[i], shape = 1 / phi[i],
      scale = exp(eta[i] + delta) * phi[i], log = TRUE))
    expect_equal(cell$adjustment, optimise(ll, c(-10, 10), maximum = TRUE, tol = 1e-10)$maximum,
      tolerance = 1e-6)
    expect_equal(cell$adjustment, log(sum(d$response[i] / exp(eta[i]) / phi[i]) / sum(1 / phi[i])),
      tolerance = 1e-12)
    expect_equal(cell$std_error, 1 / sqrt(sum(1 / phi[i])), tolerance = 1e-12)
    expect_equal(.implied_loglik(cell$adjustment, d$response[i], eta[i], phi[i], "Gamma"),
      ll(cell$adjustment), tolerance = 1e-10)
    expect_equal(2 * (ll(cell$adjustment) - ll(cell$lower - cell$baseline)), qchisq(.95, 1), tolerance = 1e-7)
    expect_equal(2 * (ll(cell$adjustment) - ll(cell$upper - cell$baseline)), qchisq(.95, 1), tolerance = 1e-7)
    expect_gt(cell$upper - cell$estimate, cell$estimate - cell$lower)
  }
  expect_s3_class(plot_implied_residuals(x), "ggplot")
  expect_s3_class(ggplot2::ggplot_build(plot(x)), "ggplot_built")
  expect_identical(unserialize(serialize(x, NULL)), x)
  x
}

test_that("Gamma GLMs retain their native scale and positive continuous response", {
  d <- gamma_implied_fixture()
  m <- glm(response ~ year + area + x + log_effort + offset(log_effort),
    family = Gamma(link = "log"), data = d)
  fields <- c("coefficients", "linear.predictors", "fitted.values", "y", "weights", "prior.weights", "offset")
  before <- m[fields]
  seed <- .Random.seed
  z <- check_gamma_implied(m, d, summary(m)$dispersion)
  expect_identical(.Random.seed, seed)
  expect_identical(m[fields], before)
  baseline <- rowSums(predict(m, type = "terms")[, c("year", "area")])
  for (j in seq_len(nrow(z$table))) {
    i <- d$year == z$table$level[j] & d$area == z$table$group[j]
    expect_equal(z$table$baseline[j], mean(baseline[i]), tolerance = 1e-12)
  }
  expect_error(implied_effects(m, method = "traditional"), "Gaussian")
  expect_error(implied_effects(m, interval = "descriptive"), "log-response variance")
  expect_true(all(is.na(implied_effects(m, interval = "none")$table$lower)))
  broken <- m
  broken$model$response[1] <- 0
  expect_error(implied_effects(broken), "strictly positive")
  broken$model$response[1] <- -1
  expect_error(implied_effects(broken), "strictly positive")
  weighted <- glm(response ~ year + area + x, family = Gamma(link = "log"), data = d,
    weights = rep(2, nrow(d)))
  expect_error(implied_effects(weighted), "Non-unit")
})

test_that("Gamma GAMs retain smooths, vessel effects, offsets, and effort slopes", {
  skip_if_not_installed("mgcv")
  d <- gamma_implied_fixture()
  m <- mgcv::gam(response ~ year + area + s(month, k = 8, bs = "cc") +
    s(vessel, bs = "re") + x + log_effort + offset(log_effort),
    data = d, family = Gamma(link = "log"), method = "REML")
  fields <- c("coefficients", "linear.predictors", "fitted.values", "y", "weights", "prior.weights", "offset", "sig2", "scale")
  before <- m[fields]
  z <- check_gamma_implied(m, d, m$sig2)
  expect_equal(unname(m$linear.predictors), unname(drop(model.matrix(m) %*% coef(m)) + m$offset))
  expect_equal(m$sig2, summary(m)$dispersion)
  expect_identical(m[fields], before)
  expect_equal(z$table, implied_effects(m, data = d[nrow(d):1, ], groups = "area")$table)
  for (value in c(NA_real_, 0, -1, Inf)) {
    bad <- m
    bad$sig2 <- value
    expect_error(implied_effects(bad), "positive dispersion")
  }
  sparse <- d[!(d$year == "2013" | (d$year == "2014" & d$area == "A" & d$record > 3)), ]
  sparse$year <- droplevels(sparse$year)
  s <- mgcv::gam(response ~ year + area + s(x, k = 4), data = sparse,
    family = Gamma(link = "log"), method = "REML")
  p <- plot(implied_effects(s))
  shown <- p$layers[[1]]$data
  expect_false(subset(shown, group == "B" & level == "2012")$segment ==
    subset(shown, group == "B" & level == "2014")$segment)
  expect_equal(subset(p$data, group == "A" & level == "2014")$status, "sparse")
})

test_that("Gamma glmmTMB converts native sigma and retains varying dispersion", {
  skip_if_not_installed("glmmTMB")
  d <- gamma_implied_fixture()
  m <- glmmTMB::glmmTMB(response ~ year + area + x + (1 | vessel) + offset(log_effort),
    family = Gamma(link = "log"), data = d, dispformula = ~ x)
  expect_true(m$sdr$pdHess)
  before <- m$obj$env$last.par.best
  phi <- predict(m, type = "disp")^2
  expect_equal(phi, unname(exp(-drop(model.matrix(~ x, d) %*% glmmTMB::fixef(m)$disp))), tolerance = 1e-12)
  check_gamma_implied(m, d, phi)
  expect_identical(m$obj$env$last.par.best, before)
  m0 <- glmmTMB::glmmTMB(response ~ year + area + x,
    family = Gamma(link = "log"), data = d)
  expect_equal(unique(predict(m0, type = "disp")^2), sigma(m0)^2)
})

test_that("Gamma shifts and intervals are stable, unit invariant, and effect scaled", {
  y <- c(.01, 1.25, 37, 106)
  eta <- log(c(.008, 1.7, 33, 120))
  phi <- c(.5, 1, 2, .7)
  a <- .implied_shift(y, eta, phi, "Gamma")
  ci <- .implied_profile(a$shift, y, eta, phi, "Gamma", .95)
  expect_equal(.implied_shift(y * 1000, eta + log(1000), phi, "Gamma"), a, tolerance = 1e-12)
  expect_equal(.implied_shift(y * 1.5, eta, phi, "Gamma")$shift, a$shift + log(1.5), tolerance = 1e-12)
  extreme <- .implied_shift(y, eta - 1000, phi, "Gamma")
  expect_equal(extreme$shift, a$shift + 1000, tolerance = 1e-12)
  expect_equal(.implied_profile(extreme$shift, y, eta - 1000, phi, "Gamma", .95) - 1000,
    ci, tolerance = 1e-10)
  expect_equal(.implied_loglik(extreme$shift, y, eta - 1000, phi, "Gamma"),
    .implied_loglik(a$shift, y, eta, phi, "Gamma"), tolerance = 1e-10)
  expect_equal(.implied_shift(y, eta, 1, "Gamma")$std_error, .5)
  expect_lt(diff(.implied_profile(a$shift, y, eta, phi, "Gamma", .8)), diff(ci))
  expect_error(.implied_shift(y, eta, phi, "other"), "Unsupported")
  expect_error(.implied_loglik(0, y, eta, phi, "other"), "Unsupported")
  expect_error(.implied_profile(0, y, eta, phi, "other", .95), "Unsupported")
})
