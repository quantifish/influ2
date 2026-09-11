calibration_fixture <- function(n = 240L) {
  set.seed(709)
  d <- data.frame(year = factor(rep(2000:2003, length.out = n)), x = rnorm(n),
    target = factor(rep(c("A", "B"), each = n / 2)))
  d$present <- rbinom(n, 1, plogis(-0.3 + 1.4 * d$x))
  d
}

test_that("Bernoulli routing uses family and known trials, not observed support", {
  d <- calibration_fixture()
  fit <- glm(present ~ year + x, binomial(), data = d)
  result <- influ_residuals(fit, nsim = 40)
  expect_identical(result$metadata$response_kind, "bernoulli")
  expect_identical(.resid_response_panel(result, "auto"), "calibration")
  expect_s3_class(plot(result), "patchwork")
  expect_s3_class(ggplot2::autoplot(result), "patchwork")
  expect_identical(plot(result, type = "calibration")$data, result$calibration$bins)
  expect_identical(plot(result, type = "distribution")$data, result$ecdf)
  expect_s3_class(plot(result, response_diagnostic = "distribution"), "patchwork")
  expect_identical(plot(result, type = "calibration", response_scale = "log1p")$scales$get_scales("x")$limits, c(0, 1))
  for (value in 0:1) {
    d$present <- value
    m <- glm(present ~ 1, binomial(), data = d, control = glm.control(maxit = 100))
    a <- influ_residuals(m, data = d, year = "year", nsim = 20)
    expect_identical(a$metadata$response_kind, "bernoulli")
    expect_equal(nrow(a$calibration$bins), 1)
    expect_true(a$calibration$settings$constant)
    expect_match(plot(a, type = "calibration")$labels$subtitle, "overall frequency only")
  }
  # A Poisson sample of only zero/one values is still a count model.
  d$present <- rep(0:1, nrow(d) / 2)
  count <- influ_residuals(glm(present ~ year, poisson(), data = d), nsim = 20)
  expect_identical(.resid_response_panel(count, "auto"), "distribution")
  expect_error(plot(count, type = "calibration"), "not a positive or combined")
  d$positive <- exp(d$x)
  positive <- influ_residuals(glm(positive ~ year, Gamma(link = "log"), data = d), nsim = 20)
  expect_identical(positive$metadata$response_kind, "positive_continuous")
  expect_identical(.resid_response_panel(positive, "auto"), "distribution")
})

test_that("bins preserve ties, reduce sparse support, and validate inputs", {
  tied <- rep(c(0, 0.1, 0.5, 0.9, 1), c(30, 60, 10, 70, 30))
  bins <- .resid_probability_bins(tied, 10, 20)
  expect_true(all(vapply(split(bins, tied), function(x) length(unique(x)) == 1L, logical(1))))
  expect_true(all(table(bins) >= 20))
  expect_lte(length(unique(bins)), 5)
  expect_identical(.resid_probability_bins(rep(0.3, 8), 10, 20), rep(1L, 8))
  expect_identical(.resid_probability_bins(c(0, 1e-12, 0.5, 1 - 1e-12, 1), 10, 1), c(1L, 1L, 2L, 3L, 3L))
  for (p in list(c(NA, 0.2), c(-0.1, 0.2), c(0.2, Inf), c(0, 1.1))) {
    expect_error(.resid_probability_bins(p, 10, 1), "finite fitted probabilities")
  }
  expect_error(.resid_probability_bins(0.3, 0, 1), "at least 1")
  d <- calibration_fixture(12)
  m <- glm(present ~ 1, binomial(), data = d)
  a <- influ_residuals(m, data = d, year = "year", nsim = 20,
    calibration_groups = "target")
  expect_true(all(a$calibration$bins$sparse))
  expect_true(all(is.na(a$calibration$groups$table$lower)))
  expect_warning(plot(a, type = "calibration_groups"), "No stored simulation envelope")
})

test_that("fixed-bin envelopes and grouped raw differences reproduce joint simulations", {
  d <- calibration_fixture()
  fit <- glm(present ~ year + x, binomial(), data = d)
  before <- .Random.seed
  a <- influ_residuals(fit, data = d, nsim = 40, batch_size = 7, seed = 632,
    calibration_groups = c("year", "target"))
  expect_identical(.Random.seed, before)
  b <- influ_residuals(fit, data = d, nsim = 40, batch_size = 40, seed = 632,
    calibration_groups = c("year", "target"))
  expect_identical(a$calibration, b$calibration)
  expect_identical(a$observations, b$observations)
  expect_equal(a$observations$probability, as.numeric(fitted(fit)))
  set.seed(632)
  u <- runif(nrow(d))
  sims <- matrix(rbinom(nrow(d) * 40, 1, fitted(fit)), nrow = nrow(d))
  expect_equal(a$observations$pit,
    (rowSums(sims < d$present) + u * (rowSums(sims == d$present) + 1)) / 41)
  expect_equal(a$observations$predicted, rowMeans(sims))
  for (i in seq_len(nrow(a$calibration$bins))) {
    j <- which(a$observations$calibration_bin == i)
    proportions <- colMeans(sims[j, , drop = FALSE])
    expect_equal(unlist(a$calibration$bins[i, c("lower", "median", "upper")]),
      quantile(proportions, c(0.025, 0.5, 0.975)), ignore_attr = TRUE)
  }
  g <- a$calibration$groups
  for (i in seq_len(nrow(g$table))) {
    j <- d$year == g$columns$year[i] & d$target == g$columns$target[i]
    expect_equal(g$table$observed[i] - g$table$predicted[i], mean(d$present[j] - fitted(fit)[j]))
    expect_equal(g$table$lower[i], quantile(colMeans(sims[j, , drop = FALSE]), 0.025), ignore_attr = TRUE)
  }
  expect_s3_class(plot(a, type = "calibration_groups"), "ggplot")
  expect_false(any(c("simulated", "simulations", "rows") %in% names(a$calibration)))
  expect_error(influ_residuals(fit, calibration_groups = "present", nsim = 20), "observed response")
  expect_error(influ_residuals(fit, calibration_groups = "not_a_column", nsim = 20), "aligned model data")
})

test_that("probability and group alignment survive missing data and reordering", {
  d <- calibration_fixture()
  d$x[8] <- NA
  fit <- glm(present ~ year + x, binomial(), data = d, subset = x > -1, na.action = na.exclude)
  a <- influ_residuals(fit, data = d[nrow(d):1, ], nsim = 20,
    calibration_groups = c("year", "target"))
  expect_identical(a$observations$row, rownames(fit$model))
  expect_equal(a$observations$probability, as.numeric(fit$fitted.values))
  d$present[20] <- 1 - d$present[20]
  if ("20" %in% rownames(fit$model)) expect_error(influ_residuals(fit, data = d, nsim = 20), "does not match")
})

test_that("known binomial denominators are distinct from arbitrary fitting weights", {
  d <- calibration_fixture()
  d$trials <- rep(2:7, length.out = nrow(d))
  d$success <- rbinom(nrow(d), d$trials, plogis(d$x))
  a_fit <- glm(cbind(success, trials-success) ~ year + x, binomial(), data = d)
  b_fit <- glm(I(success/trials) ~ year + x, binomial(), weights = trials, data = d)
  expect_error(influ_residuals(b_fit, nsim = 20), "explicit `trial_counts`")
  a <- influ_residuals(a_fit, nsim = 20)
  b <- influ_residuals(b_fit, data = d, trial_counts = "trials", nsim = 20)
  expect_equal(a$observations, b$observations)
  expect_equal(a$calibration, b$calibration)
  expect_identical(.resid_response_panel(a, "auto"), "distribution")
  expect_match(plot(a, type = "calibration")$labels$y, "successes / trials")
  j <- a$observations$calibration_bin == 1
  expect_equal(a$calibration$bins$observed[1], sum(d$success[j]) / sum(d$trials[j]))
  expect_equal(a$calibration$bins$predicted[1], weighted.mean(fitted(a_fit)[j], d$trials[j]))
  expect_error(influ_residuals(b_fit, data = d, trial_counts = "success", nsim = 20), "must match")
})

test_that("legacy saved objects retain distributions and never invent probabilities or uncertainty", {
  d <- calibration_fixture()
  a <- influ_residuals(glm(present ~ year + x, binomial(), data = d), nsim = 20)
  old <- a
  old$metadata$response_kind <- NULL
  old$calibration <- NULL
  old$observations[c("probability", "trials", "calibration_bin")] <- NULL
  expect_warning(plot(old), "older diagnostic")
  expect_no_warning(plot(old, type = "distribution"))
  expect_no_warning(plot(old, response_diagnostic = "distribution"))
  expect_error(plot(old, type = "calibration"), "no fitted-probability")
  a$calibration$bins[c("lower", "upper", "median")] <- NA_real_
  expect_warning(p <- plot(a, type = "calibration"), "without invented uncertainty")
  expect_match(p$labels$subtitle, "no stored predictive")
})

test_that("glmmTMB preserves combined routing and supports hurdle encounter checks", {
  skip_if_not_installed("glmmTMB")
  d <- calibration_fixture()
  d$catch <- ifelse(d$present == 1, exp(0.1 + d$x + rnorm(nrow(d), sd = 0.3)), 0)
  fit <- glmmTMB::glmmTMB(catch ~ year + x, ziformula = ~year + x,
    family = glmmTMB::ziGamma(link = "log"), data = d)
  original <- fit$fit$par
  combined <- influ_residuals(fit, nsim = 20)
  expect_identical(combined$metadata$response_kind, "combined")
  expect_identical(.resid_response_panel(combined, "auto"), "distribution")
  encounter <- influ_residuals(fit, component = "encounter", nsim = 20)
  expect_identical(.resid_response_panel(encounter, "auto"), "calibration")
  expect_equal(encounter$observations$observed, as.numeric(d$catch > 0))
  expect_equal(encounter$observations$probability, as.numeric(1 - predict(fit, type = "zprob")))
  expect_identical(fit$fit$par, original)
  expect_error(influ_residuals(fit, component = "positive", nsim = 20), "separate positive fit")
  d$catch <- rpois(nrow(d), 1)
  zi <- glmmTMB::glmmTMB(catch ~ year, ziformula = ~1, family = poisson(), data = d)
  expect_identical(influ_residuals(zi, nsim = 20)$metadata$response_kind, "combined")
  expect_error(influ_residuals(zi, component = "encounter", nsim = 20), "zero-inflation probability is not")
})

test_that("glmmTMB binomial missing rows and trial aggregation stay aligned", {
  skip_if_not_installed("glmmTMB")
  d <- calibration_fixture()
  d$x[3] <- NA
  fit <- glmmTMB::glmmTMB(present ~ year + x, family = binomial(),
    data = d, na.action = na.exclude)
  a <- influ_residuals(fit, data = d[nrow(d):1, ], nsim = 20)
  expect_identical(a$observations$row, rownames(fit$frame))
  expect_equal(a$observations$probability, as.numeric(predict(fit, type = "response")[-3]))
  d$trials <- rep(2:7, length.out = nrow(d))
  d$success <- rbinom(nrow(d), d$trials, 0.4)
  grouped <- glmmTMB::glmmTMB(cbind(success, trials - success) ~ year,
    family = binomial(), data = d)
  b <- influ_residuals(grouped, nsim = 20)
  expect_equal(b$observations$trials, d$trials)
  expect_equal(b$observations$probability, as.numeric(predict(grouped, type = "response")))
  expect_s3_class(plot(b, type = "calibration"), "ggplot")
})

test_that("tinyVAST Bernoulli and standard delta encounter probabilities are native", {
  skip_if_not_installed("tinyVAST")
  d <- calibration_fixture()
  d$var <- "catch"
  d$dist <- "binomial"
  fit <- tinyVAST::tinyVAST(present ~ year + x, data = d,
    family = list(binomial = binomial()), spatial_domain = NULL)
  a <- influ_residuals(fit, nsim = 20)
  expect_identical(a$metadata$response_kind, "bernoulli")
  expect_equal(a$observations$probability, as.numeric(predict(fit, what = "mu_g")))
  d$catch <- ifelse(d$present == 1, exp(0.1 + 0.2 * d$x + rnorm(nrow(d), sd = 0.4)), 0)
  d$dist <- "delta_gamma"
  delta <- tinyVAST::tinyVAST(catch ~ year + x, data = d,
    family = list(delta_gamma = tinyVAST::delta_gamma()), spatial_domain = NULL)
  b <- influ_residuals(delta, component = "encounter", nsim = 20)
  expect_equal(b$observations$probability, as.numeric(plogis(predict(delta, what = "p1_g"))))
  expect_identical(influ_residuals(delta, nsim = 20)$metadata$response_kind, "combined")
})

test_that("posterior probability averaging uses fixed joint draws in compact batches", {
  skip_if_not_installed("brms")
  p <- matrix(seq(0.05, 0.95, length.out = 60 * 12), nrow = 60)
  seen <- list()
  testthat::local_mocked_bindings(posterior_epred = function(object, draw_ids, sort, dpar = NULL) {
    seen[[length(seen) + 1L]] <<- draw_ids
    if (is.null(dpar)) p[draw_ids, , drop = FALSE] * 5 else 1 - p[draw_ids, , drop = FALSE]
  }, .package = "brms")
  adapter <- list(backend = "brms", data = data.frame(row = 1:12), trials = rep(5, 12))
  ids <- 60:1
  expect_equal(.resid_fitted_probability(list(), adapter, ids), colMeans(p))
  expect_identical(unlist(seen), ids)
  expect_lte(max(lengths(seen)), 25)
  expect_equal(.resid_fitted_probability(list(), adapter, ids, hurdle = TRUE), colMeans(p))
})

test_that("sdmTMB joint delta components route and simulate natively", {
  skip_if_not_installed("sdmTMB")
  d <- calibration_fixture()
  d$catch <- ifelse(d$present == 1, exp(0.1 + 0.3 * d$x + rnorm(nrow(d), sd = 0.4)), 0)
  fit <- sdmTMB::sdmTMB(catch ~ year + x, data = d,
    spatial = "off", family = sdmTMB::delta_gamma(), silent = TRUE)
  a <- influ_residuals(fit, component = "combined", nsim = 20)
  b <- influ_residuals(fit, component = "encounter", nsim = 20)
  c <- influ_residuals(fit, component = "positive", nsim = 20, groups = "year")
  expect_identical(.resid_response_panel(a, "auto"), "distribution")
  expect_identical(.resid_response_panel(b, "auto"), "calibration")
  expect_identical(.resid_response_panel(c, "auto"), "distribution")
  expect_equal(b$observations$probability, predict(fit, type = "response")$est1)
  expect_equal(c$observations$observed, d$catch[d$catch > 0])
  expect_identical(c$observations$row, rownames(d)[d$catch > 0])
  expect_true(all(c$observations$observed > 0))
  expect_equal(plot_predicted_residuals(c)$data, c$observations)
  grouped <- plot_grouped_residuals(c, groups = "year", min_n = 1)
  expect_equal(sum(grouped$data$n), sum(d$catch > 0))
  expect_identical(attr(grouped, "residual_metadata")$component, "positive")
  expect_equal(rownames(c$groups), c$observations$row)
  expect_identical(attr(plot_predicted_residuals(a), "residual_metadata")$component, "combined")
})

test_that("an intercept-only pooled pass does not hide a grouped discrepancy", {
  d <- expand.grid(year = factor(2000:2003), id = 1:100, target = c("A", "B"))
  d$present <- as.integer(d$id <= ifelse(d$target == "A", 20, 80))
  fit <- glm(present ~ 1, binomial(), data = d)
  a <- influ_residuals(fit, data = d, year = "year", nsim = 100,
    calibration_groups = c("year", "target"))
  expect_equal(a$calibration$bins$observed, a$calibration$bins$predicted)
  expect_true(all(abs(a$calibration$groups$table$observed - a$calibration$groups$table$predicted) > 0.25))
  expect_true(all(a$calibration$groups$table$observed < a$calibration$groups$table$lower |
    a$calibration$groups$table$observed > a$calibration$groups$table$upper))
})

test_that("calibration display is visually stable", {
  skip_if_not_installed("vdiffr")
  old <- ggplot2::theme_set(ggplot2::theme_bw())
  on.exit(ggplot2::theme_set(old), add = TRUE)
  d <- calibration_fixture()
  fit <- glm(present ~ year + x, binomial(), data = d)
  vdiffr::expect_doppelganger("bernoulli calibration overview", function() {
    print(plot(influ_residuals(fit, nsim = 40, seed = 609)))
  })
})
