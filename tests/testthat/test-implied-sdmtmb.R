implied_sdmtmb_fixture <- local({
  value <- NULL
  function() {
    skip_if_not_installed("sdmTMB")
    if (!is.null(value)) return(value)
    set.seed(491)
    d <- expand.grid(year = factor(2011:2013), area = factor(c("A", "B")), record = 1:90)
    d$year_scaled <- as.numeric(d$year) - 2
    d$x <- runif(nrow(d), -1, 1)
    d$effort <- runif(nrow(d), -.6, .6)
    d$vessel <- factor(sample(1:9, nrow(d), TRUE))
    re <- rnorm(9, sd = .6)[d$vessel]
    mu <- exp(1 + .25 * as.numeric(d$year) + .35 * (d$area == "B") + .3 * d$x + re + d$effort)
    present <- rbinom(nrow(d), 1, plogis(.5 + .3 * d$year_scaled + .4 * d$x + re))
    d$group <- as.character(d$area)
    d$group[d$record <= 6] <- "zero"
    d$group[d$record > 6 & d$record <= 12] <- "one"
    present[d$group == "zero"] <- 0
    present[d$group == "one"] <- 1
    d$group <- factor(d$group, levels = c("A", "B", "zero", "one", "unused"))
    d$response <- present * rlnorm(nrow(d), log(mu) - .7^2 / 2, .7)
    rownames(d) <- paste0("observation", seq_len(nrow(d)))
    m <- sdmTMB::sdmTMB(list(response ~ year_scaled + area + x + (1 | vessel),
      response ~ year + area + x + (1 | vessel)), data = d, offset = d$effort,
      family = sdmTMB::delta_lognormal(), spatial = "off", silent = TRUE)
    value <<- list(data = d, model = m)
    value
  }
})

test_that("sdmTMB positive effects use native lognormal means, offsets, and joint rows", {
  f <- implied_sdmtmb_fixture(); m <- f$model; d <- f$data
  mode <- m$tmb_obj$env$last.par.best
  saved <- m$last.par.best
  rng <- .Random.seed
  z <- implied_effects(m, year = "year", groups = "area", component = "positive")
  expect_identical(m$tmb_obj$env$last.par.best, mode)
  expect_identical(m$last.par.best, saved)
  expect_identical(.Random.seed, rng)
  p <- predict(m, type = "link", offset = m$offset)
  sigma <- exp(m$parlist$ln_phi[2])
  X <- m$tmb_data$X_ij[[2]]
  columns <- which(attr(X, "assign") %in% c(1, 2))
  base <- drop(X[, columns] %*% m$parlist$b_j2[columns]); base <- base - mean(base)
  for (j in seq_len(nrow(z$table))) {
    cell <- z$table[j, ]
    i <- d$year == cell$level & d$area == cell$group & d$response > 0
    expected <- mean(log(d$response[i]) - (p$est2[i] - sigma^2 / 2))
    expect_equal(cell$n, sum(i))
    expect_equal(cell$baseline, mean(base[i]))
    expect_equal(cell$adjustment, expected, tolerance = 1e-7)
    expect_equal(cell$std_error, sigma / sqrt(sum(i)))
    ll <- function(delta) sum(dlnorm(d$response[i], p$est2[i] + delta - sigma^2 / 2, sigma, log = TRUE))
    expect_equal(.implied_loglik(cell$adjustment, d$response[i], p$est2[i],
      rep(sigma, sum(i)), "lognormal"), ll(cell$adjustment))
    expect_equal(cell$adjustment, optimize(ll, c(-5, 5), maximum = TRUE)$maximum, tolerance = 1e-6)
    expect_equal(2 * (ll(expected) - ll(cell$lower - cell$baseline)), qchisq(.95, 1), tolerance = 1e-6)
    expect_equal(2 * (ll(expected) - ll(cell$upper - cell$baseline)), qchisq(.95, 1), tolerance = 1e-6)
  }
  expect_identical(z$metadata$component, "positive")
  expect_equal(z$metadata$n, sum(d$response > 0))
  expect_equal(z$metadata$n_total, nrow(d))
  expect_equal(z$table, implied_effects(m, data = d[nrow(d):1, ], year = "year", groups = "area", component = "positive")$table)
  expect_lt(as.numeric(object.size(z)), 25000)
  expect_identical(unserialize(serialize(z, NULL)), z)
  expect_s3_class(ggplot2::ggplot_build(plot(z)), "ggplot_built")
  expect_match(plot(z)$labels$title, "positive component")
  expect_match(plot(z)$labels$caption, "not the combined")
  # Compare a separate saved model: no live original objective is required.
  restored <- unserialize(serialize(m, NULL))
  expect_equal(implied_effects(restored, year = "year", groups = "area", component = "positive")$table, z$table)
})

test_that("sdmTMB spatial and yearly fields remain in component likelihoods", {
  skip_if_not_installed("sdmTMB")
  data("pcod_2011", package = "sdmTMB")
  data("pcod_mesh_2011", package = "sdmTMB")
  d <- pcod_2011
  # Add a known changing spatial gradient; the stock dataset alone may estimate
  # a zero yearly-field variance, which would not exercise field retention.
  d$density <- d$density * exp(2 * sin(2 * as.numeric(scale(d$X))) +
    .6 * as.numeric(scale(d$Y)) * (as.numeric(factor(d$year)) - 2.5))
  d$group <- factor(ifelse(d$depth_scaled < 0, "shallow", "deep"))
  m <- sdmTMB::sdmTMB(density ~ factor(year) + depth_scaled, data = d,
    mesh = pcod_mesh_2011, family = sdmTMB::delta_lognormal(), time = "year",
    spatial = "on", spatiotemporal = "iid", silent = TRUE)
  expect_true(m$sd_report$pdHess)
  mode <- m$tmb_obj$env$last.par.best
  z <- implied_effects(m, year = "year", groups = "group", component = "positive", min_n = 3)
  r <- .implied_sdmtmb_report(m)
  expect_gt(max(abs(r$omega_s_A[, 2])), .01)
  expect_gt(max(abs(r$epsilon_st_A_vec[, 2])), .01)
  native <- m$tmb_obj$report(mode)
  expect_equal(r$eta_i, native$eta_i)
  y <- d$density; positive <- y > 0
  ll <- sum(dbinom(as.numeric(positive), 1, plogis(r$eta_i[, 1]), log = TRUE)) +
    sum(dlnorm(y[positive], r$eta_i[positive, 2] - r$phi[2]^2 / 2, r$phi[2], log = TRUE))
  expect_lt(abs(ll + sum(native$jnll_obs)), 1e-7)
  for (j in which(z$table$status == "ok")) {
    cell <- z$table[j, ]
    i <- d$year == cell$level & d$group == cell$group & positive
    expect_equal(cell$adjustment, mean(log(y[i]) - r$eta_i[i, 2] + r$phi[2]^2 / 2))
  }
  expect_identical(m$tmb_obj$env$last.par.best, mode)
})

test_that("encounter effects use the selected annual predictor and Bernoulli likelihood", {
  f <- implied_sdmtmb_fixture(); m <- f$model; d <- f$data
  z <- implied_effects(m, year = "year", year_term = "year_scaled", groups = "area", component = "encounter")
  p <- predict(m, type = "link", offset = m$offset)
  for (j in seq_len(nrow(z$table))) {
    cell <- z$table[j, ]
    i <- d$year == cell$level & d$area == cell$group
    ll <- function(delta) sum(dbinom(as.numeric(d$response[i] > 0), 1, plogis(p$est1[i] + delta), log = TRUE))
    expect_equal(cell$n, sum(i))
    expect_lt(abs(cell$adjustment - optimize(ll, c(-10, 10), maximum = TRUE, tol = 1e-9)$maximum), 1e-6)
    expect_equal(2 * (ll(cell$adjustment) - ll(cell$lower - cell$baseline)), qchisq(.95, 1), tolerance = 1e-6)
  }
  expect_equal(z$metadata$baseline_terms, c("year_scaled", "area"))
  expect_equal(z$metadata$year_levels, levels(d$year))
  expect_equal(plot(z)$labels$y, "Implied effect (log-odds scale)")
  expect_error(implied_effects(m, year = "year", groups = "area", component = "encounter"), "year_term")
  expect_error(implied_effects(m, year = "year", year_term = "x", groups = "area", component = "encounter"), "constant within")
})

test_that("combined response shifts and profile limits match independent native densities", {
  f <- implied_sdmtmb_fixture(); m <- f$model; d <- f$data
  z <- implied_effects(m, year = "year", groups = "area", component = "combined")
  p <- predict(m, type = "link", offset = m$offset)
  sigma <- exp(m$parlist$ln_phi[2])
  for (j in seq_len(nrow(z$table))) {
    cell <- z$table[j, ]
    i <- d$year == cell$level & d$area == cell$group
    y <- d$response[i]; pos <- y > 0
    e1 <- p$est1[i]; e2 <- p$est2[i]
    ll <- function(theta) sum(dbinom(as.numeric(pos), 1, plogis(e1 + theta[1]), log = TRUE)) +
      sum(dlnorm(y[pos], e2[pos] + theta[2] - sigma^2 / 2, sigma, log = TRUE))
    mle <- optim(c(0, 0), function(theta) -ll(theta), method = "BFGS", control = list(reltol = 1e-12))$par
    expect_equal(c(cell$encounter_adjustment, cell$positive_adjustment), mle, tolerance = 1e-5)
    expect_equal(cell$baseline, mean(plogis(e1) * exp(e2)), tolerance = 1e-7)
    expect_equal(cell$estimate, mean(plogis(e1 + mle[1]) * exp(e2 + mle[2])), tolerance = 1e-5)
    expect_equal(cell$adjustment, log(cell$estimate / cell$baseline))
    # Independently constrain the derived mean and maximise over encounter.
    profile <- function(target) optimize(function(delta) {
      delta2 <- log(target / mean(plogis(e1 + delta) * exp(e2)))
      ll(c(delta, delta2))
    }, c(-15, 15), maximum = TRUE, tol = 1e-10)$objective
    for (bound in c(cell$lower, cell$upper)) {
      expect_equal(2 * (ll(mle) - profile(bound)), qchisq(.95, 1), tolerance = 1e-5)
    }
  }
  expect_equal(z$metadata$baseline, "observed_fitted_mean")
  expect_equal(plot(z)$labels$y, "Expected response")
  expect_match(plot(z)$labels$subtitle, "not a standardised index")
  expect_true(all(is.na(implied_effects(m, year = "year", groups = "area", component = "combined", interval = "none")$table$lower)))
  expect_error(implied_effects(m, year = "year", groups = "area", component = "combined", interval = "descriptive"), "require method")
})

test_that("component support counts and missing cells are preserved", {
  f <- implied_sdmtmb_fixture(); m <- f$model
  positive <- implied_effects(m, year = "year", groups = "group", component = "positive")
  encounter <- implied_effects(m, year = "year", year_term = "year_scaled", groups = "group", component = "encounter")
  combined <- implied_effects(m, year = "year", groups = "group", component = "combined")
  expect_true(all(subset(positive$table, group %in% c("zero", "unused"))$status == "empty"))
  expect_true(all(subset(encounter$table, group == "zero")$status == "boundary_zero"))
  expect_true(all(subset(encounter$table, group == "one")$status == "boundary_one"))
  expect_true(all(subset(combined$table, group == "zero")$status == "empty_positive"))
  expect_true(all(subset(combined$table, group == "one")$status == "boundary_one"))
  expect_true(all(subset(combined$table, group == "unused")$status == "empty"))
  expect_identical(positive$metadata$baseline_terms, "year")
  expect_false(positive$metadata$baseline_group_present)
  expect_match(plot(positive)$labels$subtitle, "no fixed group")
  expect_true(all(subset(implied_effects(m, year = "year", groups = "group", component = "positive", min_n = 20)$table, group == "one")$status == "sparse"))
})

test_that("sdmTMB implied effects reject ambiguous or unvalidated requests", {
  f <- implied_sdmtmb_fixture(); m <- f$model; d <- f$data
  calc <- function(model = m, data = NULL, ...) implied_effects(model, data = data, year = "year", groups = "area", component = "positive", ...)
  expect_error(implied_effects(m, year = "year", groups = "area"), "explicit component")
  expect_error(calc(method = "traditional"), "Gaussian")
  expect_error(calc(interval = "descriptive"), "log-response variance")
  d$response[1] <- d$response[1] + 1
  expect_error(calc(data = d), "does not match")
  expect_error(calc(data = f$data[-1, ]), "original row names")
  bad <- m; bad$tmb_data$y_i[1, 2] <- 1
  expect_error(calc(bad), "likelihood rows")
  bad <- m; bad$sd_report$pdHess <- FALSE
  expect_error(calc(bad), "positive-definite")
  bad <- m; bad$model$convergence <- 1L
  expect_error(calc(bad), "converged ML")
  bad <- m; bad$reml <- TRUE
  expect_error(calc(bad), "converged ML")
  bad <- m; bad$tmb_data$weights_i[1] <- 2
  expect_error(calc(bad), "Non-unit")
  bad <- m; bad$family$type <- "poisson_link_delta"
  expect_error(calc(bad), "Poisson-link")
  bad <- m; bad$family[[2]]$family <- "lognormal_mix"
  expect_error(calc(bad), "mixture")
  bad <- m; bad$last.par.best[1] <- bad$last.par.best[1] + 1
  expect_error(calc(bad), "joint mode")
  bad <- m; bad$nonlocal_parsed <- list()
  expect_error(calc(bad), "Nonlocal")
})

test_that("single-response sdmTMB lognormal and Bernoulli fits use the same engine", {
  f <- implied_sdmtmb_fixture(); d <- f$data
  d <- d[d$response > 0, ]
  m <- sdmTMB::sdmTMB(response ~ year + area + x, data = d,
    family = sdmTMB::lognormal(), offset = d$effort, spatial = "off", silent = TRUE)
  z <- implied_effects(m, year = "year", groups = "area")
  expect_true(all(z$table$status == "ok"))
  expect_equal(z$table, implied_effects(m, year = "year", groups = "area", component = "conditional")$table)
  expect_error(implied_effects(m, year = "year", groups = "area", component = "positive"), "single-response", ignore.case = TRUE)
  d <- f$data; d$encounter <- as.numeric(d$response > 0)
  b <- sdmTMB::sdmTMB(encounter ~ year + area + x, data = d,
    family = binomial(), spatial = "off", silent = TRUE)
  z <- implied_effects(b, year = "year", groups = "area")
  expect_equal(z$metadata$family, "binomial")
  expect_true(all(is.finite(z$table$estimate)))
})
