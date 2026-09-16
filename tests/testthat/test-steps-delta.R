delta_step_fixture <- function() {
  set.seed(618)
  n <- 360L
  d <- data.frame(year = factor(rep(2001:2004, each = n / 4)),
    x = rnorm(n), X = runif(n), Y = runif(n), off = runif(n, 0.2, 1.3))
  d$year_scaled <- (as.integer(d$year) - 2.5) / 2
  d$y <- rbinom(n, 1, plogis(0.6 - 0.5 * d$year_scaled + 0.4 * d$x)) *
    rlnorm(n, 0.5 + 0.2 * as.integer(d$year) + 0.5 * d$x + d$off, 0.5)
  mesh <- sdmTMB::make_mesh(d, c("X", "Y"), n_knots = 10)
  sdmTMB::sdmTMB(list(y ~ year_scaled + x, y ~ year + x), data = d, mesh = mesh,
    family = sdmTMB::delta_lognormal(), spatial = "off", spatiotemporal = "off",
    offset = "off", silent = TRUE)
}

test_that("paired delta stages refit once, preserve offsets, and reuse the original", {
  skip_if_not_installed("sdmTMB")
  m <- delta_step_fixture()
  yearly <- c(occurrence = "year_scaled", positive = "year")
  stages <- list(Reduced = list(formula = list(~year_scaled, ~year),
    spatial = "off", spatiotemporal = "off"),
    Repeated = list(formula = list(~year_scaled, ~year),
      spatial = "off", spatiotemporal = "off"), Full = list(formula = m$formula))
  original <- serialize(m, NULL)
  z <- influ_steps(m, year = "year", year_term = yearly, component = "positive",
    steps = stages, refit = TRUE, keep_fits = TRUE, uncertainty = "analytic")
  expect_identical(z$steps$status, c("refitted", "reused step", "reused original"))
  expect_identical(z$fits$Full, m)
  expect_identical(z$fits$Reduced$offset, m$offset)
  expect_identical(rep(z$fits$Reduced$tmb_data$weights_i, length.out = 2L * nrow(m$data)),
    rep(m$tmb_data$weights_i, length.out = 2L * nrow(m$data)))
  expect_identical(z$fits$Reduced$data, m$data)
  expect_identical(z$fits$Reduced$spde, m$spde)
  expect_identical(serialize(m, NULL), original)
  expect_true(all(is.finite(z$indices$lower)))
  expect_s3_class(plot_step(z), "ggplot")
  manual <- sdmTMB::sdmTMB(list(y ~ year_scaled, y ~ year), data = m$data,
    mesh = m$spde, family = sdmTMB::delta_lognormal(), spatial = "off",
    spatiotemporal = "off", offset = m$offset, silent = TRUE)
  expect_equal(z$fits$Reduced$parlist$b_j, manual$parlist$b_j, tolerance = 1e-7)
  expect_equal(z$fits$Reduced$parlist$b_j2, manual$parlist$b_j2, tolerance = 1e-7)
  expect_error(influ_steps(m, year = "year", year_term = yearly,
    component = "positive", refit = TRUE), "explicit.*steps")
  expect_error(.step_refit_models(m, "year", stages, year_term = "year"), "one annual predictor")
  expect_error(.step_refit_models(m, "year", list(Bad = list(formula = list(~x, ~year))),
    year_term = yearly), "additive year")
  expect_error(.step_refit_models(m, "year", list(Bad = list(formula = list(x~year_scaled, ~year))),
    year_term = yearly), "original response")
})

test_that("paired annual predictors give native occurrence, positive, and combined contrasts", {
  skip_if_not_installed("sdmTMB")
  m <- delta_step_fixture()
  yearly <- c(occurrence = "year_scaled", positive = "year")
  X <- m$tmb_data$X_ij
  beta <- list(m$parlist$b_j, m$parlist$b_j2)
  centred <- lapply(1:2, function(k) {
    j <- which(attr(X[[k]], "assign") == 1L)
    eta <- drop(X[[k]][, j, drop = FALSE] %*% beta[[k]][j])
    as.numeric(tapply(eta, m$data$year, mean)) - mean(eta)
  })
  ref <- mean(drop(X[[1L]] %*% beta[[1L]]))
  expected <- list(occurrence = plogis(ref + centred[[1L]]) - plogis(ref), positive = exp(centred[[2L]]),
    unconditional_mean = plogis(ref + centred[[1L]]) / plogis(ref) * exp(centred[[2L]]))
  for (component in names(expected)) {
    z <- influ_steps(m, year = "year", year_term = yearly, component = component,
      uncertainty = "none")
    expect_equal(z$indices$estimate, expected[[component]], tolerance = 1e-10)
    expect_identical(z$metadata$estimand, "year_effect_contrast")
    expect_match(z$metadata$validation, "fitted response")
    sim <- influ_steps(m, year = "year", year_term = yearly, component = component,
      uncertainty = "simulation", ndraws = 100L, seed = 13)
    expect_true(all(is.finite(sim$indices$lower)))
    expect_true(all(sim$indices$upper > sim$indices$lower))
  }
  expect_error(influ_steps(m, year = "year", year_term = yearly), "component")
  expect_error(influ_steps(m, year = "year", component = "positive"), "additive year")
  bad <- m
  bad$data$year_scaled[1] <- 12
  expect_error(influ_steps(bad, year = "year", year_term = yearly,
    component = "positive"), "constant within")
  bad <- m
  bad$offset <- m$offset + 1
  expect_error(influ_steps(list(m, bad), year = "year", year_term = yearly,
    component = "positive"), "offset")
  # The existing full-decomposition guard remains; no offset is dropped.
  expect_error(influ(m, focus = "year"), "offset")
})

test_that("delta step references, uncertainty, and structure guards are explicit", {
  skip_if_not_installed("sdmTMB")
  m <- delta_step_fixture()
  yearly <- c(occurrence = "year_scaled", positive = "year")
  calculate <- function(fit = m, ...) influ_steps(fit, year = "year",
    year_term = yearly, component = "unconditional_mean", ...)
  ref <- m$data[match(levels(m$data$year), m$data$year), , drop = FALSE]
  ref$x <- 0
  weights <- 1:4
  z <- calculate(reference_data = ref, reference_weights = weights, uncertainty = "none")
  X <- stats::predict(m, newdata = ref, re_form = NA, offset = rep(0, nrow(ref)),
    return_tmb_data = TRUE)$proj_X_ij
  annual <- lapply(1:2, function(k) {
    cols <- which(attr(m$tmb_data$X_ij[[k]], "assign") == 1L)
    beta <- m$parlist[[c("b_j", "b_j2")[k]]]
    observed <- drop(m$tmb_data$X_ij[[k]][, cols, drop = FALSE] %*% beta[cols])
    reference <- drop(X[[k]][, cols, drop = FALSE] %*% beta[cols])
    as.numeric(tapply(observed, m$data$year, mean)) - weighted.mean(reference, weights)
  })
  p_ref <- weighted.mean(drop(X[[1L]] %*% m$parlist$b_j), weights)
  expected <- plogis(p_ref + annual[[1L]]) / plogis(p_ref) * exp(annual[[2L]])
  expect_equal(z$indices$estimate, expected, tolerance = 1e-10)
  expect_identical(z$metadata$reference, "prediction_grid")
  single <- influ_steps(m, year = "year", year_term = yearly, component = "positive",
    reference_data = ref, reference_weights = weights, uncertainty = "analytic")
  expect_equal(single$indices$estimate, exp(annual[[2L]]), tolerance = 1e-10)
  set.seed(138)
  old_rng <- .Random.seed
  sim <- calculate(uncertainty = "simulation", ndraws = 150L, seed = 13)
  expect_identical(.Random.seed, old_rng)
  expect_equal(sim, calculate(uncertainty = "simulation", ndraws = 150L, seed = 13))
  expect_lt(as.numeric(object.size(sim)), 30000)
  expect_error(calculate(reference_data = ref[, setdiff(names(ref), "year_scaled")]), "annual predictors")
  expect_error(.step_year_terms(m$formula, "year", c(a = "year_scaled", b = "year")), "Name paired")
  expect_identical(.step_year_terms(m$formula, "year", rev(yearly)), unname(yearly))
  expect_error(.step_update_formula(m$formula, ~year, unname(yearly)), "paired list")
  bad <- m
  bad$family$type <- "poisson-link"
  expect_error(calculate(bad), "standard delta")
  bad <- m
  bad$time_varying <- ~1
  expect_error(calculate(bad), "time-varying")
  bad <- m
  attr(bad$tmb_data$X_ij[[1L]], "assign") <- rep(0L, ncol(bad$tmb_data$X_ij[[1L]]))
  expect_error(calculate(bad), "annual design")
  bad <- m
  names(bad$sd_report$par.fixed)[1] <- "absent"
  expect_error(calculate(bad), "joint fixed-effect covariance")
  bad <- m
  bad$tmb_data$weights_i[nrow(m$data) + 1L] <- 2
  expect_error(.step_locked_data(bad, bad$formula, "sdmTMB"), "different likelihood weights")
  bad$offset <- NULL
  bad$tmb_data$weights_i <- m$tmb_data$weights_i
  expect_error(.step_locked_data(bad, bad$formula, "sdmTMB"), "align with the fitted rows")
  expect_error(influ_steps(sim, year_term = yearly), "cannot be refitted")
})
