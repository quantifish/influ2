test_that("brms index guards distinguish trials from other response additions", {
  skip_if_not_installed("brms")
  make <- function(formula) structure(list(formula = formula, fit = TRUE), class = "brmsfit")
  check <- function(model) influ2:::.index_prediction_guards(model, "brms")
  expect_silent(check(make(brms::bf(y | trials(n) ~ year + x))))
  for (formula in list(brms::bf(y | weights(w) ~ year + x),
                      brms::bf(y | trunc(lb = 0) ~ year + x))) {
    expect_error(check(make(formula)), "response additions other than trials")
  }
  expect_error(check(make(brms::bf(y ~ year + gp(x)))), "dedicated joint prediction adapter")
  expect_error(check(make(brms::bf(y ~ year, sigma ~ gp(x)))), "dedicated joint prediction adapter")
  model <- make(brms::bf(y ~ year))
  model$autocor <- TRUE
  expect_error(check(model), "dedicated joint prediction adapter")
})

test_that("brms reference profiles include trials and distributional predictors", {
  skip_if_not_installed("brms")
  trial_model <- list(formula = brms::bf(y | trials(n) ~ year + x,
                                       family = brms::brmsfamily("binomial")))
  check <- function(model, data) influ2:::.index_reference_predictors(model, "brms", "year", data)
  expect_error(check(trial_model, data.frame(x = 0)), "missing: n")
  expect_silent(check(trial_model, data.frame(x = 0, n = 10)))
  distributional <- list(formula = brms::bf(y ~ year + x, sigma ~ z))
  expect_error(check(distributional, data.frame(x = 0)), "missing: z")
  expect_silent(check(distributional, data.frame(x = 0, z = 1)))
  # brms also represents distributional terms as nested brmsformula objects.
  distributional$formula$pforms$sigma <- brms::bf(sigma ~ z)
  expect_silent(influ2:::.index_prediction_guards(
    structure(c(distributional, list(fit = TRUE)), class = "brmsfit"), "brms"))
})

test_that("brms index batches reject insufficient and malformed native draws", {
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  testthat::local_mocked_bindings(ndraws = function(object) object$n, .package = "posterior")
  testthat::local_mocked_bindings(posterior_epred = function(object, newdata, draw_ids, ...) {
    object$predictions
  }, .package = "brms")
  run <- function(model) influ2:::.index_brms(model, years = "2000", batches = list(1:2),
    newdata = function(i, rows) data.frame(x = rows), weights = c(0.25, 0.75),
    ndraws = 4, draw_batch_size = 4)
  expect_error(run(list(n = 1)), "At least two existing posterior draws")
  for (bad in list(rep(1, 8), matrix(1, 2, 4), matrix(NA_real_, 4, 2), array(1, c(4, 2, 1)))) {
    expect_error(run(list(n = 4, predictions = bad)), "finite draw-by-row matrix")
  }
  predictions <- cbind(1:4, (1:4)^2)
  good <- run(list(n = 4, predictions = predictions))
  expect_equal(as.numeric(good$draws), 0.25 * (1:4) + 0.75 * (1:4)^2)
  expect_identical(good$draw_ids, 1:4)
})

test_that("distributional brms CPUE indices reach the expected-response adapter", {
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  model <- structure(list(data = data.frame(y = 1:6, year = factor(rep(1:3, 2)),
    x = 1:6, z = 6:1), formula = brms::bf(y ~ year + x, sigma ~ z),
    family = brms::brmsfamily("gaussian"), fit = TRUE), class = "brmsfit")
  testthat::local_mocked_bindings(ndraws = function(object) 4L, .package = "posterior")
  testthat::local_mocked_bindings(posterior_epred = function(object, newdata, draw_ids, ...) {
    outer(draw_ids, as.numeric(newdata$year) + newdata$x, "+")
  }, .package = "brms")
  # Mock only the posterior prediction boundary; use brms's real formula parser
  # and influ2's full reference preparation, batching, and annual reduction.
  index <- cpue_index(model, reference_data = data.frame(x = c(0, 2), z = 1),
    reference_weights = c(1, 3), retain = "draws")
  expected <- outer(1:4, 1:3, "+") + 1.5
  expect_equal(unname(index$draws), expected)
  expect_equal(index$table$Mean, colMeans(expected))
  expect_equal(index$table$SD, apply(expected, 2, stats::sd))
})

test_that("glmmTMB index guards do not accept unsupported or unreliable fits", {
  skip_if_not_installed("glmmTMB")
  fixture <- bentley_fixture()
  fit <- glmmTMB::glmmTMB(catch ~ year + area, poisson(), data = fixture$data)
  check <- function(model) influ2:::.index_prediction_guards(model, "glmmTMB")
  expect_silent(check(fit))
  changed <- fit
  changed$modelInfo$REML <- TRUE
  expect_error(check(changed), "require ML")
  changed <- fit
  changed$modelInfo$reTrms$zi$cnms <- list(vessel = "(Intercept)")
  expect_error(check(changed), "zero-component random effects")
  changed <- fit
  changed$sdr$pdHess <- FALSE
  expect_error(check(changed), "converged with a positive-definite Hessian")
  changed <- fit
  changed$fit$convergence <- 1L
  expect_error(check(changed), "converged with a positive-definite Hessian")
})

test_that("CDI probability-link labels identify zero versus encounter effects", {
  for (link in c("logit", "probit", "cloglog")) {
    coefficients <- data.frame(estimate = 0, std_error = 0.1, lower = -0.2, upper = 0.2,
      centred_estimate = 0, centred_std_error = 0.1, centred_lower = -0.2, centred_upper = 0.2,
      cdi_scale = "difference", link = link, component = "conditional", complement = FALSE)
    encounter <- influ2:::.cdi_plot_coefficients(coefficients, "month", "centred", "auto")
    expect_false(encounter$ratio)
    expect_false(grepl("zero", encounter$label))
    coefficients$complement <- TRUE
    zero <- influ2:::.cdi_plot_coefficients(coefficients, "month", "centred", "auto")
    expect_match(zero$label, "zero")
    expect_equal(zero$data$estimate, coefficients$centred_estimate)
  }
})
