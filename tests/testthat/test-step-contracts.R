test_that("stored step sequences can be relabelled without recalculation", {
  diagnostic <- influ(bentley_fixture()$model, focus = "year")
  steps <- influ_steps(list(First = diagnostic, Second = diagnostic))
  relabelled <- influ_steps(steps, labels = c("Baseline", "Full"))
  expect_identical(relabelled$steps$label, c("Baseline", "Full"))
  expect_identical(relabelled$indices$Model, c("Baseline", "Full")[steps$indices$step_id])
  expect_identical(relabelled$indices$estimate, steps$indices$estimate)
  expect_identical(relabelled$indices$lower, steps$indices$lower)
  expect_identical(steps$steps$label, c("First", "Second"))
  expect_identical(summary(relabelled), relabelled$steps)
  printed <- capture.output(result <- withVisible(print(relabelled)))
  expect_identical(result$value, relabelled)
  expect_false(result$visible)
  expect_true(any(grepl("not spatial abundance", printed, fixed = TRUE)))
  expect_true(any(grepl("Refitted: 0", printed, fixed = TRUE)))
  for (args in list(list(refit = TRUE), list(year = "month"),
                   list(component = "conditional"), list(uncertainty = "none"))) {
    expect_error(do.call(influ_steps, c(list(steps), args)), "cannot be refitted or recalculated")
  }
})

test_that("step entry points reject ambiguous instructions before fitting", {
  fit <- bentley_fixture()$model
  diagnostic <- influ(fit, focus = "year")
  for (bad in list(NA, 1, c(TRUE, FALSE))) {
    expect_error(influ_steps(fit, refit = bad), "TRUE or FALSE")
    expect_error(influ_steps(fit, keep_fits = bad), "TRUE or FALSE")
  }
  expect_error(influ_steps(fit, "year", rescale = 1), "display rescaling")
  expect_error(influ_steps(fit, uncertainty = "none", uncertainty = "none"), "unique names")
  expect_error(influ_steps(fit, refit_args = list(seed = 1)), "refit = TRUE")
  expect_error(influ_steps(diagnostic, refit = TRUE), "one original fitted model")
  expect_error(influ_steps(list(fit), refit = TRUE), "one original fitted model")
  for (bad in list(NULL, 1, list(fit, NULL))) {
    expect_error(influ_steps(bad), "supported fitted models")
  }
  expect_error(influ_steps(fit, year = NA_character_), "one focus-variable name")
  expect_error(influ_steps(fit, refit = TRUE, data = fit$model), "lock the original analysis rows")
  expect_error(influ_steps(diagnostic, uncertainty = "none"), "precomputed diagnostic")
})

test_that("step comparisons reject malformed and mismatched stored summaries", {
  original <- influ(bentley_fixture()$model, focus = "year")
  mutations <- list(
    missing = function(x) { x$indices <- x$indices[x$indices$series == "nominal", ]; x },
    duplicate = function(x) { x$indices <- rbind(x$indices, x$indices[x$indices$series == "standardised", ][1, ]); x },
    zero = function(x) { x$indices$estimate[x$indices$series == "standardised"] <- 0; x },
    response = function(x) { x$metadata$response <- "other_catch"; x },
    count = function(x) { x$metadata$n_observations <- 1L; x },
    composition = function(x) { x$composition$n[x$composition$term == "year"] <- 1; x },
    multivariate = function(x) { x$indices$response <- rep(c("catch", "other"), length.out = nrow(x$indices)); x }
  )
  messages <- c("standardised year-effect", "one finite index", "must be positive",
                "response definitions", "observation counts", "focus composition", "Select one response")
  for (i in seq_along(mutations)) {
    expect_error(influ_steps(list(original, mutations[[i]](original))), messages[i])
  }
  no_composition <- original
  no_composition$composition <- data.frame()
  expect_identical(influ_steps(no_composition)$metadata$validation,
                   "retained diagnostic summaries only")
  no_composition$composition <- original$composition[original$composition$term != "year", ]
  expect_equal(influ_steps(no_composition)$indices$estimate,
               influ_steps(original)$indices$estimate)
})

test_that("refitting rejects malformed stage specifications before model updates", {
  fit <- bentley_fixture()$model
  for (bad in list(1, list(), list(~year), list(A = 1), list(A = list(1)))) {
    expect_error(influ_steps(fit, refit = TRUE, steps = bad), "list|formula")
  }
  expect_error(influ_steps(fit, refit = TRUE,
    refit_args = list(formula = ~year)), "formulas in.*steps")
  expect_error(influ_steps(fit, refit = TRUE,
    steps = list(A = list(formula = ~year, formula. = ~year))), "only one")
  expect_error(influ_steps(fit, refit = TRUE,
    steps = list(A = area ~ year)), "original response")
  expect_error(influ_steps(fit, refit = TRUE,
    steps = list(A = ~year + unknown_predictor)), "locked analysis data")
  expect_error(influ2:::.step_backend(list()), "does not support")
  expect_error(influ2:::.step_main_formula(list(formula = ~year), "tinyVAST"), "two-sided")
  expect_error(influ2:::.step_refit_models(fit, ""), "temporal variable")
  expect_error(influ2:::.step_refit_models(fit, "year", process = 1), "must be a function")
})

test_that("step convergence gates identify unreliable backend results", {
  check <- influ2:::.step_check_convergence
  fit <- bentley_fixture()$model
  fit$converged <- FALSE
  expect_error(check(fit, "glm", "Bad fit"), "Bad fit.*did not converge")
  fit$converged <- TRUE
  fit$outer.info <- list(conv = "iteration limit reached")
  expect_error(check(fit, "gam", "Smooth"), "incomplete GAM outer convergence")
  for (backend in c("glmmTMB", "sdmTMB", "tinyVAST")) {
    model <- list()
    opt <- switch(backend, glmmTMB = "fit", sdmTMB = "model", tinyVAST = "opt")
    sdr <- switch(backend, glmmTMB = "sdr", sdmTMB = "sd_report", tinyVAST = "sdrep")
    model[[opt]] <- list(convergence = 1L)
    model[[sdr]] <- list(pdHess = TRUE)
    expect_error(check(model, backend, "Spatial"), "unsuccessful optimiser")
    model[[opt]]$convergence <- 0L
    model[[sdr]]$pdHess <- FALSE
    expect_error(check(model, backend, "Spatial"), "positive-definite Hessian")
    model[[sdr]]$pdHess <- TRUE
    expect_identical(check(model, backend, "Spatial"), model)
  }
})

test_that("brms step convergence distinguishes R-hat and divergences", {
  skip_if_not_installed("brms")
  model <- list(rhat = c(1, 1.005), divergent = 0)
  testthat::local_mocked_bindings(
    rhat = function(object, ...) object$rhat,
    nuts_params = function(object, ...) data.frame(Parameter = "divergent__", Value = object$divergent),
    .package = "brms"
  )
  check <- function(x) influ2:::.step_check_convergence(x, "brmsfit", "Posterior")
  expect_identical(check(model), model)
  model$rhat <- c(NA, Inf)
  expect_error(check(model), "no available MCMC convergence")
  model$rhat <- c(1, 1.02)
  expect_error(check(model), "R-hat above 1.01")
  model$rhat <- c(NA, 1.001)
  model$divergent <- 1
  expect_error(check(model), "divergent posterior transitions")
})
