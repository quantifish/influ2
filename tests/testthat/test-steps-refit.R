step_refit_data <- function() {
  set.seed(112)
  data <- data.frame(year = factor(rep(2001:2005, each = 40)),
    x = stats::rnorm(200), z = stats::runif(200), group = factor(rep(1:10, 20)),
    effort = stats::runif(200, 1, 3), off = stats::rnorm(200, 0, 0.1),
    w = rep(c(1, 2), 100), keep = rep(c(TRUE, TRUE, FALSE, TRUE), 50))
  data$y <- stats::rpois(200, exp(1 + 0.2 * as.numeric(data$year) + data$x / 3))
  data
}

test_that("automatic steps fit reduced formulas on locked rows and reuse the full model", {
  data <- step_refit_data()
  data$x[c(3, 7, 41)] <- NA
  model <- stats::glm(y ~ year + x + z, data = data, family = stats::poisson())
  result <- .step_refit_models(model, "year")
  expect_identical(result$steps$status, c("refitted", "refitted", "reused original"))
  expect_identical(result$fits[[3L]], model)
  expect_true(all(vapply(result$fits, function(fit) nrow(stats::model.frame(fit)), integer(1)) == 197L))
  manual <- stats::glm(y ~ year, data = result$data, family = stats::poisson())
  expect_equal(stats::coef(result$fits[[1L]]), stats::coef(manual))
  expect_identical(attr(stats::terms(result$fits[[2L]]), "term.labels"), c("year", "x"))
})

test_that("refits preserve subsets, fitting weights, and both kinds of offsets", {
  data <- step_refit_data()
  data$x[2] <- NA
  model <- stats::glm(y ~ year + x + offset(log(effort)), data = data,
    family = stats::poisson(), weights = w, subset = keep, offset = off)
  result <- .step_refit_models(model, "year", steps = list(Baseline = ~year, Full = formula(model)))
  manual <- stats::glm(y ~ year + offset(log(effort)), data = result$data,
    family = stats::poisson(), weights = w, offset = off)
  expect_equal(stats::coef(result$fits[[1L]]), stats::coef(manual))
  expect_equal(stats::model.weights(stats::model.frame(result$fits[[1L]])), result$data$w)
  expect_equal(stats::model.offset(stats::model.frame(result$fits[[1L]])),
    log(result$data$effort) + result$data$off)
  expect_identical(result$fits[[2L]], model)
  expect_error(.step_refit_models(model, "year", steps = list(
    Invalid = ~year + offset(2 * log(effort))
  )), "original offset expressions")
  no_offset <- stats::glm(y ~ year + x, data = data, family = stats::poisson())
  expect_error(.step_refit_models(no_offset, "year", steps = list(
    Invalid = ~year + offset(log(effort))
  )), "original offset expressions")
})

test_that("step refits use fitted raw data and reject mutated transformed covariates", {
  data <- step_refit_data()
  model <- stats::glm(y ~ year + x, data = data, family = stats::poisson())
  fitted_response <- data$y
  data$y <- rep(99, nrow(data))
  data$x <- data$x + 100
  result <- .step_refit_models(model, "year")
  expect_equal(result$data$y, fitted_response)
  expect_equal(result$data$x, stats::model.frame(model)$x)
  transformed <- stats::glm(y ~ year + poly(z, 2), data = data, family = stats::poisson())
  data$z <- data$z + 100
  expect_error(.step_refit_models(transformed, "year"), "no longer reproduce")
})

test_that("duplicate step specifications fit once and process compactly", {
  data <- step_refit_data()
  model <- stats::glm(y ~ year + x, data = data, family = stats::poisson())
  calls <- 0L
  result <- .step_refit_models(model, "year", steps = list(
    First = ~year, Again = ~year, Full = formula(model)),
    process = function(fit) {
      calls <<- calls + 1L
      list(coefficients = stats::coef(fit))
    })
  expect_equal(calls, 2L)
  expect_identical(result$steps$status, c("refitted", "reused step", "reused original"))
  expect_identical(result$fits[[1L]], result$fits[[2L]])
  expect_false(any(vapply(result$fits, inherits, logical(1), "glm")))
  passed_data <- NULL
  .step_refit_models(model, "year", steps = list(Full = formula(model)),
    process = function(fit, data = NULL) {
      passed_data <<- data
      stats::coef(fit)
    })
  expect_equal(passed_data, result$data)
})

test_that("unsafe or unsupported stage specifications fail explicitly", {
  data <- step_refit_data()
  model <- stats::glm(y ~ year + x, data = data, family = stats::poisson())
  expect_error(.step_refit_models(model, "year", list(~year)), "uniquely named")
  expect_error(.step_refit_models(model, "year", list(A = ~x)), "additive year")
  expect_error(.step_refit_models(model, "year", list(A = z ~year)), "original response")
  expect_error(.step_refit_models(model, "year", refit_args = list(data = data)), "overriding")
  expect_error(.step_refit_models(model, "year", refit_args = list(recompile = FALSE)), "recompilation")
  expect_error(.step_refit_models(model, "year", list(A = list(testmode = TRUE))), "overriding")
  interaction <- stats::glm(y ~ year * x, data = data, family = stats::poisson())
  expect_error(.step_refit_models(interaction, "year"), "year interactions")
  failed <- model
  failed$converged <- FALSE
  expect_error(.step_refit_models(failed, "year", list(Full = formula(model))), "did not converge")
})

test_that("GAM plans retain complete smooth blocks", {
  skip_if_not_installed("mgcv")
  data <- step_refit_data()
  model <- mgcv::gam(y ~ year + s(x, k = 4) + z, family = stats::poisson(), data = data, method = "REML")
  result <- .step_refit_models(model, "year")
  expect_length(result$fits, 3L)
  expect_length(result$fits[[1L]]$smooth, 0L)
  expect_length(result$fits[[2L]]$smooth, 1L)
  expect_identical(result$fits[[3L]], model)
  manual <- mgcv::gam(y ~ year + s(x, k = 4), family = stats::poisson(), data = data, method = "REML")
  expect_equal(stats::coef(result$fits[[2L]]), stats::coef(manual), tolerance = 1e-8)
  model$call[[1L]] <- quote(gam)
  unqualified <- .step_refit_models(model, "year")
  expect_equal(stats::coef(unqualified$fits[[2L]]), stats::coef(manual), tolerance = 1e-8)
  expect_identical(unqualified$fits[[2L]]$call[[1L]], quote(mgcv::gam))
})

test_that("BRMS refits clear file caches and permit recompilation", {
  model <- list(file = "cached-fit", stan_args = list(file = "another-cache"),
    influ2_draws = matrix(0, 4, 2))
  locked <- list(data = data.frame(year = 1:3))
  captured <- NULL
  testthat::local_mocked_bindings(update = function(object, ...) {
    captured <<- c(list(object = object), list(...))
    "newly fitted"
  }, .package = "stats")
  result <- .step_refit_one(model, y ~ year, list(chains = 2), locked, "brmsfit")
  expect_identical(result, "newly fitted")
  expect_null(captured$object$file)
  expect_null(captured$object$stan_args$file)
  expect_null(captured$object$influ2_draws)
  expect_null(captured$recompile)
  expect_identical(captured$file_refit, "always")
  expect_identical(captured$newdata, locked$data)
})

test_that("execution controls do not refit the unchanged full stage", {
  data <- step_refit_data()
  model <- stats::glm(y ~ year + x, data = data, family = stats::poisson())
  result <- .step_refit_models(model, "year", steps = list(Full = formula(model)),
    refit_args = list(seed = 90, cores = 2, refresh = 0))
  expect_identical(result$steps$status, "reused original")
  expect_identical(result$fits[[1]], model)
})

test_that("tinyVAST reuse compares stored settings rather than mutable call symbols", {
  data <- step_refit_data()
  space_term <- "changed"
  model <- structure(list(
    formula = y ~ year + x, data = data,
    call = quote(tinyVAST::tinyVAST(y ~ year + x, data = data, space_term = space_term)),
    internal = list(space_term = "original"),
    opt = list(convergence = 0), sdrep = list(pdHess = TRUE)
  ), class = "tinyVAST")
  refit_calls <- 0L
  testthat::local_mocked_bindings(.step_refit_one = function(model, formula, args, locked, backend) {
    refit_calls <<- refit_calls + 1L
    model
  })
  result <- .step_refit_models(model, "year", steps = list(
    Changed = list(space_term = space_term), Original = list(space_term = "original")))
  expect_equal(refit_calls, 1L)
  expect_identical(result$steps$status, c("refitted", "reused original"))
  model$internal <- NULL
  result <- .step_refit_models(model, "year", steps = list(Unknown = list(space_term = space_term)))
  expect_identical(result$steps$status, "refitted")
})

test_that("latent backends enforce explicit stages and convergence gates", {
  for (backend in c("tinyVAST", "sdmTMB")) {
    expect_error(.step_auto_formulas(list(), y ~ year + x, "year", backend), "explicit")
  }
  expect_error(.step_check_convergence(list(fit = list(convergence = 1), sdr = list(pdHess = TRUE)),
    "glmmTMB", "Failed"), "unsuccessful optimiser")
  expect_error(.step_check_convergence(list(opt = list(convergence = 0), sdrep = list(pdHess = FALSE)),
    "tinyVAST", "Failed"), "positive-definite")
})

test_that("tinyVAST reconstruction preserves the domain and explicit NULL fields", {
  skip_if_not_installed("tinyVAST")
  captured <- NULL
  testthat::local_mocked_bindings(tinyVAST = function(formula, data,
      spatial_domain = NULL, space_term = NULL, spacetime_term = NULL, ...) {
    captured <<- list(formula = formula, data = data, domain = spatial_domain,
      space_term = space_term, spacetime_term = spacetime_term)
  }, .package = "tinyVAST")
  data <- step_refit_data()
  model <- list(
    call = quote(tinyVAST::tinyVAST(y ~ year + x, data = missing_data,
      spatial_domain = missing_domain, space_term = "persistent", spacetime_term = "AR1")),
    spatial_domain = list(saved_domain = TRUE)
  )
  .step_refit_one(model, y ~ year, list(space_term = NULL, spacetime_term = NULL),
    list(data = data), "tinyVAST")
  expect_identical(captured$data, data)
  expect_identical(captured$domain, model$spatial_domain)
  expect_null(captured$space_term)
  expect_null(captured$spacetime_term)
})

test_that("sdmTMB reduced refits retain their mesh and reuse matching field settings", {
  skip_if_not_installed("sdmTMB")
  data <- step_refit_data()
  data$X <- stats::runif(nrow(data))
  data$Y <- stats::runif(nrow(data))
  mesh <- sdmTMB::make_mesh(data, c("X", "Y"), n_knots = 12)
  model <- sdmTMB::sdmTMB(y ~ year + x, data = data, mesh = mesh,
    family = stats::poisson(), spatial = "off", spatiotemporal = "off")
  model$call[[1L]] <- quote(sdmTMB)
  result <- .step_refit_models(model, "year", steps = list(
    Baseline = list(formula = ~year, spatial = "off", spatiotemporal = "off"),
    Full = list(formula = ~year + x, spatial = "off", spatiotemporal = "off")
  ))
  expect_identical(result$steps$status, c("refitted", "reused original"))
  expect_match(result$steps$settings[1], "spatial = .off.")
  expect_identical(result$fits[[1L]]$spde, model$spde)
  expect_identical(result$fits[[2L]], model)
  expect_identical(result$fits[[1L]]$call[[1L]], quote(sdmTMB::sdmTMB))
})

test_that("saved unqualified glmmTMB calls refit without attaching the package", {
  skip_if_not_installed("glmmTMB")
  data <- step_refit_data()
  model <- glmmTMB::glmmTMB(y ~ year + x, data = data, family = stats::poisson())
  model$call[[1L]] <- quote(glmmTMB)
  result <- .step_refit_models(model, "year")
  manual <- glmmTMB::glmmTMB(y ~ year, data = data, family = stats::poisson())
  expect_equal(glmmTMB::fixef(result$fits[[1L]]), glmmTMB::fixef(manual))
  expect_identical(result$fits[[1L]]$call[[1L]], quote(glmmTMB::glmmTMB))
})
