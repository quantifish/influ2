check_backend_data <- function() {
  set.seed(8021)
  d <- data.frame(year = factor(rep(2011:2013, each = 40)),
    vessel = factor(rep(1:6, length.out = 120)), x = runif(120, -1, 1))
  effect <- c(-.7, -.4, -.1, .2, .45, .65)[d$vessel]
  eta <- .7 + .12 * as.integer(d$year) + .5 * d$x + effect
  d$y <- rpois(nrow(d), exp(eta))
  d$normal <- eta + rnorm(nrow(d), sd = .3)
  d$delta <- rbinom(nrow(d), 1, plogis(.5 + .2 * d$x)) *
    rlnorm(nrow(d), .8 + .2 * as.integer(d$year) + .5 * d$x, .4)
  rownames(d) <- paste0("check-row-", seq_len(nrow(d)))
  d
}

check_backend_state <- function(model) {
  object <- if (inherits(model, "sdmTMB")) model$tmb_obj else
    if (inherits(model, c("glmmTMB", "tinyVAST"))) model$obj else NULL
  if (is.null(object)) return(NULL)
  list(last_par = object$env$last.par, best = object$env$last.par.best,
    random = object$env$random, map = object$env$map)
}

check_backend_roundtrip <- function(model, data, backend, component = "auto") {
  original <- model
  before <- check_backend_state(model)
  set.seed(2801); rng <- .Random.seed
  report <- check_cpue(model, data = data, year = "year", component = component,
    nsim = 20, batch_size = 5, seed = 704,
    residual_method = "simulation_pit", residual_tests = "none")
  expect_s3_class(report, "influ_check")
  expect_identical(report$metadata$backend, backend)
  expect_null(report$metadata$residual_error)
  expect_s3_class(report$residuals, "influ_residuals")
  expect_identical(.Random.seed, rng)
  expect_identical(model, original)
  expect_identical(check_backend_state(model), before)
  if (is.null(report$residuals)) return(invisible(report))
  residual <- report$residuals
  residual_before <- residual
  testthat::local_mocked_bindings(influ_residuals = function(...) stop("Residual reuse must not simulate"))
  reused <- check_cpue(model, data = data, year = "year", residuals = residual,
    component = component)
  expect_identical(reused$residuals$observations, residual$observations)
  expect_identical(reused$residuals$qq, residual$qq)
  expect_identical(residual, residual_before)
  expect_identical(.Random.seed, rng)
  expect_identical(check_backend_state(model), before)
  expect_equal(reused$model_statistics, report$model_statistics)
  expect_match(reused$metadata$residual_provenance, "caller remains")
  expect_null(reused$residuals$dharma)
  expect_true(all(as.data.frame(reused)$status %in%
    c("no_flag", "review", "issue", "not_assessed", "not_applicable")))
  expect_true(all(c("year", "x") %in% unique(reused$term_inventory$variables |> unlist())))
  expect_false(any(vapply(reused, function(x) is.environment(x) || is.function(x) ||
    typeof(x) == "externalptr", logical(1))))
  invisible(report)
}

test_that("GLM public reports calculate once and safely reuse same-fit residuals", {
  d <- check_backend_data()
  fit <- glm(y ~ year + x, data = d, family = poisson())
  report <- check_backend_roundtrip(fit, d, "GLM")
  expect_equal(report$model_statistics$nobs, nrow(d))
  expect_equal(report$model_statistics$AIC, AIC(fit))
})

test_that("GAM public reports preserve native smooths and adjusted fit summaries", {
  skip_if_not_installed("mgcv")
  d <- check_backend_data()
  fit <- mgcv::gam(y ~ year + s(x, k = 5), data = d, family = poisson(), method = "REML")
  report <- check_backend_roundtrip(fit, d, "GAM")
  expect_true(any(report$term_inventory$term_type == "smooth"))
  expect_equal(report$model_statistics$deviance_explained, summary(fit)$dev.expl)
  expect_equal(report$model_statistics$adjusted_R2, summary(fit)$r.sq)
  expect_true(any(report$term_checks$check == "smooth_summary"))
})

test_that("lme4 public reports preserve native mixed effects and residual reuse", {
  skip_if_not_installed("lme4", "1.1.36")
  d <- check_backend_data()
  fit <- lme4::glmer(y ~ year + x + (1 | vessel), data = d, family = poisson())
  report <- check_backend_roundtrip(fit, d, "lme4")
  expect_true(any(report$term_inventory$term_type == "random"))
  expect_equal(report$model_statistics$AIC, AIC(fit))
  expect_identical(report$residuals$metadata$conditioning, "new_effects")
})

test_that("glmmTMB public reports preserve native objective state", {
  skip_if_not_installed("glmmTMB")
  d <- check_backend_data()
  fit <- glmmTMB::glmmTMB(y ~ year + x + (1 | vessel), data = d, family = poisson())
  report <- check_backend_roundtrip(fit, d, "glmmTMB")
  expect_equal(report$model_statistics$AIC, AIC(fit))
  expect_identical(report$residuals$metadata$conditioning, "new_effects")
  # A genuine reloaded object cannot silently reinitialise its original
  # objective. Reusing the previously calculated residuals remains possible.
  reloaded <- unserialize(serialize(fit, NULL))
  state <- check_backend_state(reloaded)
  set.seed(991); rng <- .Random.seed
  testthat::local_mocked_bindings(influ_residuals = function(...) stop("No pointer reinitialisation through simulation"))
  partial <- check_cpue(reloaded, data = d)
  expect_null(partial$residuals)
  expect_match(partial$metadata$residual_error, "saved native objective")
  expect_false(.check_fit_live_objective(reloaded$obj))
  reused <- check_cpue(reloaded, data = d, residuals = report$residuals)
  expect_identical(reused$residuals$observations, report$residuals$observations)
  expect_true(is.na(reused$model_statistics$deviance))
  expect_false(.check_fit_live_objective(reloaded$obj))
  expect_identical(check_backend_state(reloaded), state)
  expect_identical(.Random.seed, rng)
})

test_that("sdmTMB public reports preserve native state across residual reuse", {
  skip_if_not_installed("sdmTMB")
  d <- check_backend_data()
  fit <- sdmTMB::sdmTMB(y ~ year + x, data = d, family = poisson(), spatial = "off", silent = TRUE)
  report <- check_backend_roundtrip(fit, d, "sdmTMB")
  expect_equal(report$model_statistics$AIC, AIC(fit))
  expect_identical(report$residuals$metadata$conditioning, "fitted")
  reloaded <- unserialize(serialize(fit, NULL))
  state <- check_backend_state(reloaded)
  set.seed(772); rng <- .Random.seed
  testthat::local_mocked_bindings(influ_residuals = function(...) stop("No saved-objective evaluation"))
  partial <- check_cpue(reloaded, data = d)
  expect_null(partial$residuals)
  expect_match(partial$metadata$residual_error, "saved native objective")
  reused <- check_cpue(reloaded, data = d, residuals = report$residuals)
  expect_identical(reused$residuals$observations, report$residuals$observations)
  expect_false(.check_fit_live_objective(reloaded$tmb_obj))
  expect_identical(check_backend_state(reloaded), state)
  expect_identical(.Random.seed, rng)
})

test_that("tinyVAST public reports do not run native null-model fits", {
  skip_if_not_installed("tinyVAST")
  d <- check_backend_data()
  fit <- tinyVAST::tinyVAST(y ~ year + x, data = d, family = poisson(), spatial_domain = NULL,
    control = tinyVAST::tinyVASTcontrol(calculate_deviance_explained = FALSE))
  testthat::local_mocked_bindings(deviance_explained = function(...) stop("No null-model fit"),
    .package = "tinyVAST")
  report <- check_backend_roundtrip(fit, d, "tinyVAST")
  expect_equal(report$model_statistics$AIC, AIC(fit))
  expect_true(is.na(report$model_statistics$deviance_explained))
  expect_identical(report$residuals$metadata$conditioning, "fitted")
})

test_that("saved complete brms fits support the public report without new MCMC", {
  skip_if_not_installed("brms")
  skip_if_not_installed("rstan")
  fit <- readRDS(test_path("fixtures", "brms-implied", "gaussian.rds"))
  testthat::local_mocked_bindings(brm = function(...) stop("No MCMC"),
    update.brmsfit = function(...) stop("No model refit"),
    bayes_R2 = function(...) stop("No new Bayesian R-squared calculation"),
    loo = function(...) stop("No LOO calculation"), .package = "brms")
  report <- check_backend_roundtrip(fit, fit$data, "brms")
  expect_true(is.na(report$model_statistics$bayes_R2))
  expect_identical(report$residuals$metadata$conditioning, "posterior_predictive")
  expect_true(all(c("rhat", "bulk_ess", "tail_ess", "divergences") %in% report$overall$check))
})

test_that("sdmTMB delta reuse keeps encounter, positive, and combined responses distinct", {
  skip_if_not_installed("sdmTMB")
  d <- check_backend_data()
  fit <- sdmTMB::sdmTMB(delta ~ year + x, data = d,
    family = sdmTMB::delta_lognormal(), spatial = "off", silent = TRUE)
  for (component in c("combined", "encounter", "positive")) {
    report <- check_backend_roundtrip(fit, d, "sdmTMB", component)
    if (is.null(report$residuals)) next
    expect_identical(report$residuals$metadata$component, component)
    if (component == "positive") {
      expect_identical(report$residuals$observations$row, rownames(d)[d$delta > 0])
      expect_equal(report$residuals$observations$observed, d$delta[d$delta > 0])
      expect_true(all(report$term_checks$status[report$term_checks$component == "encounter"] == "not_assessed"))
    } else if (component == "encounter") {
      expect_equal(report$residuals$observations$observed, as.numeric(d$delta > 0))
      expect_identical(report$residuals$metadata$response_kind, "bernoulli")
      expect_true(all(report$term_checks$status[report$term_checks$component == "positive"] == "not_assessed"))
    } else expect_equal(report$residuals$observations$observed, d$delta)
  }
})

test_that("brms hurdle reports preserve encounter and combined residual targets", {
  skip_if_not_installed("brms")
  skip_if_not_installed("rstan")
  fit <- readRDS(test_path("fixtures", "brms-implied", "hurdle.rds"))
  testthat::local_mocked_bindings(brm = function(...) stop("No MCMC"),
    update.brmsfit = function(...) stop("No refit"), .package = "brms")
  for (component in c("combined", "encounter")) {
    report <- check_backend_roundtrip(fit, fit$data, "brms", component)
    if (is.null(report$residuals)) next
    expect_identical(report$residuals$metadata$component, component)
    expected <- if (component == "encounter") as.numeric(fit$data$delta > 0) else fit$data$delta
    expect_equal(report$residuals$observations$observed, expected)
  }
  unsupported <- check_cpue(fit, data = fit$data, component = "positive", nsim = 20,
    residual_method = "simulation_pit")
  expect_null(unsupported$residuals)
  expect_match(unsupported$metadata$residual_error, "positive-only|positive-component")
  expect_identical(unsupported$overall$status[unsupported$overall$check == "residual_tests"], "not_assessed")
})

test_that("quick reports distinguish absent optional diagnostics from successful checks", {
  d <- check_backend_data()
  fit <- glm(y ~ year + x, data = d, family = poisson())
  testthat::local_mocked_bindings(.resid_dharma_available = function() FALSE)
  set.seed(679); rng <- .Random.seed
  report <- check_cpue(fit, data = d)
  expect_s3_class(report, "influ_check")
  expect_null(report$residuals)
  expect_match(report$metadata$residual_error, "DHARMa")
  expect_identical(report$overall$status[report$overall$check == "residual_tests"], "not_assessed")
  quick <- check_cpue(fit, data = d, simulate = FALSE)
  expect_null(quick$metadata$residual_error)
  expect_identical(.Random.seed, rng)
  # A compact backend fixture can describe data/terms without installing its
  # optional native backend or treating unavailable MCMC checks as passes.
  compact <- structure(list(formula = list(formula = y ~ year + x), data = d,
    family = list(family = "poisson")), class = "brmsfit")
  testthat::local_mocked_bindings(.check_fit_backend_available = function(...) FALSE)
  quick <- check_cpue(compact, data = d, simulate = FALSE)
  expect_s3_class(quick, "influ_check")
  expect_true(all(quick$overall$status[quick$overall$check %in%
    c("backend_availability", "rhat", "bulk_ess", "tail_ess", "divergences")] == "not_assessed"))
  expect_true(is.na(quick$model_statistics$AIC))
  expect_null(quick$residuals)
  expect_identical(.Random.seed, rng)
})
