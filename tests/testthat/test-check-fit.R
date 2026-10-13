check_fit_status <- function(x, name) x$checks$status[x$checks$check == name]

test_that("GLM checks expose native numerical status and descriptive statistics", {
  fit <- glm(mpg ~ wt + hp, data = mtcars, family = gaussian())
  original <- fit
  set.seed(246); rng <- .Random.seed
  x <- .check_fit(fit)
  expect_identical(x$backend, "GLM")
  expect_equal(nrow(x$model_statistics), 1L)
  expect_identical(check_fit_status(x, "convergence"), "no_flag")
  expect_identical(check_fit_status(x, "finite_coefficients"), "no_flag")
  expect_identical(check_fit_status(x, "finite_uncertainty"), "no_flag")
  expect_identical(check_fit_status(x, "gradient"), "not_applicable")
  for (name in c("AIC", "BIC", "deviance")) {
    fun <- get(name, envir = asNamespace("stats"))
    expect_equal(x$model_statistics[[name]], as.numeric(fun(fit)))
  }
  expect_equal(x$model_statistics$logLik, as.numeric(logLik(fit)))
  expect_equal(x$model_statistics$df, attr(logLik(fit), "df"))
  expect_equal(x$model_statistics$nobs, nobs(fit))
  expect_equal(x$model_statistics$deviance_explained, 1 - fit$deviance / fit$null.deviance)
  expect_true(is.na(x$model_statistics$adjusted_R2))
  expect_true(is.na(x$model_statistics$bayes_R2))
  expect_identical(fit, original)
  expect_identical(.Random.seed, rng)
  expect_false(any(vapply(x$model_statistics, is.list, logical(1))))
  expect_null(x[["model"]])
})

test_that("failed or absent GLM diagnostics cannot become successful checks", {
  fit <- glm(mpg ~ wt, data = mtcars)
  fit$converged <- FALSE
  fit$boundary <- TRUE
  fit$null.deviance <- 0
  x <- .check_fit(fit)
  expect_identical(check_fit_status(x, "convergence"), "issue")
  expect_identical(check_fit_status(x, "boundary"), "review")
  expect_true(is.na(x$model_statistics$deviance_explained))
  fit$converged <- NULL
  expect_identical(check_fit_status(.check_fit(fit), "convergence"), "not_assessed")
  d <- transform(mtcars, wt_copy = wt)
  aliased <- glm(mpg ~ wt + wt_copy, data = d)
  expect_identical(check_fit_status(.check_fit(aliased), "finite_coefficients"), "review")
  aliased$coefficients[1L] <- Inf
  expect_identical(check_fit_status(.check_fit(aliased), "finite_coefficients"), "issue")
  expect_error(.check_fit(lm(mpg ~ wt, mtcars)), "supported")
})

test_that("negative-binomial dispersion status remains explicit", {
  skip_if_not_installed("MASS")
  fit <- MASS::glm.nb(Days ~ Sex + Age, data = MASS::quine)
  x <- .check_fit(fit)
  expect_identical(check_fit_status(x, "dispersion"), "no_flag")
  expect_identical(check_fit_status(x, "dispersion_convergence"), "no_flag")
  fit$theta <- NA_real_; fit$th.warn <- "iteration limit reached"
  x <- .check_fit(fit)
  expect_identical(check_fit_status(x, "dispersion"), "not_assessed")
  expect_identical(check_fit_status(x, "dispersion_convergence"), "issue")
  expect_match(x$checks$message[x$checks$check == "dispersion_convergence"], "iteration limit")
})

test_that("GAM native explained deviance and adjusted R-squared remain distinct", {
  skip_if_not_installed("mgcv")
  fit <- mgcv::gam(mpg ~ s(wt, k = 5), data = mtcars, method = "REML")
  native <- summary(fit)
  x <- .check_fit(fit)
  expect_identical(x$backend, "GAM")
  expect_equal(x$model_statistics$deviance_explained, native$dev.expl)
  expect_equal(x$model_statistics$adjusted_R2, native$r.sq)
  expect_identical(check_fit_status(x, "outer_convergence"), "no_flag")
  expect_identical(check_fit_status(x, "basis_dimension"), "not_assessed")
  fit$outer.info$conv <- "step failed"
  expect_identical(check_fit_status(.check_fit(fit), "outer_convergence"), "issue")
  fit$outer.info <- NULL
  expect_identical(check_fit_status(.check_fit(fit), "outer_convergence"), "not_assessed")
})

test_that("lme4 preserves native singularity and optimizer distinctions", {
  skip_if_not_installed("lme4", "1.1.36")
  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy, REML = FALSE)
  original <- fit
  x <- .check_fit(fit)
  expect_identical(x$backend, "lme4")
  expect_identical(check_fit_status(x, "convergence"), "no_flag")
  expect_identical(check_fit_status(x, "singularity"), "no_flag")
  expect_identical(check_fit_status(x, "finite_uncertainty"), "no_flag")
  expect_equal(x$model_statistics$AIC, AIC(fit))
  expect_true(is.na(x$model_statistics$deviance_explained))
  expect_identical(fit, original)
  restricted <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy, REML = TRUE)
  restricted_summary <- .check_fit(restricted)$model_statistics
  expect_true(is.na(restricted_summary$deviance))
  expect_match(restricted_summary$method, "REML")
  expect_match(restricted_summary$notes, "not a response deviance")
  fit@optinfo$conv$opt <- 1L
  expect_identical(check_fit_status(.check_fit(fit), "convergence"), "issue")
  fit@optinfo$conv$opt <- NULL
  expect_identical(check_fit_status(.check_fit(fit), "convergence"), "not_assessed")
  fit@optinfo$conv$opt <- 0L
  fit@optinfo$conv$lme4$messages <- "Model failed to converge with max|grad|"
  expect_identical(check_fit_status(.check_fit(fit), "convergence"), "issue")
})

test_that("glmmTMB numerical checks preserve objective state and component boundaries", {
  skip_if_not_installed("glmmTMB")
  fit <- glmmTMB::glmmTMB(count ~ spp + (1 | site), data = glmmTMB::Salamanders,
    family = poisson())
  state <- fit$obj$env$last.par.best; rng <- .Random.seed
  x <- .check_fit(fit)
  expect_identical(x$backend, "glmmTMB")
  expect_identical(check_fit_status(x, "convergence"), "no_flag")
  expect_identical(check_fit_status(x, "positive_definite_hessian"), "no_flag")
  expect_identical(check_fit_status(x, "finite_uncertainty"), "no_flag")
  expect_identical(check_fit_status(x, "gradient"), "not_assessed")
  expect_equal(x$model_statistics$AIC, AIC(fit))
  expect_identical(fit$obj$env$last.par.best, state)
  expect_identical(.Random.seed, rng)
  reloaded <- unserialize(serialize(fit, NULL))
  expect_false(.check_fit_live_objective(reloaded$obj))
  before <- reloaded$obj$env$last.par.best
  testthat::local_mocked_bindings(residuals.glmmTMB = function(...) stop("No native residual/prediction reinitialisation allowed"),
    .package = "glmmTMB")
  reloaded_check <- .check_fit(reloaded)
  expect_true(is.na(reloaded_check$model_statistics$deviance))
  expect_match(reloaded_check$model_statistics$notes, "not reinitialised")
  expect_equal(reloaded_check$model_statistics$AIC, AIC(reloaded))
  expect_identical(check_fit_status(reloaded_check, "native_objective_availability"), "not_assessed")
  expect_false(.check_fit_live_objective(reloaded$obj))
  expect_identical(reloaded$obj$env$last.par.best, before)
  fit$sdr$pdHess <- FALSE
  expect_identical(check_fit_status(.check_fit(fit), "positive_definite_hessian"), "issue")
  fit$sdr$pdHess <- NULL
  expect_identical(check_fit_status(.check_fit(fit), "positive_definite_hessian"), "not_assessed")
  fit$sdr$cov.fixed[1L, 1L] <- NA_real_
  expect_identical(check_fit_status(.check_fit(fit), "finite_uncertainty"), "issue")
  zi <- glmmTMB::glmmTMB(count ~ spp, ziformula = ~1,
    data = glmmTMB::Salamanders, family = poisson())
  expect_true(is.na(.check_fit(zi)$model_statistics$deviance))
  expect_match(.check_fit(zi)$model_statistics$notes, "combined zero-inflated")
})

test_that("spatial native checks use stored fields and never fit null models", {
  set.seed(762)
  d <- data.frame(x = runif(75)); d$y <- rpois(75, exp(.4 + d$x))
  skip_if_not_installed("sdmTMB")
  s <- sdmTMB::sdmTMB(y ~ x, d, family = poisson(), spatial = "off", silent = TRUE)
  state <- s$tmb_obj$env$last.par.best; rng <- .Random.seed
  x <- .check_fit(s)
  expect_identical(x$backend, "sdmTMB")
  expect_equal(x$model_statistics$logLik, as.numeric(logLik(s)))
  expect_equal(x$model_statistics$deviance, deviance(s))
  expect_identical(check_fit_status(x, "convergence"), "no_flag")
  expect_identical(s$tmb_obj$env$last.par.best, state)
  expect_identical(.Random.seed, rng)
  reloaded <- unserialize(serialize(s, NULL))
  expect_false(.check_fit_live_objective(reloaded$tmb_obj))
  before <- reloaded$tmb_obj$env$last.par.best
  testthat::local_mocked_bindings(residuals.sdmTMB = function(...) stop("No saved objective evaluation allowed"),
    .package = "sdmTMB")
  reloaded_check <- .check_fit(reloaded)
  expect_true(is.na(reloaded_check$model_statistics$deviance))
  expect_match(reloaded_check$model_statistics$notes, "not reinitialised")
  expect_equal(reloaded_check$model_statistics$AIC, AIC(reloaded))
  expect_identical(check_fit_status(reloaded_check, "native_objective_availability"), "not_assessed")
  expect_false(.check_fit_live_objective(reloaded$tmb_obj))
  expect_identical(reloaded$tmb_obj$env$last.par.best, before)
  s$gradients <- NULL; s$bad_eig <- NULL
  missing <- .check_fit(s)
  expect_identical(check_fit_status(missing, "gradient"), "not_assessed")
  expect_identical(check_fit_status(missing, "eigenvalues"), "not_assessed")
  s$gradients <- c(a = .002); s$bad_eig <- TRUE
  flagged <- .check_fit(s)
  expect_identical(check_fit_status(flagged, "gradient"), "review")
  expect_identical(check_fit_status(flagged, "eigenvalues"), "issue")
  skip_if_not_installed("tinyVAST")
  t <- tinyVAST::tinyVAST(y ~ x, data = d, family = poisson(), spatial_domain = NULL,
    control = tinyVAST::tinyVASTcontrol(calculate_deviance_explained = FALSE))
  state <- t$obj$env$last.par.best; rng <- .Random.seed
  testthat::local_mocked_bindings(deviance_explained = function(...) stop("No null-model fit allowed"),
    .package = "tinyVAST")
  x <- .check_fit(t)
  expect_identical(x$backend, "tinyVAST")
  expect_equal(x$model_statistics$AIC, AIC(t))
  expect_equal(x$model_statistics$BIC, -2 * as.numeric(logLik(t)) + log(nrow(d)) * attr(logLik(t), "df"))
  expect_equal(x$model_statistics$deviance, t$rep$deviance)
  expect_true(is.na(x$model_statistics$deviance_explained))
  expect_match(x$model_statistics$notes, "null model")
  expect_identical(t$obj$env$last.par.best, state)
  expect_identical(.Random.seed, rng)
  t$deviance_explained <- .67
  expect_equal(.check_fit(t)$model_statistics$deviance_explained, .67)
  t$sdrep$pdHess <- NULL
  t$internal$convergence_diagnostics$final_grads <- numeric()
  t$internal$convergence_diagnostics$bad_eig <- NULL
  t$convergence_diagnostics$bad_eig <- NULL
  x <- .check_fit(t)
  expect_identical(check_fit_status(x, "positive_definite_hessian"), "not_assessed")
  expect_identical(check_fit_status(x, "gradient"), "not_assessed")
  expect_identical(check_fit_status(x, "eigenvalues"), "not_assessed")
})

test_that("brms reports compact fixtures as unavailable and reuses only saved R-squared", {
  fit <- structure(list(data = data.frame(y = 1:8), criteria = list(
    bayes_R2 = matrix(c(.7, .04), nrow = 1L,
      dimnames = list(NULL, c("Estimate", "Est.Error"))))), class = "brmsfit")
  x <- .check_fit(fit)
  expect_identical(x$backend, "brms")
  expect_equal(x$model_statistics$bayes_R2, .7)
  expect_equal(x$model_statistics$bayes_R2_sd, .04)
  expect_equal(x$model_statistics$nobs, 8)
  expect_true(is.na(x$model_statistics$AIC))
  for (check in c("rhat", "bulk_ess", "tail_ess", "divergences"))
    expect_identical(check_fit_status(x, check), "not_assessed")
  fit$criteria$bayes_R2 <- matrix(.5, nrow = 2L, ncol = 2L,
    dimnames = list(NULL, c("Estimate", "Est.Error")))
  expect_true(is.na(.check_fit(fit)$model_statistics$bayes_R2))
  skip_if_not_installed("brms")
  original <- readRDS(system.file("extdata", "brms-fixtures", "fit2.rds", package = "influ2"))
  expect_identical(check_fit_status(.check_fit(original), "rhat"), "not_assessed")
})

test_that("brms native convergence thresholds and failures remain explicit without sampling", {
  fit <- structure(list(fit = TRUE, data = data.frame(y = 1:8)), class = "brmsfit")
  mock_summary <- list(fixed = data.frame(Rhat = c(1, 1.02), Bulk_ESS = c(600, 390), Tail_ESS = c(500, 800)))
  mock_nuts <- data.frame(Parameter = rep("divergent__", 2), Value = c(0, 1))
  testthat::local_mocked_bindings(.check_fit_backend_available = function(...) TRUE,
    .check_fit_brms_summary = function(...) mock_summary,
    .check_fit_brms_nuts = function(...) mock_nuts)
  x <- .check_fit(fit)
  expect_identical(check_fit_status(x, "rhat"), "review")
  expect_identical(check_fit_status(x, "bulk_ess"), "review")
  expect_identical(check_fit_status(x, "tail_ess"), "no_flag")
  expect_identical(check_fit_status(x, "divergences"), "issue")
  mock_summary$fixed$Rhat[] <- 1
  mock_summary$fixed$Bulk_ESS[] <- 600
  mock_nuts$Value[] <- 0
  expect_identical(check_fit_status(.check_fit(fit), "rhat"), "no_flag")
  expect_identical(check_fit_status(.check_fit(fit), "divergences"), "no_flag")
  mock_summary$fixed$Rhat[1L] <- NA_real_
  expect_identical(check_fit_status(.check_fit(fit), "rhat"), "not_assessed")
  mock_summary <- NULL; mock_nuts <- NULL
  expect_identical(check_fit_status(.check_fit(fit), "bulk_ess"), "not_assessed")
  expect_identical(check_fit_status(.check_fit(fit), "divergences"), "not_assessed")
})

test_that("saved complete brms fits provide native sampler checks without prediction", {
  skip_if_not_installed("brms")
  skip_if_not_installed("rstan")
  fit <- readRDS(test_path("fixtures", "brms-implied", "gaussian.rds"))
  set.seed(413); rng <- .Random.seed
  original <- fit
  testthat::local_mocked_bindings(bayes_R2 = function(...) stop("No new Bayesian R-squared calculation"),
    posterior_predict = function(...) stop("No new posterior prediction"),
    loo = function(...) stop("No new LOO calculation"), .package = "brms")
  x <- .check_fit(fit)
  expect_true(is.finite(x$checks$statistic[x$checks$check == "rhat"]))
  expect_true(is.finite(x$checks$statistic[x$checks$check == "bulk_ess"]))
  expect_true(is.finite(x$checks$statistic[x$checks$check == "tail_ess"]))
  expect_true(is.finite(x$checks$statistic[x$checks$check == "divergences"]))
  expect_true(is.na(x$model_statistics$bayes_R2))
  expect_identical(fit, original)
  expect_identical(.Random.seed, rng)
})

test_that("optional backend absence and RNG restoration are explicit", {
  fit <- structure(list(converged = TRUE, coefficients = 1, model = data.frame(y = 1:3)), class = c("gam", "glm"))
  testthat::local_mocked_bindings(.check_fit_backend_available = function(...) FALSE)
  seed <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) .Random.seed else NULL
  if (!is.null(seed)) rm(".Random.seed", envir = .GlobalEnv)
  on.exit(if (!is.null(seed)) assign(".Random.seed", seed, envir = .GlobalEnv), add = TRUE)
  x <- .check_fit(fit)
  expect_identical(check_fit_status(x, "backend_availability"), "not_assessed")
  expect_identical(check_fit_status(x, "finite_uncertainty"), "not_assessed")
  expect_match(x$model_statistics$notes, "unavailable")
  expect_true(is.na(x$model_statistics$AIC))
  expect_false(exists(".Random.seed", .GlobalEnv, inherits = FALSE))
})

test_that("missing saved source data remain unavailable without call reconstruction", {
  fit <- glm(mpg ~ wt, data = mtcars, model = FALSE)
  original <- fit
  x <- .check_fit(fit)
  expect_equal(x$model_statistics$nobs, nobs(fit))
  expect_identical(check_fit_status(x, "convergence"), "no_flag")
  expect_identical(fit, original)
  expect_null(fit$model)
  skip_if_not_installed("brms")
  compact <- structure(list(criteria = list()), class = "brmsfit")
  x <- .check_fit(compact)
  expect_true(is.na(x$model_statistics$nobs))
  expect_identical(check_fit_status(x, "rhat"), "not_assessed")
})
