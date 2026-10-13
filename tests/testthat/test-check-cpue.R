check_cpue_fixture <- function() {
  d <- data.frame(year = factor(rep(2010:2013, each = 20)),
    x = seq(-1, 1, length.out = 80), y = rep(0:3, length.out = 80))
  list(data = d, model = glm(y ~ year + x, family = poisson(), data = d))
}

test_that("quick checks expose statistics and every term without simulation", {
  f <- check_cpue_fixture()
  before <- f$model
  set.seed(71); rng <- .Random.seed
  local_mocked_bindings(influ_residuals = function(...) stop("No simulation!"))
  x <- check_cpue(f$model, simulate = FALSE)
  expect_s3_class(x, "influ_check")
  expect_equal(x$model_statistics$AIC, AIC(f$model))
  expect_setequal(x$term_inventory$term, c("year", "x"))
  expect_true(all(x$term_checks$status == "not_assessed"))
  expect_null(x$residuals)
  expect_null(x$predictors)
  expect_identical(f$model, before)
  expect_identical(.Random.seed, rng)
  expect_match(paste(capture.output(print(x)), collapse = "\n"), "not proof")
  expect_named(summary(x), c("model_statistics", "overall", "terms", "term_inventory", "metadata"))
  expect_equal(nrow(as.data.frame(x)), nrow(x$overall) + nrow(x$term_checks))
  expect_s3_class(plot(x, type = "status"), "ggplot")
  expect_error(plot(x), "no residuals")
  expect_equal(check_cpue(f$model, simulate = FALSE, terms = "x")$term_inventory$term, "x")
})

test_that("the report reuses residuals and is compact, serialisable, and state-safe", {
  f <- check_cpue_fixture()
  r <- influ_residuals(f$model, nsim = 30, seed = 44)
  set.seed(50); rng <- .Random.seed
  before <- r
  local_mocked_bindings(influ_residuals = function(...) stop("Do not simulate again!"))
  x <- check_cpue(f$model, residuals = r)
  expect_identical(x$residuals$observations, r$observations)
  expect_identical(x$residuals$qq, r$qq)
  expect_identical(r, before)
  expect_identical(.Random.seed, rng)
  expect_match(x$metadata$residual_provenance, "caller remains")
  expect_match(x$overall$message[x$overall$check == "residual_tests"], "No native")
  expect_s3_class(plot(x, annotations = "model"), "patchwork")
  expect_s3_class(ggplot2::autoplot(x, type = "term", term = "x"), "ggplot")
  expect_s3_class(plot(x, type = "term", term = "year"), "ggplot")
  expect_match(plot(x, type = "term", term = "x")$labels$caption, "conditioning")
  expect_identical(unserialize(serialize(x, NULL)), x)
  unsafe <- function(z) is.environment(z) || is.function(z) || typeof(z) == "externalptr" ||
    (is.list(z) && any(vapply(z, unsafe, logical(1))))
  expect_false(unsafe(x))
  augmented <- r
  augmented$attached_model <- f$model
  expect_null(check_cpue(f$model, residuals = augmented)$residuals$attached_model)
  augmented$metadata$hidden <- new.env()
  expect_error(check_cpue(f$model, residuals = augmented), "hidden model")
  expect_error(plot(x, type = "term"), "Specify")
  expect_error(plot(x, type = "term", term = "bad"), "missing")
  expect_error(check_cpue(f$model, residuals = r, conditioning = "new_effects"), "disagrees")
  expect_error(check_cpue(f$model, residuals = r, residual_method = "dharma"), "disagrees")
})

test_that("native saved TMB objectives are not initialised by the report", {
  skip_if_not_installed("sdmTMB")
  d <- check_cpue_fixture()$data
  fit <- sdmTMB::sdmTMB(y ~ year + x, data = d, family = poisson(),
    spatial = "off", spatiotemporal = "off", silent = TRUE)
  saved <- unserialize(serialize(fit, NULL))
  before <- saved$tmb_obj$env$last.par.best
  local_mocked_bindings(influ_residuals = function(...) stop("No pointer initialisation!"))
  x <- check_cpue(saved)
  expect_match(x$metadata$residual_error, "native objective")
  expect_null(x$residuals)
  expect_false(.check_fit_live_objective(saved$tmb_obj))
  expect_identical(saved$tmb_obj$env$last.par.best, before)
})

test_that("new native DHARMa checks annotate only stored results", {
  skip_if_not_installed("DHARMa", "0.4.7")
  f <- check_cpue_fixture()
  set.seed(61); rng <- .Random.seed
  x <- check_cpue(f$model, nsim = 30, seed = 62, alpha = 0.1)
  expect_identical(.Random.seed, rng)
  expect_identical(x$residuals$metadata$residual_method, "dharma")
  expect_identical(x$residuals$metadata$residual_tests$alpha, 0.1)
  expect_identical(attr(x$residuals$tests, "test_metadata")$alpha, 0.1)
  expect_true(all(c("uniformity", "dispersion") %in% x$overall$check))
  expect_true(all(c("year_homogeneity", "year_uniformity") %in% x$term_checks$check))
  expect_null(x$residuals$dharma)
  expect_match(x$metadata$residual_provenance, "calculated directly")
  local_mocked_bindings(.resid_dharma_test_table = function(...) stop("No new tests!"))
  p <- plot(x)
  expect_s3_class(p, "patchwork")
  expect_match(p$patches$annotation$subtitle, "AIC")
  expect_match(p$patches$annotation$caption, "not calibrated")
  z <- check_cpue(f$model, residuals = x$residuals)
  expect_identical(z$residuals$observations, x$residuals$observations)
  expect_identical(.Random.seed, rng)
})

test_that("missing or refused residual calculation returns an honest partial report", {
  f <- check_cpue_fixture()
  local_mocked_bindings(influ_residuals = function(...) stop("Backend-specific residual simulation unavailable"))
  x <- check_cpue(f$model)
  expect_null(x$residuals)
  expect_match(x$metadata$residual_error, "unavailable")
  expect_true(all(x$term_checks$status == "not_assessed"))
  bad <- f$model
  bad$converged <- FALSE
  x <- check_cpue(bad)
  expect_match(x$metadata$residual_error, "skipped")
  expect_true(any(x$overall$status == "issue"))
  expect_identical(check_cpue(bad, simulate = FALSE)$metadata$residual_error, NULL)
  expect_error(check_cpue(f$model, simulate = NA), "TRUE or FALSE")
  expect_error(check_cpue(f$model, alpha = 1), "alpha")
  expect_error(check_cpue(f$model, terms = "missing"), "Unknown")
  expect_error(check_cpue(f$model, year = "missing"), "year")
  expect_error(check_cpue(f$model, conditioning = "unknown"), "conditioning")
  expect_error(check_cpue(f$model, response_transform = "bad"), "response_transform")
  expect_error(check_cpue(f$model, residual_method = "simulation_pit", residual_tests = "standard"), "requires")
  expect_error(check_cpue(f$model, dharma_max_mb = 0), "dharma_max_mb")
  expect_error(check_cpue(f$model, seed = 2.5), "integer")
  expect_error(check_cpue(f$model, trial_counts = "missing"), "trial_counts")
  expect_error(check_cpue(f$model, residuals = list()), "influ_residuals")
})

test_that("source guards reject unsafe IDs, encodings, and changed data", {
  f <- check_cpue_fixture()
  r <- influ_residuals(f$model, nsim = 20, seed = 5)
  bad <- r; bad$metadata$backend <- "brms"
  expect_error(check_cpue(f$model, residuals = bad), "backend")
  bad <- r; bad$observations <- bad$observations[1:20, ]
  expect_error(check_cpue(f$model, residuals = bad), "every fitted row")
  bad <- r; bad$observations$row[1] <- "missing"
  expect_error(check_cpue(f$model, residuals = bad), "observation IDs")
  bad <- r; bad$observations$observed[1] <- 7
  expect_error(check_cpue(f$model, residuals = bad), "observed responses")
  changed <- f$data; changed$x[1] <- 10
  expect_error(check_cpue(f$model, data = changed, residuals = r), "does not match")
  d <- transform(f$data, positive = y + 1)
  fit <- glm(log(positive) ~ year + x, data = d)
  r <- influ_residuals(fit, data = d, nsim = 20, seed = 5)
  expect_s3_class(check_cpue(fit, data = d, residuals = r), "influ_check")
  bad <- r; bad$metadata$response <- "positive"
  bad$observations$observed <- d$positive
  expect_error(check_cpue(fit, data = d, residuals = bad), "response")
})

test_that("binomial counts, factors, and known trials retain safe reuse", {
  d <- check_cpue_fixture()$data
  d$success <- rep(0:4, length.out = nrow(d)); d$trials <- 5
  fits <- list(cbind = glm(cbind(success, trials - success) ~ year + x, data = d, family = binomial()),
    weighted = glm(I(success / trials) ~ year + x, data = d, weights = trials, family = binomial()),
    factor = glm(factor(success > 2) ~ year + x, data = d, family = binomial()))
  for (name in names(fits)) {
    r <- influ_residuals(fits[[name]], data = d, nsim = 20, seed = 5,
      trial_counts = if (name == "weighted") "trials" else NULL)
    expect_s3_class(check_cpue(fits[[name]], data = d, residuals = r), "influ_check")
    expect_identical(r$observations, check_cpue(fits[[name]], data = d, residuals = r)$residuals$observations)
    bad <- r; bad$observations$trials[1] <- 2
    expect_error(check_cpue(fits[[name]], data = d, residuals = bad), "trials")
  }
})

test_that("retained DHARMa objects are copied into a matrix-free report", {
  skip_if_not_installed("DHARMa", "0.4.7")
  f <- check_cpue_fixture()
  r <- influ_residuals(f$model, nsim = 20, residual_method = "dharma", retain_dharma = TRUE)
  original <- r
  x <- check_cpue(f$model, residuals = r)
  expect_null(x$residuals$dharma)
  expect_identical(r, original)
  expect_match(x$residuals$metadata$retention, "no model or observation")
  expect_lt(as.numeric(object.size(x$residuals)), as.numeric(object.size(r)))
})

test_that("glmmTMB term views do not silently become auxiliary component checks", {
  skip_if_not_installed("glmmTMB")
  d <- check_cpue_fixture()$data
  fit <- glmmTMB::glmmTMB(y ~ year + x, ziformula = ~ x, family = poisson(), data = d)
  x <- check_cpue(fit, nsim = 30, residual_method = "simulation_pit")
  expect_true(any(x$term_checks$component == "zero_inflation"))
  expect_error(plot(x, type = "term", term = "x"), "ambiguous")
  expect_error(plot(x, type = "term", term = "x", component = "zero_inflation"), "does not assess")
  expect_s3_class(plot(x, type = "term", term = "x", component = "conditional"), "ggplot")
})
