dharma_external_args <- function(kind = "distribution") {
  case <- residual_baseline_case(kind)
  args <- list(simulations = case$simulations, data = case$data,
    response = "response", year = "year", response_kind = case$adapter$response_kind,
    conditioning = "Fixed joint test simulations", component = case$adapter$component,
    seed = 601L, batch_size = 7L, groups = "fleet", residual_method = "dharma",
    integer_response = kind %in% c("distribution", "bernoulli", "grouped"))
  if (kind %in% c("bernoulli", "grouped")) {
    args$data$p <- case$adapter$probability
    args$data$trials <- case$adapter$trials
    args$probability <- "p"
    args$probability_conditioning <- "Fixed probabilities"
    args$trial_counts <- "trials"
  }
  args
}

test_that("DHARMa calculates its own residuals for aligned supplied simulations", {
  skip_if_not_installed("DHARMa", "0.4.7")
  for (kind in c("distribution", "continuous", "combined", "positive", "bernoulli", "grouped")) {
    args <- dharma_external_args(kind)
    before <- args
    set.seed(21)
    rng <- .Random.seed
    x <- do.call(as_influ_residuals, args)
    expect_identical(.Random.seed, rng)
    expect_identical(args, before)
    native <- DHARMa::createDHARMa(args$simulations, args$data$response,
      fittedPredictedResponse = rowMeans(args$simulations),
      integerResponse = args$integer_response, seed = x$metadata$dharma_seed, method = "PIT")
    expect_identical(x$observations$pit, native$scaledResiduals)
    expect_identical(x$observations$residual,
      residuals(native, quantileFunction = qnorm, outlierValues = x$metadata$normal_score_limits))
    expect_identical(x$observations$simulation_outlier,
      x$observations$pit == 0 | x$observations$pit == 1)
    expect_identical(x$qq$residual, sort(x$observations$residual))
    expect_identical(x$metadata$component, args$component)
    expect_identical(x$metadata$scheme, args$conditioning)
    expect_identical(x$metadata$dharma_method, "PIT")
    expect_identical(x$metadata$dharma_seed, as.integer(args$seed + 104729))
    expect_identical(x$metadata$integer_response, args$integer_response)
    expect_identical(x$observations$row, rownames(args$data))
    expect_null(x$dharma)
    default_args <- args
    default_args$residual_method <- "simulation_pit"
    default_args$integer_response <- NULL
    default <- do.call(as_influ_residuals, default_args)
    expect_identical(x$observations$predicted, default$observations$predicted)
    for (field in c("ecdf", "observed_ecdf", "calibration", "groups")) {
      expect_identical(x[[field]], default[[field]])
    }
    args$batch_size <- 1L
    expect_identical(do.call(as_influ_residuals, args)$observations, x$observations)
    args$batch_size <- ncol(args$simulations)
    expect_identical(do.call(as_influ_residuals, args)$observations, x$observations)
  }
})

test_that("uniform endpoints survive and finite normal displays are explicit", {
  skip_if_not_installed("DHARMa", "0.4.7")
  args <- dharma_external_args("continuous")
  args$data$response[1:2] <- c(-100, 100)
  x <- do.call(as_influ_residuals, args)
  expect_identical(x$observations$pit[1:2], c(0, 1))
  expect_equal(x$observations$residual[1:2], x$metadata$normal_score_limits)
  expect_true(all(is.finite(x$observations$residual)))
  expect_true(all(diff(x$observations$residual[order(x$observations$pit)]) >= 0))
  expect_output(print(x), "DHARMa residual diagnostics")
  expect_output(print(x), "0/1 endpoints")
  expect_match(plot(x)$patches$annotation$caption, "DHARMa PIT")
  for (type in c("qq", "fitted", "year")) {
    p <- plot(x, type = type)
    expect_match(p$labels$y, "DHARMa")
    expect_match(p$labels$caption, "Uniform PIT values remain unchanged")
    expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  }
  expect_s3_class(ggplot2::autoplot(x), "patchwork")
  p <- plot_predicted_residuals(x)
  expect_identical(attr(p, "residual_metadata")$residual_method, "dharma")
  p <- plot_grouped_residuals(x, groups = "fleet", min_n = 2)
  expect_match(p$labels$y, "DHARMa")
  expect_match(p$labels$caption, "0/1 endpoints")
  if (.resid_bayesplot_available()) {
    for (type in c("pit_ecdf", "pit_ecdf_diff")) {
      p <- plot(x, type = type)
      expect_identical(p$labels$x, "DHARMa PIT value")
      built <- ggplot2::ggplot_build(p)$data
      expect_equal(built[[3L]]$y[1L], mean(x$observations$pit == 0))
    }
    expect_s3_class(plot(x, panels = c("qq", "pit_ecdf", "year", "pit_ecdf_diff")), "patchwork")
  }
})

test_that("retention is opt-in and genuine DHARMa operations reuse simulations", {
  skip_if_not_installed("DHARMa", "0.4.7")
  args <- dharma_external_args()
  compact <- do.call(as_influ_residuals, args)
  args$retain_dharma <- TRUE
  full <- do.call(as_influ_residuals, args)
  expect_s3_class(full$dharma, "DHARMa")
  expect_equal(full$dharma$simulatedResponse, unname(args$simulations))
  expect_equal(full$dharma$fittedPredictedResponse, full$observations$predicted)
  expect_identical(full$observations, compact$observations)
  expect_null(full$dharma$fittedModel)
  expect_s3_class(DHARMa::testUniformity(full$dharma, plot = FALSE), "htest")
  expect_s3_class(DHARMa::testDispersion(full$dharma, plot = FALSE), "htest")
  grouped <- DHARMa::recalculateResiduals(full$dharma, group = args$data$year, seed = 41)
  expect_s3_class(grouped, "DHARMa")
  # DHARMa retains duplicate original fields; scaledResiduals are grouped.
  expect_equal(length(grouped$scaledResiduals), length(unique(args$data$year)))
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(full, path)
  restored <- readRDS(path)
  expect_identical(restored$observations, full$observations)
  expect_identical(restored$dharma$scaledResiduals, full$dharma$scaledResiduals)
  expect_s3_class(plot(restored), "patchwork")
  # Compact output must not capture the input matrix or a simulation closure.
  retained <- function(z) is.matrix(z) || is.function(z) || is.environment(z) ||
    (is.list(z) && any(vapply(z, retained, logical(1))))
  expect_false(retained(compact))
  args$retain_dharma <- FALSE
  args$simulations <- args$simulations[, rep(1:24, 50), drop = FALSE]
  colnames(args$simulations) <- NULL
  larger <- do.call(as_influ_residuals, args)
  expect_false(retained(larger))
  expect_lte(as.numeric(object.size(larger)), as.numeric(object.size(compact)) + 2048)
})

test_that("DHARMa requests fail explicitly on dependency, size, or input errors", {
  expect_error(.resid_method_options("osa", FALSE, 256), "arg")
  for (flag in list(NA, 1, logical(), c(TRUE, FALSE))) {
    expect_error(.resid_method_options("simulation_pit", flag, 256), "TRUE or FALSE")
  }
  for (size in list(NA, Inf, 0, -1, "big", c(1, 2))) {
    expect_error(.resid_method_options("simulation_pit", FALSE, size), "positive finite")
  }
  expect_error(.resid_method_options("simulation_pit", TRUE, 256), "requires")
  expect_warning(.resid_dharma_memory(100000, 150, 256), "additional working copies")
  expect_error(.resid_dharma_memory(100000, 500, 256), "exceeding")
  expect_true(.resid_dharma_integer(list(response_kind = "distribution", family = "Negative Binomial(2)")))
  expect_false(.resid_dharma_integer(list(response_kind = "combined", family = c("binomial", "Gamma"))))
  expect_false(.resid_dharma_integer(list(response_kind = "distribution", family = "tweedie")))
  local_mocked_bindings(.resid_dharma_available = function() FALSE)
  expect_error(influ_residuals("not fitted", residual_method = "dharma"), "optional package 'DHARMa'")
  expect_error(as_influ_residuals(NULL, residual_method = "dharma"), "optional package 'DHARMa'")
})

test_that("DHARMa external declarations and allocation failures preserve RNG", {
  skip_if_not_installed("DHARMa", "0.4.7")
  args <- dharma_external_args()
  args$integer_response <- NULL
  expect_error(do.call(as_influ_residuals, args), "Declare `integer_response")
  for (flag in list(NA, 1, c(TRUE, FALSE))) {
    args$integer_response <- flag
    expect_error(do.call(as_influ_residuals, args), "Declare `integer_response")
  }
  args <- dharma_external_args("bernoulli")
  args$integer_response <- NULL
  expect_true(do.call(as_influ_residuals, args)$metadata$integer_response)
  args$integer_response <- FALSE
  expect_error(do.call(as_influ_residuals, args), "require.*TRUE")
  args <- dharma_external_args()
  args$residual_method <- "simulation_pit"
  expect_error(do.call(as_influ_residuals, args), "only used")
  args <- dharma_external_args()
  args$dharma_max_mb <- .001
  set.seed(52)
  rng <- .Random.seed
  expect_error(do.call(as_influ_residuals, args), "exceeding")
  expect_identical(.Random.seed, rng)
  args$dharma_max_mb <- 256
  args$simulations[1, 1] <- Inf
  expect_error(do.call(as_influ_residuals, args), "finite responses")
  expect_identical(.Random.seed, rng)
  local_mocked_bindings(.resid_dharma_calculate = function(...) stop("native failure"))
  args$simulations[1, 1] <- 0
  expect_error(do.call(as_influ_residuals, args), "native failure")
  expect_identical(.Random.seed, rng)
})

test_that("malformed DHARMa output is not replaced by another residual", {
  skip_if_not_installed("DHARMa", "0.4.7")
  local_mocked_bindings(createDHARMa = function(...) list(scaledResiduals = c(0, NA, 1)), .package = "DHARMa")
  expect_error(.resid_dharma_calculate(matrix(0, 3, 20), 1:3, 1:3, TRUE, 1), "invalid scaled residuals")
})

test_that("endpoint placeholders cannot reverse interior tail ordering", {
  skip_if_not_installed("DHARMa", "0.4.7")
  args <- dharma_external_args("continuous")
  for (all_endpoints in c(FALSE, TRUE)) {
    p <- c(0, 1, rep(if (all_endpoints) 0 else 1e-8, nrow(args$data) - 2L))
    local_mocked_bindings(.resid_dharma_calculate = function(...) {
      structure(list(scaledResiduals = p), class = "DHARMa")
    })
    x <- do.call(as_influ_residuals, args)
    expect_true(all(is.finite(x$observations$residual)))
    expect_true(all(diff(x$observations$residual[order(p)]) >= 0))
    interior <- p > 0 & p < 1
    expect_identical(x$observations$residual[interior], qnorm(p[interior]))
    expect_gte(x$metadata$normal_score_limits[2L], qnorm(24.5 / 25))
  }
})

test_that("native frequentist simulators feed the DHARMa bridge unchanged", {
  skip_if_not_installed("DHARMa", "0.4.7")
  set.seed(901)
  d <- expand.grid(year = factor(2010:2013), vessel = factor(1:20))
  d$x <- rnorm(nrow(d))
  d$y <- rpois(nrow(d), exp(.5 + .2 * d$x + rnorm(20, sd = .4)[d$vessel]))
  fits <- list(glm = glm(y ~ year + x, data = d, family = poisson()))
  if (requireNamespace("mgcv", quietly = TRUE)) {
    fits$gam <- mgcv::gam(y ~ year + s(x, k = 4), data = d, family = poisson(), method = "ML")
  }
  if (requireNamespace("glmmTMB", quietly = TRUE)) {
    fits$glmmTMB <- glmmTMB::glmmTMB(y ~ year + x + (1 | vessel), data = d, family = poisson())
    d$n <- 5L
    d$success <- rbinom(nrow(d), d$n, plogis(.2 * d$x))
    fits$binomial <- glmmTMB::glmmTMB(cbind(success, n - success) ~ year + x,
      data = d, family = binomial())
  }
  for (name in names(fits)) {
    f <- fits[[name]]
    settings <- if (inherits(f, "glmmTMB")) f$obj$env$data else NULL
    a <- influ_residuals(f, nsim = 20, batch_size = 7, seed = 46)
    b <- influ_residuals(f, nsim = 20, batch_size = 7, seed = 46,
      residual_method = "dharma", retain_dharma = TRUE)
    replay <- DHARMa::createDHARMa(b$dharma$simulatedResponse,
      b$observations$observed, fittedPredictedResponse = b$observations$predicted,
      integerResponse = TRUE, method = "PIT", seed = b$metadata$dharma_seed)
    expect_identical(b$observations$pit, replay$scaledResiduals)
    expect_identical(a$observations$predicted, b$observations$predicted)
    expect_identical(a$calibration, b$calibration)
    expect_identical(a$metadata$conditioning, b$metadata$conditioning)
    expect_true(b$metadata$integer_response)
    expect_identical(b$observations$row, rownames(model.frame(f)))
    if (inherits(f, "glmmTMB")) {
      expect_identical(f$obj$env$data, settings)
      conditional <- influ_residuals(f, nsim = 20, conditioning = "fitted",
        residual_method = "dharma", retain_dharma = TRUE)
      expect_identical(conditional$metadata$conditioning, "fitted")
      expect_identical(f$obj$env$data, settings)
    }
  }
})

test_that("genuine brms posterior predictions retain the combined-response target", {
  skip_if_not_installed("DHARMa", "0.4.7")
  skip_if_not_installed("brms")
  skip_if_not_installed("rstan")
  m <- readRDS(test_path("fixtures", "brms-implied", "hurdle_gamma.rds"))
  for (component in c("combined", "encounter")) {
    x <- influ_residuals(m, nsim = 20, residual_method = "dharma",
      component = component, retain_dharma = TRUE, seed = 71)
    expect_identical(x$metadata$conditioning, "posterior_predictive")
    expect_identical(x$metadata$component, component)
    expect_identical(x$dharma$integerResponse, component == "encounter")
    expect_identical(x$dharma$scaledResiduals, x$observations$pit)
    response <- m$data[[all.vars(m$formula$formula[[2L]])[1L]]]
    expected <- if (component == "encounter") as.numeric(response > 0) else response
    expect_equal(x$dharma$observedResponse, expected)
  }
})

test_that("spatial backend simulations retain fields and explicit delta components", {
  skip_if_not_installed("DHARMa", "0.4.7")
  skip_if_not_installed("sdmTMB")
  d <- as.data.frame(sdmTMB::pcod_2011)
  mesh <- sdmTMB::make_mesh(d, c("X", "Y"), n_knots = 15)
  f <- sdmTMB::sdmTMB(density ~ depth_scaled, data = d, mesh = mesh,
    time = "year", spatial = "on", spatiotemporal = "iid",
    family = sdmTMB::delta_gamma(), silent = TRUE)
  before <- f$tmb_obj$env$data
  best <- f$tmb_obj$env$last.par.best
  for (component in c("combined", "encounter", "positive")) {
    x <- influ_residuals(f, nsim = 20, component = component,
      residual_method = "dharma", retain_dharma = TRUE, seed = 3)
    expect_identical(x$metadata$component, component)
    expect_identical(x$metadata$conditioning, "fitted")
    expect_identical(x$dharma$integerResponse, component == "encounter")
    expect_identical(x$dharma$scaledResiduals, x$observations$pit)
    if (component == "positive") {
      expect_true(all(x$dharma$observedResponse > 0))
      expect_true(all(x$dharma$simulatedResponse > 0))
      expect_equal(x$observations$row, rownames(d)[d$density > 0])
    }
  }
  x <- influ_residuals(f, nsim = 20, conditioning = "conditional_draw",
    residual_method = "dharma", retain_dharma = TRUE)
  expect_identical(x$metadata$conditioning, "conditional_draw")
  expect_identical(f$tmb_obj$env$data, before)
  expect_identical(f$tmb_obj$env$last.par.best, best)
})

test_that("tinyVAST native joint simulations work without DHARMa model dispatch", {
  skip_if_not_installed("DHARMa", "0.4.7")
  skip_if_not_installed("tinyVAST")
  skip_if_not_installed("fmesher")
  set.seed(7)
  d <- data.frame(year = rep(1:3, each = 80), var = "density", dist = "poisson",
    x = runif(240), ycoord = runif(240))
  d$response <- rpois(240, exp(3 * sin(d$x * 5)))
  mesh <- fmesher::fm_mesh_2d(d[c("x", "ycoord")], cutoff = .2)
  f <- tinyVAST::tinyVAST(response ~ factor(year), data = d,
    family = list(poisson = poisson()), spatial_domain = mesh,
    space_term = "density <-> density, spatial_sd", space_columns = c("x", "ycoord"))
  before <- f$obj$env$data
  best <- f$obj$env$last.par.best
  for (conditioning in c("fitted", "conditional_draw")) {
    x <- influ_residuals(f, nsim = 20, conditioning = conditioning,
      residual_method = "dharma", retain_dharma = TRUE)
    expect_identical(x$metadata$conditioning, conditioning)
    expect_identical(x$metadata$backend, "tinyVAST")
    expect_true(x$dharma$integerResponse)
    expect_identical(x$observations$pit, x$dharma$scaledResiduals)
  }
  expect_identical(f$obj$env$data, before)
  expect_identical(f$obj$env$last.par.best, best)
})

test_that("DHARMa restores an initially absent caller seed", {
  skip_if_not_installed("DHARMa", "0.4.7")
  args <- dharma_external_args()
  seed <- .Random.seed
  on.exit(assign(".Random.seed", seed, envir = .GlobalEnv))
  rm(".Random.seed", envir = .GlobalEnv)
  x <- do.call(as_influ_residuals, args)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  expect_identical(x$metadata$residual_method, "dharma")
})
