# Keep the disk path accountable to the genuine full-matrix DHARMa path. These
# tests compare numerical results, not retention metadata or method labels.
dharma_stream_args <- function(kind = "distribution") {
  case <- residual_baseline_case(kind)
  args <- list(simulations = case$simulations, data = case$data,
    response = "response", year = "year", response_kind = case$adapter$response_kind,
    conditioning = "Fixed joint streaming-test simulations", component = case$adapter$component,
    seed = 601L, batch_size = 7L, groups = "fleet", residual_method = "dharma",
    integer_response = kind %in% c("distribution", "bernoulli", "grouped"))
  if (kind %in% c("bernoulli", "grouped")) {
    args$data$p <- case$adapter$probability
    args$data$trials <- case$adapter$trials
    args$probability <- "p"
    args$probability_conditioning <- "Fixed test probabilities"
    args$trial_counts <- "trials"
  }
  args
}

dharma_stream_compare <- function(disk, memory, tests = FALSE) {
  for (name in c("observations", "qq", "ecdf", "observed_ecdf", "calibration", "groups")) {
    expect_identical(disk[[name]], memory[[name]], info = name)
  }
  for (name in c("component", "conditioning", "scheme", "response_kind",
      "integer_response", "dharma_seed", "endpoint_count", "normal_score_limits")) {
    expect_identical(disk$metadata[[name]], memory$metadata[[name]], info = name)
  }
  if (tests) {
    skip_if_dharma_equivalent_unvalidated()
    columns <- c("scope", "term", "check", "status", "statistic", "df", "p_value",
      "p_adjusted", "component", "conditioning", "n")
    expect_equal(disk$tests[columns], memory$tests[columns], tolerance = 1e-12)
    expect_true(all(grepl("not calibrated", disk$tests$message)))
  }
}

dharma_stream_has_heavy_storage <- function(z) {
  is.matrix(z) || is.function(z) || is.environment(z) ||
    (is.list(z) && any(vapply(z, dharma_stream_has_heavy_storage, logical(1))))
}

test_that("disk DHARMa preserves native PIT values across tied and continuous inputs", {
  skip_if_not_installed("DHARMa", "0.4.7")
  for (kind in c("distribution", "continuous", "combined", "positive", "bernoulli", "grouped")) {
    args <- dharma_stream_args(kind)
    args$dharma_storage <- "memory"
    memory <- do.call(as_influ_residuals, args)
    input <- args
    for (batch in c(1L, 3L, 7L)) {
      args$batch_size <- batch
      args$dharma_storage <- "memory"
      reference <- do.call(as_influ_residuals, args)
      args$dharma_storage <- "disk"
      # The response matrix is larger than this cap, requiring multiple native
      # getQuantile observation slabs rather than a hidden full-matrix call.
      args$dharma_max_mb <- .006
      set.seed(28)
      rng <- .Random.seed
      device <- grDevices::dev.cur()
      disk <- do.call(as_influ_residuals, args)
      expect_identical(.Random.seed, rng)
      expect_identical(grDevices::dev.cur(), device)
      dharma_stream_compare(disk, reference)
      expect_identical(disk$observations, memory$observations)
      expect_null(disk$dharma)
      expect_false(dharma_stream_has_heavy_storage(disk))
      args$dharma_max_mb <- 256
    }
    expect_identical(input$simulations, args$simulations)
    expect_identical(input$data, args$data)
  }
})

test_that("unvalidated DHARMa versions retain genuine vector checks but disclose missing equivalents", {
  skip_if_not_installed("DHARMa", "0.4.7")
  args <- dharma_stream_args()
  args$residual_tests <- "full"
  args$dharma_storage <- "disk"
  local_mocked_bindings(.resid_dharma_stream_version = function() "9.9.9")
  x <- do.call(as_influ_residuals, args)
  equivalent <- x$tests$check %in% c("dispersion", "outliers")
  expect_identical(x$tests$status[equivalent], rep("not_assessed", 2))
  expect_true(all(is.na(x$tests$p_value[equivalent])))
  expect_true(all(grepl("equivalent", x$tests$method[equivalent])))
  expect_true(all(grepl("memory", x$tests$message[equivalent])))
  expect_true(is.finite(x$tests$p_value[x$tests$check == "uniformity"]))
  expect_true(all(is.finite(x$tests$p_value[x$tests$check == "year_uniformity"])))
  expect_true(is.finite(x$tests$p_value[x$tests$check == "quantiles"]))
})

test_that("PIT randomisation is unaffected by row-slab boundaries or endpoint rows", {
  skip_if_not_installed("DHARMa", "0.4.7")
  args <- dharma_stream_args("continuous")
  args$batch_size <- 1L
  n <- nrow(args$data)
  args$data$response <- rep(c(-100, 100, .25, .5, .75), length.out = n)
  args$simulations <- outer(seq_len(n), seq_len(24L), function(i, j) (i + j) / 50)
  # Include untied rows between tied rows. Native runif(a, a) does not consume
  # an RNG value, so manually advancing an RNG for every row would be wrong.
  args$simulations[seq(3L, n, by = 5L), 1:12] <- .25
  args$simulations[seq(5L, n, by = 5L), 1:8] <- .75
  rownames(args$simulations) <- rownames(args$data)
  args$dharma_storage <- "memory"
  memory <- do.call(as_influ_residuals, args)
  expect_true(any(memory$observations$pit == 0))
  expect_true(any(memory$observations$pit == 1))
  for (maximum in c(.002, .003, .01)) {
    args$dharma_storage <- "disk"
    args$dharma_max_mb <- maximum
    disk <- do.call(as_influ_residuals, args)
    dharma_stream_compare(disk, memory)
    expect_identical(disk$observations$pit,
      DHARMa::createDHARMa(args$simulations, args$data$response,
        fittedPredictedResponse = memory$observations$predicted,
        integerResponse = FALSE, seed = memory$metadata$dharma_seed,
        method = "PIT")$scaledResiduals)
  }
})

test_that("small disk budgets reduce response batches as well as quantile slabs", {
  skip_if_not_installed("DHARMa", "0.4.7")
  args <- dharma_stream_args()
  args$dharma_storage <- "disk"
  args$dharma_max_mb <- .001
  disk <- do.call(as_influ_residuals, args)
  expect_identical(disk$metadata$batch_size, 1L)
  expect_identical(disk$metadata$dharma_row_block_size, 1L)
  expect_lte(disk$metadata$dharma_working_mb, .001)
  expect_gt(disk$metadata$dharma_cache_mb, .001)
  # The ECDF grid is defined by the effective first response batch. Match that
  # batch when comparing with the pre-existing full-matrix path.
  args$dharma_storage <- "memory"
  args$dharma_max_mb <- 256
  args$batch_size <- 1L
  memory <- do.call(as_influ_residuals, args)
  dharma_stream_compare(disk, memory)
})

test_that("disk summaries retain DHARMa standard and full test statistics", {
  skip_if_not_installed("DHARMa", "0.4.7")
  for (kind in c("distribution", "continuous", "combined", "bernoulli", "grouped")) {
    for (mode in c("standard", "full")) {
      args <- dharma_stream_args(kind)
      args$residual_tests <- mode
      args$dharma_storage <- "memory"
      memory <- do.call(as_influ_residuals, args)
      args$dharma_storage <- "disk"
      args$dharma_max_mb <- .006
      set.seed(42)
      rng <- .Random.seed
      disk <- do.call(as_influ_residuals, args)
      dharma_stream_compare(disk, memory, tests = TRUE)
      expect_identical(.Random.seed, rng)
      expect_false(dharma_stream_has_heavy_storage(disk))
      expect_null(disk$dharma)
      if (mode == "full") {
        expect_identical(disk$metadata$residual_tests$outlier_type,
          if (args$integer_response) "bootstrap" else "binomial")
      }
    }
  }
})

test_that("storage choices do not manufacture a partial native DHARMa object", {
  skip_if_not_installed("DHARMa", "0.4.7")
  args <- dharma_stream_args()
  automatic <- do.call(as_influ_residuals, args)
  args$dharma_storage <- "disk"
  explicit_disk <- do.call(as_influ_residuals, args)
  expect_identical(automatic, explicit_disk)
  args$retain_dharma <- TRUE
  expect_error(do.call(as_influ_residuals, args), "retain_dharma.*memory|disk.*retain_dharma")
  args$dharma_storage <- "auto"
  retained <- do.call(as_influ_residuals, args)
  expect_s3_class(retained$dharma, "DHARMa")
  expect_equal(retained$dharma$simulatedResponse, unname(args$simulations))
  expect_identical(retained$observations, automatic$observations)
  expect_identical(retained$dharma$scaledResiduals, automatic$observations$pit)
  args$dharma_storage <- "memory"
  explicit_memory <- do.call(as_influ_residuals, args)
  expect_identical(explicit_memory, retained)
  args$dharma_storage <- "unknown"
  expect_error(do.call(as_influ_residuals, args), "arg")
})

test_that("streamed residuals remain usable after serialisation without a cache", {
  skip_if_not_installed("DHARMa", "0.4.7")
  args <- dharma_stream_args()
  args$dharma_storage <- "disk"
  args$residual_tests <- "full"
  x <- do.call(as_influ_residuals, args)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(x, path)
  restored <- readRDS(path)
  expect_identical(restored, x)
  expect_false(dharma_stream_has_heavy_storage(restored))
  expect_s3_class(plot(restored, annotations = "tests"), "patchwork")
  expect_s3_class(ggplot2::ggplot_build(plot(restored, type = "qq", annotations = "tests")), "ggplot_built")
})

test_that("disk storage never enters the full native object allocation path", {
  skip_if_not_installed("DHARMa", "0.4.7")
  args <- dharma_stream_args()
  args$dharma_storage <- "disk"
  args$residual_tests <- "full"
  local_mocked_bindings(.resid_dharma_calculate = function(...) {
    stop("Unexpected full-matrix helper")
  })
  local_mocked_bindings(createDHARMa = function(...) {
    stop("Unexpected full native object allocation")
  }, .package = "DHARMa")
  x <- do.call(as_influ_residuals, args)
  expect_identical(x$metadata$residual_method, "dharma")
  expect_true(all(c("uniformity", "dispersion", "outliers") %in% x$tests$check))
  expect_null(x$dharma)
  expect_false(dharma_stream_has_heavy_storage(x))
})

test_that("native frequentist backends preserve simulation targets under disk storage", {
  skip_if_not_installed("DHARMa", "0.4.7")
  set.seed(208)
  d <- expand.grid(year = factor(2010:2013), vessel = factor(1:16))
  d$x <- runif(nrow(d), -1, 1)
  vessel <- seq(-.6, .6, length.out = 16L)
  d$y <- rpois(nrow(d), exp(.7 + .4 * d$x + vessel[d$vessel]))
  fits <- list(glm = glm(y ~ year + x, family = poisson(), data = d))
  if (requireNamespace("mgcv", quietly = TRUE)) {
    fits$gam <- mgcv::gam(y ~ year + s(x, k = 4), family = poisson(), data = d, method = "ML")
  }
  if (requireNamespace("glmmTMB", quietly = TRUE)) {
    fits$glmmTMB <- glmmTMB::glmmTMB(y ~ year + x + (1 | vessel), family = poisson(), data = d)
  }
  if (requireNamespace("lme4", quietly = TRUE)) {
    fits$lme4 <- lme4::glmer(y ~ year + x + (1 | vessel), family = poisson(), data = d)
  }
  for (name in names(fits)) {
    f <- fits[[name]]
    conditional <- if (name %in% c("glmmTMB", "lme4")) c("fitted", "new_effects") else "fitted"
    for (conditioning in conditional) {
      args <- list(model = f, nsim = 24L, batch_size = 3L, seed = 70L,
        conditioning = conditioning, residual_method = "dharma", residual_tests = "standard")
      # The first argument is named `model` in the public function.
      args$dharma_storage <- "memory"
      memory <- do.call(influ_residuals, args)
      args$dharma_storage <- "disk"
      args$dharma_max_mb <- .006
      disk <- do.call(influ_residuals, args)
      dharma_stream_compare(disk, memory, tests = TRUE)
      expect_identical(disk$metadata$backend, name)
    }
  }
})

test_that("check_cpue forwards disk budgets without changing overall or term statistics", {
  skip_if_not_installed("DHARMa", "0.4.7")
  set.seed(189)
  d <- data.frame(year = factor(rep(2010:2013, each = 20)),
    x = runif(80, -1, 1))
  d$y <- rpois(80, exp(1 + .3 * d$x))
  fit <- glm(y ~ year + x, family = poisson(), data = d)
  set.seed(23)
  rng <- .Random.seed
  input <- fit
  memory <- check_cpue(fit, nsim = 24L, batch_size = 1L, seed = 82L,
    residual_tests = "full", dharma_storage = "memory")
  disk <- check_cpue(fit, nsim = 24L, batch_size = 1L, seed = 82L,
    residual_tests = "full", dharma_storage = "disk", dharma_max_mb = .003)
  expect_null(disk$metadata$residual_error)
  expect_identical(disk$residuals$metadata$dharma_storage, "disk")
  expect_gt(disk$residuals$metadata$dharma_matrix_mb, .003)
  expect_lte(disk$residuals$metadata$dharma_working_mb, .003)
  dharma_stream_compare(disk$residuals, memory$residuals, tests = TRUE)
  columns <- c("scope", "term", "check", "status", "statistic", "df", "p_value",
    "p_adjusted", "component", "conditioning", "n")
  expect_equal(disk$overall[columns], memory$overall[columns], tolerance = 1e-12)
  expect_equal(disk$term_checks[columns], memory$term_checks[columns], tolerance = 1e-12)
  expect_identical(disk$model_statistics, memory$model_statistics)
  expect_identical(disk$term_inventory, memory$term_inventory)
  expect_identical(fit, input)
  expect_identical(.Random.seed, rng)
  expect_null(disk$residuals$dharma)
  guarded <- check_cpue(fit, nsim = 24L, batch_size = 1L,
    dharma_storage = "memory", dharma_max_mb = .003)
  expect_null(guarded$residuals)
  expect_match(guarded$metadata$residual_error, "exceeding")
  expect_identical(.Random.seed, rng)
  local_mocked_bindings(influ_residuals = function(...) stop("Do not simulate again"))
  reused <- check_cpue(fit, residuals = disk$residuals)
  expect_identical(reused$residuals$observations, disk$residuals$observations)
  expect_identical(reused$residuals$tests, disk$residuals$tests)
  expect_identical(.Random.seed, rng)
})

test_that("brms posterior draws keep their response and component targets on disk", {
  skip_if_not_installed("DHARMa", "0.4.7")
  skip_if_not_installed("brms")
  skip_if_not_installed("rstan")
  for (kind in c("hurdle_gamma", "Gamma")) {
    fit <- readRDS(test_path("fixtures", "brms-implied", paste0(kind, ".rds")))
    parts <- if (kind == "hurdle_gamma") c("combined", "encounter") else "positive"
    for (part in parts) {
      args <- list(model = fit, nsim = 24L, batch_size = 3L, seed = 70L,
        component = part, residual_method = "dharma", residual_tests = "standard",
        dharma_storage = "memory")
      memory <- do.call(influ_residuals, args)
      args$dharma_storage <- "disk"
      args$dharma_max_mb <- .02
      disk <- do.call(influ_residuals, args)
      dharma_stream_compare(disk, memory, tests = TRUE)
      expect_identical(disk$metadata$conditioning, "posterior_predictive")
      expect_identical(disk$metadata$component, part)
      expect_identical(disk$metadata$backend, "brms")
    }
  }
})

test_that("sdmTMB disk diagnostics preserve joint fields and delta components", {
  skip_if_not_installed("DHARMa", "0.4.7")
  skip_if_not_installed("sdmTMB")
  d <- as.data.frame(sdmTMB::pcod_2011)
  mesh <- sdmTMB::make_mesh(d, c("X", "Y"), n_knots = 15)
  fit <- sdmTMB::sdmTMB(density ~ depth_scaled, data = d, mesh = mesh,
    time = "year", spatial = "on", spatiotemporal = "iid",
    family = sdmTMB::delta_gamma(), silent = TRUE)
  before <- fit$tmb_obj$env$data
  parameters <- fit$tmb_obj$env$last.par.best
  for (conditioning in c("fitted", "conditional_draw", "new_effects")) {
    for (component in c("combined", "encounter", "positive")) {
      args <- list(model = fit, nsim = 24L, batch_size = 1L, seed = 39L,
        conditioning = conditioning, component = component,
        residual_method = "dharma", residual_tests = "standard", dharma_storage = "memory")
      memory <- do.call(influ_residuals, args)
      args$dharma_storage <- "disk"
      args$dharma_max_mb <- .04
      disk <- do.call(influ_residuals, args)
      dharma_stream_compare(disk, memory, tests = TRUE)
      expect_identical(disk$metadata$backend, "sdmTMB")
      expect_identical(disk$metadata$component, component)
      expect_identical(disk$metadata$conditioning, conditioning)
      expect_identical(fit$tmb_obj$env$data, before)
      expect_identical(fit$tmb_obj$env$last.par.best, parameters)
    }
  }
})

test_that("tinyVAST disk diagnostics retain the same spatial conditioning", {
  skip_if_not_installed("DHARMa", "0.4.7")
  skip_if_not_installed("tinyVAST")
  skip_if_not_installed("fmesher")
  set.seed(7)
  d <- data.frame(year = rep(1:3, each = 80), var = "density", dist = "poisson",
    x = runif(240), ycoord = runif(240))
  d$response <- rpois(240, exp(3 * sin(d$x * 5)))
  mesh <- fmesher::fm_mesh_2d(d[c("x", "ycoord")], cutoff = .2)
  fit <- tinyVAST::tinyVAST(response ~ factor(year), data = d,
    family = list(poisson = poisson()), spatial_domain = mesh,
    space_term = "density <-> density, spatial_sd", space_columns = c("x", "ycoord"))
  before <- fit$obj$env$data
  parameters <- fit$obj$env$last.par.best
  for (conditioning in c("fitted", "conditional_draw")) {
    args <- list(model = fit, nsim = 24L, batch_size = 3L, seed = 71L,
      conditioning = conditioning, residual_method = "dharma",
      residual_tests = "standard", dharma_storage = "memory")
    memory <- do.call(influ_residuals, args)
    args$dharma_storage <- "disk"
    args$dharma_max_mb <- .02
    disk <- do.call(influ_residuals, args)
    dharma_stream_compare(disk, memory, tests = TRUE)
    expect_identical(disk$metadata$backend, "tinyVAST")
    expect_identical(disk$metadata$conditioning, conditioning)
    expect_identical(fit$obj$env$data, before)
    expect_identical(fit$obj$env$last.par.best, parameters)
  }
})

test_that("the response cache reproduces exact row slabs and cleans up twice safely", {
  skip_if_not_installed("DHARMa", "0.4.7")
  args <- dharma_stream_args()
  cache <- .resid_dharma_cache_new(nrow(args$data), ncol(args$simulations),
    maximum = .006, batch_size = 3L)
  on.exit(.resid_dharma_cache_close(cache))
  path <- cache$path
  expect_true(file.exists(path))
  expect_gt(cache$row_block_size, 0L)
  expect_lt(cache$row_block_size, nrow(args$data))
  expect_lte(cache$working_mb, .006)
  for (first in seq.int(1L, ncol(args$simulations), by = 3L)) {
    ids <- seq.int(first, min(first + 2L, ncol(args$simulations)))
    .resid_dharma_cache_write(cache, args$simulations[, ids, drop = FALSE], ids)
  }
  flush(cache$connection)
  expect_equal(file.info(path)$size, 8 * length(args$simulations))
  for (bounds in list(c(1L, 1L), c(1L, 8L), c(11L, 18L), c(30L, 30L))) {
    expect_identical(.resid_dharma_cache_read(cache, bounds[1L], bounds[2L]),
      unname(args$simulations[seq.int(bounds[1L], bounds[2L]), , drop = FALSE]))
  }
  quantile <- DHARMa::getQuantile
  slab_rows <- integer()
  local_mocked_bindings(getQuantile = function(simulations, observed, ...) {
    slab_rows <<- c(slab_rows, nrow(simulations))
    expect_identical(ncol(simulations), ncol(args$simulations))
    quantile(simulations, observed, ...)
  }, .package = "DHARMa")
  set.seed(84)
  rng <- .Random.seed
  pit <- .resid_dharma_cache_quantiles(cache, args$data$response,
    integer_response = TRUE, seed = 104L)
  expect_identical(.Random.seed, rng)
  expect_equal(sum(slab_rows), nrow(args$data))
  expect_true(all(slab_rows <= cache$row_block_size))
  expect_gt(length(slab_rows), 1L)
  native <- DHARMa::createDHARMa(args$simulations, args$data$response,
    fittedPredictedResponse = rowMeans(args$simulations), integerResponse = TRUE,
    method = "PIT", seed = 104L)
  expect_identical(pit, native$scaledResiduals)
  .resid_dharma_cache_close(cache)
  expect_false(file.exists(path))
  expect_silent(.resid_dharma_cache_close(cache))
  expect_error(.resid_dharma_cache_read(cache, 1L, 1L), "Invalid observation slab")
  expect_error(.resid_dharma_cache_read_column(cache, 1L), "Invalid simulation column")
  expect_error(.resid_dharma_cache_write(cache,
    args$simulations[, 1L, drop = FALSE], 1L), "Invalid.*cache simulation batch")
})

test_that("bounded cache guards reject inadequate budgets and malformed reads or writes", {
  args <- dharma_stream_args()
  expect_error(.resid_dharma_cache_new(nrow(args$data), ncol(args$simulations),
    maximum = .0001, batch_size = 1L), "cannot fit one response column")
  cache <- .resid_dharma_cache_new(nrow(args$data), ncol(args$simulations),
    maximum = .006, batch_size = 3L)
  on.exit(.resid_dharma_cache_close(cache))
  expect_error(.resid_dharma_cache_read(cache, 1L, 1L), "incomplete")
  expect_error(.resid_dharma_cache_read_column(cache, 1L), "incomplete")
  for (ids in list(2L, .5, NA_real_, "1", integer())) {
    expect_error(.resid_dharma_cache_write(cache,
      args$simulations[, 1L, drop = FALSE], ids), "Invalid.*cache simulation batch")
  }
  for (first in seq.int(1L, ncol(args$simulations), by = 3L)) {
    ids <- seq.int(first, min(first + 2L, ncol(args$simulations)))
    .resid_dharma_cache_write(cache, args$simulations[, ids, drop = FALSE], ids)
  }
  for (id in list(0, 25, NA_real_, Inf, .5, c(1, 2), "1")) {
    expect_error(.resid_dharma_cache_read_column(cache, id), "Invalid simulation column")
  }
  for (bounds in list(c(0, 1), c(1, 31), c(2, 1), c(1, 30), c(1, 1.5), c(NA, 1), c(1, Inf))) {
    expect_error(.resid_dharma_cache_read(cache, bounds[1L], bounds[2L]), "Invalid observation slab")
  }
  for (id in c(1L, 12L, 24L)) {
    expect_identical(.resid_dharma_cache_read_column(cache, id),
      unname(args$simulations[, id]))
  }
})

test_that("bad native quantile slabs are rejected rather than silently recycled", {
  skip_if_not_installed("DHARMa", "0.4.7")
  args <- dharma_stream_args()
  args$dharma_storage <- "disk"
  args$dharma_max_mb <- .006
  bad <- .5
  local_mocked_bindings(getQuantile = function(...) bad, .package = "DHARMa")
  set.seed(120)
  rng <- .Random.seed
  for (value in list(.5, rep(NA_real_, 8L), rep(Inf, 8L), rep(-.1, 8L), rep(1.1, 8L), "bad")) {
    bad <- value
    expect_error(do.call(as_influ_residuals, args), "invalid scaled residuals")
    expect_identical(.Random.seed, rng)
  }
})

test_that("one joint simulation pass suffices and failed runs remove temporary caches", {
  skip_if_not_installed("DHARMa", "0.4.7")
  case <- residual_baseline_case("distribution")
  adapter <- case$adapter
  seen <- integer()
  fail <- FALSE
  adapter$simulate <- function(ids) {
    seen <<- c(seen, ids)
    if (fail && min(ids) > 7L) stop("Intentional simulation failure")
    case$simulations[, ids, drop = FALSE]
  }
  adapter$family <- "poisson"
  cache_new <- .resid_dharma_cache_new
  paths <- character()
  local_mocked_bindings(
    .resid_adapter = function(...) adapter,
    .resid_year = function(...) list(name = "year", source = "explicit",
      levels = c("2010", "2012", "2013")),
    .resid_dharma_cache_new = function(...) {
      cache <- cache_new(...)
      paths <<- c(paths, cache$path)
      cache
    })
  args <- list(model = "fixed adapter", data = case$data, year = "year",
    nsim = 24L, batch_size = 7L, seed = 76L, residual_method = "dharma",
    dharma_storage = "disk", residual_tests = "full", dharma_max_mb = .006)
  set.seed(19)
  rng <- .Random.seed
  x <- do.call(influ_residuals, args)
  expect_identical(seen, seq_len(24L))
  expect_identical(.Random.seed, rng)
  expect_false(any(file.exists(paths)))
  expect_false(dharma_stream_has_heavy_storage(x))
  seen <- integer()
  fail <- TRUE
  expect_error(do.call(influ_residuals, args), "Intentional simulation failure")
  expect_identical(.Random.seed, rng)
  expect_false(any(file.exists(paths)))
  fail <- FALSE
  local_mocked_bindings(getQuantile = function(...) stop("Intentional DHARMa failure"),
    .package = "DHARMa")
  expect_error(do.call(influ_residuals, args), "Intentional DHARMa failure")
  expect_identical(.Random.seed, rng)
  expect_false(any(file.exists(paths)))
})
