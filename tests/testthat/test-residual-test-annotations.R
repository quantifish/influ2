residual_test_args <- function(kind = "distribution") {
  case <- residual_baseline_case(kind)
  args <- list(simulations = case$simulations, data = case$data,
    response = "response", year = "year", response_kind = case$adapter$response_kind,
    conditioning = "Fixed supplied response simulations", component = case$adapter$component,
    seed = 501L, batch_size = 7L, residual_method = "dharma",
    integer_response = kind %in% c("distribution", "bernoulli", "grouped"))
  if (kind %in% c("bernoulli", "grouped")) {
    args$data$probability <- case$adapter$probability
    args$data$trials <- case$adapter$trials
    args$probability <- "probability"
    args$probability_conditioning <- "Declared fixed probabilities"
    args$trial_counts <- "trials"
  }
  args
}

test_that("residual test options are explicit and cannot relabel default PIT", {
  expect_identical(.resid_test_options(c("none", "standard", "full"), "simulation_pit"), "none")
  expect_identical(.resid_test_options("standard", "dharma"), "standard")
  expect_error(.resid_test_options("standard", "simulation_pit"), "not DHARMa")
  expect_error(.resid_test_options("full", "simulation_pit"), "not DHARMa")
  expect_error(.resid_test_options("unsupported", "dharma"), "arg")
  expect_error(influ_residuals("not a fit", residual_tests = "standard"), "not DHARMa")
  args <- residual_test_args()
  args$residual_method <- "simulation_pit"
  args$integer_response <- NULL
  args$residual_tests <- "standard"
  expect_error(do.call(as_influ_residuals, args), "not DHARMa")
})

test_that("no-tests defaults preserve compact DHARMa output exactly", {
  skip_if_not_installed("DHARMa", "0.4.7")
  args <- residual_test_args()
  implicit <- do.call(as_influ_residuals, args)
  args$residual_tests <- "none"
  explicit <- do.call(as_influ_residuals, args)
  expect_identical(explicit, implicit)
  expect_null(explicit$tests)
  expect_identical(plot(explicit)$patches$annotation, plot(explicit, annotations = "none")$patches$annotation)
  for (type in c("qq", "fitted", "year", "distribution")) {
    expect_identical(plot(explicit, type = type)$labels,
      plot(explicit, type = type, annotations = "none")$labels)
  }
})

test_that("standard saved tests agree with native DHARMa and restore state", {
  skip_if_not_installed("DHARMa", "0.4.7")
  args <- residual_test_args()
  args$dharma_storage <- "memory"
  unchanged <- do.call(as_influ_residuals, args)
  args$residual_tests <- "standard"
  set.seed(30)
  rng <- .Random.seed
  device <- grDevices::dev.cur()
  input <- args
  x <- do.call(as_influ_residuals, args)
  expect_identical(.Random.seed, rng)
  expect_identical(grDevices::dev.cur(), device)
  expect_identical(args, input)
  expect_identical(x$observations, unchanged$observations)
  expect_identical(x$qq, unchanged$qq)
  expect_identical(x$ecdf, unchanged$ecdf)
  expect_null(x$dharma)
  expect_s3_class(x$tests, "data.frame")
  expect_true(all(x$tests$status %in% c("no_flag", "review", "not_assessed")))
  expect_true(all(grepl("not calibrated", x$tests$message)))
  expect_true(all(grepl("No flag is not proof", x$tests$message)))
  expect_identical(x$metadata$residual_tests$dharma_version, as.character(packageVersion("DHARMa")))
  native <- DHARMa::createDHARMa(args$simulations, args$data$response,
    fittedPredictedResponse = rowMeans(args$simulations), integerResponse = TRUE,
    seed = x$metadata$dharma_seed, method = "PIT")
  ks <- DHARMa::testUniformity(native, plot = FALSE)
  dispersion <- DHARMa::testDispersion(native, type = "DHARMa", plot = FALSE)
  categorical <- .resid_test_categorical(native, factor(args$data$year))
  find <- function(name) x$tests[x$tests$check == name, , drop = FALSE]
  expect_equal(find("uniformity")$statistic, unname(ks$statistic))
  expect_equal(find("uniformity")$p_value, ks$p.value)
  expect_equal(find("dispersion")$statistic, unname(dispersion$statistic))
  expect_equal(find("dispersion")$p_value, dispersion$p.value)
  expect_equal(find("year_uniformity")$p_value, categorical$uniformity$p.value)
  expect_equal(find("year_uniformity")$p_adjusted,
    p.adjust(categorical$uniformity$p.value, method = "holm"))
  expect_equal(find("year_homogeneity")$statistic, categorical$homogeneity$`F value`[1L])
  expect_equal(find("year_homogeneity")$df, categorical$homogeneity$Df[1L])
  expect_equal(find("year_homogeneity")$p_value, categorical$homogeneity$`Pr(>F)`[1L])
  retained <- function(z) is.matrix(z) || is.function(z) || is.environment(z) ||
    (is.list(z) && any(vapply(z, retained, logical(1))))
  expect_false(retained(x))
  expect_identical(.resid_dharma_test_table(native, x, "standard"), x$tests)
  expect_identical(.Random.seed, rng)
  bad <- x
  bad$observations$pit[1L] <- 0
  expect_error(.resid_dharma_test_table(native, bad), "does not match")
  native$refit <- TRUE
  expect_error(.resid_dharma_test_table(native, x), "non-refitted")
  native$refit <- FALSE
  native$observedResponse[1L] <- 100
  expect_error(.resid_dharma_test_table(native, x), "response data")
})

test_that("full tests use bootstrap for counts without retaining diagnostic fits", {
  skip_if_not_installed("DHARMa", "0.4.7")
  args <- residual_test_args()
  args$dharma_storage <- "memory"
  args$residual_tests <- "full"
  set.seed(52)
  rng <- .Random.seed
  x <- do.call(as_influ_residuals, args)
  expect_identical(.Random.seed, rng)
  expect_true(all(c("quantiles", "outliers") %in% x$tests$check))
  expect_identical(x$metadata$residual_tests$outlier_type, "bootstrap")
  expect_equal(x$metadata$residual_tests$outlier_nboot, ncol(args$simulations))
  expect_identical(x$metadata$residual_tests$refit, FALSE)
  outlier <- x$tests[x$tests$check == "outliers", , drop = FALSE]
  expect_match(outlier$method, "bootstrapped")
  expect_match(outlier$message, "no model refit")
  expect_true(is.na(outlier$df))
  expect_null(x$dharma)
  args <- residual_test_args("continuous")
  args$dharma_storage <- "memory"
  args$residual_tests <- "full"
  y <- do.call(as_influ_residuals, args)
  expect_identical(y$metadata$residual_tests$outlier_type, "binomial")
  expect_match(y$tests$method[y$tests$check == "outliers"], "binomial")
})

test_that("sparse years and incomplete tests are visibly unassessed", {
  skip_if_not_installed("DHARMa", "0.4.7")
  args <- residual_test_args()
  args$data$year <- c(2009, rep(2010, 29L))
  args$residual_tests <- "standard"
  x <- do.call(as_influ_residuals, args)
  sparse <- x$tests[x$tests$term == "2009", , drop = FALSE]
  expect_identical(sparse$status, "not_assessed")
  expect_true(is.na(sparse$p_value))
  expect_identical(x$tests$status[x$tests$check == "year_homogeneity"], "not_assessed")
  args$data$year <- 2010
  x <- do.call(as_influ_residuals, args)
  expect_identical(x$tests$status[x$tests$check == "year_homogeneity"], "not_assessed")
  args$residual_tests <- "full"
  args$simulations[,] <- 2
  x <- do.call(as_influ_residuals, args)
  expect_identical(x$tests$status[x$tests$check == "quantiles"], "not_assessed")
  expect_match(x$tests$message[x$tests$check == "quantiles"], "distinct predictive")
})

test_that("year label named response cannot change a categorical test scope", {
  skip_if_not_installed("DHARMa", "0.4.7")
  args <- residual_test_args()
  args$data$year <- rep(c("response", "other", "third"), each = 10L)
  args$residual_tests <- "standard"
  x <- do.call(as_influ_residuals, args)
  d <- x$tests[x$tests$check == "year_uniformity" & x$tests$term == "response", , drop = FALSE]
  expect_identical(d$scope, "term")
  expect_equal(nrow(x$tests[x$tests$scope == "overall", , drop = FALSE]), 2L)
})

test_that("native failures are disclosed rather than accepted or substituted", {
  skip_if_not_installed("DHARMa", "0.4.7")
  args <- residual_test_args()
  args$residual_tests <- "standard"
  local_mocked_bindings(.resid_test_categorical = function(...) stop("Native categorical failure"))
  x <- do.call(as_influ_residuals, args)
  d <- x$tests[x$tests$check %in% c("year_uniformity", "year_homogeneity"), , drop = FALSE]
  expect_identical(d$status, rep("not_assessed", 2L))
  expect_true(all(is.na(d$p_value)))
  expect_true(all(grepl("Native categorical failure", d$message)))
  expect_true(all(grepl("not a successful check", d$action)))
  run <- .resid_test_run(function() {
    warning("Recorded native warning")
    message("Recorded native message")
    5
  })
  expect_identical(run$value, 5)
  expect_true(any(grepl("Recorded native warning", run$notes)))
  expect_true(any(grepl("Recorded native message", run$notes)))
  expect_null(run$error)
})

test_that("annotated plotting formats stored tests without repeating calculations", {
  skip_if_not_installed("DHARMa", "0.4.7")
  args <- residual_test_args()
  args$residual_tests <- "standard"
  x <- do.call(as_influ_residuals, args)
  before <- x
  set.seed(100)
  rng <- .Random.seed
  local_mocked_bindings(.resid_dharma_test_table = function(...) stop("Do not recalculate!"))
  for (type in c("qq", "fitted", "year", "distribution", "pit_ecdf", "pit_ecdf_diff")) {
    if (grepl("^pit_", type) && !.resid_bayesplot_available()) next
    p <- plot(x, type = type, annotations = "tests")
    expect_s3_class(p, "ggplot")
    expect_match(p$labels$subtitle, if (type == "year") "Holm" else if (type == "fitted") "not calculated" else "DHARMa")
  }
  p <- plot(x, annotations = "tests")
  expect_s3_class(p, "patchwork")
  expect_match(p$patches$annotation$caption, "not calibrated model acceptance")
  p <- ggplot2::autoplot(x, annotations = "all")
  expect_match(p$patches$annotation$subtitle, "Model statistics not stored")
  expect_identical(.Random.seed, rng)
  expect_identical(x, before)
  plain <- x
  plain$tests <- NULL
  expect_match(plot(plain, type = "qq", annotations = "tests")$labels$subtitle, "Tests not stored")
  expect_match(plot(plain, type = "qq", annotations = "model")$labels$caption, "Model statistics not stored")
  model <- x
  model$model_statistics <- data.frame(nobs = 30, deviance_explained = 0.5)
  expect_match(plot(model, type = "qq", annotations = "model")$labels$caption, "Explained deviance = 50%")
  expect_match(plot(model, annotations = "model")$patches$annotation$subtitle, "Explained deviance = 50%")
  zero <- model
  zero$tests$p_value[zero$tests$check == "dispersion"] <- 0
  expect_match(plot(zero, type = "distribution", annotations = "tests")$labels$subtitle,
    "Monte Carlo p = 0")
  threshold <- x
  threshold$metadata$residual_tests$alpha <- 0.1
  threshold$tests$p_adjusted[threshold$tests$check == "year_uniformity"] <- c(0.08, 0.2, 0.9)
  p <- plot(threshold, type = "year", annotations = "tests")
  expect_match(p$labels$subtitle, "1/3 years")
  expect_match(p$labels$subtitle, "KS p < 0.1", fixed = TRUE)
  threshold$metadata$residual_tests$alpha <- NULL
  expect_match(plot(threshold, type = "year", annotations = "tests")$labels$subtitle,
    "0/3 years with Holm-adjusted KS p < 0.05", fixed = TRUE)
  expect_error(plot(x, annotations = "wrong"), "arg")
})

test_that("annotated overview wraps long captions without changing defaults", {
  skip_if_not_installed("DHARMa", "0.4.7")
  args <- residual_test_args()
  args$residual_tests <- "standard"
  x <- do.call(as_influ_residuals, args)
  x$metadata$scheme <- paste(rep("Long backend simulation conditioning description with fitted latent effects and joint posterior dependence", 5L), collapse = " ")
  original <- .resid_panel_caption(x, .resid_select_panels(x, NULL, "auto"))
  expect_identical(plot(x)$patches$annotation$caption, original)
  for (annotations in c("tests", "model", "all")) {
    caption <- plot(x, annotations = annotations)$patches$annotation$caption
    lines <- strsplit(caption, "\n", fixed = TRUE)[[1L]]
    expect_lte(max(nchar(lines)), 110L)
    expect_match(paste(lines, collapse = " "), "joint posterior dependence")
    if (annotations != "model") {
      expect_true(any(grepl("Stored DHARMa p-values", lines)))
      expect_match(paste(lines, collapse = " "), "not calibrated model acceptance tests")
    }
  }
})
