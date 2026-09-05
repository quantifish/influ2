test_that("GLM diagnostics reproduce the frozen Bentley fixture", {
  fixture <- bentley_fixture()
  diagnostic <- influ(fixture$model, focus = "year")

  current <- subset(
    influ_effects(diagnostic),
    term %in% c("area", "vessel") & scale == "link",
    select = c(focus, level, term, estimate)
  )
  names(current)[4] <- "link_influence"
  comparison <- merge(
    fixture$reference,
    current,
    by = c("focus", "level", "term"),
    suffixes = c("_legacy", "_new")
  )

  expect_equal(
    comparison$link_influence_new,
    comparison$link_influence_legacy,
    tolerance = 1e-10
  )
  expect_equal(
    exp(comparison$link_influence_new),
    comparison$natural_influence,
    tolerance = 1e-10
  )
})

test_that("GLM diagnostics agree with Bentley proto on the lobster example", {
  skip_if_not_installed("proto")
  data("lobsters_per_pot", package = "influ2", envir = environment())

  model <- stats::glm(
    lobsters ~ year + month + stats::poly(depth, 3) + stats::poly(soak, 3),
    family = stats::poisson(link = "log"),
    data = lobsters_per_pot
  )
  # The legacy method refits reduced models with update(), so keep the data
  # self-contained in the stored call rather than relying on a test frame.
  model$call$data <- lobsters_per_pot
  legacy_environment <- new.env(parent = globalenv())
  legacy_environment$proto <- proto::proto
  sys.source(
    system.file("legacy", "influ-proto.R", package = "influ2"),
    envir = legacy_environment
  )
  legacy_diagnostic <- legacy_environment$Influence$new(
    model = model,
    data = lobsters_per_pot,
    response = "lobsters",
    focus = "year"
  )
  legacy_diagnostic$calc()

  legacy <- stats::reshape(
    as.data.frame(legacy_diagnostic$influences),
    direction = "long",
    varying = names(legacy_diagnostic$influences)[-1],
    v.names = "link_influence_legacy",
    timevar = "term",
    times = names(legacy_diagnostic$influences)[-1]
  )
  current <- subset(
    influ_effects(influ(model, focus = "year")),
    term != "year" & scale == "link",
    select = c(level, term, estimate)
  )
  names(current)[3] <- "link_influence_new"
  comparison <- merge(
    legacy[c("level", "term", "link_influence_legacy")],
    current,
    by = c("level", "term")
  )

  expect_equal(nrow(comparison), 54L)
  expect_equal(
    comparison$link_influence_new,
    comparison$link_influence_legacy,
    tolerance = 1e-10
  )

  legacy_metrics <- subset(
    legacy_diagnostic$summary,
    term %in% c("month", "stats::poly(depth, 3)", "stats::poly(soak, 3)"),
    select = c(term, overall, trend)
  )
  current_metrics <- influ_metrics(influ(model, focus = "year"))
  current_metrics <- stats::reshape(
    current_metrics[c("term", "metric", "estimate")],
    direction = "wide",
    idvar = "term",
    timevar = "metric"
  )
  names(current_metrics) <- sub("^estimate\\.", "", names(current_metrics))
  metric_comparison <- merge(legacy_metrics, current_metrics, by = "term")
  expect_equal(metric_comparison$overall.x, metric_comparison$overall.y, tolerance = 1e-10)
  expect_equal(metric_comparison$trend.x, metric_comparison$trend.y, tolerance = 1e-10)
})

test_that("influ_diag has a stable compact schema", {
  fixture <- bentley_fixture()
  diagnostic <- influ(fixture$model, focus = "year")

  expect_s3_class(diagnostic, "influ_diag")
  expect_s3_class(diagnostic, "influ_diag_glm")
  expect_named(
    influ_effects(diagnostic),
    c(
      "focus", "level", "term", "component", "scale", "estimate",
      "std_error", "lower", "upper", "method"
    )
  )
  expect_true(all(c("nominal", "standardised") %in% diagnostic$indices$series))
  expect_null(diagnostic$model)
  expect_null(influ_draws(diagnostic))
  expect_true(all(c("overall", "trend") %in% influ_metrics(diagnostic)$metric))
  expect_lt(as.numeric(object.size(diagnostic)), as.numeric(object.size(fixture$model)))
})

test_that("uncertainty calculation and retention are separate", {
  fixture <- bentley_fixture()

  none <- influ(fixture$model, focus = "year", uncertainty = "none")
  expect_true(all(is.na(none$influence$std_error)))
  expect_identical(none$uncertainty$method, "none")

  simulated <- influ(
    fixture$model,
    focus = "year",
    uncertainty = "simulation",
    retain = "derived_draws",
    ndraws = 100,
    seed = 42
  )
  expect_equal(nrow(influ_draws(simulated)), 100)
  expect_gt(ncol(influ_draws(simulated)), 0)
  expect_identical(simulated$retained$mode, "derived_draws")
  expect_true(all(is.finite(influ_metrics(simulated)$std_error)))

  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  disk <- influ(
    fixture$model,
    focus = "year",
    uncertainty = "simulation",
    retain = "disk",
    draws_path = path,
    ndraws = 20,
    seed = 42
  )
  expect_null(influ_draws(disk))
  expect_true(file.exists(disk$retained$path))
  expect_equal(nrow(readRDS(disk$retained$path)), 20)
})

test_that("frequentist preparation permits unavailable covariance", {
  fixture <- bentley_fixture()
  matrix <- stats::model.matrix(fixture$model)
  prepared <- influ2:::.prepare_frequentist_matrix(
    fixture$model,
    matrix,
    influ2:::.glm_term_columns(fixture$model, matrix),
    V = NULL
  )
  expect_null(prepared$vcov)
  expect_equal(ncol(prepared$X), length(prepared$beta))
})

test_that("an explicit prediction grid controls the reference distribution", {
  fixture <- bentley_fixture()
  observed <- influ(fixture$model, focus = "year")
  reference <- fixture$data[rep(1L, 4L), , drop = FALSE]
  reference$year <- factor(
    levels(fixture$data$year)[1],
    levels = levels(fixture$data$year)
  )
  reference$area <- factor(
    levels(fixture$data$area)[1],
    levels = levels(fixture$data$area)
  )
  reference$vessel <- factor(
    levels(fixture$data$vessel)[1],
    levels = levels(fixture$data$vessel)
  )
  grid <- influ(
    fixture$model,
    focus = "year",
    reference_data = reference,
    reference_weights = c(1, 1, 1, 8)
  )

  expect_identical(grid$metadata$reference, "prediction_grid")
  expect_equal(grid$metadata$n_reference, 4L)
  expect_false(isTRUE(all.equal(
    subset(observed$influence, term == "area" & scale == "link")$estimate,
    subset(grid$influence, term == "area" & scale == "link")$estimate
  )))
})

test_that("supported families deliberately exclude quasi and extended families", {
  expect_setequal(
    influ_families()$family,
    c(
      "gaussian", "binomial", "poisson", "negative_binomial",
      "lognormal", "gamma", "tweedie"
    )
  )
  expect_error(
    influ(stats::glm(cbind(1, 1) ~ 1, family = stats::quasibinomial()), focus = "x"),
    "Unsupported family"
  )
  expect_error(influ2:::.normalise_family_name("gengamma"), "Unsupported family")
})

test_that("backend family aliases map to the initial supported families", {
  aliases <- c(
    normal = "gaussian",
    bernoulli = "binomial",
    nbinom1 = "negative_binomial",
    nbinom2 = "negative_binomial",
    negbinomial = "negative_binomial",
    lognormal = "lognormal",
    Gamma = "gamma",
    tweedie = "tweedie"
  )

  observed <- vapply(
    names(aliases),
    function(x) influ2:::.normalise_family_name(x)$family,
    character(1)
  )
  expect_identical(unname(observed), unname(aliases))

  hurdle <- influ2:::.normalise_family_name("hurdle_nbinom2")
  expect_identical(hurdle$family, "negative_binomial")
  expect_identical(hurdle$structure, "hurdle")

  zero_inflated <- influ2:::.normalise_family_name("zero_inflated_poisson")
  expect_identical(zero_inflated$family, "poisson")
  expect_identical(zero_inflated$structure, "zero_inflated")
})

test_that("standard plotting methods use the common object", {
  diagnostic <- influ(bentley_fixture()$model, focus = "year")
  expect_s3_class(plot(diagnostic), "ggplot")
  expect_s3_class(plot(diagnostic, type = "index"), "ggplot")
  expect_s3_class(plot(diagnostic, type = "cdi", term = "area"), "patchwork")
  expect_s3_class(ggplot2::autoplot(diagnostic), "ggplot")
})

test_that("influ_diag validation rejects malformed intervals", {
  diagnostic <- influ(bentley_fixture()$model, focus = "year")
  diagnostic$influence$lower[1] <- diagnostic$influence$upper[1] + 1
  expect_error(
    influ2:::validate_influ_diag(diagnostic),
    "lower bounds must not exceed upper bounds"
  )
})

test_that("summary handles non-finite influence estimates", {
  diagnostic <- influ(bentley_fixture()$model, focus = "year")
  diagnostic$influence$estimate[diagnostic$influence$term == "area"] <- NA_real_
  observed <- summary(diagnostic)$term_summary
  observed <- observed[observed$term == "area", , drop = FALSE]

  expect_true(is.na(observed$maximum_absolute_link_influence))
  expect_true(is.na(observed$level_at_maximum))
})
