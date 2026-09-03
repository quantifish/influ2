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
