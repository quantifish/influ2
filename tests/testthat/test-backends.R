test_that("mgcv GAM terms use the common influence schema", {
  skip_if_not_installed("mgcv")
  fixture <- bentley_fixture()
  model <- mgcv::gam(
    catch ~ year + s(as.numeric(area), k = 3),
    family = stats::poisson(link = "log"),
    data = fixture$data
  )
  diagnostic <- influ(model, focus = "year")

  expect_s3_class(diagnostic, "influ_diag_gam")
  expect_true(any(grepl("^s\\(", diagnostic$influence$term)))
  expect_identical(diagnostic$uncertainty$method, "analytic covariance")
})

test_that("brms posterior projections do not retain observation-level arrays", {
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  fit <- readRDS(test_path("brm1.rds"))

  summary_only <- influ(fit, focus = "year")
  expect_s3_class(summary_only, "influ_diag_brms")
  expect_null(summary_only$draws)
  expect_identical(summary_only$uncertainty$method, "posterior draws")

  retained <- influ(
    fit,
    focus = "year",
    retain = "derived_draws",
    ndraws = 50
  )
  expect_equal(nrow(retained$draws), 50)
  expect_lt(ncol(retained$draws), nrow(fit$data))

  preview <- influ(fit, focus = "year", uncertainty = "none")
  expect_true(all(is.na(preview$influence$std_error)))
})

test_that("glmmTMB separates conditional, zero, and random components", {
  skip_if_not_installed("glmmTMB")
  set.seed(1)
  data <- data.frame(
    year = factor(rep(1:4, each = 40)),
    vessel = factor(rep(1:20, each = 8)),
    x = stats::rnorm(160)
  )
  eta <- 0.2 + as.numeric(data$year) * 0.1 + data$x * 0.2
  data$catch <- ifelse(
    stats::rbinom(160, 1, 0.2) == 1,
    0,
    stats::rpois(160, exp(eta))
  )
  model <- glmmTMB::glmmTMB(
    catch ~ year + x + (1 | vessel),
    ziformula = ~year,
    family = glmmTMB::nbinom2(),
    data = data
  )
  diagnostic <- influ(model, focus = "year")

  expect_s3_class(diagnostic, "influ_diag_glmmtmb")
  expect_true(all(c("conditional", "zero_probability", "random_effects") %in%
    diagnostic$influence$component))
})

test_that("glmmTMB recognises an intercept-only zero-inflation component", {
  skip_if_not_installed("glmmTMB")
  set.seed(11)
  data <- data.frame(
    year = factor(rep(1:3, each = 40)),
    catch = stats::rpois(120, 2)
  )
  data$catch[stats::rbinom(120, 1, 0.15) == 1] <- 0
  model <- glmmTMB::glmmTMB(
    catch ~ year,
    ziformula = ~1,
    family = stats::poisson(),
    data = data
  )

  expect_identical(influ2:::.glmmTMB_structure(model), "zero_inflated")
  expect_s3_class(influ(model, focus = "year"), "influ_diag_glmmtmb")
})

test_that("sdmTMB exposes fixed and spatiotemporal influence components", {
  skip_if_not_installed("sdmTMB")
  data("pcod_2011", package = "sdmTMB")
  data("pcod_mesh_2011", package = "sdmTMB")
  model <- sdmTMB::sdmTMB(
    present ~ as.factor(year) + depth_scaled,
    data = pcod_2011,
    mesh = pcod_mesh_2011,
    family = stats::binomial(),
    time = "year",
    spatial = "on",
    spatiotemporal = "iid",
    silent = TRUE
  )
  diagnostic <- influ(model, focus = "year")

  expect_s3_class(diagnostic, "influ_diag_sdmtmb")
  expect_true(any(diagnostic$influence$term == "spatial_field"))
  expect_true(any(diagnostic$influence$term == "spatiotemporal_field"))
})

test_that("tinyVAST fixed terms use the common influence schema", {
  skip_if_not_installed("tinyVAST")
  set.seed(2)
  data <- data.frame(
    y = stats::rpois(60, 2),
    x = stats::rnorm(60),
    year = factor(rep(1:3, each = 20)),
    time = rep(1:3, each = 20),
    var = "y",
    dist = "poisson"
  )
  model <- tinyVAST::tinyVAST(
    y ~ year + x,
    data = data,
    family = list(poisson = stats::poisson()),
    spatial_domain = NULL
  )
  diagnostic <- influ(model, focus = "year")

  expect_s3_class(diagnostic, "influ_diag_tinyvast")
  expect_setequal(unique(diagnostic$influence$term), c("year", "x"))
})
