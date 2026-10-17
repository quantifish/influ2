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

test_that("mgcv Tweedie models use ratio-scale influence", {
  skip_if_not_installed("mgcv")
  suppressPackageStartupMessages(
    library("mgcv", character.only = TRUE)
  )
  on.exit(detach("package:mgcv"), add = TRUE)
  set.seed(8)
  n <- 240
  data <- data.frame(
    year = factor(rep(1:4, each = n / 4)),
    x = stats::runif(n)
  )
  response_mean <- exp(
    0.2 + 0.1 * as.numeric(data$year) + 0.2 * data$x
  )
  data$y <- stats::rgamma(n, shape = 2, scale = response_mean / 2)
  model <- mgcv::gam(
    y ~ year + s(x, k = 4),
    family = mgcv::tw(theta = 1.5, link = "log"),
    method = "REML",
    data = data
  )
  diagnostic <- influ(model, focus = "year")

  expect_identical(diagnostic$family$family, "tweedie")
  expect_true("ratio" %in% diagnostic$influence$scale)
  expect_true(all(is.finite(diagnostic$influence$estimate)))
})

test_that("brms posterior projections do not retain observation-level arrays", {
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  fit <- readRDS(system.file(
    "extdata", "brms-fixtures", "fit2.rds", package = "influ2"
  ))

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

test_that("brms group-level terms retain joint uncertainty compactly", {
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  fit <- readRDS(system.file(
    "extdata", "brms-fixtures", "fit2.rds", package = "influ2"
  ))

  diagnostic <- influ(
    fit,
    focus = "year",
    retain = "derived_draws",
    ndraws = 40
  )

  expect_true(any(diagnostic$influence$term == "month"))
  expect_true(any(grepl("group_level$", diagnostic$influence$component)))
  expect_equal(nrow(diagnostic$draws), 40)
  expect_lt(ncol(diagnostic$draws), nrow(fit$data))
  month_coefficients <- subset(
    diagnostic$coefficients,
    term == "month" & component == "conditional:group_level"
  )
  expect_true(all(is.finite(month_coefficients$lower)))
  expect_true(all(is.finite(month_coefficients$upper)))
  expect_s3_class(
    plot(
      diagnostic,
      type = "cdi",
      term = "month",
      component = "conditional:group_level"
    ),
    "patchwork"
  )
})

test_that("brms smooth bases are projected without observation-by-draw arrays", {
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  fit_path <- system.file(
    "extdata", "brms-fixtures", "fit2.rds", package = "influ2"
  )
  skip_if_not(file.exists(fit_path), "development brms smooth fixture unavailable")
  fit <- readRDS(fit_path)

  diagnostic <- influ(fit, focus = "year", ndraws = 40)

  expect_true(any(grepl("^s\\(depth", diagnostic$influence$term)))
  expect_true(any(grepl("smooth$", diagnostic$influence$component)))
  expect_null(diagnostic$draws)

  reference <- expand.grid(
    year = levels(fit$data$year),
    month = levels(fit$data$month),
    depth = stats::median(fit$data$depth),
    soak = stats::median(fit$data$soak)
  )
  reference$year <- factor(reference$year, levels = levels(fit$data$year))
  reference$month <- factor(reference$month, levels = levels(fit$data$month))
  grid_diagnostic <- influ(
    fit,
    focus = "year",
    reference_data = reference,
    ndraws = 10
  )
  expect_identical(grid_diagnostic$metadata$reference, "prediction_grid")
  expect_true(all(is.finite(grid_diagnostic$influence$estimate)))
})

test_that("brms hurdle components are combined draw by draw", {
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  fit_path <- system.file(
    "extdata", "brms-fixtures", "m1.rds", package = "influ2"
  )
  skip_if_not(file.exists(fit_path), "development brms hurdle fixture unavailable")
  fit <- readRDS(fit_path)

  diagnostic <- influ(
    fit,
    focus = "year",
    retain = "derived_draws",
    ndraws = 40
  )
  components <- unique(diagnostic$influence$component)

  expect_true("positive" %in% components)
  expect_true("unconditional_mean" %in% components)
  expect_equal(nrow(diagnostic$draws), 40)

  positive <- subset(
    diagnostic$influence,
    component == "positive" & term == "year"
  )
  combined <- subset(
    diagnostic$influence,
    component == "unconditional_mean" & term == "year"
  )
  # This fixture uses hu ~ 1, so its combined year influence equals the
  # positive-component influence exactly.
  expect_equal(combined$estimate, positive$estimate, tolerance = 1e-10)
})

test_that("glmmTMB separates conditional, zero, and random components", {
  skip_if_not_installed("glmmTMB")
  set.seed(1)
  data <- expand.grid(
    replicate = seq_len(5),
    vessel = factor(seq_len(20)),
    year = factor(seq_len(4))
  )
  data$x <- stats::rnorm(nrow(data))
  vessel_effect <- stats::rnorm(20, sd = 0.35)
  eta <- 0.2 + as.numeric(data$year) * 0.1 + data$x * 0.2 +
    vessel_effect[data$vessel]
  data$catch <- ifelse(
    stats::rbinom(nrow(data), 1, 0.2) == 1,
    0,
    stats::rnbinom(nrow(data), mu = exp(eta), size = 2)
  )
  model <- glmmTMB::glmmTMB(
    catch ~ year + x + (1 | vessel),
    ziformula = ~year,
    family = glmmTMB::nbinom2(),
    data = data
  )
  diagnostic <- influ(model, focus = "year", ndraws = 30, seed = 1)

  expect_s3_class(diagnostic, "influ_diag_glmmtmb")
  expect_true(all(c(
    "conditional", "zero_probability", "unconditional_mean", "random_effects"
  ) %in%
    diagnostic$influence$component))
  expect_identical(
    diagnostic$uncertainty$method,
    "analytic covariance; joint coefficient simulation"
  )
  random_rows <- subset(
    diagnostic$influence,
    component == "random_effects"
  )
  expect_true(all(is.finite(random_rows$std_error)))
  expect_true(all(random_rows$method == "analytic covariance"))
})

test_that("glmmTMB zero-inflation random effects form their own term", {
  skip_if_not_installed("glmmTMB")
  set.seed(21)
  data <- expand.grid(
    replicate = seq_len(15),
    vessel = factor(seq_len(12)),
    year = factor(seq_len(4))
  )
  data$area <- factor(sample(letters[1:6], nrow(data), TRUE))
  vessel_effect <- stats::rnorm(12, sd = 0.9)
  area_zero <- stats::rnorm(6, sd = 0.8)
  mu <- exp(0.4 + 0.1 * as.numeric(data$year) + vessel_effect[data$vessel])
  zero <- stats::plogis(-1 + area_zero[data$area])
  data$catch <- ifelse(stats::rbinom(nrow(data), 1, zero) == 1, 0,
    stats::rpois(nrow(data), mu))
  model <- glmmTMB::glmmTMB(
    catch ~ year + (1 | vessel),
    ziformula = ~ year + (1 | area),
    family = stats::poisson(),
    data = data
  )
  Z <- glmmTMB::getME(model, "Z")
  Zzi <- glmmTMB::getME(model, "Zzi")
  # getME(model, "b") concatenates both blocks, which used to break influ().
  expect_gt(length(glmmTMB::getME(model, "b")), ncol(Z))

  expect_no_warning(diagnostic <- influ(model, focus = "year"))
  link_rows <- function(part) {
    rows <- diagnostic$influence
    rows[rows$component == part & rows$scale == "link", , drop = FALSE]
  }
  contrast <- function(contribution) {
    unname(as.numeric(tapply(contribution, data$year, mean) - mean(contribution)))
  }

  # Each random-effect term uses its own block of modes.
  expect_equal(link_rows("random_effects")$estimate,
    contrast(as.numeric(Z %*% glmmTMB::ranef(model)$cond$vessel[[1]])),
    tolerance = 1e-10)
  zero_link <- link_rows("zero_probability_random_effects")
  expect_identical(unique(zero_link$term), "zi_random_effects")
  expect_equal(zero_link$estimate,
    contrast(as.numeric(Zzi %*% glmmTMB::ranef(model)$zi$area[[1]])),
    tolerance = 1e-10)
  expect_true(all(is.finite(zero_link$std_error) & zero_link$std_error > 0))

  # Probability differences are evaluated at the full zero-inflation predictor.
  eta <- diagnostic$metadata$glmmTMB$reference_predictor[["zero_inflation"]]
  zero_difference <- subset(diagnostic$influence,
    term == "zi_random_effects" & scale == "difference")
  expect_equal(zero_difference$estimate,
    stats::plogis(eta + zero_link$estimate) - stats::plogis(eta),
    tolerance = 1e-10)
  expect_identical(diagnostic$metadata$glmmTMB$zi_random_cdi_group, "area")
  expect_s3_class(plot(diagnostic, type = "cdi", term = "zi_random_effects"),
    "patchwork")
})

test_that("glmmTMB covariance for zero-inflation random effects matches latent covariance", {
  skip_if_not_installed("glmmTMB")
  set.seed(22)
  data <- expand.grid(replicate = seq_len(15), vessel = factor(seq_len(10)),
    year = factor(seq_len(3)))
  data$area <- factor(sample(letters[1:5], nrow(data), TRUE))
  mu <- exp(0.5 + stats::rnorm(10, sd = 0.9)[data$vessel])
  zero <- stats::plogis(-1 + stats::rnorm(5, sd = 0.8)[data$area])
  data$catch <- ifelse(stats::rbinom(nrow(data), 1, zero) == 1, 0,
    stats::rpois(nrow(data), mu))
  model <- glmmTMB::glmmTMB(catch ~ year + (1 | vessel),
    ziformula = ~ (1 | area), family = stats::poisson(), data = data)

  # The joint-precision route used for the zero-inflation block reproduces
  # glmmTMB's own latent covariance on the conditional block.
  report <- TMB::sdreport(model$obj, par.fixed = model$fit$par,
    getJointPrecision = TRUE)
  index <- which(rownames(report$jointPrecision) == "b")
  joint <- as.matrix(solve(report$jointPrecision))[index, index]
  latent <- influ2:::.glmmTMB_random_covariance(model, length(index))
  expect_gt(max(abs(latent)), 1e-3)
  expect_equal(joint, latent, tolerance = 1e-8, ignore_attr = TRUE)
  zero_covariance <- influ2:::.glmmTMB_zi_random_covariance(model)
  expect_equal(dim(zero_covariance), c(5L, 5L))
  expect_true(all(eigen(zero_covariance, only.values = TRUE)$values > 0))
})

test_that("glmmTMB probability-scale influence uses the full reference predictor", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("lme4")
  set.seed(5)
  data <- expand.grid(replicate = seq_len(20), vessel = factor(seq_len(15)),
    year = factor(seq_len(5)))
  late <- as.numeric(data$vessel) > 7
  sampled <- stats::runif(nrow(data)) < stats::plogis(-1 +
    0.4 * as.numeric(data$year) * ifelse(late, 1, -1))
  data <- data[sampled, ]
  data$y <- stats::rbinom(nrow(data), 1, stats::plogis(-2 +
    0.2 * as.numeric(data$year) + stats::rnorm(15)[data$vessel]))
  tmb <- influ(glmmTMB::glmmTMB(y ~ year + (1 | vessel),
    family = stats::binomial(), data = data), focus = "year", uncertainty = "none")
  lme4_fit <- influ(lme4::glmer(y ~ year + (1 | vessel),
    family = stats::binomial(), data = data), focus = "year", uncertainty = "none")
  probability <- function(x, part) {
    rows <- x$influence
    rows$estimate[rows$component == part & rows$scale == "difference"]
  }

  # Before, glmmTMB used a random-effects-only baseline (p near 0.5) and
  # overstated these probability differences by about a third.
  expect_equal(probability(tmb, "random_effects"),
    probability(lme4_fit, "random_effects"), tolerance = 1e-3)
  expect_equal(probability(tmb, "conditional"),
    probability(lme4_fit, "conditional"), tolerance = 1e-3)
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

test_that("glmmTMB hurdle components use a joint unconditional mean", {
  skip_if_not_installed("glmmTMB")
  set.seed(31)
  n <- 480
  data <- data.frame(
    year = factor(rep(1:4, each = n / 4)),
    x = stats::rnorm(n)
  )
  positive_mean <- exp(0.5 + 0.1 * as.numeric(data$year) + 0.2 * data$x)
  zero_probability <- stats::plogis(
    -0.8 + 0.15 * as.numeric(data$year)
  )
  data$catch <- ifelse(
    stats::rbinom(n, 1, zero_probability),
    0,
    1 + stats::rnbinom(n, mu = positive_mean, size = 2)
  )
  model <- glmmTMB::glmmTMB(
    catch ~ year + x,
    ziformula = ~year,
    family = glmmTMB::truncated_nbinom2(),
    data = data
  )
  diagnostic <- influ(model, focus = "year", ndraws = 100, seed = 1)

  expect_identical(diagnostic$family$structure, "hurdle")
  expect_true(all(c("positive", "occurrence", "unconditional_mean") %in%
    diagnostic$influence$component))
  expect_true(all(
    diagnostic$influence$method[
      diagnostic$influence$component == "unconditional_mean"
    ] == "joint coefficient simulation"
  ))
})

test_that("original data are recovered for transformed mixed-model terms", {
  skip_if_not_installed("glmmTMB")
  set.seed(18)
  data <- data.frame(
    year = factor(rep(1:4, each = 40)),
    month = factor(rep(1:8, each = 20)),
    depth = stats::runif(160, 5, 100)
  )
  month_effect <- stats::rnorm(8, sd = 0.25)
  data$catch <- stats::rpois(
    160,
    exp(0.3 + 0.1 * as.numeric(data$year) +
      month_effect[data$month] + 0.003 * data$depth)
  )
  model <- glmmTMB::glmmTMB(
    catch ~ year + poly(depth, 2) + (1 | month),
    family = stats::poisson(),
    data = data
  )

  recovered <- influ2:::.resolve_influ_data(model)
  diagnostic <- influ(model, focus = "year")

  expect_true(all(c("depth", "month") %in% names(recovered)))
  expect_true("random_effects" %in% diagnostic$influence$component)
  expect_true(all(is.finite(subset(
    diagnostic$influence,
    component == "random_effects"
  )$std_error)))
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
  diagnostic <- influ(model, focus = "year", ndraws = 30, seed = 1)

  expect_s3_class(diagnostic, "influ_diag_sdmtmb")
  reference <- pcod_2011[as.integer(seq(1, nrow(pcod_2011), length.out = 30)), ]
  preview <- influ(model, focus = "year", reference_data = reference, uncertainty = "none")
  observed <- predict(model)
  projected <- predict(model, newdata = reference)
  for (term in c("spatial_field", "spatiotemporal_field")) {
    # Sum the two separately labelled fields and compare their link-scale
    # contrasts with the native total random-field prediction.
    expect_true(term %in% preview$influence$term)
  }
  fields <- subset(preview$influence, scale == "link" & term %in%
    c("spatial_field", "spatiotemporal_field"))
  actual <- tapply(fields$estimate, fields$level, sum)
  expected <- tapply(observed$est_rf, pcod_2011$year, mean) - mean(projected$est_rf)
  expect_equal(actual, expected, tolerance = 1e-7)
  expect_true(all(is.na(fields$std_error)))
  expect_true(any(diagnostic$influence$term == "spatial_field"))
  expect_true(any(diagnostic$influence$term == "spatiotemporal_field"))
  expect_true(all(is.finite(subset(
    diagnostic$influence,
    grepl("field$", term)
  )$std_error)))
  expect_true(all(is.finite(diagnostic$influence$estimate)))
  expect_s3_class(
    plot(
      diagnostic,
      type = "components",
      term = c("spatial_field", "spatiotemporal_field")
    ),
    "ggplot"
  )
})

test_that("sdmTMB reference grids retain smooth and IID contributions", {
  skip_if_not_installed("sdmTMB")
  set.seed(41)
  n <- 400
  data <- data.frame(
    year = factor(sample(2001:2005, n, TRUE)),
    depth = stats::runif(n, 10, 100),
    group = factor(sample(letters[1:6], n, TRUE))
  )
  group_effect <- stats::rnorm(6, sd = 0.4)
  data$y <- stats::rpois(n, exp(0.3 + 0.012 * data$depth -
    0.00012 * data$depth^2 + 0.1 * as.integer(data$year) +
    group_effect[data$group]))
  # The smooth and IID effects are latent, so the joint-draw path is used
  # without needing a spatial field.
  model <- sdmTMB::sdmTMB(y ~ year + s(depth) + (1 | group), data = data,
    family = stats::poisson(), spatial = "off", silent = TRUE)
  link_rows <- function(x) {
    e <- subset(influ_effects(x), scale == "link")
    e[order(e$component, e$term, e$level), c("component", "term", "level", "estimate")]
  }

  # The fitted rows as an explicit grid define the same reference
  # distribution, so every term, including the smooth, must be unchanged.
  observed <- influ(model, focus = "year", uncertainty = "none")
  expect_true(all(c("smooths", "iid_random_effects") %in% observed$influence$term))
  expect_no_warning(gridded <- influ(model, focus = "year",
    uncertainty = "none", reference_data = data))
  expect_equal(link_rows(gridded), link_rows(observed),
    ignore_attr = TRUE, tolerance = 1e-8)

  joint_observed <- influ(model, focus = "year", ndraws = 20, seed = 1)
  joint_gridded <- influ(model, focus = "year", ndraws = 20, seed = 1,
    reference_data = data)
  expect_true(all(c("smooths", "iid_random_effects") %in%
    joint_gridded$influence$term))
  expect_equal(link_rows(joint_gridded), link_rows(joint_observed),
    ignore_attr = TRUE, tolerance = 1e-8)

  # On a grid where only depth varies, the projected smooth must account for
  # all variation in sdmTMB's own projected predictor.
  grid <- data.frame(depth = seq(12, 98, length.out = 30),
    year = factor("2003", levels = levels(data$year)),
    group = factor("c", levels = levels(data$group)))
  report <- model$tmb_obj$report(model$last.par.best)
  smooth <- influ2:::.sdmTMB_reference_smooth(model, grid, report)
  projected <- influ2:::.sdmTMB_reference_contribution(NULL, "smooths", 1L,
    nrow(grid), smooth, influ2:::.sdmTMB_smooth_parameters(model))
  predictor <- stats::predict(model, newdata = grid, return_tmb_object = TRUE)
  remainder <- as.numeric(predictor$obj$report(model$last.par.best)$proj_eta) - projected
  expect_gt(stats::sd(projected), 0.01)
  expect_lt(diff(range(remainder)), 1e-10)

  # A native layout that no longer reproduces eta_smooth_i is not used.
  broken <- model
  broken$tmb_data$b_smooth_start <- broken$tmb_data$b_smooth_start + 1L
  expect_null(influ2:::.sdmTMB_reference_smooth(broken, grid, report))

  expect_null(influ2:::.sdmTMB_reference_contribution(list(), "unknown", 1L, 3L))
  expect_warning(influ2:::.sdmTMB_warn_unprojected("unknown"),
    "unavailable for sdmTMB term\\(s\\): unknown")
})

test_that("sdmTMB delta reference grids retain each component's smooth", {
  skip_if_not_installed("sdmTMB")
  set.seed(43)
  n <- 400
  data <- data.frame(
    year = factor(sample(2001:2005, n, TRUE)),
    depth = stats::runif(n, 10, 100)
  )
  encounter <- stats::plogis(-0.5 + 0.02 * (data$depth - 50))
  data$density <- ifelse(stats::rbinom(n, 1, encounter) == 1,
    stats::rgamma(n, 2, 2 / exp(0.3 + 0.0002 * (data$depth - 40)^2)), 0)
  model <- sdmTMB::sdmTMB(density ~ year + s(depth), data = data,
    family = sdmTMB::delta_gamma(), spatial = "off", silent = TRUE)
  rows <- function(x) {
    e <- subset(influ_effects(x), term == "smooths" & scale == "link")
    e[order(e$component, e$level), c("component", "level", "estimate")]
  }

  observed <- influ(model, focus = "year", uncertainty = "none")
  expect_no_warning(gridded <- influ(model, focus = "year",
    uncertainty = "none", reference_data = data))
  expect_setequal(unique(rows(gridded)$component),
    c("occurrence:smooths", "positive:smooths"))
  expect_equal(rows(gridded), rows(observed), ignore_attr = TRUE,
    tolerance = 1e-8)
})

test_that("sdmTMB delta fixed effects have a joint unconditional mean", {
  skip_if_not_installed("sdmTMB")
  data("pcod_2011", package = "sdmTMB")
  data("pcod_mesh_2011", package = "sdmTMB")
  model <- sdmTMB::sdmTMB(
    density ~ as.factor(year) + depth_scaled,
    data = pcod_2011,
    mesh = pcod_mesh_2011,
    family = sdmTMB::delta_gamma(),
    time = "year",
    spatial = "on",
    spatiotemporal = "iid",
    silent = TRUE
  )
  diagnostic <- influ(
    model,
    focus = "year",
    retain = "derived_draws",
    ndraws = 50,
    seed = 1
  )

  expect_identical(diagnostic$family$structure, "hurdle")
  expect_true(all(c("occurrence", "positive", "unconditional_mean") %in%
    diagnostic$influence$component))
  expect_equal(nrow(diagnostic$draws), 50)
  expect_true(all(c("spatial_field", "spatiotemporal_field") %in%
    subset(
      diagnostic$influence,
      component == "unconditional_mean" & method == "joint precision simulation"
    )$term))
  checks <- influ_residuals(model, nsim = 20, batch_size = 7)
  expect_identical(checks$metadata$response_structure, "combined delta response")
  expect_equal(checks$observations$observed, pcod_2011$density)
  expect_true(all(is.finite(checks$observations$residual)))
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
  diagnostic <- influ(model, focus = "year", ndraws = 30, seed = 1)

  expect_s3_class(diagnostic, "influ_diag_tinyvast")
  expect_setequal(unique(diagnostic$influence$term), c("year", "x"))
})

test_that("tinyVAST exposes fitted spatial and spatiotemporal components", {
  skip_if_not_installed("tinyVAST")
  skip_if_not_installed("fmesher")
  set.seed(2)
  n <- 120
  data <- data.frame(
    x = stats::runif(n),
    ycoord = stats::runif(n),
    time = rep(1:4, each = n / 4)
  )
  data$year <- factor(data$time)
  data$var <- "catch"
  data$dist <- "poisson"
  eta <- 0.3 + 0.08 * data$time +
    0.5 * sin(2 * pi * data$x) +
    0.35 * cos(2 * pi * (data$ycoord + 0.15 * data$time))
  data$catch <- stats::rpois(n, exp(eta))
  mesh <- fmesher::fm_mesh_2d(data[c("x", "ycoord")], n = 25)
  model <- tinyVAST::tinyVAST(
    catch ~ year,
    data = data,
    family = list(poisson = stats::poisson()),
    spatial_domain = mesh,
    space_term = "catch <-> catch, spatial_sd",
    spacetime_term = "catch <-> catch, 0, spatiotemporal_sd",
    space_columns = c("x", "ycoord")
  )
  diagnostic <- influ(model, focus = "year", ndraws = 30, seed = 1)

  expect_true(any(diagnostic$influence$term == "spatial_field"))
  reference <- data[c(3, 17, 42, 61, 79, 99), ]
  preview <- influ(model, focus = "year", reference_data = reference, uncertainty = "none")
  for (term in c("spatial_field", "spatiotemporal_field")) {
    what <- if (term == "spatial_field") "pomega1_g" else "pepsilon1_g"
    observed <- as.numeric(tinyVAST::project(model, newdata = data, what = what,
      extra_times = numeric(0), future_var = FALSE, past_var = FALSE, parm_var = FALSE))
    projected <- as.numeric(tinyVAST::project(model, newdata = reference, what = what,
      extra_times = numeric(0), future_var = FALSE, past_var = FALSE, parm_var = FALSE))
    effect <- preview$influence[preview$influence$term == term & preview$influence$scale == "link", ]
    expected <- tapply(observed, data$year, mean) - mean(projected)
    # Absolute log-scale accuracy: a relative tolerance is unstable for
    # near-zero field contrasts from native sparse numerical projections.
    expect_lt(max(abs(effect$estimate - as.numeric(expected[effect$level]))), 1e-8)
    expect_true(all(is.na(effect$std_error)))
  }
  expect_true(any(diagnostic$influence$term == "spatiotemporal_field"))
  expect_true(any(grepl("latent_fields$", diagnostic$influence$component)))
  expect_true(all(is.finite(subset(
    diagnostic$influence,
    grepl("field$", term)
  )$std_error)))
  expect_true(all(is.finite(diagnostic$influence$estimate)))
  checks <- influ_residuals(model, nsim = 20, batch_size = 7)
  expect_identical(checks$metadata$year, "year")
  expect_equal(checks$observations$observed, data$catch)
  expect_true(all(is.finite(checks$observations$residual)))
  expect_s3_class(
    plot(
      diagnostic,
      type = "components",
      term = c("spatial_field", "spatiotemporal_field")
    ),
    "ggplot"
  )
})

test_that("tinyVAST mixed-family responses use labelled components", {
  skip_if_not_installed("tinyVAST")
  set.seed(24)
  data <- expand.grid(
    year = factor(1:3),
    replicate = seq_len(25),
    var = c("count", "biomass")
  )
  data$time <- as.integer(data$year)
  data$dist <- ifelse(data$var == "count", "poisson", "normal")
  data$x <- stats::rnorm(nrow(data))
  data$response <- ifelse(
    data$var == "count",
    stats::rpois(nrow(data), exp(0.2 + 0.1 * as.integer(data$year))),
    stats::rnorm(nrow(data), 0.2 * as.integer(data$year), 1)
  )
  model <- tinyVAST::tinyVAST(
    response ~ year + x,
    data = data,
    family = list(
      poisson = stats::poisson(),
      normal = stats::gaussian()
    ),
    spatial_domain = NULL
  )
  diagnostic <- influ(model, focus = "year")

  expect_identical(diagnostic$family$structure, "multivariate")
  expect_setequal(
    unique(diagnostic$influence$component),
    c("count:conditional", "biomass:conditional")
  )
  expect_setequal(
    names(diagnostic$metadata$family_by_response),
    c("count", "biomass")
  )
})

test_that("tinyVAST delta fixed effects have a joint unconditional mean", {
  skip_if_not_installed("tinyVAST")
  set.seed(9)
  n <- 300
  data <- data.frame(
    year = factor(rep(1:5, each = n / 5)),
    x = stats::rnorm(n),
    time = rep(1:5, each = n / 5),
    var = "catch",
    dist = "dgamma"
  )
  positive_probability <- stats::plogis(
    -0.5 + 0.15 * as.numeric(data$year)
  )
  positive_mean <- exp(
    0.3 + 0.08 * as.numeric(data$year) + 0.2 * data$x
  )
  data$catch <- ifelse(
    stats::rbinom(n, 1, positive_probability),
    stats::rgamma(n, shape = 2, scale = positive_mean / 2),
    0
  )
  model <- tinyVAST::tinyVAST(
    catch ~ year + x,
    data = data,
    family = list(
      dgamma = tinyVAST::delta_gamma(link1 = "logit")
    ),
    delta_options = list(formula = ~year),
    spatial_domain = NULL
  )
  diagnostic <- influ(
    model,
    focus = "year",
    retain = "derived_draws",
    ndraws = 50,
    seed = 1
  )

  expect_identical(diagnostic$family$structure, "hurdle")
  expect_true(all(c("occurrence", "positive", "unconditional_mean") %in%
    diagnostic$influence$component))
  expect_equal(nrow(diagnostic$draws), 50)
  checks <- influ_residuals(model, nsim = 20, batch_size = 7)
  expect_identical(checks$metadata$response_structure, "combined delta response")
  expect_equal(checks$observations$observed, data$catch)
  expect_true(all(is.finite(checks$observations$residual)))
})
