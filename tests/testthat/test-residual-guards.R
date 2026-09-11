residual_fixture <- function() {
  set.seed(381)
  data <- expand.grid(
    year = factor(2011:2014), month = factor(1:6), replicate = 1:12
  )
  data$x <- stats::rnorm(nrow(data))
  data$catch <- stats::rnbinom(
    nrow(data), mu = exp(1 + 0.12 * as.numeric(data$year) + 0.3 * data$x),
    size = 2
  )
  data
}

test_that("implied residuals align omitted, excluded, and subset observations", {
  data <- residual_fixture()
  data$x[c(2, 7, 14)] <- NA_real_
  for (action in c("na.omit", "na.exclude")) {
    model <- stats::glm(
      catch ~ year + x, family = stats::poisson(), data = data,
      na.action = get(action, envir = asNamespace("stats"))
    )
    implicit <- plot_grouped_residuals(model, groups = "month", min_n = 1)
    explicit <- plot_grouped_residuals(
      model, data = data, groups = "month", min_n = 1
    )
    permuted <- plot_grouped_residuals(
      model, data = data[nrow(data):1, ], groups = "month", min_n = 1
    )
    expect_equal(implicit$data, explicit$data)
    expect_equal(explicit$data, permuted$data)
    expect_equal(sum(explicit$data$n), stats::nobs(model))
    predicted <- plot_predicted_residuals(model, trend = "none")
    expect_equal(nrow(predicted$data), stats::nobs(model))
    expect_true(all(is.finite(predicted$data$predicted)))
    expect_true(all(is.finite(predicted$data$residual)))
  }
  model <- stats::glm(
    catch ~ year + x, family = stats::poisson(), data = data,
    subset = month != "1", na.action = stats::na.exclude
  )
  implicit <- plot_grouped_residuals(model, groups = "month", min_n = 1)
  explicit <- plot_grouped_residuals(
    model, data = data, groups = "month", min_n = 1
  )
  expect_equal(implicit$data, explicit$data)
  expect_false("1" %in% explicit$data$group)
  expect_equal(sum(explicit$data$n), stats::nobs(model))
})

test_that("implied residuals reject changed data rather than reassigning residuals", {
  data <- residual_fixture()
  model <- stats::glm(
    catch ~ year + poly(x, 2), family = stats::poisson(), data = data
  )
  changed <- data[nrow(data):1, ]
  rownames(changed) <- NULL
  expect_error(
    plot_grouped_residuals(model, data = changed, groups = "month", min_n = 1),
    "does not match"
  )
  changed <- data
  changed$x[1] <- changed$x[1] + 1
  expect_error(
    plot_grouped_residuals(model, data = changed, groups = "month", min_n = 1),
    "does not reproduce"
  )
  changed <- data[-1, ]
  expect_error(
    plot_grouped_residuals(model, data = changed, groups = "month", min_n = 1),
    "original row names"
  )
})

test_that("both helpers require retained fitted observations", {
  data <- residual_fixture()
  model <- glm(catch ~ year + x, family = poisson(), data = data, model = FALSE)
  expect_error(plot_grouped_residuals(model, data = data, groups = "month"), "retained model frame")
  expect_error(plot_predicted_residuals(model), "retained model frame")
})

test_that("reconstructing polynomial bases preserves checked group rows", {
  data <- residual_fixture()
  model <- glm(catch ~ year + poly(x, 2), family = poisson(), data = data)
  checks <- influ_residuals(model, data = data, groups = "month", nsim = 20)
  permuted <- influ_residuals(model, data = data[nrow(data):1, ],
    groups = "month", nsim = 20)
  expect_equal(checks, permuted)
  changed <- data
  changed$x[1] <- changed$x[1] + 1
  expect_error(influ_residuals(model, data = changed, groups = "month", nsim = 20),
    "does not reproduce")
  expect_false(.same_observation_column(c(1, 2), c(1, 2 + 1e-12)))
  expect_true(.same_observation_column(c(1, 2), c(1, 2 + 1e-12), tolerance = 1e-10))
})

test_that("the two helpers reuse exactly the unified residual calculation", {
  data <- residual_fixture()
  models <- list(GLM = glm(catch ~ year + x, data = data, family = poisson()))
  if (requireNamespace("MASS", quietly = TRUE))
    models$NB <- MASS::glm.nb(catch ~ year + x, data = data)
  if (requireNamespace("mgcv", quietly = TRUE))
    models$GAM <- mgcv::gam(catch ~ year + s(x, k = 5), data = data,
      family = mgcv::nb(), method = "REML")
  if (requireNamespace("glmmTMB", quietly = TRUE)) {
    models$glmmTMB <- glmmTMB::glmmTMB(catch ~ year + x + (1 | month),
      data = data, family = glmmTMB::nbinom2())
    models$ZI <- glmmTMB::glmmTMB(catch ~ year + x,
      data = data, family = poisson(), ziformula = ~1)
  }
  if (requireNamespace("sdmTMB", quietly = TRUE))
    models$sdmTMB <- sdmTMB::sdmTMB(catch ~ year + x, data = data,
      family = sdmTMB::nbinom2(), spatial = "off", spatiotemporal = "off", silent = TRUE)
  if (requireNamespace("tinyVAST", quietly = TRUE))
    models$tinyVAST <- tinyVAST::tinyVAST(catch ~ year + x,
      data = transform(data, var = "catch", dist = "poisson", time = as.numeric(year)),
      family = list(poisson = poisson()), spatial_domain = NULL,
      control = tinyVAST::tinyVASTcontrol(calculate_deviance_explained = FALSE))
  for (model in models) {
    checks <- influ_residuals(model, data = if (inherits(model, "tinyVAST")) NULL else data,
      groups = "month", nsim = 20)
    p <- plot_predicted_residuals(checks, trend = "none")
    expect_equal(p$data, checks$observations)
    expect_equal(plot_predicted_residuals(model, nsim = 20)$data, p$data)
    expect_equal(attr(p, "residual_metadata"), checks$metadata)
    grouped <- plot_grouped_residuals(checks, groups = "month", min_n = 1)
    expect_equal(sum(grouped$data$n), nrow(checks$observations))
    expect_false(any(c("implied", "estimate") %in% names(grouped$data)))
    expect_identical(attr(grouped, "residual_metadata")$component, checks$metadata$component)
  }
})

test_that("stored residuals cannot be silently recalculated or relabelled", {
  data <- residual_fixture()
  model <- glm(catch ~ year + x, data = data, family = poisson())
  checks <- influ_residuals(model, data = data, groups = "month", nsim = 20)
  expect_error(plot_predicted_residuals(checks, nsim = 50), "Calculation arguments")
  expect_error(plot_grouped_residuals(checks, groups = "month", nsim = 50), "Calculation arguments")
  expect_error(plot_grouped_residuals(checks, groups = "month", data = data), "original data and year")
  expect_error(plot_grouped_residuals(checks, groups = "month", year = "wrong"), "original data and year")
  expect_error(plot_grouped_residuals(checks, groups = "area"), "lacks the grouping column")
  expect_error(plot_grouped_residuals(checks, groups = c("year", "month")), "one grouping")
  expect_error(plot_grouped_residuals(checks, groups = "month", min_n = 10000), "No year-by-group")
  expect_equal(plot_grouped_residuals(checks, groups = "month", year = "year")$data,
    plot_grouped_residuals(checks, groups = "month")$data)
  checks$groups <- checks$groups[nrow(checks$groups):1, , drop = FALSE]
  expect_error(plot_grouped_residuals(checks, groups = "month"), "not aligned")
  for (bad in list("pearson", "response", "deviance", NA_character_, c("a", "b"))) {
    expect_error(plot_predicted_residuals(model, type = bad), "generalised")
    expect_error(plot_grouped_residuals(model, type = bad), "generalised")
  }
  for (type in c("generalised", "generalized")) {
    expect_s3_class(plot_predicted_residuals(model, type = type, nsim = 20), "ggplot")
  }
  expect_error(plot_predicted_residuals(influ(model, focus = "year")), "not influ_diag")
})

test_that("group retention checks missing, changed, and response-defined columns", {
  data <- residual_fixture()
  model <- glm(catch ~ year + x, data = data, family = poisson())
  for (bad in list(character(), c("month", "month"), NA_character_, "")) {
    expect_error(influ_residuals(model, groups = bad, nsim = 20), "unique original-data")
  }
  expect_error(influ_residuals(model, data = data, groups = "absent", nsim = 20), "must name columns")
  data$month[1] <- NA
  expect_error(influ_residuals(model, data = data, groups = "month", nsim = 20), "without missing")
  expect_error(influ_residuals(model, groups = "catch", nsim = 20), "model response")
  data$bad <- Inf
  expect_error(influ_residuals(model, data = data, groups = "bad", nsim = 20), "finite categorical")
})

test_that("generalised grouping works without a pure year coefficient", {
  data <- residual_fixture()
  model <- glm(catch ~ year * month + x, data = data, family = poisson())
  expect_s3_class(plot_grouped_residuals(model, groups = "month", nsim = 20), "ggplot")
})

test_that("compact brms and multivariate objects cannot fabricate residuals", {
  skip_if_not_installed("brms")
  fit <- readRDS(system.file("extdata", "brms-fixtures", "fit2.rds", package = "influ2"))
  expect_error(plot_predicted_residuals(fit), "Compact brms influence fixtures")
  expect_error(plot_grouped_residuals(fit, groups = "month"), "native fitted object")
  multivariate <- structure(list(formula = structure(list(), class = "mvbrmsformula")), class = "brmsfit")
  expect_error(.check_residual_model(multivariate), "one response")
  mixed <- structure(list(data = data.frame(var = c("a", "b")),
    internal = list(variable_column = "var")), class = "tinyVAST")
  expect_error(.check_residual_model(mixed), "single tinyVAST response")
})

test_that("sparse years are not bridged and singleton bars are absent", {
  data <- residual_fixture()
  fit <- glm(catch ~ year + x, data = data, family = poisson())
  checks <- influ_residuals(fit, data = data, groups = "month", nsim = 20)
  # Editing the stored test object controls sparsity without changing residual values.
  keep <- !(checks$observations$year == "2012" & checks$groups$month == "1")
  checks$observations <- checks$observations[keep, ]
  checks$groups <- checks$groups[keep, , drop = FALSE]
  p <- plot_grouped_residuals(checks, groups = "month", min_n = 1)
  expect_equal(length(unique(p$data$segment[p$data$group == "1"])), 2L)
  one <- !duplicated(checks$observations$year)
  checks$observations <- checks$observations[one, ]
  checks$groups <- checks$groups[one, , drop = FALSE]
  p <- plot_grouped_residuals(checks, groups = "month", min_n = 1)
  expect_true(all(is.na(p$data$std_error)))
})

test_that("group retention does not change simulation, and plots never call native residuals", {
  data <- residual_fixture()
  fit <- glm(catch ~ year + x, data = data, family = poisson())
  testthat::local_mocked_bindings(residuals = function(...) {
    stop("Native residual extraction must not be called")
  }, .package = "stats")
  a <- influ_residuals(fit, nsim = 20, seed = 15)
  b <- influ_residuals(fit, data = data, groups = c("month", "year"), nsim = 20, seed = 15)
  expect_equal(a$observations, b$observations)
  expect_equal(a$qq, b$qq)
  expect_identical(names(b$groups), c("month", "year"))
  expect_s3_class(plot_grouped_residuals(b, groups = "month"), "ggplot")
  expect_s3_class(plot_predicted_residuals(fit, nsim = 20), "ggplot")
  set.seed(491)
  state <- .Random.seed
  plot_grouped_residuals(b, groups = "month")
  plot_predicted_residuals(b)
  expect_identical(.Random.seed, state)
})

test_that("grouped normal scores detect a deliberately omitted catch difference", {
  set.seed(825)
  d <- expand.grid(year = factor(2001:2004), area = c("A", "B"), id = 1:60)
  d$catch <- rpois(nrow(d), ifelse(d$area == "A", 1, 12))
  fit <- glm(catch ~ year, data = d, family = poisson())
  p <- plot_grouped_residuals(fit, data = d, groups = "area", nsim = 100)
  expect_true(all(p$data$residual[p$data$group == "A"] < -0.8))
  expect_true(all(p$data$residual[p$data$group == "B"] > 0.8))
  expect_identical(p$labels$y, "Mean normal-score rank residual")
})

test_that("categorical time labels retain order without forcing numeric years", {
  d <- residual_fixture()
  d$year <- factor(d$year, labels = c("first", "second", "third", "fourth"))
  fit <- glm(catch ~ year + x, data = d, family = poisson())
  p <- plot_grouped_residuals(fit, groups = "month", nsim = 20)
  expect_true(all(p$data$x %in% 1:4))
  expect_identical(p$scales$scales[[2]]$labels, sort(levels(d$year)))
})
