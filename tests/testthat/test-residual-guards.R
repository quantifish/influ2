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
    implicit <- plot_implied_residuals(model, groups = "month", min_n = 1)
    explicit <- plot_implied_residuals(
      model, data = data, groups = "month", min_n = 1
    )
    permuted <- plot_implied_residuals(
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
  implicit <- plot_implied_residuals(model, groups = "month", min_n = 1)
  explicit <- plot_implied_residuals(
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
    plot_implied_residuals(model, data = changed, groups = "month", min_n = 1),
    "does not match"
  )
  changed <- data
  changed$x[1] <- changed$x[1] + 1
  expect_error(
    plot_implied_residuals(model, data = changed, groups = "month", min_n = 1),
    "does not reproduce"
  )
  changed <- data[-1, ]
  expect_error(
    plot_implied_residuals(model, data = changed, groups = "month", min_n = 1),
    "original row names"
  )
})

test_that("saved model calls cannot substitute for retained fitted observations", {
  data <- residual_fixture()
  model <- stats::glm(
    catch ~ year + x, family = stats::poisson(), data = data,
    subset = month != "1", model = FALSE
  )
  expect_null(.residual_model_frame(model))
  expect_error(
    plot_implied_residuals(model, data = data, groups = "month", min_n = 1),
    "model = TRUE"
  )
  expected <- plot_predicted_residuals(model, trend = "none")
  data <- data[nrow(data):1, ]
  rownames(data) <- NULL
  data$year <- factor(rep(2011:2014, length.out = nrow(data)))
  data$month <- factor(rep("1", nrow(data)), levels = as.character(1:6))
  expect_error(
    plot_implied_residuals(model, data = data, groups = "month", min_n = 1),
    "retained model frame"
  )
  expect_error(
    plot_implied_residuals(model, groups = "month", min_n = 1),
    "retained model frame"
  )
  # Prediction helpers can use native stored values directly; they
  # must not reevaluate the now-mutated data or subset expression.
  actual <- plot_predicted_residuals(model, trend = "none")
  expect_equal(actual$data, expected$data)
  expect_equal(nrow(actual$data), stats::nobs(model))
  expect_equal(unname(.residual_estimate(model)), unname(actual$data$residual))
})

test_that("reconstructing lobster polynomial bases allows only numerical roundoff", {
  skip_if_not_installed("MASS")
  data("lobsters_per_pot", package = "influ2")
  model <- MASS::glm.nb(
    lobsters ~ year + month + poly(depth, 3) + poly(soak, 3),
    data = lobsters_per_pot
  )
  plot <- plot_implied_residuals(
    model, data = lobsters_per_pot, groups = "month", min_n = 1
  )
  expect_s3_class(plot, "ggplot")
  permuted <- plot_implied_residuals(
    model, data = lobsters_per_pot[nrow(lobsters_per_pot):1, ],
    groups = "month", min_n = 1
  )
  expect_equal(plot$data, permuted$data)
  changed <- lobsters_per_pot
  changed$depth[1] <- changed$depth[1] + 5
  expect_error(
    plot_implied_residuals(model, data = changed, groups = "month", min_n = 1),
    "does not reproduce"
  )
  expect_false(.same_observation_column(c(1, 2), c(1, 2 + 1e-12)))
  expect_true(.same_observation_column(c(1, 2), c(1, 2 + 1e-12), tolerance = 1e-10))
})

test_that("residual extraction rejects ambiguous, empty, and nonfinite values", {
  expect_error(.observation_estimate(numeric(), "Residuals"), "finite observation")
  expect_error(.observation_estimate(c(NA_real_, Inf), "Residuals"), "finite observation")
  expect_error(.observation_estimate(array(1, c(2, 3, 4)), "Residuals"), "multidimensional")
  expect_error(.observation_estimate(matrix(1, 2, 3), "Residuals"), "multi-column")
  summary <- cbind(Estimate = c(1, 2), Est.Error = c(0.1, 0.2))
  rownames(summary) <- c("a", "b")
  expect_equal(.observation_estimate(summary, "Residuals", summary = TRUE),
    c(a = 1, b = 2))
  expect_error(.observation_estimate(summary, "Residuals"), "multi-column")
  expect_error(.check_residual_model(list(), NA_character_), "one residual type")
  expect_error(.check_residual_model(list(), c("pearson", "response")), "one residual type")
})

test_that("negative-binomial residual plots use response-scale fitted values", {
  skip_if_not_installed("MASS")
  data <- residual_fixture()
  model <- MASS::glm.nb(catch ~ year + x, data = data)
  plot <- plot_predicted_residuals(model, trend = "none")
  expect_equal(unname(plot$data$predicted), unname(stats::predict(model, type = "response")))
  expect_equal(unname(plot$data$residual), unname(stats::residuals(model, type = "pearson")))
  expect_s3_class(plot(influ_residuals(model, nsim = 20), type = "qq"), "ggplot")
  expect_s3_class(plot_implied_residuals(model, groups = "month", min_n = 1), "ggplot")
})

test_that("GAM residual plots preserve native negative-binomial residuals", {
  skip_if_not_installed("mgcv")
  data <- residual_fixture()
  model <- mgcv::gam(
    catch ~ year + s(x, k = 5), data = data, family = mgcv::nb(), method = "REML"
  )
  plot <- plot_predicted_residuals(model, trend = "none")
  expect_equal(as.numeric(plot$data$predicted), as.numeric(stats::predict(model, type = "response")))
  expect_equal(unname(plot$data$residual), unname(stats::residuals(model, type = "pearson")))
  expect_s3_class(plot(influ_residuals(model, nsim = 20), type = "qq"), "ggplot")
  expect_s3_class(plot_implied_residuals(model, data = data, groups = "month", min_n = 1), "ggplot")
})

test_that("glmmTMB residual plots support random effects and zero inflation", {
  skip_if_not_installed("glmmTMB")
  data <- residual_fixture()
  model <- glmmTMB::glmmTMB(
    catch ~ year + x + (1 | month), data = data, family = glmmTMB::nbinom2()
  )
  plot <- plot_predicted_residuals(model, trend = "none")
  expect_equal(unname(plot$data$predicted), unname(stats::predict(model, type = "response")))
  expect_equal(unname(plot$data$residual), unname(stats::residuals(model, type = "pearson")))
  expect_s3_class(plot(influ_residuals(model, nsim = 20), type = "qq"), "ggplot")
  expect_s3_class(plot_implied_residuals(model, groups = "month", min_n = 1), "ggplot")
  data$catch[seq.int(1L, nrow(data), by = 3L)] <- 0
  zi <- glmmTMB::glmmTMB(
    catch ~ year + x, ziformula = ~year, data = data, family = stats::poisson()
  )
  plot <- plot_predicted_residuals(zi, trend = "none")
  expect_equal(unname(plot$data$predicted), unname(stats::predict(zi, type = "response")))
  expect_equal(unname(plot$data$residual), unname(stats::residuals(zi, type = "pearson")))
  expect_s3_class(plot(influ_residuals(zi, nsim = 20), type = "qq"), "ggplot")
})

test_that("compact brms fixtures give an actionable residual error without MCMC", {
  skip_if_not_installed("brms")
  fit <- readRDS(system.file("extdata", "brms-fixtures", "fit2.rds", package = "influ2"))
  expect_error(plot_predicted_residuals(fit), "Compact brms influence fixtures")
  expect_error(influ_residuals(fit, nsim = 20), "complete brmsfit")
  expect_error(plot_implied_residuals(fit, groups = "month"), "native fitted object")
  multivariate <- structure(list(formula = structure(list(), class = "mvbrmsformula")), class = "brmsfit")
  expect_error(.check_residual_model(multivariate), "one response")
})

test_that("sdmTMB delta residuals cannot be paired with an unconditional mean", {
  model <- structure(list(family = list(delta = TRUE)), class = "sdmTMB")
  expect_error(plot_predicted_residuals(model), "explicit model component")
  expect_error(plot_implied_residuals(model), "explicit model component")
})

test_that("sdmTMB spatial residual plots retain their native response meaning", {
  skip_if_not_installed("sdmTMB")
  data("pcod_2011", package = "sdmTMB")
  data("pcod_mesh_2011", package = "sdmTMB")
  model <- sdmTMB::sdmTMB(
    present ~ as.factor(year) + depth_scaled, data = pcod_2011,
    mesh = pcod_mesh_2011, family = stats::binomial(), time = "year",
    spatial = "on", spatiotemporal = "iid", silent = TRUE
  )
  plot <- plot_predicted_residuals(model, trend = "none")
  expect_equal(unname(plot$data$predicted), unname(stats::fitted(model)))
  expect_equal(unname(plot$data$residual), unname(stats::residuals(model, type = "pearson")))
  expect_s3_class(plot_implied_residuals(model, groups = "present", min_n = 1), "ggplot")
  raw <- plot_predicted_residuals(model, trend = "none", type = "response")
  expect_equal(unname(raw$data$residual), pcod_2011$present - raw$data$predicted)
  checks <- influ_residuals(model, nsim = 20, batch_size = 7)
  expect_equal(checks$observations$observed, pcod_2011$present)
  expect_match(checks$metadata$scheme, "conditional on fitted latent")
  expect_identical(checks$metadata$year, "year")
  expect_true(all(is.finite(checks$observations$residual)))
  expect_s3_class(plot(checks, type = "qq"), "ggplot")
})

test_that("unsupported sdmTMB Pearson residuals retain an explicit native-type boundary", {
  skip_if_not_installed("sdmTMB")
  data <- residual_fixture()
  model <- sdmTMB::sdmTMB(
    catch ~ year + x, data = data, family = sdmTMB::nbinom2(),
    spatial = "off", spatiotemporal = "off", silent = TRUE
  )
  native <- tryCatch(stats::residuals(model, type = "pearson"), error = identity)
  if (inherits(native, "error")) {
    expect_error(plot_predicted_residuals(model), "Pearson residuals are unavailable")
  } else {
    expect_equal(.residual_estimate(model), native)
  }
  raw <- plot_predicted_residuals(model, trend = "none", type = "response")
  expect_equal(unname(raw$data$residual), data$catch - raw$data$predicted)
  checks <- influ_residuals(model, nsim = 20)
  expect_equal(checks$observations$observed, data$catch)
  expect_s3_class(plot(checks), "patchwork")
})

test_that("tinyVAST residual types are explicit and native failures are visible", {
  model <- structure(list(), class = "tinyVAST")
  expect_error(.residual_estimate(model), "type = .*deviance")
  mixed <- structure(list(
    data = data.frame(var = c("a", "b")), internal = list(variable_column = "var")
  ), class = "tinyVAST")
  expect_error(.check_residual_model(mixed, "deviance"), "single tinyVAST response")
  skip_if_not_installed("tinyVAST")
  data <- residual_fixture()
  data$var <- "catch"
  data$dist <- "poisson"
  data$time <- as.numeric(data$year)
  model <- tinyVAST::tinyVAST(
    catch ~ year + x, data = data, family = list(poisson = stats::poisson()),
    spatial_domain = NULL
  )
  expect_error(plot_predicted_residuals(model), "does not provide this residual type")
  plot <- plot_predicted_residuals(model, type = "deviance", trend = "none")
  expect_equal(unname(plot$data$predicted), unname(stats::fitted(model)))
  expect_equal(unname(plot$data$residual), unname(stats::residuals(model, type = "deviance")))
  expect_s3_class(plot_implied_residuals(model, groups = "month", type = "deviance", min_n = 1), "ggplot")
  checks <- influ_residuals(model, nsim = 20, batch_size = 1)
  expect_equal(checks$observations$observed, data$catch)
  expect_match(checks$metadata$scheme, "conditional on fitted latent")
  expect_s3_class(plot(checks), "patchwork")
  expect_s3_class(plot(checks, type = "qq"), "ggplot")
  native_response <- stats::residuals(model, type = "response")
  if (!length(native_response)) {
    expect_error(plot_predicted_residuals(model, type = "response"), "returned no values")
  } else {
    expect_equal(.residual_estimate(model, "response"), native_response)
  }
})
