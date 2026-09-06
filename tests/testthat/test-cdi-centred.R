test_that("centred GLM CDI agrees with Bentley term predictions and covariance", {
  fixture <- bentley_fixture()
  model <- fixture$model
  diagnostic <- influ(model, focus = "year")
  d <- subset(diagnostic$coefficients, term == "area")
  prediction <- predict(model, type = "terms", se.fit = TRUE)
  means <- tapply(prediction$fit[, "area"], fixture$data$area, mean)
  ses <- tapply(prediction$se.fit[, "area"], fixture$data$area, mean)
  expect_equal(d$centred_estimate, as.numeric(means[d$level]), tolerance = 1e-12)
  expect_equal(d$centred_std_error, as.numeric(ses[d$level]), tolerance = 1e-12)
  expect_equal(d$relative_estimate, exp(as.numeric(means[d$level])))
  expect_equal(d$relative_lower, exp(d$centred_estimate + qnorm(.025) * d$centred_std_error))
  expect_equal(d$relative_upper, exp(d$centred_estimate + qnorm(.975) * d$centred_std_error))
  baseline <- d$level == levels(fixture$data$area)[1]
  expect_equal(d$estimate[baseline], 0)
  expect_equal(d$std_error[baseline], 0)
  expect_gt(d$centred_std_error[baseline], 0)

  proportion <- prop.table(table(fixture$data$area))
  expect_equal(sum(d$centred_estimate * proportion[d$level]), 0, tolerance = 1e-12)
  annual <- tapply(prediction$fit[, "area"], fixture$data$year, mean)
  influence <- subset(diagnostic$influence, term == "area" & scale == "ratio")
  expect_equal(influence$estimate, exp(as.numeric(annual[influence$level])))
})

test_that("centred CDI is invariant to reference levels and contrast coding", {
  fixture <- bentley_fixture()
  reference <- subset(influ(fixture$model, focus = "year")$coefficients, term == "area")
  reordered <- fixture$data
  reordered$area <- relevel(reordered$area, tail(levels(reordered$area), 1))
  refit <- glm(catch ~ year + area + vessel, data = reordered, family = poisson())
  alternative <- glm(catch ~ year + area + vessel, data = fixture$data,
    family = poisson(), contrasts = list(area = "contr.sum"))
  for (model in list(refit, alternative)) {
    d <- subset(influ(model, focus = "year")$coefficients, term == "area")
    d <- d[match(reference$level, d$level), ]
    expect_equal(d$centred_estimate, reference$centred_estimate, tolerance = 1e-9)
    expect_equal(d$centred_std_error, reference$centred_std_error, tolerance = 1e-9)
    expect_equal(d$relative_lower, reference$relative_lower, tolerance = 1e-9)
  }
})

test_that("CDI uses observed weights or the explicit weighted prediction grid", {
  fixture <- bentley_fixture()
  model <- fixture$model
  w <- rep(c(0, 1, 2, 4), length.out = nrow(fixture$data))
  reference <- fixture$data[seq_len(7), ]
  reference_weights <- c(1, 2, 0, 3, 2, 1, 9)
  X <- model.matrix(model)
  columns <- which(attr(X, "assign") == 2)
  V <- vcov(model)[columns, columns]
  beta <- coef(model)[columns]
  for (explicit in c(FALSE, TRUE)) {
    diagnostic <- if (explicit) {
      influ(model, focus = "year", weights = w, reference_data = reference,
        reference_weights = reference_weights)
    } else influ(model, focus = "year", weights = w)
    ref_X <- if (explicit) .glm_reference_matrix(model, reference)[, columns] else X[, columns]
    ref_w <- if (explicit) reference_weights else w
    centre <- colSums(ref_X * ref_w) / sum(ref_w)
    d <- subset(diagnostic$coefficients, term == "area")
    for (i in seq_len(nrow(d))) {
      row <- which(as.character(fixture$data$area) == d$level[i])[1]
      contrast <- X[row, columns] - centre
      expect_equal(d$centred_estimate[i], sum(contrast * beta))
      expect_equal(d$centred_std_error[i], sqrt(drop(t(contrast) %*% V %*% contrast)))
    }
  }
})

test_that("joint simulations are centred before reducing and transforming", {
  fixture <- bentley_fixture()
  model <- fixture$model
  draws <- .draw_mvn(400L, coef(model), vcov(model), seed = 91)
  X <- model.matrix(model)
  columns <- which(attr(X, "assign") == 2)
  centre <- colMeans(X[, columns])
  diagnostic <- influ(model, focus = "year", uncertainty = "simulation",
    ndraws = 400, seed = 91, probs = c(.1, .8))
  d <- subset(diagnostic$coefficients, term == "area")
  row <- which(as.character(fixture$data$area) == d$level[1])[1]
  delta <- drop(draws[, columns] %*% (X[row, columns] - centre))
  expect_equal(d$centred_estimate[1], mean(delta))
  expect_equal(d$centred_std_error[1], sd(delta))
  expect_equal(d$relative_estimate[1], mean(exp(delta)))
  expect_equal(d$relative_lower[1], unname(quantile(exp(delta), .1)))
  expect_equal(d$relative_upper[1], unname(quantile(exp(delta), .8)))
  expect_null(diagnostic$draws)
  preview <- influ(model, focus = "year", uncertainty = "none")
  expect_true(all(is.na(preview$coefficients$centred_lower)))
  expect_true(all(is.na(preview$coefficients$relative_upper)))
  expect_null(preview$draws)
})

test_that("GAM smooth CDI propagates covariance of the full centred basis", {
  skip_if_not_installed("mgcv")
  data <- bentley_fixture()$data
  data$depth <- seq_len(nrow(data)) / nrow(data)
  model <- mgcv::gam(catch ~ year + s(depth, k = 5), family = poisson(), data = data)
  diagnostic <- influ(model, focus = "year")
  X <- predict(model, type = "lpmatrix")
  columns <- model$smooth[[1]]$first.para:model$smooth[[1]]$last.para
  contribution <- drop(X[, columns] %*% coef(model)[columns])
  groups <- .cdi_level_groups(data, "s(depth)", contribution, rep(1, nrow(data)))
  d <- subset(diagnostic$coefficients, term == "s(depth)")
  i <- groups[[d$level[1]]]
  contrast <- colMeans(X[i, columns, drop = FALSE]) - colMeans(X[, columns])
  expect_equal(d$centred_estimate[1], sum(contrast * coef(model)[columns]))
  expect_equal(d$centred_std_error[1],
    sqrt(drop(t(contrast) %*% vcov(model)[columns, columns] %*% contrast)))
})

test_that("CDI plotting defaults and model-coded option use their stated scale", {
  fixture <- bentley_fixture()
  diagnostic <- influ(fixture$model, focus = "year")
  coefficients <- subset(diagnostic$coefficients, term == "area")
  centred <- .cdi_plot_coefficients(coefficients, "area", "centred", "auto")
  expect_true(centred$ratio)
  expect_identical(centred$label, "Relative area effect")
  expect_equal(centred$data$estimate, coefficients$relative_estimate)
  old <- .cdi_plot_coefficients(coefficients, "area", "model", "auto")
  expect_false(old$ratio)
  expect_equal(old$data$estimate, coefficients$estimate)
  expect_equal(old$data$lower, coefficients$lower)
  link <- .cdi_plot_coefficients(coefficients, "area", "centred", "link")
  expect_false(link$ratio)
  expect_equal(link$data$estimate, coefficients$centred_estimate)
  expect_s3_class(plot(diagnostic, type = "cdi", term = "area"), "patchwork")
  expect_s3_class(ggplot2::autoplot(diagnostic, type = "cdi", term = "area",
    coefficient_reference = "model"), "patchwork")
  expect_error(plot(diagnostic, type = "cdi", term = "area",
    coefficient_reference = "unknown"), "arg")
})

test_that("CDI labels respect link and zero-probability component orientation", {
  for (link in c("identity", "logit", "probit", "cloglog")) {
    spec <- .new_family_spec(if (link == "identity") "gaussian" else "binomial",
      link, complement = link != "identity")
    d <- .cdi_summary_row("month", "01", .2, -.1, family_spec = spec,
      method = "none", probs = c(.025, .975))
    display <- .cdi_plot_coefficients(d, "month", "centred", "auto")
    expect_false(display$ratio)
    expect_equal(display$data$estimate, -.1)
    if (link != "identity") expect_match(display$label, "zero")
  }
  lognormal <- .cdi_summary_row("month", "01", .2, -.1,
    family_spec = .new_family_spec("lognormal", "identity"),
    method = "none", probs = c(.025, .975))
  expect_true(.cdi_plot_coefficients(lognormal, "month", "centred", "auto")$ratio)
  extra_zero <- .cdi_summary_row("month", "01", .2, -.1,
    family_spec = .new_family_spec("binomial", "logit"),
    method = "none", probs = c(.025, .975))
  extra_zero$component <- "response:zero_probability:group_level"
  expect_match(.cdi_plot_coefficients(extra_zero, "month", "centred", "auto")$label,
    "log-odds of zero")
})

test_that("CDI does not silently overlay different model components", {
  d <- influ(bentley_fixture()$model, focus = "year")
  other <- d
  for (table in c("coefficients", "composition", "influence")) {
    other[[table]]$component <- "occurrence"
    d[[table]] <- rbind(d[[table]], other[[table]])
  }
  expect_error(plot(d, type = "cdi", term = "area"), "one component")
  expect_s3_class(plot(d, type = "cdi", term = "area", component = "conditional"), "patchwork")
})

test_that("zero-weight reference rows cannot contaminate centred effects", {
  data <- data.frame(year = factor(rep(1:3, each = 2)),
    depth = c(1, 4, 2, 6, 3, 8), catch = c(1, 3, 2, 4, 4, 5))
  model <- glm(catch ~ year + depth, data = data, family = poisson())
  reference <- data[1:2, ]
  reference$depth[2] <- Inf
  weighted <- influ(model, focus = "year", reference_data = reference,
    reference_weights = c(1, 0))
  single <- influ(model, focus = "year", reference_data = reference[1, ])
  expect_equal(weighted$coefficients, single$coefficients)
  expect_equal(weighted$influence, single$influence)
  expect_error(influ(model, focus = "year", reference_data = reference),
    "finite on positive-weight rows")
})
