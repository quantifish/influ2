test_that("model-neutral comparison and step plots share the index schema", {
  fixture <- bentley_fixture()
  full <- influ(fixture$model, focus = "year")
  reduced_model <- stats::glm(
    catch ~ year + area,
    family = stats::poisson(link = "log"),
    data = fixture$data
  )
  reduced <- influ(reduced_model, focus = "year")

  comparison <- plot_compare(
    list(full, reduced),
    labels = c("Full", "Reduced")
  )
  steps <- plot_step(
    list(reduced, full),
    labels = c("Reduced", "Full")
  )

  expect_s3_class(comparison, "ggplot")
  expect_setequal(unique(comparison$data$Model), c("Full", "Reduced"))
  expect_s3_class(steps, "ggplot")
  expect_setequal(unique(steps$data$Step), c("Current", "Previous"))
})

test_that("data extent reports the observed proportion by focus level", {
  data <- data.frame(
    year = rep(1:2, each = 3),
    catch = c(1, NA, 3, NA, NA, 2),
    effort = c(1, 2, 3, 4, NA, 6)
  )
  plot <- plot_data_extent(data, "year", c("catch", "effort"))

  expect_s3_class(plot, "ggplot")
  catch <- plot$data[plot$data$variable == "catch", ]
  expect_equal(catch$proportion, c(2 / 3, 1 / 3))
})

test_that("implied residual coefficients follow the fisheries definition", {
  fixture <- bentley_fixture()
  plot <- plot_implied_residuals(
    fixture$model,
    data = fixture$data,
    year = "year",
    groups = "area",
    min_n = 1
  )
  expect_s3_class(plot, "ggplot")

  first <- plot$data[1, ]
  residual <- stats::residuals(fixture$model, type = "pearson")
  keep <- as.character(fixture$data$year) == first$level &
    as.character(fixture$data$area) == first$group
  year_effect <- influ(
    fixture$model,
    focus = "year",
    uncertainty = "none"
  )$influence
  year_effect <- subset(
    year_effect,
    term == "year" & scale == "link" & level == first$level
  )$estimate

  expect_equal(
    first$implied,
    year_effect + mean(residual[keep]),
    tolerance = 1e-12
  )
  expect_equal(
    first$std_error,
    stats::sd(residual[keep]) / sqrt(sum(keep)),
    tolerance = 1e-12
  )
})

test_that("predicted-residual and Q-Q plots support GLMs", {
  model <- bentley_fixture()$model
  expect_s3_class(plot_predicted_residuals(model, trend = "none"), "ggplot")
  expect_s3_class(plot_predicted_residuals(model, trend = "lm"), "ggplot")
  expect_s3_class(plot_qq(model), "ggplot")
  expect_error(plot_predicted_residuals(model, trend = "bad"), "must be")
  expect_error(plot_qq(model, probs = c(0.9, 0.1)), "increasing")
})

test_that("BRMS comparison helpers validate their inputs", {
  expect_error(get_bayes_R2(list(stats::lm(mpg ~ wt, mtcars))), "brmsfit")
  expect_error(table_criterion(list(stats::lm(mpg ~ wt, mtcars))), "brmsfit")
  expect_error(
    table_criterion(list(), criterion = "not-a-criterion"),
    "Unknown"
  )
})
