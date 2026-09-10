test_that("comparison rescaling preserves curve shapes and scales uncertainty", {
  original <- influ(bentley_fixture()$model, focus = "year")
  changed <- original
  keep <- changed$indices$series == "standardised"
  columns <- c("estimate", "std_error", "lower", "upper")
  changed$indices[keep, columns] <- changed$indices[keep, columns] * 3
  fits <- list(original, changed)
  raw <- plot_compare(fits, labels = c("A", "B"))$data
  rescaled <- plot_compare(fits, labels = c("A", "B"), rescale = 2)$data
  for (model in c("A", "B")) {
    rows <- raw$Model == model
    multiplier <- 2 / exp(mean(log(raw$estimate[rows])))
    expect_equal(rescaled[rows, columns], raw[rows, columns] * multiplier)
    expect_equal(exp(mean(log(rescaled$estimate[rows]))), 2)
    expect_equal(rescaled$std_error[rows] / rescaled$estimate[rows],
                 raw$std_error[rows] / raw$estimate[rows])
  }
  expect_equal(original, fits[[1]])
  expect_error(plot_compare(original, rescale = "mean"), "raw.*positive numeric")
  for (bad in list(-1, Inf, NA_real_)) {
    expect_error(plot_compare(original, rescale = bad), "finite and positive")
  }
})

test_that("overlap rescaling uses only shared years, regardless of row order", {
  first <- influ(bentley_fixture()$model, focus = "year")
  first$indices <- first$indices[first$indices$series == "standardised", ]
  second <- first
  second$indices <- second$indices[-1, ]
  columns <- c("estimate", "std_error", "lower", "upper")
  second$indices[, columns] <- second$indices[, columns] * 4
  extra <- second$indices[1, ]
  extra$level <- "extra"
  extra[, columns] <- extra[, columns] * 10
  second$indices <- rbind(extra, second$indices[nrow(second$indices):1, ])
  raw <- plot_compare(list(first, second), labels = c("A", "B"))$data
  scaled <- plot_compare(list(first, second), labels = c("A", "B"),
                         rescale_series = 1)$data
  expect_equal(scaled[scaled$Model == "A", columns], raw[raw$Model == "A", columns])
  expect_equal(scaled[scaled$Model == "B", columns], raw[raw$Model == "B", columns] / 4)
  for (bad in list(0, 3, c(1, 2))) {
    expect_error(plot_compare(list(first, second), rescale_series = bad), "one supplied series")
  }
  second$indices$estimate[1] <- 0
  expect_error(plot_compare(second, rescale = 1), "finite, positive")
})

test_that("comparisons refuse absent indices and infer only a usable focus", {
  fit <- bentley_fixture()$model
  for (bad in list(NULL, list(), 1)) {
    expect_error(plot_compare(bad), "non-empty list")
  }
  expect_error(influ2:::.comparison_focus(list()), "formula cannot be recovered")
  intercept <- stats::update(fit, . ~ 1, data = fit$model)
  expect_error(plot_compare(intercept), "no predictor")
  diagnostic <- influ(fit, focus = "year")
  diagnostic$indices <- diagnostic$indices[diagnostic$indices$series == "nominal", ]
  expect_error(plot_compare(diagnostic), "standardised index")
  diagnostic <- influ(fit, focus = "year")
  diagnostic$indices$component <- "positive"
  expect_identical(unique(plot_compare(diagnostic)$data$component), "positive")
})
