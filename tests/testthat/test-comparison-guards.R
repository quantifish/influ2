test_that("identity-link comparisons retain negative year differences", {
  fixture <- bentley_fixture()
  fit <- stats::glm(catch ~ year + area + vessel,
    family = stats::gaussian(), data = fixture$data)
  comparison <- plot_compare(fit, year = "year")
  built <- ggplot2::ggplot_build(comparison)

  expect_identical(unique(comparison$data$scale), "difference")
  expect_true(any(comparison$data$estimate < 0))
  expect_false(anyNA(built$data[[3]]$y))
  expect_equal(sort(built$data[[3]]$y), sort(comparison$data$estimate))
  expect_identical(comparison$labels$y, "Year-effect difference")
})

test_that("ratio comparisons still start their y-axis at zero", {
  comparison <- plot_compare(bentley_fixture()$model, year = "year")
  expect_identical(unique(comparison$data$scale), "ratio")
  expect_equal(comparison$scales$get_scales("y")$limits[1], 0)
  expect_identical(comparison$labels$y, "Standardised index")
})

test_that("link comparisons retain negative contrasts", {
  diagnostic <- influ(bentley_fixture()$model, focus = "year", uncertainty = "none")
  keep <- diagnostic$indices$series == "standardised"
  diagnostic$indices$estimate[keep] <- log(diagnostic$indices$estimate[keep])
  diagnostic$indices$scale[keep] <- "link"
  comparison <- plot_compare(diagnostic, show_probs = FALSE)
  built <- ggplot2::ggplot_build(comparison)

  expect_true(any(comparison$data$estimate < 0))
  expect_false(anyNA(built$data[[2]]$y))
  expect_identical(comparison$labels$y, "Year-effect contrast (link scale)")
})

test_that("comparisons reject incompatible index scales", {
  fixture <- bentley_fixture()
  identity_fit <- stats::glm(catch ~ year + area + vessel,
    family = stats::gaussian(), data = fixture$data)
  expect_error(
    plot_compare(list(fixture$model, identity_fit), year = "year"),
    "same index scale"
  )
  ratio <- influ(fixture$model, focus = "year", uncertainty = "none")
  link <- ratio
  link$indices$scale[link$indices$series == "standardised"] <- "link"
  expect_error(plot_compare(list(ratio, link)), "same index scale")
})

test_that("comparison labels do not silently collapse model curves", {
  fit <- bentley_fixture()$model
  for (labels in list(c("Same", "Same"), c("Same", " Same "),
      c("First", ""), c("First", " "), c("First", NA_character_),
      "One", c(1, 2))) {
    expect_error(plot_compare(list(fit, fit), labels = labels),
      "unique, non-empty label")
  }
  comparison <- plot_compare(list(fit, fit))
  expect_length(unique(comparison$data$Model), 2L)
  expect_match(unique(comparison$data$Model)[2], " #1$")
  built <- ggplot2::ggplot_build(comparison)
  expect_length(unique(built$data[[3]]$group), 2L)
  expect_equal(as.numeric(table(comparison$data$Model)), rep(nlevels(fit$model$year), 2L))
})

test_that("brms criteria explain the compact fixture boundary", {
  skip_if_not_installed("brms")
  fixture <- readRDS(system.file(
    "extdata", "brms-fixtures", "fit2.rds", package = "influ2"
  ))
  expect_null(fixture$fit)
  expect_false(is.null(fixture$influ2_draws))
  for (helper in list(get_bayes_R2, table_criterion)) {
    expect_error(helper(fixture), "original complete brmsfit")
    expect_error(helper(list(fixture)), "compact influence-only fixture")
    expect_error(helper(fixture), "do not run MCMC")
  }
})
