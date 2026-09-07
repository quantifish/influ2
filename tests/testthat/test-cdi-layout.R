monthly_cdi_fixture <- function() {
  data("lobsters_per_pot", package = "influ2", envir = environment())
  model <- stats::glm(lobsters ~ year + month + poly(depth, 3) + poly(soak, 3),
    family = stats::poisson(), data = lobsters_per_pot)
  influ(model, focus = "year")
}

test_that("CDI term axes are aligned and the year axis is on the outside", {
  diagnostic <- monthly_cdi_fixture()
  p <- plot(diagnostic, type = "cdi", term = "month")
  expect_identical(p[[1]]$scales$get_scales("x")$position, "top")
  expect_identical(p[[1]]$scales$get_scales("x")$limits,
    p[[3]]$scales$get_scales("x")$limits)
  expect_equal(p[[1]]$theme$axis.text.x$angle, 0)
  expect_equal(p[[3]]$theme$axis.text.x$angle, 0)
  expect_identical(p[[3]]$scales$get_scales("y")$limits,
    p[[4]]$scales$get_scales("x")$limits)
  expect_identical(p[[4]]$scales$get_scales("x")$position, "top")
  expect_identical(p[[4]]$labels$x, "year")
  layout <- ggplot2::ggplotGrob(p[[4]])
  expect_false(inherits(layout$grobs[[match("axis-r", layout$layout$name)]], "zeroGrob"))
  expect_true(inherits(layout$grobs[[match("axis-l", layout$layout$name)]], "zeroGrob"))

  # The legend is the guide itself, not a parent plot layout containing empty
  # guide positions. In particular its title must fit inside the top-right cell.
  legend <- attr(p[[2]], "grobs")$full
  expect_s3_class(legend, "gtable")
  expect_true("guides" %in% legend$layout$name)
  expect_false(any(grepl("guide-box", legend$layout$name)))
})

test_that("Bayesian monthly CDI shares the same labelled layout", {
  skip_if_not_installed("brms")
  fit <- readRDS(system.file("extdata", "brms-fixtures", "fit2.rds", package = "influ2"))
  diagnostic <- influ(fit, focus = "year")
  p <- plot(diagnostic, type = "cdi", term = "month")
  expect_identical(p[[1]]$scales$get_scales("x")$position, "top")
  expect_identical(p[[4]]$scales$get_scales("x")$position, "top")
  expect_equal(p[[3]]$theme$axis.text.x$angle, 0)
})

test_that("monthly CDI labels and size legend remain visually stable", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("monthly CDI labelled layout", function() {
    print(plot(monthly_cdi_fixture(), type = "cdi", term = "month"))
  })
})
