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

  size_scale <- p[[3]]$scales$get_scales("size")
  expect_equal(size_scale$breaks(range(diagnostic$composition$proportion[
    diagnostic$composition$term == "month"])), c(0.05, 0.10, 0.15, 0.20))
  expect_equal(p[[3]]$guides$guides$size$params$ncol, 1)
})

test_that("CDI size keys are positive, in range, and limited to four", {
  for (limits in list(c(0, 0.27), c(0.006, 0.265), c(0.16, 0.6),
                      c(0.001, 0.003), c(1, 1))) {
    breaks <- .cdi_proportion_breaks(limits)
    expect_true(length(breaks) >= 1L && length(breaks) <= 4L)
    expect_true(all(is.finite(breaks) & breaks > 0))
    expect_true(all(breaks >= limits[1] & breaks <= limits[2]))
    expect_false(is.unsorted(breaks, strictly = TRUE))
  }
})

test_that("CDI label thinning preserves both ends and the supplied ordering", {
  for (n in c(1L, 12L, 20L, 21L, 40L, 41L, 200L)) {
    levels <- rev(sprintf("V%03d", seq_len(n)))
    breaks <- .cdi_axis_breaks(levels)
    expect_length(breaks, min(20L, n))
    expect_identical(breaks[c(1, length(breaks))], levels[c(1, n)])
    expect_false(is.unsorted(match(breaks, levels), strictly = TRUE))
  }
})

dense_cdi_fixture <- function(random = FALSE) {
  d <- expand.grid(year = factor(2019:2022), vessel = factor(sprintf("V%03d", 1:41)),
    replicate = 1:3)
  d$yes <- as.integer((as.integer(d$vessel) + as.integer(d$year) + d$replicate) %% 3L != 0L)
  fit <- if (random) {
    mgcv::gam(yes ~ year + s(vessel, bs = "re"), family = binomial(), data = d)
  } else stats::glm(yes ~ year + vessel, family = binomial(), data = d)
  influ(fit, focus = "year")
}

test_that("fixed and random CDI axes share breaks without discarding vessels", {
  skip_if_not_installed("mgcv")
  for (random in c(FALSE, TRUE)) {
    d <- dense_cdi_fixture(random)
    term <- if (random) "s(vessel)" else "vessel"
    p <- plot(d, type = "cdi", term = term)
    top <- p[[1]]$scales$get_scales("x")
    bottom <- p[[3]]$scales$get_scales("x")
    expect_identical(p[[1]]$labels$y, "Effect (log-odds)")
    expect_identical(top$limits, bottom$limits)
    expect_identical(top$breaks, bottom$breaks)
    expect_length(top$limits, 41L)
    expect_length(top$breaks, 20L)
    expect_equal(p[[1]]$theme$axis.text.x$angle, 0)
    expect_equal(p[[3]]$theme$axis.text.x$angle, 0)
    expect_equal(nrow(p[[1]]$data), 41L)
    expect_equal(nrow(p[[3]]$data), 41L * 4L)
    top_data <- ggplot2::ggplot_build(p[[1]])$data[[3]]
    bottom_data <- ggplot2::ggplot_build(p[[3]])$data[[1]]
    expect_equal(sort(unique(as.numeric(top_data$x))), seq_len(41L))
    expect_equal(sort(unique(as.numeric(bottom_data$x))), seq_len(41L))
    expect_equal(p[[1]]$data$estimate,
      d$coefficients$centred_estimate[d$coefficients$term == term])
  }
})

test_that("long CDI labels use position-aware angled axis guides", {
  d <- monthly_cdi_fixture()
  levels <- sprintf("Long category %02d", 1:12)
  d$coefficients$level[d$coefficients$term == "month"] <- levels
  rows <- d$composition$term == "month"
  original <- unique(d$composition$term_level[rows])
  d$composition$term_level[rows] <- levels[match(d$composition$term_level[rows], original)]
  p <- plot(d, type = "cdi", term = "month")
  for (k in c(1, 3)) {
    expect_equal(p[[k]]$scales$get_scales("x")$guide$params$angle, 45)
    expect_s3_class(ggplot2::ggplotGrob(p[[k]]), "gtable")
  }
})

test_that("dense encounter CDI labels remain visually stable", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("dense encounter CDI horizontal labels", function() {
    print(plot(dense_cdi_fixture(), type = "cdi", term = "vessel"))
  })
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
