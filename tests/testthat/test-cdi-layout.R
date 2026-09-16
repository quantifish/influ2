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

test_that("CDI labels use a regular stride without an arbitrary count limit", {
  expect_identical(.cdi_regular_indices(numeric(), 4), integer())
  expect_identical(.cdi_regular_indices(1, 4), 1L)
  expect_identical(.cdi_regular_indices(1:41 * 10, 4), 1:41)
  expect_identical(.cdi_regular_indices(1:22 * 4, 5), seq.int(1L, 22L, 2L))
  expect_identical(.cdi_regular_indices(1:41 * 2, 4), seq.int(1L, 41L, 3L))
  expect_identical(.cdi_regular_indices(1:41 * 2, c(4, 7)), seq.int(1L, 41L, 5L))
})

draw_cdi_axes <- function(p, width = 10) {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path, width = width, height = 7)
  on.exit({ grDevices::dev.off(); unlink(path) })
  print(p)
  p[[1]]$scales$get_scales("x")$guide$cdi_state$drawn
}

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
    expect_length(top$breaks, 41L)
    expect_false(top$guide$params$check.overlap)
    expect_false(bottom$guide$params$check.overlap)
    expect_identical(top$guide$cdi_state, bottom$guide$cdi_state)
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
    drawn <- draw_cdi_axes(p)
    expect_identical(drawn$top$labels, drawn$bottom$labels)
    expect_length(unique(diff(drawn$top$indices)), 1L)
  }
})

test_that("CDI label spacing responds to output width and survives serialisation", {
  p <- plot(dense_cdi_fixture(), type = "cdi", term = "vessel")
  counts <- vapply(c(7, 10, 14, 28), function(width) {
    drawn <- draw_cdi_axes(p, width)
    expect_length(drawn, 2L)
    expect_identical(drawn$top$indices, drawn$bottom$indices)
    expect_equal(drawn$top$positions_mm, drawn$bottom$positions_mm)
    expect_length(unique(diff(drawn$top$indices)), 1L)
    expect_true(all(diff(drawn$top$positions_mm[drawn$top$indices]) >=
      drawn$top$label_width_mm + 1.5))
    length(drawn$top$indices)
  }, integer(1))
  expect_false(is.unsorted(counts))
  expect_lt(counts[1], counts[3])
  expect_equal(counts[4], 41L)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(p, path)
  expect_identical(draw_cdi_axes(readRDS(path), 7)$top$indices,
    draw_cdi_axes(p, 7)$top$indices)
})

test_that("CDI label measurement includes every label and both axis fonts", {
  p <- plot(dense_cdi_fixture(), type = "cdi", term = "vessel")
  original <- draw_cdi_axes(p, 14)
  p[[3]] <- p[[3]] + ggplot2::theme(axis.text.x = ggplot2::element_text(size = 22))
  large <- draw_cdi_axes(p, 14)
  expect_identical(large$top$indices, large$bottom$indices)
  expect_lt(length(large$top$indices), length(original$top$indices))
  expect_gt(large$top$label_width_mm, original$top$label_width_mm)

  state <- new.env(parent = emptyenv())
  q <- ggplot2::ggplot(data.frame(x = factor(c("I", "MMMMMM")), y = 1:2),
    ggplot2::aes(x, y)) + ggplot2::geom_point() +
    ggplot2::scale_x_discrete(guide = .cdi_axis_guide(0, state))
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({ grDevices::dev.off(); unlink(path) })
  print(q)
  template <- state$templates$bottom
  expected <- grid::convertWidth(grid::grobWidth(grid::textGrob("MMMMMM",
    gp = template$gp)), "mm", valueOnly = TRUE)
  expect_equal(state$drawn$bottom$label_width_mm, expected)
})

test_that("22 short statistical-area labels all fit at presentation size", {
  d <- expand.grid(year = factor(2019:2022), area = factor(sprintf("%03d", 1:22)),
    replicate = 1:3)
  d$yes <- as.integer((as.integer(d$area) + as.integer(d$year) + d$replicate) %% 3L != 0L)
  p <- plot(influ(stats::glm(yes ~ year + area, binomial(), d), focus = "year"),
    type = "cdi", term = "area")
  for (k in c(1, 3, 4)) p[[k]] <- p[[k]] + ggplot2::theme(text = ggplot2::element_text(size = 13))
  drawn <- draw_cdi_axes(p, 14)
  expect_identical(drawn$top$labels, levels(d$area))
  expect_identical(drawn$top$labels, drawn$bottom$labels)
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
  drawn <- draw_cdi_axes(p, 7)
  expect_identical(drawn$top$indices, drawn$bottom$indices)
  expect_length(unique(diff(drawn$top$indices)), 1L)
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
