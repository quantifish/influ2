gap_index_fixture <- function(years, mean = seq_along(years)) {
  .new_cpue_index(data.frame(Year = as.character(years), Mean = as.numeric(mean),
    Median = NA_real_, SD = as.numeric(mean) / 10, CV = .1,
    Qlower = as.numeric(mean) * .8, Qupper = as.numeric(mean) * 1.2),
    list(method = "standardised", scale = "response", units = "lobsters per pot",
      year = "year", rescale = "raw", random_effects = "conditional"))
}

test_that("calculated index lines and intervals do not bridge missing calendar years", {
  full <- gap_index_fixture(2000:2005)
  recent <- gap_index_fixture(c(2001, 2003, 2004), c(2, 4, 3))
  source <- list(Full = full, Recent = recent)
  before <- serialize(source, NULL)
  p <- plot_compare(source)
  expect_identical(serialize(source, NULL), before)
  d <- p$data
  expect_equal(subset(d, Model == "Full")$line_segment, rep(1, 6))
  expect_equal(subset(d, Model == "Recent")$line_segment, c(1, 2, 2))
  expect_equal(d$line_segment, d$ribbon_segment)
  expect_equal(subset(d, Model == "Recent")$year_position, c(2L, 4L, 5L))
  expect_equal(subset(d, Model == "Recent")$x, c(2001, 2003, 2004))
  expect_equal(subset(d, Model == "Recent")$Qlower, recent$table$Qlower)
  expect_equal(subset(d, Model == "Recent")$Qupper, recent$table$Qupper)
  built <- ggplot2::ggplot_build(p)
  for (layer in built$data[1:2]) {
    groups <- split(layer$x, layer$group)
    expect_true(all(vapply(groups, function(x) all(diff(sort(unique(x))) <= 1), logical(1))))
  }
})

test_that("globally missing years also break each calculated index trajectory", {
  x <- gap_index_fixture(c(2000, 2002, 2003, 2005))
  p <- plot_index(x)
  expect_equal(p$data$year_position, c(1L, 3L, 4L, 6L))
  expect_equal(p$data$line_segment, c(1, 2, 2, 3))
  expect_equal(p$data$ribbon_segment, c(1, 2, 2, 3))
  b <- ggplot2::ggplot_build(p)
  expect_equal(length(unique(b$data[[1L]]$group)), 3L)
  expect_equal(length(unique(b$data[[2L]]$group)), 3L)
  expect_equal(p$data$Mean, x$table$Mean)
  expect_equal(p$data$Qlower, x$table$Qlower)
})

test_that("missing uncertainty splits only ribbons, not finite point-estimate lines", {
  x <- gap_index_fixture(2000:2004)
  x$table$Qlower[3L] <- NA_real_
  x$table$Qupper[3L] <- NA_real_
  p <- plot_index(x)
  expect_equal(p$data$line_segment, rep(1, 5))
  expect_equal(p$data$ribbon_segment, c(1, 1, 2, 3, 3))
  b <- ggplot2::ggplot_build(p)
  expect_equal(length(unique(b$data[[2L]]$group)), 1L)
  expect_equal(p$data$Qlower, x$table$Qlower)
  x$table$Mean[3L] <- NA_real_
  p <- plot_index(x)
  expect_equal(p$data$line_segment, c(1, 1, 2, 3, 3))
  expect_true(is.na(p$data$Mean[3L]))
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("fishing-year gaps retain empty positions without equating labels", {
  x <- gap_index_fixture(c("1999/00", "2001/02", "2002/03"))
  p <- plot_index(x)
  expect_identical(levels(p$data$x), c("1999/00", "2000/01", "2001/02", "2002/03"))
  expect_equal(p$data$year_position, c(1L, 3L, 4L))
  expect_equal(p$data$line_segment, c(1, 2, 2))
  expect_identical(p$scales$get_scales("x")$limits, levels(p$data$x))
  expect_false(p$scales$get_scales("x")$drop)
  x$table$Year <- c("Before", "During", "After")
  p <- plot_index(x)
  expect_identical(levels(p$data$x), x$table$Year)
  expect_equal(p$data$line_segment, rep(1, 3))
  # Similar-looking identifiers are not silently collapsed onto numeric 1.
  x$table$Year <- c("01", "1", "02")
  p <- plot_index(x)
  expect_true(is.factor(p$data$x))
  expect_identical(levels(p$data$x), x$table$Year)
})

test_that("regular annual indices preserve their coordinates and all stored summaries", {
  x <- gap_index_fixture(2000:2004)
  p <- plot_index(x)
  expect_identical(p$data$x, as.numeric(2000:2004))
  expect_equal(p$data$line_segment, rep(1, 5))
  expect_equal(p$data$ribbon_segment, rep(1, 5))
  expect_equal(p$data[names(x$table)], x$table)
  expect_null(p$scales$get_scales("x"))
  expect_equal(length(ggplot2::ggplot_build(p)$data), 3L)
  plain <- plot_index(x, show_probs = FALSE)
  expect_equal(length(ggplot2::ggplot_build(plain)$data), 2L)
  expect_equal(plain$data[names(x$table)], x$table)
})

test_that("saved calculated-index plotting does not load or recalculate backends", {
  x <- gap_index_fixture(c(2000, 2002, 2003))
  local_mocked_bindings(.require_model_backend = function(...) stop("Unexpected backend load"),
    cpue_index = function(...) stop("Unexpected recalculation"))
  expect_s3_class(plot_index(x), "ggplot")
  expect_s3_class(plot_compare(list(A = x, B = x)), "ggplot")
  restored <- unserialize(serialize(plot_compare(list(A = x, B = x)), NULL))
  expect_equal(restored$data$line_segment, rep(c(1, 2, 2), 2))
})
