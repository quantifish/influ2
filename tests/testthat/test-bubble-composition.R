bubble_fixture <- function() {
  cells <- data.frame(
    year = c("2020", "2020", "2021", "2021"),
    month = c("Jan", "Feb", "Jan", "Feb"),
    n = c(1L, 3L, 2L, 6L)
  )
  cells[rep(seq_len(nrow(cells)), cells$n), c("year", "month")]
}

test_that("bubble sizes use the requested count or composition denominator", {
  data <- bubble_fixture()
  raw <- plot_bubble(data, c("year", "month"))$data
  expected <- c("2020/Jan" = 1, "2020/Feb" = 3, "2021/Jan" = 2, "2021/Feb" = 6)
  expect_equal(raw$size, unname(expected[paste(raw$year, raw$month, sep = "/")]))
  overall <- plot_bubble(data, c("year", "month"), sum_by = "all")$data
  expect_equal(overall$size, raw$size / nrow(data))
  expect_equal(sum(overall$size), 1)
  for (mode in c("row", "column")) {
    proportions <- plot_bubble(data, c("year", "month"), sum_by = mode)$data
    margin <- if (mode == "row") "year" else "month"
    expect_equal(as.numeric(tapply(proportions$size, proportions[[margin]], sum)), c(1, 1))
    expect_equal(proportions$size,
                 raw$size / ave(raw$size, raw[[margin]], FUN = sum))
  }
  aliases <- c(rows = "row", y = "row", col = "column", cols = "column",
               columns = "column", x = "column")
  for (alias in names(aliases)) {
    expect_identical(plot_bubble(data, c("year", "month"), sum_by = alias)$data,
                     plot_bubble(data, c("year", "month"), sum_by = aliases[[alias]])$data)
  }
})

test_that("mapped bubble colours preserve counts and requested ordering", {
  data <- bubble_fixture()
  data$vessel <- rep(c("A", "B"), length.out = nrow(data))
  plot <- plot_bubble(data, c("year", "month"), fill = "vessel", sum_by = "row",
                      sort_order = c("Feb", "Jan"), xlab = "Month", ylab = "Year",
                      zlab = "Proportion")
  expect_identical(levels(plot$data$month), c("Feb", "Jan"))
  expect_equal(as.numeric(tapply(plot$data$size, plot$data$year, sum)), c(1, 1))
  expect_identical(plot$labels$x, "Month")
  expect_identical(plot$labels$y, "Year")
  expect_identical(plot$labels$size, "Proportion")
  rendered <- ggplot2::ggplot_build(plot)
  expect_equal(length(unique(rendered$data[[1]]$colour)), 2L)
  expect_equal(nrow(rendered$data[[1]]), nrow(plot$data))
  # Mapping to an existing grouping variable must not duplicate that column.
  expect_equal(sum(plot_bubble(data, c("year", "month"), fill = "month")$data$size),
               nrow(data))
})

test_that("invalid bubble controls and missing colours fail explicitly", {
  data <- bubble_fixture()
  expect_error(plot_bubble(as.matrix(data)), "must be a data frame")
  expect_error(plot_bubble(data, "year"), "exactly two columns")
  expect_error(plot_bubble(data, c("year", "absent")), "exactly two columns")
  for (value in list(NA_character_, character(), c("raw", "all"), 1)) {
    expect_error(plot_bubble(data, c("year", "month"), sum_by = value),
                 "one character value")
  }
  expect_error(plot_bubble(data, c("year", "month"), sum_by = "invalid"), "arg")
  for (value in list(NA_real_, Inf, -1, numeric(), c(0, 1), "opaque")) {
    expect_error(plot_bubble(data, c("year", "month"), alpha = value),
                 "between zero and one")
  }
  for (value in list(NA_character_, character(), c("red", "blue"), 1)) {
    expect_error(plot_bubble(data, c("year", "month"), fill = value),
                 "one colour or column name")
  }
  for (value in list(c("Jan", "Jan"), c("Jan", NA), c("Jan", "Mar"))) {
    expect_error(plot_bubble(data, c("year", "month"), sort_order = value),
                 "every observed horizontal-group level")
  }
  data$colour <- NA_character_
  expect_error(plot_bubble(data, c("year", "month"), fill = "colour"),
               "colour-mapping column must not contain missing")
})
