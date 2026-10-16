.domain_method_fixture <- function() {
  tab <- data.frame(Year = c("2001", "2002", "2003"), Mean = c(2, 4, 8),
    SD = c(0.2, 0.4, 0.8), Qlower = c(1, 2, 4), Qupper = c(3, 6, 12))
  index <- structure(list(table = tab, metadata = list(backend = "mock",
    rescale = "raw")), class = "influ_index")
  alternate <- index
  alternate$table[c("Mean", "SD", "Qlower", "Qupper")] <-
    alternate$table[c("Mean", "SD", "Qlower", "Qupper")] * 2
  partition <- tab
  partition$class <- "sampled"
  partition$share <- 0.6
  partition$share_lower <- 0.4
  partition$share_upper <- 0.8
  structure(list(baseline = index, sampled_only = list(cell_time = alternate),
    omission = list(spatial = alternate), partition = partition,
    coverage = data.frame(Year = tab$Year, class = "sampled",
      cells = 2L, weight = 2, share = 0.6),
    domains = data.frame(Year = tab$Year, definition = "cell_time",
      cells = 2L, weight = 2, share = 0.6),
    decomposition = data.frame(Year = tab$Year, class = "sampled",
      term = "spatial", component = "conditional", mean = c(-1, 0, 1),
      SD = 0.1, Q10 = c(-1.2, -0.2, 0.8), Q50 = c(-1, 0, 1),
      Q90 = c(-0.8, 0.2, 1.2)),
    trend = data.frame(series = "Full index", window = "all", slope = 0.1),
    retained_cells = data.frame(Year = rep(tab$Year, each = 2),
      cell = rep(1:2, 3), term = "spatial", component = "conditional",
      contribution = seq(-1, 1, length.out = 6), sampled = rep(c(TRUE, FALSE), 3),
      X = rep(1:2, 3), Y = rep(1:2, 3)),
    metadata = list(backend = "mock", omission_definition = "cell_time",
      caveats = "Prediction only")),
    class = "influ_domain")
}

test_that("domain methods reuse stored tables and metadata", {
  x <- .domain_method_fixture()
  expect_identical(as.data.frame(x), x$coverage)
  for (what in c("coverage", "decomposition", "partition", "trend")) {
    expect_identical(summary(x, what = what), x[[what]])
  }
  for (what in c("sampled_only", "omission")) {
    d <- as.data.frame(x, what = what)
    expect_identical(d$Mean, c(x$baseline$table$Mean, x[[what]][[1]]$table$Mean))
    expect_identical(unique(d$Series), c("Full index", names(x[[what]])))
    expect_identical(attr(d, "index_metadata")[[1]], x$baseline$metadata)
  }
  expect_output(value <- print(x), "Prediction only")
  expect_output(print(x), "Omission definition: cell_time")
  expect_identical(value, x)
  expect_error(as.data.frame(x, what = "unknown"), "arg")
})

test_that("domain plots include full index and share one display divisor", {
  x <- .domain_method_fixture()
  original <- x
  p <- plot(x)
  expect_s3_class(p, "patchwork")
  expect_equal(p[[1]]$data$Mean, c(0.5, 1, 2, 1, 2, 4))
  expect_identical(p[[2]]$data$definition, x$domains$definition)
  expect_identical(x, original)
  expect_s3_class(ggplot2::autoplot(x), "patchwork")
  for (type in c("coverage", "decomposition", "omission", "partition", "share")) {
    p <- plot(x, type = type)
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))
  }
  expect_true("Full index" %in% plot(x, type = "omission")$data$Series)
  expect_true("Full index" %in% plot(x, type = "partition")$data$class)
  expect_true("Full index" %in% plot(x, type = "share")$data$class)
  x$sampled_only[[1]]$table$Mean[] <- NA_real_
  expect_error(plot(x), "No common years")
})

test_that("domain maps validate retained cells and selectors", {
  x <- .domain_method_fixture()
  p <- plot(x, type = "map", years = "2002", terms = "spatial")
  expect_true(all(p$data$Year == "2002"))
  expect_no_error(ggplot2::ggplot_build(p))
  expect_error(plot(x, type = "map", years = "missing"), "selectors")
  expect_error(plot(x, type = "map", coords = c("missing", "Y")), "coordinate")
  x$retained_cells <- NULL
  expect_error(plot(x, type = "map"), "retained cell")
})

test_that("domain display normalisation uses only common valid years", {
  x <- .domain_method_fixture()
  x$sampled_only[[1]]$table$Mean[1] <- NA_real_
  d <- plot(x)[[1]]$data
  expect_equal(d$Mean[1:3], x$baseline$table$Mean / sqrt(4 * 8))
  x$sampled_only[[1]]$table$Mean[2] <- 0
  d <- plot(x)[[1]]$data
  expect_equal(d$Mean[1:3], x$baseline$table$Mean / 8)
})

test_that("domain plots preserve missing years and finite estimates without bounds", {
  x <- .domain_method_fixture()
  x$omission[[1]]$table <- x$omission[[1]]$table[c(1, 3), ]
  p <- plot(x, type = "omission")
  alternate <- p$data[p$data$Series == "spatial", ]
  expect_identical(alternate$.year, c(2001, 2003))
  expect_length(unique(alternate$.segment), 2)
  x$omission[[1]]$table$Qlower[1] <- NA_real_
  p <- plot(x, type = "omission")
  expect_true(all(is.finite(p$data$Mean)))
  expect_no_error(ggplot2::ggplot_build(p))
})
