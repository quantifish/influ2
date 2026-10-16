domain_sampling_fixture <- function() {
  list(data = data.frame(cell = c("a", "a", "b", "b", "b", "outside"),
    year = c(2000, 2000, 2000, 2002, 2002, 2002)),
    reference = data.frame(cell = c("a", "b", "c")),
    years = c(2000, 2002, 2005))
}

test_that("default sampling uses half the nearest distinct centre spacing with a warning", {
  ref <- data.frame(X = c(0, 2, 4, 2), Y = c(0, 0, 0, 0))
  expect_warning(rule <- .domain_default_sampled(ref),
    "half the nearest distinct grid-centre spacing")
  expect_equal(rule, list(coords = c("X", "Y"), distance = 1))
  expect_warning(reordered <- .domain_default_sampled(ref[c(4, 3, 1, 2), ]),
    "heuristic")
  expect_equal(reordered, rule)
  diagonal <- data.frame(easting = c(0, 3), northing = c(0, 4))
  expect_warning(rule <- .domain_default_sampled(diagonal,
    c("easting", "northing")), "heuristic")
  expect_equal(rule, list(coords = c("easting", "northing"), distance = 2.5))
  irregular <- data.frame(X = c(0, 4, 4.5), Y = c(0, 0, 0))
  expect_warning(rule <- .domain_default_sampled(irregular), "heuristic")
  expect_equal(rule$distance, 0.25)
  data <- data.frame(year = c(1, 1), X = c(0, 0.25), Y = c(0, 0))
  counted <- .domain_sampling(data, irregular, "year", 1, rule, "cell_time")
  expect_equal(unname(counted$counts), matrix(c(2L, 0L, 0L), 3))
})

test_that("default sampling refuses missing, non-finite, or coincident centres", {
  for (ref in list(data.frame(X = 0, Y = 0),
      data.frame(X = c(0, 0), Y = c(0, 0)),
      data.frame(X = c(0, NA), Y = c(0, 1)),
      data.frame(X = c(0, Inf), Y = c(0, 1)),
      data.frame(X = c("0", "1"), Y = c(0, 1)),
      data.frame(x = c(0, 1), y = c(0, 1)))) {
    expect_error(.domain_default_sampled(ref), "Supply `sampled` explicitly",
      fixed = TRUE)
  }
  ref <- data.frame(X = c(0, 1), Y = c(0, 1))
  expect_error(.domain_default_sampled(ref, "X"), "`coords`")
  expect_error(.domain_default_sampled(ref, c("X", "X")), "`coords`")
  expect_error(.domain_default_sampled(ref, c("X", NA)), "`coords`")
})

test_that("explicit cell IDs count observations and threshold each year", {
  f <- domain_sampling_fixture()
  x <- .domain_sampling(f$data, f$reference, "year", f$years, "cell",
    min_obs = 2, min_years = 1, weights = c(2, 3, 5),
    sampled_definition = c("cell_time", "cell_ever"))
  expect_equal(unname(x$counts), matrix(c(2L, 1L, 0L, 0L, 2L, 0L,
    0L, 0L, 0L), 3))
  expect_identical(x$cell_time, x$counts >= 2)
  expect_identical(unname(x$class[, 1]),
    c("sampled", "previously_sampled", "never_sampled"))
  expect_identical(unname(x$class[, 3]),
    c("previously_sampled", "previously_sampled", "never_sampled"))
  expect_equal(unname(x$masks$cell_ever), matrix(c(TRUE, TRUE, FALSE), 3, 3))
  expect_identical(names(x$masks), c("cell_time", "cell_ever"))
  expect_identical(x$min_years, 1L)
  expect_equal(x$coverage$weight[x$coverage$Year == "2000"], c(2, 3, 5))
  expect_equal(tapply(x$coverage$share, x$coverage$Year, sum), rep(1, 3),
    ignore_attr = TRUE)
  expect_equal(x$domains$share, c(0.2, 0.3, 0, 0.5, 0.5, 0.5))
  expect_identical(x$domains$variable_domain, rep(c(TRUE, FALSE), each = 3))
})

test_that("minimum sampled years accepts counts and proportions rounded up", {
  f <- domain_sampling_fixture()
  args <- list(data = f$data, reference_data = f$reference, year = "year",
    years = f$years, sampled = "cell", sampled_definition = "cell_ever")
  count <- do.call(.domain_sampling, c(args, list(min_years = 2)))
  proportion <- do.call(.domain_sampling, c(args, list(min_years = 0.5)))
  expect_identical(count, proportion)
  expect_equal(unname(count$masks$cell_ever), matrix(c(FALSE, TRUE, FALSE), 3, 3))
  expect_identical(count$min_years, 2L)
  expect_warning(do.call(.domain_sampling, c(args, list(min_years = 3))), "empty")
  expect_warning(do.call(.domain_sampling, c(args,
    list(min_years = 2, weights = c(99, 1, 0)))), "small")
  expect_silent(do.call(.domain_sampling, c(args,
    list(min_years = 2, weights = c(99, 1, 0), core_threshold = 0))))
  expect_identical(unname(count$class[, 1]),
    c("sampled", "sampled", "never_sampled"))
  expect_warning(.domain_sampling(f$data, f$reference, "year", f$years,
    "cell", "cell_always", min_years = 1), "empty")
  always <- .domain_sampling(f$data, f$reference, "year", f$years[1:2],
    "cell", "cell_always", min_years = 1)
  expect_equal(unname(always$masks$cell_always), matrix(c(FALSE, TRUE, FALSE), 3, 2))
})

test_that("coordinate sampling counts distances inclusively without a dense array", {
  data <- data.frame(year = c(1, 1, 2), X = c(0, 1, 0), Y = c(0, 0, 2))
  ref <- data.frame(X = c(0, 3), Y = c(0, 0))
  x <- .domain_sampling(data, ref, "year", 1:2,
    list(coords = c("X", "Y"), distance = 1), "cell_time", min_obs = 2)
  expect_equal(unname(x$counts), matrix(c(2L, 0L, 0L, 0L), 2))
  expect_equal(unname(x$cell_time), matrix(c(TRUE, FALSE, FALSE, FALSE), 2))
  exact <- .domain_sampling(data, ref, "year", 1:2,
    list(coords = c("X", "Y"), distance = 0), "cell_time")
  expect_equal(unname(exact$counts), matrix(c(1L, 0L, 0L, 0L), 2))
})

test_that("logical indicators align optional dimension names explicitly", {
  ref <- data.frame(cell = c("a", "b"), row.names = c("a", "b"))
  data <- data.frame(year = c("early", "late"))
  m <- matrix(c(TRUE, TRUE, FALSE, FALSE), 2,
    dimnames = list(c("b", "a"), c("late", "early")))
  x <- .domain_sampling(data, ref, "year", c("early", "late"), m, "cell_time")
  expect_equal(x$counts, matrix(c(0L, 0L, 1L, 1L), 2,
    dimnames = list(c("a", "b"), c("early", "late"))))
  positional <- .domain_sampling(data, ref, "year", c("early", "late"),
    unname(m), "cell_time")
  expect_equal(unname(positional$counts), matrix(c(1L, 1L, 0L, 0L), 2))
  expect_error(.domain_sampling(data, ref, "year", c("early", "late"),
    m, min_obs = 2), "indicator")
  colnames(m) <- c("early", "unknown")
  expect_error(.domain_sampling(data, ref, "year", c("early", "late"),
    m), "dimnames")
  expect_error(.domain_sampling(data, ref, "year", c("early", "late"),
    matrix(NA, 2, 2)), "without NA")
  expect_error(.domain_sampling(data, ref, "year", c("early", "late"),
    matrix(TRUE, 1, 2)), "logical.*matrix")
})

test_that("sampling inputs and thresholds fail early on ambiguous values", {
  f <- domain_sampling_fixture()
  call <- function(...) .domain_sampling(f$data, f$reference, "year", f$years, ...)
  expect_error(call(), "`sampled` is required", fixed = TRUE)
  expect_error(call(NULL), "`sampled` is required", fixed = TRUE)
  expect_error(call("missing"), "shared cell-ID")
  expect_error(call(matrix(1, 3, 3)), "logical matrix")
  for (value in list(0, -1, 1.5, NA_real_, Inf, TRUE, c(1, 2))) {
    expect_error(call("cell", min_obs = value), "min_obs")
  }
  for (value in list(0, -1, 1.5, 4, NA_real_, Inf, TRUE)) {
    expect_error(call("cell", min_years = value), "min_years")
  }
  for (value in list(c(1, 2), c(0, 0, 0), c(1, NA, 1),
      c(1, -1, 1), c(1, Inf, 1))) {
    expect_error(call("cell", weights = value), "weights")
  }
  for (value in list(-1, 1.1, NA_real_, Inf)) {
    expect_error(call("cell", core_threshold = value), "core_threshold")
  }
  expect_error(call("cell", sampled_definition = "auto"), "sampled_definition")
  expect_error(call("cell", sampled_definition = c("cell_time", "cell_time")),
    "sampled_definition")
  expect_error(.domain_sampling(f$data, f$reference, "missing", f$years, "cell"),
    "`year`")
  expect_error(.domain_sampling(f$data, f$reference, "year", c(2000, 2000), "cell"),
    "`years`")
  f$data$year[1] <- NA
  expect_error(call("cell"), "year.*non-missing")
})

test_that("missing IDs and malformed coordinates cannot silently imply unsampled cells", {
  f <- domain_sampling_fixture()
  f$reference$cell[1] <- NA
  expect_error(.domain_sampling(f$data, f$reference, "year", f$years, "cell"),
    "Cell IDs")
  f$reference$cell <- c("a", "a", "b")
  expect_error(.domain_sampling(f$data, f$reference, "year", f$years, "cell"),
    "unique")
  data <- data.frame(year = 1, X = 0, Y = 0)
  ref <- data.frame(X = 0, Y = 0)
  for (distance in list(-1, NA_real_, Inf, c(1, 2))) {
    expect_error(.domain_sampling(data, ref, "year", 1,
      list(coords = c("X", "Y"), distance = distance)), "distance")
  }
  expect_error(.domain_sampling(data, ref, "year", 1,
    list(coords = "X", distance = 1)), "Coordinate")
  ref$X <- NA_real_
  expect_error(.domain_sampling(data, ref, "year", 1,
    list(coords = c("X", "Y"), distance = 1)), "finite numeric")
})

test_that("single-cell and empty-observation inputs preserve matrix shapes", {
  x <- .domain_sampling(data.frame(year = numeric(), cell = character()),
    data.frame(cell = "a"), "year", 2000, "cell", "cell_time")
  expect_identical(dim(x$counts), c(1L, 1L))
  expect_identical(dim(x$cell_time), c(1L, 1L))
  expect_equal(x$coverage$cells, c(0, 0, 1))
  expect_equal(x$domains$share, 0)
  expect_identical(unname(x$class[1, 1]), "never_sampled")
})
