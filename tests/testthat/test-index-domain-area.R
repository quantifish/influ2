domain_area_reduce_fixture <- function() {
  reduce <- influ2:::.domain_reduce
  env <- new.env(parent = environment(reduce))
  env$.index_free_tmb <- function(...) invisible(NULL)
  env$.domain_projection <- function(model, backend, data, offset, year = NULL) {
    terms <- function(par) {
      m <- function(x) matrix(x, ncol = 1L,
        dimnames = list(NULL, "conditional"))
      list(intercept = m(rep(1, nrow(data))),
        year = m(as.numeric(as.character(data$year)) / 1000),
        x = m(data$x))
    }
    list(obj = list(), terms = terms, response = function(par, subtract = NULL) {
      eta <- Reduce(`+`, terms(par))
      if (!is.null(subtract)) eta <- eta - subtract
      as.numeric(exp(eta))
    })
  }
  environment(reduce) <- env
  years <- c("2000", "2001", "2002")
  ref <- data.frame(x = c(0, 0.1, 0.2, 0.3))
  sampled <- matrix(c(TRUE, TRUE, FALSE, FALSE,
    TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE), 4L, 3L)
  classes <- matrix("previously_sampled", 4L, 3L)
  classes[4L, ] <- "never_sampled"
  classes[sampled] <- "sampled"
  args <- list(model = list(), info = list(mode = 0, year = "year"),
    years = years, newdata = function(y, rows) {
      d <- ref[rows, , drop = FALSE]
      d$year <- as.numeric(years[y])
      d
    }, weights = c(2, 1.5, 10, 7), area = c(2, 3, 5, 7),
    classes = classes, masks = list(cell_time = sampled,
      cell_ever = matrix(c(TRUE, TRUE, TRUE, FALSE), 4L, 3L),
      cell_always = matrix(c(TRUE, FALSE, FALSE, FALSE), 4L, 3L)),
    cell_time = sampled, omit = character(), where = "all",
    covariate_reference = "zero", decompose = FALSE, retain_cells = NULL,
    positive = c(2L, 4L, 5L, 7L), reference_data = ref,
    settings = list(backend = "mock", method = "integrated", year = "year",
      scale = "response", units = "kg", area_units = "km^2",
      response_units = "kg/km^2", total_area = 20.5,
      catchability = "not adjusted", probs = c(0.025, 0.975),
      reference = "fixed common domain; area-weighted response total",
      rescale = "raw"), uncertainty = FALSE, ndraws = 2L,
    batch_size = 3L, draw_batch_size = 2L, seed = 1L, offset = NULL,
    sampled_only = TRUE)
  run <- function(...) {
    changes <- list(...)
    a <- args
    a[names(changes)] <- changes
    do.call(reduce, a)
  }
  list(args = args, run = run, reduce = reduce)
}

test_that("restricted totals report their actual fixed or annual integration area", {
  f <- domain_area_reduce_fixture()
  x <- f$run()
  annual <- stats::setNames(c(3.5, 13.5, 2), f$args$years)
  expect_equal(x$baseline$metadata$total_area, 20.5)
  expect_equal(x$baseline$metadata$parent_total_area, 20.5)
  expect_equal(x$sampled_only$cell_ever$metadata$total_area, 13.5)
  expect_equal(x$sampled_only$cell_always$metadata$total_area, 2)
  expect_equal(x$sampled_only$cell_time$metadata$total_area, annual)
  expect_equal(x$sampled_only$cell_time$metadata$retained_area, annual)
  expect_equal(x$sampled_only$cell_time$metadata$retained_weight, annual)
  expect_equal(x$unsampled_only$metadata$total_area, 20.5 - annual)
  for (restricted in c(x$sampled_only, list(x$unsampled_only))) {
    expect_equal(restricted$metadata$parent_total_area, 20.5)
  }
  # Numerical totals are unchanged by the metadata correction.
  expected <- vapply(seq_along(f$args$years), function(y) {
    value <- exp(1 + as.numeric(f$args$years[y]) / 1000 + f$args$reference_data$x)
    sum(value * f$args$weights * f$args$cell_time[, y])
  }, numeric(1))
  expect_equal(x$sampled_only$cell_time$table$Mean, expected)
})

test_that("raw row areas stay distinct from effective averaging-weighted areas", {
  f <- domain_area_reduce_fixture()
  x <- f$run()
  expect_equal(x$baseline$metadata$parent_row_area, 17)
  expect_equal(x$sampled_only$cell_ever$metadata$retained_row_area,
    stats::setNames(rep(10, 3L), f$args$years))
  expect_equal(x$sampled_only$cell_time$metadata$retained_row_area,
    stats::setNames(c(5, 10, 2), f$args$years))
  expect_match(x$baseline$metadata$row_area_interpretation,
    "not necessarily unique geographic area", fixed = TRUE)
  expect_match(x$baseline$metadata$area_convention,
    "averaging weights are not normalised internally", fixed = TRUE)
  repeated <- f$run(area = c(2, 2, 5, 5), weights = c(0.5, 1.5, 2, 3))
  expect_equal(repeated$baseline$metadata$total_area, 7)
  expect_equal(repeated$baseline$metadata$parent_row_area, 14)
  expect_equal(repeated$sampled_only$cell_ever$metadata$total_area, 4)
  # Older direct reducer fixtures cannot claim raw geographic area provenance.
  unknown <- f$run(area = NULL)
  expect_null(unknown$baseline$metadata$parent_row_area)
  expect_null(unknown$sampled_only$cell_time$metadata$retained_row_area)
  expect_error(f$run(area = c(2, 3)), "align with positive-weight")
  expect_error(f$run(area = c(2, 3, 0, 7)), "align with positive-weight")
})

test_that("ordinary index comparisons retain cross-domain area safeguards", {
  f <- domain_area_reduce_fixture()
  x <- f$run()
  for (restricted in x$sampled_only[c("cell_ever", "cell_time")]) {
    pair <- list(Full = x$baseline, Restricted = restricted)
    expect_error(plot_compare(pair), "agree on area")
    expect_error(index_ratio(pair, normalise = "common"), "total_area")
  }
  all_sampled <- f$args$cell_time
  all_sampled[] <- TRUE
  same <- f$run(cell_time = all_sampled,
    classes = matrix("sampled", 4L, 3L),
    masks = lapply(f$args$masks, function(mask) {
      mask[] <- TRUE
      mask
    }))
  expect_equal(same$sampled_only$cell_time$metadata$total_area,
    same$baseline$metadata$total_area)
  expect_s3_class(plot_compare(list(Full = same$baseline,
    Same_domain = same$sampled_only$cell_time)), "ggplot")
})

test_that("public area inputs stay aligned after zero-weight rows are discarded", {
  f <- domain_area_reduce_fixture()
  public <- influ2:::index_domain
  env <- new.env(parent = environment(public))
  captured <- NULL
  env$.index_spatial_info <- function(...) list(
    data = data.frame(year = factor(rep(f$args$years, each = 4L))),
    year = "year", time = "year", mode = 0, family = "poisson", link = "log")
  env$.domain_sampling_data <- function(model, backend, info) info$data
  env$.domain_reduce <- function(..., area = NULL) {
    captured <<- area
    do.call(f$reduce, list(..., area = area))
  }
  environment(public) <- env
  reference <- data.frame(x = c(0, NA_real_, 0.1, 0.2, 0.3),
    cell_area = c(2, 0, 3, 5, 7), averaging = c(1, 20, 0.5, 2, 1))
  sampled <- matrix(FALSE, 5L, 3L)
  sampled[c(1L, 3L, 4L, 5L), ] <- f$args$cell_time
  result <- public(structure(list(), class = "sdmTMB"), reference,
    year = "year", sampled = sampled, area = "cell_area",
    reference_weights = "averaging", area_units = "km^2",
    response_units = "kg/km^2", uncertainty = "none", decompose = FALSE)
  expect_equal(captured, c(2, 3, 5, 7))
  expect_equal(result$baseline$metadata$total_area, 20.5)
  expect_equal(result$sampled_only$cell_ever$metadata$total_area, 13.5)
  expect_equal(result$sampled_only$cell_time$metadata$retained_row_area,
    stats::setNames(c(5, 10, 2), f$args$years))
  expect_equal(result$sampled_only$cell_time$table$Mean,
    f$run()$sampled_only$cell_time$table$Mean)
})

test_that("native GAM domain totals agree with explicit restricted integration", {
  skip_if_not_installed("mgcv")
  f <- domain_area_reduce_fixture()
  data <- data.frame(year = factor(rep(2000:2002, each = 12L)),
    x = rep(c(0, 0.1, 0.2, 0.3), 9L),
    response = rep(c(1, 2, 3, 4, 4, 3, 2, 1, 2, 3, 4, 5), 3L))
  model <- mgcv::gam(response ~ year + x, family = poisson(), data = data)
  reference <- data.frame(x = c(0, NA_real_, 0.1, 0.2, 0.3),
    cell_area = c(2, 0, 3, 5, 7), averaging = c(1, 20, 0.5, 2, 1))
  sampled <- matrix(FALSE, 5L, 3L)
  positive <- c(1L, 3L, 4L, 5L)
  sampled[positive, ] <- f$args$cell_time
  result <- index_domain(model, reference, year = "year", sampled = sampled,
    area = "cell_area", reference_weights = "averaging", area_units = "km^2",
    response_units = "kg/km^2", uncertainty = "none", decompose = FALSE)
  integrate <- function(rows) integrate_index(model, reference[rows, , drop = FALSE],
    area = "cell_area", averaging_weights = "averaging", year = "year",
    area_units = "km^2", response_units = "kg/km^2", uncertainty = "none")
  expected <- integrate(positive)
  restricted <- integrate(positive[1:3])
  expect_equal(result$baseline$table$Mean, expected$table$Mean)
  expect_equal(result$baseline$metadata$total_area, expected$metadata$total_area)
  expect_equal(result$sampled_only$cell_ever$table$Mean, restricted$table$Mean)
  expect_equal(result$sampled_only$cell_ever$metadata$total_area,
    restricted$metadata$total_area)
  expect_equal(result$sampled_only$cell_time$metadata$retained_area,
    stats::setNames(c(3.5, 13.5, 2), f$args$years))
  expect_equal(result$sampled_only$cell_time$metadata$retained_row_area,
    stats::setNames(c(5, 10, 2), f$args$years))
  for (field in c("total_area", "parent_total_area", "area_convention",
      "parent_row_area", "row_area_interpretation")) {
    expect_identical(result$metadata[[field]], result$baseline$metadata[[field]])
  }
})
