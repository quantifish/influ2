test_that("domain index summaries preserve empty-domain semantics", {
  metadata <- list(method = "standardised", family = "poisson", link = "log",
    probs = c(0.025, 0.975))
  draws <- cbind(`1` = c(1, 2, 3), `2` = rep(NA_real_, 3))
  index <- .domain_index(c(`1` = 2, `2` = NA_real_), draws, metadata)
  expect_s3_class(index, "influ_index")
  expect_equal(index$table$SD, c(1, NA_real_))
  expect_null(index$covariance$log)
  expect_equal(index$covariance$response[1, 1], 1)
  empty <- .domain_index(c(`1` = NA_real_, `2` = NA_real_),
    matrix(NA_real_, 3, 2), metadata)
  expect_true(all(is.na(empty$table$Mean)))
  expect_true(all(is.na(empty$table$SD)))
})

test_that("domain trend differences use paired joint draws", {
  metadata <- list(method = "standardised", family = "poisson", link = "log",
    probs = c(0.025, 0.975))
  years <- as.character(c(2000, 2002, 2005))
  time <- as.numeric(years) - 2000
  draws <- exp(outer(c(0.1, 0.2, 0.3, 0.4), time))
  colnames(draws) <- years
  baseline <- .domain_index(stats::setNames(exp(0.2 * time), years), draws, metadata)
  changed <- .domain_index(stats::setNames(exp(0.5 * time), years),
    sweep(draws, 2, exp(0.3 * time), "*"), metadata)
  result <- list(baseline = baseline, sampled_only = list(cell_ever = changed),
    unsampled_only = baseline, omission = list())
  trend <- .domain_trends(result, years, years[1:2], metadata$probs)
  subset <- trend[trend$series == "cell_ever", ]
  expect_equal(subset$slope, rep(0.5, 2))
  expect_equal(subset$slope_difference, rep(0.3, 2))
  expect_equal(subset$difference_lower, rep(0.3, 2))
  expect_equal(subset$difference_upper, rep(0.3, 2))
  expect_equal(subset$correlation, c(1, 1))
  zero <- baseline
  zero$table$Mean[1] <- 0
  zero$draws[, 1] <- 0
  result$sampled_only <- list(empty = zero)
  trend <- .domain_trends(result, years, NULL, metadata$probs)
  expect_true(is.na(trend$slope[trend$series == "empty"]))
  expect_true(is.na(trend$difference_lower[trend$series == "empty"]))
})

test_that("domain reference rules require explicit fixed values", {
  expect_identical(.domain_rule("depth", "sampled_mean"), list(rule = "sampled_mean"))
  expect_identical(.domain_rule("depth", list(depth = "zero")), list(rule = "zero"))
  fixed <- list(rule = "fixed", value = list(depth = 40))
  expect_identical(.domain_rule("depth", list(depth = fixed)), fixed)
  expect_error(.domain_rule("depth", list(x = "zero")), "depth")
  for (bad in list(NA_character_, "fixed", c("zero", "sampled_mean"),
      list(rule = "fixed", value = list(depth = NA_real_)),
      list(rule = "fixed", value = list(depth = Inf)),
      list(rule = "fixed", value = list(depth = function() 40)),
      list(rule = "fixed", value = 40))) {
    expect_error(.domain_rule("depth", list(depth = bad)), "Covariate references")
  }
})

test_that("domain API rejects unsupported models without loading backends", {
  expect_error(index_domain(list(), data.frame(x = 1), sampled = "cell"),
    "requires an sdmTMB or univariate tinyVAST")
})

test_that("domain API keeps omission definitions independent of comparisons", {
  captured <- new.env(parent = emptyenv())
  data <- data.frame(year = factor(rep(1:2, each = 2)), cell = rep(1:2, 2))
  ref <- data.frame(cell = 1:2, X = c(0, 2), Y = c(0, 0))
  info <- list(data = data, year = "year", time = "year",
    family = "poisson", link = "log")
  local_mocked_bindings(
    .index_spatial_info = function(...) info,
    .domain_default_sampled = function(reference_data, coords) {
      captured$coords <- coords
      warning("Using half the grid spacing", call. = FALSE)
      list(coords = coords, distance = 1)
    },
    .domain_sampling = function(data, reference_data, year, years, sampled,
        sampled_definition, min_obs, min_years, weights, core_threshold) {
      captured$sampled <- sampled
      masks <- stats::setNames(lapply(sampled_definition,
        function(x) matrix(TRUE, 2, 2)), sampled_definition)
      list(counts = matrix(1L, 2, 2), cell_time = matrix(TRUE, 2, 2),
        masks = masks, class = matrix("sampled", 2, 2),
        coverage = data.frame(), domains = data.frame(), min_years = 1L)
    },
    .domain_reduce = function(model, info, years, newdata, weights, classes,
        masks, cell_time, omit, where, covariate_reference, decompose,
        retain_cells, positive, reference_data, settings, ...) {
      captured$settings <- settings
      captured$omission_definition <- settings$omission_definition
      captured$definitions <- names(masks)
      captured$weights <- weights
      captured$newdata <- newdata(c(1L, 2L), c(1L, 2L))
      index <- .domain_index(stats::setNames(c(1, 2), years), NULL, settings)
      partition <- expand.grid(Year = years,
        class = c("sampled", "previously_sampled", "never_sampled"),
        stringsAsFactors = FALSE)
      partition$share <- as.numeric(partition$class == "sampled")
      list(baseline = index, sampled_only = list(), omission = list(),
        unsampled_only = index, partition = partition)
    },
    .package = "influ2")
  model <- structure(list(mesh = list(xy_cols = c("X", "Y"))), class = "sdmTMB")
  expect_warning(domain <- index_domain(model, ref,
    sampled_definition = "cell_ever", reference_weights = c(1, 3),
    uncertainty = "none"), "half the grid spacing")
  expect_s3_class(domain, "influ_domain")
  expect_identical(captured$settings$sampled_definition, "cell_ever")
  expect_identical(captured$settings$omission_definition, "cell_time")
  expect_identical(captured$omission_definition, "cell_time")
  expect_identical(captured$definitions, "cell_ever")
  expect_equal(captured$weights, c(0.25, 0.75))
  expect_identical(captured$coords, c("X", "Y"))
  expect_identical(captured$sampled$distance, 1)
  expect_identical(as.character(captured$newdata$year), c("1", "2"))
  expect_identical(domain$metadata$sampled_rule, captured$sampled)
  expect_true(domain$metadata$sampled_heuristic)
  expect_identical(domain$baseline$metadata$sampled_rule, captured$sampled)
  expect_identical(domain$baseline$metadata$covariate_reference, "sampled_mean")
  expect_identical(domain$baseline$metadata$year_reference, "first year")
  index_domain(model, ref, sampled = "cell", sampled_definition = "cell_time",
    omission_definition = "cell_always", uncertainty = "none")
  expect_identical(captured$omission_definition, "cell_always")
  tiny <- structure(list(internal = list(space_columns = c("east", "north"))),
    class = "tinyVAST")
  tiny_ref <- stats::setNames(ref, c("cell", "east", "north"))
  expect_warning(index_domain(tiny, tiny_ref, uncertainty = "none"),
    "half the grid spacing")
  expect_identical(captured$coords, c("east", "north"))
  expect_error(index_domain(model, ref, sampled = "cell",
    prediction_offset = c("x", "y")), "prediction_offset")
  expect_error(index_domain(model, ref, sampled = "cell",
    reference_weights = c(0, 0)), "positive finite total")
})
