ratio_index_fixture <- function(year = c("2000", "2001", "2002"),
    mean = c(1, 2, 3), method = "standardised", scale = "response",
    units = "lobsters per pot") {
  .new_cpue_index(data.frame(Year = as.character(year), Mean = mean,
    Median = NA_real_, SD = mean / 10, CV = 0.1,
    Qlower = mean * 0.8, Qupper = mean * 1.2),
    list(method = method, scale = scale, units = units, year = "year",
      backend = "test", family = "poisson", link = "log", rescale = "raw",
      response = "cpue", component = "combined", probs = c(0.05, 0.95)))
}

ratio_diag_fixture <- function(years = c("2000", "2001", "2002"),
    standardised = c(0.5, 1, 2), nominal = c(2, 4, 16),
    component = "conditional") {
  structure(list(focus = "year", indices = rbind(
    data.frame(level = years, estimate = standardised,
      scale = "ratio", series = "standardised", component = component),
    data.frame(level = years, estimate = nominal,
      scale = "response", series = "nominal", component = component)),
    metadata = list(response = "cpue", reference = "observed")),
    class = "influ_diag")
}

test_that("index ratios retain original estimates and isolate calculation from plotting", {
  a <- ratio_index_fixture()
  b <- ratio_index_fixture(mean = c(2, 4, 6))
  source <- list(Base = a, Double = b)
  original <- source
  set.seed(84); rng <- .Random.seed
  result <- index_ratio(source, reference = "Base")
  expect_s3_class(result, "influ_index_ratio")
  expect_equal(result$table$ratio, rep(2, 3))
  expect_identical(result$table$numerator, rep("Double", 3))
  expect_identical(result$table$denominator, rep("Base", 3))
  expect_equal(result$table$numerator_estimate, b$table$Mean)
  expect_equal(result$table$denominator_estimate, a$table$Mean)
  expect_equal(result$table$numerator_display, b$table$Mean)
  expect_true(all(result$table$status == "ok"))
  expect_identical(as.data.frame(result), result$table)
  expect_identical(print(result), result)
  expect_equal(summary(result)$minimum, 2)
  expect_equal(summary(result)$defined_years, 3L)
  expect_true(all(c("method", "component", "response", "units") %in%
    result$metadata$verified_fields))
  expect_identical(result$metadata$uncertainty,
    "point estimates only; cross-series covariance not assumed")
  expect_false(any(grepl("lower|upper|std_error|Qlower|Qupper", names(result$table))))
  expect_s3_class(plot_index_ratio(result), "ggplot")
  expect_equal(ggplot2::ggplot_build(plot(result))$data,
    ggplot2::ggplot_build(autoplot.influ_index_ratio(result))$data)
  expect_identical(source, original)
  expect_identical(.Random.seed, rng)
  expect_equal(index_ratio(source)$table, result$table)
  inverse <- index_ratio(source, reference = "Double")
  expect_equal(inverse$table$ratio, rep(0.5, 3))
})

test_that("unequal and disjoint year ranges produce explicit undefined rows", {
  a <- ratio_index_fixture(year = c("2000", "2001", "2002", "2003"), mean = 1:4)
  b <- ratio_index_fixture(year = c("2001", "2003", "2004"), mean = c(4, 8, 10))
  result <- index_ratio(list(A = a, B = b))
  expect_identical(result$table$year, as.character(2000:2004))
  expect_equal(result$table$ratio, c(NA, 2, NA, 2, NA))
  expect_identical(result$table$status, c("missing_numerator", "ok",
    "missing_numerator", "ok", "missing_denominator"))
  expect_equal(summary(result)$undefined_years, 3L)
  p <- plot(result)
  expect_match(p$labels$caption, "3 undefined")
  line <- ggplot2::ggplot_build(p)$data[[2L]]
  expect_length(unique(line$group), 2L)
  disjoint <- index_ratio(list(A = a,
    B = ratio_index_fixture(year = c("2010", "2011"), mean = c(1, 2))))
  expect_true(all(is.na(disjoint$table$ratio)))
  expect_equal(summary(disjoint)$defined_years, 0L)
  expect_true(is.na(summary(disjoint)$minimum))
  expect_s3_class(ggplot2::ggplot_build(plot(disjoint)), "ggplot_built")
  expect_error(index_ratio(list(A = a,
    B = ratio_index_fixture(year = c("2010", "2011"), mean = c(1, 2))),
    normalise = "common"), "shared year")
})

test_that("both globally missing numeric years and non-numeric gaps split lines", {
  a <- ratio_index_fixture(year = c("2000", "2002", "2003"))
  b <- ratio_index_fixture(year = c("2000", "2002", "2003"), mean = c(2, 4, 6))
  lines <- ggplot2::ggplot_build(plot(index_ratio(list(A = a, B = b))))$data[[2L]]
  expect_length(unique(lines$group), 2L)
  a$table$Year <- c("1999/00", "2000/01", "2001/02")
  b$table$Year <- c("1999/00", "2001/02", "2002/03")
  result <- index_ratio(list(A = a, B = b))
  expect_identical(result$table$year,
    c("1999/00", "2000/01", "2001/02", "2002/03"))
  p <- ggplot2::ggplot_build(plot(result))
  expect_equal(p$layout$panel_params[[1]]$x$get_labels(), result$table$year)
  expect_length(unique(p$data[[2L]]$group), 2L)
})

test_that("canonical fishing-year gaps are displayed without changing calculated rows", {
  for (years in list(c("2010/11", "2012/13", "2013/14"),
      c("2010-2011", "2012-2013", "2013-2014"))) {
    a <- ratio_index_fixture(year = years)
    b <- ratio_index_fixture(year = years, mean = c(2, 4, 6))
    result <- index_ratio(list(A = a, B = b))
    expect_identical(result$table$year, years)
    expected <- if (grepl("/", years[1], fixed = TRUE)) {
      c("2010/11", "2011/12", "2012/13", "2013/14")
    } else c("2010-2011", "2011-2012", "2012-2013", "2013-2014")
    built <- ggplot2::ggplot_build(plot(result))
    expect_identical(built$layout$panel_params[[1L]]$x$get_labels(), expected)
    expect_length(unique(built$data[[2L]]$group), 2L)
    expect_equal(as.numeric(built$data[[2L]]$x), c(1, 3, 4))
  }
  a <- ratio_index_fixture(year = c("2010/11", "2011/12", "2012/13"))
  b <- ratio_index_fixture(year = a$table$Year, mean = c(2, NA, 6))
  result <- index_ratio(list(A = a, B = b))
  expect_identical(result$table$status, c("ok", "missing_numerator", "ok"))
  built <- ggplot2::ggplot_build(plot(result))
  expect_length(unique(built$data[[2L]]$group), 2L)
  expect_equal(as.numeric(built$data[[2L]]$x), c(1, 3))
  labels <- c("late", "early", "middle")
  a$table$Year <- b$table$Year <- labels
  b$table$Mean <- c(2, 4, 6)
  dateless <- index_ratio(list(A = a, B = b))
  built <- ggplot2::ggplot_build(plot(dateless))
  expect_identical(dateless$table$year, labels)
  expect_identical(built$layout$panel_params[[1L]]$x$get_labels(), labels)
  expect_length(unique(built$data[[2L]]$group), 1L)
})

test_that("common normalisation uses one shared positive baseline rather than separate windows", {
  a <- ratio_index_fixture(year = as.character(2000:2004), mean = c(100, 2, 4, 8, 100))
  b <- ratio_index_fixture(year = as.character(2001:2003), mean = c(4, 8, 16))
  raw <- index_ratio(list(A = a, B = b))
  expect_equal(raw$table$ratio[2:4], rep(2, 3))
  common <- index_ratio(list(A = a, B = b), normalise = "common")
  expect_identical(common$metadata$baseline_years, as.character(2001:2003))
  expect_equal(common$metadata$divisors, c(A = 4, B = 8))
  expect_equal(common$table$ratio[2:4], rep(1, 3))
  expect_identical(common$table$numerator_estimate, raw$table$numerator_estimate)
  expect_match(plot(common)$labels$subtitle, "2001, 2002, 2003")
  specific <- index_ratio(list(A = a, B = b), normalise = "common",
    baseline_years = 2001)
  expect_equal(specific$metadata$divisors, c(A = 2, B = 4))
  b$table$Mean[2] <- 0
  positive <- index_ratio(list(A = a, B = b), normalise = "common")
  expect_identical(positive$metadata$baseline_years, c("2001", "2003"))
  expect_equal(positive$table$ratio[3], 0)
  expect_error(index_ratio(list(A = a, B = b), normalise = "common",
    baseline_years = "2002"), "positive finite estimate")
  expect_error(index_ratio(list(A = a, B = b), baseline_years = 2001), "requires")
  for (bad in list(character(), NA, c("2001", "2001"), "missing",
      matrix("2001"), list("2001"))) {
    expect_error(index_ratio(list(A = a, B = b), normalise = "common",
      baseline_years = bad), "baseline_years")
  }
})

test_that("common normalisation is shared across every numerator and denominator", {
  a <- ratio_index_fixture(year = as.character(2000:2003), mean = c(1, 2, 4, 8))
  b <- ratio_index_fixture(year = as.character(2001:2003), mean = c(2, 4, 8))
  c <- ratio_index_fixture(year = as.character(2002:2003), mean = c(8, 16))
  r <- index_ratio(list(A = a, B = b, C = c), normalise = "common")
  expect_identical(r$metadata$baseline_years, c("2002", "2003"))
  expect_equal(r$metadata$divisors, c(A = sqrt(32), B = sqrt(32), C = sqrt(128)))
  expect_equal(r$table$ratio[r$table$status == "ok"], rep(1, 5))
  expect_identical(r$metadata$comparisons$numerator, c("B", "C"))
})

test_that("zero denominators and nonfinite estimates are never clipped or divided", {
  a <- ratio_index_fixture(mean = c(0, 2, 3))
  b <- ratio_index_fixture(mean = c(2, 0, 6))
  r <- index_ratio(list(A = a, B = b))
  expect_identical(r$table$status, c("zero_denominator", "ok", "ok"))
  expect_equal(r$table$ratio, c(NA, 0, 2))
  expect_error(plot(r, log = TRUE), "zero numerators")
  expect_s3_class(plot(r), "ggplot")
  a$table$Mean <- c(Inf, NA, Inf)
  b$table$Mean <- c(2, Inf, Inf)
  r <- index_ratio(list(A = a, B = b))
  expect_identical(r$table$status,
    c("nonfinite_denominator", "nonfinite_numerator", "nonfinite_both"))
  expect_true(all(is.na(r$table$ratio)))
  huge <- index_ratio(list(A = ratio_index_fixture(mean = rep(1e-300, 3)),
    B = ratio_index_fixture(mean = rep(1e300, 3))))
  expect_true(all(huge$table$status == "nonfinite_ratio"))
  expect_true(all(is.na(huge$table$ratio)))
  underflow <- index_ratio(list(A = ratio_index_fixture(mean = rep(1e300, 3)),
    B = ratio_index_fixture(mean = rep(1e-300, 3))))
  expect_true(all(underflow$table$status == "underflow_ratio"))
  expect_true(all(is.na(underflow$table$ratio)))
  tiny <- index_ratio(list(A = ratio_index_fixture(mean = rep(1e-300, 3)),
    B = ratio_index_fixture(mean = rep(2e-300, 3))))
  expect_equal(tiny$table$ratio, rep(2, 3), tolerance = 1e-12)
  expect_s3_class(ggplot2::ggplot_build(plot(tiny, log = TRUE)), "ggplot_built")
  extreme_a <- ratio_index_fixture(mean = c(1e-300, 1e300, 1e300))
  extreme_b <- ratio_index_fixture(mean = 2 * extreme_a$table$Mean)
  normalised <- index_ratio(list(A = extreme_a, B = extreme_b), normalise = "common")
  expect_true(is.na(normalised$table$numerator_display[1]))
  expect_true(is.na(normalised$table$denominator_display[1]))
  expect_equal(normalised$table$numerator_estimate[1], 2e-300)
  expect_equal(normalised$table$ratio, rep(1, 3), tolerance = 1e-12)
  expect_true(all(normalised$table$status == "ok"))
})

test_that("compatible units and target metadata are checked without backend restrictions", {
  a <- ratio_index_fixture()
  b <- ratio_index_fixture(mean = c(2, 4, 6))
  for (field in c("method", "year", "response", "component", "units")) {
    bad <- b
    bad$metadata[[field]] <- if (field == "method") "integrated" else "different"
    expect_error(index_ratio(list(A = a, B = bad)), "incompatible")
  }
  for (scale in c("difference", "link", NA, "unsupported")) {
    bad <- b; bad$metadata$scale <- scale
    expect_error(index_ratio(list(A = a, B = bad)), "scale")
  }
  bad <- b; bad$table$Mean[1] <- -1
  expect_error(index_ratio(list(A = a, B = bad)), "non-negative")
  bad <- b; bad$metadata$method <- NULL
  expect_error(index_ratio(list(A = a, B = bad)), "method")
  b$metadata$backend <- "another_backend"
  b$metadata$random_effects <- "different fitted convention"
  b$metadata$response <- NULL
  r <- index_ratio(list(A = a, B = b))
  expect_equal(r$table$ratio, rep(2, 3))
  expect_true("response" %in% r$metadata$unverified_fields)
  expect_match(paste(r$metadata$notes, collapse = " "), "random_effects")
  integrated_a <- a; integrated_b <- b
  integrated_a$metadata$method <- integrated_b$metadata$method <- "integrated"
  for (field in c("area_units", "response_units", "total_area", "catchability")) {
    integrated_a$metadata[[field]] <- 1
    integrated_b$metadata[[field]] <- 2
    expect_error(index_ratio(list(A = integrated_a, B = integrated_b)), field)
    integrated_b$metadata[[field]] <- 1
  }
  expect_s3_class(index_ratio(list(A = integrated_a, B = integrated_b)), "influ_index_ratio")
})

test_that("separately rescaled input periods cannot silently define a common anchor", {
  a <- ratio_index_fixture()
  b <- ratio_index_fixture(year = c("2001", "2002"), mean = c(1, 3))
  a$metadata$scale <- b$metadata$scale <- "ratio"
  a$metadata$rescale <- b$metadata$rescale <- 1
  expect_error(index_ratio(list(A = a, B = b)), "different periods")
  expect_s3_class(index_ratio(list(A = a, B = b), normalise = "common"), "influ_index_ratio")
  b <- a; b$metadata$rescale <- 2
  expect_error(index_ratio(list(A = a, B = b)), "different rescaling targets")
  expect_s3_class(index_ratio(list(A = a, B = b), normalise = "common"), "influ_index_ratio")
})

test_that("standardised versus nominal comparisons are explicit shape comparisons", {
  diagnostic <- ratio_diag_fixture()
  expect_error(index_ratio(diagnostic), "explicitly select")
  expect_error(index_ratio(diagnostic, series = c("standardised", "nominal")), "shape comparison")
  r <- index_ratio(diagnostic, series = c("standardised", "nominal"),
    reference = "nominal", normalise = "common")
  expected <- (c(0.5, 1, 2) / geo_mean(c(0.5, 1, 2))) /
    (c(2, 4, 16) / geo_mean(c(2, 4, 16)))
  expect_equal(r$table$ratio, expected)
  expect_identical(r$table$numerator, rep("standardised", 3))
  alias <- index_ratio(diagnostic, series = c("standardized", "nominal"),
    reference = "nominal", normalise = "common")
  expect_equal(alias$table, r$table)
  expect_identical(diagnostic$indices$estimate, c(0.5, 1, 2, 2, 4, 16))
  b <- diagnostic; b$indices$estimate[b$indices$series == "standardised"] <- c(1, 2, 4)
  expect_equal(index_ratio(list(A = diagnostic, B = b))$table$ratio, rep(2, 3))
  expect_s3_class(index_ratio(list(A = diagnostic, B = b), series = "nominal"), "influ_index_ratio")
  expect_error(index_ratio(list(A = diagnostic, B = b),
    series = c("nominal", "standardised")), "exactly one")
  multi <- diagnostic
  other <- diagnostic$indices; other$component <- "positive"
  multi$indices <- rbind(multi$indices, other)
  expect_error(index_ratio(list(A = multi, B = multi)), "Multiple components")
  expect_equal(index_ratio(list(A = multi, B = multi), component = "positive")$table$ratio,
    rep(1, 3))
  expect_error(index_ratio(list(A = multi, B = multi), component = "absent"), "not available")
})

test_that("stored step ratios use previous, first, or explicit denominator without refits", {
  diags <- list(ratio_diag_fixture(), ratio_diag_fixture(standardised = c(1, 2, 4)),
    ratio_diag_fixture(standardised = c(4, 8, 16)))
  labels <- c("Year", "Depth", "Vessel")
  indices <- do.call(rbind, lapply(seq_along(diags), function(i) {
    d <- diags[[i]]$indices
    d <- d[d$series == "standardised", ]
    d$step_id <- i; d$Model <- labels[i]
    d
  }))
  steps <- structure(list(focus = "year", indices = indices,
    steps = data.frame(step_id = 1:3, label = labels),
    metadata = list(response = "cpue", component = "conditional", scale = "ratio",
      reference = "observed"), fits = list(function() stop("Do not refit"))),
    class = "influ_steps")
  original <- steps
  previous <- index_ratio(steps)
  expect_identical(previous$metadata$comparisons$denominator, c("Year", "Depth"))
  expect_equal(previous$table$ratio, c(rep(2, 3), rep(4, 3)))
  first <- index_ratio(steps, reference = "first")
  expect_identical(first$metadata$comparisons$denominator, c("Year", "Year"))
  expect_equal(first$table$ratio, c(rep(2, 3), rep(8, 3)))
  explicit <- index_ratio(steps, reference = "Depth")
  expect_equal(explicit$table$ratio, c(rep(0.5, 3), rep(4, 3)))
  relabel <- index_ratio(steps, labels = c("A", "B", "C"))
  expect_identical(relabel$metadata$comparisons$numerator, c("B", "C"))
  expect_identical(steps, original)
  expect_null(previous$fits)
  expect_error(index_ratio(steps, series = "nominal"), "only used with influence")
  malformed <- steps; malformed$steps$step_id <- NULL
  expect_error(index_ratio(malformed), "incomplete")
})

test_that("ratio line identity cannot collide when model labels contain slashes", {
  labels <- c("B / A", "A", "A / B")
  indices <- do.call(rbind, lapply(1:3, function(i) {
    data.frame(level = c("2000", "2001"), estimate = c(1, 2) * c(1, 2, 8)[i],
      scale = "ratio", series = "standardised", component = "conditional",
      step_id = i, Model = labels[i])
  }))
  steps <- structure(list(focus = "year", indices = indices,
    steps = data.frame(step_id = 1:3, label = labels),
    metadata = list(response = "cpue", component = "conditional", scale = "ratio")),
    class = "influ_steps")
  result <- index_ratio(steps)
  expect_equal(result$table$ratio, c(2, 2, 4, 4))
  built <- ggplot2::ggplot_build(plot(result))
  line <- built$data[[2L]]
  expect_length(unique(line$group), 2L)
  expect_length(unique(line$colour), 2L)
  expect_equal(vapply(split(line$y, line$group), function(y) diff(range(y)),
    numeric(1)), c("1" = 0, "2" = 0), tolerance = 1e-12)
  expect_equal(built$plot$scales$get_scales("colour")$get_labels(),
    c("A / (B / A)", "(A / B) / A"))
  expect_length(unique(built$plot$data$pair_id), 2L)
})

test_that("singleton and entirely undefined comparisons retain valid plot structure", {
  singleton <- index_ratio(list(A = ratio_index_fixture("2000", 1),
    B = ratio_index_fixture("2000", 2)))
  built <- ggplot2::ggplot_build(plot(singleton))
  expect_equal(nrow(built$data[[3L]]), 1L)
  expect_equal(built$data[[3L]]$y, 2)
  disjoint <- index_ratio(list(A = ratio_index_fixture("2000", 1),
    B = ratio_index_fixture("2001", 2)))
  built <- ggplot2::ggplot_build(plot(disjoint))
  expect_equal(nrow(built$data[[2L]]), 0L)
  expect_equal(nrow(built$data[[3L]]), 0L)
  expect_equal(built$data[[1L]]$yintercept, 1)
  expect_equal(summary(disjoint)$undefined_years, 2L)
  empty <- singleton; empty$table <- empty$table[FALSE, ]
  expect_error(plot(empty), "no annual rows")
})

test_that("native calculated GLM indices give their manual annual ratios", {
  withr::local_seed(71)
  d <- data.frame(year = factor(rep(2000:2003, each = 25)), x = runif(100), z = runif(100))
  d$cpue <- rpois(100, exp(0.2 * as.numeric(d$year) + d$x + d$z))
  a <- glm(cpue ~ year + x, poisson(), data = d)
  b <- glm(cpue ~ year + x + z, poisson(), data = d)
  ia <- cpue_index(a, reference_data = data.frame(x = 0.5))
  ib <- cpue_index(b, reference_data = data.frame(x = 0.5, z = 0.5))
  rng <- .Random.seed
  r <- index_ratio(list(X = ia, XZ = ib))
  expect_equal(r$table$ratio, ib$table$Mean / ia$table$Mean)
  expect_identical(.Random.seed, rng)
  expect_true("response" %in% r$metadata$unverified_fields)
  diagnostic <- influ(a, focus = "year")
  nominal <- index_ratio(diagnostic, series = c("standardised", "nominal"),
    reference = "nominal", normalise = "common")
  expect_equal(nrow(nominal$table), 4L)
  sequence <- influ_steps(list(A = influ(a, focus = "year"), B = influ(b, focus = "year")))
  expect_equal(nrow(index_ratio(sequence)$table), 4L)
})

test_that("invalid arguments and malformed saved tables fail clearly", {
  a <- ratio_index_fixture()
  expect_error(index_ratio(a), "list of calculated")
  expect_error(index_ratio(list(a)), "at least two")
  expect_error(index_ratio(list(a, a), reference = "absent"), "not a supplied")
  for (ref in list(NA, 1, c("A", "B"), "")) {
    expect_error(index_ratio(list(A = a, B = a), reference = ref), "reference")
  }
  for (labels in list(c("A", "A"), c("A", " "), NA_character_, 1:2)) {
    expect_error(index_ratio(list(a, a), labels = labels), "labels")
  }
  expect_identical(index_ratio(list(a, a))$metadata$series, c("Index 1", "Index 2"))
  expect_error(index_ratio(list(A = a, B = a), series = "nominal"), "only used")
  expect_error(index_ratio(list(A = a, B = a), component = "conditional"), "Select calculated")
  expect_error(index_ratio(list(A = a, B = a), component = NA), "component")
  for (s in list(NA, 1, character(), "missing", c("nominal", "nominal"))) {
    expect_error(index_ratio(ratio_diag_fixture(), series = s), "series")
  }
  bad <- a; bad$table$Year[1] <- NA
  expect_error(index_ratio(list(A = a, B = bad)), "year labels")
  bad <- a; bad$table$Year[2] <- bad$table$Year[1]
  expect_error(index_ratio(list(A = a, B = bad)), "year labels")
  bad <- a; bad$table$Mean <- as.character(bad$table$Mean)
  expect_error(index_ratio(list(A = a, B = bad)), "numeric")
  bad <- a; bad$table <- NULL
  expect_error(index_ratio(list(A = a, B = bad)), "missing its annual table")
  for (field in c("year", "response", "component", "units")) {
    bad <- a; bad$metadata[[field]] <- c("A", "B")
    expect_error(index_ratio(list(A = a, B = bad)), "one non-missing target")
  }
  diagnostic <- ratio_diag_fixture(); diagnostic$indices$series <- "missing"
  expect_error(index_ratio(list(A = diagnostic, B = diagnostic)), "not available")
  diagnostic <- ratio_diag_fixture(); diagnostic$indices$level <- NULL
  expect_error(index_ratio(list(A = diagnostic, B = diagnostic)), "missing its stored")
  expect_error(plot_index_ratio(a), "influ_index_ratio")
  r <- index_ratio(list(A = a, B = a))
  expect_error(plot(r, log = NA), "TRUE or FALSE")
  expect_error(plot(r, something = TRUE), "Unused")
})

test_that("compact saved ratio objects contain no fitted state and reload without refits", {
  a <- ratio_index_fixture()
  # Unused source fields may contain arbitrarily large or live native state.
  a$model <- new.env(parent = emptyenv())
  a$draws <- matrix(0, 100, 3)
  a$metadata$call <- quote(stop("Never evaluate source calls"))
  a$metadata$reference <- new.env(parent = emptyenv())
  r <- index_ratio(list(A = a, B = a))
  expect_false(saved_result_has_live_state(r))
  expect_null(r$model)
  expect_null(r$draws)
  expect_lt(as.numeric(object.size(r)), 25000)
  path <- tempfile(fileext = ".rds")
  withr::defer(unlink(path))
  saveRDS(r, path)
  restored <- readRDS(path)
  expect_identical(restored, r)
  expect_equal(ggplot2::ggplot_build(plot(restored))$data,
    ggplot2::ggplot_build(plot(r))$data)
  out <- tempfile(fileext = ".png")
  withr::defer(unlink(out))
  expect_silent(ggplot2::ggsave(out, plot(restored), width = 6, height = 4, dpi = 80))
  expect_true(file.exists(out))
})

test_that("saved ratio calculations and views need no native fitting packages", {
  skip_if_not_installed("callr")
  a <- ratio_index_fixture()
  r <- index_ratio(list(A = a, B = a))
  source_file <- normalizePath(test_path("..", "..", "R", "index-ratio.R"),
    mustWork = FALSE)
  if (!file.exists(source_file)) skip("Source file is unavailable in a bare installation")
  result <- callr::r(function(object, source_file) {
    env <- new.env(parent = asNamespace("influ2"))
    sys.source(file.path(dirname(source_file), "plot-cdi.R"), envir = env)
    sys.source(source_file, envir = env)
    before <- loadedNamespaces()
    table <- env$as.data.frame.influ_index_ratio(object)
    stats <- env$summary.influ_index_ratio(object)
    p <- env$plot_index_ratio(object)
    built <- ggplot2::ggplot_build(p)
    list(table = table, summary = stats, n_layers = length(built$data),
      newly_loaded = setdiff(loadedNamespaces(), before))
  }, list(object = r, source_file = source_file))
  expect_identical(result$table, r$table)
  expect_identical(result$summary, summary(r))
  expect_equal(result$n_layers, 3L)
  expect_false(any(result$newly_loaded %in%
    c("brms", "rstan", "glmmTMB", "lme4", "mgcv", "sdmTMB", "tinyVAST")))
})
