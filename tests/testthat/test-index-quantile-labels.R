quantile_label_fixture <- function(probs = c(0.025, 0.975)) {
  .new_cpue_index(data.frame(Year = c("2001", "2003"), Mean = c(2, 3),
    Median = c(NA_real_, NA_real_), SD = c(0.2, 0.3), CV = c(0.1, 0.1),
    Qlower = c(1.6, 2.4), Qupper = c(2.4, 3.6)),
    list(method = "standardised", uncertainty = "delta", probs = probs))
}

test_that("reporting names reflect probabilities without changing stored values", {
  withr::local_seed(341)
  for (probs in list(c(0.025, 0.975), c(0.05, 0.95), c(0.1, 0.95))) {
    x <- quantile_label_fixture(probs)
    original <- x
    rng <- .Random.seed
    tab <- index_table(x)
    labels <- paste0("Q", probs)
    expect_identical(names(tab), c("Year", "Mean", "SD", "CV", labels, "Method"))
    expect_identical(tab[[labels[1]]], x$table$Qlower)
    expect_identical(tab[[labels[2]]], x$table$Qupper)
    generic <- index_table(x, quantile_names = "generic")
    names(tab) <- names(generic)
    expect_identical(tab, generic)
    expect_identical(index_table(x, include_median = "always",
      quantile_names = "generic"), as.data.frame(x))
    expect_identical(x, original)
    expect_identical(.Random.seed, rng)
    lognormal <- index_table(x, format = "lognormal")
    expect_true(all(labels %in% names(lognormal)))
    expect_identical(tail(names(lognormal), 3),
      c("Meanlog", "SDlog", "LognormalMedian"))
  }
  expect_error(index_table(x, quantile_names = "unknown"), "arg")
})

test_that("probability headings are unambiguous and independent of print options", {
  withr::local_options(list(digits = 3, scipen = -9, OutDec = ","))
  options_before <- options()
  ordinary <- index_table(quantile_label_fixture(c(0.05, 0.95)))
  expect_identical(grep("^Q", names(ordinary), value = TRUE), c("Q0.05", "Q0.95"))
  for (probs in list(c(1e-30, 1e-6), c(0.5, 0.5 + .Machine$double.eps),
      c(1 / 3, 1 - .Machine$double.eps / 2))) {
    headings <- grep("^Q", names(index_table(quantile_label_fixture(probs))), value = TRUE)
    expect_false(anyDuplicated(headings) > 0)
    expect_false(any(grepl("[eE,]", headings)))
    expect_identical(as.numeric(sub("^Q", "", headings)), probs)
  }
  expect_identical(options(), options_before)
})

test_that("unknown stored probabilities are not guessed", {
  for (probs in list(NULL, NA_real_, c(0.05, NA), c(0.95, 0.05),
      c(0, 1), c(0.05, Inf), c("0.05", "0.95"),
      matrix(c(0.05, 0.95), nrow = 1))) {
    x <- quantile_label_fixture(probs)
    original <- x
    expect_warning(tab <- index_table(x), "probabilities are missing or invalid")
    expect_identical(grep("^Q", names(tab), value = TRUE), c("Qlower", "Qupper"))
    expect_silent(generic <- index_table(x, quantile_names = "generic"))
    expect_identical(tab, generic)
    expect_identical(x, original)
  }
  x <- quantile_label_fixture()
  x$metadata <- NULL
  expect_warning(index_table(x), "probabilities are missing or invalid")
})

test_that("available columns and preview limits keep their intended labels", {
  x <- quantile_label_fixture()
  x$table$Qupper <- NULL
  expect_identical(grep("^Q", names(index_table(x)), value = TRUE), "Q0.025")
  x$table$Qlower <- NULL
  expect_identical(names(index_table(x)), c("Year", "Mean", "SD", "CV", "Method"))
  x <- quantile_label_fixture(c(0.05, 0.95))
  x$metadata$uncertainty <- "none"
  x$table[c("SD", "CV", "Qlower", "Qupper")] <- NA_real_
  tab <- index_table(x)
  expect_identical(grep("^Q", names(tab), value = TRUE), c("Q0.05", "Q0.95"))
  expect_true(all(is.na(tab$Q0.05)))
  expect_true(all(is.na(tab$Q0.95)))
})

test_that("calculated and year-effect indices retain their actual interval choices", {
  d <- data.frame(year = factor(rep(2001:2003, each = 8)),
    cpue = rep(c(0, 1, 2, 3, 4, 2, 1, 3), 3))
  fit <- glm(cpue ~ year, poisson(), data = d)
  probs <- c(0.05, 0.95)
  standardised <- cpue_index(fit, reference_data = data.frame(dummy = 0),
    probs = probs)
  integrated <- integrate_index(fit, data.frame(dummy = 0), area = 2,
    area_units = "km^2", response_units = "fish/km^2", probs = probs)
  diagnostic <- influ(fit, focus = "year", probs = probs)
  contrast <- cpue_index(diagnostic, method = "year_effect",
    probs = c(0.1, 0.9))
  native_contrast <- cpue_index(fit, method = "year_effect", probs = probs)
  for (x in list(standardised, integrated, contrast, native_contrast)) {
    expect_identical(x$metadata$probs, probs)
    expect_silent(tab <- index_table(x))
    expect_identical(grep("^Q", names(tab), value = TRUE), c("Q0.05", "Q0.95"))
    expect_identical(tab$Q0.05, x$table$Qlower)
    expect_identical(tab$Q0.95, x$table$Qupper)
  }
  expect_identical(contrast$table, native_contrast$table)
  expect_match(paste(capture.output(print(contrast)), collapse = " "), "Q0.05")
})
