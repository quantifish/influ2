output_index_fixture <- function() {
  set.seed(91)
  d <- data.frame(year = factor(rep(c(2000, 2002, 2005), each = 80)), x = runif(240))
  d$cpue <- rpois(240, exp(1 + 0.2 * as.numeric(d$year) + 0.4 * d$x))
  fit <- glm(cpue ~ year * x, family = poisson(), data = d)
  list(fit = fit, ref = data.frame(x = c(0.1, 0.6, 0.9)),
    index = cpue_index(fit, reference_data = data.frame(x = c(0.1, 0.6, 0.9))))
}

test_that("reporting omits unavailable medians without changing the full schema", {
  x <- output_index_fixture()$index
  original <- x
  expect_false("Median" %in% names(index_table(x)))
  expect_true("Median" %in% names(as.data.frame(x)))
  expect_true(all(is.na(as.data.frame(x)$Median)))
  expect_identical(index_table(x, include_median = "always"), as.data.frame(x))
  expect_identical(index_table(x, include_median = "never"), index_table(x))
  expect_identical(x, original)
  expect_false(any(grepl("Median", capture.output(print(x)))))
  x$table$Median <- x$table$Mean / 1.05
  expect_equal(index_table(x)$Median, x$table$Median)
  expect_false("Median" %in% names(index_table(x, include_median = "never")))
  expect_error(index_table(list()), "influ_index")
})

test_that("lognormal assessment parameters are explicitly moment-matched", {
  x <- output_index_fixture()$index
  x$table$Mean <- c(2, 5, 10)
  x$table$SD <- c(0, 1.5, 6)
  x$table$CV <- x$table$SD / x$table$Mean
  tab <- index_table(x, format = "lognormal")
  expect_equal(tab$SDlog^2, log1p(x$table$CV^2))
  expect_equal(exp(tab$Meanlog + tab$SDlog^2 / 2), x$table$Mean)
  variance <- expm1(tab$SDlog^2) * exp(2 * tab$Meanlog + tab$SDlog^2)
  expect_equal(sqrt(variance), x$table$SD)
  expect_equal(tab$LognormalMedian, exp(tab$Meanlog))
  expect_false("Median" %in% names(tab))
  x$table$Median <- c(2, 4.7, 8.4)
  expect_equal(index_table(x, format = "lognormal")$Median, x$table$Median)
  expect_false(isTRUE(all.equal(index_table(x, format = "lognormal")$Median,
    index_table(x, format = "lognormal")$LognormalMedian)))
  for (value in c(0, -1, NA, Inf)) {
    bad <- x; bad$table$Mean[1] <- value
    expect_error(index_table(bad, format = "lognormal"), "positive finite")
  }
  for (value in c(-1, NA, Inf)) {
    bad <- x; bad$table$SD[1] <- value
    expect_error(index_table(bad, format = "lognormal"), "non-negative")
  }
  x$table$Mean <- rep(1e-200, 3)
  x$table$SD <- rep(1, 3)
  expect_true(all(is.finite(index_table(x, format = "lognormal")$SDlog)))
})

test_that("covariance uses index ordering and cannot be recovered from marginal SDs", {
  f <- output_index_fixture(); x <- f$index
  original <- x
  set.seed(47); rng <- .Random.seed
  V <- index_vcov(x, "response")
  S <- index_vcov(x)
  expect_identical(dimnames(S), list(x$table$Year, x$table$Year))
  expect_equal(sqrt(diag(V)), x$table$SD, ignore_attr = TRUE)
  expect_equal(S, V / outer(x$table$Mean, x$table$Mean))
  expect_identical(vcov(x), S)
  expect_identical(vcov(x, scale = "response"), V)
  expect_identical(index_vcov(x, years = c(2005, 2000)), S[c(3, 1), c(3, 1)])
  expect_identical(dim(index_vcov(x, years = 2000)), c(1L, 1L))
  expect_silent(index_vcov(x, require_pd = TRUE))
  expect_identical(x, original)
  expect_identical(.Random.seed, rng)
  for (years in list(character(), c(2000, 2000), "absent", NA, matrix(2000), list(2000))) {
    expect_error(index_vcov(x, years = years), "years")
  }
  expect_error(index_vcov(x, require_pd = NA), "TRUE or FALSE")
  expect_error(vcov(x, extra = 1), "Unused")
  old <- x; old$covariance <- NULL
  expect_error(index_vcov(old), "marginal SDs cannot recover")
  missing_response <- x; missing_response$covariance$response <- NULL
  expect_error(index_vcov(missing_response, "response"), "No response-scale")
  preview <- cpue_index(f$fit, reference_data = f$ref, uncertainty = "none")
  expect_null(preview$covariance)
  expect_error(index_vcov(preview), "No joint covariance")
  expect_error(index_table(preview, format = "lognormal"), "preview")
  contrast <- cpue_index(update(f$fit, . ~ year + x, data = model.frame(f$fit)),
    method = "year_effect")
  expect_error(index_vcov(contrast), "year-effect")
  expect_error(index_table(contrast, format = "lognormal"), "year-effect")
})

test_that("misaligned, invalid, and non-positive log-index inputs fail explicitly", {
  x <- output_index_fixture()$index
  bad <- x; bad$table <- bad$table[3:1, ]
  expect_error(index_vcov(bad), "aligned exactly")
  bad <- x; bad$table$Year[2] <- bad$table$Year[1]
  expect_error(index_vcov(bad), "unique")
  bad <- x; bad$covariance$log[1, 2] <- Inf
  expect_error(index_vcov(bad), "finite")
  bad <- x; bad$covariance$log[1, 2] <- 1
  expect_error(index_vcov(bad), "symmetric")
  bad <- x; bad$covariance$log[1, 1] <- -1
  expect_error(index_vcov(bad), "semidefinite")
  d <- data.frame(year = factor(rep(1:3, each = 10)), cpue = rep(c(-5, -4), 15))
  negative <- cpue_index(glm(cpue ~ year, data = d), reference_data = data.frame(dummy = 0))
  expect_null(negative$covariance$log)
  expect_silent(index_vcov(negative, "response"))
  expect_error(index_vcov(negative), "positive index")
  draws <- matrix(c(-1, 1, 2, 3, 4, 5), 3, 2)
  expect_null(.index_draw_covariance(draws, colMeans(draws), c("1", "2"))$log)
  positive <- matrix(c(1, 2, 4), ncol = 1)
  single <- .index_draw_covariance(positive, mean(positive), "2001")
  expect_identical(dim(single$log), c(1L, 1L))
  expect_equal(unname(single$log[1, 1]), var(log(positive[, 1])))
})

test_that("relative normalisation preserves its singular covariance", {
  f <- output_index_fixture()
  raw <- f$index
  relative <- cpue_index(f$fit, reference_data = f$ref, rescale = 1)
  n <- nrow(raw$table)
  H <- diag(n) - matrix(1 / n, n, n)
  expected <- H %*% index_vcov(raw) %*% t(H)
  expect_equal(unname(index_vcov(relative)), unname(expected), tolerance = 1e-10)
  expect_equal(as.numeric(index_vcov(relative) %*% rep(1, n)), rep(0, n), tolerance = 1e-12)
  expect_error(index_vcov(relative, require_pd = TRUE), "singular")
  # A supported principal submatrix is not forced to retain the full constraint.
  expect_silent(index_vcov(relative, years = raw$table$Year[-1], require_pd = TRUE))
  expect_equal(index_vcov(relative, "response"),
    index_vcov(relative) * outer(relative$table$Mean, relative$table$Mean))
})

test_that("area and unit scaling transform the response covariance only", {
  f <- output_index_fixture()
  base <- cpue_index(f$fit, reference_data = f$ref, reference_weights = c(1, 2, 3))
  a <- integrate_index(f$fit, f$ref, area = c(1, 2, 3), area_units = "km^2",
    response_units = "fish/km^2")
  b <- integrate_index(f$fit, f$ref, area = c(10, 20, 30), area_units = "km^2",
    response_units = "fish/km^2")
  expect_equal(index_vcov(a, "response"), index_vcov(base, "response") * 36)
  expect_equal(index_vcov(b, "response"), index_vcov(a, "response") * 100)
  expect_equal(index_vcov(a), index_vcov(base))
  expect_equal(index_vcov(b), index_vcov(a))
  expect_equal(a$table$SD, sqrt(diag(index_vcov(a, "response"))), ignore_attr = TRUE)
  expect_equal(index_table(b, format = "lognormal")$SDlog,
    index_table(a, format = "lognormal")$SDlog)
})

test_that("matrix plots show stored cross-year uncertainty without recalculation", {
  x <- output_index_fixture()$index
  original <- x
  for (type in c("correlation", "covariance")) {
    for (scale in c("log", "response")) {
      p <- plot(x, type = type, scale = scale)
      expected <- index_vcov(x, scale)
      if (type == "correlation") expected <- cov2cor(expected)
      expect_equal(p$data$value, as.vector(expected))
      expect_equal(levels(p$data$column), x$table$Year)
      expect_equal(levels(p$data$row), rev(x$table$Year))
      expect_silent(ggplot2::ggplot_build(p))
      expect_equal(ggplot2::autoplot(x, type = type, scale = scale)$data, p$data)
    }
  }
  expect_identical(x, original)
  x$covariance$log[,] <- 0
  expect_error(plot(x, type = "correlation"), "zero variance")
  expect_silent(ggplot2::ggplot_build(plot(x, type = "covariance")))
})

test_that("annual correlation display has a visual regression baseline", {
  skip_if_not_installed("vdiffr")
  local_edition(3)
  theme <- ggplot2::theme_set(ggplot2::theme_bw())
  on.exit(ggplot2::theme_set(theme), add = TRUE)
  vdiffr::expect_doppelganger("annual log-index correlation", plot(output_index_fixture()$index,
    type = "correlation"))
})
