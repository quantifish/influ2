stream_test_fixture <- function(simulated, observed, integer = TRUE, block = 7L) {
  list(cache = list(n = nrow(simulated), nsim = ncol(simulated),
    row_block_size = as.integer(block), method = "PIT"), simulated = simulated,
    observed = observed, predicted = rowMeans(simulated), integer = integer)
}

stream_native_fixture <- function(x, seed = 491L) {
  suppressWarnings(DHARMa::createDHARMa(x$simulated, x$observed,
    fittedPredictedResponse = x$predicted, integerResponse = x$integer,
    seed = seed, method = "PIT"))
}

stream_test_require_reference <- function() {
  skip_if_not_installed("DHARMa", "0.4.7")
  skip_if(!.resid_dharma_stream_version() %in% c("0.4.7", "0.5.0"),
    "Bounded-memory equivalent parity is not validated for this DHARMa version")
}

test_that("bounded-memory dispersion matches native DHARMa on shared responses", {
  stream_test_require_reference()
  set.seed(581)
  n <- 63L
  count <- matrix(rnbinom(n * 41L, mu = rep(seq(2, 20, length.out = n), 41L),
    size = 1.8), n)
  continuous <- matrix(rnorm(n * 41L, mean = rep(seq(2, 20, length.out = n), 41L)), n)
  fixtures <- list(stream_test_fixture(count, rpois(n, 8)),
    stream_test_fixture(continuous, rnorm(n, 10), FALSE),
    stream_test_fixture(count, count[, 1]),
    stream_test_fixture(matrix(2, n, 41), rep(2, n)),
    stream_test_fixture(matrix(rep(c(0, 1, 2), 41), 3), c(0, 1, 2)))
  for (x in fixtures) {
    native <- stream_native_fixture(x)
    expected <- DHARMa::testDispersion(native, plot = FALSE)
    for (block in c(1L, 7L, 16L, 100L)) {
      x$cache$row_block_size <- block
      local_mocked_bindings(.resid_dharma_stream_block = function(cache, first, last)
        x$simulated[first:last, , drop = FALSE])
      set.seed(77)
      before <- .Random.seed
      actual <- .resid_dharma_stream_dispersion(x$cache, x$observed, x$predicted)
      expect_equal(actual$statistic, expected$statistic, tolerance = 1e-12)
      expect_equal(actual$p.value, expected$p.value, tolerance = 1e-12)
      expect_identical(.Random.seed, before)
      expect_s3_class(actual, "htest")
      expect_match(actual$method, "bounded-memory equivalent")
    }
  }
})

test_that("count bootstrap preserves shared columns and native RNG ordering", {
  stream_test_require_reference()
  set.seed(731)
  fixtures <- list(
    stream_test_fixture(matrix(rpois(37L * 23L, 3), 37), c(99, rep(0, 10), rpois(26, 3))),
    stream_test_fixture(matrix(1, 37, 23), c(0, 2, rep(1, 35))),
    stream_test_fixture(matrix(rep(0:4, 23), 5), 0:4),
    stream_test_fixture(matrix(rep(c(0, 1, 2), 2), 3), c(-1, 1, 3)))
  for (x in fixtures) {
    native <- stream_native_fixture(x)
    for (block in c(1L, 4L, 19L, 100L)) {
      x$cache$row_block_size <- block
      local_mocked_bindings(.resid_dharma_stream_block = function(cache, first, last)
        x$simulated[first:last, , drop = FALSE])
      for (seed in c(18L, 291L)) {
        nboot <- min(11L, x$cache$nsim)
        set.seed(seed)
        expected <- DHARMa::testOutliers(native, type = "bootstrap", nBoot = nboot, plot = FALSE)
        expected_state <- .Random.seed
        set.seed(seed)
        actual <- .resid_dharma_stream_outliers(x$cache, x$observed,
          native$scaledResiduals, TRUE, nboot)
        expect_identical(actual$statistic, expected$statistic)
        expect_identical(actual$parameter, expected$parameter)
        expect_identical(actual$p.value, expected$p.value)
        expect_identical(actual$conf.int, expected$conf.int)
        expect_identical(actual$estimate, expected$estimate)
        expect_identical(.Random.seed, expected_state)
        expect_s3_class(actual, "htest")
        expect_match(actual$method, "bounded-memory equivalent")
      }
    }
  }
})

test_that("dispersion retains exact native Monte Carlo ties", {
  stream_test_require_reference()
  # The observed response is itself a reference column. Block-summed moments
  # alone formerly dropped 27 of these 100 exact ties by a few ulps.
  for (seed in seq_len(100L)) {
    set.seed(seed)
    n <- c(5L, 7L, 13L, 31L, 83L)[(seed %% 5L) + 1L]
    x <- stream_test_fixture(matrix(rpois(n * 11L, 3), n), numeric(n), block = 3L)
    x$observed <- x$simulated[, 1L]
    native <- stream_native_fixture(x)
    local_mocked_bindings(.resid_dharma_stream_block = function(cache, first, last)
      x$simulated[first:last, , drop = FALSE])
    actual <- .resid_dharma_stream_dispersion(x$cache, x$observed, x$predicted)
    expected <- DHARMa::testDispersion(native, plot = FALSE)
    expect_identical(actual$p.value, expected$p.value)
    expect_equal(actual$statistic, expected$statistic, tolerance = 1e-12)
  }
})

test_that("continuous outlier counts use native finite-rank boundaries", {
  stream_test_require_reference()
  set.seed(722)
  x <- stream_test_fixture(matrix(rnorm(29L * 31L), 29), c(-99, 99, rnorm(27)), FALSE)
  native <- stream_native_fixture(x)
  expected <- DHARMa::testOutliers(native, type = "binomial", plot = FALSE)
  local_mocked_bindings(.resid_dharma_stream_block = function(...) stop("Should not read simulations"))
  set.seed(17)
  before <- .Random.seed
  actual <- .resid_dharma_stream_outliers(x$cache, x$observed,
    native$scaledResiduals, FALSE)
  for (field in c("statistic", "parameter", "p.value", "conf.int", "estimate", "null.value", "margin")) {
    expect_identical(actual[[field]], expected[[field]])
  }
  expect_identical(.Random.seed, before)
  expect_s3_class(actual, "htest")
  # Exact equality with a finite-rank threshold is not an outlier.
  boundaries <- c(1 / 32, 1 - 1 / 32, rep(0.5, 27))
  actual <- .resid_dharma_stream_outliers(x$cache, x$observed, boundaries, FALSE)
  expect_equal(unname(actual$statistic), 0)
})

test_that("bounded-memory reducers validate dimensions and PIT method", {
  stream_test_require_reference()
  x <- stream_test_fixture(matrix(1, 3, 10), rep(1, 3))
  local_mocked_bindings(.resid_dharma_stream_block = function(cache, first, last)
    x$simulated[first:last, , drop = FALSE])
  bad <- x$cache
  bad$n <- 0L
  expect_error(.resid_dharma_stream_dispersion(bad, x$observed, x$predicted), "invalid dimensions")
  bad <- x$cache
  bad$nsim <- 1L
  expect_error(.resid_dharma_stream_check(bad, x$observed), "invalid dimensions")
  bad <- x$cache
  bad$row_block_size <- NA_integer_
  expect_error(.resid_dharma_stream_check(bad, x$observed), "row-block")
  bad <- x$cache
  bad$method <- "traditional"
  expect_error(.resid_dharma_stream_check(bad, x$observed), "PIT")
  expect_error(.resid_dharma_stream_dispersion(x$cache, x$observed[-1], x$predicted), "Observed")
  expect_error(.resid_dharma_stream_dispersion(x$cache, x$observed, c(NA, 1, 1)), "Predictive")
  expect_error(.resid_dharma_stream_outliers(x$cache, x$observed, c(0.5, NA, 0.5), TRUE), "PIT values")
  expect_error(.resid_dharma_stream_outliers(x$cache, x$observed, rep(0.5, 3), NA), "integer_response")
  expect_error(.resid_dharma_stream_outliers(x$cache, x$observed, rep(0.5, 3), TRUE, 0), "nboot")
  expect_message(out <- .resid_dharma_stream_outliers(x$cache, x$observed,
    rep(0.5, 3), TRUE, 11), "reduced")
  expect_s3_class(out, "htest")
})

test_that("real disk caches preserve dispersion ties and count bootstrap", {
  stream_test_require_reference()
  set.seed(3)
  n <- 83L
  simulated <- matrix(rpois(n * 11L, 3), n)
  x <- stream_test_fixture(simulated, simulated[, 1L], block = 3L)
  native <- stream_native_fixture(x)
  cache <- .resid_dharma_cache_new(n, 11L, maximum = 0.01, batch_size = 3L)
  on.exit(.resid_dharma_cache_close(cache), add = TRUE)
  for (first in seq.int(1L, 11L, by = cache$batch_size)) {
    ids <- first:min(11L, first + cache$batch_size - 1L)
    .resid_dharma_cache_write(cache, simulated[, ids, drop = FALSE], ids)
  }
  for (block in c(1L, 3L, cache$row_block_size)) {
    cache$row_block_size <- block
    actual <- .resid_dharma_stream_dispersion(cache, x$observed, x$predicted)
    expected <- DHARMa::testDispersion(native, plot = FALSE)
    expect_identical(actual$p.value, expected$p.value)
    expect_equal(actual$statistic, expected$statistic, tolerance = 1e-12)
    set.seed(891)
    expected <- DHARMa::testOutliers(native, type = "bootstrap", nBoot = 7L, plot = FALSE)
    expected_state <- .Random.seed
    set.seed(891)
    actual <- .resid_dharma_stream_outliers(cache, x$observed, native$scaledResiduals, TRUE, 7L)
    expect_identical(actual$p.value, expected$p.value)
    expect_identical(actual$estimate, expected$estimate)
    expect_identical(.Random.seed, expected_state)
  }
})

test_that("equivalent tests do not claim unchecked future DHARMa parity", {
  skip_if_not_installed("DHARMa")
  x <- stream_test_fixture(matrix(1, 3, 10), rep(1, 3))
  local_mocked_bindings(.resid_dharma_stream_version = function() "0.6.0")
  expect_error(.resid_dharma_stream_dispersion(x$cache, x$observed, x$predicted),
    "not been validated against DHARMa 0.6.0")
  expect_error(.resid_dharma_stream_outliers(x$cache, x$observed, rep(0.5, 3), TRUE),
    "native tests")
})

test_that("unchecked future equivalents do not suppress genuine vector tests", {
  skip_if_not_installed("DHARMa", "0.4.7")
  set.seed(384)
  n <- 60L
  simulations <- matrix(rpois(n * 24L, rep(seq(1, 8, length.out = n), 24L)), n)
  data <- data.frame(response = rpois(n, seq(1, 8, length.out = n)),
    year = factor(rep(2010:2012, each = 20L)))
  rownames(simulations) <- rownames(data)
  args <- list(simulations = simulations, data = data, response = "response",
    year = "year", response_kind = "distribution",
    conditioning = "Shared supplied response simulations",
    residual_method = "dharma", residual_tests = "full", integer_response = TRUE,
    batch_size = 3L, seed = 181L, dharma_storage = "memory")
  native <- do.call(as_influ_residuals, args)
  local_mocked_bindings(.resid_dharma_stream_version = function() "0.6.0")
  args$dharma_storage <- "disk"
  future <- do.call(as_influ_residuals, args)
  equivalent <- future$tests$check %in% c("dispersion", "outliers")
  expect_true(all(future$tests$status[equivalent] == "not_assessed"))
  expect_true(all(is.na(future$tests$p_value[equivalent])))
  expect_true(all(grepl("not been validated against DHARMa 0.6.0",
    future$tests$message[equivalent], fixed = TRUE)))
  for (column in c("check", "status", "statistic", "df", "p_value", "p_adjusted")) {
    expect_equal(future$tests[[column]][!equivalent], native$tests[[column]][!equivalent])
  }
  expect_true(all(is.finite(future$tests$p_value[future$tests$check == "uniformity"])))
  expect_identical(future$observations, native$observations)
  expect_null(future$dharma)
})

test_that("malformed cache readers do not produce successful diagnostics", {
  skip_if_not_installed("DHARMa")
  cache <- stream_test_fixture(matrix(1, 3, 10), rep(1, 3))$cache
  local_mocked_bindings(.resid_dharma_cache_read = function(...) matrix(1, 1, 10))
  expect_error(.resid_dharma_stream_block(cache, 1L, 3L), "invalid simulation row block")
  local_mocked_bindings(.resid_dharma_cache_read = function(...) matrix(NA_real_, 3, 10))
  expect_error(.resid_dharma_stream_block(cache, 1L, 3L), "invalid simulation row block")
})

test_that("count bootstrap rejects malformed native quantiles", {
  stream_test_require_reference()
  x <- stream_test_fixture(matrix(1, 3, 10), rep(1, 3))
  local_mocked_bindings(.resid_dharma_stream_block = function(cache, first, last)
    x$simulated[first:last, , drop = FALSE])
  local_mocked_bindings(getQuantile = function(...) 0.5, .package = "DHARMa")
  expect_error(.resid_dharma_stream_outliers(x$cache, x$observed, rep(0.5, 3), TRUE),
    "invalid scaled residuals")
})
