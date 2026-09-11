# These tests audit frozen study results, not a new Monte Carlo run in CI.
n09_result_fixture <- function() {
  readRDS(system.file("extdata", "n09-validation.rds", package = "influ2"))
}

test_that("the N09 artefact records the complete bounded production design", {
  x <- n09_result_fixture()
  expect_identical(x$schema_version, 1L)
  expect_identical(x$metadata$mode, "production")
  expect_identical(x$metadata$settings$nrep, 100L)
  expect_identical(x$metadata$settings$n, 480L)
  expect_identical(x$metadata$settings$nsim, 499L)
  expect_identical(x$metadata$settings$high_nsim, 1999L)
  expect_identical(x$metadata$settings$sensitivity_ids, 1:10)
  expect_equal(nrow(x$fits), 500L)
  expect_true(all(x$fits$replicate %in% 1:100))
  expect_true(all(x$metrics$replicate %in% 1:100))
  expect_false(anyDuplicated(x$fits[c("backend", "replicate", "scenario")]) > 0L)
  expect_false(anyDuplicated(x$metrics[c("backend", "replicate", "scenario", "conditioning", "variant")]) > 0L)
  expect_true(all(nchar(x$metadata$runtime_md5) == 32))
  expect_true(all(nchar(x$metadata$study_md5) == 32))
  expect_true(all(c("influ2", "glmmTMB", "sdmTMB", "bayesplot") %in% names(x$metadata$package_versions)))
  expect_true(all(x$metrics$success == !nzchar(x$metrics$error)))
  expect_true(all(x$fits$valid == (x$fits$convergence == 0 & x$fits$pd_hessian &
    is.finite(x$fits$loglik)), na.rm = TRUE))
  expect_true(all(x$metrics$variant[x$metrics$replicate > 10] == "primary"))
  finite <- x$metrics[x$metrics$conditioning == "finite_truth", ]
  expect_true(all(is.finite(finite$simulation_seed)))
  expect_true(all(finite$simulation_seed != finite$seed))
  expect_equal(nrow(finite), 200L)
})

test_that("N09 rates and Monte Carlo intervals agree with replicate records", {
  x <- n09_result_fixture()
  for (i in seq_len(nrow(x$summary))) {
    s <- x$summary[i, ]
    d <- subset(x$metrics, backend == s$backend & scenario == s$scenario &
      conditioning == s$conditioning & variant == "primary")
    good <- d[d$success, ]
    expect_equal(s$diagnostic_attempts, nrow(d))
    expect_equal(s$usable, nrow(good))
    expect_equal(s$diagnostic_failures, sum(!d$success))
    expect_equal(s$crossings, sum(good$dkw_crossing))
    expect_equal(s$displayed_crossings, sum(good$displayed_band_crossing))
    if (nrow(good)) {
      p <- mean(good$dkw_crossing)
      z <- qnorm(.975)
      n <- nrow(good)
      centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
      radius <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
      expect_equal(s$rate, p)
      expect_equal(s$lower, max(0, centre - radius))
      expect_equal(s$upper, min(1, centre + radius))
      expect_equal(s$residual_sd, mean(good$residual_sd))
    }
  }
  for (i in seq_len(nrow(x$fit_summary))) {
    s <- x$fit_summary[i, ]
    d <- subset(x$fits, backend == s$backend & scenario == s$scenario)
    expect_equal(s$attempted, 100L)
    expect_equal(s$eligible, sum(d$valid))
    expect_equal(s$errors, sum(nzchar(d$error)))
  }
})

test_that("N09 sensitivity calculations preserve pairing and denominators", {
  x <- n09_result_fixture()
  for (i in seq_len(nrow(x$sensitivity))) {
    s <- x$sensitivity[i, ]
    d <- subset(x$metrics, backend == s$backend & scenario == s$scenario &
      conditioning == s$conditioning & replicate <= 10)
    primary <- d[d$variant == "primary", ]
    extra <- d[d$variant == s$variant, ]
    paired <- merge(primary, extra, by = "replicate", suffixes = c("_p", "_e"))
    paired <- paired[paired$success_p & paired$success_e, ]
    expect_equal(s$paired, nrow(paired))
    expect_equal(s$datasets, 10L)
    expect_equal(s$flag_changes, sum(paired$dkw_crossing_p != paired$dkw_crossing_e))
    expect_equal(s$mean_absolute_distance_change,
      mean(abs(paired$dkw_distance_p - paired$dkw_distance_e)))
  }
})

test_that("N09 examples are compact, aligned, and selected before visual review", {
  x <- n09_result_fixture()
  has_live_state <- function(y) {
    if (typeof(y) %in% c("environment", "externalptr", "closure", "weakref")) return(TRUE)
    if (is.list(y)) return(any(vapply(y, has_live_state, logical(1))))
    FALSE
  }
  expect_false(has_live_state(x))
  expect_lt(file.info(system.file("extdata", "n09-validation.rds", package = "influ2"))$size, 1e6)
  for (backend in names(x$examples)) {
    e <- x$examples[[backend]]
    f <- x$fits[x$fits$backend == backend, ]
    m <- x$metrics[x$metrics$backend == backend, ]
    valid_fit <- tapply(f$valid, f$replicate, all)
    valid_diagnostic <- tapply(m$success, m$replicate, all)
    first <- min(as.integer(names(valid_fit)[valid_fit & valid_diagnostic[names(valid_fit)]]))
    expect_equal(e$replicate, first)
    expect_length(e$checks, if (backend == "glmmTMB") 4L else 9L)
    for (key in names(e$checks)) {
      checks <- e$checks[[key]]
      expect_s3_class(checks, "influ_residuals")
      expect_identical(checks$observations$row, rownames(e$data))
      expect_equal(checks$observations$residual, qnorm(checks$observations$pit))
      expect_null(checks$model)
      expect_null(checks$simulations)
      u <- sort(checks$observations$pit)
      n <- length(u)
      D <- max(seq_len(n) / n - u, u - (seq_len(n) - 1) / n)
      parts <- strsplit(key, ":", fixed = TRUE)[[1]]
      row <- subset(m, replicate == e$replicate & scenario == parts[1] &
        conditioning == parts[2] & variant == "primary")
      expect_equal(nrow(row), 1L)
      expect_equal(row$dkw_distance, D)
    }
  }
  expect_equal(x$band_audit$rate, x$band_audit$crossings / x$band_audit$n)
  expect_true(all(x$band_audit$n == 10000L))
  expect_equal(x$bands$x, seq(0, 1, length.out = 100))
  expect_equal(x$bands$interval_grid, seq_len(100) / 100)
})
