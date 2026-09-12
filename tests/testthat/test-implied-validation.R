# Audit the frozen IV01 experiment; never run its Monte Carlo fits in CI.
iv01_fixture <- function() readRDS(system.file("extdata", "implied-validation.rds", package = "influ2"))

test_that("IV01 retains its fixed design and every production attempt", {
  x <- iv01_fixture()
  expect_identical(x$schema_version, 1L)
  expect_identical(x$metadata$mode, "production")
  expect_identical(x$metadata$settings$nrep, 100L)
  expect_identical(x$metadata$settings$min_n, 10L)
  expect_equal(x$metadata$settings$level, .95)
  expect_equal(nrow(x$fits), 400L)
  expect_equal(nrow(x$metrics), 800L)
  expect_equal(nrow(x$cells), 14400L)
  expect_setequal(x$fits$replicate, 1:100)
  expect_equal(length(unique(x$fits$seed)), 400L)
  expect_false(anyDuplicated(x$fits[c("replicate", "sampling", "signal")]) > 0L)
  expect_false(anyDuplicated(x$metrics[c("replicate", "sampling", "signal", "route")]) > 0L)
  expect_false(anyDuplicated(x$cells[c("replicate", "sampling", "signal", "route", "level", "group")]) > 0L)
  expect_true(all(x$fits$valid == (x$fits$convergence == 0 & x$fits$pd_hessian & is.finite(x$fits$loglik))))
  expect_true(all(x$metrics$success == !nzchar(x$metrics$error)))
  expect_true(all(nchar(x$metadata$runtime_md5) == 32))
  expect_true(all(nchar(x$metadata$study_md5) == 32))
  expect_true(all(c("glmmTMB", "TMB", "influ2") %in% names(x$metadata$package_versions)))
  counts <- c(108, 80, 12, 0, 12, 9, 24, 24, 4, 36, 9, 24, 12, 9, 9, 60, 96, 120)
  d <- x$cells
  position <- match(d$level, as.character(2011:2016)) +
    6L * (match(d$group, x$metadata$settings$seasons) - 1L)
  expect_equal(d$n, ifelse(d$sampling == "balanced", 36, counts[position]))
  expected <- ifelse(d$n == 0, "empty", ifelse(d$n < 10, "sparse", "ok"))
  # No boundary cells occurred in this frozen run; tests elsewhere exercise them.
  expect_identical(d$status, expected)
  expect_true(all(is.na(d$adjustment[d$status != "ok"])))
  injected <- ifelse(d$signal == "null", 0, .6 * seq(-1, 1, length.out = 6)[
    match(d$level, as.character(2011:2016))] * (match(d$group, x$metadata$settings$seasons) - 2))
  expect_equal(d$injected, injected)
  oracle <- d$route == "known_parameters"
  expect_equal(d$target[oracle], injected[oracle])
})

test_that("IV01 dataset metrics are independently recoverable from cell records", {
  x <- iv01_fixture()
  key <- function(d) do.call(paste, c(d[c("replicate", "sampling", "signal", "route")], sep = ":"))
  groups <- split(x$cells, key(x$cells))
  mean_or_na <- function(y) if (length(y)) mean(y) else NA_real_
  rows <- lapply(key(x$metrics), function(k) {
    d <- groups[[k]]
    ok <- d$status == "ok" & is.finite(d$adjustment) & is.finite(d$target)
    ci <- ok & is.finite(d$lower_shift) & is.finite(d$upper_shift)
    strong <- ci & abs(d$injected) >= .3
    excluded <- d$lower_shift > 0 | d$upper_shift < 0
    data.frame(cells = nrow(d), usable = sum(ok), intervals = sum(ci),
      empty = sum(d$status == "empty"), sparse = sum(d$status == "sparse"),
      boundary = sum(d$status == "boundary_zero"), strong_cells = sum(strong),
      bias = mean_or_na((d$adjustment - d$target)[ok]),
      rmse = sqrt(mean_or_na((d$adjustment - d$target)[ok]^2)),
      containment = mean_or_na((d$lower_shift <= d$target & d$upper_shift >= d$target)[ci]),
      zero_exclusion = mean_or_na(excluded[ci]),
      any_zero_exclusion = if (any(ci)) any(excluded[ci]) else NA,
      width = mean_or_na((d$upper_shift - d$lower_shift)[ci]),
      strong_direction = mean_or_na((sign(d$adjustment) == sign(d$injected))[strong]),
      strong_exclusion = mean_or_na(excluded[strong]),
      target_injection_rmse = sqrt(mean_or_na((d$target - d$injected)[ok]^2)))
  })
  expected <- do.call(rbind, rows)
  actual <- x$metrics[, names(expected)]
  rownames(actual) <- NULL
  expect_equal(actual, expected, tolerance = 1e-12)
})

test_that("IV01 summaries use datasets rather than cells as independent units", {
  x <- iv01_fixture()
  measures <- c("bias", "rmse", "containment", "zero_exclusion", "any_zero_exclusion",
    "width", "strong_direction", "strong_exclusion", "target_injection_rmse")
  for (j in seq_len(nrow(x$summary))) {
    s <- x$summary[j, ]
    d <- subset(x$metrics, sampling == s$sampling & signal == s$signal & route == s$route)
    expect_equal(s$attempted, nrow(d))
    expect_equal(s$successful, sum(d$success))
    expected <- vapply(measures, function(name) {
      v <- d[[name]][d$success & is.finite(d[[name]])]
      c(mean = if (length(v)) mean(v) else NA_real_, n = length(v),
        mcse = if (length(v) > 1L) sd(v) / sqrt(length(v)) else NA_real_)
    }, numeric(3))
    expect_equal(unlist(s[measures], use.names = FALSE), unname(expected["mean", ]))
    expect_equal(unlist(s[paste0(measures, "_n")], use.names = FALSE), unname(expected["n", ]))
    expect_equal(unlist(s[paste0(measures, "_mcse")], use.names = FALSE), unname(expected["mcse", ]))
    counts <- c("cells", "usable", "intervals", "empty", "sparse", "boundary", "strong_cells")
    expect_equal(unlist(s[counts], use.names = FALSE), unname(colSums(d[d$success, counts])))
  }
  for (j in seq_len(nrow(x$fit_summary))) {
    s <- x$fit_summary[j, ]
    d <- subset(x$fits, sampling == s$sampling & signal == s$signal)
    expect_equal(c(s$attempted, s$eligible, s$errors, s$warnings),
      c(nrow(d), sum(d$valid), sum(nzchar(d$error)), sum(nzchar(d$warnings))))
    expect_equal(s$mean_dispersion, mean(d$dispersion[d$valid]))
  }
  for (j in seq_len(nrow(x$cell_summary))) {
    s <- x$cell_summary[j, ]
    d <- subset(x$cells, sampling == s$sampling & signal == s$signal & route == s$route &
      level == s$level & group == s$group)
    d <- d[d$status == "ok" & is.finite(d$adjustment), ]
    expect_equal(s$usable, nrow(d))
    if (nrow(d)) expect_equal(c(s$adjustment, s$target, s$width),
      c(mean(d$adjustment), mean(d$target), mean(d$upper_shift - d$lower_shift)))
  }
})

test_that("IV01 examples are compact, reproducibly selected, and preserve plot gaps", {
  x <- iv01_fixture()
  has_live_state <- function(y) {
    if (typeof(y) %in% c("environment", "externalptr", "closure", "weakref")) return(TRUE)
    if (is.list(y)) return(any(vapply(y, has_live_state, logical(1))))
    FALSE
  }
  expect_false(has_live_state(x))
  expect_lt(file.info(system.file("extdata", "implied-validation.rds", package = "influ2"))$size, 1e6)
  valid <- tapply(x$fits$valid, x$fits$replicate, all) & tapply(x$metrics$success, x$metrics$replicate, all)
  expect_equal(x$examples$replicate, min(as.integer(names(valid)[valid])))
  for (name in names(x$examples$cases)) {
    e <- x$examples$cases[[name]]
    scenario <- strsplit(name, ":", fixed = TRUE)[[1]]
    cells <- subset(x$cells, replicate == x$examples$replicate & sampling == scenario[1] &
      signal == scenario[2] & route == "fitted_model")
    expect_s3_class(e$result, "influ_implied")
    keys <- cells[c("level", "group", "n", "status")]
    rownames(keys) <- NULL
    expect_identical(e$result$table[names(keys)], keys)
    expect_equal(e$result$table$adjustment, cells$adjustment)
    expect_equal(e$result$table$baseline, cells$baseline)
    expect_equal(e$truth$target, cells$target)
    before <- serialize(e$result, NULL)
    p <- plot(e$result)
    expect_identical(serialize(e$result, NULL), before)
    for (layer in p$layers) {
      if (inherits(layer$geom, c("GeomLine", "GeomPoint", "GeomErrorbar"))) {
        expect_true(all(layer$data$status == "ok"))
        if (inherits(layer$geom, "GeomLine"))
          expect_true(all(vapply(split(layer$data$position, layer$data$segment),
            function(v) all(diff(v) == 1L), logical(1))))
      }
    }
    expect_equal(nrow(ggplot2::ggplot_build(p)$data[[5]]), sum(cells$status == "ok"))
  }
})
