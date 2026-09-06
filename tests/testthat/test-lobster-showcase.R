lobster_showcase_data <- function() {
  data("lobsters_per_pot", package = "influ2", envir = environment())
  lobsters_per_pot
}

test_that("the lobster showcase retains holes, covariate overlap, and known truth", {
  data <- lobster_showcase_data()
  expect_named(data, c("lobsters", "year", "month", "depth", "soak"))
  expect_identical(levels(data$year), as.character(2000:2017))
  expect_identical(levels(data$month), sprintf("%02d", 1:12))
  expect_true(all(stats::complete.cases(data)))
  expect_true(all(is.finite(data$lobsters) & data$lobsters >= 0 &
    data$lobsters == floor(data$lobsters)))
  expect_true(all(is.finite(data$depth) & data$depth > 0))
  expect_true(all(is.finite(data$soak) & data$soak > 0))

  coverage <- table(data$year, data$month)
  expect_true(any(coverage == 0L))
  expect_true(all(rowSums(coverage > 0L) >= 8L))
  expect_true(all(colSums(coverage > 0L) >= 10L))
  for (variable in c("depth", "soak")) {
    ranges <- vapply(split(data[[variable]], data$year), stats::quantile,
      numeric(2), probs = c(0.05, 0.95), names = FALSE)
    # A common interval remains inside every year's central 90% range:
    # changing coverage must not create completely separated year strata.
    expect_lt(max(ranges[1, ]), min(ranges[2, ]))
  }
  design <- stats::model.matrix(
    ~year + month + poly(depth, 3) + poly(soak, 3), data = data
  )
  expect_equal(qr(design)$rank, ncol(design))

  simulation <- attr(data, "simulation")
  expect_true(is.list(simulation))
  expect_length(simulation$seed, 1L)
  expect_true(is.finite(simulation$seed))
  expect_type(simulation$scenario, "character")
  expect_length(simulation$scenario, 1L)
  expect_true(nzchar(simulation$scenario))
  truth <- simulation$year_effect
  expect_named(truth, c("year", "log_effect"))
  expect_identical(as.character(truth$year), levels(data$year))
  expect_true(all(is.finite(truth$log_effect)))

  weights <- as.numeric(table(data$year)[as.character(truth$year)])
  observed_centre <- mean(truth$log_effect[
    match(as.character(data$year), as.character(truth$year))
  ])
  expect_equal(stats::weighted.mean(truth$log_effect, weights), observed_centre)
  truth_log_ratio <- truth$log_effect - observed_centre
  expect_equal(stats::weighted.mean(truth_log_ratio, weights), 0,
    tolerance = 1e-12)
})

test_that("refitted lobster steps visibly correct the known year-effect bias", {
  data <- lobster_showcase_data()
  model <- stats::glm(
    lobsters ~ year + month + poly(depth, 3) + poly(soak, 3),
    data = data, family = stats::poisson(link = "log")
  )
  sequence <- influ_steps(
    model, year = "year", refit = TRUE, uncertainty = "none"
  )
  expect_equal(nrow(sequence$steps), 4L)
  expect_identical(sequence$metadata$estimand, "year_effect_contrast")
  expect_identical(sequence$metadata$reference, "observed")
  indices <- influ_indices(sequence)
  truth <- attr(data, "simulation")$year_effect
  weights <- as.numeric(table(data$year)[as.character(truth$year)])
  truth_log_ratio <- truth$log_effect -
    stats::weighted.mean(truth$log_effect, weights)
  names(truth_log_ratio) <- as.character(truth$year)

  estimates <- lapply(sequence$steps$step_id, function(id) {
    step <- indices[indices$step_id == id, , drop = FALSE]
    expect_identical(step$level, as.character(truth$year))
    expect_true(all(is.finite(step$estimate) & step$estimate > 0))
    # Each refit uses the same observation-weighted contrast, not separate
    # equal-year display normalisation or an integrated abundance index.
    expect_equal(stats::weighted.mean(log(step$estimate), weights), 0,
      tolerance = 1e-8)
    stats::setNames(step$estimate, step$level)
  })
  rmse <- vapply(estimates, function(estimate) {
    sqrt(mean((log(estimate) - truth_log_ratio[names(estimate)])^2))
  }, numeric(1))
  expect_lt(tail(rmse, 1L), rmse[1L] / 2)
  for (i in seq.int(2L, length(estimates))) {
    relative_change <- estimates[[i]] / estimates[[i - 1L]] - 1
    expect_gt(max(abs(relative_change)), 0.1)
  }
  # Intermediate errors need not improve monotonically: nuisance terms can
  # be correlated. Poisson intervals are not a coverage test for NB counts.
})

test_that("the compact lobster BRMS fixture uses the bundled observations", {
  skip_if_not_installed("brms")
  path <- system.file("extdata", "brms-fixtures", "fit2.rds", package = "influ2")
  expect_true(file.exists(path))
  fit <- readRDS(path)
  data <- lobster_showcase_data()
  expect_true(all(names(data) %in% names(fit$data)))
  expect_equal(nrow(fit$data), nrow(data))
  # Compare each real data column, ignoring data-frame-level terms and
  # simulation metadata, plus BRMS's stored factor contrast matrices.
  # The observed values, classes, and factor levels must still be identical.
  for (column in names(data)) {
    observed <- fit$data[[column]]
    if (is.factor(observed)) attr(observed, "contrasts") <- NULL
    expect_identical(observed, data[[column]], info = column)
  }
})
