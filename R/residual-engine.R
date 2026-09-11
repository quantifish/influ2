# Shared numerical engine. Callers validate their input contract and own RNG
# restoration. Keep randomisers after native adapter preparation: brms draw
# selection, for example, consumes RNG before this point.
.resid_summarise <- function(adapter, time, group_data, nsim, batch_size, seed,
    grid_size, level, calibration_bins, calibration_min_n, calibration_groups) {
  observed <- adapter$observed
  n <- length(observed)
  # Draw randomisers before simulation so they do not depend on batching.
  randomiser <- stats::runif(n)
  calibration <- .resid_calibration_setup(adapter, calibration_bins,
    calibration_min_n, calibration_groups, nsim)
  less <- ties <- total <- numeric(n)
  grid <- ecdfs <- NULL
  for (start in seq.int(1L, nsim, by = batch_size)) {
    ids <- seq.int(start, min(nsim, start + batch_size - 1L))
    sims <- adapter$simulate(ids)
    if (!is.matrix(sims) || !is.numeric(sims) ||
        !identical(dim(sims), c(n, length(ids))) || any(!is.finite(sims))) {
      stop("Response simulations must contain finite responses in an ",
        "observation-by-simulation matrix matching the aligned rows.", call. = FALSE)
    }
    if (is.null(grid)) {
      grid <- sort(unique(as.numeric(stats::quantile(
        c(observed, sims), seq(0, 1, length.out = grid_size), names = FALSE
      ))))
      if (all(observed >= 0) && all(sims >= 0)) grid <- sort(unique(c(0, grid)))
      ecdfs <- matrix(NA_real_, length(grid), nsim)
    }
    # Update by whole simulation, never independently resample observations.
    # Sequential summation also avoids batch-dependent roundoff differences.
    for (j in seq_along(ids)) {
      sim <- sims[, j]
      less <- less + (sim < observed)
      ties <- ties + (sim == observed)
      total <- total + sim
      ecdfs[, ids[j]] <- findInterval(grid, sort(sim)) / n
      calibration <- .resid_calibration_update(calibration, sim, ids[j], adapter$trials)
    }
  }
  pit <- (less + randomiser * (ties + 1)) / (nsim + 1)
  pit <- pmin(1 - .Machine$double.eps, pmax(.Machine$double.eps, pit))
  residual <- stats::qnorm(pit)
  tail <- (1 - level) / 2
  ord <- seq_len(n)
  ecdf_intervals <- t(apply(ecdfs, 1L, stats::quantile,
    probs = c(tail, 0.5, 1 - tail), names = FALSE))
  observed_grid <- sort(unique(c(range(grid), observed)))
  result <- structure(list(
    observations = data.frame(row = rownames(adapter$data), observed = observed,
      predicted = total / nsim, pit = pit, residual = residual,
      year = factor(as.character(adapter$data[[time$name]]), levels = time$levels)),
    qq = data.frame(theoretical = stats::qnorm(stats::ppoints(n)),
      residual = sort(residual),
      lower = stats::qnorm(stats::qbeta(tail, ord, n + 1 - ord)),
      upper = stats::qnorm(stats::qbeta(1 - tail, ord, n + 1 - ord))),
    ecdf = data.frame(response = grid, lower = ecdf_intervals[, 1L],
      median = ecdf_intervals[, 2L], upper = ecdf_intervals[, 3L]),
    observed_ecdf = data.frame(response = observed_grid,
      probability = findInterval(observed_grid, sort(observed)) / n),
    calibration = .resid_calibration_finish(calibration, level),
    groups = group_data,
    metadata = list(schema_version = 2L, backend = adapter$backend, scheme = adapter$scheme,
      response = adapter$response, year = time$name, year_source = time$source,
      nsim = nsim, batch_size = min(batch_size, nsim), seed = seed,
      level = level, grid_size = length(grid),
      response_structure = adapter$structure,
      response_family = adapter$family, component = adapter$component,
      response_kind = adapter$response_kind, prediction_type = adapter$prediction_type,
      calibration = "Exploratory ranks; not a calibrated goodness-of-fit test",
      retention = "No model or observation-by-simulation matrix retained")
  ), class = "influ_residuals")
  if (!is.null(adapter$conditioning)) {
    result$metadata$conditioning <- adapter$conditioning
    result$metadata$conditioning_requested <- adapter$conditioning_requested
  }
  if (!is.null(adapter$probability)) {
    result$observations$probability <- adapter$probability
    result$observations$trials <- adapter$trials
    result$observations$calibration_bin <- calibration$bin
  }
  result
}
