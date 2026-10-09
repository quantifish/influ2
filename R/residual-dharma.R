# DHARMa owns the quantile calculation. Do not relabel influ2's finite-sample
# ranks as DHARMa residuals, or delegate simulation conditioning to a second
# model adapter with potentially different defaults.
.resid_summarise_method <- function(adapter, time, group_data, nsim, batch_size, seed,
    grid_size, level, calibration_bins, calibration_min_n, calibration_groups,
    residual_method = "simulation_pit", retain_dharma = FALSE, dharma_max_mb = 256,
    integer_response = NULL) {
  dharma <- residual_method == "dharma"
  if (dharma) {
    matrix_mb <- .resid_dharma_memory(length(adapter$observed), nsim, dharma_max_mb)
    full <- matrix(NA_real_, length(adapter$observed), nsim)
    native <- adapter$simulate
    adapter$simulate <- function(ids) {
      sims <- native(ids)
      # The original engine validates before use. Do not mask its error with
      # assignment/recycling when a native backend returns malformed output.
      if (is.matrix(sims) && is.numeric(sims) &&
          identical(dim(sims), c(length(adapter$observed), length(ids)))) {
        full[, ids] <<- sims
      }
      sims
    }
  }
  # Preserve the independently frozen default engine, including native RNG
  # preparation, joint simulation order, calibration, and compact ECDFs.
  result <- .resid_summarise(adapter, time, group_data, nsim, batch_size, seed,
    grid_size, level, calibration_bins, calibration_min_n, calibration_groups)
  if (!dharma) return(result)
  integer_response <- integer_response %||% .resid_dharma_integer(adapter)
  # Separate the DHARMa tie-randomisation stream from the simulation stream,
  # including brms draw selection. Reusing the original seed would reuse its
  # initial random numbers. Record this deterministic sub-seed for native replay.
  dharma_seed <- as.integer((as.double(seed) + 104729) %% .Machine$integer.max)
  native_dharma <- .resid_dharma_calculate(full, adapter$observed,
    result$observations$predicted, integer_response, dharma_seed)
  pit <- native_dharma$scaledResiduals
  endpoints <- pit == 0 | pit == 1
  # Endpoint placeholders must not be less extreme than finite interior scores.
  # A fixed cap alone could put PIT=1 below PIT=.9999 and reverse the ordering.
  interior <- stats::qnorm(pit[!endpoints])
  bound <- max(stats::qnorm((nsim + 0.5) / (nsim + 1)), abs(interior))
  limits <- c(-bound, bound)
  residual <- stats::residuals(native_dharma, quantileFunction = stats::qnorm,
    outlierValues = limits)
  result$observations$pit <- pit
  result$observations$residual <- residual
  result$observations$simulation_outlier <- endpoints
  result$qq$residual <- sort(residual)
  result$metadata$residual_method <- "dharma"
  result$metadata$dharma_version <- as.character(utils::packageVersion("DHARMa"))
  result$metadata$dharma_method <- "PIT"
  result$metadata$dharma_seed <- dharma_seed
  result$metadata$integer_response <- integer_response
  result$metadata$endpoint_count <- sum(endpoints)
  result$metadata$normal_score_limits <- limits
  result$metadata$dharma_matrix_mb <- matrix_mb
  if (retain_dharma) {
    result$dharma <- native_dharma
    result$metadata$retention <- "DHARMa object and full response matrix retained; no fitted model retained"
  }
  result
}

.resid_dharma_available <- function() {
  requireNamespace("DHARMa", quietly = TRUE) &&
    utils::packageVersion("DHARMa") >= "0.4.7"
}

.resid_method_options <- function(residual_method, retain_dharma, dharma_max_mb) {
  method <- match.arg(residual_method, c("simulation_pit", "dharma"))
  if (!is.logical(retain_dharma) || length(retain_dharma) != 1L || is.na(retain_dharma)) {
    stop("`retain_dharma` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(dharma_max_mb) || length(dharma_max_mb) != 1L ||
      !is.finite(dharma_max_mb) || dharma_max_mb <= 0) {
    stop("`dharma_max_mb` must be a positive finite matrix-size limit in MiB.", call. = FALSE)
  }
  if (retain_dharma && method != "dharma") {
    stop("`retain_dharma = TRUE` requires `residual_method = 'dharma'`.", call. = FALSE)
  }
  if (method == "dharma" && !.resid_dharma_available()) {
    stop("DHARMa residuals require optional package 'DHARMa' >= 0.4.7. Install or update it; the default simulation-PIT engine does not require it.", call. = FALSE)
  }
  method
}

.resid_dharma_memory <- function(n, nsim, maximum) {
  mb <- 8 * as.double(n) * as.double(nsim) / 1024^2
  if (mb > maximum) {
    stop("The DHARMa response matrix would require ", format(round(mb, 1), trim = TRUE),
      " MiB, exceeding `dharma_max_mb = ", maximum,
      "`. Reduce `nsim`, use the compact default, or explicitly increase the limit. Peak memory is larger than the matrix alone.", call. = FALSE)
  }
  if (mb >= 100) {
    warning("DHARMa needs a full response matrix (", format(round(mb, 1), trim = TRUE),
      " MiB) and additional working copies; `batch_size` does not limit this memory. The matrix is discarded unless `retain_dharma = TRUE`.", call. = FALSE)
  }
  mb
}

.resid_dharma_integer <- function(adapter) {
  if (adapter$response_kind %in% c("bernoulli", "grouped_binomial")) return(TRUE)
  family <- tolower(adapter$family)
  all(grepl("binomial|bernoulli|poisson|nbinom|negbin|negative binomial|genpois|compois", family))
}

.resid_dharma_calculate <- function(simulations, observed, predicted, integer_response, seed) {
  result <- DHARMa::createDHARMa(simulatedResponse = simulations,
    observedResponse = observed, fittedPredictedResponse = predicted,
    integerResponse = integer_response, seed = seed, method = "PIT")
  pit <- result$scaledResiduals
  if (!is.numeric(pit) || length(pit) != length(observed) ||
      any(!is.finite(pit) | pit < 0 | pit > 1)) {
    stop("DHARMa returned invalid scaled residuals; no substitute residual calculation was used.", call. = FALSE)
  }
  result
}

.resid_method_label <- function(x) {
  if (identical(x$metadata$residual_method, "dharma")) "DHARMa PIT" else "simulation-based PIT"
}

.resid_endpoint_caption <- function(x) {
  if (!identical(x$metadata$residual_method, "dharma")) return(NULL)
  limits <- x$metadata$normal_score_limits
  paste0("DHARMa 0/1 endpoints: ", x$metadata$endpoint_count,
    "; normal-score display limits +/-", format(round(limits[2L], 2), trim = TRUE),
    ". Uniform PIT values remain unchanged.")
}
