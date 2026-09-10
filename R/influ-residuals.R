#' Calculate compact simulation-based residual diagnostics
#'
#' Calculate once, then use [plot.influ_residuals()] for a four-panel overview:
#' a normal-score rank Q-Q plot, residuals against the predictive mean,
#' residuals by year, and a response-adaptive calibration or distribution panel.
#'
#' @param model A fitted GLM, `mgcv` GAM, `glmmTMB`, `brmsfit`, `sdmTMB`, or
#'   single-response `tinyVAST` model. No model is fitted by this function.
#' @param data Original model data, retaining original row names. Usually not
#'   needed; supply it if a transformed time term hides the raw year column.
#' @param year Name of the time column. By default, recognise year/fishing-year
#'   names in the formula, then native time metadata or a time-named term,
#'   then the first single-variable formula term (with a warning). Ambiguous
#'   choices require an explicit override. No arbitrary grouping is selected.
#' @param nsim Number of complete response simulations, at least 20.
#' @param batch_size Maximum number of simulations requested in each batch.
#' @param seed Integer random seed. The caller's random-number state is restored.
#' @param grid_size Approximate number of ECDF grid points, at least 20.
#' @param level Pointwise predictive interval coverage for ECDFs, and nominal
#'   independent-uniform reference coverage for the Q-Q panel.
#' @param component Response to diagnose: `"auto"` retains a joint model's
#'   `"combined"` response; `"encounter"` selects presence in supported hurdle
#'   models, and `"positive"` selects native sdmTMB component-2 simulations at
#'   rows with observed positive catch. Separate encounter/positive fits may
#'   also be labelled explicitly. Unsupported component extraction fails.
#' @param calibration_bins Requested number of roughly equal-observation-count
#'   probability bins. Defaults to 10; ties are never split. Near ties within
#'   `1e-8` are kept together, and under-supported bins are merged.
#' @param calibration_min_n Minimum observation count per probability bin,
#'   default 20. Smaller datasets remain a single flagged sparse bin. Scientific
#'   groups below this size remain visible but have no predictive envelope.
#' @param calibration_groups Optional character vector of original-data columns
#'   defining a joint scientific grouping, e.g. `c("year", "target")`. These
#'   columns must not be defined from the outcome. Supply `data` if necessary.
#' @param trial_counts For weighted one-column binomial GLM/GAM/glmmTMB fits,
#'   the name of the known trial-count column in `data`. It must equal the
#'   fitted trial weights. Arbitrary case weights are not treated as trials.
#'   Two-column success/failure responses need no override.
#' @param groups Optional names of original-data grouping columns to retain
#'   for [plot_implied_residuals()]. Specify these independently of the response.
#'   Only these columns, not the complete model data, are stored.
#'
#' @details Each simulation is a joint response vector, preserving the native
#'   method's within-draw dependence. For observation \eqn{i}, let \eqn{L_i}
#'   count simulated responses below the observation and \eqn{E_i} count ties.
#'   The randomised finite-simulation rank is
#'   \eqn{(L_i + U_i(E_i + 1))/(B + 1)}, with independent uniform \eqn{U_i}.
#'   Its normal score is a simulation-based quantile residual, not a Pearson
#'   residual or an exact analytic PIT. Randomisation includes zeros and other
#'   atoms without adding arbitrary noise to catches. Increase `nsim` and
#'   inspect seed sensitivity for important conclusions.
#'
#'   GLMs and GAMs simulate observation error at fitted parameters, including
#'   fitted smooths. `glmmTMB` uses its native simulation of new random effects.
#'   `sdmTMB` and `tinyVAST` use `type = "mle-eb"`: observation error conditional
#'   on fitted latent effects. brms uses joint posterior predictive draws,
#'   including existing group effects. These are different diagnostic targets,
#'   not interchangeable uncertainty estimates. The predictive mean on the
#'   horizontal axis is estimated from these same simulations, so it matches
#'   their conditioning rather than mixing in differently conditioned fitted
#'   values. No refitting, MCMC, or leave-one-out calculation is performed.
#'
#'   The Q-Q band is an independent-uniform reference, not a calibrated
#'   goodness-of-fit test for estimated, hierarchical, spatial, or Bayesian
#'   models. Posterior predictive ranks reuse the observations and need not be
#'   uniform. ECDF bands are pointwise simulated-response bands, not simultaneous
#'   confidence bands. By default, zero-inflated and delta simulations describe
#'   the combined response, not either component separately. Censored,
#'   multivariate, quasi-family, and non-binomial weighted fits are not supported.
#'   Native simulation failures are reported, not replaced by another family.
#'   A positive-component check needs its own fitted component and matching
#'   observations, or an explicit native component diagnostic; do not relabel
#'   or subset the combined-response overview as a positive-component check.
#'
#'   Bernoulli calibration uses native fitted probabilities, including fitted
#'   effects, separately from the simulation mean used in the first three
#'   panels. brms averages expected probabilities over the same posterior draw
#'   identities used for simulation. Bins are fixed before simulation. Whole
#'   simulated response vectors are reduced to proportions within those bins;
#'   their pointwise predictive envelopes are not confidence intervals for an
#'   underlying calibration curve or calibrated goodness-of-fit tests. In
#'   particular, glmmTMB simulations redraw random effects although binning uses
#'   fitted conditional probabilities, so the envelope need not centre on the
#'   identity line. Dependence is only that represented by the native simulator;
#'   no extra vessel or temporal dependence is added. Fitted-data calibration,
#'   including grouped checks, is exploratory. Matching overall or annual means
#'   may follow from fitted intercept/year effects and does not validate a model.
#'
#'   The object retains neither the fitted model nor an observation-by-simulation
#'   matrix. Working storage includes an observation-by-batch matrix and a
#'   grid-by-simulation matrix, plus compact bin/group simulation summaries.
#'   Native backends may allocate additional memory.
#'   The ECDF grid spans observations and the first simulation batch; it is
#'   deliberately compact, not an exact representation of every simulated jump.
#'   For binomial GLMs and `glmmTMB`, responses are success counts (including
#'   proportion responses with integer trial weights).
#'
#' @return An S3 `influ_residuals` object containing observation-level ranks,
#'   normal scores, predictive means, year labels, Q-Q reference coordinates,
#'   compact ECDF summaries, and explicit calculation metadata.
#' @seealso [plot.influ_residuals()], [plot_predicted_residuals()],
#'   [plot_implied_residuals()]
#' @examples
#' if (requireNamespace("glmmTMB", quietly = TRUE)) {
#' data(lobsters_per_pot)
#' fit <- glmmTMB::glmmTMB(
#'   lobsters ~ year + poly(depth, 3) + poly(soak, 3) + (1 | month),
#'   family = glmmTMB::nbinom2(), data = lobsters_per_pot)
#' checks <- influ_residuals(fit, nsim = 50, seed = 42)
#' checks
#' plot(checks)
#' plot(checks, type = "qq")
#' }
#' @md
#' @export
influ_residuals <- function(model, data = NULL, year = NULL, nsim = 250L,
                            batch_size = 25L, seed = 1L, grid_size = 201L,
                            level = 0.95,
                            component = c("auto", "combined", "encounter", "positive"),
                            calibration_bins = 10L, calibration_min_n = 20L,
                            calibration_groups = NULL, trial_counts = NULL,
                            groups = NULL) {
  component <- match.arg(component)
  .resid_integer(calibration_bins, "calibration_bins", 1L)
  .resid_integer(calibration_min_n, "calibration_min_n", 1L)
  .resid_integer(nsim, "nsim", 20L)
  .resid_integer(batch_size, "batch_size", 1L)
  .resid_integer(grid_size, "grid_size", 20L)
  .resid_integer(seed, "seed", 0L)
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) ||
      level <= 0 || level >= 1) {
    stop("`level` must be a number between zero and one.", call. = FALSE)
  }
  saved_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else NULL
  on.exit({
    if (is.null(saved_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    } else assign(".Random.seed", saved_seed, envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)

  if (!is.null(groups)) {
    if (!is.character(groups) || !length(groups) || anyNA(groups) ||
        any(!nzchar(groups)) || anyDuplicated(groups)) {
      stop("`groups` must contain unique original-data column names.", call. = FALSE)
    }
    if (is.null(data) && !all(groups %in% names(.residual_model_frame(model)))) {
      data <- .resolve_influ_data(model, NULL)
    }
  }
  adapter <- .resid_adapter(model, data, nsim, trial_counts, component)
  if (!all(groups %in% names(adapter$data))) {
    stop("`groups` must name columns in the model data; supply original `data`.", call. = FALSE)
  }
  group_data <- adapter$data[groups %||% character()]
  if (anyNA(group_data) || any(!vapply(group_data, function(x) {
    is.atomic(x) && is.null(dim(x)) &&
      (!is.numeric(x) || all(is.finite(x)))
  }, logical(1)))) {
    stop("Grouping columns must be finite categorical values without missing values.", call. = FALSE)
  }
  if (any(groups %in% adapter$response_variables)) {
    stop("Residual groups must not be defined from the model response.", call. = FALSE)
  }
  time <- .resid_year(model, adapter$data, year)
  observed <- adapter$observed
  n <- length(observed)
  if (n < 3L || any(!is.finite(observed))) {
    stop("At least three finite observed responses are required.", call. = FALSE)
  }
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
      stop("Native simulations must contain finite responses in an ",
        "observation-by-simulation matrix matching the fitted rows.", call. = FALSE)
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
  if (!is.null(adapter$probability)) {
    result$observations$probability <- adapter$probability
    result$observations$trials <- adapter$trials
    result$observations$calibration_bin <- calibration$bin
  }
  result
}

.resid_integer <- function(x, name, minimum) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x < minimum || x > .Machine$integer.max || x != floor(x)) {
    stop("`", name, "` must be an integer of at least ", minimum, ".", call. = FALSE)
  }
}

.resid_formula <- function(model) {
  f <- if (inherits(model, "brmsfit")) model$formula$formula else {
    if (inherits(model, "tinyVAST")) model$formula else {
      if (inherits(model, "sdmTMB")) model$formula[[1L]] else stats::formula(model)
    }
  }
  if (!inherits(f, "formula") || length(f) != 3L) {
    stop("A single response formula is required.", call. = FALSE)
  }
  f
}

.resid_year <- function(model, data, year) {
  f <- .resid_formula(model)
  variables <- intersect(all.vars(f[[3L]]), names(data))
  normalise <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  source <- "explicit"
  if (is.null(year)) {
    matches <- variables[normalise(variables) %in%
      c("year", "yr", "fy", "fishingyear", "fishyear", "fyear", "yearf")]
    source <- "year-name detection"
    if (!length(matches)) {
      native <- if (inherits(model, "sdmTMB")) model$time else {
        if (inherits(model, "tinyVAST")) model$internal$time_column else NULL
      }
      matches <- intersect(native, names(data))
      matches <- matches[vapply(matches, function(name) {
        length(unique(data[[name]])) > 1L
      }, logical(1))]
      source <- "native time metadata"
    }
    if (!length(matches)) {
      matches <- variables[normalise(variables) %in% c("time", "season")]
      source <- "time-name detection"
    }
    if (!length(matches)) {
      terms <- attr(stats::terms(f, keep.order = TRUE), "term.labels")
      if (length(terms)) {
        first <- all.vars(stats::as.formula(paste("~", terms[1L])))
        matches <- intersect(first, names(data))
      }
      source <- "first formula term"
    }
    if (length(matches) != 1L) {
      stop("Could not identify one unambiguous time column. Supply `year =` ",
        "with the fishing-year column name and original `data` if needed.", call. = FALSE)
    }
    year <- matches
    if (source == "first formula term") {
      warning("Using first formula term '", year,
        "' as time. Set `year =` explicitly if this is not fishing year.", call. = FALSE)
    }
  }
  if (!is.character(year) || length(year) != 1L || is.na(year) ||
      !year %in% names(data)) {
    stop("`year` must name one column in the fitted data.", call. = FALSE)
  }
  value <- data[[year]]
  if (!is.atomic(value) || !is.null(dim(value)) || anyNA(value) ||
      (is.numeric(value) && any(!is.finite(value)))) {
    stop("The time column must contain finite, non-missing labels.", call. = FALSE)
  }
  labels <- unique(as.character(value))
  numeric <- suppressWarnings(as.numeric(labels))
  levels <- if (all(is.finite(numeric)) && !anyDuplicated(numeric)) {
    labels[order(numeric)]
  } else sort(labels)
  list(name = year, source = source, levels = levels)
}

#' @rdname influ_residuals
#' @param x An `influ_residuals` object.
#' @param ... Reserved for future methods; currently unused.
#' @export
print.influ_residuals <- function(x, ...) {
  cat("Simulation-based residual diagnostics (", x$metadata$backend, ")\n", sep = "")
  cat(nrow(x$observations), "observations;", x$metadata$nsim, "simulations\n")
  cat("Time:", x$metadata$year, "[", x$metadata$year_source, "]\n")
  cat(x$metadata$scheme, "\n", x$metadata$calibration, "\n", sep = "")
  invisible(x)
}
