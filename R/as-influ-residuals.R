#' Diagnose externally simulated responses
#'
#' Reduce an already generated response-simulation matrix to the same compact
#' diagnostic returned by [influ_residuals()]. No model is fitted, and no new
#' responses are simulated.
#'
#' @param simulations Numeric matrix with observations in rows and complete
#'   joint response simulations in columns (at least 20). Row names are required
#'   and must exactly match the observation IDs in `data`, in the same order.
#'   No transposition, sorting, recycling, or omission is performed. Optional
#'   column names must be unique, non-empty simulation identifiers.
#' @param data Data frame containing the matching observations, time labels,
#'   and any requested grouping or calibration columns. Exactly one row per
#'   simulation row is required; supply only the rows actually simulated.
#' @param response Name of the numeric observed-response column in `data`.
#'   For Bernoulli/binomial inputs, supply integer success counts, not proportions.
#' @param year Name of the time column in `data`. This is required because no
#'   fitted formula or native time metadata is available for automatic detection.
#' @param response_kind Explicit response interpretation: `"distribution"`
#'   for general counts or continuous responses, `"positive_continuous"` for
#'   strictly positive continuous responses, `"combined"` for a non-negative
#'   combined hurdle/delta/zero-inflated response, `"bernoulli"` for one-trial
#'   successes, or `"grouped_binomial"` for known multi-trial successes. This
#'   is not inferred from observed zeros, ones, or the supplied simulations.
#' @param conditioning Required non-empty description of how the response
#'   simulations were generated, including parameter uncertainty and treatment
#'   of random, spatial, and spatiotemporal effects. This is a user declaration,
#'   not something that influ2 can verify from a matrix.
#' @param component `NULL` (default) means `"combined"` when `response_kind`
#'   is `"combined"`, and `"single"` otherwise. Explicit `"encounter"` requires
#'   Bernoulli inputs. Explicit `"positive"` requires strictly positive inputs
#'   with kind `"distribution"` or `"positive_continuous"`. These labels declare
#'   what was supplied; they never extract, transform, or filter components.
#' @param observation_id Optional name of a unique, non-missing ID column in
#'   `data`; otherwise use its row names. IDs are compared as character strings.
#' @param probability Name of the fitted success-probability column in `data`,
#'   required for Bernoulli/binomial calibration, and disallowed otherwise.
#'   Supply original fitted probabilities, not row means of the response
#'   simulations. For posterior predictions, average expected probabilities
#'   over the same posterior draw IDs used for the response simulations.
#' @param probability_conditioning Required description of the conditioning
#'   used for `probability`, when supplied. Fixed probability bins may use
#'   different conditioning from response simulations (e.g. fitted versus
#'   resimulated random effects); record that difference explicitly.
#' @param trial_counts Name of a positive-integer trial-count column in `data`.
#'   Required for `"grouped_binomial"`, where at least one row must have more
#'   than one trial. Bernoulli defaults to one trial per row. Never use arbitrary
#'   fitting or area weights as trial counts.
#' @param batch_size Maximum number of supplied simulation columns processed
#'   in each batch. No additional responses are generated.
#' @param calibration_groups Optional character vector of columns in `data`
#'   defining a joint scientific grouping, e.g. `c("year", "target")`. These
#'   columns must not be defined from the outcome.
#' @inheritParams influ_residuals
#'
#' @details Each column must be a whole response vector from the intended
#'   predictive distribution, not fitted means, parameter draws, or PIT values.
#'   Existing within-column dependence is preserved. The predictive mean and
#'   response ECDF are calculated from these same supplied simulations. No
#'   separate fitted-mean argument can introduce a different prediction target.
#'
#'   Validate native output order before assigning row names. For example,
#'   brms posterior predictions normally need transposition to put observations
#'   in rows; do not guess orientation from a square matrix. The caller is
#'   responsible for the provenance of the IDs and declared response structure.
#'   Matrix alignment checks cannot detect incorrectly assigned identifiers.
#'
#'   A combined-response diagnostic is not a positive-component diagnostic.
#'   For the latter, generate positive-component simulations at the matching
#'   observed-positive rows. Do not remove zeros separately within simulation
#'   columns or relabel a combined matrix as a component calculation. No
#'   conversion of censored, multivariate, weighted, or other special response
#'   encodings is provided; resolve the predictive target before using this API.
#'
#'   The shared engine randomises ties and applies `qnorm(pit)` exactly as for
#'   fitted models. The caller's random-number state is restored, including on
#'   error. Reusing the same matrix, seed, batch size, and RNG kind reproduces
#'   the result. A native adapter can consume random numbers when preparing
#'   simulations (e.g. selecting posterior draws), so an identical seed alone
#'   does not guarantee identical ranks between the two entry points.
#'
#'   This initial interface accepts an in-memory matrix, not a generator or
#'   on-disk stream. It is processed in observation-by-batch blocks; the returned
#'   object retains neither the matrix nor a closure capturing it. The caller's
#'   original matrix still occupies memory until they release it. Temporary
#'   ECDF/calibration storage and the first-batch ECDF grid follow the existing
#'   [influ_residuals()] calculation. Changing batch size can change that grid,
#'   but not the ranks or sequentially calculated predictive means for fixed
#'   supplied simulations. No counters or full simulation-retention mode is added.
#'
#'   These remain exploratory predictive checks. Fitted-data ranks are not
#'   automatically uniform or calibrated for parameter estimation, posterior
#'   data reuse, or latent dependence. This constructor does not implement
#'   LOO-PIT, OSA, or validate a model-specific simulation scheme.
#'
#' @return An `influ_residuals` object, using the same plotting and grouped
#'   residual helpers as fitted-model results. Metadata identifies external
#'   input, explicit alignment, response component, and declared conditioning.
#' @seealso [influ_residuals()], [plot.influ_residuals()],
#'   [plot_predicted_residuals()], [plot_implied_residuals()]
#' @examples
#' d <- data.frame(year = rep(2010:2012, each = 10),
#'   cpue = rep(c(0, 1, 2, 3, 4), 6))
#' # A small supplied-simulation example, not a fitted CPUE model.
#' set.seed(42)
#' sims <- matrix(rpois(nrow(d) * 30, lambda = 2), nrow = nrow(d),
#'   dimnames = list(rownames(d), NULL))
#' checks <- as_influ_residuals(sims, d, response = "cpue", year = "year",
#'   response_kind = "distribution",
#'   conditioning = "Independent Poisson responses at a fixed mean of 2")
#' checks
#' @export
#' @md
as_influ_residuals <- function(simulations, data, response, year, response_kind,
    conditioning, component = NULL, observation_id = NULL, probability = NULL,
    probability_conditioning = NULL, trial_counts = NULL, batch_size = 25L,
    seed = 1L, grid_size = 201L, level = 0.95, groups = NULL,
    calibration_bins = 10L, calibration_min_n = 20L, calibration_groups = NULL) {
  if (!is.matrix(simulations) || !is.numeric(simulations)) {
    stop("`simulations` must be a numeric observation-by-simulation matrix.", call. = FALSE)
  }
  nsim <- ncol(simulations)
  .resid_integer(nsim, "number of simulation columns", 20L)
  .resid_integer(batch_size, "batch_size", 1L)
  .resid_integer(seed, "seed", 0L)
  .resid_integer(grid_size, "grid_size", 20L)
  .resid_integer(calibration_bins, "calibration_bins", 1L)
  .resid_integer(calibration_min_n, "calibration_min_n", 1L)
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1) {
    stop("`level` must be a number between zero and one.", call. = FALSE)
  }
  if (!is.data.frame(data) || nrow(data) < 3L || nrow(data) != nrow(simulations) ||
      anyNA(names(data)) || any(!nzchar(names(data))) || anyDuplicated(names(data))) {
    stop("`data` must have at least three rows, exactly match the simulation rows, and have unique non-empty column names.", call. = FALSE)
  }
  data <- as.data.frame(data)
  .resid_external_text(conditioning, "conditioning")
  .resid_external_text(response_kind, "response_kind")
  response_kind <- match.arg(response_kind, c("distribution", "positive_continuous",
    "combined", "bernoulli", "grouped_binomial"))
  if (is.null(component)) component <- if (response_kind == "combined") "combined" else "single"
  component <- match.arg(component, c("single", "combined", "encounter", "positive"))
  if ((component == "combined") != (response_kind == "combined") ||
      (component == "encounter" && response_kind != "bernoulli") ||
      (component == "positive" && !response_kind %in% c("distribution", "positive_continuous"))) {
    stop("`component` must agree with the declared `response_kind`; no components are extracted or relabelled automatically.", call. = FALSE)
  }
  ids <- if (is.null(observation_id)) rownames(data) else {
    .resid_external_column(data, observation_id, "observation_id")
  }
  .resid_external_ids(ids, "Observation IDs")
  ids <- as.character(ids)
  .resid_external_ids(rownames(simulations), "Simulation row names")
  if (!identical(rownames(simulations), ids)) {
    stop("Simulation row names must exactly match observation IDs in data order; align them explicitly before calling as_influ_residuals().", call. = FALSE)
  }
  if (!is.null(colnames(simulations))) .resid_external_ids(colnames(simulations), "Simulation column names")
  if (!identical(rownames(data), ids)) rownames(data) <- ids
  observed <- .resid_external_column(data, response, "response")
  if (!is.numeric(observed) || any(!is.finite(observed))) {
    stop("`response` must identify one finite numeric observed value per row.", call. = FALSE)
  }
  .resid_external_support(observed, response_kind, component)
  .resid_external_column(data, year, "year")
  # Use the same finite time-label ordering as native diagnostics.
  time <- .resid_time_labels(data, year, "explicit external column")
  .resid_external_groups(data, groups, response, "groups")
  .resid_external_groups(data, calibration_groups, response, "calibration_groups")
  p <- trials <- prediction_type <- NULL
  if (response_kind %in% c("bernoulli", "grouped_binomial")) {
    p <- .resid_external_column(data, probability, "probability")
    if (!is.numeric(p) || any(!is.finite(p) | p < 0 | p > 1)) {
      stop("`probability` must identify finite fitted success probabilities in [0, 1].", call. = FALSE)
    }
    .resid_external_text(probability_conditioning, "probability_conditioning")
    prediction_type <- paste("User-supplied fitted probabilities:", probability_conditioning)
    trials <- if (is.null(trial_counts) && response_kind == "bernoulli") rep(1, nrow(data)) else {
      .resid_external_column(data, trial_counts, "trial_counts")
    }
    if (!is.numeric(trials) || any(!is.finite(trials) | trials < 1 | trials != round(trials)) ||
        (response_kind == "bernoulli" && any(trials != 1)) ||
        (response_kind == "grouped_binomial" && all(trials == 1))) {
      stop("Trial counts must be positive integers: all one for Bernoulli, and at least one greater than one for grouped binomial.", call. = FALSE)
    }
  } else if (!is.null(probability) || !is.null(probability_conditioning) || !is.null(trial_counts)) {
    stop("Probability and trial-count inputs apply only to Bernoulli/binomial response kinds.", call. = FALSE)
  }
  adapter <- list(backend = "external", observed = as.numeric(observed), data = data,
    response = response, family = "user-supplied response distribution",
    structure = paste("User-supplied", component, "response"), component = component,
    response_kind = response_kind, scheme = conditioning, probability = p,
    trials = trials, prediction_type = prediction_type, response_variables = response,
    simulate = function(ids) {
      block <- simulations[, ids, drop = FALSE]
      .resid_external_support(block, response_kind, component)
      # IDs have already been validated. Do not let matrix dimnames become
      # incidental names on predictive means or data-frame row names.
      dimnames(block) <- NULL
      block
    })
  saved_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else NULL
  on.exit({
    if (is.null(saved_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    } else assign(".Random.seed", saved_seed, envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
  result <- .resid_summarise(adapter, time, data[groups %||% character()], nsim,
    batch_size, seed, grid_size, level, calibration_bins, calibration_min_n, calibration_groups)
  result$metadata$input <- "external_response_matrix"
  result$metadata$observation_id <- observation_id %||% "row.names"
  result$metadata$alignment <- "Exact observation ID and row-order match; no automatic reordering"
  result
}

.resid_external_text <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop("`", name, "` must be one non-empty description.", call. = FALSE)
  }
}

.resid_external_column <- function(data, name, argument) {
  if (!is.character(name) || length(name) != 1L || is.na(name) || !name %in% names(data)) {
    stop("`", argument, "` must name one column in aligned `data`.", call. = FALSE)
  }
  x <- data[[name]]
  if (!is.atomic(x) || !is.null(dim(x)) || length(x) != nrow(data) || anyNA(x) ||
      (is.numeric(x) && any(!is.finite(x)))) {
    stop("`", argument, "` must identify finite, non-missing scalar values, one per row.", call. = FALSE)
  }
  x
}

.resid_external_ids <- function(x, label) {
  if (is.null(x) || !is.atomic(x) || !is.null(dim(x)) || anyNA(x) ||
      (is.numeric(x) && any(!is.finite(x))) || any(!nzchar(trimws(as.character(x)))) ||
      anyDuplicated(as.character(x))) {
    stop(label, " must be unique, non-missing, non-empty identifiers.", call. = FALSE)
  }
}

.resid_external_support <- function(x, kind, component) {
  if ((kind == "positive_continuous" || component == "positive") && any(x <= 0, na.rm = TRUE)) {
    stop("A positive response/component requires strictly positive observations and simulations; no zeros are filtered out.", call. = FALSE)
  }
  if (kind == "combined" && any(x < 0, na.rm = TRUE)) {
    stop("Combined CPUE responses must be non-negative, including any zero responses.", call. = FALSE)
  }
}

.resid_external_groups <- function(data, groups, response, name) {
  if (is.null(groups)) return(invisible(NULL))
  if (!is.character(groups) || !length(groups) || anyNA(groups) ||
      anyDuplicated(groups) || !all(groups %in% names(data))) {
    stop("`", name, "` must name distinct columns in aligned `data`.", call. = FALSE)
  }
  if (response %in% groups) stop("Residual groups must not be defined from the observed response.", call. = FALSE)
  for (column in groups) .resid_external_column(data, column, name)
  invisible(NULL)
}
