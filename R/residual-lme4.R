# Keep the frozen six-backend simulation engine unchanged.
.resid_backend_adapter <- function(model, data, nsim, trial_counts, component,
    conditioning, response_transform = "identity") {
  if (inherits(model, "merMod")) return(.resid_lme4_adapter(model, data,
    trial_counts, component, conditioning, response_transform))
  if (response_transform != "identity") stop("`response_transform` is currently an lme4 option only.", call. = FALSE)
  .resid_adapter(model, data, nsim, trial_counts, component, conditioning)
}

.resid_lme4_adapter <- function(model, data, trial_counts, component,
    conditioning, response_transform) {
  info <- .lme4_check(model, response_transform)
  if (is.null(data) && !is.null(trial_counts) &&
      !trial_counts %in% names(stats::model.frame(model))) data <- .resolve_influ_data(model)
  data <- .lme4_data(model, data)
  if (!is.character(conditioning) || length(conditioning) != 1L || is.na(conditioning)) stop("Supply one explicit lme4 conditioning choice.", call. = FALSE)
  requested <- conditioning
  if (conditioning == "backend_default") conditioning <- "new_effects"
  if (!conditioning %in% c("fitted", "new_effects")) stop("lme4 residual conditioning supports 'fitted' or 'new_effects' only.", call. = FALSE)
  frame <- stats::model.frame(model)
  observed <- stats::model.response(frame)
  weights <- stats::model.weights(frame)
  binomial <- info$canonical == "binomial"
  trials <- NULL
  if (binomial) {
    if (is.matrix(observed)) {
      if (ncol(observed) != 2L || (!is.null(weights) && any(weights != 1))) stop("Binomial cbind responses require two columns without additional case weights.", call. = FALSE)
      trials <- rowSums(observed)
      observed <- observed[, 1L]
    } else if (is.factor(observed)) {
      if (nlevels(observed) != 2L || (!is.null(weights) && any(weights != 1))) stop("Binomial factors require two levels and unit weights.", call. = FALSE)
      observed <- as.integer(observed) - 1L
      trials <- rep(1, length(observed))
    } else {
      trials <- weights %||% rep(1, length(observed))
      if (any(trials != 1) && is.null(trial_counts)) stop("Weighted one-column binomial fits require explicit `trial_counts`, or use cbind(successes, failures).", call. = FALSE)
      observed <- observed * trials
    }
    if (!is.null(trial_counts) && !isTRUE(all.equal(as.numeric(.resid_known_trials(trial_counts, data)), as.numeric(trials)))) stop("`trial_counts` must match fitted binomial trial counts.", call. = FALSE)
    if (any(!is.finite(trials) | trials <= 0 | trials != round(trials)) ||
        any(!is.finite(observed) | observed < 0 | observed > trials | abs(observed - round(observed)) > 1e-7)) stop("Binomial responses require positive integer trials and integer successes.", call. = FALSE)
    observed <- round(observed)
  } else {
    if (!is.null(trial_counts)) stop("`trial_counts` applies only to binomial lme4 fits.", call. = FALSE)
    if (!is.null(weights) && any(!is.finite(weights) | weights != 1)) stop("Case-weighted lme4 simulations require a separate predictive-weight contract.", call. = FALSE)
  }
  if (info$transform == "log") observed <- exp(observed)
  if (!is.numeric(observed) || !is.null(dim(observed)) || any(!is.finite(observed))) stop("One finite numeric response per fitted lme4 row is required.", call. = FALSE)
  n <- length(observed)
  simulation_weights <- if (binomial) trials else rep(1, n)
  simulation_model <- model
  # This value-copy of the frame removes only output reinsertion metadata.
  # Native coefficients/response modules remain fitted to the same rows, and
  # no slot or reference-class parameter of the caller's model is changed.
  attr(simulation_model@frame, "na.action") <- NULL
  simulate <- function(ids) {
    # One native joint vector per draw gives the same RNG stream at every
    # batch size, without retaining a full observation-by-simulation matrix.
    out <- vapply(ids, function(id) {
      v <- stats::simulate(simulation_model, nsim = 1L, newdata = data,
        re.form = if (conditioning == "fitted") NULL else NA,
        # Native newdata simulation does not recover trial weights, and
        # na.exclude can reinsert NA weights from the original fitting call.
        # Supply the retained, validated simulation-row weights explicitly.
        weights = simulation_weights)[[1L]]
      if (binomial) {
        if (is.matrix(v)) v <- v[, 1L] else {
          if (is.factor(v)) v <- as.integer(v) - 1L else v <- v * trials
        }
      }
      if (info$transform == "log") v <- exp(v)
      as.numeric(v)
    }, numeric(n))
    matrix(out, nrow = n)
  }
  adapter <- list(backend = "lme4", observed = as.numeric(observed), data = data,
    response = if (binomial) "Successes" else info$natural_response,
    native_response = info$response, response_transform = info$transform,
    family = if (info$transform == "log") "lognormal" else info$family$family,
    trials = trials, structure = "single response", simulate = simulate,
    scheme = if (conditioning == "fitted") "Native observation simulations conditional on fitted random effects" else "Native observation simulations with new random effects at fitted variance parameters",
    conditioning = conditioning, conditioning_requested = requested,
    response_variables = all.vars(stats::formula(model)[[2L]]))
  if (binomial) adapter$conditional_probability <- function(hurdle = FALSE) {
    .align_observation_values(stats::predict(model, type = "response", re.form = NULL),
      frame, "Fitted binomial probabilities", model)
  }
  .resid_response_adapter(adapter, model, component, 1L)
}
