.resid_known_trials <- function(trial_counts, data) {
  if (!is.character(trial_counts) || length(trial_counts) != 1L ||
      is.na(trial_counts) || !trial_counts %in% names(data)) {
    stop("`trial_counts` must name the known trial-count column in aligned `data`.", call. = FALSE)
  }
  data[[trial_counts]]
}

.resid_fitted_probability <- function(model, adapter, draw_ids = NULL, hurdle = FALSE) {
  backend <- adapter$backend
  n <- nrow(adapter$data)
  if (backend %in% c("glm", "gam")) {
    probability <- model$fitted.values
  } else if (backend == "glmmTMB") {
    probability <- if (hurdle) 1 - stats::predict(model, type = "zprob") else {
      stats::predict(model, type = "response")
    }
    alignment_model <- model
    alignment_model$na.action <- attr(model$frame, "na.action")
    probability <- .align_observation_values(probability, .residual_model_frame(model),
      "Fitted encounter probabilities", alignment_model)
  } else if (backend == "sdmTMB") {
    value <- stats::predict(model, type = "response", offset = model$offset)
    probability <- if (hurdle) value$est1 else value$est
  } else if (backend == "tinyVAST") {
    if (hurdle) {
      family <- model$internal$family[[1L]]
      if (!identical(family$type, "standard")) {
        stop("tinyVAST encounter calibration currently requires the standard delta link; use a separate encounter fit for other delta parameterisations.", call. = FALSE)
      }
      probability <- family[[1L]]$linkinv(stats::predict(model, what = "p1_g"))
    } else probability <- stats::predict(model, what = "mu_g")
  } else {
    # Keep posterior averaging compact, using the same joint draw identities
    # selected for posterior response simulation (no new MCMC or random draws).
    probability <- numeric(n)
    for (start in seq.int(1L, length(draw_ids), by = 25L)) {
      ids <- draw_ids[seq.int(start, min(length(draw_ids), start + 24L))]
      args <- list(object = model, draw_ids = ids, sort = FALSE)
      if (hurdle) args$dpar <- "hu"
      value <- do.call(brms::posterior_epred, args)
      if (!is.matrix(value) || !identical(dim(value), c(length(ids), n))) {
        stop("Native posterior probabilities could not be aligned to the observed rows.", call. = FALSE)
      }
      probability <- probability + colSums(if (hurdle) 1 - value else value)
    }
    probability <- probability / length(draw_ids)
    if (!hurdle) probability <- probability / adapter$trials
  }
  if (!is.numeric(probability) || length(probability) != n ||
      any(!is.finite(probability) | probability < 0 | probability > 1)) {
    stop("Native fitted probabilities must be finite, lie in [0, 1], and align with every fitted response row.", call. = FALSE)
  }
  as.numeric(probability)
}

.resid_response_adapter <- function(adapter, model, component, nsim, draw_ids = NULL) {
  family <- tolower(adapter$family)
  backend <- adapter$backend
  n <- length(adapter$observed)
  delta <- grepl("combined", adapter$structure)
  hurdle <- delta && (backend %in% c("sdmTMB", "tinyVAST") ||
    any(grepl("hurdle", family)) ||
    (backend == "glmmTMB" && any(grepl("gamma|truncated", family))))
  if (component == "auto") component <- if (delta) "combined" else "single"
  if (component == "combined" && !delta) {
    stop("`component = 'combined'` requires a joint delta, hurdle, or zero-inflated model.", call. = FALSE)
  }
  probability_response <- !delta && length(family) == 1L &&
    family %in% c("binomial", "bernoulli", "betabinomial", "beta_binomial")
  if (component %in% c("encounter", "positive") && delta) {
    if (!hurdle) stop("Encounter/positive extraction from zero-inflated count mixtures is not supported: the zero-inflation probability is not P(catch = 0). Use a separate component-aware fit; combined diagnostics remain available.", call. = FALSE)
    if (component == "positive") {
      if (backend != "sdmTMB") {
        stop("A positive-only diagnostic for this joint model requires a separate positive fit or a native component simulator. Automatic positive-component extraction is currently supported for sdmTMB only; combined simulations are never filtered into a positive diagnostic.", call. = FALSE)
      }
      keep <- which(adapter$observed > 0)
      # Native component 2 simulates a positive amount at each row, including
      # rows whose combined simulated outcome would be zero. Select rows by
      # the ORIGINAL observation, never by each simulated outcome.
      adapter$simulate <- function(ids) stats::simulate(model,
        nsim = length(ids), type = "mle-eb", model = 2,
        seed = sample.int(.Machine$integer.max, 1L), silent = TRUE)[keep, , drop = FALSE]
      adapter$data <- adapter$data[keep, , drop = FALSE]
      adapter$observed <- adapter$observed[keep]
      adapter$family <- model$family[[2L]]$family
      adapter$structure <- "positive component of delta response"
      adapter$response_kind <- "positive_continuous"
      adapter$component <- component
      adapter$trials <- NULL
      return(adapter)
    }
    adapter$trials <- rep(1, n)
    adapter$probability <- .resid_fitted_probability(model, adapter, draw_ids, hurdle = TRUE)
    original_simulate <- adapter$simulate
    adapter$simulate <- function(ids) 1 * (original_simulate(ids) > 0)
    adapter$observed <- as.numeric(adapter$observed > 0)
    adapter$response <- "Encounter (positive catch)"
    adapter$structure <- "encounter component of hurdle/delta response"
    adapter$response_kind <- "bernoulli"
  } else if (probability_response) {
    if (backend == "brms") {
      adapter$trials <- if (family == "bernoulli") rep(1, n) else {
        brms::standata(model)$trials
      }
    } else if (backend == "tinyVAST") {
      adapter$trials <- model$tmb_inputs$tmb_data$size_i
    }
    if (is.null(adapter$trials)) stop("Known binomial trial counts are required for response routing and calibration.", call. = FALSE)
    if (length(adapter$trials) == 1L) adapter$trials <- rep(adapter$trials, n)
    adapter$response_kind <- if (all(adapter$trials == 1)) "bernoulli" else "grouped_binomial"
    if (component == "positive") stop("A binomial response is not a positive-catch component.", call. = FALSE)
    if (component == "encounter" && adapter$response_kind != "bernoulli") {
      stop("An encounter component requires one trial per observation; use explicit binomial calibration for grouped trials.", call. = FALSE)
    }
    adapter$probability <- .resid_fitted_probability(model, adapter, draw_ids)
  } else {
    if (component == "encounter") stop("An encounter component requires a Bernoulli or supported joint hurdle/delta model, not a count or continuous family.", call. = FALSE)
    positive <- length(family) == 1L && family %in% c("gamma", "lognormal", "inverse.gaussian")
    if (component == "positive" && !positive) {
      stop("A positive component requires an explicitly supported positive-response family.", call. = FALSE)
    }
    adapter$response_kind <- if (delta) "combined" else if (positive) "positive_continuous" else "distribution"
  }
  adapter$component <- component
  if (!is.null(adapter$probability)) {
    adapter$prediction_type <- if (backend == "brms") {
      "Posterior mean encounter/success probabilities including existing group effects"
    } else "Fitted encounter/success probabilities conditional on fitted effects"
  }
  adapter
}
