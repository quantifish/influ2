.resid_adapter <- function(model, data, nsim, trial_counts = NULL, component = "auto",
    conditioning = "backend_default") {
  backend <- if (inherits(model, "brmsfit")) "brms" else {
    choices <- c("glmmTMB", "sdmTMB", "tinyVAST", "gam", "glm")
    choices[vapply(choices, inherits, logical(1), x = model)][1L]
  }
  if (is.na(backend)) stop("No residual simulation adapter for this model class.", call. = FALSE)
  requested_conditioning <- conditioning
  conditioning <- .resid_conditioning(backend, conditioning)
  if (backend %in% c("brms", "tinyVAST")) .check_residual_model(model)
  frame <- .residual_model_frame(model)
  if (is.null(frame)) stop("A retained model frame is required for residual diagnostics.", call. = FALSE)
  f <- .resid_formula(model)
  # Use the retained frame by default, not a possibly changed call$data object.
  # Explicit data are checked against every available fitted observation column.
  if (is.null(data) && all(all.vars(f[[3L]]) %in% names(frame))) data <- frame else {
    data <- .residual_observations(model, data, seq_len(nrow(frame)))$data
  }
  structure <- "single response"
  if (backend %in% c("glm", "gam", "glmmTMB")) {
    observed <- stats::model.response(frame)
  } else {
    lhs <- f[[2L]]
    # brms response additions encode censoring, trials, weights, etc. Only
    # trials are currently supported; do not interpret censoring limits as y.
    if (backend == "brms" && is.call(lhs) && identical(lhs[[1L]], as.name("|"))) {
      additions <- all.names(lhs[[3L]], functions = TRUE)
      if (any(!additions %in% c("trials", all.vars(lhs[[3L]])))) {
        stop("Only brms `trials()` response additions are supported; censored, ",
          "weighted, and other special responses need a dedicated diagnostic.", call. = FALSE)
      }
      lhs <- lhs[[2L]]
    }
    if (!is.symbol(lhs) || !as.character(lhs) %in% names(data)) {
      stop("This backend requires a single named observed response column.", call. = FALSE)
    }
    observed <- data[[as.character(lhs)]]
  }
  response <- paste(deparse(f[[2L]]), collapse = " ")
  family <- if (backend == "glmmTMB") stats::family(model)$family else {
    if (backend == "tinyVAST") model$internal$family[[1L]]$family else model$family$family
  }
  if (any(grepl("quasi", family, ignore.case = TRUE))) {
    stop("Quasi families have no predictive distribution for these diagnostics.", call. = FALSE)
  }
  weights <- if (backend == "sdmTMB") model$tmb_data$weights_i else {
    if (backend == "tinyVAST") model$tmb_inputs$tmb_data$weights_i else {
      if (backend %in% c("glm", "gam")) model$prior.weights else {
        if (backend == "glmmTMB") stats::model.weights(frame) else NULL
      }
    }
  }
  binomial <- length(family) == 1L && family %in% c("binomial", "betabinomial")
  trial_weights <- binomial && backend %in% c("glm", "gam", "glmmTMB")
  if (!is.null(weights) && any(!is.finite(weights) | weights != 1) && !trial_weights) {
    stop("Case-weighted fits need an explicit predictive-weight interpretation. ",
      "Only binomial trial weights in GLMs, GAMs, and glmmTMB are supported.", call. = FALSE)
  }
  trials <- NULL
  if (binomial && backend %in% c("glm", "gam", "glmmTMB")) {
    if (is.matrix(observed) && ncol(observed) == 2L) {
      trials <- rowSums(observed)
      observed <- observed[, 1L]
      if (backend == "glmmTMB" && !is.null(weights) && any(weights != 1)) {
        stop("Binomial case weights are not trial counts for a two-column response.", call. = FALSE)
      }
      if (backend %in% c("glm", "gam") &&
          !isTRUE(all.equal(as.numeric(model$prior.weights), as.numeric(trials)))) {
        stop("Additional binomial case weights are not supported.", call. = FALSE)
      }
    } else if (is.factor(observed)) {
      if (nlevels(observed) != 2L) stop("Binomial factors must have two levels.", call. = FALSE)
      trials <- rep(1, length(observed))
      observed <- as.integer(observed) - 1L
      if (!is.null(weights) && any(weights != 1)) stop("Weighted factor responses are not supported.", call. = FALSE)
    } else {
      trials <- weights %||% rep(1, length(observed))
      if (any(trials != 1)) {
        if (is.null(trial_counts)) {
          stop("Weighted one-column binomial responses need explicit `trial_counts` (a column in `data`), or use cbind(successes, failures). Arbitrary case weights are not assumed to be trials.", call. = FALSE)
        }
        known <- .resid_known_trials(trial_counts, data)
        if (!isTRUE(all.equal(as.numeric(known), as.numeric(trials)))) {
          stop("`trial_counts` must match the fitted binomial trial weights.", call. = FALSE)
        }
      }
      observed <- observed * trials
    }
    if (any(!is.finite(trials) | trials <= 0 | trials != round(trials)) ||
        any(!is.finite(observed) | abs(observed - round(observed)) > 1e-7) ||
        any(observed < 0 | observed > trials)) {
      stop("Binomial responses require positive integer trials and integer successes.", call. = FALSE)
    }
    observed <- round(observed)
    response <- "Successes"
  }
  if (binomial && backend == "sdmTMB") {
    trials <- model$tmb_data$size
    if (!is.null(trials) && any(trials != 1)) {
      stop("Multi-trial sdmTMB residual simulation is not yet validated; ",
        "use its native component-aware residual workflow.", call. = FALSE)
    }
  }
  if (!is.null(trial_counts)) {
    if (!binomial || !backend %in% c("glm", "gam", "glmmTMB")) {
      stop("`trial_counts` is an explicit interpretation of GLM/GAM/glmmTMB binomial trial weights only; other backends must supply native trial metadata.", call. = FALSE)
    }
    if (!isTRUE(all.equal(as.numeric(.resid_known_trials(trial_counts, data)), as.numeric(trials)))) {
      stop("`trial_counts` must match the fitted binomial trial counts.", call. = FALSE)
    }
  }
  if (!is.numeric(observed) || !is.null(dim(observed)) ||
      length(observed) != nrow(data) || any(!is.finite(observed))) {
    stop("One finite numeric observed response per fitted row is required.", call. = FALSE)
  }
  n <- length(observed)
  if (backend %in% c("glm", "gam")) {
    # Removing na.action prevents simulate.lm()/fitted() from reinserting
    # excluded rows. The retained fitted frame defines the simulation order.
    model$na.action <- NULL
    mu <- as.numeric(model$fitted.values)
    if (length(mu) != n || any(!is.finite(mu))) stop("Fitted means do not match observed rows.", call. = FALSE)
    if (binomial) {
      simulate <- function(ids) matrix(stats::rbinom(n * length(ids),
        size = trials, prob = mu), nrow = n)
    } else if (backend == "gam" && is.function(model$family$rd)) {
      simulate <- function(ids) vapply(ids, function(i) {
        model$family$rd(mu, wt = rep(1, n), scale = model$sig2)
      }, numeric(n))
    } else {
      simulate <- function(ids) as.matrix(stats::simulate(model, nsim = length(ids)))
    }
    scheme <- "Observation simulations at fitted parameters (including fitted smooths)"
  } else if (backend == "glmmTMB") {
    codes <- unlist(lapply(model$obj$env$data[c("terms", "termszi", "termsdisp")],
      function(terms) lapply(terms, `[[`, "simCode")), use.names = FALSE)
    if (conditioning == "new_effects" && any(codes != 2)) {
      # Other packages can change native simulation controls in place. Honour
      # our explicit target without changing or resetting the supplied fit.
      model$obj <- .resid_tmb_object(model, backend, conditioning)$obj
    }
    simulate <- function(ids) {
      value <- stats::simulate(model, nsim = length(ids))
      if (binomial) {
        vapply(value, function(v) {
          if (!is.matrix(v) || ncol(v) != 2L) {
            stop("Expected native binomial success/failure simulations.", call. = FALSE)
          }
          as.numeric(v[, 1L])
        }, numeric(n))
      } else as.matrix(value)
    }
    scheme <- "Native simulations at fitted parameters; random effects resimulated"
    if (.glmmTMB_has_component(model, "zi")) structure <- "combined zero-inflated response"
  } else if (backend == "brms") {
    available <- brms::ndraws(model)
    if (nsim > available) stop("`nsim` exceeds available posterior draws; reduce it.", call. = FALSE)
    draws <- sample.int(available, nsim, replace = FALSE)
    simulate <- function(ids) t(brms::posterior_predict(model,
      draw_ids = draws[ids], sort = FALSE, cores = 1L))
    scheme <- "Posterior predictive simulations including existing group effects"
    if (any(grepl("hurdle|zero_inflated", family))) structure <- "combined hurdle/zero-inflated response"
  } else if (backend == "sdmTMB") {
    simulate <- function(ids) {
      stats::simulate(model, nsim = length(ids), type = "mle-eb",
        seed = sample.int(.Machine$integer.max, 1L), silent = TRUE)
    }
    scheme <- "mle-eb observation simulations conditional on fitted latent effects"
    if (isTRUE(model$family$delta)) structure <- "combined delta response"
  } else {
    simulate <- function(ids) {
      value <- stats::simulate(model, nsim = length(ids), type = "mle-eb",
        seed = sample.int(.Machine$integer.max, 1L))
      if (is.null(dim(value)) && length(ids) == 1L) value <- matrix(value, ncol = 1L)
      value
    }
    scheme <- "mle-eb observation simulations conditional on fitted latent effects"
    if (isTRUE(model$internal$family[[1L]]$delta)) structure <- "combined delta response"
  }
  adapter <- list(backend = backend, observed = as.numeric(observed), data = data,
    response = response, simulate = simulate, scheme = scheme, structure = structure,
    family = family, trials = trials, response_variables = all.vars(f[[2L]]))
  adapter$conditioning <- conditioning
  adapter$conditioning_requested <- requested_conditioning
  if ((backend == "glmmTMB" && conditioning == "fitted") ||
      (backend %in% c("sdmTMB", "tinyVAST") && conditioning != "fitted")) {
    prepared <- .resid_conditioned_simulator(model, backend, conditioning, nsim)
    adapter$simulate <- prepared$simulate
    if (backend == "sdmTMB") adapter$simulate_component <- prepared$simulate
    if (conditioning == "conditional_draw") {
      adapter$conditional_probability <- prepared$probability
    }
    adapter$scheme <- switch(conditioning,
      fitted = "Observation simulations conditional on fitted random effects and parameters",
      conditional_draw = "Fitted parameters; one shared joint conditional latent draw (Gaussian approximation)",
      new_effects = "Fitted parameters; latent processes resimulated, fitted smooths held fixed")
  }
  .resid_response_adapter(adapter, model, component, nsim,
    draw_ids = if (backend == "brms") draws else NULL)
}
