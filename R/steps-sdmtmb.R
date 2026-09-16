# Fixed annual contrasts only. In standard delta models the native effort
# offset acts on the positive log-link component and cancels from its ratio.
# This does not enable a full offset-dependent CDI decomposition or an index
# integrated over space. Latent processes affect coefficients through fitting.
.step_sdmtmb_diag <- function(model, focus, component, probs, year_term = NULL,
                               data = model$data, uncertainty = "auto",
                               weights = NULL, reference_data = NULL,
                               reference_weights = NULL, ndraws = 1000L,
                               seed = NULL) {
  .require_model_backend(model)
  if (!isTRUE(model$family$delta) || !identical(model$family$type, "standard") ||
      !identical(model$family$link, c("logit", "log")) ||
      !model$family$family[2L] %in% c("Gamma", "gamma", "lognormal")) {
    stop("Joint sdmTMB step indices currently require standard delta-Gamma or delta-lognormal (logit/log) models.", call. = FALSE)
  }
  if (!is.null(model$nonlocal_parsed) || !is.null(model$time_varying) ||
      !is.null(model$threshold_parameter)) {
    stop("Nonlocal, time-varying coefficient, or threshold terms need a separate step-index definition.", call. = FALSE)
  }
  if (is.null(component)) stop("Multiple index components are available; supply `component =` explicitly: occurrence, positive, or unconditional_mean.", call. = FALSE)
  component <- match.arg(component, c("occurrence", "positive", "unconditional_mean"))
  uncertainty <- match.arg(uncertainty, c("auto", "none", "analytic", "simulation"))
  if (!is.null(seed)) {
    old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      get(".Random.seed", envir = .GlobalEnv)
    } else NULL
    on.exit(if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    } else assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
  }
  data <- as.data.frame(data)
  annual <- .step_year_terms(model$formula, focus, year_term)
  validate_year <- function(d) {
    if (!all(c(focus, annual) %in% names(d)) || anyNA(d[c(focus, annual)])) {
      stop("All annual predictors must be present and non-missing in the analysis/reference data.", call. = FALSE)
    }
    for (name in annual) {
      if (any(vapply(split(d[[name]], d[[focus]], drop = TRUE),
          function(x) length(unique(x)) != 1L, logical(1)))) {
        stop("Each `year_term` must be constant within each requested year.", call. = FALSE)
      }
    }
  }
  validate_year(data)
  if (!is.null(reference_data)) validate_year(reference_data)
  specs <- .sdmTMB_family_specs(model)
  reference_matrices <- if (is.null(reference_data)) NULL else {
    stats::predict(model, newdata = reference_data, re_form = NA,
      offset = rep(0, nrow(reference_data)), return_tmb_data = TRUE)$proj_X_ij
  }
  parts <- lapply(1:2, function(k) {
    X <- model$tmb_data$X_ij[[k]]
    labels <- attr(model$terms[[k]], "term.labels")
    term <- .step_year_term(model$formula[[k]], annual[k])
    columns <- which(attr(X, "assign") == match(term, labels))
    beta <- model$parlist[[c("b_j", "b_j2")[k]]]
    if (!length(columns) || nrow(X) != nrow(data) || ncol(X) != length(beta) ||
        any(!is.finite(X)) || any(!is.finite(beta))) {
      stop("Cannot align the native fixed annual design and coefficient vector.", call. = FALSE)
    }
    list(X = X, beta = beta, columns = columns,
      reference_X = if (is.null(reference_matrices)) NULL else {
        .align_reference_matrix(reference_matrices[[k]], X)
      }, index = which(names(model$sd_report$par.fixed) == c("b_j", "b_j2")[k]))
  })
  if (any(vapply(parts, function(p) length(p$index) != length(p$beta), logical(1)))) {
    stop("Cannot align the native joint fixed-effect covariance.", call. = FALSE)
  }
  response <- all.vars(model$formula[[1L]])[1L]
  if (component != "unconditional_mean") {
    k <- match(component, c("occurrence", "positive"))
    p <- parts[[k]]
    result <- .influ_linear_engine("sdmTMB", model, data, NULL, focus,
      specs[[component]], p$X, p$beta,
      model$sd_report$cov.fixed[p$index, p$index, drop = FALSE],
      stats::setNames(list(p$columns), focus), uncertainty = uncertainty,
      probs = probs, weights = weights, reference_data = reference_data,
      reference_X = p$reference_X, reference_weights = reference_weights,
      component = component, ndraws = ndraws, seed = seed)
    result$metadata$response <- response
  } else {
    beta <- unlist(lapply(parts, `[[`, "beta"))
    index <- unlist(lapply(parts, `[[`, "index"))
    draws <- if (uncertainty == "none") matrix(beta, nrow = 1L) else {
      .draw_mvn(ndraws, beta, model$sd_report$cov.fixed[index, index, drop = FALSE], seed)
    }
    w <- .resolve_influ_weights(data, weights)
    rw <- if (is.null(reference_data)) w else .resolve_influ_weights(reference_data, reference_weights)
    projection <- lapply(1:2, function(k) {
      p <- parts[[k]]
      d <- draws[, seq_along(p$beta) + if (k == 2L) length(parts[[1L]]$beta) else 0L, drop = FALSE]
      ref <- p$reference_X %||% p$X
      B <- .term_contrast(p$X[, p$columns, drop = FALSE], .focus_info(data, focus), w,
        ref[, p$columns, drop = FALSE], rw)
      list(family_spec = specs[[c("occurrence", "positive")[k]]],
        eta_reference = as.numeric(d %*% .weighted_col_mean(ref, rw)),
        term_deltas = stats::setNames(list(d[, p$columns, drop = FALSE] %*% t(B)), focus),
        reference = if (is.null(reference_data)) "observed" else "prediction_grid",
        method = if (uncertainty == "none") "none" else "joint coefficient simulation")
    })
    result <- .two_part_combined_diag("sdmTMB", model, data, response, focus,
      specs$overall, projection[[2L]], projection[[1L]], probability_is_zero = FALSE,
      weights = weights, probs = probs)
  }
  result$metadata$year_term <- stats::setNames(annual, c("occurrence", "positive"))
  result$metadata$estimand <- "fixed annual contrast; positive log-link effort offset cancels"
  result
}
