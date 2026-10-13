# Native, read-only numerical checks. These describe availability as well as
# findings; a missing diagnostic is never interpreted as successful convergence.
.check_fit_backend <- function(model) {
  classes <- c(gam = "GAM", glm = "GLM", merMod = "lme4", glmmTMB = "glmmTMB",
    brmsfit = "brms", sdmTMB = "sdmTMB", tinyVAST = "tinyVAST")
  found <- names(classes)[vapply(names(classes), function(x) inherits(model, x), logical(1))]
  if (!length(found)) stop("Model checking requires a supported GLM, GAM, lme4, glmmTMB, brms, sdmTMB, or tinyVAST fit.", call. = FALSE)
  unname(classes[found[1L]])
}

.check_fit_try <- function(fun) {
  notes <- character()
  value <- tryCatch(withCallingHandlers(fun(), warning = function(w) {
    notes <<- c(notes, conditionMessage(w)); invokeRestart("muffleWarning")
  }, message = function(m) {
    notes <<- c(notes, conditionMessage(m)); invokeRestart("muffleMessage")
  }), error = function(e) e)
  if (inherits(value, "error")) {
    notes <- c(notes, conditionMessage(value)); value <- NULL
  }
  list(value = value, notes = unique(notes))
}

.check_fit_scalar <- function(x) {
  if (is.numeric(x) && length(x) == 1L && is.finite(x)) as.numeric(x) else NA_real_
}

.check_fit_backend_available <- function(package) requireNamespace(package, quietly = TRUE)

.check_fit_brms_summary <- function(model) summary(model)

.check_fit_brms_nuts <- function(model) brms::nuts_params(model)

.check_fit_live_objective <- function(object) {
  pointer <- .check_fit_try(function() object$env$ADFun$ptr)$value
  if (!identical(typeof(pointer), "externalptr")) return(FALSE)
  # Reading a pointer is harmless; trying the objective to check it is not.
  # Reloaded TMB pointers are NULL and native residual methods can initialise
  # them by evaluating the original fitted objective. Do not do that here.
  attributes(pointer) <- NULL
  !identical(pointer, methods::new("externalptr"))
}

.check_fit_cached_r2 <- function(x) {
  out <- c(estimate = NA_real_, sd = NA_real_)
  # add_criterion(brmsfit, "bayes_R2") stores the native summary matrix.
  # Never collapse multiple responses or calculate new posterior predictions.
  if ((is.matrix(x) || is.data.frame(x)) && nrow(x) == 1L &&
      all(c("Estimate", "Est.Error") %in% colnames(x))) {
    out[] <- vapply(c("Estimate", "Est.Error"), function(nm)
      .check_fit_scalar(x[1L, nm]), numeric(1))
  }
  out
}

.check_fit_statistics <- function(model, backend, available) {
  out <- data.frame(backend = backend, nobs = NA_real_, df = NA_real_,
    df_residual = NA_real_, logLik = NA_real_, AIC = NA_real_, BIC = NA_real_,
    deviance = NA_real_, deviance_explained = NA_real_, adjusted_R2 = NA_real_,
    bayes_R2 = NA_real_, bayes_R2_sd = NA_real_, method = "", notes = "",
    stringsAsFactors = FALSE)
  notes <- character()
  fetch <- function(label, fun) {
    value <- .check_fit_try(fun)
    if (length(value$notes)) notes <<- c(notes, paste0(label, ": ", value$notes))
    value$value
  }
  if (available) out$nobs <- .check_fit_scalar(fetch("nobs", function() stats::nobs(model)))
  if (!is.finite(out$nobs)) {
    frame <- if (backend %in% c("GLM", "GAM")) model$model else if (backend == "glmmTMB") model$frame else if (backend %in% c("brms", "sdmTMB", "tinyVAST")) model$data else NULL
    if (is.data.frame(frame)) out$nobs <- nrow(frame)
  }
  if (backend == "tinyVAST") {
    y <- model$tmb_inputs$tmb_data$y_i
    if (is.numeric(y) && length(y)) out$nobs <- sum(!is.na(y))
  }
  if (backend == "brms") {
    r2 <- .check_fit_cached_r2(model$criteria$bayes_R2)
    out$bayes_R2 <- r2["estimate"]; out$bayes_R2_sd <- r2["sd"]
    out$method <- "Saved native Bayesian R-squared summary only; not frequentist likelihood criteria"
    if (!is.finite(out$bayes_R2)) notes <- c(notes,
      "Bayesian R-squared not saved as a single-response native summary; no posterior prediction or LOO calculation was requested.")
  } else {
    if (available) {
      ll <- fetch("logLik", function() stats::logLik(model))
      out$logLik <- .check_fit_scalar(ll); out$df <- .check_fit_scalar(attr(ll, "df"))
      out$AIC <- .check_fit_scalar(fetch("AIC", function() stats::AIC(model)))
      if (backend == "tinyVAST") {
        if (all(is.finite(c(out$logLik, out$df, out$nobs))) && out$nobs > 0)
          out$BIC <- -2 * out$logLik + log(out$nobs) * out$df
      } else out$BIC <- .check_fit_scalar(fetch("BIC", function() stats::BIC(model)))
      out$df_residual <- .check_fit_scalar(fetch("df.residual", function() stats::df.residual(model)))
      if (backend == "tinyVAST") out$deviance <- .check_fit_scalar(model$rep$deviance) else if (backend == "lme4" &&
          isTRUE(fetch("isREML", function() lme4::isREML(model)))) {
        notes <- c(notes, "The native REML criterion is not a response deviance; no deviance value is substituted.")
      } else if (backend %in% c("sdmTMB", "glmmTMB") &&
          !.check_fit_live_objective(if (backend == "sdmTMB") model$tmb_obj else model$obj)) {
        notes <- c(notes,
          "Native deviance not assessed: the saved/reloaded objective pointer is unavailable; native state was not reinitialised.")
      } else {
        zi <- backend == "glmmTMB" && isTRUE(.glmmTMB_has_component(model, "zi"))
        if (!zi) out$deviance <- .check_fit_scalar(fetch("deviance", function() stats::deviance(model))) else
          notes <- c(notes, "Native component deviance is not a combined zero-inflated response deviance.")
      }
    }
    out$method <- "Native descriptive likelihood statistics; backend-specific deviance, not cross-backend adequacy or ranking"
    restricted <- if (backend == "lme4" && available) .check_fit_try(function() lme4::isREML(model))$value else
      switch(backend, glmmTMB = model$modelInfo$REML, sdmTMB = model$reml,
        tinyVAST = model$internal$control$reml, FALSE)
    if (isTRUE(restricted)) out$method <- paste(out$method,
      "Restricted/REML likelihood values are descriptive, not comparisons across fixed-effect structures.")
    if (backend %in% c("GLM", "GAM")) {
      numerator <- .check_fit_scalar(model$deviance)
      denominator <- .check_fit_scalar(model$null.deviance)
      if (is.finite(numerator) && is.finite(denominator) && denominator > 0)
        out$deviance_explained <- 1 - numerator / denominator else
          notes <- c(notes, "Deviance explained requires finite saved fitted and positive null deviances.")
      if (backend == "GAM" && available) {
        native <- fetch("summary.gam", function() summary(model))
        out$adjusted_R2 <- .check_fit_scalar(native$r.sq)
        out$method <- paste(out$method, "GAM R-squared is native adjusted response-scale R-squared.")
      }
    } else if (backend == "tinyVAST") {
      out$deviance_explained <- .check_fit_scalar(model$deviance_explained)
      if (!is.finite(out$deviance_explained)) notes <- c(notes,
        "Deviance explained was not saved; the native function fits a null model and is not called here.")
    }
    if (backend %in% c("lme4", "glmmTMB", "sdmTMB", "tinyVAST")) notes <- c(notes,
      "No generic marginal/conditional R-squared or reconstructed null-model deviance is substituted.")
  }
  if (!available) notes <- c(notes, "Optional native backend unavailable; native methods were not called.")
  out$notes <- paste(unique(notes), collapse = " ")
  out
}

.check_fit <- function(model) {
  backend <- .check_fit_backend(model)
  seed <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv) else NULL
  on.exit({
    if (is.null(seed)) {
      if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    } else assign(".Random.seed", seed, envir = .GlobalEnv)
  }, add = TRUE)
  package <- switch(backend, GLM = "stats", GAM = "mgcv", backend)
  available <- .check_fit_backend_available(package)
  rows <- list()
  row <- function(check, status, statistic = NA_real_, method = "Stored native fit diagnostic",
      message = "", action = "", term = "") {
    rows[[length(rows) + 1L]] <<- .cpue_check_row(scope = "numerical", term = term,
      check = check, status = status, statistic = statistic, method = method,
      message = message, action = action)
  }
  flag <- function(check, value, bad = "issue", message, method = "Stored native fit diagnostic", action = "Inspect the native fit diagnostics before interpreting results.") {
    status <- if (length(value) != 1L || is.na(value) || !is.logical(value)) "not_assessed" else if (value) "no_flag" else bad
    row(check, status, message = if (status == "not_assessed") paste(message, "Diagnostic unavailable; not a successful check.") else message,
      method = method, action = if (status == "no_flag") "No numerical flag in this check; this does not establish model adequacy." else action)
  }
  gradient <- function(value, threshold, method, applicable = TRUE) {
    if (!applicable) return(row("gradient", "not_applicable", method = method,
      message = "This backend does not provide a supported stored gradient diagnostic."))
    if (!is.numeric(value) || !length(value) || any(!is.finite(value))) return(row("gradient", "not_assessed", method = method,
      message = "Complete finite stored gradients unavailable; the objective was not recomputed.",
      action = "Review the native convergence diagnostics."))
    largest <- max(abs(value))
    row("gradient", if (largest > threshold) "review" else "no_flag", statistic = largest,
      method = method, message = paste0("Maximum absolute stored gradient; native-scale screening threshold ", threshold, "."),
      action = "Inspect parameter scaling and native convergence messages; no optimisation is rerun.")
  }
  uncertainty <- function(V, method = "Stored covariance diagonal") {
    if (is.null(V) || !length(V) || is.null(dim(V)) || nrow(V) != ncol(V)) return(row("finite_uncertainty", "not_assessed", method = method,
      message = "A stored/native square covariance matrix was unavailable.", action = "Check native uncertainty availability."))
    diagonal <- if (inherits(V, "Matrix") && requireNamespace("Matrix", quietly = TRUE)) Matrix::diag(V) else diag(V)
    good <- length(diagonal) > 0L && all(is.finite(diagonal) & diagonal >= 0)
    row("finite_uncertainty", if (good) "no_flag" else "issue",
      statistic = sum(!is.finite(diagonal) | diagonal < 0), method = method,
      message = "Count of non-finite or negative estimated-parameter variances; zero variance alone is not failure.",
      action = if (good) "Inspect scientific identifiability separately." else "Do not interpret unavailable or invalid parameter uncertainty.")
  }
  coefficients <- function(value) {
    if (!is.numeric(value) || !length(value)) return(row("finite_coefficients", "not_assessed",
      message = "Estimated coefficients unavailable."))
    aliased <- sum(is.na(value)); invalid <- sum(!is.finite(value) & !is.na(value))
    row("finite_coefficients", if (invalid) "issue" else if (aliased) "review" else "no_flag",
      statistic = aliased + invalid,
      message = paste0(aliased, " aliased/missing and ", invalid, " infinite coefficients."),
      action = "Review rank deficiency and parameter identifiability; missing coefficients are not silently treated as zero.")
  }
  if (!available) row("backend_availability", "not_assessed", message = paste("Optional package", package, "is unavailable."),
    action = "Install the native backend to enable its methods; no substitute backend is used.")
  if (backend %in% c("sdmTMB", "glmmTMB") &&
      !.check_fit_live_objective(if (backend == "sdmTMB") model$tmb_obj else model$obj)) {
    row("native_objective_availability", "not_assessed",
      message = "The saved/reloaded native objective pointer is unavailable. Stored convergence and likelihood summaries remain readable, but native deviance is not calculated.",
      action = "If needed, reload/reinitialise the native fit explicitly outside this checker; its state is not changed here.")
  }
  if (backend %in% c("GLM", "GAM")) {
    flag("convergence", model$converged, message = "Native iterative fit convergence flag.")
    if (!is.null(model$boundary)) flag("boundary", !model$boundary, bad = "review", message = "Native GLM boundary flag.")
    coefficients(model$coefficients)
    if (available) uncertainty(.check_fit_try(function() stats::vcov(model, complete = FALSE))$value,
      "Native vcov, excluding aliased coefficients") else uncertainty(NULL)
    gradient(NULL, NA_real_, "Native GLM/GAM optimiser status used instead of a generic gradient rule", FALSE)
    if (inherits(model, "negbin")) {
      theta <- .check_fit_scalar(model$theta)
      flag("dispersion", if (is.finite(theta)) theta > 0 else NULL, message = "Finite positive NB2 size estimate.")
      row("dispersion_convergence", if (is.null(model$th.warn)) "no_flag" else "issue",
        message = paste(c("Native negative-binomial dispersion warnings:", model$th.warn %||% "none"), collapse = " "),
        action = "Review dispersion estimation before interpreting NB2 uncertainty.")
    }
    if (backend == "GAM") {
      outer <- model$outer.info$conv
      flag("outer_convergence", if (is.character(outer) && length(outer) == 1L) identical(outer, "full convergence") else NULL,
        message = paste("Native smoothing-parameter optimisation:", outer %||% "unavailable"))
      row("basis_dimension", "not_assessed", method = "mgcv basis-dimension checking not requested",
        message = "No stochastic k-check or concurvity calculation is run by the numerical overview.",
        action = "Use an explicitly requested native GAM basis-dimension/concurvity check when relevant.")
    }
  } else if (backend == "lme4") {
    opt <- model@optinfo
    code <- opt$conv$opt
    messages <- opt$conv$lme4$messages %||% character()
    nonboundary <- messages[!grepl("boundary.*singular", messages, ignore.case = TRUE)]
    flag("convergence", if (is.numeric(code) && length(code) && all(is.finite(code))) all(code == 0) && !length(nonboundary) else NULL,
      message = paste(c("Native optimiser/convergence messages:", nonboundary %||% "none"), collapse = " "))
    if (available) {
      singular <- .check_fit_try(function() lme4::isSingular(model))$value
      flag("singularity", if (is.logical(singular)) !singular else NULL, bad = "review",
        message = "Native lme4 singular covariance flag; a boundary estimate is not automatically a failed optimisation.")
      coef <- .check_fit_try(function() lme4::fixef(model, add.dropped = TRUE))$value
      coefficients(coef)
      uncertainty(.check_fit_try(function() stats::vcov(model))$value, "Native fixed-effect covariance diagonal")
    } else { coefficients(NULL); uncertainty(NULL) }
    grad <- opt$derivs$gradient
    row("gradient", if (is.numeric(grad) && length(grad) && all(is.finite(grad))) "no_flag" else "not_assessed",
      statistic = if (is.numeric(grad) && length(grad) && all(is.finite(grad))) max(abs(grad)) else NA_real_,
      method = "Raw native lme4 gradient, with native scaled-gradient convergence messages",
      message = "Raw gradients are descriptive only; no universal cross-backend threshold is applied.",
      action = "Inspect the native scaled-gradient convergence messages above.")
  } else if (backend %in% c("glmmTMB", "sdmTMB", "tinyVAST")) {
    optimisation <- switch(backend, glmmTMB = model$fit, sdmTMB = model$model, tinyVAST = model$opt)
    report <- switch(backend, glmmTMB = model$sdr, sdmTMB = model$sd_report, tinyVAST = model$sdrep)
    code <- optimisation$convergence
    flag("convergence", if (is.numeric(code) && length(code) == 1L && is.finite(code)) code == 0 else NULL,
      message = paste("Native optimiser status:", optimisation$message %||% "message unavailable"))
    flag("positive_definite_hessian", report$pdHess, message = "Native positive-definite Hessian flag.")
    coefficients(optimisation$par)
    uncertainty(report$cov.fixed, "Stored native fixed-parameter covariance diagonal")
    if (backend == "sdmTMB") {
      gradient(model$gradients, 0.001, "sdmTMB native stored fixed-parameter gradients")
      eigen <- model$bad_eig
    } else if (backend == "tinyVAST") {
      gradient(model$internal$convergence_diagnostics$final_grads, 0.001, "tinyVAST native stored fixed-parameter gradients")
      eigen <- model$internal$convergence_diagnostics$bad_eig %||% model$convergence_diagnostics$bad_eig
    } else {
      gradient(NULL, NA_real_, "glmmTMB does not retain a supported comparable gradient summary here")
      eigen <- NULL
    }
    if (backend != "glmmTMB") flag("eigenvalues", if (is.logical(eigen)) !eigen else NULL,
      message = "Native extreme-eigenvalue screening flag; unavailable is not false.")
    row("variance_boundary", "not_assessed", method = "Scale-dependent variance-boundary review",
      message = "No universal variance/range threshold is applied, and the native objective is not recomputed.",
      action = "Inspect native variance/range estimates on their scientific units and spatial domain.")
  } else if (backend == "brms") {
    complete <- !is.null(model$fit)
    native <- if (available && complete) .check_fit_try(function() .check_fit_brms_summary(model)) else list(value = NULL, notes = character())
    tables <- list()
    collect <- function(x) {
      if ((is.matrix(x) || is.data.frame(x)) && all(c("Rhat", "Bulk_ESS", "Tail_ESS") %in% colnames(x))) {
        tables[[length(tables) + 1L]] <<- as.data.frame(x[, c("Rhat", "Bulk_ESS", "Tail_ESS"), drop = FALSE])
      } else if (is.list(x)) for (child in x) collect(child)
    }
    collect(native$value)
    diagnostics <- if (length(tables)) do.call(rbind, tables) else NULL
    for (metric in c("Rhat", "Bulk_ESS", "Tail_ESS")) {
      values <- if (is.null(diagnostics)) NULL else diagnostics[[metric]]
      finite <- if (is.numeric(values)) values[is.finite(values)] else numeric()
      incomplete <- !length(finite) || length(finite) != length(values)
      threshold <- if (metric == "Rhat") 1.01 else 400
      statistic <- if (!length(finite)) NA_real_ else if (metric == "Rhat") max(finite) else min(finite)
      problem <- is.finite(statistic) && if (metric == "Rhat") statistic > threshold else statistic < threshold
      row(tolower(metric), if (problem) "review" else if (incomplete) "not_assessed" else "no_flag", statistic,
        method = "Native brms rank-normalised convergence summary",
        message = paste0(if (metric == "Rhat") "Maximum " else "Minimum ", metric,
          "; screening threshold ", threshold, ". ", if (incomplete) "Some/all diagnostics unavailable; not a complete successful check. " else "",
          paste(native$notes, collapse = " ")),
        action = "Inspect parameter-specific diagnostics; no new MCMC or posterior predictions are calculated.")
    }
    nuts <- if (available && complete) .check_fit_try(function() .check_fit_brms_nuts(model))$value else NULL
    divergent <- if (is.data.frame(nuts) && all(c("Parameter", "Value") %in% names(nuts))) nuts$Value[nuts$Parameter == "divergent__"] else NULL
    if (is.numeric(divergent) && length(divergent) && all(is.finite(divergent))) {
      count <- sum(divergent > 0)
      row("divergences", if (count) "issue" else "no_flag", count, method = "Native post-warmup NUTS transitions",
        message = "Count of divergent posterior transitions.", action = "Review posterior geometry and native sampler diagnostics.")
    } else row("divergences", "not_assessed", message = "Native post-warmup divergence diagnostics unavailable; compact draw fixtures cannot establish MCMC convergence.")
    row("treedepth_energy", "not_assessed", message = "No universal treedepth/energy rule is inferred from incomplete sampler controls.",
      action = "Inspect native sampler controls, maximum-treedepth events, and energy diagnostics separately.")
  }
  list(backend = backend, model_statistics = .check_fit_statistics(model, backend, available),
    checks = do.call(rbind, rows))
}
