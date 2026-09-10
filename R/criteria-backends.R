.criterion_columns <- function(criterion) {
  switch(criterion, aic = "AIC", bic = "BIC", caic = c("cAIC", "cAIC_df", "EDF_random"),
    loglik = "logLik", deviance = "deviance",
    loo = c("elpd_loo", "se_elpd_loo", "p_loo", "looic", "se_looic", "pareto_k_max", "pareto_k_bad"),
    loo_r2 = c("loo_R2", "se_loo_R2"), bayes_r2 = c("bayes_R2", "se_bayes_R2"),
    log_lik = c("log_lik", "se_log_lik"))
}

.criterion_empty <- function(result, requested) {
  for (nm in unlist(lapply(requested, .criterion_columns))) result$row[[nm]] <- NA_real_
  result
}

.criterion_frequentist <- function(result, model, requested, context,
                                    caic = NULL, deviance = NULL) {
  requested <- .criterion_request(requested)
  result <- .criterion_empty(result, requested)
  ll <- .criterion_eval(context, "logLik", stats::logLik(model))
  result$row$df <- .criterion_scalar(attr(ll, "df"))
  n <- if (inherits(model, "tinyVAST")) result$row$nobs else
    .criterion_eval(context, "nobs", stats::nobs(model))
  if (is.finite(.criterion_scalar(n))) result$row$nobs <- .criterion_scalar(n)
  # tinyVAST has no native nobs method: its fitted response frame is used.
  if ("loglik" %in% requested) result$row$logLik <- .criterion_scalar(ll)
  for (crit in intersect(requested, c("aic", "bic"))) {
    value <- if (crit == "aic") .criterion_eval(context, "AIC", stats::AIC(model)) else {
      if (is.finite(result$row$nobs) && result$row$nobs > 0)
        -2 * .criterion_scalar(ll) + log(result$row$nobs) * result$row$df else NA_real_
    }
    result$row[[.criterion_columns(crit)]] <- .criterion_scalar(value)
    if (!is.finite(.criterion_scalar(value))) .criterion_note(context, crit,
      "Native likelihood, degrees of freedom, or sample size unavailable; no substitute used.")
  }
  if ("deviance" %in% requested) {
    value <- if (is.null(deviance)) .criterion_eval(context, "deviance", stats::deviance(model)) else deviance()
    result$row$deviance <- .criterion_scalar(value)
    if (!is.finite(result$row$deviance)) .criterion_note(context, "deviance",
      "A finite native response deviance is unavailable; -2 logLik is not substituted.")
  }
  if ("caic" %in% requested) {
    if (is.null(caic)) {
      .criterion_note(context, "cAIC", "No supported native conditional-AIC method for this model.")
    } else if (!isTRUE(result$eligible) || identical(result$row$AIC_type, "restricted")) {
      .criterion_note(context, "cAIC", "Not calculated for a failed, penalised, or restricted-likelihood fit.")
    } else {
      value <- .criterion_eval(context, "cAIC", caic())
      if (!is.null(value)) {
        result$row$cAIC <- .criterion_scalar(value$value)
        result$row$cAIC_df <- .criterion_scalar(value$df)
        result$row$EDF_random <- .criterion_scalar(value$edf_random)
      }
      if (!is.finite(result$row$cAIC)) .criterion_note(context, "cAIC", "Native conditional-AIC calculation unavailable or non-finite.")
    }
  }
  for (crit in intersect(requested, c("loo", "loo_r2", "bayes_r2", "log_lik"))) {
    .criterion_note(context, crit, "Bayesian posterior criterion; not applicable to this frequentist fit.", "not applicable")
  }
  result
}

.criterion_ordinary_caic <- function(model) {
  list(value = stats::AIC(model), df = attr(stats::logLik(model), "df"), edf_random = 0)
}

#' @export
.criterion_model.glm <- function(model, requested, context, dots) {
  result <- .criterion_base(model, "GLM", stats::formula(model), stats::family(model), context,
    converged = model$converged)
  result$row$df_type <- "native likelihood parameter count"
  result$row$df_residual <- .criterion_scalar(stats::df.residual(model))
  result$row$likelihood <- "ordinary observation likelihood"
  result$row$AIC_type <- "ordinary"
  result$row$cAIC_method <- "ordinary AIC (no random effects)"
  result$row$deviance_type <- "native GLM residual deviance"
  if (grepl("^quasi", model$family$family)) {
    result$eligible <- FALSE
    .criterion_note(context, "likelihood", "Quasi families have no normalised likelihood; likelihood criteria are unavailable.")
  }
  .criterion_frequentist(result, model, requested, context,
    caic = function() .criterion_ordinary_caic(model))
}

#' @export
.criterion_model.gam <- function(model, requested, context, dots) {
  result <- .criterion_base(model, "GAM", stats::formula(model), stats::family(model), context,
    converged = model$converged)
  result$row$df_type <- "mgcv effective AIC penalty (including scale/family parameters)"
  result$row$df_residual <- .criterion_scalar(stats::df.residual(model))
  result$row$likelihood <- "conditional observation likelihood (penalised coefficient estimates)"
  result$row$AIC_type <- "conditional"
  result$row$cAIC_method <- "mgcv native conditional AIC"
  result$row$deviance_type <- "native GAM residual deviance"
  .criterion_note(context, "AIC", paste(
    "mgcv conditional AIC; smoothing-parameter uncertainty correction",
    if (!is.null(model$edf2)) "available." else "not available for this fit."), "method")
  .criterion_frequentist(result, model, requested, context,
    caic = function() list(value = stats::AIC(model), df = attr(stats::logLik(model), "df")))
}

.criterion_restricted <- function(result, restricted, context) {
  if (isTRUE(restricted)) {
    result$row$likelihood <- "restricted likelihood (REML)"
    result$row$AIC_type <- "restricted"
    result$eligible <- FALSE
    .criterion_note(context, "ranking", "REML fit: native values retained for inspection, but no automatic differences or ranking across fixed-effect structures.", "method")
  }
  result
}

.criterion_prior <- function(result, prior, context) {
  if (isTRUE(prior)) {
    result$eligible <- FALSE
    result$row$likelihood <- paste(result$row$likelihood, "with parameter prior/penalty")
    .criterion_note(context, "ranking", "Parameter priors/penalties detected; native objective criteria are not ordinary maximum-likelihood comparisons.", "method")
  }
  result
}

#' @export
.criterion_model.glmmTMB <- function(model, requested, context, dots) {
  fam <- stats::family(model)
  zi <- .glmmTMB_has_component(model, "zi")
  if (zi) fam$family <- paste0("zero_inflated_", fam$family)
  result <- .criterion_base(model, "glmmTMB", stats::formula(model), fam, context,
    converged = if (is.null(model$fit$convergence)) NA else model$fit$convergence == 0,
    pdHess = if (is.null(model$sdr)) NA else model$sdr$pdHess)
  random <- length(model$obj$env$random) > 0L
  result$row$df_type <- "native likelihood parameter count (random effects integrated out)"
  result$row$df_residual <- .criterion_scalar(stats::df.residual(model))
  result$row$likelihood <- if (random) "Laplace marginal likelihood" else "ordinary observation likelihood"
  result$row$AIC_type <- if (random) "marginal" else "ordinary"
  result$row$cAIC_method <- if (random) "not available for glmmTMB" else "ordinary AIC (no random effects)"
  result$row$deviance_type <- if (zi) "unavailable for combined zero-inflated response" else "native glmmTMB residual deviance"
  result <- .criterion_restricted(result, model$modelInfo$REML, context)
  result <- .criterion_prior(result, !is.null(model$modelInfo$priors) && nrow(model$modelInfo$priors) > 0L, context)
  .criterion_frequentist(result, model, requested, context,
    caic = if (!random) function() .criterion_ordinary_caic(model) else NULL,
    deviance = if (zi) function() {
      .criterion_note(context, "deviance", "Native component deviance is not a combined zero-inflated response deviance.")
      NA_real_
    } else NULL)
}

#' @export
.criterion_model.sdmTMB <- function(model, requested, context, dots) {
  fam <- model$family
  if (isTRUE(fam$delta)) fam$family <- paste0("delta_", fam$family[2])
  frame <- .criterion_frame(model, model$formula[[1]], model$data, model$tmb_data$weights_i)
  result <- .criterion_base(model, "sdmTMB", model$formula[[1]], fam, context, frame,
    converged = if (is.null(model$model$convergence)) NA else model$model$convergence == 0,
    pdHess = if (is.null(model$sd_report)) NA else model$sd_report$pdHess)
  random <- length(model$tmb_obj$env$random) > 0L
  result$row$df_type <- "native likelihood parameter count (random effects integrated out)"
  result$row$likelihood <- if (random) "Laplace marginal likelihood" else "ordinary observation likelihood"
  result$row$AIC_type <- if (random) "marginal" else "ordinary"
  result$row$cAIC_method <- if (random) "sdmTMB native Zheng-Cadigan-Thorson approximation" else "ordinary AIC (no random effects)"
  result$row$deviance_type <- if (isTRUE(model$family$delta)) "native sum of delta-component residual deviances" else "native sdmTMB residual deviance"
  result <- .criterion_restricted(result, model$reml, context)
  prior <- vapply(model$priors, function(x) {
    # pc_matern stores tail probabilities even when both prior bounds are NA.
    if (identical(attr(x, "dist"), "pc_matern")) x <- x[1:2]
    any(is.finite(x))
  }, logical(1))
  result <- .criterion_prior(result, any(prior), context)
  .criterion_frequentist(result, model, requested, context, caic = function() {
    if (!random) return(.criterion_ordinary_caic(model))
    if (length(model$control$profile) && !identical(model$control$profile, FALSE))
      stop("Conditional-AIC parameter counting for profiled sdmTMB fits is not yet validated; native marginal criteria remain available.")
    value <- sdmTMB::cAIC(model)
    edf <- sum(sdmTMB::cAIC(model, what = "EDF"))
    list(value = value, df = length(model$model$par) + edf, edf_random = edf)
  })
}

#' @export
.criterion_model.tinyVAST <- function(model, requested, context, dots) {
  families <- model$internal$family
  fam <- list(family = paste(vapply(families, function(x)
    if (isTRUE(x$delta)) paste0("delta_", x$family[2]) else paste(x$family, collapse = "+"), character(1)), collapse = "+"),
    link = paste(vapply(families, function(x) paste(x$link, collapse = "+"), character(1)), collapse = "+"))
  frame <- .criterion_frame(model, model$formula, model$data, model$tmb_inputs$tmb_data$weights_i)
  result <- .criterion_base(model, "tinyVAST", model$formula, fam, context, frame,
    converged = if (is.null(model$opt$convergence)) NA else model$opt$convergence == 0,
    pdHess = if (is.null(model$sdrep)) NA else model$sdrep$pdHess)
  random <- length(model$obj$env$random) > 0L
  result$row$df_type <- "native likelihood parameter count (including profiled fixed effects)"
  result$row$likelihood <- if (random) "Laplace marginal likelihood" else "ordinary observation likelihood"
  result$row$AIC_type <- if (random) "marginal" else "ordinary"
  result$row$cAIC_method <- if (random) "tinyVAST native Zheng-Cadigan-Thorson approximation" else "ordinary AIC (no random effects)"
  result$row$deviance_type <- "native stored tinyVAST response deviance"
  result <- .criterion_restricted(result, model$internal$control$reml, context)
  # Native nobs is absent, so use observed response rows; do not count mesh cells.
  result$row$nobs <- sum(!is.na(model$tmb_inputs$tmb_data$y_i))
  if (result$row$nobs == 0L && !is.null(frame)) result$row$nobs <- nrow(frame$frame)
  if (length(families) > 1L || length(model$internal$variables) > 1L) {
    result$frame <- NULL
    .criterion_note(context, "ranking", "Multivariate/multi-family joint likelihood: no automatic comparison group.", "method")
  }
  .criterion_frequentist(result, model, requested, context, caic = function() {
    if (!random) return(.criterion_ordinary_caic(model))
    if (length(model$internal$control$profile))
      stop("Conditional-AIC parameter counting for profiled tinyVAST fits is not yet validated; native marginal criteria remain available.")
    # The native method does not expose EDF; do not infer it from marginal df.
    .criterion_note(context, "cAIC_df", "Native tinyVAST cAIC does not expose its effective penalty degrees of freedom.")
    list(value = tinyVAST::cAIC(model))
  }, deviance = function() model$rep$deviance)
}

#' @export
.criterion_model.brmsfit <- function(model, requested, context, dots) {
  requested <- .criterion_request(requested, bayesian = TRUE)
  fam <- model$family
  formula <- model$formula$formula
  result <- .criterion_base(model, "brms", formula, fam, context,
    frame = .criterion_frame(model, formula, model$data))
  result <- .criterion_empty(result, requested)
  result$row$likelihood <- "Bayesian posterior predictive evaluation"
  result$row$df_type <- "p_loo is predictive effective complexity, not ML parameter count"
  result$row$nobs <- .criterion_scalar(.criterion_eval(context, "nobs", stats::nobs(model)))
  if (is.na(result$row$nobs) && !is.null(result$frame)) result$row$nobs <- nrow(result$frame$frame)
  # brms addition terms can subset, censor, truncate, weight, or change trials.
  # Until those identities are validated explicitly, show criteria without ranks.
  complex_response <- is.null(formula) || grepl("\\|", paste(deparse(formula[[2L]]), collapse = ""))
  if (complex_response || length(fam$family) != 1L) {
    result$frame <- NULL
    .criterion_note(context, "ranking", "Response additions/multivariate specification: automatic observation alignment is not verified.", "method")
  }
  for (crit in intersect(requested, c("aic", "bic", "caic", "loglik", "deviance"))) {
    .criterion_note(context, crit, "Not an ML fit; no frequentist likelihood criterion or generic residual deviance is substituted.", "not applicable")
  }
  if ("loo" %in% requested) {
    value <- if (!length(dots)) model$criteria$loo else NULL
    if (is.null(value)) value <- .criterion_eval(context, "loo", do.call(brms::loo, c(list(model), dots)))
    if (!is.null(value)) {
      for (nm in c("elpd_loo", "p_loo", "looic")) result$row[[nm]] <- value$estimates[nm, "Estimate"]
      result$row$se_elpd_loo <- value$estimates["elpd_loo", "SE"]
      result$row$se_looic <- value$estimates["looic", "SE"]
      result$loo <- value
      if (!is.null(value$pointwise) && is.finite(result$row$nobs) &&
          nrow(value$pointwise) != result$row$nobs) {
        result$eligible <- FALSE
        .criterion_note(context, "loo", "Stored pointwise likelihood rows do not match the fitted response; no automatic ranking.", "warning")
      }
      k <- value$diagnostics$pareto_k
      if (length(k)) {
        result$row$pareto_k_max <- max(k)
        bad <- .criterion_eval(context, "loo", loo::pareto_k_ids(value))
        result$row$pareto_k_bad <- if (is.null(bad)) NA_real_ else length(bad)
        if (any(!is.finite(k)) || is.null(bad) || length(bad)) {
          result$eligible <- FALSE
          .criterion_note(context, "loo", "Unreliable Pareto-k diagnostics: values shown, but no automatic LOO ranking.", "warning")
        }
      } else {
        result$eligible <- FALSE
        .criterion_note(context, "loo", "Pareto-k diagnostics unavailable; LOO values are not automatically ranked.")
      }
    }
  }
  for (crit in intersect(requested, c("loo_r2", "bayes_r2"))) {
    fn <- if (crit == "loo_r2") brms::loo_R2 else brms::bayes_R2
    value <- .criterion_eval(context, crit, do.call(fn, c(list(model), dots)))
    if (!is.null(value)) {
      value <- as.matrix(value)
      if (nrow(value) != 1L) {
        .criterion_note(context, crit, "Multiple response-specific R-squared values: not collapsed to one statistic.")
      } else {
        columns <- .criterion_columns(crit)
        result$row[[columns[1]]] <- value[1L, "Estimate"]
        result$row[[columns[2]]] <- value[1L, "Est.Error"]
      }
    }
  }
  if ("log_lik" %in% requested) {
    value <- .criterion_eval(context, "log_lik", do.call(brms::log_lik, c(list(model), dots)))
    if (!is.null(value)) {
      totals <- rowSums(value)
      result$row$log_lik <- mean(totals)
      result$row$se_log_lik <- stats::sd(totals)
    }
  }
  result
}
