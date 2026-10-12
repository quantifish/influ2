#' @export
.criterion_model.merMod <- function(model, requested, context, dots) {
  .require_model_backend(model)
  if (!inherits(model, c("lmerMod", "glmerMod"))) stop("lme4 criteria support lmer/glmer, not nonlinear nlmer models.", call. = FALSE)
  opt <- model@optinfo
  messages <- opt$conv$lme4$messages %||% character()
  converged <- !any(opt$conv$opt != 0) && !any(!grepl("boundary.*singular", messages, ignore.case = TRUE))
  result <- .criterion_base(model, "lme4", stats::formula(model), stats::family(model),
    context, converged = converged)
  result$row$df_type <- "native likelihood parameter count (fixed effects, variance parameters, and dispersion)"
  result$row$df_residual <- .criterion_scalar(stats::df.residual(model))
  result$row$likelihood <- if (inherits(model, "lmerMod")) "Gaussian marginal likelihood" else "native GLMM marginal likelihood approximation"
  result$row$AIC_type <- "marginal"
  if (inherits(model, "glmerMod")) {
    result$row$nAGQ <- unname(lme4::getME(model, "devcomp")$dims["nAGQ"])
    if (!identical(as.integer(result$row$nAGQ), 1L)) {
      result$eligible <- FALSE
      .criterion_note(context, "ranking", "Non-default GLMM quadrature/PIRLS objective: native values retained, but likelihood-constant comparability is not validated for automatic cross-backend ranking.", "method")
    }
  }
  result$row$deviance_type <- if (inherits(model, "lmerMod")) "native ML criterion (not a response residual deviance)" else "native conditional response deviance"
  result <- .criterion_restricted(result, lme4::isREML(model), context)
  if (lme4::isSingular(model)) .criterion_note(context, "fit", "Singular random-effect covariance: inspect boundary estimates before interpreting comparisons.", "warning")
  .criterion_note(context, "likelihood", "Native likelihood approximation and constants must be compatible before cross-backend ranking; transformed responses remain distinct.", "method")
  .criterion_frequentist(result, model, requested, context,
    deviance = function() if (lme4::isREML(model)) NA_real_ else stats::deviance(model))
}
