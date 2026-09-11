.model_backend_available <- function(package) {
  requireNamespace(package, quietly = TRUE)
}

# Check before formula/family extraction: native S3 methods are not registered
# when the fitted model's optional backend is absent. Compact result objects
# deliberately have no backend requirement for their summary/plot methods.
.require_model_backend <- function(model) {
  packages <- c(brmsfit = "brms", glmmTMB = "glmmTMB", sdmTMB = "sdmTMB",
    tinyVAST = "tinyVAST", gam = "mgcv")
  matches <- vapply(names(packages), inherits, logical(1), x = model)
  if (!any(matches)) return(invisible(NULL))
  package <- unname(packages[which(matches)[1L]])
  if (!.model_backend_available(package)) {
    stop("Package '", package, "' is required to calculate diagnostics or indices ",
      "from this fitted model. Install it first; saved compact results can ",
      "still be summarised and plotted without it.", call. = FALSE)
  }
  invisible(NULL)
}
