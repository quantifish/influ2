.implied_tinyvast_adapter <- function(model, data, year, groups, baseline, component, year_term) {
  .require_model_backend(model)
  if (is.null(model$internal$parlist) || is.null(model$tmb_inputs$tmb_data) ||
      is.null(model$sdrep) || is.null(model$obj$env)) stop("A complete retained tinyVAST fit is required for implied effects.", call. = FALSE)
  if (isTRUE(model$internal$control$reml) || !isTRUE(model$sdrep$pdHess) ||
      !identical(as.integer(model$opt$convergence), 0L)) {
    stop("Implied effects require a converged ML tinyVAST fit with a positive-definite Hessian.", call. = FALSE)
  }
  if (length(model$internal$family) != 1L ||
      length(unique(model$tmb_inputs$tmb_data$c_i)) > 1L) {
    stop("tinyVAST implied effects currently require one response and one family.", call. = FALSE)
  }
  fam <- model$internal$family[[1L]]
  joint <- isTRUE(fam$delta)
  if (joint && (is.null(component) || !component %in% c("positive", "encounter", "combined"))) {
    stop("A joint tinyVAST fit requires explicit component = 'positive', 'encounter', or 'combined'.", call. = FALSE)
  }
  if (!joint && !is.null(component) && component != "conditional") stop("A single-response fit only supports component = 'conditional'.", call. = FALSE)
  if (joint && (!identical(fam$type, "standard") ||
      !identical(fam$family, c("binomial", "lognormal")) ||
      !identical(fam[[1L]]$link, "logit") || !identical(fam[[2L]]$link, "log"))) {
    stop("Joint tinyVAST implied effects support standard delta-lognormal only; Poisson-link delta and other mixtures require separate adapters.", call. = FALSE)
  }
  k <- if (joint && component != "encounter") 2L else 1L
  f <- if (joint) fam[[k]] else fam
  a <- .implied_native_data(model, data, year, groups, model$formula, f$family)
  raw_y <- a$observed
  td <- model$tmb_inputs$tmb_data
  n <- length(raw_y)
  if (!isTRUE(all.equal(as.numeric(td$y_i), raw_y, tolerance = 0))) stop("Native tinyVAST likelihood rows do not match the retained response.", call. = FALSE)
  if (length(td$weights_i) != n || any(!is.finite(td$weights_i) | td$weights_i != 1) ||
      any(!is.finite(td$size_i) | td$size_i != 1)) stop("Non-unit case weights or grouped binomial trials are not supported for implied effects.", call. = FALSE)
  a$backend <- "tinyVAST"
  a$family <- f$family
  a$link <- f$link
  a$component <- if (joint) component else "single fitted response"
  a$component_index <- k
  a$included <- if (identical(component, "positive")) raw_y > 0 else rep(TRUE, n)
  if (identical(component, "encounter")) a$observed <- as.numeric(raw_y > 0)
  .implied_native_family(a$family, a$link, a$observed, joint = joint)
  native <- .resid_tmb_object(model, "tinyVAST", "fitted")
  r <- native$obj$report(native$par)
  a$eta <- if (k == 1L) r$p_i else r$p2_i
  sigma <- exp(model$internal$parlist$log_sigma)
  a$dispersion <- if (a$family %in% c("poisson", "binomial")) 1 else if (a$family == "Gamma") sigma^2 else sigma
  a <- .implied_native_predictors(a)
  if (identical(component, "combined")) {
    if (!is.null(year_term)) stop("Combined implied responses do not use a year-term baseline.", call. = FALSE)
    if (length(r$p_i) != n || any(!is.finite(r$p_i))) stop("Finite aligned encounter predictors are required.", call. = FALSE)
    a$eta <- cbind(r$p_i, r$p2_i)
    return(a)
  }
  setup <- if (k == 1L) model$internal$gam_setup else model$internal$delta_gam_setup
  X <- if (k == 1L) td$X_ij else td$X2_ij
  beta <- model$internal$parlist[[if (k == 1L) "alpha_j" else "alpha2_j"]]
  labels <- attr(setup$pterms, "term.labels")
  columns <- stats::setNames(lapply(seq_along(labels), function(j) which(setup$assign == j)), labels)
  smooths <- unlist(lapply(setup$smooth, function(s) c(s$term, s$by)), use.names = FALSE)
  .implied_native_baseline(a, groups, baseline, year_term, X, beta, columns, smooths)
}
