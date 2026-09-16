# Native observation likelihood, evaluated at the saved joint mode. Do not call
# fn(): that would reoptimise latent effects. A private objective also avoids
# evaluating or modifying the caller's TMB environment (including saved fits).
.implied_sdmtmb_report <- function(model) {
  obj <- TMB::MakeADFun(data = model$tmb_data, parameters = model$parlist,
    map = model$tmb_map, random = model$tmb_random,
    profile = model$control$profile, DLL = "sdmTMB", silent = TRUE)
  mode <- model$last.par.best
  if (!identical(names(obj$env$last.par.best), names(mode)) ||
      !isTRUE(all.equal(as.numeric(obj$env$last.par.best), as.numeric(mode), tolerance = 1e-10))) {
    stop("Saved sdmTMB parameters do not match the original joint mode.", call. = FALSE)
  }
  obj$report(mode)
}

.implied_sdmtmb_adapter <- function(model, data, year, groups, baseline, component, year_term = NULL) {
  .require_model_backend(model)
  if (is.null(model$tmb_data) || is.null(model$parlist) || is.null(model$sd_report) ||
      is.null(model$data) || !nrow(model$data)) {
    stop("A complete retained sdmTMB fit is required for implied effects.", call. = FALSE)
  }
  if (isTRUE(model$reml) || !isTRUE(model$sd_report$pdHess) ||
      !identical(as.integer(model$model$convergence), 0L) ||
      !length(model$last.par.best) || any(!is.finite(model$last.par.best))) {
    stop("Implied effects require a converged ML sdmTMB fit with a positive-definite Hessian.", call. = FALSE)
  }
  joint <- isTRUE(model$family$delta)
  if (joint && (is.null(component) || !component %in% c("positive", "encounter", "combined"))) {
    stop("A joint sdmTMB implied-effect calculation requires explicit component = 'positive', 'encounter', or 'combined'.", call. = FALSE)
  }
  if (!joint && !is.null(component) && component != "conditional") {
    stop("A single-response sdmTMB fit only supports component = 'conditional'.", call. = FALSE)
  }
  k <- if (joint && component != "encounter") 2L else 1L
  family <- if (joint) model$family[[k]] else model$family
  supported <- (identical(family$family, "lognormal") && identical(family$link, "log")) ||
    (identical(family$family, "binomial") && identical(family$link, "logit"))
  if (!supported || (joint && (!identical(model$family$type, "standard") ||
      !identical(model$family$family, c("binomial", "lognormal")) ||
      !identical(model$family$link, c("logit", "log"))))) {
    stop("The sdmTMB implied-effect adapter supports Bernoulli(logit), lognormal(log), and standard delta-lognormal fits only; Poisson-link delta and mixture families require separate adapters.", call. = FALSE)
  }
  if (!is.null(model$nonlocal_parsed)) {
    stop("Nonlocal sdmTMB covariate operators are not supported for implied effects.", call. = FALSE)
  }
  frame <- .residual_model_frame(model)
  if (is.null(data)) data <- frame
  aligned <- .residual_observations(model, data, seq_len(nrow(frame)))$data
  time <- .resid_year(model, aligned, year)
  f <- model$formula[[k]]
  if (!inherits(f, "formula") || length(f) != 3L || !is.symbol(f[[2L]])) {
    stop("The sdmTMB implied-effect adapter requires a named, untransformed response.", call. = FALSE)
  }
  response <- as.character(f[[2L]])
  if (!groups %in% names(aligned) || anyNA(aligned[[groups]]) ||
      !is.atomic(aligned[[groups]]) || !is.null(dim(aligned[[groups]])) ||
      (is.numeric(aligned[[groups]]) && any(!is.finite(aligned[[groups]])))) {
    stop("`groups` must name a finite categorical column in the original model data.", call. = FALSE)
  }
  if (groups == response || time$name == response) {
    stop("Year and groups must not be defined from the response.", call. = FALSE)
  }
  if (identical(groups, time$name)) stop("Choose a grouping column different from year.", call. = FALSE)
  y <- frame[[response]]
  n <- nrow(frame)
  if (!is.numeric(y) || !is.null(dim(y)) || length(y) != n ||
      any(!is.finite(y) | y < 0) || (!joint && family$family == "lognormal" && any(y == 0))) {
    stop("Lognormal responses must be finite and positive; zeros are allowed only in joint delta fits.", call. = FALSE)
  }
  positive <- y > 0
  yi <- model$tmb_data$y_i
  response_k <- if (joint) 2L else 1L
  present <- if (joint) positive else rep(TRUE, n)
  if (!is.matrix(yi) || nrow(yi) != n || ncol(yi) != response_k ||
      !identical(unname(is.na(yi[, response_k])), !present) ||
      !isTRUE(all.equal(as.numeric(yi[present, response_k]), y[present], tolerance = 0)) ||
      (joint && !identical(as.numeric(yi[, 1L]), as.numeric(positive)))) {
    stop("Native sdmTMB likelihood rows do not match the retained response and positive observations.", call. = FALSE)
  }
  weights <- model$tmb_data$weights_i
  if (!length(weights) %in% c(n, n * response_k) || any(!is.finite(weights) | weights != 1)) {
    stop("Non-unit case weights are not yet supported for implied effects.", call. = FALSE)
  }
  if (!joint && family$family == "binomial" &&
      (any(!y %in% c(0, 1)) || any(model$tmb_data$size != 1))) {
    stop("Binomial implied effects currently require Bernoulli (0/1) observations with size = 1.", call. = FALSE)
  }
  r <- .implied_sdmtmb_report(model)
  if (!joint && family$family == "binomial") r$phi <- 1
  if (!is.matrix(r$eta_i) || !identical(dim(r$eta_i), c(n, response_k)) ||
      any(!is.finite(r$eta_i)) || length(r$phi) != response_k ||
      any(!is.finite(r$phi) | r$phi <= 0)) {
    stop("Finite aligned sdmTMB predictors and native log-scale SD are required.", call. = FALSE)
  }
  if (identical(component, "combined")) {
    if (!is.null(year_term)) stop("Combined implied responses use fitted observation means, not a selected year-term baseline.", call. = FALSE)
    return(list(backend = "sdmTMB", data = aligned, year = time$name,
      observed = y, eta = r$eta_i, dispersion = rep(r$phi[2L], n),
      response = response, component = "combined"))
  }
  if (is.null(year_term)) year_term <- time$name
  if (!is.character(year_term) || length(year_term) != 1L || is.na(year_term) ||
      !year_term %in% names(aligned) || anyNA(aligned[[year_term]])) {
    stop("`year_term` must name one retained annual predictor column.", call. = FALSE)
  }
  if (year_term == response || any(vapply(split(aligned[[year_term]], aligned[[time$name]]),
      function(x) length(unique(x)) > 1L, logical(1)))) {
    stop("`year_term` must be constant within each requested year and must not be the response.", call. = FALSE)
  }
  X <- model$tmb_data$X_ij[[k]]
  beta <- model$parlist[[if (k == 2L) "b_j2" else "b_j"]]
  terms <- model$terms[[k]]
  labels <- attr(terms, "term.labels")
  assignments <- attr(X, "assign")
  if (is.null(X) || nrow(X) != n || ncol(X) != length(beta) ||
      length(assignments) != ncol(X) || any(!is.finite(X)) || any(!is.finite(beta))) {
    stop("The sdmTMB fixed-effect matrix and coefficients are not aligned.", call. = FALSE)
  }
  variables <- lapply(labels, function(label) all.vars(stats::as.formula(paste("~", label))))
  selected <- which(vapply(variables, function(v) year_term %in% v, logical(1)))
  # Original formula terms retain smooths that the fixed-effect matrix omits.
  original <- attr(stats::terms(f), "term.labels")
  year_smooth <- vapply(original, function(label) {
    grepl("(^|::)(s|t2|te|ti)\\(", label) &&
      any(c(time$name, year_term) %in% all.vars(stats::as.formula(paste("~", label))))
  }, logical(1))
  if (!length(selected) || any(lengths(variables[selected]) != 1L) || any(year_smooth)) {
    stop("An additive fixed year term is required; set year_term explicitly if the component uses a different annual predictor. Year interactions and year smooths need a separate baseline definition.", call. = FALSE)
  }
  group_term <- which(vapply(variables, function(v) identical(v, groups), logical(1)))
  if (baseline == "year_group") selected <- union(selected, group_term)
  columns <- which(assignments %in% selected)
  if (!length(columns)) stop("No fixed-effect baseline columns were found.", call. = FALSE)
  term <- drop(X[, columns, drop = FALSE] %*% beta[columns])
  included <- if (identical(component, "positive")) positive else rep(TRUE, n)
  list(backend = "sdmTMB", family = family$family, link = family$link, frame = frame,
    data = aligned, year = time$name, observed = if (identical(component, "encounter")) as.numeric(positive) else as.numeric(y), eta = r$eta_i[, k],
    dispersion = rep(r$phi[k], n), baseline = term - mean(term),
    baseline_terms = labels[selected], response = response, log_response = FALSE,
    included = included, component = if (joint) component else "single fitted response",
    year_term = year_term, component_index = k,
    baseline_group_present = baseline == "year_group" && length(group_term) > 0L)
}
