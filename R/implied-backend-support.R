# Shared checks for native observation-likelihood adapters. These do not
# manufacture a likelihood from PIT residuals or marginal fitted means.
.implied_native_data <- function(model, data, year, groups, formula, family) {
  frame <- .residual_model_frame(model)
  if (is.null(frame) || !nrow(frame)) stop("A complete retained model frame is required.", call. = FALSE)
  if (is.null(data)) data <- frame
  aligned <- .residual_observations(model, data, seq_len(nrow(frame)))$data
  time <- .resid_year(model, aligned, year)
  lhs <- formula[[2L]]
  logged <- family == "gaussian" && is.call(lhs) && length(lhs) == 2L &&
    identical(lhs[[1L]], as.name("log")) && is.symbol(lhs[[2L]])
  if (!is.symbol(lhs) && !logged) stop("Use a named response or a Gaussian model of log(response).", call. = FALSE)
  response <- all.vars(lhs)
  if (!groups %in% names(aligned) || anyNA(aligned[[groups]]) ||
      !is.atomic(aligned[[groups]]) || !is.null(dim(aligned[[groups]])) ||
      (is.numeric(aligned[[groups]]) && any(!is.finite(aligned[[groups]])))) {
    stop("`groups` must name a finite categorical column in the original model data.", call. = FALSE)
  }
  if (groups == response || time$name == response) stop("Year and groups must not be defined from the response.", call. = FALSE)
  if (groups == time$name) stop("Choose a grouping column different from year.", call. = FALSE)
  y <- if (logged) log(frame[[response]]) else frame[[response]]
  if (!is.numeric(y) || !is.null(dim(y)) || length(y) != nrow(frame) || any(!is.finite(y))) {
    stop("A finite numeric response vector aligned with the original rows is required.", call. = FALSE)
  }
  list(frame = frame, data = aligned, year = time$name, observed = y,
    response = paste(deparse(lhs), collapse = " "), log_response = logged)
}

.implied_native_family <- function(family, link, y, joint = FALSE) {
  if (!((family == "gaussian" && link == "identity") ||
      (family == "binomial" && link == "logit") ||
      (family %in% c("poisson", "nbinom2", "Gamma", "lognormal") && link == "log"))) {
    stop("Supported native implied-effect families are Gaussian(identity), Bernoulli(logit), Poisson(log), NB2(log), Gamma(log), and lognormal; other parameterisations require a validated adapter.", call. = FALSE)
  }
  if (family == "binomial" && any(!y %in% c(0, 1))) stop("Binomial implied effects require Bernoulli (0/1) observations.", call. = FALSE)
  if (family %in% c("poisson", "nbinom2") && any(y < 0 | abs(y - round(y)) > 1e-7)) stop("Count-model responses must be non-negative integers.", call. = FALSE)
  if (family %in% c("Gamma", "lognormal") && any(y < 0 | (!joint & y == 0))) stop("Positive-family responses must be strictly positive; only joint fits allow zeros.", call. = FALSE)
}

.implied_native_baseline <- function(a, groups, baseline, year_term,
    X, beta, term_columns, smooths = character()) {
  year_term <- year_term %||% a$year
  if (!is.character(year_term) || length(year_term) != 1L || is.na(year_term) ||
      !year_term %in% names(a$data) || anyNA(a$data[[year_term]])) {
    stop("`year_term` must name one retained annual predictor column.", call. = FALSE)
  }
  if (year_term %in% all.vars(stats::as.formula(paste("~", a$response))) ||
      any(vapply(split(a$data[[year_term]], a$data[[a$year]]),
        function(x) length(unique(x)) > 1L, logical(1)))) {
    stop("`year_term` must be constant within each requested year and must not be the response.", call. = FALSE)
  }
  if (is.null(X) || nrow(X) != nrow(a$data) || ncol(X) != length(beta) ||
      any(!is.finite(X)) || any(!is.finite(beta))) stop("The fixed-effect matrix and coefficients are not aligned.", call. = FALSE)
  labels <- names(term_columns)
  vars <- lapply(labels, function(label) all.vars(stats::as.formula(paste("~", label))))
  selected <- which(vapply(vars, function(v) year_term %in% v, logical(1)))
  if (!length(selected) || any(lengths(vars[selected]) != 1L) ||
      any(c(a$year, year_term) %in% smooths)) {
    stop("An additive fixed year term is required; set year_term explicitly if needed. Year interactions and year smooths need a separate baseline definition.", call. = FALSE)
  }
  group_term <- which(vapply(vars, function(v) identical(v, groups), logical(1)))
  if (baseline == "year_group") selected <- union(selected, group_term)
  columns <- unique(unlist(term_columns[selected], use.names = FALSE))
  if (!length(columns) || any(!columns %in% seq_len(ncol(X)))) stop("No aligned fixed-effect baseline columns were found.", call. = FALSE)
  term <- drop(X[, columns, drop = FALSE] %*% beta[columns])
  a$baseline <- term - mean(term)
  a$baseline_terms <- labels[selected]
  a$baseline_group_present <- baseline == "year_group" && length(group_term) > 0L
  a$year_term <- year_term
  a
}

.implied_native_predictors <- function(a) {
  n <- nrow(a$data)
  if (length(a$eta) != n || any(!is.finite(a$eta)) ||
      !length(a$dispersion) %in% c(1L, n) || any(!is.finite(a$dispersion) | a$dispersion <= 0)) {
    stop("Finite aligned predictors and positive native dispersion values are required.", call. = FALSE)
  }
  a$eta <- as.numeric(a$eta)
  a$dispersion <- rep_len(as.numeric(a$dispersion), n)
  a
}
