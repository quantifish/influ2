# A GAM domain uses the fitted coefficient vector and its native joint
# covariance. Prediction matrices are blocked; no model or cell-by-draw array
# is retained in the diagnostic result.
.domain_gam_info <- function(model, year = NULL, data = NULL) {
  .require_model_backend(model)
  if (!inherits(model, "gam") || is.null(model$Vp) ||
      is.null(model$pterms) || is.list(stats::formula(model)) ||
      inherits(model$family, "general.family")) {
    stop("Domain diagnostics require a complete single-predictor mgcv::gam() fit.", call. = FALSE)
  }
  .index_prediction_guards(model, "gam")
  frame <- .residual_model_frame(model)
  if (is.null(frame) || !nrow(frame)) {
    stop("Retain the fitted GAM model frame for domain diagnostics.", call. = FALSE)
  }
  family <- stats::family(model)
  parsed <- .normalise_family_name(family$family)
  if (parsed$structure != "single" || !is.function(family$linkinv)) {
    stop("GAM domain diagnostics require one supported response and inverse link.", call. = FALSE)
  }
  beta <- stats::coef(model)
  if (isFALSE(model$converged) || !length(beta) || any(!is.finite(beta))) {
    stop("GAM domain diagnostics require converged, finite fitted coefficients.", call. = FALSE)
  }
  fitted_data <- if (is.null(data)) frame else {
    .residual_observations(model, data, seq_len(nrow(frame)))$data
  }
  if (is.null(year)) year <- .comparison_focus(model, NULL)
  if (!is.character(year) || length(year) != 1L || is.na(year) ||
      !year %in% names(fitted_data)) {
    stop("`year` must name a retained GAM predictor; supply aligned `data` if necessary.", call. = FALSE)
  }
  covariance <- .index_check_covariance(as.matrix(stats::vcov(model,
    unconditional = !is.null(model$Vc))), length(beta))
  if (!identical(rownames(covariance), names(beta))) {
    stop("GAM coefficient covariance is not in fitted coefficient order.", call. = FALSE)
  }
  list(data = fitted_data, year = year, time = NULL, mode = beta,
    covariance = covariance, family = family$family, link = family$link,
    covariance_target = if (!is.null(model$Vc)) {
      "smoothing-parameter uncertainty corrected coefficient covariance"
    } else "coefficient covariance conditional on estimated smoothing parameters")
}

.domain_gam_fields <- function(model, spatial_terms, spatiotemporal_terms) {
  available <- vapply(model$smooth, function(x) x$label, character(1))
  fields <- list(spatial = spatial_terms, spatiotemporal = spatiotemporal_terms)
  for (field in names(fields)) {
    x <- fields[[field]]
    if (is.null(x)) x <- character()
    if (!is.character(x) || anyNA(x) || any(!nzchar(x)) ||
        anyDuplicated(x) || any(!x %in% available)) {
      stop("`", field, "_terms` must select unique native GAM smooth labels.", call. = FALSE)
    }
    fields[[field]] <- x
  }
  if (length(intersect(fields$spatial, fields$spatiotemporal))) {
    stop("A GAM smooth cannot be both spatial and spatiotemporal.", call. = FALSE)
  }
  fields
}

.domain_gam_sampler <- function(info) {
  # A rank-deficient penalty can leave a semidefinite covariance. Preserve
  # zero-variance directions rather than jittering the fitted uncertainty.
  eig <- eigen(info$covariance, symmetric = TRUE)
  factor <- sweep(eig$vectors, 2L, sqrt(pmax(0, eig$values)), "*")
  function(n) {
    z <- matrix(stats::rnorm(length(info$mode) * n), nrow = length(info$mode))
    sweep(factor %*% z, 1L, info$mode, "+")
  }
}

.domain_gam_projection <- function(model, data, year = NULL, field_terms = NULL) {
  .require_model_backend(model)
  # predict.gam can otherwise resolve an absent predictor in the formula
  # environment, even returning plausible values if its length happens to fit.
  .index_reference_predictors(model, "gam", NULL, data)
  X <- stats::predict(model, newdata = data, type = "lpmatrix")
  beta <- stats::coef(model)
  if (!is.matrix(X) || nrow(X) != nrow(data) ||
      !identical(colnames(X), names(beta)) || any(!is.finite(X)) ||
      !is.null(attr(X, "lpi"))) {
    stop("GAM prediction design requires one finite, aligned linear predictor.", call. = FALSE)
  }
  native <- as.numeric(stats::predict(model, newdata = data, type = "link"))
  if (length(native) != nrow(data) || any(!is.finite(native))) {
    stop("GAM native predictions must be finite and aligned.", call. = FALSE)
  }
  # lpmatrix excludes formula offsets and any native smooth offset. Recover
  # the known contribution from the native fitted predictor, once per block.
  offset <- native - as.numeric(X %*% beta)
  columns <- .gam_term_columns(model, X)
  used <- unlist(columns, use.names = FALSE)
  if (anyDuplicated(used)) stop("GAM scientific terms overlap in coefficient space.", call. = FALSE)
  intercept <- setdiff(seq_len(ncol(X)), used)
  if (length(intercept) && !all(colnames(X)[intercept] == "(Intercept)")) {
    stop("GAM coefficients could not be assigned to complete native terms.", call. = FALSE)
  }
  variables <- lapply(names(columns), function(label) {
    sm <- Filter(function(x) identical(x$label, label), model$smooth)
    if (length(sm)) {
      unique(c(sm[[1L]]$term, if (!identical(sm[[1L]]$by, "NA")) sm[[1L]]$by))
    } else all.vars(str2lang(label))
  })
  names(variables) <- names(columns)
  field_terms <- field_terms %||% list(spatial = character(), spatiotemporal = character())
  zero <- matrix(0, nrow(data), 1L, dimnames = list(NULL, "conditional"))
  validate <- function(par) {
    if (!is.numeric(par) || length(par) != ncol(X) || any(!is.finite(par))) {
      stop("Supply one finite GAM coefficient vector in fitted order.", call. = FALSE)
    }
  }
  eta <- function(par) {
    validate(par)
    matrix(as.numeric(X %*% par) + offset, ncol = 1L,
      dimnames = list(NULL, "conditional"))
  }
  terms <- function(par) {
    validate(par)
    out <- list()
    if (length(intercept)) out <- .domain_term_add(out, "fixed:(Intercept)",
      as.numeric(X[, intercept, drop = FALSE] %*% par[intercept]), 1L, zero)
    for (label in names(columns)) {
      ids <- columns[[label]]
      kind <- if (label %in% vapply(model$smooth, function(x) x$label, character(1))) {
        "smooth:"
      } else "fixed:"
      out <- .domain_term_add(out, paste0(kind, label),
        as.numeric(X[, ids, drop = FALSE] %*% par[ids]), 1L, zero, variables[[label]])
    }
    out <- .domain_normalize_terms(out, year)
    for (field in names(field_terms)) {
      labels <- field_terms[[field]]
      if (length(labels)) {
        if (!all(labels %in% names(out))) {
          stop("Mapped GAM fields must retain their native smooth labels, not a year-only alias.", call. = FALSE)
        }
        value <- Reduce(`+`, out[labels])
        out[labels] <- NULL
        attr(out, "variables")[labels] <- NULL
        if (!is.null(out[[field]])) stop("GAM field alias collides with a native term.", call. = FALSE)
        out <- .domain_term_add(out, field, value[, 1L], 1L, zero,
          unique(unlist(variables[labels], use.names = FALSE)))
      }
    }
    out$offset <- matrix(offset, ncol = 1L,
      dimnames = list(NULL, "conditional"))
    # Normalisation reconstructs component matrices without native dimnames.
    # Restore one consistent component label, including the known offset.
    term_variables <- attr(out, "variables")
    out <- lapply(out, function(x) {
      colnames(x) <- "conditional"
      x
    })
    attr(out, "variables") <- term_variables
    .domain_check_terms(out, eta(par))
    out
  }
  response <- function(par, subtract = NULL) {
    value <- eta(par)
    if (!is.null(subtract)) value <- value - .domain_term_matrix(subtract, nrow(data), 1L)
    .index_check_prediction(model$family$linkinv(value[, 1L]), nrow(data))
  }
  list(obj = NULL, eta = eta, terms = terms, response = response)
}
