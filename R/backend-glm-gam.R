.glm_term_columns <- function(model, X) {
  assign <- attr(X, "assign")
  labels <- attr(stats::terms(model), "term.labels")
  if (is.null(assign)) {
    stop("The GLM model matrix does not contain term assignments.", call. = FALSE)
  }
  out <- lapply(seq_along(labels), function(i) which(assign == i))
  names(out) <- labels
  out[lengths(out) > 0L]
}

.gam_term_columns <- function(model, X) {
  labels <- attr(model$pterms, "term.labels") %||% character()
  out <- list()

  if (length(labels)) {
    for (i in seq_along(labels)) {
      columns <- which(model$assign == i)
      if (length(columns)) out[[labels[i]]] <- columns
    }
  }

  if (length(model$smooth)) {
    for (smooth in model$smooth) {
      out[[smooth$label]] <- seq.int(smooth$first.para, smooth$last.para)
    }
  }

  out <- lapply(out, function(i) i[i <= ncol(X)])
  out[lengths(out) > 0L]
}

.frequentist_family_spec <- function(model, backend, response) {
  fam <- stats::family(model)
  spec <- .new_family_spec(fam$family, fam$link, backend = backend)
  if (grepl("^log\\s*\\(", response)) spec$natural_scale <- "ratio"
  spec
}

.prepare_frequentist_matrix <- function(model, X, term_columns,
                                        beta = stats::coef(model),
  V = stats::vcov(model)) {
  original_x_names <- colnames(X)

  if (!is.null(colnames(X)) && !is.null(names(beta))) {
    keep_names <- intersect(colnames(X), names(beta))
    X <- X[, keep_names, drop = FALSE]
    beta <- beta[keep_names]
    if (!is.null(V)) {
      V <- V[keep_names, keep_names, drop = FALSE]
    }
    old_index <- match(keep_names, original_x_names)
    new_index <- seq_along(keep_names)
    names(new_index) <- old_index
    term_columns <- lapply(term_columns, function(i) {
      unname(new_index[as.character(i)][!is.na(new_index[as.character(i)])])
    })
  }

  finite <- is.finite(beta)
  if (!all(finite)) {
    old <- seq_along(beta)
    X <- X[, finite, drop = FALSE]
    if (!is.null(V)) {
      V <- V[finite, finite, drop = FALSE]
    }
    beta <- beta[finite]
    map <- seq_along(old[finite])
    names(map) <- old[finite]
    term_columns <- lapply(term_columns, function(i) {
      unname(map[as.character(i)][!is.na(map[as.character(i)])])
    })
  }

  list(
    X = X,
    beta = beta,
    vcov = V,
    term_columns = term_columns[lengths(term_columns) > 0L]
  )
}

.glm_reference_matrix <- function(model, reference_data) {
  Terms <- stats::delete.response(stats::terms(model))
  frame <- stats::model.frame(
    Terms,
    reference_data,
    xlev = model$xlevels,
    na.action = stats::na.pass
  )
  stats::model.matrix(Terms, frame, contrasts.arg = model$contrasts)
}

.align_reference_matrix <- function(reference_X, fitted_X) {
  if (is.null(reference_X)) return(NULL)
  missing <- setdiff(colnames(fitted_X), colnames(reference_X))
  if (length(missing)) {
    stop(
      "The reference grid is missing model-matrix columns: ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  reference_X[, colnames(fitted_X), drop = FALSE]
}

#' Influence diagnostics for generalised linear models
#'
#' Fixed offsets/exposure are supported for single-component log-link ratios
#' and identity-link contrasts, where the common reference offset cancels.
#' They are not estimated influence terms. Nonlinear probability and combined
#' hurdle/zero-inflated diagnostics with offsets fail explicitly. Nominal
#' summaries always describe the observed response; they are not automatically
#' divided by exposure to obtain CPUE.
#'
#' @param model A fitted object from [stats::glm()].
#' @param focus Name of the focus variable, usually year.
#' @param data Optional model data. The model frame is used by default.
#' @param weights Optional numeric observation weights or the name of a weight
#'   column in `data`.
#' @param reference_data Optional prediction grid defining the common
#'   standardisation distribution. By default the observed data are used.
#' @param reference_weights Optional numeric weights, or a column name, for
#'   `reference_data`.
#' @param uncertainty One of `"auto"`, `"none"`, `"analytic"`, or
#'   `"simulation"`.
#' @param retain One of `"summary"`, `"derived_draws"`, or `"disk"`.
#' @param probs Lower and upper interval probabilities.
#' @param ndraws Number of joint coefficient draws for simulation uncertainty.
#' @param seed Optional simulation seed.
#' @param draws_path Output path used when `retain = "disk"`.
#' @param keep_model Retain the fitted model inside the diagnostic object.
#' @param ... Reserved for future backend options.
#'
#' @return An [influ_diag] object.
#' @export
influ.glm <- function(model, focus, data = NULL, weights = NULL,
                      reference_data = NULL, reference_weights = NULL,
                      uncertainty = "auto", retain = "summary",
                      probs = c(0.025, 0.975), ndraws = 1000L, seed = NULL,
                      draws_path = NULL, keep_model = FALSE, ...) {
  data <- .resolve_influ_data(model, data)
  X <- stats::model.matrix(model)
  term_columns <- .glm_term_columns(model, X)
  prepared <- .prepare_frequentist_matrix(model, X, term_columns)
  reference_X <- if (is.null(reference_data)) NULL else {
    .align_reference_matrix(
      .glm_reference_matrix(model, as.data.frame(reference_data)),
      prepared$X
    )
  }
  response <- names(stats::model.frame(model))[1]
  family_spec <- .frequentist_family_spec(model, "glm", response)
  offset_sources <- .influ_offset_sources(
    stats::formula(model), model$call, model$offset
  )
  .check_influ_offset_scope(family_spec, offset_sources)

  out <- .influ_linear_engine(
    backend = "glm",
    model = model,
    data = data,
    response = response,
    focus = focus,
    family_spec = family_spec,
    X = prepared$X,
    beta = prepared$beta,
    vcov = prepared$vcov,
    term_columns = prepared$term_columns,
    uncertainty = uncertainty,
    retain = retain,
    probs = probs,
    weights = weights,
    reference_data = reference_data,
    reference_X = reference_X,
    reference_weights = reference_weights,
    ndraws = ndraws,
    seed = seed,
    draws_path = draws_path,
    keep_model = keep_model
  )
  .record_influ_offset_scope(out, offset_sources)
}

#' Influence diagnostics for generalised additive models
#'
#' @inheritParams influ.glm
#' @param model A fitted object from [mgcv::gam()].
#'
#' @return An [influ_diag] object.
#' @export
influ.gam <- function(model, focus, data = NULL, weights = NULL,
                      reference_data = NULL, reference_weights = NULL,
                      uncertainty = "auto", retain = "summary",
                      probs = c(0.025, 0.975), ndraws = 1000L, seed = NULL,
                      draws_path = NULL, keep_model = FALSE, ...) {
  data <- .resolve_influ_data(model, data)
  X <- stats::predict(model, newdata = data, type = "lpmatrix")
  term_columns <- .gam_term_columns(model, X)
  prepared <- .prepare_frequentist_matrix(model, X, term_columns)
  reference_X <- if (is.null(reference_data)) NULL else {
    .align_reference_matrix(
      stats::predict(model, newdata = reference_data, type = "lpmatrix"),
      prepared$X
    )
  }
  response <- names(stats::model.frame(model))[1]
  family_spec <- .frequentist_family_spec(model, "gam", response)
  offset_sources <- .influ_offset_sources(
    stats::formula(model), model$call, model$offset
  )
  .check_influ_offset_scope(family_spec, offset_sources)

  out <- .influ_linear_engine(
    backend = "gam",
    model = model,
    data = data,
    response = response,
    focus = focus,
    family_spec = family_spec,
    X = prepared$X,
    beta = prepared$beta,
    vcov = prepared$vcov,
    term_columns = prepared$term_columns,
    uncertainty = uncertainty,
    retain = retain,
    probs = probs,
    weights = weights,
    reference_data = reference_data,
    reference_X = reference_X,
    reference_weights = reference_weights,
    ndraws = ndraws,
    seed = seed,
    draws_path = draws_path,
    keep_model = keep_model
  )
  .record_influ_offset_scope(out, offset_sources)
}
