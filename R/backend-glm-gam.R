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
    V <- V[keep_names, keep_names, drop = FALSE]
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
    V <- V[finite, finite, drop = FALSE]
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

#' Influence diagnostics for generalised linear models
#'
#' @param model A fitted object from [stats::glm()].
#' @param focus Name of the focus variable, usually year.
#' @param data Optional model data. The model frame is used by default.
#' @param weights Optional numeric observation weights or the name of a weight
#'   column in `data`.
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
                      uncertainty = "auto", retain = "summary",
                      probs = c(0.025, 0.975), ndraws = 1000L, seed = NULL,
                      draws_path = NULL, keep_model = FALSE, ...) {
  data <- .resolve_influ_data(model, data)
  X <- stats::model.matrix(model)
  term_columns <- .glm_term_columns(model, X)
  prepared <- .prepare_frequentist_matrix(model, X, term_columns)
  response <- names(stats::model.frame(model))[1]
  family_spec <- .frequentist_family_spec(model, "glm", response)

  .influ_linear_engine(
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
    ndraws = ndraws,
    seed = seed,
    draws_path = draws_path,
    keep_model = keep_model
  )
}

#' Influence diagnostics for generalised additive models
#'
#' @inheritParams influ.glm
#' @param model A fitted object from [mgcv::gam()].
#'
#' @return An [influ_diag] object.
#' @export
influ.gam <- function(model, focus, data = NULL, weights = NULL,
                      uncertainty = "auto", retain = "summary",
                      probs = c(0.025, 0.975), ndraws = 1000L, seed = NULL,
                      draws_path = NULL, keep_model = FALSE, ...) {
  data <- .resolve_influ_data(model, data)
  X <- stats::predict(model, newdata = data, type = "lpmatrix")
  term_columns <- .gam_term_columns(model, X)
  prepared <- .prepare_frequentist_matrix(model, X, term_columns)
  response <- names(stats::model.frame(model))[1]
  family_spec <- .frequentist_family_spec(model, "gam", response)

  .influ_linear_engine(
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
    ndraws = ndraws,
    seed = seed,
    draws_path = draws_path,
    keep_model = keep_model
  )
}
