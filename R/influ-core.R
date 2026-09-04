#' Calculate CPUE influence diagnostics
#'
#' `influ()` is the model-neutral entry point for influence diagnostics. It
#' dispatches on the fitted model class and returns the same compact
#' [influ_diag] structure for every backend.
#'
#' @param model A fitted model object.
#' @param ... Arguments passed to a model-specific method.
#'
#' @return An object inheriting from `influ_diag`.
#' @export
influ <- function(model, ...) {
  UseMethod("influ")
}

#' @export
influ.default <- function(model, ...) {
  stop(
    "No influ() method is available for objects of class '",
    paste(class(model), collapse = "/"), "'.",
    call. = FALSE
  )
}

#' A model-neutral influence diagnostic
#'
#' An `influ_diag` stores derived diagnostics rather than observation-by-draw
#' arrays. Its tables have a stable schema across model backends, allowing the
#' same summary and plotting methods to be used for maximum-likelihood and
#' Bayesian models.
#'
#' @param backend Name of the model backend.
#' @param family An `influ_family_spec` object.
#' @param focus Name of the focus variable, usually time.
#' @param influence Long-form influence results.
#' @param coefficients Compact coefficient or field summaries.
#' @param composition Compact data-composition summaries.
#' @param indices Nominal and standardised index summaries.
#' @param metrics Compact overall and trend influence metrics.
#' @param uncertainty Description of the uncertainty calculation.
#' @param retained Description of retained posterior or sampling information.
#' @param metadata Additional model and calculation metadata.
#' @param draws Optional derived draws. These are never observation-level
#'   posterior arrays.
#' @param model Optional retained fitted model.
#'
#' @return An object of class `influ_diag` and a backend-specific subclass.
#' @keywords internal
new_influ_diag <- function(backend, family, focus, influence,
                           coefficients = NULL, composition = NULL,
                           indices = NULL, metrics = NULL, uncertainty = list(),
                           retained = list(), metadata = list(), draws = NULL,
                           model = NULL) {
  stopifnot(
    is.character(backend), length(backend) == 1L,
    inherits(family, "influ_family_spec"),
    is.character(focus), length(focus) == 1L,
    is.data.frame(influence)
  )

  out <- list(
    backend = backend,
    family = family,
    focus = focus,
    influence = influence,
    coefficients = coefficients %||% data.frame(),
    composition = composition %||% data.frame(),
    indices = indices %||% data.frame(),
    metrics = metrics %||% data.frame(),
    uncertainty = uncertainty,
    retained = retained,
    metadata = metadata,
    draws = draws,
    model = model
  )

  class(out) <- c(paste0("influ_diag_", tolower(backend)), "influ_diag")
  validate_influ_diag(out)
}

validate_influ_diag <- function(x) {
  required <- c(
    "focus", "level", "term", "component", "scale", "estimate",
    "std_error", "lower", "upper", "method"
  )
  missing <- setdiff(required, names(x$influence))
  if (length(missing)) {
    stop(
      "The influence table is missing required columns: ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }

  if (!all(x$influence$focus == x$focus)) {
    stop("Every influence row must use the object's focus variable.", call. = FALSE)
  }

  x
}

#' Extract a table from an influence diagnostic
#'
#' @param x An [influ_diag] object.
#'
#' @return A data frame.
#' @name influ_extractors
NULL

#' @rdname influ_extractors
#' @export
influ_effects <- function(x) {
  .assert_influ_diag(x)
  x$influence
}

#' @rdname influ_extractors
#' @export
influ_indices <- function(x) {
  .assert_influ_diag(x)
  x$indices
}

#' @rdname influ_extractors
#' @export
influ_composition <- function(x) {
  .assert_influ_diag(x)
  x$composition
}

#' @rdname influ_extractors
#' @export
influ_draws <- function(x) {
  .assert_influ_diag(x)
  x$draws
}

#' @rdname influ_extractors
#' @export
influ_metrics <- function(x) {
  .assert_influ_diag(x)
  x$metrics
}

.assert_influ_diag <- function(x) {
  if (!inherits(x, "influ_diag")) {
    stop("`x` must be an influ_diag object.", call. = FALSE)
  }
  invisible(x)
}

#' @export
print.influ_diag <- function(x, ...) {
  .assert_influ_diag(x)
  cat("<influ_diag>\n")
  cat("  Backend:     ", x$backend, "\n", sep = "")
  cat(
    "  Response:    ", x$family$structure, " ", x$family$family,
    " (", x$family$link, ")\n", sep = ""
  )
  cat("  Focus:       ", x$focus, "\n", sep = "")
  cat(
    "  Terms:       ", length(unique(x$influence$term)), "\n",
    sep = ""
  )
  cat(
    "  Focus levels:", length(unique(x$influence$level)), "\n",
    sep = ""
  )
  cat(
    "  Uncertainty: ", x$uncertainty$method %||% "none", "\n",
    sep = ""
  )
  cat(
    "  Retained:    ", x$retained$mode %||% "summary", "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
summary.influ_diag <- function(object, ...) {
  .assert_influ_diag(object)
  link <- object$influence[object$influence$scale == "link", , drop = FALSE]
  if (!nrow(link)) link <- object$influence

  split_rows <- split(seq_len(nrow(link)), link$term)
  term_summary <- do.call(
    rbind,
    lapply(names(split_rows), function(term) {
      z <- link[split_rows[[term]], , drop = FALSE]
      data.frame(
        term = term,
        component = paste(unique(z$component), collapse = ", "),
        maximum_absolute_link_influence = max(abs(z$estimate), na.rm = TRUE),
        level_at_maximum = as.character(z$level[which.max(abs(z$estimate))]),
        stringsAsFactors = FALSE
      )
    })
  )
  rownames(term_summary) <- NULL
  term_summary <- term_summary[
    order(term_summary$maximum_absolute_link_influence, decreasing = TRUE),
    , drop = FALSE
  ]

  structure(
    list(
      backend = object$backend,
      family = object$family,
      focus = object$focus,
      uncertainty = object$uncertainty,
      retained = object$retained,
      metrics = object$metrics,
      term_summary = term_summary
    ),
    class = "summary.influ_diag"
  )
}

#' @export
print.summary.influ_diag <- function(x, ...) {
  cat("Influence diagnostic summary\n")
  cat("  Backend: ", x$backend, "\n", sep = "")
  cat(
    "  Family:  ", x$family$structure, " ", x$family$family,
    " (", x$family$link, ")\n", sep = ""
  )
  cat("  Focus:   ", x$focus, "\n\n", sep = "")
  print(x$term_summary, row.names = FALSE)
  invisible(x)
}
