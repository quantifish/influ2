.is_step_model <- function(x) {
  inherits(x, c("glm", "gam", "glmmTMB", "brmsfit", "sdmTMB", "tinyVAST"))
}

.step_label_vector <- function(labels, n) {
  if (!is.character(labels) || length(labels) != n || anyNA(labels) ||
      any(!nzchar(trimws(labels))) || anyDuplicated(labels)) {
    stop("`labels` must contain one unique, non-empty label per step.", call. = FALSE)
  }
  labels
}

.step_focus_composition <- function(diagnostic, component) {
  composition <- diagnostic$composition
  if (!nrow(composition)) return(NULL)
  candidates <- unique(composition$term)
  is_focus <- vapply(candidates, function(term) {
    variables <- tryCatch(all.vars(stats::as.formula(paste("~", term))),
      error = function(e) character())
    identical(variables, diagnostic$focus)
  }, logical(1))
  composition <- composition[composition$term %in% candidates[is_focus], , drop = FALSE]
  if ("component" %in% names(composition) &&
      component %in% composition$component) {
    composition <- composition[composition$component == component, , drop = FALSE]
  }
  columns <- intersect(c("level", "term_level", "n", "weight"), names(composition))
  composition <- unique(composition[columns])
  if (!nrow(composition)) return(NULL)
  composition <- composition[order(composition$level, composition$term_level), , drop = FALSE]
  rownames(composition) <- NULL
  composition
}

.step_model_rows <- function(model, focus, response, data = NULL) {
  if (is.null(model)) return(NULL)
  if (is.null(data)) data <- tryCatch(.resolve_influ_data(model), error = function(e) NULL)
  if (is.null(data) || !focus %in% names(data)) return(NULL)
  frame <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  y <- if (!is.null(frame)) tryCatch(stats::model.response(frame), error = function(e) NULL)
  if (is.null(y) && length(response) == 1L && response %in% names(data)) y <- data[[response]]
  if (is.null(y) || NROW(y) != nrow(data)) return(NULL)
  ordering <- order(rownames(data))
  list(
    row = rownames(data)[ordering],
    focus = as.character(data[[focus]])[ordering],
    response = if (is.matrix(y)) unname(y[ordering, , drop = FALSE]) else unname(y[ordering]),
    weights = if (is.null(frame)) NULL else {
      w <- stats::model.weights(frame) %||% rep(1, nrow(data))
      unname(w[ordering])
    },
    offset = if (is.null(frame)) NULL else {
      offset <- stats::model.offset(frame) %||% rep(0, nrow(data))
      unname(offset[ordering])
    }
  )
}

.step_payload <- function(input, year, component, probs, arguments, keep_fits) {
  precomputed <- inherits(input, "influ_diag")
  if (precomputed && length(arguments)) {
    stop("Influence calculation arguments cannot change a precomputed diagnostic; supply fitted models instead.", call. = FALSE)
  }
  if (!precomputed) {
    # Recover the fitted analysis rows before calculating contrasts. Re-reading
    # a same-sized but subsequently changed call$data would otherwise group the
    # original design matrix using different years or covariates.
    source <- input
    if (!is.null(arguments$data)) {
      source$call$data <- as.data.frame(arguments$data)
    }
    backend <- .step_backend(source)
    if (backend %in% c("brmsfit", "sdmTMB", "tinyVAST") &&
        is.data.frame(source$data)) {
      # These backends retain their analysis data directly. Supplied delta or
      # multivariate fits need not satisfy the single-formula refit restriction.
      arguments$data <- as.data.frame(source$data)
    } else {
      locked <- .step_locked_data(source, .step_main_formula(source), backend)
      arguments$data <- locked$data
    }
  }
  diagnostic <- if (precomputed) input else {
    do.call(influ, c(list(model = input, focus = year, probs = probs), arguments))
  }
  .assert_influ_diag(diagnostic)
  if (!identical(diagnostic$focus, year)) {
    stop("All steps must use the same focus variable.", call. = FALSE)
  }
  indices <- diagnostic$indices
  indices <- indices[indices$series == "standardised", , drop = FALSE]
  if (!nrow(indices)) {
    stop("Every step must contain a standardised year-effect index. Retain an identifiable focus term in every model.", call. = FALSE)
  }
  if ("response" %in% names(indices) && length(unique(indices$response)) != 1L) {
    stop("Select one response when calculating multivariate diagnostics for a step sequence.", call. = FALSE)
  }
  components <- unique(indices$component)
  if (is.null(component)) {
    if (length(components) != 1L) {
      stop("Multiple index components are available; supply `component =` explicitly: ",
        paste(components, collapse = ", "), ".", call. = FALSE)
    }
    component <- components
  }
  if (!is.character(component) || length(component) != 1L || is.na(component) ||
      !component %in% components) {
    stop("The requested `component` is not available in every step.", call. = FALSE)
  }
  indices <- indices[indices$component == component, , drop = FALSE]
  if (anyDuplicated(indices$level) || any(!is.finite(indices$estimate)) ||
      length(unique(indices$scale)) != 1L) {
    stop("Each step must have one finite index estimate per focus level on one scale.", call. = FALSE)
  }
  if (identical(unique(indices$scale), "ratio") && any(indices$estimate <= 0)) {
    stop("Year-effect ratios must be positive.", call. = FALSE)
  }
  model <- if (precomputed) input$model else input
  formula <- if (is.null(model)) NA_character_ else {
    tryCatch(paste(deparse(stats::formula(model)), collapse = " "),
      error = function(e) NA_character_)
  }
  reference <- diagnostic$metadata$reference %||% "unknown"
  if (precomputed && !reference %in% c("observed", "observed; observed")) {
    stop("For step comparisons using an explicit reference grid, supply fitted models and common `reference_data`; a precomputed diagnostic does not retain enough information to verify that grid.", call. = FALSE)
  }
  indices$level <- as.character(indices$level)
  rownames(indices) <- NULL
  list(
    indices = indices,
    backend = diagnostic$backend,
    formula = formula,
    response = diagnostic$metadata$response,
    n_observations = diagnostic$metadata$n_observations,
    reference = reference,
    focus_composition = .step_focus_composition(diagnostic, component),
    rows = .step_model_rows(model, year, diagnostic$metadata$response, arguments$data),
    uncertainty = diagnostic$uncertainty,
    fit = if (keep_fits) model else NULL
  )
}

.check_step_payloads <- function(payloads) {
  first <- payloads[[1]]
  for (i in seq_along(payloads)[-1L]) {
    current <- payloads[[i]]
    fail <- function(what) stop("Step ", i, " has incompatible ", what,
      "; step plots require comparable year-effect contrasts.", call. = FALSE)
    if (!identical(first$response, current$response)) fail("response definitions")
    if (!setequal(first$indices$level, current$indices$level)) fail("focus levels (years)")
    if (!identical(unique(first$indices$scale), unique(current$indices$scale))) fail("index scales")
    if (!identical(unique(first$indices$component), unique(current$indices$component))) fail("index components")
    if (!identical(first$reference, current$reference)) fail("reference distributions")
    if (!identical(first$n_observations, current$n_observations)) fail("observation counts")
    if (!is.null(first$focus_composition) && !is.null(current$focus_composition) &&
        !isTRUE(all.equal(first$focus_composition, current$focus_composition, check.attributes = FALSE))) {
      fail("focus composition or reference weights")
    }
    if (!is.null(first$rows) && !is.null(current$rows)) {
      for (field in c("row", "focus", "response", "weights", "offset")) {
        a <- first$rows[[field]]
        b <- current$rows[[field]]
        if (!is.null(a) && !is.null(b) &&
            !isTRUE(all.equal(a, b, check.attributes = FALSE))) fail(paste("fitted data", field))
      }
    }
  }
  invisible(TRUE)
}

#' Calculate a sequence of refitted year-effect contrasts
#'
#' `influ_steps()` calculates and stores the indices needed for [plot_step()].
#' These are focus/year-effect contrasts, not area-integrated abundance indices.
#' Adding a spatial process changes the estimated year effects by refitting the
#' model; its field is not added to the plotted index.
#'
#' @param fits One supported fitted model, an `influ_diag`, or an ordered list
#'   of fitted models or diagnostics. An existing `influ_steps` is also accepted.
#' @param year Focus-variable name, inferred from the first input when omitted.
#' @param steps For refitting, an ordered, uniquely named list of cumulative
#'   formulas or lists of model-update arguments. Each specification updates the
#'   original model. Use `formula` in an argument list for its formula change.
#'   If omitted, ordinary single-component models start with the focus term and
#'   progressively add the remaining formula terms. Offsets are retained.
#'   Spatial models require explicit steps so field changes are deliberate.
#' @param refit Explicitly allow new model fitting. Defaults to `FALSE`.
#' @param labels Unique step labels. Defaults to list names or generated labels.
#' @param component Index component. Must be supplied if more than one is
#'   available, for example `"positive"` or `"unconditional_mean"` in a hurdle
#'   model. Even a combined-component index remains a focus-effect contrast,
#'   not the full expected response integrated over space.
#' @param probs Interval probabilities for newly calculated diagnostics.
#'   Existing diagnostics retain their original intervals.
#' @param keep_fits Retain fitted models in the result? Defaults to `FALSE` to
#'   keep the returned object small. Save expensive fits separately if needed.
#' @param refit_args Named arguments passed to the model update at every step,
#'   such as BRMS sampling controls. Stage-specific arguments take precedence.
#'   Execution-only controls (`seed`, `cores`, `refresh`, `silent`, and `verbose`)
#'   do not by themselves force an otherwise unchanged original fit to rerun.
#' @param ... Arguments passed to [influ()] for fitted-model inputs, such as
#'   `uncertainty = "none"`, `weights`, or `reference_data`. All steps use the
#'   same calculation arguments. Not used for model-fitting controls.
#'
#' @details
#' Plotting a stored result never refits models. `plot_step(model, refit = TRUE)`
#' is an explicit shortcut that calculates a sequence and immediately plots it.
#' Automatic refitting is restricted to supported formula structures. Supply
#' explicit steps or already-fitted models when a structure cannot be updated
#' safely. Refits use the original analysis rows and report convergence problems.
#' Native `update()` methods are used where available. `tinyVAST` has no such
#' method, so its refits reconstruct the recorded fitting call with the locked
#' data, stored spatial domain, and requested process settings.
#'
#' Each curve uses the centring and uncertainty supplied by [influ()]. No extra
#' plug-in rescaling is applied. With the same data and focus reference, all
#' steps use the same contrast definition. A change between steps depends on
#' the chosen order and is not a causal measure of variable importance.
#' Intervals describe each fitted model, not uncertainty in differences between
#' independently fitted models.
#'
#' Supplied fits are checked for matching fitted response and focus rows where
#' those are recoverable. Precomputed diagnostics only permit checks of retained
#' response, focus-composition, reference, and index summaries; identical raw
#' data cannot be established from those summaries alone. Refitted stages receive
#' backend convergence checks. A stage marked `supplied` records an existing
#' input, not independent confirmation of its convergence; check that fit before
#' including it. Compact posterior fixtures may not retain convergence records.
#'
#' @return An `influ_steps` object with compact `indices` and `steps` tables,
#'   `focus`, calculation `metadata`, and optional `fits`.
#' @md
#' @examples
#' data(lobsters_per_pot)
#' model <- glm(lobsters ~ year + month, family = poisson(),
#'              data = lobsters_per_pot)
#' steps <- influ_steps(model, year = "year", refit = TRUE)
#' steps
#' plot_step(steps)
#' @export
influ_steps <- function(fits, year = NULL, steps = NULL, refit = FALSE,
                        labels = NULL, component = NULL,
                        probs = c(0.025, 0.975), keep_fits = FALSE,
                        refit_args = list(), ...) {
  if (!is.logical(refit) || length(refit) != 1L || is.na(refit) ||
      !is.logical(keep_fits) || length(keep_fits) != 1L || is.na(keep_fits)) {
    stop("`refit` and `keep_fits` must each be TRUE or FALSE.", call. = FALSE)
  }
  arguments <- list(...)
  if (length(arguments) && (is.null(names(arguments)) || any(!nzchar(names(arguments))) ||
      anyDuplicated(names(arguments)))) {
    stop("Influence arguments must have unique names.", call. = FALSE)
  }
  if (any(names(arguments) %in% c("model", "focus", "keep_model", "rescale", "rescale_series"))) {
    stop("Do not override model/focus/retention or apply display rescaling to a step sequence; use a common influence reference instead.", call. = FALSE)
  }
  if (inherits(fits, "influ_steps")) {
    if (refit || !is.null(steps) || length(arguments) || length(refit_args) ||
        !is.null(component) || (!is.null(year) && !identical(year, fits$focus))) {
      stop("A stored influ_steps object cannot be refitted or recalculated; supply the original fitted model.", call. = FALSE)
    }
    if (!is.null(labels)) {
      labels <- .step_label_vector(labels, nrow(fits$steps))
      fits$steps$label <- labels
      fits$indices$Model <- labels[fits$indices$step_id]
    }
    return(fits)
  }
  probs <- .validate_probs(probs)
  if (!is.null(labels)) .step_label_vector(labels, length(labels))
  if (!refit && (!is.null(steps) || length(refit_args))) {
    stop("Supply `refit = TRUE` to request new model fits.", call. = FALSE)
  }
  single <- .is_step_model(fits) || inherits(fits, "influ_diag")
  if (refit && (!single || inherits(fits, "influ_diag"))) {
    stop("Refitting requires one original fitted model, not a list or diagnostic.", call. = FALSE)
  }
  inputs <- if (single) list(fits) else fits
  if (!is.list(inputs) || !length(inputs) || !all(vapply(inputs, function(x) {
    .is_step_model(x) || inherits(x, "influ_diag")
  }, logical(1)))) {
    stop("`fits` must contain supported fitted models or influence diagnostics.", call. = FALSE)
  }
  if (is.null(year)) {
    year <- if (inherits(inputs[[1]], "influ_diag")) inputs[[1]]$focus else {
      formula <- .step_main_formula(inputs[[1]])
      all.vars(formula[[3L]])[1L]
    }
  }
  if (!is.character(year) || length(year) != 1L || is.na(year) || !nzchar(year)) {
    stop("Supply one focus-variable name with `year =`.", call. = FALSE)
  }
  if (refit && "data" %in% names(arguments)) {
    stop("Refitted steps lock the original analysis rows; do not supply different `data` through influence arguments.", call. = FALSE)
  }
  baseline <- NULL
  process <- function(model, data = NULL) {
    step_arguments <- arguments
    if (!is.null(data)) step_arguments$data <- data
    payload <- .step_payload(model, year, component, probs, step_arguments, keep_fits)
    if (is.null(baseline)) baseline <<- payload else {
      .check_step_payloads(list(baseline, payload))
    }
    payload$rows_checked <- !is.null(payload$rows) && !is.null(baseline$rows)
    # Retain only one raw-data signature while processing a sequence. The
    # returned object contains annual summaries, not one row array per model.
    payload["rows"] <- list(NULL)
    payload
  }
  if (refit) {
    result <- .step_refit_models(fits, year = year, steps = steps,
      refit_args = refit_args, process = process)
    payloads <- result$fits
    stage_table <- result$steps
    if (is.null(labels)) labels <- stage_table$label
  } else {
    payloads <- lapply(inputs, process)
    if (is.null(labels)) labels <- names(inputs)
    if (is.null(labels)) labels <- paste0("Step ", seq_along(payloads))
    stage_table <- data.frame(
      label = labels,
      formula = vapply(payloads, `[[`, character(1), "formula"),
      backend = vapply(payloads, `[[`, character(1), "backend"),
      status = "supplied",
      refitted = FALSE,
      stringsAsFactors = FALSE
    )
  }
  labels <- .step_label_vector(labels, length(payloads))
  .check_step_payloads(payloads)
  stage_table$label <- labels
  stage_table$step_id <- seq_along(payloads)
  reference_levels <- payloads[[1]]$indices$level
  indices <- do.call(rbind, lapply(seq_along(payloads), function(i) {
    data <- payloads[[i]]$indices
    data <- data[match(reference_levels, data$level), , drop = FALSE]
    data$step_id <- i
    data$Model <- labels[i]
    data
  }))
  rownames(indices) <- NULL
  out <- list(
    indices = indices,
    steps = stage_table,
    focus = year,
    metadata = list(
      estimand = "year_effect_contrast",
      mode = if (refit) "refitted" else "supplied",
      response = payloads[[1]]$response,
      component = unique(indices$component),
      scale = unique(indices$scale),
      reference = payloads[[1]]$reference,
      normalisation = "influence reference; no additional display rescaling",
      uncertainty = lapply(payloads, `[[`, "uncertainty"),
      validation = if (all(vapply(payloads, `[[`, logical(1), "rows_checked"))) {
        "fitted response and focus rows plus diagnostic summaries"
      } else "retained diagnostic summaries only",
      cross_model_difference_intervals = FALSE
    ),
    fits = if (keep_fits) stats::setNames(lapply(payloads, `[[`, "fit"), labels) else NULL
  )
  class(out) <- "influ_steps"
  out
}

#' @rdname influ_steps
#' @param x,object An `influ_steps` object.
#' @export
print.influ_steps <- function(x, ...) {
  cat("<influ_steps>\n", "  Estimand: year-effect contrasts (not spatial abundance)\n",
    "  Focus: ", x$focus, "\n", "  Steps: ", nrow(x$steps), "\n",
    "  Refitted: ", sum(x$steps$refitted), "\n", sep = "")
  print(x$steps[c("step_id", "label", "backend", "status")], row.names = FALSE)
  invisible(x)
}

#' @rdname influ_steps
#' @export
summary.influ_steps <- function(object, ...) {
  object$steps
}

#' Plot a stored step sequence
#'
#' @param x,object An `influ_steps` object from [influ_steps()].
#' @param ... Plotting arguments passed to [plot_step()], such as `fill` or
#'   `show_probs`. Plotting a stored sequence never refits models.
#' @return A [ggplot2::ggplot()] object.
#' @md
#' @name plot.influ_steps
NULL

#' @rdname plot.influ_steps
#' @export
plot.influ_steps <- function(x, ...) {
  plot_step(x, ...)
}

#' @rdname plot.influ_steps
#' @export
autoplot.influ_steps <- function(object, ...) {
  plot_step(object, ...)
}
