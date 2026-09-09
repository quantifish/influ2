.check_residual_model <- function(model, type = NULL) {
  if (!is.null(type) && (!is.character(type) || length(type) != 1L ||
      is.na(type) || !nzchar(type))) {
    stop("`type` must name one residual type supported by the fitted model.",
      call. = FALSE)
  }
  if (inherits(model, "brmsfit")) {
    if (!requireNamespace("brms", quietly = TRUE)) {
      stop("Package 'brms' is required for brms residuals.", call. = FALSE)
    }
    if (is.null(model$fit) && !is.null(model$influ2_draws)) {
      stop(
        "Compact brms influence fixtures do not contain the native fitted ",
        "object required for prediction and residuals. Supply the original ",
        "complete brmsfit; influ() can still use these compact fixtures.",
        call. = FALSE
      )
    }
    if (inherits(model$formula, "mvbrmsformula")) {
      stop("These residual plots require one response. Use the native brms ",
        "residual interface with an explicit `resp` for multivariate models.",
        call. = FALSE)
    }
  }
  if (inherits(model, "sdmTMB") && isTRUE(model$family$delta)) {
    stop(
      "sdmTMB delta residuals require an explicit model component, whereas ",
      "fitted() returns the unconditional mean. Use native residuals(fit, ",
      "model = 1) or residuals(fit, model = 2) with matching component ",
      "predictions; these plots do not yet select delta components.",
      call. = FALSE
    )
  }
  if (inherits(model, "tinyVAST")) {
    variable <- model$internal$variable_column %||% "var"
    responses <- unique(as.character(model$data[[variable]]))
    if (length(responses) > 1L || length(model$internal$family) > 1L) {
      stop("These residual plots require a single tinyVAST response and ",
        "distribution. Inspect native residuals separately by response.",
        call. = FALSE)
    }
    if (!is.null(type) && !type %in% c("deviance", "response")) {
      stop("tinyVAST does not provide this residual type. Use an explicitly ",
        "supported native type, such as `type = \"deviance\"`; residual ",
        "types are not substituted automatically.", call. = FALSE)
    }
  }
  invisible(NULL)
}

.observation_estimate <- function(value, label, summary = FALSE) {
  if (length(dim(value)) > 2L) {
    stop(label, " must contain one value per observation, not a ",
      "multidimensional response array.", call. = FALSE)
  }
  if (is.matrix(value) || is.data.frame(value)) {
    column <- if (summary && "Estimate" %in% colnames(value)) {
      "Estimate"
    } else if (ncol(value) == 1L) {
      1L
    } else {
      stop(label, " must contain one value per observation; an ambiguous ",
        "multi-column result cannot be reduced automatically.", call. = FALSE)
    }
    observation_names <- rownames(value)
    value <- value[, column, drop = TRUE]
    names(value) <- observation_names
  }
  if (!is.numeric(value) || !length(value) || !any(is.finite(value))) {
    stop(label, " did not contain any finite observation values. Check ",
      "whether the fitted model's native method supports the requested ",
      "residual type and family.", call. = FALSE)
  }
  stats::setNames(as.numeric(value), names(value))
}

.residual_model_frame <- function(model) {
  # model.frame.glm() can reevaluate a saved call when model = FALSE. That
  # would validate against mutable external data, not the observations fitted.
  frame <- if (inherits(model, c("glm", "gam"))) {
    model$model
  } else if (inherits(model, "glmmTMB")) {
    model$frame
  } else if (inherits(model, c("brmsfit", "sdmTMB", "tinyVAST"))) {
    model$data
  } else {
    tryCatch(stats::model.frame(model), error = function(e) NULL)
  }
  if (is.null(frame)) return(NULL)
  as.data.frame(frame)
}

.align_observation_values <- function(value, frame, label, model) {
  if (is.null(frame)) return(value)
  ids <- rownames(frame)
  value_ids <- names(value)
  if (!is.null(value_ids) && !anyDuplicated(value_ids) &&
      all(ids %in% value_ids)) {
    return(value[match(ids, value_ids)])
  }
  if (length(value) == nrow(frame)) return(value)
  omitted <- model$na.action
  if (inherits(omitted, "exclude") &&
      length(value) == nrow(frame) + length(omitted)) {
    return(value[-as.integer(omitted)])
  }
  stop(label, " could not be aligned with the rows used to fit the model.",
    call. = FALSE)
}

.same_observation_column <- function(x, y, tolerance = 0) {
  if (!identical(dim(x), dim(y))) return(FALSE)
  if (is.factor(x) || is.factor(y)) {
    x <- as.character(x)
    y <- as.character(y)
  }
  isTRUE(all.equal(x, y, check.attributes = FALSE, tolerance = tolerance))
}

.residual_observations <- function(model, data, residual) {
  frame <- .residual_model_frame(model)
  if (is.null(frame)) {
    stop("The fitted observation rows could not be recovered safely for ",
      "implied residuals. A retained model frame is required; fit GLMs with ",
      "`model = TRUE` rather than reconstructing rows from a saved call.",
      call. = FALSE)
  }
  data <- .resolve_influ_data(model, data)
  ids <- rownames(frame)
  if (anyDuplicated(rownames(data)) || !all(ids %in% rownames(data))) {
    stop("`data` must retain the original row names for all observations ",
      "used to fit the model, so residuals can be aligned safely.", call. = FALSE)
  }
  data <- data[match(ids, rownames(data)), , drop = FALSE]
  common <- intersect(names(frame), names(data))
  if (!length(common) || any(!vapply(common, function(name) {
    .same_observation_column(frame[[name]], data[[name]])
  }, logical(1)))) {
    stop("`data` does not match the fitted model frame at its original row ",
      "names. Supply the original model data without changing observation ",
      "values or row identifiers.", call. = FALSE)
  }
  # Recover transformed predictors using the model's saved prediction terms,
  # when available. This also catches changes hidden inside poly(), log(), etc.
  reconstructed <- tryCatch(stats::model.frame(
    stats::terms(model), data = data, na.action = stats::na.pass,
    xlev = model$xlevels
  ), error = function(e) NULL)
  if (!is.null(reconstructed) && nrow(reconstructed) == nrow(frame)) {
    common <- intersect(names(frame), names(reconstructed))
    if (any(!vapply(common, function(name) {
      # Reapplying saved polynomial bases can differ at roundoff precision.
      # Raw fitted columns have already been checked exactly above.
      .same_observation_column(frame[[name]], reconstructed[[name]],
        tolerance = 1e-10)
    }, logical(1)))) {
      stop("`data` does not reproduce the fitted model frame. Supply the ",
        "original model data without changing observation values.", call. = FALSE)
    }
  }
  residual <- .align_observation_values(residual, frame, "Residuals", model)
  if (length(residual) != nrow(data)) {
    stop("The model returned a different number of residuals than fitted ",
      "data rows.", call. = FALSE)
  }
  list(data = data, residual = residual)
}
