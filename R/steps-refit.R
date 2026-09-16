.step_backend <- function(model) {
  classes <- c("brmsfit", "tinyVAST", "sdmTMB", "glmmTMB", "gam", "glm", "lm")
  found <- classes[vapply(classes, function(x) inherits(model, x), logical(1))]
  if (!length(found)) stop("Step refitting does not support this model class.", call. = FALSE)
  found[1]
}

.step_main_formula <- function(model, backend = .step_backend(model)) {
  formula <- if (backend == "brmsfit") model$formula$formula else {
    if (backend %in% c("tinyVAST", "sdmTMB")) model$formula else stats::formula(model)
  }
  if (backend == "sdmTMB" && is.list(formula) && length(formula) == 1L) {
    formula <- formula[[1L]]
  }
  if (backend == "sdmTMB" && isTRUE(model$family$delta) &&
      is.list(formula) && length(formula) == 2L &&
      all(vapply(formula, function(f) inherits(f, "formula") && length(f) == 3L, logical(1)))) {
    return(formula)
  }
  if (!inherits(formula, "formula") || length(formula) != 3L) {
    stop("Step refitting requires one two-sided main formula; multivariate models need separately fitted steps.", call. = FALSE)
  }
  formula
}

.step_formulas <- function(formula) if (inherits(formula, "formula")) list(formula) else formula
.step_formula_env <- function(formula) environment(.step_formulas(formula)[[1L]])
.step_formula_vars <- function(formula) unique(unlist(lapply(.step_formulas(formula), all.vars)))
.step_formula_text <- function(formula) paste(vapply(.step_formulas(formula),
  function(f) paste(deparse(f, width.cutoff = 500L), collapse = " "), character(1)), collapse = " ; ")

.step_year_terms <- function(formula, year, year_term = NULL) {
  forms <- .step_formulas(formula)
  if (is.null(year_term)) year_term <- rep(year, length(forms))
  if (!is.character(year_term) || length(year_term) != length(forms) ||
      anyNA(year_term) || any(!nzchar(year_term))) {
    stop("`year_term` must name one annual predictor per formula, in component order.", call. = FALSE)
  }
  if (length(forms) == 2L && !is.null(names(year_term))) {
    if (!setequal(names(year_term), c("occurrence", "positive"))) {
      stop("Name paired `year_term` entries 'occurrence' and 'positive'.", call. = FALSE)
    }
    year_term <- year_term[c("occurrence", "positive")]
  }
  for (k in seq_along(forms)) .step_year_term(forms[[k]], year_term[[k]])
  unname(year_term)
}

.step_update_formula <- function(original, supplied, year_term) {
  forms <- .step_formulas(original)
  updates <- .step_formulas(supplied)
  if (!is.list(updates) || length(updates) != length(forms) ||
      !all(vapply(updates, inherits, logical(1), "formula"))) {
    stop("Every step formula must be an R formula object, or a paired list matching the delta components.", call. = FALSE)
  }
  result <- lapply(seq_along(forms), function(k) {
    f <- stats::update.formula(forms[[k]], updates[[k]])
    if (!identical(f[[2L]], forms[[k]][[2L]])) {
      stop("Step refits must retain the original response.", call. = FALSE)
    }
    original_offsets <- .step_offset_terms(forms[[k]])
    stage_offsets <- .step_offset_terms(f)
    if (length(setdiff(stage_offsets, original_offsets))) {
      stop("Step formulas must preserve the original offset expressions; changing effort offsets requires separately fitted models.", call. = FALSE)
    }
    offsets <- setdiff(original_offsets, stage_offsets)
    if (length(offsets)) f <- stats::update.formula(f,
      stats::as.formula(paste("~ . +", paste(offsets, collapse = " + "))))
    environment(f) <- environment(forms[[k]])
    .step_year_term(f, year_term[[k]])
    f
  })
  if (inherits(original, "formula")) result[[1L]] else result
}

.step_settings_text <- function(args) {
  if (!length(args)) return("Original fitting settings")
  values <- vapply(args, function(value) {
    if (as.numeric(utils::object.size(value)) > 1500 || is.environment(value)) {
      return(paste0("<", paste(class(value), collapse = "/"), ", length ", length(value), ">"))
    }
    text <- paste(deparse(value, width.cutoff = 80L), collapse = " ")
    if (nchar(text) > 160L) paste0(substr(text, 1L, 157L), "...") else text
  }, character(1))
  paste(paste0(names(args), " = ", values), collapse = "; ")
}

.step_offset_terms <- function(formula) {
  terms <- stats::terms(formula, keep.order = TRUE)
  indices <- attr(terms, "offset") %||% integer()
  vapply(indices, function(i) {
    paste(deparse(attr(terms, "variables")[[i + 1L]]), collapse = " ")
  }, character(1))
}

.step_year_term <- function(formula, year) {
  terms <- attr(stats::terms(formula, keep.order = TRUE), "term.labels")
  uses_year <- vapply(terms, function(term) {
    year %in% all.vars(stats::as.formula(paste("~", term)))
  }, logical(1))
  year_terms <- terms[uses_year]
  simple <- vapply(year_terms, function(term) {
    expression <- stats::as.formula(paste("~", term))[[2L]]
    is.symbol(expression) ||
      (is.call(expression) && as.character(expression[[1]]) %in% c("factor", "as.factor") &&
        length(expression) == 2L && is.symbol(expression[[2L]]))
  }, logical(1))
  if (length(year_terms) != 1L || !all(simple)) {
    stop("Every refit formula must retain one additive year term (year or factor(year)); year interactions require an explicitly defined marginalisation outside this interface.", call. = FALSE)
  }
  year_terms
}

.step_auto_formulas <- function(model, formula, year, backend) {
  if (backend %in% c("sdmTMB", "tinyVAST")) {
    stop("Spatial refits require explicit `steps` specifying which spatial and spatiotemporal structures each stage retains.", call. = FALSE)
  }
  if (backend == "brmsfit" &&
      (length(model$formula$pforms) || isTRUE(model$formula$nl))) {
    stop("Distributional or nonlinear brms models require explicit `steps`.", call. = FALSE)
  }
  if (backend == "glmmTMB") {
    forms <- model$modelInfo$allForm
    other <- forms[c("ziformula", "dispformula")]
    if (any(vapply(other, function(f) {
      inherits(f, "formula") && length(attr(stats::terms(f), "term.labels")) > 0L
    }, logical(1)))) {
      stop("Distributional glmmTMB models require explicit `steps`.", call. = FALSE)
    }
  }
  year_term <- .step_year_term(formula, year)
  terms <- stats::terms(formula, keep.order = TRUE)
  remaining <- setdiff(attr(terms, "term.labels"), year_term)
  offsets <- .step_offset_terms(formula)
  response <- paste(deparse(formula[[2L]]), collapse = " ")
  out <- lapply(seq.int(0L, length(remaining)), function(i) {
    selected <- c(year_term, utils::head(remaining, i), offsets)
    stats::reformulate(paste0("(", selected, ")"), response = response,
      intercept = attr(terms, "intercept"), env = environment(formula))
  })
  names(out) <- c("Year only", paste0("Add ", remaining))
  out
}

.step_locked_data <- function(model, formula, backend) {
  frame <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  if (!is.data.frame(frame)) frame <- NULL
  original <- tryCatch(eval(model$call$data, envir = .step_formula_env(formula)), error = function(e) NULL)
  if (backend %in% c("brmsfit", "sdmTMB", "tinyVAST")) original <- model$data
  if (!is.data.frame(original)) original <- model$data
  if (!is.data.frame(original)) original <- frame
  if (!is.data.frame(original)) {
    stop("Cannot recover the original analysis rows for refitting; supply separately fitted steps.", call. = FALSE)
  }
  original <- as.data.frame(original)
  if (backend %in% c("brmsfit", "sdmTMB", "tinyVAST")) {
    used <- as.data.frame(model$data)
  } else {
    if (is.null(frame)) stop("The stored model frame is required to lock the refit rows.", call. = FALSE)
    used <- frame
  }
  index <- match(row.names(used), row.names(original))
  if (anyNA(index) || anyDuplicated(index)) {
    stop("Could not align the fitted rows with the original data; supply separately fitted steps.", call. = FALSE)
  }
  data <- original[index, , drop = FALSE]
  # Prefer original fitted values when the caller's data object has since
  # changed. Transformed-only variables require a separate matrix check below.
  raw_columns <- intersect(names(used), names(data))
  for (name in raw_columns) data[[name]] <- used[[name]]
  required <- .step_formula_vars(formula)
  if (!all(required %in% names(data))) {
    stop("The original variables needed for refitting are unavailable in the stored data.", call. = FALSE)
  }
  if (backend %in% c("lm", "glm", "gam", "glmmTMB")) {
    compare <- function(x, y) isTRUE(all.equal(unname(as.matrix(x)), unname(as.matrix(y)),
      tolerance = 1e-9, check.attributes = FALSE))
    matrices_match <- tryCatch({
      if (backend == "gam") {
        compare(stats::predict(model, type = "lpmatrix"),
          stats::predict(model, newdata = data, type = "lpmatrix"))
      } else if (backend == "glmmTMB") {
        all(vapply(c("cond", "zi", "disp"), function(component) {
          compare(stats::model.matrix(model, component = component),
            stats::model.matrix(model, newdata = data, component = component))
        }, logical(1)))
      } else compare(stats::model.matrix(model), .glm_reference_matrix(model, data))
    }, error = function(e) FALSE)
    response <- tryCatch(eval(formula[[2L]], envir = data, enclos = environment(formula)),
      error = function(e) NULL)
    offsets_match <- all(vapply(.step_offset_terms(formula), function(term) {
      value <- tryCatch(eval(str2lang(term), envir = data, enclos = environment(formula)),
        error = function(e) NULL)
      !is.null(value) && !is.null(frame[[term]]) && compare(value, frame[[term]])
    }, logical(1)))
    if (!matrices_match || is.null(response) ||
        !compare(stats::model.response(frame), response) || !offsets_match) {
      stop("The source data no longer reproduce the fitted model; supply the original data or separately fitted steps.", call. = FALSE)
    }
  }
  # A stored frame establishes both fitted row order and the exact fitting
  # weights, including binomial trial weights before glm's internal expansion.
  weights <- if (!is.null(frame)) stats::model.weights(frame) else NULL
  if (backend == "sdmTMB") {
    # Native vectors are authoritative, including character-column offsets
    # and saved calls whose original fitting environment no longer exists.
    native_weights <- as.numeric(model$tmb_data$weights_i)
    if (length(native_weights) == 2L * nrow(data) && isTRUE(model$family$delta)) {
      if (!identical(native_weights[seq_len(nrow(data))],
          native_weights[nrow(data) + seq_len(nrow(data))])) {
        stop("Step refits cannot reproduce different likelihood weights between delta components.", call. = FALSE)
      }
      native_weights <- native_weights[seq_len(nrow(data))]
    }
    if (length(native_weights) != nrow(data) || length(model$offset) != nrow(data)) {
      stop("Native sdmTMB weights and offset must align with the fitted rows.", call. = FALSE)
    }
    return(list(data = data, weights = native_weights, offset = as.numeric(model$offset)))
  }
  evaluate_vector <- function(expression, name) {
    value <- tryCatch(eval(expression, envir = original, enclos = environment(formula)),
      error = function(e) stop("Cannot recover original ", name, " for locked refits.", call. = FALSE))
    if (length(value) == 1L) value <- rep(value, nrow(original))
    if (length(value) != nrow(original)) {
      stop("Cannot align original ", name, " with the refit analysis rows.", call. = FALSE)
    }
    value[index]
  }
  if (is.null(weights) && !is.null(model$call$weights)) {
    weights <- evaluate_vector(model$call$weights, "weights")
  }
  offset <- if (is.null(model$call$offset)) NULL else {
    if (!is.null(frame[["(offset)"]])) frame[["(offset)"]] else {
      evaluate_vector(model$call$offset, "offset")
    }
  }
  list(data = data, weights = weights, offset = offset)
}

.step_check_convergence <- function(model, backend, label) {
  fail <- function(message) stop("Step '", label, "' ", message, call. = FALSE)
  if (backend %in% c("glm", "gam") && !isTRUE(model$converged)) {
    fail("did not converge.")
  }
  if (inherits(model, "negbin") && !is.null(model$th.warn)) {
    fail(paste0("has incomplete negative-binomial dispersion convergence: ",
      paste(model$th.warn, collapse = "; "), "."))
  }
  if (inherits(model, "negbin") &&
      (length(model$theta) != 1L || !is.finite(model$theta) || model$theta <= 0)) {
    fail("does not have a finite positive negative-binomial dispersion estimate.")
  }
  if (backend == "gam" && !is.null(model$outer.info$conv) &&
      !identical(model$outer.info$conv, "full convergence")) {
    fail(paste0("has incomplete GAM outer convergence: ", model$outer.info$conv, "."))
  }
  if (backend %in% c("glmmTMB", "sdmTMB", "tinyVAST")) {
    optimisation <- switch(backend, glmmTMB = model$fit, sdmTMB = model$model, tinyVAST = model$opt)
    report <- switch(backend, glmmTMB = model$sdr, sdmTMB = model$sd_report, tinyVAST = model$sdrep)
    if (!is.null(optimisation$convergence) && optimisation$convergence != 0L) {
      fail("has an unsuccessful optimiser convergence code.")
    }
    if (!isTRUE(report$pdHess)) fail("does not have a positive-definite Hessian.")
  }
  if (backend == "brmsfit") {
    rhats <- tryCatch(brms::rhat(model), error = function(e) NULL)
    if (is.null(rhats) || !any(is.finite(rhats))) {
      fail("has no available MCMC convergence diagnostics; use a complete fitted brms model.")
    }
    if (any(rhats > 1.01, na.rm = TRUE)) fail("has R-hat above 1.01.")
    nuts <- tryCatch(brms::nuts_params(model), error = function(e) NULL)
    if (!is.null(nuts) && any(nuts$Parameter == "divergent__" & nuts$Value > 0)) {
      fail("has divergent posterior transitions.")
    }
  }
  invisible(model)
}

.step_validate_refit_args <- function(args) {
  if (!is.list(args) || (length(args) &&
      (is.null(names(args)) || any(!nzchar(names(args))) || anyDuplicated(names(args))))) {
    stop("Refit arguments must be a uniquely named list.", call. = FALSE)
  }
  unsafe <- intersect(names(args), c("data", "newdata", "subset", "weights", "offset",
    "na.action", "evaluate", "testmode", "file", "file_refit", "fit", "previous_fit"))
  if (length(unsafe)) {
    stop("Locked refits do not allow overriding: ", paste(unsafe, collapse = ", "), ".", call. = FALSE)
  }
  if (identical(args$recompile, FALSE)) {
    stop("brms refits must allow automatic recompilation; `recompile = FALSE` is unsafe after formula changes.", call. = FALSE)
  }
  args
}

.step_refit_one <- function(model, formula, args, locked, backend) {
  if (backend == "sdmTMB") {
    # update.sdmTMB calls update.formula() on formula lists, which is invalid.
    # Reconstruct the recorded call using retained data and mesh instead.
    call <- model$call
    if (is.null(call)) stop("sdmTMB refitting requires its original call.", call. = FALSE)
    call[[1L]] <- quote(sdmTMB::sdmTMB)
    call$formula <- formula
    call$data <- locked$data
    call$mesh <- model$spde
    call$family <- model$family
    call$offset <- locked$offset
    call$weights <- locked$weights
    call$previous_fit <- NULL
    for (name in c("time", "spatial", "spatiotemporal", "reml", "control", "priors", "extra_time")) {
      if (!is.null(model[[name]])) call[name] <- list(model[[name]])
    }
    for (name in names(args)) call[name] <- list(args[[name]])
    return(eval(call, envir = .step_formula_env(formula)))
  }
  if (backend == "brmsfit") {
    model$file <- NULL
    model$stan_args$file <- NULL
    model$influ2_draws <- NULL
    args$file <- NULL
    args$file_refit <- "always"
    args$recompile <- args$recompile %||% NULL
    return(do.call(stats::update, c(list(object = model, formula. = formula,
      newdata = locked$data), args)))
  }
  if (backend == "tinyVAST") {
    # tinyVAST has no update method. Reconstruct its recorded call while
    # retaining the stored domain and injecting actual analysis data values.
    call <- model$call
    if (is.null(call)) stop("tinyVAST refitting requires its original call.", call. = FALSE)
    call[[1L]] <- quote(tinyVAST::tinyVAST)
    call$formula <- formula
    call$data <- locked$data
    call$spatial_domain <- model$spatial_domain
    call$weights <- locked$weights
    saved <- model$internal
    if (!is.null(saved)) {
      for (name in c("time_term", "space_term", "spacetime_term", "family",
          "spatial_varying", "control", "development", "space_columns",
          "time_column", "times", "variable_column", "variables", "distribution_column")) {
        if (name %in% names(saved)) call[name] <- list(saved[[name]])
      }
    }
    for (name in names(args)) call[name] <- list(args[[name]])
    return(eval(call, envir = environment(formula)))
  }
  # Saved fits may have recorded an unqualified call while their fitting
  # package was attached. Resolve the known backend without attaching it.
  if (!is.null(model$call)) {
    model$call[[1L]] <- switch(backend,
      lm = quote(stats::lm),
      glm = if (inherits(model, "negbin")) quote(MASS::glm.nb) else quote(stats::glm),
      gam = if (inherits(model, "bam")) quote(mgcv::bam) else quote(mgcv::gam),
      glmmTMB = quote(glmmTMB::glmmTMB), sdmTMB = quote(sdmTMB::sdmTMB))
  }
  extras <- c(list(data = locked$data), args)
  if (backend %in% c("glm", "lm", "gam", "glmmTMB")) {
    extras$subset <- NULL
    # NULL must be passed explicitly to erase the original subset expression.
    extras <- c(extras, list(subset = NULL, na.action = stats::na.fail))
  }
  extras <- c(extras, list(weights = locked$weights))
  # glm.nb re-estimates theta from its saved starting value and accepts
  # offsets in the formula only, not as an extra glm.control argument.
  if (!inherits(model, "negbin")) {
    extras <- c(extras, list(offset = locked$offset))
  }
  do.call(stats::update, c(list(object = model, formula. = formula), extras))
}

.step_refit_models <- function(model, year, steps = NULL, refit_args = list(),
                                process = NULL, year_term = NULL) {
  backend <- .step_backend(model)
  formula <- .step_main_formula(model, backend)
  if (!is.character(year) || length(year) != 1L || !nzchar(year)) {
    stop("`year` must name the temporal variable for step refitting.", call. = FALSE)
  }
  year_terms <- .step_year_terms(formula, year, year_term)
  locked <- .step_locked_data(model, formula, backend)
  if (!year %in% names(locked$data)) stop("`year` is absent from the fitted data.", call. = FALSE)
  refit_args <- .step_validate_refit_args(refit_args)
  if (any(c("formula", "formula.") %in% names(refit_args))) {
    stop("Specify formulas in `steps`, rather than `refit_args`.", call. = FALSE)
  }
  if (is.null(steps)) steps <- .step_auto_formulas(model, formula, year, backend)
  if (!is.list(steps) || !length(steps) || is.null(names(steps)) ||
      anyNA(names(steps)) || any(!nzchar(names(steps))) || anyDuplicated(names(steps))) {
    stop("`steps` must be a non-empty, uniquely named list of formulas or update-argument lists.", call. = FALSE)
  }
  if (!is.null(process) && !is.function(process)) stop("`process` must be a function.", call. = FALSE)
  same_formula <- function(x) {
    full <- .step_formulas(formula)
    stage <- .step_formulas(x)
    all(vapply(seq_along(full), function(k) {
      tx <- stats::terms(stage[[k]], keep.order = TRUE)
      original <- stats::terms(full[[k]], keep.order = TRUE)
      identical(attr(tx, "term.labels"), attr(original, "term.labels")) &&
        identical(attr(tx, "intercept"), attr(original, "intercept")) &&
        identical(.step_offset_terms(stage[[k]]), .step_offset_terms(full[[k]]))
    }, logical(1)))
  }
  same_args <- function(args) {
    # Execution controls govern new fits; they do not change the model whose
    # already fitted full stage can be reused.
    args <- args[setdiff(names(args), c("seed", "cores", "refresh", "silent", "verbose"))]
    all(vapply(names(args), function(name) {
      if (backend == "tinyVAST" && name %in% names(model$internal)) {
        return(identical(args[[name]], model$internal[[name]]))
      }
      if (backend == "tinyVAST" && name == "spatial_domain") {
        return(identical(args[[name]], model$spatial_domain))
      }
      if (is.null(model$call[[name]])) return(is.null(args[[name]]))
      original <- model$call[[name]]
      # A live symbol may now point to different settings. Only recorded
      # literal values, or saved backend settings above, establish equality.
      if (!is.atomic(original)) return(FALSE)
      identical(args[[name]], original)
    }, logical(1)))
  }
  fits <- vector("list", length(steps))
  names(fits) <- names(steps)
  metadata <- vector("list", length(steps))
  keys <- vector("list", length(steps))
  for (i in seq_along(steps)) {
    specification <- steps[[i]]
    if (inherits(specification, "formula")) specification <- list(formula = specification)
    if (is.list(specification) && length(specification) == 2L &&
        all(vapply(specification, inherits, logical(1), "formula")) && is.null(names(specification))) {
      specification <- list(formula = specification)
    }
    specification <- .step_validate_refit_args(specification)
    if (all(c("formula", "formula.") %in% names(specification))) {
      stop("Each step must use only one of `formula` and `formula.`.", call. = FALSE)
    }
    supplied_formula <- specification$formula %||% specification$formula. %||% formula
    specification$formula <- specification$formula. <- NULL
    stage_formula <- .step_update_formula(formula, supplied_formula, year_terms)
    if (!all(.step_formula_vars(stage_formula) %in% names(locked$data))) {
      stop("Step formulas must use variables available in the locked analysis data.", call. = FALSE)
    }
    args <- utils::modifyList(refit_args, specification, keep.null = TRUE)
    key <- list(formula = .step_formula_text(stage_formula), args = args)
    duplicate <- which(vapply(keys[seq_len(i - 1L)], identical, logical(1), y = key))
    reuse_original <- same_formula(stage_formula) && same_args(args)
    warning_messages <- character()
    if (length(duplicate)) {
      fits[[i]] <- fits[[duplicate[1L]]]
      status <- "reused step"
    } else {
      fit <- if (reuse_original) model else {
        withCallingHandlers(
          tryCatch(.step_refit_one(model, stage_formula, args, locked, backend),
            error = function(e) stop("Could not refit step '", names(steps)[i], "': ", conditionMessage(e), call. = FALSE)),
          warning = function(w) warning_messages <<- c(warning_messages, conditionMessage(w))
        )
      }
      .step_check_convergence(fit, backend, names(steps)[i])
      n_used <- if (backend %in% c("brmsfit", "tinyVAST", "sdmTMB")) nrow(fit$data) else nrow(stats::model.frame(fit))
      if (!identical(as.integer(n_used), as.integer(nrow(locked$data)))) {
        stop("Step '", names(steps)[i], "' changed the analysis rows.", call. = FALSE)
      }
      fits[[i]] <- if (is.null(process)) fit else {
        if ("data" %in% names(formals(process))) process(fit, data = locked$data) else process(fit)
      }
      rm(fit)
      status <- if (reuse_original) "reused original" else "refitted"
    }
    keys[[i]] <- key
    metadata[[i]] <- data.frame(label = names(steps)[i], formula = .step_formula_text(stage_formula),
      backend = backend, status = status, refitted = status == "refitted",
      settings = .step_settings_text(args),
      warnings = paste(unique(warning_messages), collapse = "; "), stringsAsFactors = FALSE)
  }
  list(fits = fits, steps = do.call(rbind, metadata), data = locked$data)
}
