# Search refits lock the likelihood and rows without the step plot's additive
# year restriction. Random terms can change only through explicit formulas.
.search_equal <- function(x, y) isTRUE(all.equal(x, y,
  tolerance = 1e-10, check.attributes = FALSE))

.search_live_objective <- function(object) {
  dll <- object$env$DLL
  is.character(dll) && length(dll) == 1L && dll %in% names(getLoadedDLLs()) &&
    .check_fit_live_objective(object)
}

.search_copy_family <- function(family, backend = NULL) {
  # Extended families keep estimated nuisance parameters in local mutable
  # closures. A shallow list copy allows one fit to rewrite another's family.
  copied <- tryCatch(unserialize(serialize(family, NULL)), error = function(e) {
    stop("Cannot safely isolate the native family for refitting: ",
      conditionMessage(e), call. = FALSE)
  })
  if (identical(backend, "gam") && length(copied$family) == 1L &&
      grepl("^Tweedie", copied$family)) {
    # tw() factories use a private .Theta environment parented by the caller.
    # Bind native density helpers through the namespace, not an attached
    # package/global lookup. Only copied factory environments are changed.
    for (fun in copied[vapply(copied, is.function, logical(1))]) {
      env <- environment(fun)
      if (is.environment(env) && !isNamespace(env) &&
          exists(".Theta", envir = env, inherits = FALSE) &&
          exists(".a", envir = env, inherits = FALSE) &&
          exists(".b", envir = env, inherits = FALSE)) {
        density <- get0("ldTweedie", envir = env, inherits = TRUE)
        if (!is.null(density) && !identical(density, mgcv::ldTweedie)) {
          stop("Custom Tweedie density helpers are not validated for automatic search.", call. = FALSE)
        }
        parent.env(env) <- asNamespace("mgcv")
      }
    }
  }
  copied
}

.search_locked_data <- function(model, formula, backend, data = NULL) {
  original <- .step_main_formula(model, backend)
  source <- model
  if (!is.null(data)) {
    if (!is.data.frame(data) || anyDuplicated(row.names(data))) {
      stop("Search data require unique fitted row identifiers.", call. = FALSE)
    }
    if (backend == "merMod") {
      used <- .lme4_data(model, data)
      frame <- stats::model.frame(model)
      locked <- list(data = used, weights = stats::model.weights(frame),
        offset = if (is.null(stats::getCall(model)$offset)) NULL else frame[["(offset)"]])
    } else {
      if (!backend %in% c("brmsfit", "sdmTMB", "tinyVAST")) source$call["data"] <- list(data)
      locked <- .step_locked_data(source, original, backend)
      if (backend == "sdmTMB") locked <- .search_sdmTMB_lock_trials(model, original, locked)
    }
    index <- match(row.names(locked$data), row.names(data))
    if (anyNA(index) || anyDuplicated(index)) stop("Search data do not align with the original analysis rows.", call. = FALSE)
    supplied <- data[index, , drop = FALSE]
    # The step helper can restore saved raw columns. Explicit supplied data
    # must actually agree, rather than being silently repaired in this way.
    common <- intersect(names(locked$data), names(supplied))
    if (!all(vapply(common, function(nm) .search_equal(locked$data[[nm]], supplied[[nm]]), logical(1)))) {
      stop("Supplied search data do not reproduce the original fitted data.", call. = FALSE)
    }
    if (!all(.step_formula_vars(original) %in% names(supplied))) stop("Supplied search data are missing original formula variables.", call. = FALSE)
    call <- stats::getCall(model)
    env <- list2env(as.list(supplied), parent = .step_formula_env(original))
    if (is.symbol(call$data)) assign(as.character(call$data), supplied, envir = env)
    for (nm in c("weights", "offset")) {
      if (is.null(call[[nm]]) || is.null(locked[[nm]])) next
      value <- tryCatch(eval(call[[nm]], envir = env), error = function(e) NULL)
      if (length(value) == 1L) value <- rep(value, nrow(supplied))
      if (length(value) == nrow(data) && nrow(data) != nrow(supplied)) value <- value[index]
      if (is.null(value) || !.search_equal(as.numeric(value), as.numeric(locked[[nm]]))) {
        stop("Supplied search data do not reproduce original likelihood ", nm, ".", call. = FALSE)
      }
    }
    # Native backends may add a dummy time or indexing column to analysis data.
    # Keep these fitted process coordinates even when raw supplied data omit them.
    for (nm in setdiff(names(locked$data), names(supplied))) supplied[[nm]] <- locked$data[[nm]]
    locked$data <- supplied
  } else locked <- .step_locked_data(model, original, backend)
  if (backend == "sdmTMB") locked <- .search_sdmTMB_lock_trials(model, original, locked)
  if (!all(.step_formula_vars(formula) %in% names(locked$data))) stop("Search formula variables are absent from the locked analysis data.", call. = FALSE)
  if (backend == "tinyVAST") {
    w <- model$tmb_inputs$tmb_data$weights_i
    if (length(w) != nrow(locked$data) || any(!is.finite(w))) stop("Native tinyVAST weights do not align with the fitted rows.", call. = FALSE)
    locked$weights <- as.numeric(w)
  }
  locked$formula <- original
  locked$backend <- backend
  locked$row_names <- row.names(locked$data)
  locked
}

.search_control <- function(control, backend, original = NULL) {
  if (!is.list(control) || (length(control) && (is.null(names(control)) ||
      anyNA(names(control)) || any(!nzchar(names(control))) || anyDuplicated(names(control))))) {
    stop("Search control must be a uniquely named list.", call. = FALSE)
  }
  structural <- c("map", "tmb_map", "tmb_random", "tmb_par", "start", "lower", "upper",
    "censored_upper", "normalize", "collapse_spatial_variance", "collapse_threshold",
    "gmrf_parameterization", "use_anisotropy", "sar_adjacency", "sar_weight_style",
    "get_rsr", "profile", "development", "reml", "zerodisp_val", "rank_check",
    "use_rtmb", "scalePenalty", "idLinksBases", "scale.est")
  for (nm in intersect(names(control), structural)) {
    if (is.null(original) || !nm %in% names(original) ||
        !identical(control[[nm]], original[[nm]])) stop(
      "Search controls cannot change likelihood or process setting '", nm, "'.", call. = FALSE)
  }
  disabled <- c("getsd", "run_model", "calc.derivs", "eigval_check")
  if (any(vapply(control[intersect(names(control), disabled)], function(x) identical(x, FALSE), logical(1)))) {
    stop("Search controls cannot disable fitting or numerical diagnostics.", call. = FALSE)
  }
  if (identical(control$conv_check, "skip")) stop("Search controls cannot skip convergence checks.", call. = FALSE)
  if (backend %in% c("sdmTMB", "tinyVAST") && !is.null(original)) {
    for (nm in names(control)) original[nm] <- list(control[[nm]])
    control <- original
  }
  control
}

.search_refit_args <- function(model, backend, refit_args = list()) {
  .search_validate_maps(model, backend)
  args <- .step_validate_refit_args(refit_args)
  allowed <- switch(backend, lm = "control",
    glm = c("control", "start", "etastart", "mustart"),
    gam = c("control", "optimizer", "nthreads", "chunk.size", "gc.level"),
    glmmTMB = c("control", "start", "verbose", "REML"),
    merMod = c("control", "start", "verbose", "nAGQ", "reestimate_nb", "REML"),
    brmsfit = c("iter", "warmup", "chains", "cores", "seed", "control", "refresh",
      "threads", "backend", "recompile", "silent", "open_progress", "init"),
    sdmTMB = c("control", "silent", "reml"), tinyVAST = "control", character())
  bad <- setdiff(names(args), allowed)
  if (length(bad)) stop("Search arguments cannot override model structure: ", paste(bad, collapse = ", "), ".", call. = FALSE)
  original_control <- switch(backend, sdmTMB = model$control,
    tinyVAST = model$internal$control, gam = model$control,
    glmmTMB = if (is.null(model$call$control)) glmmTMB::glmmTMBControl() else NULL,
    NULL)
  if (!is.null(args$control)) {
    if (backend == "brmsfit") {
      args$control <- .search_control(args$control, backend)
      if (any(!names(args$control) %in% c("adapt_delta", "max_treedepth", "stepsize"))) {
        stop("brms search controls are limited to sampler execution settings.", call. = FALSE)
      }
    } else args$control <- .search_control(args$control, backend, original_control)
  }
  notes <- character()
  restricted <- switch(backend,
    merMod = inherits(model, "lmerMod") && lme4::isREML(model),
    glmmTMB = isTRUE(model$modelInfo$REML), sdmTMB = isTRUE(model$reml),
    tinyVAST = isTRUE(model$internal$control$reml), FALSE)
  if (backend %in% c("merMod", "glmmTMB") && "REML" %in% names(args) && !identical(args$REML, FALSE)) {
    stop("Fixed-term searches require ML (REML = FALSE).", call. = FALSE)
  }
  if (backend == "sdmTMB" && "reml" %in% names(args) && !identical(args$reml, FALSE)) stop("Fixed-term searches require ML (reml = FALSE).", call. = FALSE)
  if (restricted) {
    if (backend %in% c("merMod", "glmmTMB")) args$REML <- FALSE
    if (backend == "sdmTMB") args$reml <- FALSE
    if (backend == "tinyVAST") {
      args$control <- args$control %||% original_control
      args$control$reml <- FALSE
    }
    notes <- "The original REML fit must be refitted with ML before comparing fixed-term candidates."
  }
  if (backend == "gam") notes <- c(notes, "GAM comparisons use native conditional AIC, not the REML smoothing-selection objective; the original smoothing method is retained.")
  if (backend == "merMod" && .normalise_family_name(stats::family(model)$family)$family == "negative_binomial") {
    flag <- args$reestimate_nb
    if (!is.logical(flag) || length(flag) != 1L || is.na(flag)) stop("lme4 negative-binomial search requires explicit reestimate_nb = TRUE or FALSE.", call. = FALSE)
    notes <- c(notes, if (flag) "lme4 negative-binomial size is re-estimated for every candidate." else "lme4 negative-binomial size is held fixed for every candidate.")
  } else if ("reestimate_nb" %in% names(args)) stop("reestimate_nb applies only to lme4 negative-binomial models.", call. = FALSE)
  list(args = args, notes = notes, needs_original_refit = restricted)
}

.search_update_formula <- function(original, supplied) {
  forms <- .step_formulas(original)
  updates <- .step_formulas(supplied)
  if (length(forms) != length(updates) || !all(vapply(updates, inherits, logical(1), "formula"))) {
    stop("Search formulas must match the original response components.", call. = FALSE)
  }
  result <- lapply(seq_along(forms), function(i) {
    f <- stats::update.formula(forms[[i]], updates[[i]])
    if (!identical(f[[2L]], forms[[i]][[2L]])) stop("Search formulas must retain the original response.", call. = FALSE)
    offsets <- .step_offset_terms(forms[[i]])
    changed <- .step_offset_terms(f)
    if (length(setdiff(changed, offsets))) stop("Search formulas cannot change effort offsets.", call. = FALSE)
    missing <- setdiff(offsets, changed)
    if (length(missing)) f <- stats::update.formula(f, stats::as.formula(paste("~ . +", paste(missing, collapse = " + "))))
    environment(f) <- environment(forms[[i]])
    f
  })
  if (inherits(original, "formula")) result[[1L]] else result
}

.search_refit_one <- function(model, formula, args, locked, backend) {
  seed <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv) else NULL
  on.exit({
    if (is.null(seed)) {
      if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    } else assign(".Random.seed", seed, envir = .GlobalEnv)
  }, add = TRUE)
  .search_validate_maps(model, backend)
  formula <- .search_update_formula(.step_main_formula(model, backend), formula)
  if (!all(.step_formula_vars(formula) %in% names(locked$data))) stop("Search formulas must use the locked analysis data.", call. = FALSE)
  if (backend %in% c("glmmTMB", "sdmTMB", "tinyVAST")) {
    object <- switch(backend, glmmTMB = model$obj, sdmTMB = model$tmb_obj, tinyVAST = model$obj)
    if (!.search_live_objective(object)) stop("Search requires a live native objective and loaded DLL; reinitialise saved TMB fits explicitly before searching.", call. = FALSE)
  }
  if (backend == "glmmTMB") {
    args$family <- .search_copy_family(model$modelInfo$family)
    args$ziformula <- stats::formula(model, component = "zi")
    args$dispformula <- stats::formula(model, component = "disp")
    args$contrasts <- model$modelInfo$contrasts
    args$map <- model$modelInfo$map
    args$sparseX <- model$modelInfo$sparseX
    args$priors <- model$modelInfo$priors
  }
  if (backend %in% c("glm", "gam") && !inherits(model, "negbin")) args$family <- .search_copy_family(stats::family(model), backend)
  if (backend == "gam") args$method <- model$method
  if (backend == "merMod" && !inherits(model, "lmerMod")) model@call["family"] <- list(.search_copy_family(stats::family(model)))
  if (backend == "brmsfit") {
    if (isTRUE(model$formula$nl)) stop("Nonlinear brms formulas require a separately designed search.", call. = FALSE)
    full <- model$formula
    full$formula <- formula
    .search_brms_priors(model, full, locked$data)
    formula <- full
  }
  if (backend == "tinyVAST" && "delta_formula" %in% names(model$internal)) {
    model$internal$family <- .search_copy_family(model$internal$family)
    saved <- model$internal
    delta <- list(formula = saved$delta_formula)
    for (nm in c("spacetime_term", "space_term", "time_term", "spatial_varying")) {
      field <- paste0("delta_", nm)
      if (field %in% names(saved)) delta[nm] <- list(saved[[field]])
    }
    model$call["delta_options"] <- list(delta)
  }
  if (backend == "sdmTMB") {
    model$family <- .search_copy_family(model$family)
    args <- .search_sdmTMB_settings(model, args)
  }
  if (backend == "sdmTMB" && isTRUE(model$family$delta)) {
    # Native fitted state stores component settings as vectors, but the
    # constructor requires a scalar or a component list.
    if (length(model$spatial) == 2L) model$spatial <- as.list(model$spatial)
    if (length(model$spatiotemporal) == 2L) model$spatiotemporal <- as.list(model$spatiotemporal)
  }
  fit <- .step_refit_one(model, formula, args, locked, backend)
  if (!inherits(fit, backend)) stop("The refit did not return the original native model class.", call. = FALSE)
  if (backend == "sdmTMB") {
    for (nm in c("random_walk", "ar1_time", "anisotropy", "share_range", "spatial_model", "size", "size_i")) {
      if (!.search_equal(fit$tmb_data[[nm]], model$tmb_data[[nm]])) {
        stop("Search changed the native sdmTMB process or trial setting '", nm, "'.", call. = FALSE)
      }
    }
    if (!.search_equal(fit$time_varying, model$time_varying) ||
        !.search_equal(fit$spatial_varying_formula, model$spatial_varying_formula)) {
      stop("Search changed a fitted sdmTMB latent-process formula.", call. = FALSE)
    }
    for (nm in c("X_rw_ik", "z_i")) {
      if (!identical(dim(fit$tmb_data[[nm]]), dim(model$tmb_data[[nm]])) ||
          !.search_equal(fit$tmb_data[[nm]], model$tmb_data[[nm]])) {
        stop("Search changed the native sdmTMB latent-process design '", nm, "'.", call. = FALSE)
      }
    }
  }
  fit
}

.search_validate_maps <- function(model, backend) {
  map <- switch(backend, glmmTMB = model$modelInfo$map,
    sdmTMB = model$control$map, tinyVAST = model$internal$control$tmb_map, NULL)
  if (length(map) && any(lengths(map) > 0L)) {
    stop("Automatic search does not support unvalidated positional parameter maps; fit explicitly specified candidates outside the search.", call. = FALSE)
  }
  invisible(NULL)
}

.search_sdmTMB_lock_trials <- function(model, formula, locked) {
  if (isTRUE(model$family$delta) || !identical(model$family$family, "binomial")) return(locked)
  f <- .step_formulas(formula)[[1L]]
  y <- eval(f[[2L]], envir = locked$data, enclos = environment(f))
  if (is.matrix(y) || is.factor(y) || all(y %in% c(0, 1))) return(locked)
  size <- model$tmb_data$size %||% model$tmb_data$size_i
  native <- as.numeric(model$tmb_data$y_i)
  if (length(size) != nrow(locked$data) || any(!is.finite(size) | size < 0 | abs(size - round(size)) > 1e-8) ||
      any(y < 0 | y > 1) || !.search_equal(as.numeric(y * size), native) ||
      !all(as.numeric(model$tmb_data$weights_i) == 1)) {
    stop("Cannot recover fitted binomial proportions and trial counts.", call. = FALSE)
  }
  locked$weights <- as.numeric(size)
  locked
}

.search_sdmTMB_settings <- function(model, args) {
  fields <- c(time_varying = "time_varying", spatial_varying = "spatial_varying_formula",
    nonlocal_formula = "nonlocal_formula", experimental = "experimental")
  for (nm in names(fields)) {
    field <- fields[[nm]]
    if (!field %in% names(model)) {
      stop("Cannot recover the fitted sdmTMB process setting '", nm, "'.", call. = FALSE)
    }
    args[nm] <- list(model[[field]])
  }
  if (isTRUE(model$nonlocal_grid_supplied)) {
    stop("Search cannot safely recover the original nonlocal prediction grid; fit explicit candidates outside the search.", call. = FALSE)
  }
  args["nonlocal_data"] <- list(NULL)
  native <- model$tmb_data
  if (!is.null(model$time_varying)) {
    if (identical(as.integer(native$ar1_time), 1L)) {
      args$time_varying_type <- "ar1"
    } else if (identical(as.integer(native$random_walk), 1L)) {
      args$time_varying_type <- "rw"
    } else if (identical(as.integer(native$random_walk), 2L)) {
      args$time_varying_type <- "rw0"
    } else stop("Cannot recover the fitted sdmTMB temporal-process type.", call. = FALSE)
  }
  anisotropy <- native$anisotropy
  if (length(anisotropy) != 1L || !anisotropy %in% c(0, 1)) {
    stop("Cannot recover fitted sdmTMB anisotropy.", call. = FALSE)
  }
  args$anisotropy <- as.logical(anisotropy)
  range <- native$share_range
  if (!length(range) || anyNA(range) || any(!range %in% c(0, 1))) {
    stop("Cannot recover fitted sdmTMB range sharing.", call. = FALSE)
  }
  args$share_range <- if (length(unique(range)) == 1L) as.logical(range[1L]) else as.logical(range)
  domain <- native$spatial_model
  if (length(domain) != 1L || !domain %in% 0:2) {
    stop("Cannot recover the fitted sdmTMB spatial-domain type.", call. = FALSE)
  }
  args$spatial_model <- c("spde", "sar", "car")[as.integer(domain) + 1L]
  args
}

.search_brms_priors <- function(model, formula, data) {
  priors <- model$prior
  if (is.null(priors) || !nrow(priors)) return(invisible(NULL))
  if (!is.data.frame(priors) || !"source" %in% names(priors)) {
    stop("Search requires complete, identifiable native brms priors.", call. = FALSE)
  }
  user <- priors[priors$source == "user", , drop = FALSE]
  if (!nrow(user)) return(invisible(NULL))
  available <- brms::get_prior(formula, data = data, family = model$family)
  keys <- c("class", "coef", "group", "resp", "dpar", "nlpar")
  if (!all(keys %in% names(user)) || !all(keys %in% names(available))) {
    stop("Search cannot validate the native brms prior structure.", call. = FALSE)
  }
  for (i in seq_len(nrow(user))) {
    matched <- rep(TRUE, nrow(available))
    for (nm in keys) matched <- matched & as.character(available[[nm]]) == as.character(user[[nm]][i])
    if (!any(matched)) stop(
      "This search candidate removes a parameter with an explicit brms prior (",
      paste(paste0(keys, "=", user[i, keys]), collapse = ", "),
      "); refit a deliberately revised prior specification outside the search.", call. = FALSE)
  }
  invisible(NULL)
}

.search_fit_status <- function(fit, backend) {
  flags <- character()
  if (backend %in% c("glmmTMB", "sdmTMB", "tinyVAST")) {
    opt <- switch(backend, glmmTMB = fit$fit, sdmTMB = fit$model, tinyVAST = fit$opt)
    if (length(opt$convergence) != 1L || !is.finite(opt$convergence)) return(list(eligible = FALSE, detail = "Native optimiser convergence diagnostics are unavailable.", flags = "missing_convergence"))
    object <- switch(backend, glmmTMB = fit$obj, sdmTMB = fit$tmb_obj, tinyVAST = fit$obj)
    if (!.search_live_objective(object)) return(list(eligible = FALSE, detail = "Native objective pointer or DLL is unavailable; it was not reinitialised.", flags = "missing_native_objective"))
  }
  if (backend == "merMod") {
    code <- fit@optinfo$conv$opt
    if (length(code) != 1L || !is.finite(code)) return(list(eligible = FALSE,
      detail = "Native optimiser convergence diagnostics are unavailable.", flags = "missing_convergence"))
    if (lme4::isSingular(fit)) flags <- "singular"
  }
  if (backend %in% c("lm", "glm", "gam")) {
    if (isTRUE(fit$boundary)) flags <- c(flags, "boundary")
    if (any(!is.finite(fit$coefficients))) return(list(eligible = FALSE, detail = "Estimated coefficients are aliased or non-finite.", flags = c(flags, "invalid_coefficients")))
  }
  error <- tryCatch({ .step_check_convergence(fit, backend, "Search candidate"); NULL }, error = function(e) conditionMessage(e))
  if (!is.null(error)) return(list(eligible = FALSE, detail = error, flags = c(flags, "convergence")))
  if (backend == "brmsfit") {
    nuts <- tryCatch(brms::nuts_params(fit), error = function(e) NULL)
    if (is.null(nuts) || !nrow(nuts)) return(list(eligible = FALSE, detail = "Native posterior transition diagnostics are unavailable.", flags = "missing_mcmc_diagnostics"))
  }
  list(eligible = TRUE, detail = if (length(flags)) paste(flags, collapse = "; ") else
    "Available native convergence checks passed; this does not establish scientific adequacy.", flags = flags)
}
