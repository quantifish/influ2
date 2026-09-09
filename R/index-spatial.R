# Response prediction adapters retain native family calculations and joint
# parameter/field dependence. Only one prediction block is held at a time.
.index_spatial_info <- function(model, backend, year) {
  if (!requireNamespace(backend, quietly = TRUE) ||
      !requireNamespace("TMB", quietly = TRUE) ||
      !requireNamespace("Matrix", quietly = TRUE)) {
    stop("The spatial backend, TMB, and Matrix must be installed.", call. = FALSE)
  }
  if (backend == "sdmTMB") {
    if (is.null(model$tmb_obj) || is.null(model$sd_report)) {
      stop("Use a complete sdmTMB fit for response indices, not a year_effect diagnostic.", call. = FALSE)
    }
    if (!is.null(model$nonlocal_parsed)) {
      stop("Nonlocal sdmTMB covariate operators require a complete external prediction grid and are not supported by the blocked index adapter.", call. = FALSE)
    }
    data <- model$data
    families <- if (isTRUE(model$family$delta)) model$family[1:2] else list(model$family)
    formulas <- model$formula
    mode <- model$last.par.best
    sd <- model$sd_report
    fixed <- model$model$par
    time <- if (identical(model$time, "_sdmTMB_time")) NULL else model$time
    convergence <- model$model$convergence
  } else {
    if (is.null(model$obj) || is.null(model$sdrep)) {
      stop("Use a complete tinyVAST fit for response indices.", call. = FALSE)
    }
    if (length(model$internal$family) != 1L || length(model$internal$variables) > 1L) {
      stop("Spatial CPUE indices currently require a univariate, single-family tinyVAST fit; do not combine species or response units implicitly.", call. = FALSE)
    }
    data <- model$data
    families <- model$internal$family
    formulas <- list(model$formula, model$internal$delta_formula)
    mode <- model$obj$env$last.par.best
    sd <- model$sdrep
    fixed <- model$opt$par
    time <- if (length(model$internal$times)) model$internal$time_column else NULL
    convergence <- model$opt$convergence
  }
  if (is.null(year) && length(time) == 1L && time %in% names(data)) year <- time
  if (is.null(year)) year <- .comparison_focus(model, NULL)
  if (!isTRUE(sd$pdHess) || (!is.null(convergence) && convergence != 0) ||
      !length(mode) || any(!is.finite(mode))) {
    stop("Spatial response indices require a converged fit with a positive-definite Hessian.", call. = FALSE)
  }
  for (f in formulas) {
    if (inherits(f, "formula") && length(f) == 3L && !is.name(f[[2L]])) {
      stop("Use a model of the natural response, not a transformed response, for a CPUE index.", call. = FALSE)
    }
  }
  family <- if (backend == "sdmTMB") model$family else families[[1L]]
  list(data = data, year = year, time = time, mode = mode, sd = sd, fixed = fixed,
    family = paste(family$family, collapse = "/"), link = paste(family$link, collapse = "/"))
}

.index_spatial_projection <- function(model, backend, data, fields, offset) {
  if (backend == "sdmTMB") {
    # sdmTMB offsets are supplied separately from prediction data. Never reuse
    # observation-specific exposure on a reference grid by position.
    offset_value <- if (is.null(offset)) rep(0, nrow(data)) else data[[offset]]
    if (is.null(offset_value) || !is.numeric(offset_value) ||
        length(offset_value) != nrow(data) || any(!is.finite(offset_value))) {
      stop("`prediction_offset` must name a finite link-scale offset column in reference_data.", call. = FALSE)
    }
    td <- stats::predict(model, newdata = data, return_tmb_data = TRUE,
      offset = offset_value, re_form_iid = NA)
    if (nrow(td$proj_X_ij[[1L]]) != nrow(data)) {
      stop("Native sdmTMB prediction rows do not match the reference rows.", call. = FALSE)
    }
    # Parameter reconstruction uses a copy of the fitted values. The original
    # objective is never evaluated or optimised by this adapter.
    parameters <- model$tmb_obj$env$parList(x = model$model$par,
      par = model$last.par.best)
    obj <- TMB::MakeADFun(data = td, parameters = parameters,
      map = model$tmb_map, random = model$tmb_random,
      profile = model$control$profile, DLL = "sdmTMB", silent = TRUE)
    response <- function(par) {
      r <- obj$report(par)
      eta <- .index_sdmtmb_eta(r, fields)
      if (!is.matrix(eta) || nrow(eta) != nrow(data)) {
        stop("Native sdmTMB response predictors could not be aligned.", call. = FALSE)
      }
      if (isTRUE(model$family$delta)) {
        # Also correct for the Poisson-link delta parameterisation: the
        # combined expectation is intensity times positive-event weight.
        mu <- model$family[[1L]]$linkinv(eta[, 1L]) *
          model$family[[2L]]$linkinv(eta[, 2L])
      } else mu <- model$family$linkinv(eta[, 1L])
      .index_check_prediction(mu, nrow(data))
    }
  } else {
    for (pair in list(c(model$internal$time_column, "times"),
        c(model$internal$variable_column, "variables"))) {
      if (length(pair) == 2L && length(model$internal[[pair[2L]]]) &&
          pair[1L] %in% names(data) &&
          !all(data[[pair[1L]]] %in% model$internal[[pair[2L]]])) {
        stop("Prediction values in `", pair[1L], "` must belong to the fitted tinyVAST domain.", call. = FALSE)
      }
    }
    dist <- model$internal$distribution_column
    if (length(dist) && dist %in% names(data) &&
        !all(data[[dist]] %in% names(model$internal$family))) {
      stop("Prediction distributions must belong to the fitted tinyVAST family.", call. = FALSE)
    }
    td <- tinyVAST::add_predictions(model, data, remove_origdata = FALSE)
    # Remove contributions only from prediction matrices, never the likelihood
    # or fitted parameters. Native mu_g preserves delta links and families.
    if (fields %in% c("none", "spatiotemporal")) {
      td$AomegaG_z[] <- 0
      td$W_gl[] <- 0
      td$W2_gl[] <- 0
    }
    if (fields %in% c("none", "spatial")) td$AepsilonG_z[] <- 0
    obj <- TMB::MakeADFun(data = td, parameters = model$internal$parlist,
      map = model$tmb_inputs$tmb_map, random = model$tmb_inputs$tmb_random,
      profile = model$internal$control$profile, DLL = "tinyVAST", silent = TRUE)
    response <- function(par) .index_check_prediction(obj$report(par)$mu_g, nrow(data))
  }
  list(obj = obj, response = response)
}

.index_sdmtmb_eta <- function(report, fields) {
  # Start from the native expected-response predictor. proj_fe can omit a
  # family-specific mixture-mean adjustment that is included in proj_eta.
  # Subtract only fields, retaining offsets, smooths, temporal coefficients,
  # and any native response-mean adjustment for every prediction target.
  switch(fields, all = report$proj_eta,
    none = report$proj_eta - report$proj_rf,
    spatial = report$proj_eta - report$proj_epsilon_st_A_vec,
    spatiotemporal = report$proj_eta - report$proj_rf + report$proj_epsilon_st_A_vec)
}

.index_spatial_sampler <- function(info, obj) {
  if (length(obj$env$last.par) != length(info$mode) ||
      !identical(names(obj$env$last.par), names(info$mode))) {
    stop("Native prediction parameter order differs from the fitted joint parameter order.", call. = FALSE)
  }
  if (length(obj$env$random)) {
    precision <- info$sd$jointPrecision
    if (is.null(precision)) {
      # Recompute precision on our own prediction objective, not the user's
      # model environment. Skip all observation-level ADREPORT covariances.
      report <- TMB::sdreport(obj, par.fixed = info$fixed,
        getJointPrecision = TRUE, getReportCovariance = FALSE,
        skip.delta.method = TRUE)
      precision <- report$jointPrecision
    }
    if (is.null(precision) || nrow(precision) != length(info$mode)) {
      stop("A matching joint precision matrix is required for spatial index uncertainty.", call. = FALSE)
    }
    factor <- Matrix::Cholesky(precision, super = TRUE)
    function(n) {
      z <- matrix(stats::rnorm(length(info$mode) * n), nrow = length(info$mode))
      z <- Matrix::solve(factor, z, system = "Lt")
      z <- Matrix::solve(factor, z, system = "Pt")
      sweep(as.matrix(z), 1L, info$mode, "+")
    }
  } else {
    covariance <- .index_check_covariance(as.matrix(info$sd$cov.fixed), length(info$mode))
    factor <- t(chol(covariance))
    function(n) {
      z <- matrix(stats::rnorm(length(info$mode) * n), nrow = length(info$mode))
      sweep(factor %*% z, 1L, info$mode, "+")
    }
  }
}

.index_spatial <- function(model, backend, info, years, newdata, weights,
    uncertainty, ndraws, batch_size, draw_batch_size, seed, fields, offset) {
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
      seed < 0 || seed > .Machine$integer.max || seed != as.integer(seed)) {
    stop("`seed` must be one non-negative integer.", call. = FALSE)
  }
  if (uncertainty && ndraws < 2L) stop("At least two joint draws are required.", call. = FALSE)
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else NULL
  on.exit(if (is.null(old_seed)) {
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  } else assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
  estimate <- numeric(length(years))
  annual <- if (uncertainty) matrix(0, ndraws, length(years),
    dimnames = list(as.character(seq_len(ndraws)), years)) else NULL
  sampler <- NULL
  n_reference <- length(weights)
  total <- length(years) * n_reference
  for (first in seq(1, total, by = batch_size)) {
    index <- seq.int(first, min(total, first + batch_size - 1L))
    y <- as.integer((index - 1L) %/% n_reference + 1L)
    rows <- as.integer((index - 1L) %% n_reference + 1L)
    d <- newdata(y, rows)
    block <- .index_spatial_projection(model, backend, d, fields, offset)
    add <- function(value) {
      result <- numeric(length(years))
      sums <- rowsum(matrix(value * weights[rows], ncol = 1L), y, reorder = FALSE)
      result[as.integer(rownames(sums))] <- sums[, 1L]
      result
    }
    # Free every temporary native objective even if a prediction fails.
    tryCatch({
      estimate <- estimate + add(block$response(info$mode))
      if (uncertainty) {
        if (is.null(sampler)) sampler <- .index_spatial_sampler(info, block$obj)
        # Replay the SAME joint parameter draws for every prediction block.
        # The random-number sequence is independent of both batch sizes.
        set.seed(as.integer(seed))
        for (start in seq.int(1L, ndraws, by = draw_batch_size)) {
          ids <- seq.int(start, min(ndraws, start + draw_batch_size - 1L))
          parameters <- sampler(length(ids))
          for (j in seq_along(ids)) {
            annual[ids[j], ] <- annual[ids[j], ] + add(block$response(parameters[, j]))
          }
        }
      }
    }, finally = .index_free_tmb(block$obj))
  }
  if (any(!is.finite(estimate)) || (!is.null(annual) && any(!is.finite(annual)))) {
    stop("Spatial index predictions or joint draws are non-finite.", call. = FALSE)
  }
  list(estimate = estimate, draws = annual)
}

.index_free_tmb <- function(obj) {
  # TMB may lazily construct spHess. Force it BEFORE FreeADFun frees the
  # gradient tape on which construction depends (not afterwards inside that
  # function). Otherwise cleanup of an unevaluated random-effects objective
  # can dereference a freed native pointer.
  invisible(obj$env$spHess)
  TMB::FreeADFun(obj)
}
