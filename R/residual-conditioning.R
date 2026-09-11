# Conditioning is resolved independently of the plotting schema. A common
# option name is offered only where its native meaning has been validated.
.resid_conditioning <- function(backend, conditioning) {
  choices <- list(glm = "fitted", gam = "fitted",
    glmmTMB = c("new_effects", "fitted"),
    sdmTMB = c("fitted", "conditional_draw", "new_effects"),
    tinyVAST = c("fitted", "conditional_draw"), brms = "posterior_predictive")
  supported <- choices[[backend]]
  if (!is.character(conditioning) || length(conditioning) != 1L ||
      is.na(conditioning) || !conditioning %in% c("backend_default", supported)) {
    stop("Unsupported `conditioning` for ", backend, ". Choose ",
      paste(c("backend_default", supported), collapse = ", "), ". ",
      "Other schemes require explicitly generated simulations and as_influ_residuals().",
      call. = FALSE)
  }
  if (conditioning == "backend_default") supported[1L] else conditioning
}

# Make a separate native objective, not a shallow copy of an R model whose
# TMB environments remain shared. Never set simulation codes on the user's fit.
.resid_tmb_object <- function(model, backend, conditioning) {
  if (!requireNamespace("TMB", quietly = TRUE)) {
    stop("TMB is required for this conditioning option.", call. = FALSE)
  }
  original <- if (backend == "sdmTMB") model$tmb_obj else model$obj
  e <- original$env
  mode <- if (backend == "sdmTMB") model$last.par.best else {
    if (backend == "glmmTMB") model$fit$parfull else e$last.par.best
  }
  if (is.null(e) || !is.function(e$parList) || !length(mode) ||
      any(!is.finite(mode)) || length(e$profile) || isTRUE(e$intern) ||
      length(e$integrate)) {
    stop("This conditioning option needs a complete, unprofiled native TMB fit.", call. = FALSE)
  }
  td <- e$data
  # The native environment contains TMB-sanitised doubles. Preserve those
  # types and its validation flag (some backends include native-only data).
  if (backend == "sdmTMB") {
    if (length(td$sim_re) != 6L) {
      stop("Unsupported sdmTMB random-effect simulation controls.", call. = FALSE)
    }
    td$sim_re[] <- if (conditioning == "new_effects") c(rep(1, 5L), 0) else rep(0, 6L)
    td$sim_obs[] <- 1
  }
  if (backend == "glmmTMB") {
    # Native simCode 1 holds effects fixed, 2 generates new effects. Apply to
    # all three model components, not only the conditional-mean component.
    for (component in c("terms", "termszi", "termsdisp")) {
      if (is.null(td[[component]])) next
      for (i in seq_along(td[[component]])) {
        if (is.null(td[[component]][[i]]$simCode)) {
          stop("This glmmTMB version does not expose validated simulation controls.", call. = FALSE)
        }
        td[[component]][[i]]$simCode[] <- if (conditioning == "fitted") 1 else 2
      }
    }
  }
  fixed <- e$lfixed()
  obj <- TMB::MakeADFun(data = td,
    parameters = e$parList(x = mode[fixed], par = mode), map = e$map,
    random = unique(names(mode)[e$random]), DLL = e$DLL, silent = TRUE)
  if (!identical(names(obj$env$last.par), names(mode)) ||
      !identical(obj$env$lrandom(), e$lrandom())) {
    stop("Native conditioning parameter order differs from the fitted model.", call. = FALSE)
  }
  if (conditioning == "conditional_draw") {
    reml <- if (backend == "sdmTMB") model$reml else model$internal$control$reml
    if (isTRUE(reml)) {
      stop("`conditional_draw` currently requires ML, not REML: native REML latent vectors can include fixed coefficients. Use `conditioning = 'fitted'` or explicit external simulations.", call. = FALSE)
    }
    report <- if (backend == "sdmTMB") model$sd_report else model$sdrep
    convergence <- if (backend == "sdmTMB") model$model$convergence else model$opt$convergence
    if (!isTRUE(report$pdHess) || !identical(as.integer(convergence), 0L)) {
      stop("A conditional draw requires a converged fit with a positive-definite Hessian.", call. = FALSE)
    }
    if (!length(obj$env$random)) {
      stop("`conditional_draw` requires latent effects; use `conditioning = 'fitted'` for this model.", call. = FALSE)
    }
    # Initialise the sparse conditional factorisation at fitted fixed
    # parameters. No outer optimisation or new model fit is performed.
    obj$fn(mode[fixed])
    obj$env$last.par <- obj$env$last.par.best <- mode
    sample <- obj$env$MC(n = 1L, keep = TRUE, antithetic = FALSE)
    effects <- as.numeric(attr(sample, "samples"))
    if (length(effects) != sum(obj$env$lrandom()) || any(!is.finite(effects))) {
      stop("Native conditional latent-effect sampling failed.", call. = FALSE)
    }
    mode[obj$env$lrandom()] <- effects
  }
  list(obj = obj, par = mode)
}

.resid_conditioned_simulator <- function(model, backend, conditioning, nsim) {
  prepared <- .resid_tmb_object(model, backend, conditioning)
  obj <- prepared$obj
  par <- prepared$par
  n <- if (backend == "glmmTMB") nrow(model$frame) else nrow(model$data)
  delta <- backend == "sdmTMB" && isTRUE(model$family$delta)
  # Per-replicate seeds are prepared once, so both the latent draw and whole
  # response vectors are unchanged by batch size or component selection.
  seeds <- sample.int(.Machine$integer.max, nsim, replace = TRUE)
  simulate <- function(ids, component = NA_integer_) {
    vapply(ids, function(i) {
      set.seed(seeds[i])
      value <- obj$simulate(par = par, complete = FALSE)
      y <- if (backend == "glmmTMB") value$yobs else value$y_i
      if (delta) {
        y <- if (is.na(component)) y[, 1L] * y[, 2L] else y[, component]
      }
      as.numeric(y)
    }, numeric(n))
  }
  probability <- function(hurdle = FALSE) {
    r <- obj$report(par)
    if (backend == "sdmTMB") {
      family <- if (hurdle) model$family[[1L]] else model$family
      value <- family$linkinv(r$eta_i[, 1L])
      if (hurdle && isTRUE(as.logical(model$tmb_data$poisson_link_delta))) {
        value <- -expm1(-value)
      }
      value
    } else if (backend == "tinyVAST") {
      if (hurdle) {
        family <- model$internal$family[[1L]]
        if (!identical(family$type, "standard")) {
          stop("tinyVAST encounter calibration currently requires the standard delta link.", call. = FALSE)
        }
        family[[1L]]$linkinv(r$p_i)
      } else r$mu_i
    } else stop("No sampled-field probability adapter for this backend.", call. = FALSE)
  }
  list(simulate = simulate, probability = probability)
}
