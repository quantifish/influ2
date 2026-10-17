# sdmTMB 1.2.0 can fit with RTMB as well as the compiled TMB objective. Use
# its native constructor where available so prediction and observation
# objectives retain the fitted backend, map, and parameter layout. Earlier
# sdmTMB releases use only the compiled objective.
.sdmTMB_make_object <- function(model, data, parameters,
                                map = model$tmb_map,
                                random = model$tmb_random,
                                profile = model$control$profile,
                                silent = TRUE, ...) {
  factory <- get0("make_sdmTMB_adfun", envir = asNamespace("sdmTMB"),
    inherits = FALSE)
  backend <- model$backend
  if (is.null(backend)) backend <- attr(model$tmb_obj, "sdmTMB_backend")
  if (is.null(backend)) backend <- "tmb"
  if (is.function(factory)) {
    return(factory(data = data, parameters = parameters, map = map,
      random = random, profile = profile, backend = backend,
      silent = silent, ...))
  }
  if (!identical(backend, "tmb")) {
    stop("This sdmTMB version does not expose its fitted objective backend.",
      call. = FALSE)
  }
  dll <- model$tmb_obj$env$DLL
  if (is.null(dll)) dll <- "sdmTMB"
  TMB::MakeADFun(data = data, parameters = parameters, map = map,
    random = random, profile = profile, DLL = dll, silent = silent, ...)
}

# Rebuild a private prediction objective without the deprecated
# return_tmb_object argument or evaluating fn(), which would optimise latent
# effects. Reports at the saved full parameter vector preserve joint draws.
.sdmTMB_prediction_object <- function(model, newdata, ...) {
  td <- stats::predict(model, newdata = newdata, return_tmb_data = TRUE, ...)
  mode <- model$last.par.best
  original <- model$tmb_obj$env
  parameters <- original$parList(x = model$model$par, par = mode)
  obj <- .sdmTMB_make_object(model, data = td, parameters = parameters)
  if (!identical(names(obj$env$last.par.best), names(mode)) ||
      !isTRUE(all.equal(as.numeric(obj$env$last.par.best), as.numeric(mode),
        tolerance = 1e-10)) ||
      !identical(obj$env$lrandom(), original$lrandom())) {
    stop("Native sdmTMB prediction parameters do not match the fitted joint mode and random-effect map.",
      call. = FALSE)
  }
  list(obj = obj, pred_tmb_data = td)
}
