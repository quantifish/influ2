.implied_brms_adapter <- function(model, data, year, groups, baseline, component, year_term, draw_id) {
  .require_model_backend(model)
  if (is.null(model$fit) || !is.null(model$influ2_draws)) stop("A complete retained brms fit with native posterior draws is required for implied effects.", call. = FALSE)
  parsed <- brms::brmsterms(model$formula)
  if (isTRUE(parsed$mv) || length(parsed$nlpars) ||
      any(vapply(parsed$dpars, function(p) !inherits(p, "btl") || length(p$ac) > 0L ||
        length(p$gp) > 0L || length(p$sp) > 0L, logical(1))) ||
      length(parsed$adforms)) {
    stop("brms implied effects require a univariate linear-predictor model without response additions, autocorrelation, Gaussian processes, or special/nonlinear predictors.", call. = FALSE)
  }
  fam <- model$family
  joint <- identical(fam$family, "hurdle_lognormal")
  if (joint && (fam$link != "identity" || fam$link_hu != "logit")) stop("brms hurdle-lognormal implied effects require identity mu and logit hu links.", call. = FALSE)
  if (joint && (is.null(component) || !component %in% c("positive", "encounter", "combined"))) stop("A joint brms fit requires explicit component = 'positive', 'encounter', or 'combined'.", call. = FALSE)
  if (!joint && !is.null(component) && component != "conditional") stop("A single supported brms response only accepts component = 'conditional'.", call. = FALSE)
  if (fam$family %in% c("lognormal", "hurdle_lognormal") && fam$link != "identity") stop("brms lognormal implied effects require the native identity link for log-location mu.", call. = FALSE)
  family <- switch(fam$family, bernoulli = "binomial", negbinomial = "nbinom2", gamma = "Gamma",
    hurdle_lognormal = "lognormal", fam$family)
  link <- if (family == "lognormal" && fam$link == "identity") "log" else fam$link
  a <- .implied_native_data(model, data, year, groups, model$formula$formula, family)
  raw_y <- a$observed
  .implied_native_family(family, link, raw_y, joint)
  if (!is.null(draw_id)) {
    .resid_integer(draw_id, "draw_id", 1L)
    if (draw_id > posterior::ndraws(model)) stop("`draw_id` exceeds the available posterior draws.", call. = FALSE)
  }
  # Collapse PARAMETERS before computing predictors: never construct an
  # observations-by-all-draws array. A selected draw preserves the joint state.
  prep <- brms::prepare_predictions(model, re_formula = NULL, draw_ids = draw_id,
    point_estimate = if (is.null(draw_id)) "mean" else NULL, check_response = TRUE)
  if (prep$ndraws != 1L || prep$nobs != length(raw_y) || length(prep$ac) ||
      !identical(names(prep$data), "Y") ||
      (!is.null(prep$old_order) && !identical(as.integer(prep$old_order), seq_along(raw_y))) ||
      !isTRUE(all.equal(as.numeric(prep$data$Y), raw_y, tolerance = 0))) {
    stop("Native brms likelihood rows must match the retained response, without weights, censoring, truncation, trials, or reordered dependence structures.", call. = FALSE)
  }
  get <- function(dpar, inv_link = TRUE) as.numeric(brms::get_dpar(prep, dpar, inv_link = inv_link))
  # A modelled hu is a link-scale predictor; an unmodelled hu is already a
  # probability. get_dpar(inv_link = FALSE) does not transform scalar dpars.
  encounter_eta <- function() {
    if (is.list(prep$dpars$hu)) -get("hu", FALSE) else -stats::qlogis(get("hu"))
  }
  a$backend <- "brms"
  a$family <- family
  a$link <- link
  a$eta <- get("mu", FALSE)
  a$dispersion <- switch(family, gaussian = get("sigma"), lognormal = get("sigma"),
    nbinom2 = get("shape"), Gamma = 1 / get("shape"), 1)
  a$component <- if (joint) component else "single fitted response"
  a$component_index <- if (identical(component, "encounter")) "hu" else "mu"
  a$included <- if (identical(component, "positive")) raw_y > 0 else rep(TRUE, length(raw_y))
  a$reference <- if (is.null(draw_id)) "posterior_mean_parameters" else "joint_posterior_draw"
  a$draw_id <- draw_id
  a$conditioning <- if (is.null(draw_id)) {
    "Posterior-mean parameters, latent effects, smooths, offsets, and dispersion held fixed; not posterior-averaged implied effects"
  } else paste("Joint posterior draw", draw_id, "held fixed; not posterior-averaged implied effects")
  a <- .implied_native_predictors(a)
  # brms mu is mean(log Y), whereas the shared lognormal kernel uses log E(Y).
  # With sigma held fixed, a local shift has the same meaning on both scales.
  if (family == "lognormal") a$eta <- a$eta + a$dispersion^2 / 2
  if (identical(component, "combined")) {
    if (!is.null(year_term)) stop("Combined implied responses do not use a year-term baseline.", call. = FALSE)
    encounter <- encounter_eta()
    if (length(encounter) == 1L) encounter <- rep(encounter, length(raw_y))
    if (length(encounter) != length(raw_y) || any(!is.finite(encounter))) stop("Finite aligned brms encounter predictors are required.", call. = FALSE)
    a$eta <- cbind(encounter, a$eta)
    return(a)
  }
  dpar <- a$component_index
  if (identical(component, "encounter")) {
    a$eta <- encounter_eta()
    if (length(a$eta) == 1L) a$eta <- rep(a$eta, length(raw_y))
    a$observed <- as.numeric(raw_y > 0)
    a$dispersion <- rep(1, length(raw_y))
    a$family <- "binomial"
    a$link <- "logit"
    a <- .implied_native_predictors(a)
  }
  design <- .brms_population_matrix(model, dpar)
  if (is.null(design)) stop("An additive fixed year term is required in the requested brms component.", call. = FALSE)
  vars <- .brms_parameter_names(design$X, dpar)
  b <- prep$dpars[[dpar]]$fe$b
  if (is.null(b) || !all(vars %in% colnames(b))) stop("Native brms baseline coefficients are not aligned.", call. = FALSE)
  beta <- as.numeric(b[1L, vars])
  if (identical(component, "encounter")) beta <- -beta
  sm <- parsed$dpars[[dpar]]$sm
  smooths <- if (inherits(sm, "formula")) all.vars(sm) else character()
  .implied_native_baseline(a, groups, baseline, year_term, design$X, beta,
    design$term_columns, smooths)
}
