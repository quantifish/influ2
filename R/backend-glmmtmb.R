.glmmTMB_has_component <- function(model, component) {
  frm <- tryCatch(stats::formula(model, component = component), error = function(e) NULL)
  if (is.null(frm)) return(FALSE)
  rhs <- gsub("\\s+", "", paste(deparse(frm[[length(frm)]]), collapse = ""))
  !rhs %in% c("0", "-1")
}

.glmmTMB_structure <- function(model) {
  fam <- stats::family(model)$family
  has_zi <- .glmmTMB_has_component(model, "zi")
  if (has_zi && grepl("^truncated_", fam)) return("hurdle")
  if (has_zi) return("zero_inflated")
  "single"
}

.glmmTMB_component_diag <- function(model, data, focus, component,
                                    family_spec, weights, uncertainty, retain,
                                    probs, ndraws, seed, draws_path,
                                    reference_data = NULL,
                                    reference_weights = NULL) {
  X <- stats::model.matrix(model, component = component)
  beta <- glmmTMB::fixef(model)[[component]]
  V <- stats::vcov(model)[[component]]
  if (!is.null(V) && component != "cond") {
    dimnames(V) <- lapply(dimnames(V), function(x) sub(paste0("^", component, "~"), "", x))
  }
  frm <- stats::formula(model, component = component)
  terms_component <- stats::terms(frm)
  assign <- attr(X, "assign")
  labels <- attr(terms_component, "term.labels")
  term_columns <- lapply(seq_along(labels), function(i) which(assign == i))
  names(term_columns) <- labels
  term_columns <- term_columns[lengths(term_columns) > 0L]
  if (!length(term_columns)) return(NULL)

  prepared <- .prepare_frequentist_matrix(
    model, X, term_columns, beta = beta, V = V
  )
  reference_X <- if (is.null(reference_data)) NULL else {
    .align_reference_matrix(
      stats::model.matrix(model, newdata = reference_data, component = component),
      prepared$X
    )
  }
  component_name <- switch(
    component,
    cond = if (family_spec$structure == "hurdle") "positive" else "conditional",
    zi = if (family_spec$structure == "hurdle") "occurrence" else "zero_probability",
    component
  )

  .influ_linear_engine(
    backend = "glmmTMB",
    model = model,
    data = data,
    response = if (component == "cond") names(stats::model.frame(model))[1] else NULL,
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
    reference_data = reference_data,
    reference_X = reference_X,
    reference_weights = reference_weights,
    component = component_name,
    ndraws = ndraws,
    seed = seed,
    draws_path = draws_path,
    keep_model = FALSE
  )
}

.glmmTMB_component_projection <- function(model, data, focus, component,
                                          draws, family_spec, weights,
                                          reference_data = NULL,
                                          reference_weights = NULL) {
  X <- stats::model.matrix(model, component = component)
  frm <- stats::formula(model, component = component)
  assign <- attr(X, "assign")
  labels <- attr(stats::terms(frm), "term.labels")
  term_columns <- lapply(seq_along(labels), function(i) which(assign == i))
  names(term_columns) <- labels
  term_columns <- term_columns[lengths(term_columns) > 0L]
  weights <- .resolve_influ_weights(data, weights)
  focus_info <- .focus_info(data, focus)
  reference_X <- if (is.null(reference_data)) X else stats::model.matrix(
    model, newdata = reference_data, component = component
  )
  reference_weights <- if (is.null(reference_data)) {
    weights
  } else {
    .resolve_influ_weights(reference_data, reference_weights)
  }
  reference_design <- .weighted_col_mean(reference_X, reference_weights)
  term_deltas <- lapply(term_columns, function(columns) {
    B_term <- .term_contrast(
      X[, columns, drop = FALSE], focus_info, weights,
      reference_X[, columns, drop = FALSE], reference_weights
    )
    B <- matrix(0, nrow = nrow(B_term), ncol = ncol(X))
    B[, columns] <- B_term
    draws %*% t(B)
  })
  list(
    family_spec = family_spec,
    eta_reference = as.numeric(draws %*% reference_design),
    term_deltas = term_deltas,
    reference = if (is.null(reference_data)) "observed" else "prediction_grid",
    method = if (nrow(draws) == 1L) "none" else "joint coefficient simulation"
  )
}

.glmmTMB_two_part_diag <- function(model, data, focus, overall_spec,
                                   probability_spec, weights, uncertainty,
                                   retain, probs, ndraws, seed,
                                   reference_data = NULL,
                                   reference_weights = NULL) {
  X_cond <- stats::model.matrix(model, component = "cond")
  X_zi <- stats::model.matrix(model, component = "zi")
  beta_cond <- glmmTMB::fixef(model)$cond
  beta_zi <- glmmTMB::fixef(model)$zi
  cond_names <- colnames(X_cond)
  zi_names <- paste0("zi~", colnames(X_zi))
  full_names <- c(cond_names, zi_names)
  beta <- c(beta_cond[cond_names], beta_zi[colnames(X_zi)])
  names(beta) <- full_names

  if (uncertainty == "none") {
    joint_draws <- matrix(beta, nrow = 1L, dimnames = list(NULL, full_names))
  } else {
    V <- stats::vcov(model, full = TRUE)
    missing <- setdiff(full_names, rownames(V))
    if (length(missing)) {
      stop(
        "Could not align the joint glmmTMB covariance for: ",
        paste(missing, collapse = ", "), ".",
        call. = FALSE
      )
    }
    joint_draws <- .draw_mvn(
      ndraws,
      beta,
      V[full_names, full_names, drop = FALSE],
      seed = seed
    )
    colnames(joint_draws) <- full_names
  }

  cond_draws <- joint_draws[, cond_names, drop = FALSE]
  zi_draws <- joint_draws[, zi_names, drop = FALSE]
  colnames(zi_draws) <- colnames(X_zi)
  main_projection <- .glmmTMB_component_projection(
    model, data, focus, "cond", cond_draws, overall_spec, weights,
    reference_data, reference_weights
  )
  probability_projection <- .glmmTMB_component_projection(
    model, data, focus, "zi", zi_draws, probability_spec, weights,
    reference_data, reference_weights
  )
  response <- names(stats::model.frame(model))[1]

  .two_part_combined_diag(
    backend = "glmmTMB",
    model = model,
    data = data,
    response = response,
    focus = focus,
    family_spec = overall_spec,
    main_projection = main_projection,
    probability_projection = probability_projection,
    probability_is_zero = TRUE,
    weights = weights,
    retain = retain,
    probs = probs,
    notes = paste0(
      "Fixed conditional and zero-probability components are combined ",
      if (uncertainty == "none") "at their estimates." else
        "with joint coefficient simulation."
    )
  )
}

.glmmTMB_reference_random_matrix <- function(model, reference_data) {
  structure <- stats::predict(model, newdata = reference_data, debug = TRUE)
  rows <- structure$data.tmb$whichPredict
  if (is.null(rows) || !length(rows) || is.null(structure$condList$Z)) {
    stop("Could not construct the glmmTMB random-effect reference matrix.", call. = FALSE)
  }
  structure$condList$Z[rows, , drop = FALSE]
}

.glmmTMB_random_covariance <- function(model, n_random) {
  latent <- stats::predict(model, type = "latent", cov.fit = TRUE)
  if (length(latent$fit) < n_random ||
      any(dim(latent$cov.fit) < c(n_random, n_random))) {
    stop("glmmTMB returned an incomplete latent random-effect covariance.", call. = FALSE)
  }
  latent$cov.fit[seq_len(n_random), seq_len(n_random), drop = FALSE]
}

.glmmTMB_random_diag <- function(model, data, focus, family_spec, weights,
                                 uncertainty, retain, probs, ndraws, seed,
                                 reference_data = NULL,
                                 reference_weights = NULL) {
  Z <- glmmTMB::getME(model, "Z")
  b <- as.numeric(glmmTMB::getME(model, "b"))
  if (!length(b) || !ncol(Z)) return(NULL)
  if (ncol(Z) != length(b)) {
    stop("The glmmTMB random-effect design and mode vector do not conform.", call. = FALSE)
  }
  colnames(Z) <- paste0("random_", seq_along(b))

  reference_Z <- if (is.null(reference_data)) NULL else {
    out <- .glmmTMB_reference_random_matrix(model, reference_data)
    if (ncol(out) != length(b)) {
      stop("The glmmTMB random-effect reference matrix does not conform.", call. = FALSE)
    }
    colnames(out) <- colnames(Z)
    out
  }

  V <- if (uncertainty == "none") NULL else {
    .glmmTMB_random_covariance(model, length(b))
  }
  random_uncertainty <- if (uncertainty == "auto") "analytic" else uncertainty

  .influ_linear_engine(
    backend = "glmmTMB",
    model = model,
    data = data,
    response = NULL,
    focus = focus,
    family_spec = family_spec,
    X = Z,
    beta = b,
    vcov = V,
    term_columns = list(random_effects = seq_along(b)),
    uncertainty = random_uncertainty,
    retain = retain,
    probs = probs,
    weights = weights,
    reference_data = reference_data,
    reference_X = reference_Z,
    reference_weights = reference_weights,
    component = "random_effects",
    ndraws = ndraws,
    seed = seed,
    keep_model = FALSE,
    notes = paste(
      "Random-effect influence uses conditional modes and their joint",
      "conditional latent covariance from glmmTMB."
    )
  )
}

#' Influence diagnostics for glmmTMB models
#'
#' The conditional model is handled with joint fixed-effect covariance.
#' Zero-inflation or hurdle occurrence terms are returned as a separate
#' component. Random-effect modes are included as a compact aggregate term,
#' with uncertainty propagated from their joint conditional latent covariance.
#' Lognormal models require a log link for their arithmetic mean. Dispersion
#' may vary, but its effects are not decomposed: the diagnostics describe
#' mean-model terms. Unlike BRMS, glmmTMB does not parameterise log-location.
#' Offsets have the same single-component restrictions as the GLM adapter,
#' and nominal summaries
#' are observed-response summaries, not automatically exposure-adjusted CPUE.
#'
#' @inheritParams influ.glm
#' @param model A fitted object from [glmmTMB::glmmTMB()].
#'
#' @return An [influ_diag] object.
#' @export
influ.glmmTMB <- function(model, focus, data = NULL, weights = NULL,
                          reference_data = NULL, reference_weights = NULL,
                          uncertainty = "auto", retain = "summary",
                          probs = c(0.025, 0.975), ndraws = 1000L,
                          seed = NULL, draws_path = NULL,
                          keep_model = FALSE, ...) {
  if (!requireNamespace("glmmTMB", quietly = TRUE)) {
    stop("Package 'glmmTMB' is required for this model.", call. = FALSE)
  }
  ndraws <- .validate_ndraws(ndraws)
  data <- .resolve_influ_data(model, data)
  response_structure <- .glmmTMB_structure(model)
  fam <- stats::family(model)
  overall_spec <- .new_family_spec(
    fam$family,
    fam$link,
    backend = "glmmTMB",
    response_structure = response_structure
  )
  .check_influ_lognormal_mean_link(overall_spec)
  offset_sources <- .influ_offset_sources(
    lapply(c("cond", "zi", "disp"), function(component) {
      stats::formula(model, component = component)
    }), model$call, stats::model.offset(stats::model.frame(model))
  )
  .check_influ_offset_scope(overall_spec, offset_sources)

  component_retain <- if (retain == "disk") "derived_draws" else retain
  components <- list(
    conditional = .glmmTMB_component_diag(
      model, data, focus, "cond", overall_spec, weights, uncertainty,
      component_retain, probs, ndraws, seed, NULL,
      reference_data, reference_weights
    )
  )

  if (.glmmTMB_has_component(model, "zi")) {
    probability_spec <- .new_family_spec(
      "binomial", "logit", backend = "glmmTMB",
      response_structure = response_structure,
      complement = response_structure == "hurdle"
    )
    components$probability <- .glmmTMB_component_diag(
      model, data, focus, "zi", probability_spec, weights, uncertainty,
      component_retain, probs, ndraws, seed, NULL,
      reference_data, reference_weights
    )
    components$unconditional_mean <- .glmmTMB_two_part_diag(
      model, data, focus, overall_spec, probability_spec, weights,
      uncertainty, component_retain, probs, ndraws, seed,
      reference_data, reference_weights
    )
  }

  components$random <- .glmmTMB_random_diag(
    model, data, focus, overall_spec, weights, uncertainty, component_retain,
    probs, ndraws, seed,
    reference_data, reference_weights
  )

  out <- .combine_influ_diags(
    components,
    backend = "glmmTMB",
    family_spec = overall_spec,
    focus = focus,
    model = model,
    keep_model = keep_model,
    notes = c(
      "Hurdle and zero-inflation fixed components include joint unconditional-mean influence.",
      "Random-effect influence includes joint conditional latent uncertainty."
    )
  )

  if (retain == "disk") {
    if (is.null(draws_path)) stop("`draws_path` is required when `retain = \"disk\"`.", call. = FALSE)
    saveRDS(out$draws, draws_path, compress = FALSE)
    out$draws <- NULL
    out$retained$mode <- "disk"
    out$retained$path <- normalizePath(draws_path, mustWork = TRUE)
  }
  if (overall_spec$family == "lognormal") {
    out$metadata$lognormal <- list(
      parameterisation = "arithmetic mean and data-scale standard deviation",
      scope = "Mean-model terms only; dispersion effects are not decomposed."
    )
  }
  .record_influ_offset_scope(out, offset_sources)
}
