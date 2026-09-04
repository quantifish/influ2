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

.glmmTMB_random_diag <- function(model, data, focus, family_spec, weights,
                                 reference_data = NULL,
                                 reference_weights = NULL) {
  conditional <- tryCatch(
    as.numeric(stats::predict(model, newdata = data, type = "link")),
    error = function(e) NULL
  )
  fixed <- tryCatch(
    as.numeric(stats::predict(model, newdata = data, type = "link", re.form = NA)),
    error = function(e) NULL
  )
  if (is.null(conditional) || is.null(fixed)) return(NULL)
  contribution <- conditional - fixed
  if (!any(abs(contribution) > sqrt(.Machine$double.eps))) return(NULL)

  reference_contribution <- NULL
  if (!is.null(reference_data)) {
    reference_conditional <- tryCatch(
      as.numeric(stats::predict(model, newdata = reference_data, type = "link")),
      error = function(e) NULL
    )
    reference_fixed <- tryCatch(
      as.numeric(stats::predict(
        model, newdata = reference_data, type = "link", re.form = NA
      )),
      error = function(e) NULL
    )
    if (is.null(reference_conditional) || is.null(reference_fixed)) return(NULL)
    reference_contribution <- reference_conditional - reference_fixed
  }

  random_spec <- family_spec
  .influ_linear_engine(
    backend = "glmmTMB",
    model = model,
    data = data,
    response = NULL,
    focus = focus,
    family_spec = random_spec,
    X = matrix(contribution, ncol = 1L, dimnames = list(NULL, "random_effects")),
    beta = 1,
    vcov = NULL,
    term_columns = list(random_effects = 1L),
    uncertainty = "none",
    retain = "summary",
    probs = c(0.025, 0.975),
    weights = weights,
    reference_data = reference_data,
    reference_X = if (is.null(reference_contribution)) NULL else {
      matrix(reference_contribution, ncol = 1L, dimnames = list(NULL, "random_effects"))
    },
    reference_weights = reference_weights,
    component = "random_effects",
    keep_model = FALSE,
    notes = "Random-effect influence currently uses conditional modes; uncertainty is not yet propagated."
  )
}

#' Influence diagnostics for glmmTMB models
#'
#' The conditional model is handled with joint fixed-effect covariance.
#' Zero-inflation or hurdle occurrence terms are returned as a separate
#' component. Random-effect modes are included as a compact aggregate term;
#' propagation of their conditional uncertainty is planned for the spatial
#' engine.
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
  data <- .resolve_influ_data(model, data)
  response_structure <- .glmmTMB_structure(model)
  fam <- stats::family(model)
  overall_spec <- .new_family_spec(
    fam$family,
    fam$link,
    backend = "glmmTMB",
    response_structure = response_structure
  )

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
    model, data, focus, overall_spec, weights,
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
      "Random-effect influence uses conditional modes without propagated uncertainty."
    )
  )

  if (retain == "disk") {
    if (is.null(draws_path)) stop("`draws_path` is required when `retain = \"disk\"`.", call. = FALSE)
    saveRDS(out$draws, draws_path, compress = FALSE)
    out$draws <- NULL
    out$retained$mode <- "disk"
    out$retained$path <- normalizePath(draws_path, mustWork = TRUE)
  }
  out
}
