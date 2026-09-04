.tinyVAST_family_specs <- function(model) {
  families <- model$internal$family
  if (length(families) != 1L) {
    stop(
      "The first tinyVAST adapter supports one response distribution at a time; ",
      "multivariate mixed-family models are pending.",
      call. = FALSE
    )
  }
  fam <- families[[1]]
  if (isTRUE(fam$delta)) {
    list(
      overall = .new_family_spec(
        fam$family[2], fam$link[2], backend = "tinyVAST",
        response_structure = "hurdle"
      ),
      occurrence = .new_family_spec(
        "binomial", fam$link[1], backend = "tinyVAST",
        response_structure = "hurdle"
      ),
      positive = .new_family_spec(
        fam$family[2], fam$link[2], backend = "tinyVAST",
        response_structure = "hurdle"
      )
    )
  } else {
    spec <- .new_family_spec(fam$family, fam$link, backend = "tinyVAST")
    list(overall = spec, conditional = spec)
  }
}

.tinyVAST_fixed_diag <- function(model, data, focus, component_index,
                                 component, family_spec, weights,
                                 uncertainty, retain, probs, ndraws, seed) {
  setup <- if (component_index == 1L) {
    model$internal$gam_setup
  } else {
    model$internal$delta_gam_setup
  }
  X <- if (component_index == 1L) {
    model$tmb_inputs$tmb_data$X_ij
  } else {
    model$tmb_inputs$tmb_data$X2_ij
  }
  param <- if (component_index == 1L) "alpha_j" else "alpha2_j"
  beta <- model$internal$parlist[[param]]
  fixed_index <- which(names(model$sdrep$par.fixed) == param)
  V <- if (length(fixed_index) == length(beta)) {
    model$sdrep$cov.fixed[fixed_index, fixed_index, drop = FALSE]
  } else {
    NULL
  }

  labels <- attr(setup$pterms, "term.labels")
  assign <- setup$assign
  term_columns <- lapply(seq_along(labels), function(i) which(assign == i))
  names(term_columns) <- labels
  term_columns <- term_columns[lengths(term_columns) > 0L]
  if (!length(term_columns)) return(NULL)

  .influ_linear_engine(
    backend = "tinyVAST",
    model = model,
    data = data,
    response = if (component_index == 1L) all.vars(model$formula)[1] else NULL,
    focus = focus,
    family_spec = family_spec,
    X = X,
    beta = beta,
    vcov = V,
    term_columns = term_columns,
    uncertainty = uncertainty,
    retain = retain,
    probs = probs,
    weights = weights,
    component = component,
    ndraws = ndraws,
    seed = seed,
    keep_model = FALSE
  )
}

.tinyVAST_fixed_projection <- function(model, data, focus, component_index,
                                       draws, family_spec, weights) {
  setup <- if (component_index == 1L) {
    model$internal$gam_setup
  } else {
    model$internal$delta_gam_setup
  }
  X <- if (component_index == 1L) {
    model$tmb_inputs$tmb_data$X_ij
  } else {
    model$tmb_inputs$tmb_data$X2_ij
  }
  labels <- attr(setup$pterms, "term.labels")
  term_columns <- lapply(seq_along(labels), function(i) which(setup$assign == i))
  names(term_columns) <- labels
  term_columns <- term_columns[lengths(term_columns) > 0L]
  weights <- .resolve_influ_weights(data, weights)
  focus_info <- .focus_info(data, focus)
  reference_design <- .weighted_col_mean(X, weights)
  term_deltas <- lapply(term_columns, function(columns) {
    B_term <- .term_contrast(X[, columns, drop = FALSE], focus_info, weights)
    B <- matrix(0, nrow = nrow(B_term), ncol = ncol(X))
    B[, columns] <- B_term
    draws %*% t(B)
  })
  list(
    family_spec = family_spec,
    eta_reference = as.numeric(draws %*% reference_design),
    term_deltas = term_deltas,
    method = if (nrow(draws) == 1L) "none" else "joint coefficient simulation"
  )
}

.tinyVAST_delta_mean_diag <- function(model, data, focus, specs, weights,
                                      uncertainty, retain, probs, ndraws,
                                      seed) {
  parameter_names <- names(model$sdrep$par.fixed)
  occurrence_index <- which(parameter_names == "alpha_j")
  positive_index <- which(parameter_names == "alpha2_j")
  occurrence_beta <- model$internal$parlist$alpha_j
  positive_beta <- model$internal$parlist$alpha2_j
  if (length(occurrence_index) != length(occurrence_beta) ||
      length(positive_index) != length(positive_beta)) {
    stop(
      "Could not align the joint tinyVAST fixed-effect covariance for the delta model.",
      call. = FALSE
    )
  }

  beta <- c(occurrence_beta, positive_beta)
  if (uncertainty == "none") {
    joint_draws <- matrix(beta, nrow = 1L)
  } else {
    fixed_index <- c(occurrence_index, positive_index)
    joint_draws <- .draw_mvn(
      ndraws,
      beta,
      model$sdrep$cov.fixed[fixed_index, fixed_index, drop = FALSE],
      seed = seed
    )
  }
  n_occurrence <- length(occurrence_beta)
  occurrence_draws <- joint_draws[, seq_len(n_occurrence), drop = FALSE]
  positive_draws <- joint_draws[, n_occurrence + seq_along(positive_beta), drop = FALSE]

  occurrence_projection <- .tinyVAST_fixed_projection(
    model, data, focus, 1L, occurrence_draws, specs$occurrence, weights
  )
  positive_projection <- .tinyVAST_fixed_projection(
    model, data, focus, 2L, positive_draws, specs$positive, weights
  )

  .two_part_combined_diag(
    backend = "tinyVAST",
    model = model,
    data = data,
    response = all.vars(model$formula)[1],
    focus = focus,
    family_spec = specs$overall,
    main_projection = positive_projection,
    probability_projection = occurrence_projection,
    probability_is_zero = FALSE,
    weights = weights,
    retain = retain,
    probs = probs,
    notes = paste0(
      "The unconditional delta mean combines occurrence and positive fixed ",
      if (uncertainty == "none") "effects at their estimates." else
        "effects using their joint covariance."
    )
  )
}

.tinyVAST_field_diag <- function(model, data, focus, component_index,
                                 component, family_spec, weights) {
  suffix <- as.character(component_index)
  what <- c(
    smooths = paste0("pgamma", suffix, "_g"),
    spatial_field = paste0("pomega", suffix, "_g"),
    spatiotemporal_field = paste0("pepsilon", suffix, "_g"),
    spatially_varying_effects = paste0("pxi", suffix, "_g")
  )

  pieces <- list()
  for (term in names(what)) {
    contribution <- tryCatch(
      tinyVAST::project(
        model,
        extra_times = numeric(0),
        newdata = data,
        what = what[[term]],
        future_var = FALSE,
        past_var = FALSE,
        parm_var = FALSE
      ),
      error = function(e) NULL
    )
    if (is.null(contribution) || length(contribution) != nrow(data)) next
    if (!any(abs(contribution) > sqrt(.Machine$double.eps), na.rm = TRUE)) next

    pieces[[term]] <- .influ_linear_engine(
      backend = "tinyVAST",
      model = model,
      data = data,
      response = NULL,
      focus = focus,
      family_spec = family_spec,
      X = matrix(contribution, ncol = 1L, dimnames = list(NULL, term)),
      beta = 1,
      vcov = NULL,
      term_columns = stats::setNames(list(1L), term),
      uncertainty = "none",
      retain = "summary",
      probs = c(0.025, 0.975),
      weights = weights,
      component = paste(component, term, sep = ":"),
      keep_model = FALSE,
      notes = paste0(term, " uses fitted latent modes; joint precision uncertainty is pending.")
    )
  }
  pieces
}

#' Influence diagnostics for tinyVAST models
#'
#' Fixed GAM terms use their marginal TMB covariance. Smooth, spatial,
#' spatiotemporal, and spatially varying contributions are obtained through
#' tinyVAST's component projection interface and reduced immediately to
#' compact focus-level diagnostics. Delta fixed effects are combined with their
#' joint covariance to obtain unconditional-mean influence.
#'
#' @inheritParams influ.glm
#' @param model A fitted `tinyVAST` object.
#'
#' @return An [influ_diag] object.
#' @export
influ.tinyVAST <- function(model, focus, data = NULL, weights = NULL,
                           uncertainty = "auto", retain = "summary",
                           probs = c(0.025, 0.975), ndraws = 1000L,
                           seed = NULL, draws_path = NULL,
                           keep_model = FALSE, ...) {
  if (!requireNamespace("tinyVAST", quietly = TRUE)) {
    stop("Package 'tinyVAST' is required for this model.", call. = FALSE)
  }
  if (is.null(model$sdrep) || is.null(model$internal$parlist)) {
    stop("The tinyVAST object must be fitted before influence is calculated.", call. = FALSE)
  }
  data <- if (is.null(data)) as.data.frame(model$data) else as.data.frame(data)
  specs <- .tinyVAST_family_specs(model)
  fam <- model$internal$family[[1]]
  is_delta <- isTRUE(fam$delta)
  component_retain <- if (retain == "disk") "derived_draws" else retain
  fixed_uncertainty <- if (uncertainty == "auto") "analytic" else uncertainty
  n_components <- if (is_delta) 2L else 1L

  components <- list()
  for (i in seq_len(n_components)) {
    component <- if (is_delta) c("occurrence", "positive")[i] else "conditional"
    spec <- if (is_delta) specs[[component]] else specs$conditional
    components[[paste0(component, "_fixed")]] <- .tinyVAST_fixed_diag(
      model, data, focus, i, component, spec, weights, fixed_uncertainty,
      component_retain, probs, ndraws, seed
    )
    components <- c(
      components,
      .tinyVAST_field_diag(model, data, focus, i, component, spec, weights)
    )
  }

  if (is_delta) {
    components$unconditional_mean <- .tinyVAST_delta_mean_diag(
      model, data, focus, specs, weights, fixed_uncertainty,
      component_retain, probs, ndraws, seed
    )
  }

  out <- .combine_influ_diags(
    components,
    backend = "tinyVAST",
    family_spec = specs$overall,
    focus = focus,
    model = model,
    keep_model = keep_model,
    notes = c(
      "Latent component modes are reduced through tinyVAST's projection interface.",
      if (is_delta) "Delta fixed effects include joint unconditional-mean influence.",
      "Sparse joint-precision uncertainty, unconditional-mean influence for latent fields, and multivariate mixed-family models are pending."
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
