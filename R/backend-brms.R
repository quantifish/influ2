.brms_available_variables <- function(model) {
  if (!is.null(model$influ2_draws)) {
    return(colnames(model$influ2_draws))
  }
  posterior::variables(model)
}

.brms_as_draws_matrix <- function(model, variables = NULL) {
  if (!is.null(model$influ2_draws)) {
    draws <- posterior::as_draws_matrix(model$influ2_draws)
    if (!is.null(variables)) draws <- draws[, variables, drop = FALSE]
    return(draws)
  }
  posterior::as_draws_matrix(model, variable = variables)
}

.brms_reference_standata <- function(model, newdata) {
  newdata <- as.data.frame(newdata)
  responses <- model$formula$resp %||% character()
  missing_responses <- setdiff(responses, names(newdata))
  for (response in missing_responses) {
    if (is.null(model$data[[response]])) {
      stop(
        "`reference_data` must contain the brms response variable '",
        response, "'.",
        call. = FALSE
      )
    }
    observed <- model$data[[response]]
    observed <- observed[!is.na(observed)]
    if (!length(observed)) {
      stop("Could not construct the brms reference model matrix.", call. = FALSE)
    }
    newdata[[response]] <- rep(observed[1], nrow(newdata))
  }
  brms::standata(model, newdata = newdata)
}

.brms_population_matrix <- function(model, dpar = NULL, newdata = NULL) {
  standata <- if (is.null(newdata)) {
    brms::standata(model)
  } else {
    .brms_reference_standata(model, newdata)
  }
  suffix <- if (is.null(dpar) || dpar == "mu") "" else paste0("_", dpar)
  X <- standata[[paste0("X", suffix)]]
  if (is.null(X)) return(NULL)

  formula <- if (is.null(dpar) || dpar == "mu") {
    model$formula$formula
  } else {
    model$formula$pforms[[dpar]]
  }
  if (is.null(formula)) return(NULL)

  labels <- attr(stats::terms(formula), "term.labels")
  assign <- attr(X, "assign")
  columns <- lapply(seq_along(labels), function(i) which(assign == i))
  names(columns) <- labels
  columns <- columns[lengths(columns) > 0L]

  list(X = X, term_columns = columns)
}

.brms_parameter_names <- function(X, dpar = NULL) {
  prefix <- if (is.null(dpar) || dpar == "mu") "b_" else paste0("b_", dpar, "_")
  paste0(prefix, colnames(X))
}

.brms_population_draws <- function(model, X, dpar = NULL, ndraws = NULL,
                                   keep_draws = TRUE) {
  ndraws <- .validate_ndraws(ndraws, allow_null = TRUE)
  variables <- .brms_parameter_names(X, dpar)
  available <- .brms_available_variables(model)
  missing <- setdiff(variables, available)
  if (length(missing)) {
    stop(
      "The brms population-level parameters could not be aligned with its ",
      "model matrix: ", paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }

  draws <- .brms_as_draws_matrix(model, variables)
  if (!is.null(ndraws) && nrow(draws) > ndraws) {
    keep <- unique(round(seq(1, nrow(draws), length.out = ndraws)))
    draws <- draws[keep, , drop = FALSE]
  }
  colnames(draws) <- colnames(X)
  beta <- colMeans(draws)
  if (!keep_draws) draws <- NULL
  list(beta = beta, draws = draws)
}

.brms_draw_matrix <- function(model, variables, ndraws = NULL) {
  ndraws <- .validate_ndraws(ndraws, allow_null = TRUE)
  available <- .brms_available_variables(model)
  missing <- setdiff(variables, available)
  if (length(missing)) {
    stop(
      "The following brms parameters could not be found: ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  draws <- .brms_as_draws_matrix(model, variables)
  if (!is.null(ndraws) && nrow(draws) > ndraws) {
    keep <- unique(round(seq(1, nrow(draws), length.out = ndraws)))
    draws <- draws[keep, , drop = FALSE]
  }
  draws
}

.brms_group_contrast <- function(J, Z, focus_info, weights, n_groups,
                                 reference_J = J, reference_Z = Z,
                                 reference_weights = weights) {
  group_sum <- function(values, groups) {
    totals <- numeric(n_groups)
    observed <- rowsum(values, groups, reorder = FALSE)
    totals[as.integer(rownames(observed))] <- observed[, 1]
    totals
  }

  overall <- group_sum(
    reference_weights * reference_Z, reference_J
  ) / sum(reference_weights)
  contrast <- matrix(
    0,
    nrow = length(focus_info$levels),
    ncol = n_groups,
    dimnames = list(focus_info$levels, NULL)
  )
  for (i in seq_along(focus_info$levels)) {
    keep <- focus_info$value == focus_info$levels[i]
    if (sum(weights[keep]) <= 0) {
      stop(
        "Every focus level must have a positive total weight; level '",
        focus_info$levels[i], "' does not.",
        call. = FALSE
      )
    }
    contrast[i, ] <- group_sum(weights[keep] * Z[keep], J[keep]) /
      sum(weights[keep]) - overall
  }
  list(contrast = contrast, overall = overall)
}

.brms_group_diag <- function(model, data, focus, group_rows, component,
                             family_spec, weights, uncertainty, retain,
                             probs, ndraws, reference_data = NULL,
                             reference_weights = NULL) {
  standata <- brms::standata(model)
  reference_standata <- if (is.null(reference_data)) {
    standata
  } else {
    .brms_reference_standata(model, reference_data)
  }
  focus_info <- .focus_info(data, focus)
  weights <- .resolve_influ_weights(data, weights)
  reference_weights <- if (is.null(reference_data)) {
    weights
  } else {
    .resolve_influ_weights(reference_data, reference_weights)
  }
  group <- group_rows$group[1]
  group_levels <- attr(model$ranef, "levels")[[group]]
  if (is.null(group_levels)) {
    stop("Could not recover levels for brms group '", group, "'.", call. = FALSE)
  }

  parameter_names <- character()
  contrasts <- list()
  overall_design <- list()
  observation_design <- list()
  slices <- list()
  offset <- 0L

  for (i in seq_len(nrow(group_rows))) {
    gn <- group_rows$gn[i]
    cn <- group_rows$cn[i]
    coefficient <- group_rows$coef[i]
    J <- standata[[paste0("J_", gn)]]
    Z <- standata[[paste0("Z_", gn, "_", cn)]]
    reference_J <- reference_standata[[paste0("J_", gn)]]
    reference_Z <- reference_standata[[paste0("Z_", gn, "_", cn)]]
    if (is.null(J) || is.null(Z)) next

    parameters <- paste0(
      "r_", group, "[", group_levels, ",", coefficient, "]"
    )
    if (!all(parameters %in% .brms_available_variables(model))) {
      stop(
        "Could not align posterior group-level parameters for '", group, "'.",
        call. = FALSE
      )
    }
    part <- .brms_group_contrast(
      J = as.integer(J),
      Z = as.numeric(Z),
      focus_info = focus_info,
      weights = weights,
      n_groups = length(group_levels),
      reference_J = as.integer(reference_J),
      reference_Z = as.numeric(reference_Z),
      reference_weights = reference_weights
    )
    parameter_names <- c(parameter_names, parameters)
    contrasts[[i]] <- part$contrast
    overall_design[[i]] <- part$overall
    observation_design[[i]] <- list(J = as.integer(J), Z = as.numeric(Z))
    slices[[i]] <- offset + seq_along(parameters)
    offset <- offset + length(parameters)
  }
  if (!length(parameter_names)) return(NULL)

  draws <- .brms_draw_matrix(model, parameter_names, ndraws)
  beta <- colMeans(draws)
  contribution <- numeric(nrow(data))
  for (i in seq_along(slices)) {
    design <- observation_design[[i]]
    contribution <- contribution +
      design$Z * beta[slices[[i]]][design$J]
  }
  B <- do.call(cbind, contrasts)
  overall <- unlist(overall_design, use.names = FALSE)
  eta_reference <- sum(overall * beta)
  eta_reference_draws <- as.numeric(draws %*% overall)
  term <- group

  diagnostic <- .influ_precomputed_engine(
    backend = "brms",
    model = model,
    data = data,
    focus = focus,
    family_spec = family_spec,
    term_contrasts = stats::setNames(list(B), term),
    term_contributions = stats::setNames(list(contribution), term),
    beta = beta,
    beta_draws = if (uncertainty == "none") NULL else draws,
    uncertainty = if (uncertainty == "none") "none" else "posterior",
    retain = retain,
    probs = probs,
    weights = weights,
    component = paste(component, "group_level", sep = ":"),
    eta_reference = eta_reference,
    eta_reference_draws = if (uncertainty == "none") NULL else eta_reference_draws,
    reference = if (is.null(reference_data)) "observed" else "prediction_grid",
    notes = paste0(
      "Group-level term '", group,
      "' is projected directly from joint posterior draws without an observation-by-draw array."
    )
  )
  keep <- weights > 0 & is.finite(contribution)
  contribution_levels <- .term_level_values(
    data[keep, , drop = FALSE], term, contribution[keep]
  )
  level_rows <- split(
    which(keep),
    factor(contribution_levels, levels = unique(contribution_levels))
  )
  coefficient_rows <- lapply(names(level_rows), function(level) {
    rows <- level_rows[[level]]
    design <- numeric(length(beta))
    for (i in seq_along(slices)) {
      part <- observation_design[[i]]
      group_totals <- rowsum(
        weights[rows] * part$Z[rows],
        part$J[rows],
        reorder = FALSE
      )
      values <- numeric(length(group_levels))
      values[as.integer(rownames(group_totals))] <- group_totals[, 1]
      design[slices[[i]]] <- values / sum(weights[rows])
    }
    centred_design <- design - overall
    row <- .cdi_summary_row(
      term = term,
      level = level,
      estimate = sum(beta * design),
      centred_estimate = sum(beta * centred_design),
      draws = if (uncertainty == "none") NULL else {
        as.numeric(draws %*% design)
      },
      centred_draws = if (uncertainty == "none") NULL else {
        as.numeric(draws %*% centred_design)
      },
      family_spec = family_spec,
      method = if (uncertainty == "none") "none" else "posterior draws",
      probs = probs
    )
    row$component <- paste(component, "group_level", sep = ":")
    row
  })
  diagnostic$coefficients <- do.call(rbind, coefficient_rows)
  rownames(diagnostic$coefficients) <- NULL
  diagnostic
}

.brms_group_diags <- function(model, data, focus, component, family_spec,
                              weights, uncertainty, retain, probs, ndraws,
                              reference_data = NULL,
                              reference_weights = NULL) {
  if (!nrow(model$ranef)) return(list())
  rows <- model$ranef
  rows <- rows[rows$dpar %in% c("", "mu"), , drop = FALSE]
  if (!nrow(rows)) return(list())
  groups <- split(rows, rows$gn)
  lapply(groups, function(group_rows) {
    .brms_group_diag(
      model, data, focus, group_rows, component, family_spec, weights,
      uncertainty, retain, probs, ndraws,
      reference_data, reference_weights
    )
  })
}

.brms_smooth_diags <- function(model, data, focus, component, family_spec,
                               weights, uncertainty, retain, probs, ndraws,
                               reference_data = NULL,
                               reference_weights = NULL) {
  standata <- brms::standata(model)
  reference_standata <- if (is.null(reference_data)) {
    standata
  } else {
    .brms_reference_standata(model, reference_data)
  }
  Xs <- standata$Xs
  smooth_columns <- attr(Xs, "smcols")
  smooth_labels <- names(attr(Xs, "bylevels"))
  if (is.null(Xs) || !length(smooth_columns)) return(list())
  if (is.null(smooth_labels) || length(smooth_labels) != length(smooth_columns)) {
    smooth_labels <- paste0("smooth_", seq_along(smooth_columns))
  }

  focus_info <- .focus_info(data, focus)
  weights <- .resolve_influ_weights(data, weights)
  reference_weights <- if (is.null(reference_data)) {
    weights
  } else {
    .resolve_influ_weights(reference_data, reference_weights)
  }
  available <- .brms_available_variables(model)
  out <- vector("list", length(smooth_columns))

  for (i in seq_along(smooth_columns)) {
    columns <- smooth_columns[[i]]
    design_parts <- list(Xs[, columns, drop = FALSE])
    reference_design_parts <- list(
      reference_standata$Xs[, columns, drop = FALSE]
    )
    parameter_parts <- list(paste0("bs_", colnames(Xs)[columns]))
    z_names <- grep(
      paste0("^Zs_", i, "_[0-9]+$"),
      names(standata),
      value = TRUE
    )
    z_names <- z_names[order(as.integer(sub("^.*_", "", z_names)))]
    for (z_name in z_names) {
      Z <- as.matrix(standata[[z_name]])
      penalty <- as.integer(sub("^.*_", "", z_name))
      label <- attr(Z, "s.label") %||% smooth_labels[i]
      safe_label <- gsub("[^[:alnum:]_]", "", label)
      parameters <- paste0(
        "s_", safe_label, "_", penalty, "[", seq_len(ncol(Z)), "]"
      )
      if (!all(parameters %in% available)) {
        stop(
          "Could not align posterior smooth parameters for '",
          smooth_labels[i], "'.",
          call. = FALSE
        )
      }
      design_parts[[length(design_parts) + 1L]] <- Z
      reference_design_parts[[length(reference_design_parts) + 1L]] <-
        as.matrix(reference_standata[[z_name]])
      parameter_parts[[length(parameter_parts) + 1L]] <- parameters
    }

    design <- do.call(cbind, design_parts)
    reference_design <- do.call(cbind, reference_design_parts)
    parameters <- unlist(parameter_parts, use.names = FALSE)
    draws <- .brms_draw_matrix(model, parameters, ndraws)
    beta <- colMeans(draws)
    B <- .term_contrast(
      design, focus_info, weights,
      reference_design, reference_weights
    )
    contribution <- as.numeric(design %*% beta)
    overall <- .weighted_col_mean(reference_design, reference_weights)
    eta_reference <- sum(overall * beta)
    eta_reference_draws <- as.numeric(draws %*% overall)
    term <- smooth_labels[i]

    out[[i]] <- .influ_precomputed_engine(
      backend = "brms",
      model = model,
      data = data,
      focus = focus,
      family_spec = family_spec,
      term_contrasts = stats::setNames(list(B), term),
      term_contributions = stats::setNames(list(contribution), term),
      beta = beta,
      beta_draws = if (uncertainty == "none") NULL else draws,
      uncertainty = if (uncertainty == "none") "none" else "posterior",
      retain = retain,
      probs = probs,
      weights = weights,
      component = paste(component, "smooth", sep = ":"),
      eta_reference = eta_reference,
      eta_reference_draws = if (uncertainty == "none") NULL else eta_reference_draws,
      reference = if (is.null(reference_data)) "observed" else "prediction_grid",
      notes = paste0(
        "Smooth term '", term,
        "' is projected directly from joint posterior basis coefficients."
      )
    )
    out[[i]]$coefficients <- .coefficient_term_summary(
      data = data,
      term = term,
      X = design,
      beta = beta,
      weights = weights,
      beta_draws = if (uncertainty == "none") NULL else draws,
      method = if (uncertainty == "none") "none" else "posterior draws",
      probs = probs,
      reference_design = overall,
      family_spec = family_spec
    )
    out[[i]]$coefficients$component <- paste(component, "smooth", sep = ":")
  }
  Filter(Negate(is.null), out)
}

.brms_component_diag <- function(model, data, focus, dpar, component,
                                 family_spec, weights, uncertainty, retain,
                                 probs, ndraws, draws_path,
                                 reference_data = NULL,
                                 reference_weights = NULL) {
  matrix_info <- .brms_population_matrix(model, dpar)
  if (is.null(matrix_info) || !length(matrix_info$term_columns)) return(NULL)

  use_draws <- uncertainty != "none"
  posterior <- .brms_population_draws(
    model, matrix_info$X, dpar, ndraws = ndraws, keep_draws = use_draws
  )
  reference_X <- if (is.null(reference_data)) NULL else {
    reference_info <- .brms_population_matrix(model, dpar, reference_data)
    .align_reference_matrix(reference_info$X, matrix_info$X)
  }

  .influ_linear_engine(
    backend = "brms",
    model = model,
    data = data,
    response = if (is.null(dpar) || dpar == "mu") names(data)[1] else NULL,
    focus = focus,
    family_spec = family_spec,
    X = matrix_info$X,
    beta = posterior$beta,
    vcov = NULL,
    term_columns = matrix_info$term_columns,
    uncertainty = if (uncertainty == "none") "none" else "posterior",
    retain = retain,
    probs = probs,
    weights = weights,
    reference_data = reference_data,
    reference_X = reference_X,
    reference_weights = reference_weights,
    component = component,
    beta_draws = posterior$draws,
    draws_path = draws_path,
    keep_model = FALSE
  )
}

.brms_population_projection <- function(model, data, focus, dpar,
                                        family_spec, weights, uncertainty,
                                        ndraws, reference_data = NULL,
                                        reference_weights = NULL) {
  matrix_info <- .brms_population_matrix(model, dpar)
  if (is.null(matrix_info)) return(NULL)
  posterior <- .brms_population_draws(
    model,
    matrix_info$X,
    dpar,
    ndraws = ndraws,
    keep_draws = TRUE
  )
  draws <- posterior$draws
  if (uncertainty == "none") {
    draws <- matrix(
      posterior$beta,
      nrow = 1L,
      dimnames = list(NULL, names(posterior$beta))
    )
  }
  weights <- .resolve_influ_weights(data, weights)
  focus_info <- .focus_info(data, focus)
  reference_X <- if (is.null(reference_data)) {
    matrix_info$X
  } else {
    .align_reference_matrix(
      .brms_population_matrix(model, dpar, reference_data)$X,
      matrix_info$X
    )
  }
  reference_weights <- if (is.null(reference_data)) {
    weights
  } else {
    .resolve_influ_weights(reference_data, reference_weights)
  }
  reference_design <- .weighted_col_mean(reference_X, reference_weights)
  term_deltas <- lapply(matrix_info$term_columns, function(columns) {
    B_term <- .term_contrast(
      matrix_info$X[, columns, drop = FALSE],
      focus_info,
      weights,
      reference_X[, columns, drop = FALSE],
      reference_weights
    )
    B <- matrix(0, nrow = nrow(B_term), ncol = ncol(matrix_info$X))
    B[, columns] <- B_term
    draws %*% t(B)
  })

  list(
    family_spec = family_spec,
    eta_reference = as.numeric(draws %*% reference_design),
    term_deltas = term_deltas,
    reference = if (is.null(reference_data)) "observed" else "prediction_grid",
    method = if (uncertainty == "none") "none" else "posterior draws"
  )
}

#' Influence diagnostics for brms models
#'
#' Population-level posterior coefficients are projected directly into the
#' compact focus-by-term contrasts. The method therefore never constructs an
#' observation-by-draw-by-term array. Joint posterior dependence is preserved
#' while calculating the requested diagnostics.
#'
#' @inheritParams influ.glm
#' @param model A fitted object from [brms::brm()].
#' @param uncertainty Either `"auto"`, `"posterior"`, or `"none"`. The
#'   `"none"` mode reduces the posterior to coefficient means before influence
#'   calculations.
#' @param ndraws Optional maximum number of posterior draws. `NULL` uses every
#'   available draw.
#'
#' @return An [influ_diag] object.
#' @export
influ.brmsfit <- function(model, focus, data = NULL, weights = NULL,
                          reference_data = NULL, reference_weights = NULL,
                          uncertainty = "auto", retain = "summary",
                          probs = c(0.025, 0.975), ndraws = NULL,
                          seed = NULL, draws_path = NULL,
                          keep_model = FALSE, ...) {
  if (!requireNamespace("brms", quietly = TRUE) ||
      !requireNamespace("posterior", quietly = TRUE)) {
    stop("Packages 'brms' and 'posterior' are required for this model.", call. = FALSE)
  }
  uncertainty <- match.arg(uncertainty, c("auto", "posterior", "none"))
  if (uncertainty == "auto") uncertainty <- "posterior"
  data <- if (is.null(data)) as.data.frame(model$data) else as.data.frame(data)

  response_structure <- .normalise_family_name(model$family$family)$structure
  overall_spec <- .new_family_spec(
    model$family$family,
    model$family$link,
    backend = "brms",
    response_structure = response_structure
  )
  component_retain <- if (retain == "disk") "derived_draws" else retain
  main_component <- if (response_structure == "hurdle") {
    "positive"
  } else if (response_structure == "zero_inflated") {
    "count"
  } else {
    "conditional"
  }

  components <- list(
    conditional = .brms_component_diag(
      model, data, focus, NULL, main_component, overall_spec, weights,
      uncertainty, component_retain, probs, ndraws, NULL,
      reference_data, reference_weights
    )
  )

  group_components <- .brms_group_diags(
    model, data, focus, main_component, overall_spec, weights,
    uncertainty, component_retain, probs, ndraws,
    reference_data, reference_weights
  )
  if (length(group_components)) {
    names(group_components) <- paste0("group_", seq_along(group_components))
    components <- c(components, group_components)
  }
  smooth_components <- .brms_smooth_diags(
    model, data, focus, main_component, overall_spec, weights,
    uncertainty, component_retain, probs, ndraws,
    reference_data, reference_weights
  )
  if (length(smooth_components)) {
    names(smooth_components) <- paste0("smooth_", seq_along(smooth_components))
    components <- c(components, smooth_components)
  }

  probability_dpar <- intersect(c("hu", "zi"), model$family$dpars)
  if (length(probability_dpar)) {
    dpar <- probability_dpar[1]
    probability_link <- model$family[[paste0("link_", dpar)]] %||% "logit"
    probability_spec <- .new_family_spec(
      "binomial", probability_link, backend = "brms",
      response_structure = response_structure,
      complement = dpar == "hu"
    )
    components$probability <- .brms_component_diag(
      model, data, focus, dpar,
      if (dpar == "hu") "occurrence" else "zero_probability",
      probability_spec, weights, uncertainty, component_retain, probs,
      ndraws, NULL, reference_data, reference_weights
    )

    main_projection <- .brms_population_projection(
      model, data, focus, NULL, overall_spec, weights, uncertainty, ndraws,
      reference_data, reference_weights
    )
    probability_projection <- .brms_population_projection(
      model, data, focus, dpar, probability_spec, weights, uncertainty, ndraws,
      reference_data, reference_weights
    )
    components$unconditional_mean <- .two_part_combined_diag(
      backend = "brms",
      model = model,
      data = data,
      response = names(data)[1],
      focus = focus,
      family_spec = overall_spec,
      main_projection = main_projection,
      probability_projection = probability_projection,
      probability_is_zero = TRUE,
      weights = weights,
      retain = component_retain,
      probs = probs,
      notes = paste0(
        "The unconditional mean combines population-level ", dpar,
        " and response components draw by draw."
      )
    )
  }

  out <- .combine_influ_diags(
    components,
    backend = "brms",
    family_spec = overall_spec,
    focus = focus,
    model = model,
    keep_model = keep_model,
    notes = c(
      "Population-level terms use compact posterior projections; no observation-by-draw arrays are retained.",
      "Group-level and smooth terms use compact joint posterior projections when present.",
      if (response_structure != "single") "Population-level hurdle or zero-inflated components are combined draw by draw for unconditional-mean influence."
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
