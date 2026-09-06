.tinyVAST_family_specs <- function(model, distribution = NULL) {
  families <- model$internal$family
  if (is.null(distribution)) {
    if (length(families) != 1L) {
      stop("`distribution` is required for a mixed-family tinyVAST model.", call. = FALSE)
    }
    fam <- families[[1]]
  } else {
    if (!distribution %in% names(families)) {
      stop("No tinyVAST family is defined for distribution '", distribution, "'.", call. = FALSE)
    }
    fam <- families[[distribution]]
  }
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

.tinyVAST_reference_matrix <- function(model, reference_data,
                                       component_index, fitted_X) {
  setup <- if (component_index == 1L) {
    model$internal$gam_setup
  } else {
    model$internal$delta_gam_setup
  }
  Terms <- stats::delete.response(setup$pterms)
  reference_X <- stats::model.matrix(Terms, reference_data)
  .align_reference_matrix(reference_X, fitted_X)
}

.tinyVAST_fixed_diag <- function(model, data, focus, component_index,
                                 component, family_spec, weights,
                                 uncertainty, retain, probs, ndraws, seed,
                                 row_index = seq_len(nrow(data)),
                                 reference_data = NULL,
                                 reference_weights = NULL) {
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
  X <- X[row_index, , drop = FALSE]
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
  reference_X <- if (is.null(reference_data)) NULL else {
    .tinyVAST_reference_matrix(
      model, reference_data, component_index, X
    )
  }

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
    reference_data = reference_data,
    reference_X = reference_X,
    reference_weights = reference_weights,
    component = component,
    ndraws = ndraws,
    seed = seed,
    keep_model = FALSE
  )
}

.tinyVAST_fixed_projection <- function(model, data, focus, component_index,
                                       draws, family_spec, weights,
                                       row_index = seq_len(nrow(data)),
                                       reference_data = NULL,
                                       reference_weights = NULL) {
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
  X <- X[row_index, , drop = FALSE]
  labels <- attr(setup$pterms, "term.labels")
  term_columns <- lapply(seq_along(labels), function(i) which(setup$assign == i))
  names(term_columns) <- labels
  term_columns <- term_columns[lengths(term_columns) > 0L]
  weights <- .resolve_influ_weights(data, weights)
  focus_info <- .focus_info(data, focus)
  reference_X <- if (is.null(reference_data)) X else {
    .tinyVAST_reference_matrix(model, reference_data, component_index, X)
  }
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

.tinyVAST_delta_mean_diag <- function(model, data, focus, specs, weights,
                                      uncertainty, retain, probs, ndraws,
                                      seed, row_index = seq_len(nrow(data)),
                                      reference_data = NULL,
                                      reference_weights = NULL) {
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
    model, data, focus, 1L, occurrence_draws, specs$occurrence, weights,
    row_index, reference_data, reference_weights
  )
  positive_projection <- .tinyVAST_fixed_projection(
    model, data, focus, 2L, positive_draws, specs$positive, weights,
    row_index, reference_data, reference_weights
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
                                 component, family_spec, weights,
                                 row_index = seq_len(nrow(data)),
                                 reference_data = NULL,
                                 reference_weights = NULL) {
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
    if (is.null(contribution) || length(contribution) < max(row_index)) next
    contribution <- contribution[row_index]
    if (length(contribution) != nrow(data)) next
    if (!any(abs(contribution) > sqrt(.Machine$double.eps), na.rm = TRUE)) next

    reference_contribution <- if (is.null(reference_data)) NULL else {
      tryCatch(
        tinyVAST::project(
          model,
          extra_times = numeric(0),
          newdata = reference_data,
          what = what[[term]],
          future_var = FALSE,
          past_var = FALSE,
          parm_var = FALSE
        ),
        error = function(e) NULL
      )
    }
    if (!is.null(reference_data) &&
        (is.null(reference_contribution) || length(reference_contribution) != nrow(reference_data))) next

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
      reference_data = reference_data,
      reference_X = if (is.null(reference_contribution)) NULL else {
        matrix(reference_contribution, ncol = 1L, dimnames = list(NULL, term))
      },
      reference_weights = reference_weights,
      component = paste(component, term, sep = ":"),
      keep_model = FALSE,
      notes = paste0(term, " uses fitted latent modes because uncertainty = 'none'.")
    )
  }
  pieces
}

.tinyVAST_joint_field_projections <- function(model, data, focus, specs,
                                              weights, ndraws, seed = NULL,
                                              row_index = seq_len(nrow(data)),
                                              reference_data = NULL,
                                              reference_weights = NULL,
                                              component_prefix = "",
                                              batch_size = 25L,
                                              probs = c(0.025, 0.975)) {
  n_components <- if (isTRUE(specs$overall$structure == "hurdle")) 2L else 1L
  focus_info <- .focus_info(data, focus)
  weights <- .resolve_influ_weights(data, weights)
  reference_weights <- if (is.null(reference_data)) {
    weights
  } else {
    .resolve_influ_weights(reference_data, reference_weights)
  }
  terms <- c("spatial_field", "spatiotemporal_field")
  prediction_data <- tinyVAST::add_predictions(
    model, model$data, remove_origdata = FALSE
  )
  prediction_obj <- TMB::MakeADFun(
    data = prediction_data,
    parameters = model$internal$parlist,
    map = model$tmb_inputs$tmb_map,
    random = model$tmb_inputs$tmb_random,
    profile = model$internal$control$profile,
    DLL = "tinyVAST",
    silent = TRUE
  )
  reference_prediction_obj <- NULL
  if (!is.null(reference_data)) {
    reference_prediction_data <- tinyVAST::add_predictions(
      model, reference_data, remove_origdata = FALSE
    )
    reference_prediction_obj <- TMB::MakeADFun(
      data = reference_prediction_data,
      parameters = model$internal$parlist,
      map = model$tmb_inputs$tmb_map,
      random = model$tmb_inputs$tmb_random,
      profile = model$internal$control$profile,
      DLL = "tinyVAST",
      silent = TRUE
    )
  }
  report_mode <- prediction_obj$report(model$obj$env$last.par.best)
  projection_name <- function(term, component_index) {
    prefix <- switch(
      term,
      spatial_field = "pomega",
      spatiotemporal_field = "pepsilon"
    )
    paste0(prefix, component_index, "_g")
  }
  point_all <- lapply(seq_len(n_components), function(component_index) {
    out <- lapply(terms, function(term) {
      value <- report_mode[[projection_name(term, component_index)]]
      if (is.null(value)) NULL else as.numeric(value)[row_index]
    })
    names(out) <- terms
    Filter(function(x) {
      !is.null(x) && any(abs(x) > sqrt(.Machine$double.eps), na.rm = TRUE)
    }, out)
  })
  if (!any(lengths(point_all))) return(NULL)

  sd_report <- model$sdrep
  if (is.null(sd_report$jointPrecision)) {
    sd_report <- TMB::sdreport(model$obj, getJointPrecision = TRUE)
  }
  if (is.null(sd_report$jointPrecision)) {
    stop("tinyVAST did not provide a joint precision matrix for latent-field uncertainty.", call. = FALSE)
  }
  factor <- Matrix::Cholesky(sd_report$jointPrecision, super = TRUE)
  if (!is.null(seed)) set.seed(seed)
  eta_reference <- lapply(seq_len(n_components), function(i) numeric(ndraws))
  term_deltas <- lapply(seq_len(n_components), function(component_index) {
    stats::setNames(
      lapply(names(point_all[[component_index]]), function(term) {
        matrix(NA_real_, nrow = ndraws, ncol = length(focus_info$levels))
      }),
      names(point_all[[component_index]])
    )
  })
  coefficient_groups <- lapply(seq_len(n_components), function(component_index) {
    term_names <- names(point_all[[component_index]])
    stats::setNames(lapply(term_names, function(term) {
      .cdi_level_groups(data, term, point_all[[component_index]][[term]], weights)
    }), term_names)
  })
  coefficient_draws <- lapply(coefficient_groups, function(terms) {
    lapply(terms, function(groups) {
      matrix(NA_real_, nrow = ndraws, ncol = length(groups),
        dimnames = list(NULL, names(groups)))
    })
  })
  centred_coefficient_draws <- coefficient_draws

  for (start in seq.int(1L, ndraws, by = batch_size)) {
    batch_n <- min(batch_size, ndraws - start + 1L)
    z <- matrix(
      stats::rnorm(length(model$obj$env$last.par.best) * batch_n),
      nrow = length(model$obj$env$last.par.best), ncol = batch_n
    )
    z <- Matrix::solve(factor, z, system = "Lt")
    z <- Matrix::solve(factor, z, system = "Pt")
    samples <- sweep(as.matrix(z), 1L, model$obj$env$last.par.best, "+")
    for (j in seq_len(batch_n)) {
      draw_index <- start + j - 1L
      report <- prediction_obj$report(samples[, j])
      reference_report <- if (is.null(reference_prediction_obj)) NULL else {
        reference_prediction_obj$report(samples[, j])
      }
      for (component_index in seq_len(n_components)) {
        eta_name <- paste0("p", component_index, "_g")
        eta <- as.numeric(report[[eta_name]])[row_index]
        if (is.null(reference_report)) {
          eta_reference[[component_index]][draw_index] <- stats::weighted.mean(eta, weights)
        } else {
          reference_eta <- as.numeric(reference_report[[eta_name]])
          eta_reference[[component_index]][draw_index] <- stats::weighted.mean(
            reference_eta, reference_weights
          )
        }
        for (term in names(point_all[[component_index]])) {
          contribution <- as.numeric(
            report[[projection_name(term, component_index)]]
          )[row_index]
          reference_contribution <- if (is.null(reference_report)) NULL else {
            as.numeric(reference_report[[projection_name(term, component_index)]])
          }
          term_deltas[[component_index]][[term]][draw_index, ] <-
            .compact_contrast(
              contribution, data, focus, weights,
              reference_contribution, reference_weights
            )
          level_values <- vapply(
            coefficient_groups[[component_index]][[term]],
            function(rows) stats::weighted.mean(contribution[rows], weights[rows]),
            numeric(1)
          )
          centre <- stats::weighted.mean(
            reference_contribution %||% contribution, reference_weights
          )
          coefficient_draws[[component_index]][[term]][draw_index, ] <- level_values
          centred_coefficient_draws[[component_index]][[term]][draw_index, ] <-
            level_values - centre
        }
      }
    }
  }

  lapply(seq_len(n_components), function(component_index) {
    component <- if (n_components == 2L) {
      c("occurrence", "positive")[component_index]
    } else {
      "conditional"
    }
    if (nzchar(component_prefix)) component <- paste(component_prefix, component, sep = ":")
    spec_name <- if (n_components == 2L) c("occurrence", "positive")[component_index] else "conditional"
    coefficient_summaries <- lapply(names(point_all[[component_index]]), function(term) {
      raw <- coefficient_draws[[component_index]][[term]]
      centred <- centred_coefficient_draws[[component_index]][[term]]
      do.call(rbind, lapply(seq_len(ncol(raw)), function(i) {
        .cdi_summary_row(
          term = term, level = colnames(raw)[i],
          estimate = mean(raw[, i]), centred_estimate = mean(centred[, i]),
          draws = raw[, i], centred_draws = centred[, i],
          family_spec = specs[[spec_name]],
          method = "joint precision simulation", probs = probs
        )
      }))
    })
    names(coefficient_summaries) <- names(point_all[[component_index]])
    list(
      component = component,
      family_spec = specs[[spec_name]],
      eta_reference = eta_reference[[component_index]],
      term_deltas = term_deltas[[component_index]],
      point_contributions = point_all[[component_index]],
      coefficient_summaries = coefficient_summaries,
      reference = if (is.null(reference_data)) "observed" else "prediction_grid",
      method = "joint precision simulation"
    )
  })
}

.tinyVAST_joint_field_diags <- function(model, data, focus, projections,
                                        weights, retain, probs,
                                        reference = "observed") {
  lapply(projections, function(projection) {
    .diag_from_term_draws(
      backend = "tinyVAST",
      model = model,
      data = data,
      focus = focus,
      family_spec = projection$family_spec,
      term_draws = projection$term_deltas,
      point_contributions = projection$point_contributions,
      coefficient_summaries = projection$coefficient_summaries,
      eta_reference_draws = projection$eta_reference,
      weights = weights,
      component = paste0(projection$component, ":latent_fields"),
      retain = retain,
      probs = probs,
      method = projection$method,
      reference = reference,
      notes = "Latent-field uncertainty uses joint sparse-precision draws reduced immediately to focus-level estimands."
    )
  })
}

.prefix_diag_components <- function(x, prefix) {
  if (is.null(x) || !nzchar(prefix)) return(x)
  for (table in c("influence", "coefficients", "composition", "indices", "metrics")) {
    if (is.data.frame(x[[table]]) && nrow(x[[table]]) && "component" %in% names(x[[table]])) {
      x[[table]]$component <- paste(prefix, x[[table]]$component, sep = ":")
    }
  }
  if (!is.null(x$draws) && ncol(x$draws)) {
    colnames(x$draws) <- paste(prefix, colnames(x$draws), sep = ":")
  }
  x
}

.tinyVAST_response_diag <- function(model, data, row_index, focus, specs,
                                    weights, uncertainty, retain, probs,
                                    ndraws, seed, prefix = "",
                                    reference_data = NULL,
                                    reference_weights = NULL) {
  is_delta <- identical(specs$overall$structure, "hurdle")
  fixed_uncertainty <- if (uncertainty == "auto") "analytic" else uncertainty
  n_components <- if (is_delta) 2L else 1L
  components <- list()

  for (component_index in seq_len(n_components)) {
    component <- if (is_delta) {
      c("occurrence", "positive")[component_index]
    } else {
      "conditional"
    }
    spec <- if (is_delta) specs[[component]] else specs$conditional
    components[[paste0(component, "_fixed")]] <- .tinyVAST_fixed_diag(
      model, data, focus, component_index, component, spec, weights,
      fixed_uncertainty, retain, probs, ndraws, seed, row_index,
      reference_data, reference_weights
    )
    if (uncertainty == "none") {
      components <- c(
        components,
        .tinyVAST_field_diag(
          model, data, focus, component_index, component, spec, weights,
          row_index, reference_data, reference_weights
        )
      )
    }
  }

  joint_projections <- NULL
  if (uncertainty != "none") {
    joint_projections <- .tinyVAST_joint_field_projections(
      model, data, focus, specs, weights, ndraws, seed,
      row_index = row_index,
      reference_data = reference_data,
      reference_weights = reference_weights,
      probs = probs
    )
    if (!is.null(joint_projections)) {
      field_diags <- .tinyVAST_joint_field_diags(
        model, data, focus, joint_projections, weights, retain, probs,
        if (is.null(reference_data)) "observed" else "prediction_grid"
      )
      names(field_diags) <- paste0("joint_fields_", seq_along(field_diags))
      components <- c(components, field_diags)
    }
  }

  if (is_delta) {
    components$unconditional_mean <- .tinyVAST_delta_mean_diag(
      model, data, focus, specs, weights, fixed_uncertainty,
      retain, probs, ndraws, seed, row_index,
      reference_data, reference_weights
    )
    if (!is.null(joint_projections)) {
      components$unconditional_latent_fields <- .two_part_combined_diag(
        backend = "tinyVAST",
        model = model,
        data = data,
        response = all.vars(model$formula)[1],
        focus = focus,
        family_spec = specs$overall,
        main_projection = joint_projections[[2]],
        probability_projection = joint_projections[[1]],
        probability_is_zero = FALSE,
        weights = weights,
        retain = retain,
        probs = probs,
        notes = "Occurrence and positive latent fields are combined using the same joint precision draws."
      )
    }
  }

  out <- .combine_influ_diags(
    components,
    backend = "tinyVAST",
    family_spec = specs$overall,
    focus = focus,
    model = model,
    keep_model = FALSE,
    notes = c(
      "Latent component modes are reduced through tinyVAST's projection interface.",
      if (is_delta) "Delta fixed effects include joint unconditional-mean influence.",
      if (uncertainty != "none") "Spatial and spatiotemporal fields use sparse joint-precision uncertainty.",
      if (is_delta && uncertainty != "none") "Delta latent fields are combined draw by draw for unconditional-mean influence."
    )
  )
  .prefix_diag_components(out, prefix)
}

#' Influence diagnostics for tinyVAST models
#'
#' Fixed GAM terms use their marginal TMB covariance. Smooth, spatial,
#' spatiotemporal, and spatially varying contributions are obtained through
#' tinyVAST's component projection interface and reduced immediately to
#' compact focus-level diagnostics. Spatial and spatiotemporal uncertainty uses
#' sparse joint-precision simulation. Delta fixed effects and latent fields are
#' combined draw by draw to obtain unconditional-mean influence. Multivariate
#' mixed-family responses are returned as response-labelled components.
#'
#' @inheritParams influ.glm
#' @param model A fitted `tinyVAST` object.
#'
#' @return An [influ_diag] object.
#' @export
influ.tinyVAST <- function(model, focus, data = NULL, weights = NULL,
                           reference_data = NULL, reference_weights = NULL,
                           uncertainty = "auto", retain = "summary",
                           probs = c(0.025, 0.975), ndraws = 1000L,
                           seed = NULL, draws_path = NULL,
                           keep_model = FALSE, ...) {
  if (!requireNamespace("tinyVAST", quietly = TRUE)) {
    stop("Package 'tinyVAST' is required for this model.", call. = FALSE)
  }
  uncertainty <- match.arg(
    uncertainty,
    c("auto", "none", "analytic", "simulation")
  )
  ndraws <- .validate_ndraws(ndraws)
  if (is.null(model$sdrep) || is.null(model$internal$parlist)) {
    stop("The tinyVAST object must be fitted before influence is calculated.", call. = FALSE)
  }
  data <- if (is.null(data)) as.data.frame(model$data) else as.data.frame(data)
  component_retain <- if (retain == "disk") "derived_draws" else retain
  variable_column <- model$internal$variable_column
  distribution_column <- model$internal$distribution_column
  if (!all(c(variable_column, distribution_column) %in% names(data))) {
    stop("The tinyVAST variable and distribution columns were not found in `data`.", call. = FALSE)
  }
  responses <- unique(as.character(data[[variable_column]]))
  response_diags <- lapply(responses, function(response) {
    row_index <- which(as.character(data[[variable_column]]) == response)
    response_data <- data[row_index, , drop = FALSE]
    distributions <- unique(as.character(response_data[[distribution_column]]))
    if (length(distributions) != 1L) {
      stop("Each tinyVAST response must use one distribution for influence diagnostics.", call. = FALSE)
    }
    distribution <- if (length(model$internal$family) == 1L) NULL else distributions
    specs <- .tinyVAST_family_specs(model, distribution)
    response_weights <- if (is.numeric(weights) && length(weights) == nrow(data)) {
      weights[row_index]
    } else {
      weights
    }
    response_reference <- NULL
    response_reference_weights <- NULL
    if (!is.null(reference_data)) {
      reference_data <- as.data.frame(reference_data)
      if (!variable_column %in% names(reference_data)) {
        stop("`reference_data` must contain the tinyVAST variable column.", call. = FALSE)
      }
      reference_index <- which(
        as.character(reference_data[[variable_column]]) == response
      )
      if (!length(reference_index)) {
        stop("`reference_data` has no rows for tinyVAST response '", response, "'.", call. = FALSE)
      }
      response_reference <- reference_data[reference_index, , drop = FALSE]
      response_reference_weights <- if (is.numeric(reference_weights) &&
          length(reference_weights) == nrow(reference_data)) {
        reference_weights[reference_index]
      } else {
        reference_weights
      }
    }
    .tinyVAST_response_diag(
      model, response_data, row_index, focus, specs, response_weights,
      uncertainty, component_retain, probs, ndraws, seed,
      prefix = if (length(responses) > 1L) response else "",
      reference_data = response_reference,
      reference_weights = response_reference_weights
    )
  })

  if (length(response_diags) == 1L) {
    out <- response_diags[[1]]
  } else {
    mixed_spec <- response_diags[[1]]$family
    mixed_spec$family <- "mixed"
    mixed_spec$link <- "varies"
    mixed_spec$structure <- "multivariate"
    out <- .combine_influ_diags(
      response_diags,
      backend = "tinyVAST",
      family_spec = mixed_spec,
      focus = focus,
      model = model,
      keep_model = keep_model,
      notes = "Multivariate tinyVAST responses are represented by response-labelled components."
    )
    out$metadata$family_by_response <- stats::setNames(
      lapply(response_diags, `[[`, "family"), responses
    )
  }
  if (isTRUE(keep_model)) out$model <- model

  if (retain == "disk") {
    if (is.null(draws_path)) stop("`draws_path` is required when `retain = \"disk\"`.", call. = FALSE)
    saveRDS(out$draws, draws_path, compress = FALSE)
    out$draws <- NULL
    out$retained$mode <- "disk"
    out$retained$path <- normalizePath(draws_path, mustWork = TRUE)
  }
  out
}
