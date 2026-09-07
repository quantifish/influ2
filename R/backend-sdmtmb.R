.sdmTMB_family_specs <- function(model) {
  if (isTRUE(model$family$delta)) {
    list(
      overall = .new_family_spec(
        model$family$family[2], model$family$link[2], backend = "sdmTMB",
        response_structure = "hurdle"
      ),
      occurrence = .new_family_spec(
        "binomial", model$family$link[1], backend = "sdmTMB",
        response_structure = "hurdle"
      ),
      positive = .new_family_spec(
        model$family$family[2], model$family$link[2], backend = "sdmTMB",
        response_structure = "hurdle"
      )
    )
  } else {
    spec <- .new_family_spec(
      model$family$family, model$family$link, backend = "sdmTMB"
    )
    list(overall = spec, conditional = spec)
  }
}

.sdmTMB_fixed_diag <- function(model, data, focus, component_index,
                               component, family_spec, weights, uncertainty,
                               retain, probs, ndraws, seed,
                               reference_data = NULL,
                               reference_weights = NULL) {
  X <- model$tmb_data$X_ij[[component_index]]
  param <- if (component_index == 1L) "b_j" else "b_j2"
  beta <- model$parlist[[param]]
  fixed_names <- names(model$sd_report$par.fixed)
  fixed_index <- which(fixed_names == param)
  V <- if (length(fixed_index) == length(beta)) {
    model$sd_report$cov.fixed[fixed_index, fixed_index, drop = FALSE]
  } else {
    NULL
  }

  frm <- model$formula[[component_index]]
  labels <- attr(stats::terms(frm), "term.labels")
  assign <- attr(X, "assign")
  term_columns <- lapply(seq_along(labels), function(i) which(assign == i))
  names(term_columns) <- labels
  term_columns <- term_columns[lengths(term_columns) > 0L]
  if (!length(term_columns)) return(NULL)
  reference_X <- if (is.null(reference_data)) NULL else {
    prediction_data <- stats::predict(
      model,
      newdata = reference_data,
      re_form = NA,
      return_tmb_data = TRUE
    )
    .align_reference_matrix(
      prediction_data$proj_X_ij[[component_index]], X
    )
  }

  .influ_linear_engine(
    backend = "sdmTMB",
    model = model,
    data = data,
    response = if (component_index == 1L && !isTRUE(model$family$delta)) all.vars(frm)[1] else NULL,
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

.sdmTMB_fixed_projection <- function(model, data, focus, component_index,
                                     draws, family_spec, weights,
                                     reference_data = NULL,
                                     reference_weights = NULL) {
  X <- model$tmb_data$X_ij[[component_index]]
  frm <- model$formula[[component_index]]
  labels <- attr(stats::terms(frm), "term.labels")
  assign <- attr(X, "assign")
  term_columns <- lapply(seq_along(labels), function(i) which(assign == i))
  names(term_columns) <- labels
  term_columns <- term_columns[lengths(term_columns) > 0L]
  weights <- .resolve_influ_weights(data, weights)
  focus_info <- .focus_info(data, focus)
  reference_X <- if (is.null(reference_data)) {
    X
  } else {
    prediction_data <- stats::predict(
      model, newdata = reference_data, re_form = NA,
      return_tmb_data = TRUE
    )
    .align_reference_matrix(
      prediction_data$proj_X_ij[[component_index]], X
    )
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

.sdmTMB_delta_mean_diag <- function(model, data, focus, specs, weights,
                                    uncertainty, retain, probs, ndraws,
                                    seed, reference_data = NULL,
                                    reference_weights = NULL) {
  parameter_names <- names(model$sd_report$par.fixed)
  occurrence_index <- which(parameter_names == "b_j")
  positive_index <- which(parameter_names == "b_j2")
  occurrence_beta <- model$parlist$b_j
  positive_beta <- model$parlist$b_j2
  if (length(occurrence_index) != length(occurrence_beta) ||
      length(positive_index) != length(positive_beta)) {
    stop(
      "Could not align the joint sdmTMB fixed-effect covariance for the delta model.",
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
      model$sd_report$cov.fixed[fixed_index, fixed_index, drop = FALSE],
      seed = seed
    )
  }
  n_occurrence <- length(occurrence_beta)
  occurrence_draws <- joint_draws[, seq_len(n_occurrence), drop = FALSE]
  positive_draws <- joint_draws[, n_occurrence + seq_along(positive_beta), drop = FALSE]

  occurrence_projection <- .sdmTMB_fixed_projection(
    model, data, focus, 1L, occurrence_draws, specs$occurrence, weights,
    reference_data, reference_weights
  )
  positive_projection <- .sdmTMB_fixed_projection(
    model, data, focus, 2L, positive_draws, specs$positive, weights,
    reference_data, reference_weights
  )

  .two_part_combined_diag(
    backend = "sdmTMB",
    model = model,
    data = data,
    response = all.vars(model$formula[[1]])[1],
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

.sdmTMB_field_diag <- function(model, data, focus, component_index,
                               component, family_spec, weights,
                               reference_data = NULL,
                               reference_weights = NULL) {
  report <- model$tmb_obj$report(model$last.par.best)
  candidates <- list(
    spatial_field = report$omega_s_A,
    spatiotemporal_field = report$epsilon_st_A_vec,
    iid_random_effects = report$eta_iid_re_i,
    smooths = report$eta_smooth_i,
    time_varying_effects = report$eta_rw_i
  )
  reference_report <- if (is.null(reference_data)) NULL else {
    prediction <- stats::predict(
      model, newdata = reference_data, return_tmb_object = TRUE
    )
    prediction$obj$report(model$last.par.best)
  }
  projected_names <- c(
    spatial_field = "proj_omega_s_A",
    spatiotemporal_field = "proj_epsilon_st_A_vec",
    spatially_varying_effects = "proj_zeta_s_A"
  )

  pieces <- list()
  for (term in names(candidates)) {
    value <- candidates[[term]]
    if (is.null(value) || !length(value)) next
    value <- as.matrix(value)
    if (ncol(value) < component_index || nrow(value) != nrow(data)) next
    contribution <- value[, component_index]
    if (!any(abs(contribution) > sqrt(.Machine$double.eps), na.rm = TRUE)) next

    reference_contribution <- NULL
    if (!is.null(reference_report)) {
      if (!term %in% names(projected_names)) next
      reference_contribution <- .sdmTMB_report_component(
        reference_report, projected_names[[term]], component_index,
        nrow(reference_data)
      )
      if (is.null(reference_contribution)) next
    }

    pieces[[term]] <- .influ_linear_engine(
      backend = "sdmTMB",
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
      notes = paste0(term, " uses empirical Bayes field modes because uncertainty = 'none'.")
    )
  }
  pieces
}

.sdmTMB_report_component <- function(report, name, component_index,
                                     n_observations) {
  value <- report[[name]]
  if (is.null(value) || !length(value)) return(NULL)
  dimensions <- dim(value)
  if (is.null(dimensions)) {
    if (length(value) != n_observations || component_index != 1L) return(NULL)
    return(as.numeric(value))
  }
  if (length(dimensions) == 2L) {
    if (dimensions[1] != n_observations || dimensions[2] < component_index) return(NULL)
    return(as.numeric(value[, component_index]))
  }
  if (length(dimensions) == 3L) {
    if (dimensions[1] != n_observations || dimensions[3] < component_index) return(NULL)
    return(rowSums(value[, , component_index, drop = FALSE], dims = 1L))
  }
  NULL
}

.sdmTMB_joint_field_projections <- function(model, data, focus, specs,
                                            weights, ndraws, seed = NULL,
                                            reference_data = NULL,
                                            reference_weights = NULL,
                                            batch_size = 25L,
                                            probs = c(0.025, 0.975)) {
  report_mode <- model$tmb_obj$report(model$last.par.best)
  candidate_names <- c(
    spatial_field = "omega_s_A",
    spatiotemporal_field = "epsilon_st_A_vec",
    iid_random_effects = "eta_iid_re_i",
    smooths = "eta_smooth_i",
    time_varying_effects = "eta_rw_i",
    spatially_varying_effects = "zeta_s_A"
  )
  projected_names <- c(
    spatial_field = "proj_omega_s_A",
    spatiotemporal_field = "proj_epsilon_st_A_vec",
    spatially_varying_effects = "proj_zeta_s_A"
  )
  n_components <- if (isTRUE(model$family$delta)) 2L else 1L
  focus_info <- .focus_info(data, focus)
  weights <- .resolve_influ_weights(data, weights)
  reference_weights <- if (is.null(reference_data)) {
    weights
  } else {
    .resolve_influ_weights(reference_data, reference_weights)
  }
  prediction_obj <- NULL
  if (!is.null(reference_data)) {
    prediction_obj <- stats::predict(
      model,
      newdata = reference_data,
      return_tmb_object = TRUE
    )$obj
  }

  active <- vector("list", n_components)
  point <- vector("list", n_components)
  for (component_index in seq_len(n_components)) {
    point[[component_index]] <- lapply(candidate_names, function(name) {
      .sdmTMB_report_component(
        report_mode, name, component_index, nrow(data)
      )
    })
    point[[component_index]] <- Filter(function(x) {
      !is.null(x) && any(abs(x) > sqrt(.Machine$double.eps), na.rm = TRUE)
    }, point[[component_index]])
    if (!is.null(reference_data)) {
      point[[component_index]] <- point[[component_index]][
        intersect(names(point[[component_index]]), names(projected_names))
      ]
    }
    active[[component_index]] <- names(point[[component_index]])
  }
  if (!any(lengths(active))) return(NULL)

  sd_report <- model$sd_report
  if (is.null(sd_report$jointPrecision)) {
    sd_report <- TMB::sdreport(model$tmb_obj, getJointPrecision = TRUE)
  }
  if (is.null(sd_report$jointPrecision)) {
    stop("sdmTMB did not provide a joint precision matrix for latent-field uncertainty.", call. = FALSE)
  }
  factor <- Matrix::Cholesky(sd_report$jointPrecision, super = TRUE)
  if (!is.null(seed)) set.seed(seed)
  eta_reference <- lapply(seq_len(n_components), function(i) numeric(ndraws))
  term_deltas <- lapply(seq_len(n_components), function(component_index) {
    stats::setNames(
      lapply(active[[component_index]], function(term) {
        matrix(NA_real_, nrow = ndraws, ncol = length(focus_info$levels))
      }),
      active[[component_index]]
    )
  })
  coefficient_groups <- lapply(seq_len(n_components), function(component_index) {
    stats::setNames(lapply(active[[component_index]], function(term) {
      .cdi_level_groups(data, term, point[[component_index]][[term]], weights)
    }), active[[component_index]])
  })
  coefficient_draws <- lapply(coefficient_groups, function(terms) {
    lapply(terms, function(groups) {
      matrix(NA_real_, nrow = ndraws, ncol = length(groups),
        dimnames = list(NULL, names(groups)))
    })
  })
  centred_coefficient_draws <- coefficient_draws

  starts <- seq.int(1L, ndraws, by = batch_size)
  for (start in starts) {
    batch_n <- min(batch_size, ndraws - start + 1L)
    z <- matrix(
      stats::rnorm(length(model$last.par.best) * batch_n),
      nrow = length(model$last.par.best), ncol = batch_n
    )
    z <- Matrix::solve(factor, z, system = "Lt")
    z <- Matrix::solve(factor, z, system = "Pt")
    samples <- sweep(as.matrix(z), 1L, model$last.par.best, "+")
    for (j in seq_len(batch_n)) {
      draw_index <- start + j - 1L
      report <- model$tmb_obj$report(samples[, j])
      reference_report <- if (is.null(prediction_obj)) NULL else {
        prediction_obj$report(samples[, j])
      }
      for (component_index in seq_len(n_components)) {
        eta <- .sdmTMB_report_component(
          report, "eta_i", component_index, nrow(data)
        )
        if (is.null(eta)) {
          stop("Could not recover sdmTMB observation-level predictors from a joint draw.", call. = FALSE)
        }
        if (is.null(reference_report)) {
          eta_reference[[component_index]][draw_index] <- stats::weighted.mean(eta, weights)
        } else {
          reference_eta <- .sdmTMB_report_component(
            reference_report, "proj_eta", component_index,
            nrow(reference_data)
          )
          eta_reference[[component_index]][draw_index] <- stats::weighted.mean(
            reference_eta, reference_weights
          )
        }
        for (term in active[[component_index]]) {
          contribution <- .sdmTMB_report_component(
            report, candidate_names[[term]], component_index, nrow(data)
          )
          reference_contribution <- if (is.null(reference_report)) NULL else {
            .sdmTMB_report_component(
              reference_report, projected_names[[term]], component_index,
              nrow(reference_data)
            )
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
    component <- if (isTRUE(model$family$delta)) {
      c("occurrence", "positive")[component_index]
    } else {
      "conditional"
    }
    spec <- if (isTRUE(model$family$delta)) specs[[component]] else specs$conditional
    coefficient_summaries <- lapply(active[[component_index]], function(term) {
      raw <- coefficient_draws[[component_index]][[term]]
      centred <- centred_coefficient_draws[[component_index]][[term]]
      do.call(rbind, lapply(seq_len(ncol(raw)), function(i) {
        .cdi_summary_row(
          term = term, level = colnames(raw)[i],
          estimate = mean(raw[, i]), centred_estimate = mean(centred[, i]),
          draws = raw[, i], centred_draws = centred[, i],
          family_spec = spec, method = "joint precision simulation", probs = probs
        )
      }))
    })
    names(coefficient_summaries) <- active[[component_index]]
    list(
      component = component,
      family_spec = spec,
      eta_reference = eta_reference[[component_index]],
      term_deltas = term_deltas[[component_index]],
      point_contributions = point[[component_index]],
      coefficient_summaries = coefficient_summaries,
      reference = if (is.null(reference_data)) "observed" else "prediction_grid",
      method = "joint precision simulation"
    )
  })
}

.sdmTMB_joint_field_diags <- function(model, data, focus, projections,
                                      weights, retain, probs,
                                      reference = "observed") {
  lapply(projections, function(projection) {
    .diag_from_term_draws(
      backend = "sdmTMB",
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

#' Influence diagnostics for sdmTMB models
#'
#' Fixed effects use the marginal covariance from TMB. Spatial,
#' spatiotemporal, IID, smooth, and time-varying contributions are projected
#' from the fitted fields without retaining dense draw arrays. Field uncertainty
#' uses sparse joint-precision simulation and is reduced directly to compact
#' focus-level estimands. Delta fixed effects and latent fields are combined
#' draw by draw to obtain unconditional-mean influence.
#' Lognormal diagnostics require a log link for the arithmetic mean. Offsets
#' have the same single-component restrictions as the GLM adapter.
#'
#' @inheritParams influ.glm
#' @param model A fitted `sdmTMB` object.
#'
#' @return An [influ_diag] object.
#' @export
influ.sdmTMB <- function(model, focus, data = NULL, weights = NULL,
                         reference_data = NULL, reference_weights = NULL,
                         uncertainty = "auto", retain = "summary",
                         probs = c(0.025, 0.975), ndraws = 1000L,
                         seed = NULL, draws_path = NULL,
                         keep_model = FALSE, ...) {
  if (!requireNamespace("sdmTMB", quietly = TRUE)) {
    stop("Package 'sdmTMB' is required for this model.", call. = FALSE)
  }
  uncertainty <- match.arg(
    uncertainty,
    c("auto", "none", "analytic", "simulation")
  )
  ndraws <- .validate_ndraws(ndraws)
  if (is.null(model$sd_report) || is.null(model$parlist)) {
    stop("The sdmTMB object must be fitted before influence is calculated.", call. = FALSE)
  }
  data <- if (is.null(data)) as.data.frame(model$data) else as.data.frame(data)
  specs <- .sdmTMB_family_specs(model)
  .check_influ_lognormal_mean_link(specs$overall)
  offset_sources <- .influ_offset_sources(
    model$formula, model$call, list(model$offset, model$tmb_data$offset_i)
  )
  .check_influ_offset_scope(specs$overall, offset_sources)
  component_retain <- if (retain == "disk") "derived_draws" else retain
  fixed_uncertainty <- if (uncertainty == "auto") "analytic" else uncertainty
  n_components <- if (isTRUE(model$family$delta)) 2L else 1L

  components <- list()
  for (i in seq_len(n_components)) {
    component <- if (isTRUE(model$family$delta)) {
      c("occurrence", "positive")[i]
    } else {
      "conditional"
    }
    spec <- if (isTRUE(model$family$delta)) specs[[component]] else specs$conditional
    components[[paste0(component, "_fixed")]] <- .sdmTMB_fixed_diag(
      model, data, focus, i, component, spec, weights, fixed_uncertainty,
      component_retain, probs, ndraws, seed,
      reference_data, reference_weights
    )
    if (uncertainty == "none") {
      fields <- .sdmTMB_field_diag(
        model, data, focus, i, component, spec, weights,
        reference_data, reference_weights
      )
      components <- c(components, fields)
    }
  }


  joint_projections <- NULL
  if (uncertainty != "none") {
    joint_projections <- .sdmTMB_joint_field_projections(
      model, data, focus, specs, weights, ndraws, seed,
      reference_data, reference_weights, probs = probs
    )
    if (!is.null(joint_projections)) {
      field_diags <- .sdmTMB_joint_field_diags(
        model, data, focus, joint_projections, weights,
        component_retain, probs,
        if (is.null(reference_data)) "observed" else "prediction_grid"
      )
      names(field_diags) <- paste0("joint_fields_", seq_along(field_diags))
      components <- c(components, field_diags)
    }
  }

  if (isTRUE(model$family$delta)) {
    components$unconditional_mean <- .sdmTMB_delta_mean_diag(
      model, data, focus, specs, weights, fixed_uncertainty,
      component_retain, probs, ndraws, seed,
      reference_data, reference_weights
    )
    if (!is.null(joint_projections)) {
      components$unconditional_latent_fields <- .two_part_combined_diag(
        backend = "sdmTMB",
        model = model,
        data = data,
        response = all.vars(model$formula[[1]])[1],
        focus = focus,
        family_spec = specs$overall,
        main_projection = joint_projections[[2]],
        probability_projection = joint_projections[[1]],
        probability_is_zero = FALSE,
        weights = weights,
        retain = component_retain,
        probs = probs,
        notes = "Occurrence and positive latent fields are combined using the same joint precision draws."
      )
    }
  }

  out <- .combine_influ_diags(
    components,
    backend = "sdmTMB",
    family_spec = specs$overall,
    focus = focus,
    model = model,
    keep_model = keep_model,
    notes = c(
      "Spatial and spatiotemporal field modes are included as separate components.",
      if (isTRUE(model$family$delta)) "Delta fixed effects include joint unconditional-mean influence.",
      if (uncertainty != "none") "Latent fields use sparse joint-precision uncertainty reduced to compact focus-level draws.",
      if (isTRUE(model$family$delta) && uncertainty != "none") "Delta latent fields are combined draw by draw for unconditional-mean influence."
    )
  )

  if (retain == "disk") {
    if (is.null(draws_path)) stop("`draws_path` is required when `retain = \"disk\"`.", call. = FALSE)
    saveRDS(out$draws, draws_path, compress = FALSE)
    out$draws <- NULL
    out$retained$mode <- "disk"
    out$retained$path <- normalizePath(draws_path, mustWork = TRUE)
  }
  .record_influ_offset_scope(out, offset_sources)
}
