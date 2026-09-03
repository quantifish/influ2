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
                               retain, probs, ndraws, seed) {
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
    component = component,
    ndraws = ndraws,
    seed = seed,
    keep_model = FALSE
  )
}

.sdmTMB_field_diag <- function(model, data, focus, component_index,
                               component, family_spec, weights) {
  report <- model$tmb_obj$report(model$last.par.best)
  candidates <- list(
    spatial_field = report$omega_s_A,
    spatiotemporal_field = report$epsilon_st_A_vec,
    iid_random_effects = report$eta_iid_re_i,
    smooths = report$eta_smooth_i,
    time_varying_effects = report$eta_rw_i
  )

  pieces <- list()
  for (term in names(candidates)) {
    value <- candidates[[term]]
    if (is.null(value) || !length(value)) next
    value <- as.matrix(value)
    if (ncol(value) < component_index || nrow(value) != nrow(data)) next
    contribution <- value[, component_index]
    if (!any(abs(contribution) > sqrt(.Machine$double.eps), na.rm = TRUE)) next

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
      component = paste(component, term, sep = ":"),
      keep_model = FALSE,
      notes = paste0(term, " uses empirical Bayes field modes; joint precision uncertainty is pending.")
    )
  }
  pieces
}

#' Influence diagnostics for sdmTMB models
#'
#' Fixed effects use the marginal covariance from TMB. Spatial,
#' spatiotemporal, IID, smooth, and time-varying contributions are projected
#' from the fitted fields without constructing dense draw arrays. Field
#' uncertainty is deliberately marked as pending until sparse joint-precision
#' simulation is available.
#'
#' @inheritParams influ.glm
#' @param model A fitted `sdmTMB` object.
#'
#' @return An [influ_diag] object.
#' @export
influ.sdmTMB <- function(model, focus, data = NULL, weights = NULL,
                         uncertainty = "auto", retain = "summary",
                         probs = c(0.025, 0.975), ndraws = 1000L,
                         seed = NULL, draws_path = NULL,
                         keep_model = FALSE, ...) {
  if (!requireNamespace("sdmTMB", quietly = TRUE)) {
    stop("Package 'sdmTMB' is required for this model.", call. = FALSE)
  }
  if (is.null(model$sd_report) || is.null(model$parlist)) {
    stop("The sdmTMB object must be fitted before influence is calculated.", call. = FALSE)
  }
  data <- if (is.null(data)) as.data.frame(model$data) else as.data.frame(data)
  specs <- .sdmTMB_family_specs(model)
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
      component_retain, probs, ndraws, seed
    )
    fields <- .sdmTMB_field_diag(
      model, data, focus, i, component, spec, weights
    )
    components <- c(components, fields)
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
      "Sparse joint-precision uncertainty for fields and combined delta-mean influence are pending."
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
