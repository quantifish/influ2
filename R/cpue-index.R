#' Calculate an assessment-ready CPUE index
#'
#' Calculate expected-response indices over the same explicit reference
#' population in every year, or extract the existing year-effect contrasts.
#' No model is fitted or updated by this function.
#' @md
#'
#' @param model A fitted GLM, `mgcv` GAM, `glmmTMB`, complete `brmsfit`,
#'   `sdmTMB`, or univariate `tinyVAST` model for
#'   response standardisation. For `method = "year_effect"`, any model supported
#'   by [influ()] or an existing `influ_diag` can be supplied.
#' @param year Name of the year variable. Defaults to the native time variable
#'   for spatial backends, otherwise the first formula predictor.
#' @param method `"standardised"` (default), its alias `"standardized"`, or
#'   `"year_effect"`. The latter is a contrast, not an expected-response index.
#' @param reference_data A non-empty data frame defining a common covariate
#'   profile or population, **without** the year column. Each row is predicted
#'   in every observed year. Supply all required predictors and any exposure
#'   variables explicitly; use one unit of exposure for a per-unit index.
#' @param reference_weights Non-negative weights, one per reference row.
#'   Defaults to equal weights. These are standardisation weights, not areas.
#' @param uncertainty `"auto"` for propagated uncertainty, or `"none"` for
#'   a point-estimate preview with missing uncertainty columns.
#' @param probs Two increasing interval probabilities.
#' @param rescale `"raw"` (default), or a positive target geometric mean across
#'   all returned years. A common normalisation is applied to each posterior
#'   draw; frequentist uncertainty includes the normalising denominator.
#' @param ndraws Maximum number of existing posterior draws for brms, or number
#'   of joint Gaussian parameter/field draws for spatial backends. The same
#'   draw identities are shared across years and prediction batches.
#' @param batch_size Maximum reference rows predicted together.
#' @param draw_batch_size Maximum posterior draws predicted together.
#' @param retain `"summary"` (default) or `"draws"`. Draw retention is available
#'   for standardised brms and spatial indices and stores only a draw-by-year
#'   matrix. Spatial draws are a joint Gaussian approximation, not MCMC.
#' @param units Optional response units, such as `"lobsters per pot"`.
#' @param spatial_fields Spatial-backend prediction target: `"all"` includes
#'   persistent, spatially varying, and spatiotemporal fields; `"spatial"`
#'   excludes spatiotemporal fields; `"spatiotemporal"` excludes persistent and
#'   spatially varying fields; `"none"` excludes all three. This changes only
#'   predictions, not the fitted model. Other smooths/time effects remain.
#' @param seed Non-negative integer seed for spatial joint draws. The caller's
#'   random-number state is restored; changing batch sizes preserves draws.
#' @param prediction_offset For sdmTMB only, the name of a numeric link-scale
#'   offset column in `reference_data`. `NULL` explicitly uses offset zero
#'   (one unit of exposure for a log-exposure offset). Other backends obtain
#'   offsets from their formula and the supplied reference predictors.
#' @param ... Arguments passed to [influ()] only for `method = "year_effect"`.
#'
#' @details For standardisation, predictions are averaged on the **response**
#'   scale, not averaged on the link scale and then back-transformed. Supplied
#'   reference weights are treated as known. Hurdle and zero-inflated fits use
#'   the backend's combined expected response, including its zero component.
#'   Binomial GLM/glmmTMB predictions are probabilities; brms binomial expected
#'   predictions are counts at the supplied number of trials. Supply one trial
#'   for comparable per-trial indices. Log-transformed responses are rejected:
#'   fit an explicit lognormal model instead of silently exponentiating a fit.
#'
#'   GLMs and GAMs use the delta method and their joint coefficient covariance.
#'   GAM smooths, including random-effect smooths, are evaluated as fitted; the
#'   smoothing-parameter correction is used when available. glmmTMB uses the
#'   joint fitted-parameter covariance and numerical derivatives of native
#'   response predictions, with conditional random effects set to zero. REML
#'   and zero-component random effects are not yet supported for glmmTMB here.
#'   brms uses [brms::posterior_epred()] with group-level effects set to zero.
#'   Setting a random effect to zero is **not** integration over its population
#'   distribution. brms smooths and other population-level terms remain present.
#'
#'   `Mean` is the fitted expected-response estimate for frequentist models,
#'   and the posterior mean of the expected-response index for brms. `Median`
#'   is a posterior median and is `NA` for frequentist estimates, rather than
#'   labelling an MLE as a posterior median. `SD` is the standard error or
#'   posterior standard deviation of the index, not observation dispersion.
#'   `CV` is `SD / Mean` when the mean is positive. Positive frequentist indices
#'   have delta-method log-scale intervals; other indices have normal intervals.
#'   These intervals are pointwise, not simultaneous. Preview mode retains the
#'   brms posterior mean, but omits uncertainty summaries; it does not replace
#'   joint predictions with predictions at posterior-mean coefficients.
#'
#'   Working storage is bounded by reference and draw batches plus a compact
#'   annual covariance or draw matrix. Native prediction code can allocate
#'   additional memory. The result does not retain the model or reference data.
#'   Spatial response estimates evaluate the fitted model at the reference
#'   locations in each observed year. sdmTMB IID group effects are set to zero;
#'   tinyVAST non-spatial temporal effects and smooths remain as fitted. Joint
#'   fixed/latent Gaussian draws propagate field and parameter uncertainty,
#'   with empirical pointwise intervals. `Mean` remains the plug-in expected
#'   response and `Median` remains unavailable for these frequentist models.
#'   This is not a Laplace bias-corrected index or integration over a new
#'   population of random effects. Grid predictions are immediately reduced
#'   to annual values; a grid-by-draw array is never retained.
#'   Use [integrate_index()] for area-weighted totals, which have different units.
#'   Year-effect results preserve the original diagnostic estimand and cannot
#'   be rescaled by this function. These indices are not biomass estimates.
#'
#' @return An S3 `influ_index` object containing `table`, `metadata`, and optional
#'   `draws`. `as.data.frame()` returns the assessment table with `Year`, `Mean`,
#'   `Median`, `SD`, `CV`, `Qlower`, `Qupper`, `Method`, `Distribution`, and `Link`.
#' @seealso [integrate_index()], [plot_index()], [plot_compare()], [geo_mean()], [influ_indices()]
#' @examples
#' if (requireNamespace("glmmTMB", quietly = TRUE)) {
#'   data(lobsters_per_pot)
#'   fit <- glmmTMB::glmmTMB(lobsters ~ year + depth + (1 | month),
#'     family = glmmTMB::nbinom2(), data = lobsters_per_pot)
#'   index <- cpue_index(fit, year = "year",
#'     reference_data = data.frame(depth = 40), units = "lobsters per pot")
#'   head(as.data.frame(index))
#'   plot_index(index)
#' }
#' @export
cpue_index <- function(model, year = NULL,
    method = c("standardised", "standardized", "year_effect"),
    reference_data = NULL, reference_weights = NULL,
    uncertainty = c("auto", "none"), probs = c(0.025, 0.975),
    rescale = "raw", ndraws = 1000L, batch_size = 250L,
    draw_batch_size = 100L, retain = c("summary", "draws"), units = NULL, ...,
    spatial_fields = c("all", "spatial", "spatiotemporal", "none"),
    seed = 1L, prediction_offset = NULL) {
  .require_model_backend(model)
  method <- match.arg(method)
  if (method == "standardized") method <- "standardised"
  uncertainty <- match.arg(uncertainty)
  retain <- match.arg(retain)
  spatial_fields <- match.arg(spatial_fields)
  spatial_backend <- if (inherits(model, "sdmTMB")) "sdmTMB" else if (
    inherits(model, "tinyVAST")) "tinyVAST" else NULL
  info <- if (!is.null(spatial_backend) && method != "year_effect") {
    .index_spatial_info(model, spatial_backend, year)
  } else NULL
  if (!is.null(info)) year <- info$year
  probs <- .validate_probs(probs)
  batch_size <- .validate_ndraws(batch_size)
  draw_batch_size <- .validate_ndraws(draw_batch_size)
  ndraws <- .validate_ndraws(ndraws)
  if (!is.null(units) && (!is.character(units) || length(units) != 1L ||
      is.na(units) || !nzchar(trimws(units)))) {
    stop("`units` must be NULL or one non-empty string.", call. = FALSE)
  }
  raw <- identical(rescale, "raw")
  if (!raw && (!is.numeric(rescale) || length(rescale) != 1L ||
      !is.finite(rescale) || rescale <= 0)) {
    stop("`rescale` must be 'raw' or a positive finite number.", call. = FALSE)
  }
  if (retain == "draws" && uncertainty == "none") {
    stop("Draw retention requires `uncertainty = 'auto'`.", call. = FALSE)
  }
  year <- .comparison_focus(model, year)
  if (!is.character(year) || length(year) != 1L || is.na(year) || !nzchar(year)) {
    stop("`year` must name one model variable.", call. = FALSE)
  }
  if (method == "year_effect") {
    if (!raw || !is.null(reference_data) || !is.null(reference_weights) ||
        retain != "summary" || spatial_fields != "all" || !is.null(prediction_offset)) {
      stop("Year-effect extraction does not accept reference data, rescaling, or draw retention; configure `influ()` first.", call. = FALSE)
    }
    diagnostic <- if (inherits(model, "influ_diag")) model else {
      influ(model, focus = year, uncertainty = uncertainty, probs = probs, ...)
    }
    if (!identical(diagnostic$focus, year)) {
      stop("`year` does not match the diagnostic focus.", call. = FALSE)
    }
    d <- .comparison_index_data(diagnostic)
    tab <- data.frame(Year = d$level, Mean = d$estimate, Median = NA_real_,
      SD = d$std_error, CV = NA_real_, Qlower = d$lower, Qupper = d$upper)
    if (uncertainty == "none") tab[c("SD", "Qlower", "Qupper")] <- NA_real_
    if (unique(d$scale) == "ratio") {
      tab$CV <- ifelse(tab$Mean > 0, tab$SD / tab$Mean, NA_real_)
    }
    # A precomputed diagnostic owns its interval probabilities.
    return(.new_cpue_index(tab, list(method = method, year = year,
      backend = diagnostic$backend, family = diagnostic$family$family,
      link = diagnostic$family$link, units = units, scale = unique(d$scale),
      uncertainty = if (uncertainty == "none") "none" else diagnostic$uncertainty$method,
      interval_source = "original diagnostic", component = unique(d$component))))
  }
  if (length(list(...))) {
    stop("Additional arguments are only accepted for `method = 'year_effect'`.", call. = FALSE)
  }
  backend <- if (inherits(model, "gam")) "gam" else if (inherits(model, "glm")) {
    "glm"
  } else if (inherits(model, "glmmTMB")) "glmmTMB" else if (inherits(model, "brmsfit")) {
    "brms"
  } else if (!is.null(spatial_backend)) {
    spatial_backend
  } else {
    stop("Response standardisation supports GLM, GAM, glmmTMB, complete brms, sdmTMB, and univariate tinyVAST fits.", call. = FALSE)
  }
  if (retain == "draws" && !backend %in% c("brms", "sdmTMB", "tinyVAST")) {
    stop("Draw retention is currently available only for brms and spatial response indices.", call. = FALSE)
  }
  if (is.null(spatial_backend) && spatial_fields != "all") {
    stop("`spatial_fields` is an sdmTMB/tinyVAST prediction option.", call. = FALSE)
  }
  if (!is.null(prediction_offset) && (backend != "sdmTMB" ||
      !is.character(prediction_offset) || length(prediction_offset) != 1L ||
      is.na(prediction_offset))) {
    stop("`prediction_offset` must name an sdmTMB reference-data column.", call. = FALSE)
  }
  if (!is.data.frame(reference_data) || !nrow(reference_data) ||
      anyDuplicated(names(reference_data)) ||
      year %in% names(reference_data)) {
    stop("Supply non-empty `reference_data` without the year column: the same reference population is used in every year.", call. = FALSE)
  }
  weights <- reference_weights %||% rep(1, nrow(reference_data))
  if (!is.numeric(weights) || length(weights) != nrow(reference_data) ||
      any(!is.finite(weights)) || any(weights < 0) || !any(weights > 0)) {
    stop("`reference_weights` must be finite, non-negative, and have positive total weight, with one weight per reference row.", call. = FALSE)
  }
  keep <- weights > 0
  reference_data <- as.data.frame(reference_data[keep, , drop = FALSE])
  weights <- weights[keep] / max(weights[keep])
  weights <- weights / sum(weights)
  if (anyNA(reference_data)) {
    stop("Reference predictors must not contain missing values on positive-weight rows.", call. = FALSE)
  }
  frame <- if (is.null(info)) .residual_model_frame(model) else info$data
  if (is.null(frame[[year]])) {
    stop("The raw year variable must be present in the stored model data.", call. = FALSE)
  }
  years <- .focus_info(frame, year)$levels
  year_values <- frame[[year]][match(years, as.character(frame[[year]]))]
  native_time <- if (!is.null(info)) info$time else NULL
  time_values <- NULL
  if (!is.null(native_time) && !identical(native_time, year)) {
    if (native_time %in% names(reference_data)) {
      stop("Do not supply a fixed native time column in reference_data; it is mapped from `year`.", call. = FALSE)
    }
    by_year <- lapply(years, function(y) unique(frame[[native_time]][as.character(frame[[year]]) == y]))
    if (any(lengths(by_year) != 1L) || anyNA(unlist(by_year))) {
      stop("`year` must map to exactly one native model time per year; use the native time variable otherwise.", call. = FALSE)
    }
    time_values <- frame[[native_time]][match(years, as.character(frame[[year]]))]
  }
  family <- if (is.null(info)) stats::family(model) else info[c("family", "link")]
  if (grepl("^quasi", family$family)) {
    stop("Quasi families are not supported.", call. = FALSE)
  }
  if (is.null(info)) {
    .index_prediction_guards(model, backend)
    .index_reference_predictors(model, backend, year, reference_data)
  }
  batches <- split(seq_len(nrow(reference_data)),
    ceiling(seq_len(nrow(reference_data)) / batch_size))
  newdata <- function(i, rows) {
    d <- reference_data[rows, , drop = FALSE]
    d[[year]] <- if (length(i) == 1L) rep(year_values[i], length(rows)) else year_values[i]
    if (!is.null(time_values)) d[[native_time]] <- if (length(i) == 1L) rep(time_values[i], length(rows)) else time_values[i]
    d
  }
  if (!is.null(info)) {
    result <- .index_spatial(model, backend, info, years, newdata, weights,
      uncertainty != "none", ndraws, batch_size, draw_batch_size, seed,
      spatial_fields, prediction_offset)
    estimate <- result$estimate
    draws <- result$draws
    if (!raw) {
      if (any(estimate <= 0) || (!is.null(draws) && any(draws <= 0))) {
        stop("Rescaling requires positive indices in every joint draw.", call. = FALSE)
      }
      estimate <- estimate * (rescale / geo_mean(estimate))
      if (!is.null(draws)) draws <- draws * (rescale / exp(rowMeans(log(draws))))
    }
    tab <- data.frame(Year = years, Mean = estimate, Median = NA_real_, SD = NA_real_,
      CV = NA_real_, Qlower = NA_real_, Qupper = NA_real_)
    if (!is.null(draws)) {
      tab$SD <- apply(draws, 2L, stats::sd)
      intervals <- apply(draws, 2L, stats::quantile, probs = probs, names = FALSE)
      tab$Qlower <- intervals[1L, ]
      tab$Qupper <- intervals[2L, ]
    }
    uncertainty_label <- "joint Gaussian parameter/field simulation"
  } else if (backend == "brms") {
    result <- .index_brms(model, years, batches, newdata, weights,
      ndraws, draw_batch_size)
    draws <- result$draws
    if (!raw) {
      if (any(draws <= 0)) stop("Rescaling requires positive indices in every draw.", call. = FALSE)
      draws <- draws * (rescale / exp(rowMeans(log(draws))))
    }
    estimate <- colMeans(draws)
    tab <- data.frame(Year = years, Mean = estimate, Median = NA_real_, SD = NA_real_,
      CV = NA_real_, Qlower = NA_real_, Qupper = NA_real_)
    if (uncertainty == "auto") {
      tab$Median <- apply(draws, 2, stats::median)
      tab$SD <- apply(draws, 2, stats::sd)
      intervals <- apply(draws, 2, stats::quantile, probs = probs, names = FALSE)
      tab$Qlower <- intervals[1, ]
      tab$Qupper <- intervals[2, ]
    }
    uncertainty_label <- "posterior expected-response draws"
  } else {
    result <- .index_frequentist(model, backend, years, batches, newdata,
      weights, uncertainty != "none", batch_size)
    estimate <- result$estimate
    covariance <- result$covariance
    if (!raw) {
      if (any(estimate <= 0)) stop("Rescaling requires positive index estimates.", call. = FALSE)
      multiplier <- rescale / geo_mean(estimate)
      if (!is.null(covariance)) {
        jacobian <- multiplier * (diag(length(estimate)) -
          outer(estimate, 1 / estimate) / length(estimate))
        covariance <- jacobian %*% covariance %*% t(jacobian)
      }
      estimate <- estimate * multiplier
    }
    tab <- data.frame(Year = years, Mean = estimate, Median = NA_real_, SD = NA_real_,
      CV = NA_real_, Qlower = NA_real_, Qupper = NA_real_)
    if (!is.null(covariance)) {
      tab$SD <- sqrt(pmax(0, diag(covariance)))
      q <- stats::qnorm(probs)
      tab$Qlower <- estimate + q[1] * tab$SD
      tab$Qupper <- estimate + q[2] * tab$SD
      if (all(estimate > 0)) {
        tab$Qlower <- exp(log(estimate) + q[1] * tab$SD / estimate)
        tab$Qupper <- exp(log(estimate) + q[2] * tab$SD / estimate)
      }
    }
    uncertainty_label <- "joint-covariance delta method"
    draws <- NULL
  }
  tab$CV <- ifelse(tab$Mean > 0, tab$SD / tab$Mean, NA_real_)
  .new_cpue_index(tab, list(method = method, year = year, backend = backend,
    family = family$family, link = family$link, units = units,
    scale = if (raw) "response" else "ratio", rescale = rescale,
    uncertainty = if (uncertainty == "none") "none" else uncertainty_label,
    probs = probs, reference_rows = nrow(reference_data),
    reference_variables = names(reference_data),
    reference = "fixed common population; response-scale weighted mean",
    random_effects = if (backend %in% c("brms", "glmmTMB")) {
      "group-level effects set to zero; not population-marginal"
    } else if (backend == "sdmTMB") {
      paste0("spatial fields: ", spatial_fields, "; IID effects zero; other fitted terms retained")
    } else if (backend == "tinyVAST") {
      paste0("spatial fields: ", spatial_fields, "; fitted temporal effects and smooths retained")
    } else "all fitted terms, including GAM smooths",
    spatial_fields = if (!is.null(info)) spatial_fields else NULL,
    prediction_offset = prediction_offset,
    bias_correction = if (!is.null(info)) "none (plug-in expected response)" else NULL,
    ndraws = if (backend %in% c("brms", "sdmTMB", "tinyVAST")) nrow(draws) else NULL,
    seed = if (!is.null(info) && uncertainty != "none") seed else NULL,
    draw_ids = if (backend == "brms") result$draw_ids else NULL),
    if (retain == "draws") draws else NULL)
}

.new_cpue_index <- function(table, metadata, draws = NULL) {
  table$Method <- metadata$method
  table$Distribution <- metadata$family
  table$Link <- metadata$link
  structure(list(table = table, metadata = metadata, draws = draws), class = "influ_index")
}

#' @rdname cpue_index
#' @param x An `influ_index` object.
#' @param row.names,optional Passed to the data-frame method.
#' @export
as.data.frame.influ_index <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(x$table, row.names = row.names, optional = optional, ...)
}

#' @rdname cpue_index
#' @export
print.influ_index <- function(x, ...) {
  cat("CPUE index |", x$metadata$backend, "|", x$metadata$method, "\n")
  cat("Scale:", x$metadata$scale, "| Uncertainty:", x$metadata$uncertainty, "\n")
  print(x$table, row.names = FALSE, ...)
  invisible(x)
}
