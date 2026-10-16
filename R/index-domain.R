#' Diagnose prediction-domain support and index dependence
#'
#' Classify an explicit prediction domain, decompose native link predictors,
#' and compare full-domain indices with restricted-domain and term-omission
#' predictions. No model is refitted.
#' @md
#' @param model A complete fitted sdmTMB or univariate tinyVAST model.
#' @param reference_data A non-empty prediction data frame without `year`.
#' @param year Year column; defaults to the model's native time variable.
#' @param sampled Sampling rule: a shared cell-ID column, a list
#'   `list(coords = c("X", "Y"), distance = d)`, or a logical matrix with
#'   reference rows and years in index order. When omitted or `NULL`, use half
#'   the nearest distinct grid-centre spacing, with a warning. Supply a rule
#'   explicitly when this default does not describe the sampling design.
#' @param sampled_definition One or more of `"cell_time"`, `"cell_ever"`,
#'   `"cell_always"`. All are computed by default for sampled-only comparisons.
#' @param omission_definition A single definition for unsampled omission masks;
#'   defaults to `"cell_time"` independently of `sampled_definition`.
#' @param min_obs Minimum observations per cell-year.
#' @param min_years Minimum sampled years for `"cell_ever"`; a positive count,
#'   or a proportion strictly between zero and one (rounded up).
#' @param sampled_only Return a restricted index for each definition?
#' @param area Optional explicit cell areas (numeric values or a column name).
#'   Supplying areas requests totals rather than weighted means.
#' @param area_units,response_units Required unit strings when supplying area.
#' @param reference_weights Non-negative reference weights. With area these
#'   are within-cell averaging weights, multiplied by area.
#' @param decompose Summarise link-scale terms by year and domain class?
#' @param omit Native term names to omit, including `"year"`, `"spatial"`,
#'   `"spatiotemporal"` and `"svc"`. Unknown names are errors.
#' @param where One or more of `"unsampled"`, `"never_sampled"`, `"all"`.
#' @param covariate_reference `"sampled_mean"` (default), `"zero"`, or a named
#'   list of rules per omitted covariate. A fixed rule is
#'   `list(rule = "fixed", value = list(depth = 40))`.
#' @param ndraws,seed,batch_size,draw_batch_size,uncertainty,probs,prediction_offset,units
#'   As in [cpue_index()].
#' @param retain_cells Optional year labels for which to keep cell-level terms.
#' @param trend_window Optional year labels defining a second trend window.
#' @param core_threshold Warn below this retained-weight share for fixed domains.
#' @param unsampled_threshold,share_change_threshold Flag years exceeding these
#'   unsampled-share and consecutive-share-change thresholds.
#'
#' @details The default sampling distance is a warned grid-spacing heuristic,
#'   not inferred cell areas. Sampling interpretation depends on the supplied
#'   cell size or distance. Reporting classes always
#'   describe actual same-year support: `sampled`, `previously_sampled`
#'   (sampled in another year, not necessarily an earlier year), and
#'   `never_sampled`. The chosen definitions govern omission masks and
#'   sampled-only domains.
#'
#'   Contributions add on the link scale, separately for delta components;
#'   they must not be back-transformed and added on the response scale.
#'   Decomposition quantiles use reference-weighted, 1024-bin histograms
#'   spanning each group's observed range; their resolution is recorded in
#'   metadata. Means and SDs use all positive-weight cells. Only compact
#'   annual summaries and histograms are accumulated, unless cells are retained.
#'
#'   Omission is a prediction-target counterfactual, **not a refit**; use
#'   [plot_step()] to study refitted models. Fields are set to zero; the year
#'   term is replaced by its first-year value at the same reference row.
#'   `"sampled_mean"` replaces a covariate's additive contribution by its
#'   reference-weighted mean over cells sampled that year, separately in each
#'   joint draw and delta component. It is not the prediction at a mean raw
#'   covariate. `"zero"` is appropriate only for deliberately centred terms.
#'   `"fixed"` reevaluates the term at explicit raw predictor values, retaining
#'   its other predictors; this also handles interactions and smooths.
#'
#'   Weighted-mean restricted indices renormalise weights each year. An empty
#'   restricted mean is undefined (`NA`); an empty restricted total is zero.
#'   `"cell_time"` is a variable-domain diagnostic, not an alternative index.
#'   For area totals compare shapes using [index_ratio()] with common
#'   normalisation, or [plot_compare()] with `rescale = 1`.
#'   Slopes use numeric year labels when available, otherwise ordered year
#'   positions. All comparisons use paired joint Gaussian draws, not
#'   independent marginal intervals. Nonpositive series have undefined log
#'   comparisons; no small positive replacement is made.
#' @return An `influ_domain` containing ordinary `influ_index` objects in
#'   `baseline`, `sampled_only`, and `omission`; coverage, decomposition,
#'   response-scale partition and trend tables; shared annual
#'   `partition_draws`; and optional `retained_cells`.
#' @seealso [cpue_index()], [integrate_index()], [index_ratio()]
#' @export
index_domain <- function(model, reference_data, year = NULL, sampled = NULL,
    sampled_definition = c("cell_time", "cell_ever", "cell_always"),
    min_obs = 1L, min_years = 1, sampled_only = TRUE,
    area = NULL, area_units = NULL, response_units = NULL,
    reference_weights = NULL, decompose = TRUE, omit = character(),
    where = c("unsampled", "all"), covariate_reference = "sampled_mean",
    ndraws = 500L, seed = 1L, batch_size = 250L, draw_batch_size = 100L,
    retain_cells = NULL, uncertainty = c("auto", "none"),
    probs = c(0.025, 0.975), trend_window = NULL, core_threshold = 0.05,
    unsampled_threshold = 0.5, share_change_threshold = 0.2,
    prediction_offset = NULL, units = NULL,
    omission_definition = c("cell_time", "cell_ever", "cell_always")) {
  backend <- if (inherits(model, "sdmTMB")) "sdmTMB" else if (
    inherits(model, "tinyVAST")) "tinyVAST" else {
    stop("`index_domain()` requires an sdmTMB or univariate tinyVAST fit.", call. = FALSE)
  }
  info <- .index_spatial_info(model, backend, year)
  year <- info$year
  if (!is.character(year) || length(year) != 1L || is.na(year) ||
      !year %in% names(info$data)) {
    stop("`year` must name a variable in the stored model data.", call. = FALSE)
  }
  omission_definition <- match.arg(omission_definition)
  sampled_definition <- match.arg(sampled_definition,
    c("cell_time", "cell_ever", "cell_always"), several.ok = TRUE)
  uncertainty <- match.arg(uncertainty)
  probs <- .validate_probs(probs)
  ndraws <- .validate_ndraws(ndraws)
  batch_size <- .validate_ndraws(batch_size)
  draw_batch_size <- .validate_ndraws(draw_batch_size)
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
      seed < 0 || seed > .Machine$integer.max || seed != as.integer(seed)) {
    stop("`seed` must be one non-negative integer.", call. = FALSE)
  }
  if (uncertainty == "auto" && ndraws < 2L) stop("At least two joint draws are required.", call. = FALSE)
  for (flag in list(sampled_only, decompose)) {
    if (!is.logical(flag) || length(flag) != 1L || is.na(flag)) {
      stop("`sampled_only` and `decompose` must be TRUE or FALSE.", call. = FALSE)
    }
  }
  if (!is.character(omit) || anyNA(omit) || anyDuplicated(omit)) {
    stop("`omit` must contain unique term names.", call. = FALSE)
  }
  where <- match.arg(where, c("unsampled", "never_sampled", "all"), several.ok = TRUE)
  if (!is.null(prediction_offset) && (backend != "sdmTMB" ||
      !is.character(prediction_offset) || length(prediction_offset) != 1L ||
      is.na(prediction_offset) || !nzchar(prediction_offset))) {
    stop("`prediction_offset` must name an sdmTMB reference-data column.", call. = FALSE)
  }
  for (threshold in list(unsampled_threshold, share_change_threshold)) {
    if (!is.numeric(threshold) || length(threshold) != 1L ||
        !is.finite(threshold) || threshold < 0 || threshold > 1) {
      stop("Share thresholds must be numbers between zero and one.", call. = FALSE)
    }
  }
  if (!is.data.frame(reference_data) || !nrow(reference_data) ||
      anyDuplicated(names(reference_data)) || year %in% names(reference_data)) {
    stop("Supply non-empty `reference_data` without the year column.", call. = FALSE)
  }
  if (is.null(sampled)) {
    coords <- if (backend == "sdmTMB") model$mesh$xy_cols else NULL
    if (length(coords) != 2L) coords <- c("X", "Y")
    sampled <- .domain_default_sampled(reference_data, coords = coords)
  }
  n <- nrow(reference_data)
  resolve <- function(x, name) {
    if (is.character(x) && length(x) == 1L && !is.na(x) &&
        x %in% names(reference_data)) x <- reference_data[[x]]
    if (!is.numeric(x) || !is.null(dim(x)) ||
        !length(x) %in% c(1L, n) || any(!is.finite(x)) || any(x < 0)) {
      stop("`", name, "` must supply finite non-negative reference values.", call. = FALSE)
    }
    rep(x, length.out = n)
  }
  weights <- resolve(reference_weights %||% rep(1, n), "reference_weights")
  integrated <- !is.null(area)
  if (integrated) {
    area <- resolve(area, "area")
    for (unit in list(area_units, response_units)) {
      if (!is.character(unit) || length(unit) != 1L || is.na(unit) ||
          !nzchar(trimws(unit))) stop("Supply `area_units` and `response_units` for totals.", call. = FALSE)
    }
    weights <- weights * area
    if (is.null(units)) units <- paste0(response_units, " * ", area_units)
  }
  if (!is.null(units) && (!is.character(units) || length(units) != 1L ||
      is.na(units) || !nzchar(trimws(units)))) stop("`units` must be a non-empty string.", call. = FALSE)
  if (!is.finite(sum(weights)) || !any(weights > 0)) {
    stop("Reference weights must have a positive finite total.", call. = FALSE)
  }
  if (anyNA(reference_data[weights > 0, , drop = FALSE])) {
    stop("Reference predictors must not contain missing values on positive-weight rows.", call. = FALSE)
  }
  years <- .focus_info(info$data, year)$levels
  year_values <- info$data[[year]][match(years, as.character(info$data[[year]]))]
  if (anyNA(year_values)) stop("The year variable must be present in model data.", call. = FALSE)
  if (!is.null(retain_cells) && (!is.atomic(retain_cells) || anyNA(retain_cells) ||
      anyDuplicated(retain_cells) || !all(as.character(retain_cells) %in% years))) {
    stop("`retain_cells` must select unique observed years.", call. = FALSE)
  }
  retain_cells <- as.character(retain_cells)
  if (!is.null(trend_window) && (length(trend_window) < 2L || anyNA(trend_window) ||
      anyDuplicated(trend_window) || !all(as.character(trend_window) %in% years))) {
    stop("`trend_window` must select at least two unique observed years.", call. = FALSE)
  }
  sampling <- .domain_sampling(info$data, reference_data, year, years, sampled,
    unique(c(sampled_definition, omission_definition)),
    min_obs, min_years, weights, core_threshold)
  # Match cpue_index: discard zero-weight rows before native predictions.
  positive <- which(weights > 0)
  reference_data <- reference_data[positive, , drop = FALSE]
  weights <- weights[positive]
  if (!integrated) weights <- (weights / max(weights)) / sum(weights / max(weights))
  classes <- sampling$class[positive, , drop = FALSE]
  masks <- lapply(sampling$masks, function(x) x[positive, , drop = FALSE])
  cell_time <- sampling$cell_time[positive, , drop = FALSE]
  native_time <- info$time
  time_values <- NULL
  if (!is.null(native_time) && !identical(native_time, year)) {
    if (native_time %in% names(reference_data)) stop("Do not supply a fixed native time column.", call. = FALSE)
    by_year <- lapply(years, function(y) unique(info$data[[native_time]][as.character(info$data[[year]]) == y]))
    if (any(lengths(by_year) != 1L) || anyNA(unlist(by_year))) {
      stop("`year` must map to exactly one native time per year.", call. = FALSE)
    }
    time_values <- info$data[[native_time]][match(years, as.character(info$data[[year]]))]
  }
  newdata <- function(y, rows) {
    d <- reference_data[rows, , drop = FALSE]
    d[[year]] <- year_values[y]
    if (!is.null(time_values)) d[[native_time]] <- time_values[y]
    d
  }
  settings <- list(backend = backend, method = if (integrated) "integrated" else "standardised",
    year = year, family = info$family, link = info$link, units = units,
    scale = "response", rescale = "raw", probs = probs,
    uncertainty = if (uncertainty == "none") "none" else "joint Gaussian parameter/field simulation",
    reference_rows = length(weights), reference_variables = names(reference_data),
    spatial_fields = "all", prediction_offset = prediction_offset,
    bias_correction = "none (plug-in expected response)", seed = seed,
    sampled_definition = sampled_definition,
    omission_definition = omission_definition, min_obs = min_obs,
    min_years = sampling$min_years, area_units = area_units, response_units = response_units,
    reference = if (integrated) "fixed common domain; area-weighted response total" else
      "fixed common population; response-scale weighted mean",
    total_area = if (integrated) sum(weights) else NULL,
    catchability = if (integrated) "not adjusted" else NULL)
  result <- .domain_reduce(model, info, years, newdata, weights, classes,
    masks, cell_time, omit, where, covariate_reference, decompose, retain_cells,
    positive, reference_data, settings, uncertainty == "auto", ndraws,
    batch_size, draw_batch_size, seed, prediction_offset, sampled_only)
  result$coverage <- sampling$coverage
  result$domains <- sampling$domains
  result$counts <- sampling$counts
  result$metadata <- c(settings, list(
    sampled_rule = if (is.matrix(sampled)) "user-supplied logical matrix" else sampled,
    variable_domain = "cell_time",
    quantile_method = "1024-bin reference-weighted histogram; error at most one bin width",
    quantile_bins = 1024L, covariate_reference = covariate_reference,
    year_reference = "first year", core_threshold = core_threshold,
    trend_window = if (!is.null(trend_window)) as.character(trend_window) else NULL,
    counterfactual = "prediction target only; no refitting",
    trend_time = if (all(is.finite(suppressWarnings(as.numeric(years)))) &&
      !anyDuplicated(suppressWarnings(as.numeric(years)))) "numeric year" else "ordered year position"))
  result$trend <- .domain_trends(result, years, trend_window, probs)
  p <- result$partition
  unsampled <- p[p$class != "sampled", , drop = FALSE]
  share <- tapply(unsampled$share, factor(unsampled$Year, levels = years), sum)
  result$flags <- data.frame(Year = years, unsampled_share = as.numeric(share),
    high_unsampled_share = share > unsampled_threshold,
    changing_unsampled_share = c(FALSE, abs(diff(share)) > share_change_threshold),
    row.names = NULL)
  structure(result, class = "influ_domain")
}

.domain_rule <- function(term, rules) {
  rule <- if (is.list(rules)) rules[[term]] else rules
  if (is.null(rule)) stop("Supply a `covariate_reference` rule for `", term, "`.", call. = FALSE)
  if (is.character(rule) && length(rule) == 1L && !is.na(rule) &&
      rule %in% c("sampled_mean", "zero")) return(list(rule = rule))
  if (is.list(rule) && identical(rule$rule, "fixed") && is.list(rule$value) &&
      length(rule$value) && !is.null(names(rule$value)) &&
      all(nzchar(names(rule$value))) && !anyDuplicated(names(rule$value)) &&
      all(vapply(rule$value, function(x) is.atomic(x) && length(x) == 1L &&
        !anyNA(x) && (!is.numeric(x) || is.finite(x)), logical(1)))) return(rule)
  stop("Covariate references must be 'sampled_mean', 'zero', or list(rule = 'fixed', value = list(...)).", call. = FALSE)
}

.domain_index <- function(estimate, draws, metadata) {
  years <- names(estimate)
  tab <- data.frame(Year = years, Mean = unname(estimate), Median = NA_real_,
    SD = NA_real_, CV = NA_real_, Qlower = NA_real_, Qupper = NA_real_)
  covariance <- NULL
  if (!is.null(draws)) {
    valid <- colSums(is.finite(draws)) == nrow(draws)
    if (any(valid)) {
      tab$SD[valid] <- apply(draws[, valid, drop = FALSE], 2L, stats::sd)
      interval <- apply(draws[, valid, drop = FALSE], 2L, stats::quantile,
        probs = metadata$probs, names = FALSE)
      tab$Qlower[valid] <- interval[1L, ]
      tab$Qupper[valid] <- interval[2L, ]
    }
    response <- stats::cov(draws)
    dimnames(response) <- list(years, years)
    log_covariance <- if (all(is.finite(draws)) && all(draws > 0) &&
        all(is.finite(estimate)) && all(estimate > 0)) stats::cov(log(draws)) else NULL
    if (!is.null(log_covariance)) dimnames(log_covariance) <- list(years, years)
    covariance <- list(response = response, log = log_covariance)
  }
  tab$CV <- ifelse(tab$Mean > 0, tab$SD / tab$Mean, NA_real_)
  .new_cpue_index(tab, metadata, draws, covariance)
}

.domain_interval <- function(x, probs) {
  if (is.null(x) || !length(x) || any(!is.finite(x))) return(c(NA_real_, NA_real_))
  as.numeric(stats::quantile(x, probs = probs, names = FALSE))
}

.domain_trends <- function(result, years, window, probs) {
  time <- suppressWarnings(as.numeric(years))
  if (any(!is.finite(time)) || anyDuplicated(time)) time <- seq_along(years)
  windows <- list(all = seq_along(years))
  if (!is.null(window)) windows$window <- which(years %in% as.character(window))
  series <- c(list(full = result$baseline), result$sampled_only,
    list(unsampled_only = result$unsampled_only), result$omission)
  out <- list()
  for (w in names(windows)) {
    ids <- windows[[w]]
    slope <- function(v) {
      if (length(ids) < 2L || any(!is.finite(v[ids])) || any(v[ids] <= 0)) return(NA_real_)
      x <- time[ids] - mean(time[ids])
      sum(x * log(v[ids])) / sum(x^2)
    }
    correlation <- function(v, b) {
      if (length(ids) < 2L || any(!is.finite(c(v[ids], b[ids]))) ||
          any(c(v[ids], b[ids]) <= 0) ||
          stats::sd(log(v[ids])) == 0 || stats::sd(log(b[ids])) == 0) return(NA_real_)
      stats::cor(log(v[ids]), log(b[ids]))
    }
    base <- result$baseline
    base_slope <- slope(base$table$Mean)
    bd <- if (!is.null(base$draws)) apply(base$draws, 1L, slope) else NULL
    for (name in names(series)) {
      s <- series[[name]]
      value <- slope(s$table$Mean)
      sd <- if (!is.null(s$draws)) apply(s$draws, 1L, slope) else NULL
      cd <- if (!is.null(s$draws)) vapply(seq_len(nrow(s$draws)),
        function(i) correlation(s$draws[i, ], base$draws[i, ]), numeric(1)) else NULL
      si <- .domain_interval(sd, probs)
      di <- .domain_interval(sd - bd, probs)
      ci <- .domain_interval(cd, probs)
      out[[length(out) + 1L]] <- data.frame(series = name, window = w,
        slope = value, slope_lower = si[1L], slope_upper = si[2L],
        slope_difference = value - base_slope, difference_lower = di[1L],
        difference_upper = di[2L], correlation = correlation(s$table$Mean, base$table$Mean),
        correlation_lower = ci[1L], correlation_upper = ci[2L])
    }
  }
  do.call(rbind, out)
}
