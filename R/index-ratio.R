#' Calculate point-estimate ratios between stored annual indices
#'
#' Compare annual series without fitting models or repeating predictions.
#' Ratios are a separate diagnostic, not uncertainty intervals for differences
#' between models.
#' @md
#' @param indices A named list of calculated [cpue_index()] objects or
#'   `influ_diag` objects from [influ()]; one diagnostic with an explicit `series` selection;
#'   or a stored [influ_steps] sequence. Fitted models are not accepted.
#' @param reference Denominator label. Defaults to the first supplied series.
#'   For steps, defaults to `"previous"`; `"first"` or a step label compares
#'   every other step with that denominator. A series is never divided by itself.
#' @param labels Optional unique display labels, one per supplied series or step.
#' @param normalise `"none"` retains the supplied estimates and their scaling.
#'   `"common"` divides each series by its geometric mean over exactly the same
#'   positive, matched years. This is display normalisation, not a new index or
#'   an uncertainty calculation.
#' @param baseline_years Optional unique year labels for common normalisation.
#'   Every nominated year must have a finite, positive estimate in every series.
#'   When omitted, all shared years meeting that requirement are used.
#' @param series For one diagnostic, explicitly select both
#'   `c("standardised", "nominal")`; `"standardized"` is also accepted. Such a
#'   comparison requires `normalise = "common"`, because the standardised
#'   diagnostic is a year-effect contrast, not expected CPUE in response units.
#'   For a list of diagnostics, select one series shared by all inputs; the
#'   default is `"standardised"`.
#' @param component One stored diagnostic component. Required if more than one
#'   component is available. Encounter, positive, and combined-response targets
#'   are never silently combined.
#' @details Only matching year labels are divided. The returned table spans
#'   the union of years and marks missing estimates, zero denominators, and
#'   non-finite ratios explicitly. A zero numerator is a valid ratio of zero.
#'   No interpolation, extrapolation, or small positive replacement is used.
#'
#'   Expected-response, year-effect, and area-integrated indices cannot be mixed.
#'   Known response, component, units, area, and catchability metadata must agree.
#'   Matching metadata cannot establish that independently chosen reference
#'   populations answer the same question: the analyst must verify those
#'   populations and any exposure/trial conventions. Older index objects do not
#'   store all response definitions; unverified fields are recorded in metadata.
#'   Common normalisation compares shapes, not absolute CPUE or abundance.
#'   Already rescaled expected-response indices covering different periods
#'   require common normalisation rather than comparing separate full-period
#'   geometric-mean anchors.
#'
#'   The result stores point estimates only. Marginal index intervals or
#'   separate annual covariance matrices do not identify cross-model covariance,
#'   so dividing interval endpoints would not give a valid ratio interval.
#'   No independence assumption, posterior pairing, or model refit is made.
#' @return A compact `influ_index_ratio` with `table` and `metadata`.
#'   `as.data.frame()` returns the stored year, numerator and denominator labels,
#'   original estimates, display estimates, ratio, and status.
#' @seealso [plot_index_ratio()], [cpue_index()], [influ_steps()], [plot_compare()]
#' @examples
#' data(lobsters_per_pot)
#' fit_a <- glm(lobsters ~ year + depth, poisson(), data = lobsters_per_pot)
#' fit_b <- glm(lobsters ~ year + depth + soak, poisson(), data = lobsters_per_pot)
#' index_a <- cpue_index(fit_a, reference_data = data.frame(depth = 40),
#'   uncertainty = "none")
#' index_b <- cpue_index(fit_b,
#'   reference_data = data.frame(depth = 40, soak = 1), uncertainty = "none")
#' ratios <- index_ratio(list(Depth = index_a, Depth_soak = index_b),
#'   reference = "Depth")
#' plot_index_ratio(ratios)
#' @export
index_ratio <- function(indices, reference = NULL, labels = NULL,
    normalise = c("none", "common"), baseline_years = NULL,
    series = NULL, component = NULL) {
  normalise <- match.arg(normalise)
  if (!is.null(baseline_years) && normalise != "common") {
    stop("`baseline_years` requires `normalise = 'common'`.", call. = FALSE)
  }
  prepared <- .ratio_prepare_inputs(indices, labels, series, component)
  inputs <- prepared$inputs
  labels <- prepared$labels
  if (length(inputs) < 2L) stop("Supply at least two stored annual series.", call. = FALSE)
  compatibility <- .ratio_check_targets(inputs, prepared$nominal_comparison, normalise)
  if (is.null(reference)) reference <- if (prepared$steps) "previous" else labels[1L]
  if (!is.character(reference) || length(reference) != 1L || is.na(reference) ||
      !nzchar(reference)) stop("`reference` must name one denominator series.", call. = FALSE)
  if (prepared$steps && reference == "previous") {
    pairs <- data.frame(numerator = labels[-1L], denominator = labels[-length(labels)])
  } else {
    if (prepared$steps && reference == "first") reference <- labels[1L]
    if (!reference %in% labels) stop("`reference` is not a supplied series label.", call. = FALSE)
    pairs <- data.frame(numerator = setdiff(labels, reference), denominator = reference)
  }
  years <- .ratio_order_years(unique(unlist(lapply(inputs, function(x) x$data$year),
    use.names = FALSE)))
  baseline <- character()
  divisors <- stats::setNames(rep(1, length(inputs)), labels)
  log_divisors <- stats::setNames(rep(0, length(inputs)), labels)
  if (normalise == "common") {
    eligible <- lapply(inputs, function(x) {
      x$data$year[is.finite(x$data$estimate) & x$data$estimate > 0]
    })
    shared <- Reduce(intersect, eligible)
    if (is.null(baseline_years)) {
      baseline <- years[years %in% shared]
    } else {
      if (!(is.character(baseline_years) || is.numeric(baseline_years) ||
          is.factor(baseline_years)) || !is.null(dim(baseline_years)) ||
          !length(baseline_years) || anyNA(baseline_years)) {
        stop("`baseline_years` must contain unique, non-missing year labels.", call. = FALSE)
      }
      baseline <- as.character(baseline_years)
      if (anyDuplicated(baseline) || any(!nzchar(baseline)) ||
          any(!baseline %in% shared)) {
        stop("Every `baseline_years` label must have a positive finite estimate in every series.", call. = FALSE)
      }
    }
    if (!length(baseline)) stop("Common normalisation needs at least one shared year with positive finite estimates in every series.", call. = FALSE)
    log_divisors[] <- vapply(inputs, function(x) {
      mean(log(x$data$estimate[match(baseline, x$data$year)]))
    }, numeric(1))
    divisors[] <- exp(log_divisors)
  }
  rows <- lapply(seq_len(nrow(pairs)), function(i) {
    numerator <- pairs$numerator[i]
    denominator <- pairs$denominator[i]
    a <- inputs[[match(numerator, labels)]]$data
    b <- inputs[[match(denominator, labels)]]$data
    an <- a$estimate[match(years, a$year)]
    bn <- b$estimate[match(years, b$year)]
    status <- rep("ok", length(years))
    status[is.na(an)] <- "missing_numerator"
    status[is.na(bn)] <- "missing_denominator"
    status[is.na(an) & is.na(bn)] <- "missing_both"
    status[!is.na(an) & !is.finite(an)] <- "nonfinite_numerator"
    status[!is.na(bn) & !is.finite(bn)] <- "nonfinite_denominator"
    status[!is.na(an) & !is.finite(an) & !is.na(bn) & !is.finite(bn)] <- "nonfinite_both"
    status[is.finite(bn) & bn == 0] <- "zero_denominator"
    ok <- status == "ok"
    ratio <- rep(NA_real_, length(years))
    ratio[ok & an == 0] <- 0
    positive <- ok & an > 0
    ratio[positive] <- exp(log(an[positive]) - log(bn[positive]) +
      log_divisors[denominator] - log_divisors[numerator])
    overflow <- ok & !is.finite(ratio)
    status[overflow] <- "nonfinite_ratio"
    ratio[overflow] <- NA_real_
    underflow <- positive & is.finite(ratio) & ratio == 0
    status[underflow] <- "underflow_ratio"
    ratio[underflow] <- NA_real_
    data.frame(year = years, numerator = numerator, denominator = denominator,
      numerator_estimate = an, denominator_estimate = bn,
      numerator_display = .ratio_display_estimate(an, log_divisors[numerator]),
      denominator_display = .ratio_display_estimate(bn, log_divisors[denominator]), ratio = ratio,
      status = status, stringsAsFactors = FALSE)
  })
  table <- do.call(rbind, rows)
  rownames(table) <- NULL
  structure(list(table = table, metadata = list(
    focus = inputs[[1]]$metadata$year, series = labels, comparisons = pairs,
    normalise = normalise, baseline_years = baseline, divisors = divisors,
    source = lapply(inputs, `[[`, "metadata"),
    verified_fields = compatibility$verified, unverified_fields = compatibility$unverified,
    notes = unique(c(compatibility$notes,
      if (prepared$nominal_comparison) "Nominal summaries describe the observed response; exposure adjustment is not performed automatically.",
      if (normalise == "common") "Shape comparison: all series normalised over the same stored baseline years." else "Supplied scaling retained; reference populations and centring are not recalculated.",
      "Point-estimate ratios only: cross-model covariance is unavailable; no ratio intervals are inferred.",
      "Matching year labels only; missing values and zero denominators are undefined.",
      "Unrepresentable positive ratios are undefined, not zero; unrepresentable normalised display estimates are NA while original estimates are retained.")),
    uncertainty = "point estimates only; cross-series covariance not assumed")),
    class = "influ_index_ratio")
}

.ratio_display_estimate <- function(estimate, log_divisor) {
  if (identical(unname(log_divisor), 0)) return(estimate)
  out <- rep(NA_real_, length(estimate))
  out[is.finite(estimate) & estimate == 0] <- 0
  positive <- is.finite(estimate) & estimate > 0
  out[positive] <- exp(log(estimate[positive]) - log_divisor)
  out[!is.finite(out) | (positive & out == 0)] <- NA_real_
  out
}

.ratio_labels <- function(labels, defaults, n) {
  if (is.null(labels)) labels <- defaults
  if (is.null(labels)) labels <- paste("Index", seq_len(n))
  if (!is.character(labels) || length(labels) != n || anyNA(labels) ||
      any(!nzchar(trimws(labels))) || anyDuplicated(trimws(labels))) {
    stop("`labels` must contain one unique, non-empty label per series.", call. = FALSE)
  }
  trimws(labels)
}

.ratio_prepare_inputs <- function(indices, labels, series, component) {
  steps <- inherits(indices, "influ_steps")
  single <- inherits(indices, "influ_diag")
  nominal_comparison <- FALSE
  if (!is.null(component) && (!is.character(component) || length(component) != 1L ||
      is.na(component) || !nzchar(component))) stop("`component` must name one stored component.", call. = FALSE)
  if (!is.null(series)) {
    if (!is.character(series) || anyNA(series) || !length(series)) stop("`series` must select stored diagnostic series.", call. = FALSE)
    series[series == "standardized"] <- "standardised"
    if (any(!series %in% c("standardised", "nominal")) || anyDuplicated(series)) {
      stop("`series` must select 'standardised' and/or 'nominal' without duplicates.", call. = FALSE)
    }
  }
  if (steps) {
    if (!is.null(series)) stop("`series` is only used with influence diagnostics.", call. = FALSE)
    if (!is.data.frame(indices$steps) || !is.data.frame(indices$indices) ||
        !all(c("step_id", "label") %in% names(indices$steps)) ||
        !all(c("step_id", "level", "estimate", "scale") %in% names(indices$indices))) {
      stop("The stored step sequence is incomplete.", call. = FALSE)
    }
    labels <- .ratio_labels(labels, indices$steps$label, nrow(indices$steps))
    inputs <- lapply(seq_len(nrow(indices$steps)), function(i) {
      d <- indices$indices[indices$indices$step_id == indices$steps$step_id[i], , drop = FALSE]
      meta <- indices$metadata
      meta$year <- indices$focus
      meta$method <- "year_effect"
      .ratio_diagnostic_payload(d, meta, "standardised", component)
    })
  } else if (single) {
    if (is.null(series) || length(series) != 2L) {
      stop("For one diagnostic, explicitly select `series = c('standardised', 'nominal')`.", call. = FALSE)
    }
    labels <- .ratio_labels(labels, series, length(series))
    inputs <- lapply(series, function(s) .ratio_from_diagnostic(indices, s, component))
    nominal_comparison <- TRUE
  } else {
    if (!is.list(indices) || !length(indices) || inherits(indices, "influ_index") ||
        !all(vapply(indices, function(x) inherits(x, c("influ_index", "influ_diag")), logical(1)))) {
      stop("Supply a list of calculated influ_index or influ_diag objects, one diagnostic, or stored influ_steps; fitted models are not accepted.", call. = FALSE)
    }
    if (length(series) > 1L) stop("For a list of diagnostics, select exactly one `series`.", call. = FALSE)
    labels <- .ratio_labels(labels, names(indices), length(indices))
    inputs <- lapply(indices, function(x) {
      if (inherits(x, "influ_index")) {
        if (!is.null(series)) stop("`series` is only used with influence diagnostics.", call. = FALSE)
        if (!is.null(component)) stop("Select calculated index components before calling `index_ratio()`.", call. = FALSE)
        .ratio_from_index(x)
      } else .ratio_from_diagnostic(x, series %||% "standardised", component)
    })
  }
  list(inputs = inputs, labels = labels, steps = steps,
    nominal_comparison = nominal_comparison)
}

.ratio_from_index <- function(x) {
  if (!is.data.frame(x$table) || !all(c("Year", "Mean") %in% names(x$table)) ||
      !is.list(x$metadata)) stop("The calculated index is missing its annual table or metadata.", call. = FALSE)
  meta <- x$metadata
  if (identical(meta$method, "standardized")) meta$method <- "standardised"
  .ratio_payload(x$table$Year, x$table$Mean, meta)
}

.ratio_from_diagnostic <- function(x, series, component) {
  meta <- x$metadata
  meta$year <- x$focus
  meta$method <- if (series == "nominal") "nominal" else "year_effect"
  .ratio_diagnostic_payload(x$indices, meta, series, component)
}

.ratio_diagnostic_payload <- function(d, meta, series, component) {
  if (!is.data.frame(d) || !all(c("level", "estimate", "scale", "series") %in% names(d))) {
    stop("The diagnostic is missing its stored annual index table.", call. = FALSE)
  }
  d <- d[d$series == series, , drop = FALSE]
  if (!nrow(d)) stop("The requested diagnostic series is not available.", call. = FALSE)
  if ("component" %in% names(d)) {
    available <- unique(d$component)
    if (is.null(component)) {
      if (length(available) != 1L || anyNA(available)) stop("Multiple components are available; supply `component =` explicitly.", call. = FALSE)
      component <- available
    }
    if (!component %in% available) stop("The requested component is not available in every series.", call. = FALSE)
    d <- d[d$component == component, , drop = FALSE]
  }
  if ("response" %in% names(d)) {
    response <- unique(d$response)
    if (length(response) != 1L) stop("Select one diagnostic response before calculating ratios.", call. = FALSE)
    meta$response <- response
  }
  meta$component <- component
  meta$scale <- unique(d$scale)
  meta$series <- series
  .ratio_payload(d$level, d$estimate, meta)
}

.ratio_payload <- function(years, estimates, metadata) {
  if (!(is.character(years) || is.numeric(years) || is.factor(years)) ||
      !is.null(dim(years)) || !length(years) || anyNA(years)) {
    stop("Annual series need unique, non-missing year labels.", call. = FALSE)
  }
  years <- as.character(years)
  if (any(!nzchar(years)) || anyDuplicated(years)) stop("Annual series need unique, non-missing year labels.", call. = FALSE)
  if (!is.numeric(estimates) || !is.null(dim(estimates)) ||
      length(estimates) != length(years)) stop("Annual index estimates must be numeric and aligned with the year labels.", call. = FALSE)
  if (any(estimates < 0, na.rm = TRUE)) stop("Ratios require non-negative response estimates or positive multiplicative contrasts, not signed effects.", call. = FALSE)
  if (!is.character(metadata$scale) || length(metadata$scale) != 1L ||
      is.na(metadata$scale) || !metadata$scale %in% c("response", "ratio")) {
    stop("Ratios require a response or multiplicative ratio scale; signed link/difference contrasts are not divided.", call. = FALSE)
  }
  if (!is.character(metadata$method) || length(metadata$method) != 1L ||
      !metadata$method %in% c("standardised", "integrated", "year_effect", "nominal")) {
    stop("The stored index method is missing or unsupported.", call. = FALSE)
  }
  for (field in c("year", "response", "component", "units")) {
    value <- metadata[[field]]
    if (!is.null(value) && (!is.character(value) || length(value) != 1L ||
        is.na(value) || !nzchar(value))) {
      stop("Stored ", field, " metadata must identify one non-missing target.", call. = FALSE)
    }
  }
  fields <- c("method", "scale", "year", "response", "component", "units", "rescale",
    "area_units", "response_units", "total_area", "catchability", "random_effects",
    "spatial_fields", "response_transform", "reference", "series", "nominal_scope")
  # Keep only atomic provenance: never retain fitted objects, calls, or grids.
  meta <- lapply(metadata[intersect(fields, names(metadata))], function(value) {
    if (is.atomic(value) && is.null(dim(value))) value else NULL
  })
  list(data = data.frame(year = years, estimate = estimates,
    stringsAsFactors = FALSE), metadata = meta)
}

.ratio_check_targets <- function(inputs, nominal_comparison, normalise) {
  fields <- c("method", "year", "response", "component", "units")
  meta <- lapply(inputs, `[[`, "metadata")
  if (nominal_comparison && normalise != "common") {
    stop("Standardised year-effect and nominal response series need `normalise = 'common'` for a shape comparison.", call. = FALSE)
  }
  verified <- unverified <- notes <- character()
  if (any(vapply(meta, function(x) identical(x$method, "integrated"), logical(1)))) {
    fields <- c(fields, "area_units", "response_units", "total_area", "catchability")
  }
  for (field in fields) {
    values <- lapply(meta, `[[`, field)
    known <- vapply(values, function(x) !is.null(x) && length(x) && !anyNA(x), logical(1))
    if (field == "method" && nominal_comparison) next
    if (sum(known) > 1L && !all(vapply(values[known], identical, logical(1), values[known][[1L]]))) {
      stop("Compared annual series have incompatible ", field, " metadata; verify common targets before calculating ratios.", call. = FALSE)
    }
    if (all(known)) verified <- c(verified, field) else unverified <- c(unverified, field)
  }
  scales <- vapply(meta, `[[`, character(1), "scale")
  if (length(unique(scales)) != 1L && !nominal_comparison && normalise != "common") {
    stop("Compared scales differ; use raw comparable indices or explicitly choose common normalisation.", call. = FALSE)
  }
  rescaled <- vapply(meta, function(x) is.numeric(x$rescale), logical(1))
  unequal_periods <- !all(vapply(inputs, function(x) setequal(x$data$year,
    inputs[[1L]]$data$year), logical(1)))
  if (normalise == "none" && any(rescaled) && unequal_periods) {
    stop("Already rescaled indices cover different periods; use `normalise = 'common'` to share one baseline period.", call. = FALSE)
  }
  if (normalise == "none" && all(rescaled)) {
    anchors <- lapply(meta, `[[`, "rescale")
    if (!all(vapply(anchors, identical, logical(1), anchors[[1L]]))) {
      stop("Compared indices use different rescaling targets; use common normalisation or recalculate comparable raw indices.", call. = FALSE)
    }
  }
  for (field in c("random_effects", "spatial_fields", "response_transform", "reference")) {
    values <- lapply(meta, `[[`, field)
    if (!all(vapply(values, identical, logical(1), values[[1L]]))) {
      notes <- c(notes, paste("Sources differ in", field, "convention; verify the intended target comparison."))
    }
  }
  list(verified = verified, unverified = unverified, notes = notes)
}

.ratio_order_years <- function(years) {
  numeric <- suppressWarnings(as.numeric(years))
  if (all(is.finite(numeric)) && !anyDuplicated(numeric)) years[order(numeric)] else years
}

#' Plot a separate index-ratio diagnostic
#'
#' Plot stored point-estimate ratios with a reference line at one. Missing or
#' undefined ratios remain gaps; lines never bridge an internally missing year.
#' @md
#' @param x,object An `influ_index_ratio` from [index_ratio()].
#' @param log Use a logarithmic ratio axis? Requires strictly positive defined
#'   ratios; zero ratios are retained in linear displays, never replaced.
#' @param ... Reserved for future use; unused arguments are rejected.
#' @return A ggplot object, using saved values only.
#' @seealso [index_ratio()]
#' @export
plot_index_ratio <- function(x, log = FALSE, ...) {
  if (!inherits(x, "influ_index_ratio") || !is.data.frame(x$table) ||
      !all(c("year", "numerator", "denominator", "ratio", "status") %in% names(x$table))) {
    stop("Supply an `influ_index_ratio` calculated by `index_ratio()`.", call. = FALSE)
  }
  if (length(list(...))) stop("Unused index-ratio plotting arguments.", call. = FALSE)
  if (!is.logical(log) || length(log) != 1L || is.na(log)) stop("`log` must be TRUE or FALSE.", call. = FALSE)
  d <- x$table
  if (!nrow(d)) stop("The stored ratio table has no annual rows.", call. = FALSE)
  good <- d$status == "ok" & is.finite(d$ratio)
  if (log && any(d$ratio[good] <= 0)) stop("A log ratio display requires positive ratios; use the linear display to retain zero numerators.", call. = FALSE)
  # Calendar and consistently formatted fishing years share CDI's complete
  # display axis. Calculation tables still contain actual supplied labels only.
  years <- .cdi_year_axis(unique(d$year))$labels
  numeric <- suppressWarnings(as.numeric(years))
  numeric_axis <- all(is.finite(numeric)) && !anyDuplicated(numeric)
  d$x <- if (numeric_axis) numeric[match(d$year, years)] else factor(d$year, levels = years)
  # Display labels are not keys: valid labels containing '/' can otherwise
  # turn distinct ordered pairs into exactly the same pasted string.
  pairs <- unique(d[c("numerator", "denominator")])
  d$pair_id <- NA_integer_
  for (i in seq_len(nrow(pairs))) {
    matched <- d$numerator == pairs$numerator[i] &
      d$denominator == pairs$denominator[i]
    d$pair_id[matched] <- i
  }
  enclose <- function(label) ifelse(grepl("/", label, fixed = TRUE),
    paste0("(", label, ")"), label)
  display <- make.unique(paste(enclose(pairs$numerator), "/",
    enclose(pairs$denominator)), sep = " #")
  d$Comparison <- factor(display[d$pair_id], levels = display)
  d$segment <- NA_integer_
  for (pair_id in seq_len(nrow(pairs))) {
    rows <- which(d$pair_id == pair_id)
    position <- match(d$year[rows], years)
    ordering <- order(position)
    rows <- rows[ordering]
    positions <- position[ordering]
    valid <- good[rows]
    separated <- c(TRUE, !utils::head(valid, -1L) | !utils::tail(valid, -1L) |
      diff(positions) != 1L)
    if (numeric_axis && all(numeric == floor(numeric))) {
      separated <- separated | c(TRUE, diff(d$x[rows]) > 1)
    }
    d$segment[rows] <- cumsum(separated)
  }
  defined <- d[good, , drop = FALSE]
  caption <- paste("Point-estimate ratios only; no ratio uncertainty is inferred.",
    if (any(!good)) paste(sum(!good), "undefined year/comparison ratios remain gaps; inspect as.data.frame() for their status.") else "")
  subtitle <- if (identical(x$metadata$normalise, "common")) {
    paste("Common geometric-mean baseline:", paste(x$metadata$baseline_years, collapse = ", "))
  } else "Supplied index scaling retained"
  caption <- paste(strwrap(trimws(caption), width = 90L), collapse = "\n")
  subtitle <- paste(strwrap(subtitle, width = 90L), collapse = "\n")
  p <- ggplot2::ggplot(defined, ggplot2::aes(x = .data$x, y = .data$ratio,
    colour = .data$Comparison,
    group = interaction(.data$pair_id, .data$segment))) +
    ggplot2::geom_hline(yintercept = 1, linetype = 2, colour = "grey45") +
    ggplot2::geom_line(na.rm = TRUE) + ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::labs(x = x$metadata$focus %||% "Year", y = "Index ratio",
      colour = "Numerator / denominator", subtitle = subtitle, caption = trimws(caption))
  if (numeric_axis) p <- p + ggplot2::scale_x_continuous(limits = range(numeric)) else {
    p <- p + ggplot2::scale_x_discrete(limits = years, drop = FALSE)
  }
  if (log) p <- p + ggplot2::scale_y_log10() else p <- p +
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.05)))
  p
}

#' @rdname index_ratio
#' @md
#' @param x,object A calculated `influ_index_ratio`.
#' @param row.names,optional Passed to the data-frame method.
#' @param ... Additional arguments passed to the data-frame method; otherwise unused.
#' @export
as.data.frame.influ_index_ratio <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(x$table, row.names = row.names, optional = optional, ...)
}

#' @rdname index_ratio
#' @export
print.influ_index_ratio <- function(x, ...) {
  cat("<influ_index_ratio>\n  Comparisons: ", nrow(x$metadata$comparisons),
    "\n  Normalisation: ", x$metadata$normalise,
    "\n  Uncertainty: point estimates only\n", sep = "")
  print(x$table, row.names = FALSE, ...)
  invisible(x)
}

#' @rdname index_ratio
#' @export
summary.influ_index_ratio <- function(object, ...) {
  d <- object$table
  out <- lapply(seq_len(nrow(object$metadata$comparisons)), function(i) {
    pair <- object$metadata$comparisons[i, ]
    current <- d[d$numerator == pair$numerator & d$denominator == pair$denominator, ]
    ok <- current$status == "ok"
    data.frame(numerator = pair$numerator, denominator = pair$denominator,
      defined_years = sum(ok), undefined_years = sum(!ok),
      minimum = if (any(ok)) min(current$ratio[ok]) else NA_real_,
      maximum = if (any(ok)) max(current$ratio[ok]) else NA_real_,
      stringsAsFactors = FALSE)
  })
  rownames_out <- do.call(rbind, out)
  rownames(rownames_out) <- NULL
  rownames_out
}

#' @rdname plot_index_ratio
#' @md
#' @export
plot.influ_index_ratio <- function(x, ...) plot_index_ratio(x, ...)

#' @rdname plot_index_ratio
#' @md
#' @export
autoplot.influ_index_ratio <- function(object, ...) plot_index_ratio(object, ...)
