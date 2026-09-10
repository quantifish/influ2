.resid_general_type <- function(type) {
  if (!is.character(type) || length(type) != 1L || is.na(type) ||
      !type %in% c("quantile", "generalised", "generalized")) {
    stop("These helpers use generalised simulation-based quantile residuals. ",
      "Native residual types (including Pearson) are no longer supported; ",
      "remove `type` or use `type = \"quantile\"`.", call. = FALSE)
  }
}

.resid_helper_input <- function(fit, ...) {
  if (inherits(fit, "influ_diag")) {
    stop("Supply the fitted model or an influ_residuals object, not influ_diag.", call. = FALSE)
  }
  if (inherits(fit, "influ_residuals")) {
    if (length(list(...))) {
      stop("Calculation arguments cannot change stored residuals; recalculate ",
        "with influ_residuals() first.", call. = FALSE)
    }
    return(fit)
  }
  influ_residuals(fit, ...)
}

#' Plot generalised residual departures by year and group
#'
#' The maintained successor to the historical residual-implied coefficient
#' display. Plot each year-by-group mean normal-score rank residual around
#' zero, using exactly the same calculation as [influ_residuals()].
#'
#' @details This function no longer adds residuals to year coefficients.
#'   Normal-score residuals are dimensionless; adding them (or Pearson residuals)
#'   to link-scale effects does not produce coefficients of an interaction.
#'   Positive departures indicate observations tending towards the upper part
#'   of their predictive distributions, not a percentage correction to CPUE.
#'   Actual coefficient effects remain available through [influ()]; an
#'   interaction-specific index requires a separately fitted model.
#'
#'   Bars show mean plus/minus SD/sqrt(n), a descriptive iid standard error,
#'   not an interval accounting for dependence, model estimation, or simulation
#'   error. They are not confidence intervals for interaction coefficients.
#'   Singleton strata have no bar. Unsupported and missing strata are not joined
#'   across intervening sampled years. All panels share the same residual scale.
#'   Specify groups independently of the outcome; response-defined selection
#'   invalidates the zero reference. Outcome columns are rejected, but derived
#'   outcome groups cannot be detected automatically.
#'
#'   To calculate once and redraw without simulation, retain the required
#'   columns with `influ_residuals(fit, groups = c("area", "gear"))`. Then pass
#'   that object here. Original-data alignment and component selection are
#'   performed during calculation. A combined delta diagnostic is not a
#'   positive-component diagnostic; use an explicit supported `component`.
#'   Saved objects lacking the group columns must be recalculated. Data cannot
#'   be attached later to an object without its fitted-observation provenance.
#'
#' @references Starr, P. J., and Kendrick, T. H. (2019). FLA 1 Fishery
#'   Characterisation and CPUE. New Zealand Fisheries Assessment Report
#'   2019/09, Figure O.9; Middleton, D. A. J. (2025). A Rapid Update of CPUE
#'   for the Snapper Fishery in SNA 2 to 2024. FAR 2025/32, Appendix C.
#'   These motivate the grouping, not the new normal-score scale.
#'   Dunn, P. K., and Smyth, G. K. (1996). Randomized quantile residuals.
#'   Journal of Computational and Graphical Statistics 5(3), 236-244.
#' @param fit A supported fitted model or a precomputed `influ_residuals` object.
#' @param data Original model data with original row names, for calculation only.
#' @param year Time column; `NULL` uses the standard automatic detection.
#'   With a stored object, an explicit value must match its recorded time column.
#' @param groups One retained categorical column used for panels.
#' @param type `"quantile"`, `"generalised"`, or `"generalized"`. All select
#'   the same simulation-based normal-score residuals; native types are rejected.
#' @param min_n Minimum records required in a year-by-group stratum.
#' @param colour Colour used for departures.
#' @param ... Calculation options passed to [influ_residuals()], e.g. `nsim`,
#'   `batch_size`, `seed`, and `component`. Not accepted for a stored object.
#' @return A ggplot. Its `data` contains stratum means, counts, and descriptive
#'   standard errors, not implied coefficients. The `residual_metadata`
#'   attribute records the simulation target.
#' @md
#' @export
plot_implied_residuals <- function(fit, data = NULL, year = NULL,
    groups = "area", type = "quantile", min_n = 10L, colour = "purple4", ...) {
  .resid_general_type(type)
  .resid_integer(min_n, "min_n", 1L)
  if (!is.character(groups) || length(groups) != 1L || is.na(groups) || !nzchar(groups)) {
    stop("`groups` must name one grouping column.", call. = FALSE)
  }
  if (inherits(fit, "influ_residuals")) {
    checks <- .resid_helper_input(fit, ...)
    if (!is.null(data) || (!is.null(year) && !identical(year, checks$metadata$year))) {
      stop("Stored residuals retain their original data and year; recalculate to change them.", call. = FALSE)
    }
  } else {
    checks <- .resid_helper_input(fit, data = data, year = year, groups = groups, ...)
  }
  if (!groups %in% names(checks$groups)) {
    stop("This object lacks the grouping column. Recalculate with ",
      "influ_residuals(..., groups = \"", groups, "\").", call. = FALSE)
  }
  d <- checks$observations
  if (nrow(checks$groups) != nrow(d) ||
      !identical(rownames(checks$groups), as.character(d$row))) {
    stop("Stored groups are not aligned with the residual observations.", call. = FALSE)
  }
  group <- checks$groups[[groups]]
  key <- interaction(d$year, factor(group), drop = TRUE, lex.order = TRUE)
  strata <- do.call(rbind, lapply(split(seq_len(nrow(d)), key), function(i) {
    r <- d$residual[i]
    data.frame(level = as.character(d$year[i[1L]]), group = as.character(group[i[1L]]),
      n = length(i), residual = mean(r), std_error = stats::sd(r) / sqrt(length(i)))
  }))
  strata <- strata[strata$n >= min_n, , drop = FALSE]
  if (!nrow(strata)) stop("No year-by-group strata meet `min_n`.", call. = FALSE)
  strata$lower <- strata$residual - strata$std_error
  strata$upper <- strata$residual + strata$std_error
  levels <- levels(d$year)
  strata$position <- match(strata$level, levels)
  strata <- strata[order(strata$group, strata$position), , drop = FALSE]
  strata$segment <- cumsum(c(TRUE, diff(strata$position) != 1L |
    utils::head(strata$group, -1L) != utils::tail(strata$group, -1L)))
  numeric_levels <- suppressWarnings(as.numeric(levels))
  positions <- if (all(is.finite(numeric_levels)) && !anyDuplicated(numeric_levels)) {
    numeric_levels
  } else seq_along(levels)
  strata$x <- positions[strata$position]
  ticks <- unique(round(seq(1, length(levels), length.out = min(5L, length(levels)))))
  p <- ggplot2::ggplot(strata,
    ggplot2::aes(x = .data$x, y = .data$residual, group = .data$segment)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "grey55") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
      width = 0.15, colour = colour, na.rm = TRUE) +
    ggplot2::geom_line(colour = colour) +
    ggplot2::geom_point(ggplot2::aes(size = .data$n), colour = colour) +
    ggplot2::scale_size_area(max_size = 4) +
    ggplot2::scale_x_continuous(breaks = positions[ticks], labels = levels[ticks],
      expand = ggplot2::expansion(mult = 0.08)) +
    ggplot2::facet_wrap(~group, ncol = 3) +
    ggplot2::labs(x = checks$metadata$year, y = "Mean normal-score rank residual",
      title = "Generalised residual departures",
      subtitle = paste(checks$metadata$component, "response | mean +/- descriptive SE"),
      size = "Records")
  attr(p, "residual_metadata") <- checks$metadata
  p
}

#' Plot predictive means against generalised residuals
#'
#' Uses [influ_residuals()], never native Pearson or deviance residuals.
#' The horizontal coordinate is the predictive mean from the same simulations,
#' preserving their response component and random-effect conditioning.
#' Pass a precomputed result to avoid repeating simulation when styling plots.
#' @param fit A supported fitted model or an `influ_residuals` object.
#' @param trend One of `"loess"`, `"lm"`, `"linear"`, or `"none"`.
#' @param type `"quantile"`, `"generalised"`, or `"generalized"` (equivalent).
#' @param ... Calculation options passed to [influ_residuals()]. Not accepted
#'   for a precomputed object. Complete brms fits are needed for calculation;
#'   existing posterior draws are used without fitting or running MCMC.
#' @return A ggplot with a `residual_metadata` attribute describing the target.
#' @md
#' @export
plot_predicted_residuals <- function(fit, trend = "loess", type = "quantile", ...) {
  .resid_general_type(type)
  trend <- match.arg(trend, c("loess", "lm", "linear", "none"))
  checks <- .resid_helper_input(fit, ...)
  p <- plot(checks, type = "fitted")
  # The unified panel adds at most one smoother; replace it only when requested.
  if (trend != "loess") {
    p$layers <- Filter(function(layer) !inherits(layer$geom, "GeomSmooth"), p$layers)
    if (trend != "none") p <- p + ggplot2::geom_smooth(method = "lm",
      formula = y ~ x, se = FALSE, colour = "steelblue4")
  }
  attr(p, "residual_metadata") <- checks$metadata
  p
}
