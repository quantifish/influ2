#' Plot residual-implied annual effects
#'
#' Display the fixed baseline and local residual-implied trajectories. This
#' restores the effect-scale question of the historical plot, with an explicit
#' traditional option. Use [plot_grouped_residuals()] for grouped PIT scores.
#' @param fit An `influ_implied` result or a supported fitted model.
#' @param colour Colour of implied-effect points, lines, and intervals.
#' @param ncol Number of facet columns, default three.
#' @param ... Calculation arguments passed to [implied_effects()] for a fitted
#'   model, including `method = "traditional"`. Not accepted for stored results:
#'   calculate a separate result to change its method, interval, or baseline.
#' @return A ggplot. Its data retain every stratum and its status; metadata
#'   describe the effect scale and conditional interpretation.
#' @examples
#' data(lobsters_per_pot)
#' fit <- glm(lobsters ~ year + month + depth, family = poisson(),
#'   data = lobsters_per_pot)
#' plot_implied_residuals(fit, groups = "month")
#' @md
#' @export
plot_implied_residuals <- function(fit, colour = "purple4", ncol = 3L, ...) {
  .resid_integer(ncol, "ncol", 1L)
  if (inherits(fit, "influ_implied")) {
    if (length(list(...))) stop("Calculation arguments cannot change stored implied effects; recalculate first.", call. = FALSE)
    result <- fit
  } else result <- implied_effects(fit, ...)
  d <- result$table
  m <- result$metadata
  d$group <- factor(d$group, levels = m$group_levels)
  levels <- m$year_levels
  numeric_levels <- suppressWarnings(as.numeric(levels))
  positions <- if (all(is.finite(numeric_levels)) && !anyDuplicated(numeric_levels)) numeric_levels else seq_along(levels)
  d$position <- match(d$level, levels)
  d$x <- positions[d$position]
  d <- d[order(d$group, d$position), ]
  usable <- d$status == "ok" & is.finite(d$estimate)
  if (!any(usable)) stop("No finite supported implied effects to plot; inspect the result table for sparse or boundary strata.", call. = FALSE)
  # Keep unsupported cells in data, but prevent lines from spanning them.
  d$segment <- cumsum(c(TRUE, diff(d$position) != 1L |
    utils::head(d$group, -1L) != utils::tail(d$group, -1L) |
    !utils::head(usable, -1L) | !utils::tail(usable, -1L)))
  shown <- d[usable, ]
  ticks <- unique(round(seq(1, length(levels), length.out = min(7L, length(levels)))))
  title <- if (m$method == "likelihood") "Residual-implied effects" else if (
    m$traditional_scale == "standardised") "Historical standardised-residual convention" else "Traditional log-residual implied effects"
  band <- switch(m$interval, conditional_profile = paste0(100 * m$level, "% conditional profile intervals"),
    descriptive = "Mean +/- one descriptive SE", none = "Intervals omitted")
  baseline <- if (m$baseline == "year_group") "Year + available group main effect" else "Year effect"
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$x, group = .data$segment)) +
    ggplot2::geom_line(data = shown, ggplot2::aes(y = .data$baseline), colour = "grey55") +
    ggplot2::geom_hline(yintercept = 0, linetype = 3, colour = "grey75") +
    ggplot2::geom_errorbar(data = shown,
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper), colour = colour, width = .15, na.rm = TRUE) +
    ggplot2::geom_line(data = shown, ggplot2::aes(y = .data$estimate), colour = colour) +
    ggplot2::geom_point(data = shown, ggplot2::aes(y = .data$estimate, size = .data$n), colour = colour) +
    ggplot2::scale_size_area(max_size = 4) +
    ggplot2::scale_x_continuous(breaks = positions[ticks], labels = levels[ticks],
      expand = ggplot2::expansion(mult = .06)) +
    ggplot2::facet_wrap(~group, ncol = ncol) +
    ggplot2::labs(x = m$year, y = if (m$method == "traditional" && m$traditional_scale == "standardised") {
      "Term + standardised residual (mixed scales)"
    } else if (m$log_response || m$link == "log") "Implied effect (log scale)" else "Implied effect (response scale)",
      title = title, subtitle = paste(baseline, "|", band), size = "Records",
      caption = paste("Grey: fixed baseline. Original model held fixed; not a refitted interaction.",
        sum(!usable), "empty, sparse, or boundary strata omitted."))
  attr(p, "implied_metadata") <- m
  p
}

#' @rdname plot_implied_residuals
#' @param x,object An `influ_implied` result.
#' @export
plot.influ_implied <- function(x, ...) plot_implied_residuals(x, ...)

#' @rdname plot_implied_residuals
#' @export
autoplot.influ_implied <- function(object, ...) plot_implied_residuals(object, ...)

#' @rdname implied_effects
#' @param x An `influ_implied` result.
#' @param ... Unused for printing and table extraction.
#' @export
print.influ_implied <- function(x, ...) {
  cat("Residual-implied effects (", x$metadata$method, ")\n", sep = "")
  cat(x$metadata$backend, "|", x$metadata$response, "| grouped by", x$metadata$groups, "and", x$metadata$year, "\n")
  cat(sum(x$table$status == "ok"), "supported strata of", nrow(x$table), "|", x$metadata$interval, "\n")
  cat("Original fitted model held fixed; not a refitted interaction.\n")
  invisible(x)
}

#' @rdname implied_effects
#' @param row.names Optional row names for the extracted table.
#' @param optional Passed to [as.data.frame()].
#' @export
as.data.frame.influ_implied <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(x$table, row.names = row.names, optional = optional, ...)
}
