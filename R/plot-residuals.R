#' Plot a four-panel CPUE residual diagnostic
#'
#' Plot a precomputed [influ_residuals()] result without simulation or refitting.
#'
#' @param x,object An `influ_residuals` object.
#' @param type The four-panel `"overview"` (default), or one of `"qq"`,
#'   `"fitted"`, `"year"`, and `"distribution"`.
#' @param response_scale Scale for the response ECDF: `"identity"` or
#'   `"log1p"`, which retains zero catches. The latter requires non-negative
#'   responses and is labelled explicitly.
#' @param ... Reserved for future methods; currently unused.
#'
#' @details The year panel shows a boxplot for each sampled year and its sample
#'   size. Numeric years retain their spacing, including gaps; other labels are
#'   ordered lexically. Reference lines mark the normal-score median and
#'   quartiles. No smoother across years conceals changes in spread or tails.
#'   The fitted panel's horizontal variable is the simulation-based predictive
#'   mean under the conditioning recorded in the result. A descriptive loess
#'   curve is added when there are sufficient distinct fitted means.
#'
#'   The Q-Q envelope is a pointwise independent-uniform reference, not a
#'   model-specific calibration. The ECDF envelope is a pointwise predictive
#'   band on a compact grid. Neither envelope provides an automatic pass/fail
#'   test. Read the calculation metadata and [influ_residuals()] limitations.
#'
#' @return A ggplot or a four-panel patchwork object, which can be customised.
#' @export
plot.influ_residuals <- function(x,
    type = c("overview", "qq", "fitted", "year", "distribution"),
    response_scale = c("identity", "log1p"), ...) {
  type <- match.arg(type)
  response_scale <- match.arg(response_scale)
  if (type == "overview") {
    panels <- lapply(c("qq", "fitted", "year", "distribution"), function(p) {
      plot(x, type = p, response_scale = response_scale)
    })
    return(patchwork::wrap_plots(panels, ncol = 2) +
      patchwork::plot_annotation(caption = paste(
        x$metadata$backend, "|", x$metadata$nsim, "simulations |", x$metadata$scheme
      ), tag_levels = "A"))
  }
  purple <- "purple4"
  ylabel <- "Normal-score rank residual"
  coverage <- paste0(format(100 * x$metadata$level, trim = TRUE), "%")
  if (type == "qq") {
    return(ggplot2::ggplot(x$qq,
      ggplot2::aes(x = .data$theoretical, y = .data$residual)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
        fill = "grey85") +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey40") +
      ggplot2::geom_point(colour = purple, alpha = 0.5, size = 1) +
      ggplot2::labs(title = "Distributional Q-Q check",
        subtitle = paste(coverage, "pointwise iid-uniform reference"),
        x = "Theoretical normal quantile", y = ylabel))
  }
  d <- x$observations
  if (type == "fitted") {
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$predicted, y = .data$residual)) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "grey40") +
      ggplot2::geom_point(colour = purple, alpha = 0.25, size = 1)
    if (length(unique(d$predicted)) >= 5L && nrow(d) >= 20L) {
      p <- p + ggplot2::geom_smooth(method = "loess", formula = y ~ x,
        se = FALSE, colour = "steelblue4", linewidth = 0.7)
    }
    return(p + ggplot2::labs(title = "Residuals against fitted values",
      subtitle = "Predictive mean estimated from the same simulations",
      x = "Predictive mean response", y = ylabel))
  }
  if (type == "year") {
    levels <- levels(d$year)
    numeric <- suppressWarnings(as.numeric(levels))
    positions <- if (all(is.finite(numeric)) && !anyDuplicated(numeric)) {
      numeric
    } else seq_along(levels)
    d$position <- positions[as.integer(d$year)]
    counts <- as.integer(table(d$year))
    width <- if (length(positions) > 1L) min(diff(positions)) * 0.65 else 0.65
    return(ggplot2::ggplot(d, ggplot2::aes(x = .data$position,
      y = .data$residual, group = .data$year)) +
      ggplot2::geom_hline(yintercept = stats::qnorm(c(0.25, 0.75)),
        linetype = 3, colour = "grey70") +
      ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "grey40") +
      ggplot2::geom_boxplot(width = width, fill = "mediumpurple1", alpha = 0.5,
        outlier.size = 0.8, outlier.alpha = 0.4) +
      ggplot2::scale_x_continuous(breaks = positions,
        labels = paste0(levels, "\n(n=", counts, ")")) +
      ggplot2::labs(title = paste("Residuals by", x$metadata$year),
        subtitle = "Within-year spread, median, and tails; sample sizes below",
        x = x$metadata$year, y = ylabel) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)))
  }
  if (response_scale == "log1p" &&
      any(c(x$observed_ecdf$response, x$ecdf$response) < 0)) {
    stop("`response_scale = 'log1p'` requires non-negative responses.", call. = FALSE)
  }
  # Keep the envelope stepwise too: interpolating between count thresholds
  # would manufacture a discrepancy between a ribbon and matching ECDF steps.
  count <- nrow(x$ecdf)
  index <- rep(seq_len(count), each = 2L)
  band <- x$ecdf[index[-length(index)], , drop = FALSE]
  band$response <- x$ecdf$response[c(1L, rep(seq_len(count)[-1L], each = 2L))]
  ggplot2::ggplot(x$ecdf, ggplot2::aes(x = .data$response)) +
    ggplot2::geom_ribbon(data = band, ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
      fill = "grey80", alpha = 0.7) +
    ggplot2::geom_step(ggplot2::aes(y = .data$median, colour = "Simulated median"),
      linewidth = 0.7) +
    ggplot2::geom_step(data = x$observed_ecdf,
      ggplot2::aes(y = .data$probability, colour = "Observed"), linewidth = 0.7) +
    ggplot2::scale_colour_manual(values = c("Observed" = purple,
      "Simulated median" = "steelblue4"), name = NULL) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = ggplot2::expansion(mult = 0)) +
    ggplot2::scale_x_continuous(trans = response_scale) +
    ggplot2::labs(title = "Observed and simulated response distributions",
      subtitle = paste(coverage, "pointwise predictive band; compact ECDF grid"),
      x = paste0(x$metadata$response,
        if (response_scale == "log1p") " (log1p axis; zero retained)" else ""),
      y = "Cumulative probability") +
    ggplot2::theme(legend.position = "bottom")
}

#' @rdname plot.influ_residuals
#' @export
autoplot.influ_residuals <- function(object, ...) {
  plot(object, ...)
}
