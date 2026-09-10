#' Plot a four-panel CPUE residual diagnostic
#'
#' Plot a precomputed [influ_residuals()] result without simulation or refitting.
#'
#' @param x,object An `influ_residuals` object.
#' @param type The four-panel `"overview"` (default), or one of `"qq"`,
#'   `"fitted"`, `"year"`, `"distribution"`, `"calibration"`,
#'   `"calibration_groups"`, `"pit_ecdf"`, and `"pit_ecdf_diff"`.
#'   Grouped calibration shows observed-minus-predicted
#'   proportions for the scientific groups chosen during calculation.
#' @param response_scale Scale for the response ECDF: `"identity"` or
#'   `"log1p"`, which retains zero responses. The latter requires non-negative
#'   responses and is labelled explicitly.
#' @param response_diagnostic Fourth overview panel: `"auto"` chooses
#'   probability calibration for Bernoulli/encounter responses and the existing
#'   ECDF for other families (including grouped binomial and combined responses).
#'   `"distribution"` and `"calibration"` explicitly select a panel. Explicit
#'   `type` takes precedence. A calibration panel always uses probability axes,
#'   never `response_scale`. It requires stored fitted-probability summaries.
#' @param panels Optional character vector of exactly four panel types, in
#'   row-wise order, used only with `type = "overview"`. Any standalone type
#'   above is allowed, including repetitions. `"auto"` selects the response
#'   check using `response_diagnostic`. The default `NULL` is equivalent to
#'   `c("qq", "fitted", "year", "auto")`. Required summaries must already
#'   exist in `x`; selecting a panel never recalculates residuals.
#' @param pit_grid_size Number of evaluation points for the PIT ECDF and its
#'   simultaneous reference limits, between 2 and 1000 (default 100). Used
#'   only for PIT-ECDF panels, not for the stored response ECDF grid.
#' @param ... Reserved for future methods; currently unused.
#'
#' @details In the default overview, panels A-C use simulation-based randomised PIT (probability integral
#'   transform) ranks on the standard-normal scale, `qnorm(pit)`. The overview
#'   caption identifies the selected panels and distinguishes panel D: a response
#'   ECDF or probability-calibration check, not a PIT-residual distribution.
#'   Transforming the ranks does not establish normality or model calibration.
#'
#'   The year panel shows a boxplot for each sampled year and its sample
#'   size through box widths proportional to the square root of the number of
#'   observations. Numeric years retain their spacing, including gaps; other labels are
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
#'   Calibration uses fixed, roughly equal-count bins of original fitted
#'   probabilities. Grey ranges are pointwise predictive envelopes for observed
#'   bin proportions, not confidence intervals for a calibration curve. Point
#'   size represents observation count; crosses identify sparse support.
#'   Grouped binomial calibration is available explicitly with known trials;
#'   it pools successes/trials and trial-weights predicted probabilities.
#'   Set bin/group options in [influ_residuals()], not while plotting: discarded
#'   simulations cannot be re-binned. Older objects without response metadata
#'   retain the distribution overview with an informative warning. Explicit
#'   distribution plots remain unchanged. Missing envelopes are not fabricated.
#'
#'   Use `plot(x, type = "qq")` to draw exactly the Q-Q panel from the overview
#'   on its own. Here `x` must be the result of [influ_residuals()], not an
#'   `influ_diag` influence summary. The returned ggplot reuses stored results;
#'   no simulations or model fits are repeated. This is the supported Q-Q
#'   workflow, replacing the retired `plot_qq()` native-residual helper. It is
#'   a different diagnostic, not a reproduction of that helper's residuals.
#'   The Q-Q ribbon here is not posterior uncertainty around individual points.
#'
#'   Use `plot(x, type = "distribution")` for the standalone response ECDF,
#'   including when a Bernoulli overview defaults to calibration. It reuses
#'   the stored observed ECDF, simulated median, and pointwise predictive band.
#'   This is not an ECDF of residuals or a LOO-PIT diagnostic. brms results
#'   summarise existing posterior predictive draws, not new MCMC.
#'
#'   `type = "pit_ecdf"` uses the optional package **bayesplot** to plot the
#'   ECDF of the stored PIT values against a uniform reference. The difference
#'   version, `"pit_ecdf_diff"`, plots `ECDF(u) - u` against PIT value `u`,
#'   with zero as the reference. These reuse the same ranks as the normal-score
#'   Q-Q plot, not new residuals, an analytic PIT, or LOO-PIT. `response_scale`
#'   does not change their uniform horizontal scale.
#'
#'   The PIT plots delegate to `bayesplot::ppc_pit_ecdf()` with
#'   `method = "independent"`, numerically adjusted simultaneous reference limits, and
#'   the stored `level`. The limits assume independent uniform PIT values;
#'   they are not fitted-model-calibrated bands, and do not correct parameter
#'   estimation, posterior predictive reuse, or latent dependence. bayesplot's
#'   alternative dependence-aware tests are not automatically applied to these
#'   fitted-data ranks. No p-value, refit, or further response simulation is
#'   requested. The bridge does not change bayesplot's global theme or colours.
#'
#' @seealso [influ_residuals()] for a worked calculation and standalone Q-Q
#'   and ECDF examples; `vignette("residual-diagnostics")` for Bayesian examples.
#'   [as_influ_residuals()] creates the same plotting object from supplied simulations.
#' @return A ggplot or a four-panel patchwork object, which can be customised.
#' @md
#' @export
plot.influ_residuals <- function(x,
    type = c("overview", "qq", "fitted", "year", "distribution", "calibration", "calibration_groups", "pit_ecdf", "pit_ecdf_diff"),
    response_scale = c("identity", "log1p"), ...,
    response_diagnostic = c("auto", "distribution", "calibration"),
    panels = NULL, pit_grid_size = 100L) {
  type <- match.arg(type)
  response_scale <- match.arg(response_scale)
  response_diagnostic <- match.arg(response_diagnostic)
  if (type != "overview" && !is.null(panels)) {
    stop("`panels` is only used with `type = 'overview'`.", call. = FALSE)
  }
  if (type == "overview") {
    selected <- .resid_select_panels(x, panels, response_diagnostic)
    plots <- lapply(selected, function(p) {
      plot(x, type = p, response_scale = response_scale, pit_grid_size = pit_grid_size)
    })
    return(patchwork::wrap_plots(plots, ncol = 2) +
      patchwork::plot_annotation(caption = .resid_panel_caption(x, selected), tag_levels = "A"))
  }
  if (type %in% c("pit_ecdf", "pit_ecdf_diff")) {
    return(.plot_residual_pit(x, difference = type == "pit_ecdf_diff", grid_size = pit_grid_size))
  }
  if (type %in% c("calibration", "calibration_groups")) {
    return(.plot_residual_calibration(x, grouped = type == "calibration_groups"))
  }
  purple <- "purple4"
  ylabel <- "Normal-score PIT residual"
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
    width <- if (length(positions) > 1L) min(diff(positions)) * 0.65 else 0.65
    return(ggplot2::ggplot(d, ggplot2::aes(x = .data$position,
      y = .data$residual, group = .data$year)) +
      ggplot2::geom_hline(yintercept = stats::qnorm(c(0.25, 0.75)),
        linetype = 3, colour = "grey70") +
      ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "grey40") +
      ggplot2::geom_boxplot(width = width, varwidth = TRUE,
        fill = "mediumpurple1", alpha = 0.5,
        outlier.size = 0.8, outlier.alpha = 0.4) +
      ggplot2::scale_x_continuous(breaks = positions,
        labels = levels) +
      ggplot2::labs(title = paste("Residuals by", x$metadata$year),
        subtitle = "Box widths proportional to square root of sample size",
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
