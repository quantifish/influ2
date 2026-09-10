.plot_level <- function(x) {
  numeric <- suppressWarnings(as.numeric(as.character(x)))
  if (all(is.finite(numeric))) numeric else factor(x, levels = unique(x))
}

.plot_influ_effects <- function(x, term = NULL, component = NULL,
                                scale = NULL) {
  d <- x$influence
  if (!is.null(term)) d <- d[d$term %in% term, , drop = FALSE]
  if (!is.null(component)) d <- d[d$component %in% component, , drop = FALSE]
  if (is.null(scale)) d <- d[d$scale != "link", , drop = FALSE]
  else d <- d[d$scale %in% scale, , drop = FALSE]
  if (!nrow(d)) stop("No influence rows match the requested plot.", call. = FALSE)

  d$x <- .plot_level(d$level)
  baseline <- unique(d[c("scale")])
  baseline$baseline <- ifelse(baseline$scale == "ratio", 1, 0)

  ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = .data$x,
      y = .data$estimate,
      colour = .data$term,
      group = interaction(.data$term, .data$component)
    )
  ) +
    ggplot2::geom_hline(
      data = baseline,
      ggplot2::aes(yintercept = .data$baseline),
      inherit.aes = FALSE,
      linetype = 3,
      colour = "grey45"
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper, fill = .data$term),
      alpha = 0.14,
      colour = NA,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::facet_grid(.data$component ~ .data$scale, scales = "free_y") +
    ggplot2::labs(
      x = x$focus,
      y = "Influence",
      colour = "Term",
      fill = "Term"
    ) +
    ggplot2::theme_bw()
}

.plot_influ_indices <- function(x) {
  d <- x$indices
  if (!nrow(d)) stop("This diagnostic does not contain index results.", call. = FALSE)
  d <- unique(d)
  d$x <- .plot_level(d$level)

  ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = .data$x,
      y = .data$estimate,
      colour = .data$series,
      group = interaction(.data$series, .data$component)
    )
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper, fill = .data$series),
      alpha = 0.14,
      colour = NA,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::facet_wrap(~component + scale, scales = "free_y") +
    ggplot2::labs(
      x = x$focus,
      y = "Index",
      colour = "Series",
      fill = "Series"
    ) +
    ggplot2::theme_bw()
}

.cdi_plot_coefficients <- function(coefficients, term, coefficient_reference,
                                   coefficient_scale) {
  coefficient_reference <- match.arg(coefficient_reference, c("centred", "model"))
  coefficient_scale <- match.arg(coefficient_scale, c("auto", "link"))
  ratio <- coefficient_reference == "centred" && coefficient_scale == "auto" &&
    all(coefficients$cdi_scale == "ratio")
  prefix <- if (coefficient_reference == "model") "" else if (ratio) "relative_" else "centred_"
  columns <- paste0(prefix, c("estimate", "std_error", "lower", "upper"))
  if (!all(columns %in% names(coefficients))) {
    stop("Recalculate this diagnostic with influ() to obtain centred CDI summaries.", call. = FALSE)
  }
  coefficients[c("estimate", "std_error", "lower", "upper")] <- coefficients[columns]
  if (any(!is.finite(coefficients$estimate))) {
    stop("Finite CDI effects are unavailable; recalculate the diagnostic or use coefficient_scale = 'link'.", call. = FALSE)
  }
  if (ratio) {
    values <- unlist(coefficients[c("estimate", "lower", "upper")], use.names = FALSE)
    if (any(values <= 0 | is.infinite(values), na.rm = TRUE)) {
      stop("CDI ratios exceed the plotting range; use coefficient_scale = 'link'.", call. = FALSE)
    }
    label <- paste("Relative", term, "effect")
  } else {
    link <- unique(coefficients$link)
    zero_probability <- all(coefficients$complement) ||
      any(grepl("(^|:)zero_probability($|:)", coefficients$component))
    units <- if (all(coefficients$cdi_scale == "ratio")) "log scale" else {
      switch(link,
        identity = NULL,
        logit = if (zero_probability) "log-odds of zero" else "log-odds",
        probit = if (zero_probability) "probit of zero probability" else "probit scale",
        cloglog = if (zero_probability) "cloglog of zero probability" else "cloglog scale",
        paste(link, "scale")
      )
    }
    label <- if (coefficient_reference == "centred") {
      paste("Centred", term, "effect")
    } else paste(term, "contribution")
    if (length(units)) label <- paste0(label, " (", units, ")")
  }
  list(data = coefficients, ratio = ratio, label = label)
}

.plot_influ_cdi <- function(x, term = NULL, component = NULL,
                            coefficient_reference = "centred",
                            coefficient_scale = "auto") {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required for CDI plots.", call. = FALSE)
  }
  if (is.null(term)) term <- unique(x$influence$term)[1]
  if (length(term) != 1L) stop("A CDI plot displays one term at a time.", call. = FALSE)

  composition <- x$composition[x$composition$term == term, , drop = FALSE]
  effects <- x$influence[
    x$influence$term == term & x$influence$scale != "link",
    , drop = FALSE
  ]
  coefficients <- x$coefficients[x$coefficients$term == term, , drop = FALSE]
  if (!is.null(component)) {
    composition <- composition[composition$component %in% component, , drop = FALSE]
    effects <- effects[effects$component %in% component, , drop = FALSE]
    coefficients <- coefficients[coefficients$component %in% component, , drop = FALSE]
  }
  if (!nrow(composition) || !nrow(effects)) {
    stop("No CDI information is available for term '", term, "'.", call. = FALSE)
  }
  if (!nrow(coefficients)) {
    stop("No CDI coefficient summaries are available for this term and component.", call. = FALSE)
  }
  components <- unique(coefficients$component)
  if (length(components) != 1L) {
    stop(
      "A CDI plot displays one component at a time. Select component = one of: ",
      paste(components, collapse = ", "), ".", call. = FALSE
    )
  }
  # Combined response means have influence results but no separate term
  # coefficient. Keep all three panels on the selected component.
  composition <- composition[composition$component == components, , drop = FALSE]
  effects <- effects[effects$component == components, , drop = FALSE]
  coefficient_display <- .cdi_plot_coefficients(
    coefficients, term, coefficient_reference, coefficient_scale
  )
  coefficients <- coefficient_display$data

  term_levels <- unique(as.character(coefficients$level))
  if (!length(term_levels)) term_levels <- unique(as.character(composition$term_level))
  focus_levels <- unique(as.character(effects$level))
  coefficients$level <- factor(coefficients$level, levels = term_levels)
  composition$term_level <- factor(composition$term_level, levels = term_levels)
  composition$focus_level <- factor(composition$level, levels = focus_levels)
  effects$focus_level <- factor(effects$level, levels = focus_levels)
  # Months and other short categorical labels read cleanly horizontally.
  # Retain angled labels for the longer bin labels used by continuous terms.
  label_angle <- if (length(term_levels) <= 12L &&
      max(nchar(term_levels)) <= 6L) 0 else 45
  label_hjust <- if (label_angle == 0) 0.5 else 1

  coefficient_plot <- ggplot2::ggplot(
    coefficients,
    ggplot2::aes(
      x = .data$level,
      y = .data$estimate,
      group = .data$component
    )
  ) +
    ggplot2::geom_hline(
      yintercept = if (coefficient_display$ratio) 1 else 0,
      linetype = 3, colour = "grey45"
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
      width = 0.18,
      colour = "purple4",
      na.rm = TRUE
    ) +
    ggplot2::geom_point(colour = "purple4", size = 1.8) +
    ggplot2::scale_x_discrete(limits = term_levels, position = "top") +
    ggplot2::labs(x = NULL, y = coefficient_display$label) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = label_angle, hjust = label_hjust),
      legend.position = "none",
      plot.margin = ggplot2::margin(b = 1, r = 1, unit = "mm")
    )
  if (coefficient_display$ratio) {
    coefficient_plot <- coefficient_plot + ggplot2::scale_y_log10()
  }

  distribution_with_legend <- ggplot2::ggplot(
    composition,
    ggplot2::aes(
      x = .data$term_level,
      y = .data$focus_level,
      size = .data$proportion
    )
  ) +
    ggplot2::geom_point(colour = "purple4", fill = "purple", alpha = 0.65) +
    ggplot2::scale_x_discrete(limits = term_levels) +
    ggplot2::scale_y_discrete(limits = focus_levels) +
    ggplot2::scale_size_area(max_size = 10, breaks = .cdi_proportion_breaks) +
    ggplot2::guides(size = ggplot2::guide_legend(
      ncol = 1, title.position = "top"
    )) +
    ggplot2::labs(
      x = term,
      y = x$focus,
      size = "Proportion"
    ) +
    ggplot2::theme_bw()
  legend <- .cdi_size_legend(distribution_with_legend)
  legend_plot <- patchwork::wrap_elements(full = legend, clip = FALSE)
  distribution_plot <- distribution_with_legend +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = label_angle, hjust = label_hjust),
      plot.margin = ggplot2::margin(t = 1, r = 1, unit = "mm")
    )

  baseline <- ifelse(effects$scale == "ratio", 1, 0)
  influence_plot <- ggplot2::ggplot(
    effects,
    ggplot2::aes(
      x = .data$focus_level,
      y = .data$estimate,
      group = interaction(.data$component, .data$scale)
    )
  ) +
    ggplot2::geom_hline(
      yintercept = unique(baseline),
      linetype = 3,
      colour = "grey45"
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
      width = 0.18,
      colour = "purple4",
      na.rm = TRUE
    ) +
    ggplot2::geom_line(colour = "purple4", na.rm = TRUE) +
    ggplot2::geom_point(colour = "purple4", na.rm = TRUE) +
    # Before coord_flip(), the top x axis becomes the right-hand year axis.
    ggplot2::scale_x_discrete(limits = focus_levels, position = "top") +
    ggplot2::coord_flip() +
    ggplot2::labs(x = x$focus, y = "Influence") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(t = 1, l = 1, unit = "mm")
    )

  coefficient_plot + legend_plot + distribution_plot + influence_plot +
    patchwork::plot_layout(
      nrow = 2,
      ncol = 2,
      heights = c(1, 2),
      widths = c(2, 1)
    )
}

.cdi_proportion_breaks <- function(limits) {
  breaks <- pretty(limits, n = 5)
  breaks <- breaks[breaks > 0 & breaks >= limits[1] & breaks <= limits[2]]
  # Keep a short, single-column reference without altering the bubble scale.
  if (!length(breaks)) return(limits[2])
  utils::head(breaks, 4L)
}

.cdi_size_legend <- function(plot) {
  guides <- gtable::gtable_filter(ggplot2::ggplotGrob(plot), "guide-box", fixed = TRUE)
  # ggplot2 >= 3.5 includes empty slots for each legend position. Extract the
  # actual legend, without the parent plot's layout widths and heights.
  active <- which(!vapply(guides$grobs, inherits, logical(1), "zeroGrob"))
  if (length(active) != 1L) {
    stop("A CDI plot requires one size legend.", call. = FALSE)
  }
  guides$grobs[[active]]
}

#' Plot a model-neutral influence diagnostic
#'
#' @param x An [influ_diag] object.
#' @param object An [influ_diag] object passed to `autoplot()`.
#' @param type One of `"influence"`, `"index"`, `"cdi"`, or
#'   `"components"`.
#' @param term Optional term selection.
#' @param component Optional component selection.
#' @param scale Optional influence scale. By default the natural response
#'   contrast is plotted rather than the link-scale contrast.
#' @param coefficient_reference For CDI plots, `"centred"` (default) subtracts
#'   the term's mean over the same weighted reference distribution used for
#'   influence. `"model"` displays the original model-coded contribution on
#'   the link scale, including its reference factor level.
#' @param coefficient_scale For CDI plots, `"auto"` (default) displays centred
#'   log-response effects as ratios on a logarithmic axis. Other links remain
#'   in their labelled link units. `"link"` displays centred link effects for
#'   every model. The `"model"` reference always uses link units.
#' @param ... Reserved for future plotting options.
#'
#' @details CDI intervals use the probabilities supplied to [influ()] (95%
#'   by default). Centring propagates the joint coefficient covariance or is
#'   performed within each posterior/simulation draw. Ratio summaries are
#'   calculated after transforming those draws. A CDI plot displays one
#'   component at a time; select `component` when a term occurs in several
#'   model components. Zero-probability components retain their fitted link
#'   orientation and are explicitly labelled as such.
#'   Short term labels (including months) are horizontal on the upper fitted-
#'   effect axis and the lower composition axis. The influence panel's focus
#'   labels are on the right, with the same level ordering as the composition.
#'   The proportion legend has one column and at most four reference bubbles.
#'
#' @seealso \code{\link{influ_residuals}} and \code{\link{plot.influ_residuals}}
#'   for the separate residual overview and standalone
#'   \code{plot(checks, type = "qq")} diagnostic. Residual plots require an
#'   \code{influ_residuals} object, not an \code{influ_diag}.
#' @return A `ggplot` or `patchwork` object.
#' @export
plot.influ_diag <- function(x, type = c("influence", "index", "cdi", "components"),
                            term = NULL, component = NULL, scale = NULL,
                            coefficient_reference = "centred",
                            coefficient_scale = "auto", ...) {
  type <- match.arg(type)
  switch(
    type,
    influence = .plot_influ_effects(x, term, component, scale),
    components = .plot_influ_effects(x, term, component, scale),
    index = .plot_influ_indices(x),
    cdi = .plot_influ_cdi(x, term, component, coefficient_reference, coefficient_scale)
  )
}

#' @rdname plot.influ_diag
#' @importFrom ggplot2 autoplot
#' @export
autoplot.influ_diag <- function(object, ...) {
  plot(object, ...)
}
