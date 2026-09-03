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

.plot_influ_cdi <- function(x, term = NULL, component = NULL) {
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

  composition$x <- .plot_level(composition$level)
  composition$term_level <- factor(
    composition$term_level,
    levels = unique(composition$term_level[order(composition$effect)])
  )
  effects$x <- .plot_level(effects$level)

  coefficient_plot <- ggplot2::ggplot(
    coefficients,
    ggplot2::aes(x = .data$level, y = .data$estimate, colour = .data$component)
  ) +
    ggplot2::geom_point() +
    ggplot2::coord_flip() +
    ggplot2::labs(x = term, y = "Term contribution", colour = "Component") +
    ggplot2::theme_bw()

  distribution_plot <- ggplot2::ggplot(
    composition,
    ggplot2::aes(
      x = .data$x,
      y = .data$term_level,
      size = .data$proportion,
      colour = .data$effect
    )
  ) +
    ggplot2::geom_point(alpha = 0.8) +
    ggplot2::labs(
      x = x$focus,
      y = term,
      size = "Proportion",
      colour = "Contribution"
    ) +
    ggplot2::theme_bw()

  influence_plot <- .plot_influ_effects(
    x, term = term, component = component
  ) + ggplot2::theme(legend.position = "bottom")

  (coefficient_plot / distribution_plot) | influence_plot
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
#' @param ... Reserved for future plotting options.
#'
#' @return A `ggplot` or `patchwork` object.
#' @export
plot.influ_diag <- function(x, type = c("influence", "index", "cdi", "components"),
                            term = NULL, component = NULL, scale = NULL, ...) {
  type <- match.arg(type)
  switch(
    type,
    influence = .plot_influ_effects(x, term, component, scale),
    components = .plot_influ_effects(x, term, component, scale),
    index = .plot_influ_indices(x),
    cdi = .plot_influ_cdi(x, term, component)
  )
}

#' @rdname plot.influ_diag
#' @export
autoplot.influ_diag <- function(object, ...) {
  plot(object, ...)
}
