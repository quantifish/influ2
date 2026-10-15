#' Overlay coefficient, distribution, and influence diagnostics
#'
#' Display one CDI, or overlay several model runs within the same three-panel
#' CDI layout. Saved diagnostics are plotted without recalculating estimates,
#' uncertainty intervals, or centring references.
#'
#' @param fits A fitted model supported by [influ()], an `influ_diag`, or a
#'   non-empty list of either. A named list supplies default model labels.
#' @param labels Optional unique, non-empty model labels.
#' @param year Optional focus-variable name when calculating diagnostics from
#'   models. Saved diagnostics must already use this focus.
#' @param term One term shared by all diagnostics. Defaults to the first
#'   influence term in the first diagnostic.
#' @param component One component shared by all diagnostics. This must be
#'   supplied when several components have coefficient summaries.
#' @param years `"union"` (default) displays all available years; `"overlap"`
#'   displays years available in every diagnostic. Integer calendar years keep
#'   their chronological spacing, including empty positions for missing years.
#'   Consistently formatted fishing years such as `"2020/21"` or `"2020-2021"`
#'   are also recognised; arbitrary text labels are not reinterpreted as years.
#' @param coefficient_reference,coefficient_scale,coefficient_order As in
#'   [plot.influ_diag()]. Multiple-model plots retain each diagnostic's own
#'   stored reference; they do not calculate a common centring reference.
#' @param order_by For ascending or descending ordering with multiple models,
#'   the label or number of the model supplying the ordering estimates. Levels
#'   unavailable in that model are appended in their original order.
#' @param colours For multiple models, optional colours, one per model. A named
#'   vector is matched to model labels. The same colours identify coefficients,
#'   bubbles, and lines. A single CDI retains its established purple styling.
#' @param ... Arguments passed to [influ()] for fitted-model inputs. These do
#'   not refit models. Plotting a single diagnostic delegates to its existing
#'   CDI renderer. In mixed lists these arguments apply only to fitted models;
#'   saved diagnostics are unchanged. Arguments in `...` are rejected when all
#'   inputs are saved diagnostics, rather than silently ignoring them.
#'
#' @details Coefficients and distribution bubbles use identical horizontal
#'   model offsets within each category. Bubble areas share one proportion
#'   scale. Missing categories and years are not filled with zero, and influence
#'   lines do not connect across missing integer years. The overlap view only
#'   filters displayed years: coefficients retain their original reference.
#'
#'   Compared effects must have compatible components, links, and orientations.
#'   Matching scales do not establish that responses or model populations are
#'   scientifically comparable. In particular, encounter, positive-response,
#'   zero-probability, and combined-response components are not interchangeable.
#'   Interval-labelled continuous bins cannot safely be matched from rounded
#'   compact summaries, so multi-model comparisons reject them. To compare such
#'   covariates, define shared categorical strata before calculating diagnostics.
#'   Aggregate latent-field effects are also rejected when their stored levels
#'   are field values rather than identified strata. An aggregate
#'   `random_effects` term is comparable only when both diagnostics record the
#'   same single random-intercept grouping variable and retain its level IDs.
#'
#'   The comparison caption identifies model-native centring. Model-coded
#'   coefficients (`coefficient_reference = "model"`) remain available for a
#'   single CDI, but multiple-model plots require centred summaries because
#'   compact diagnostics cannot verify identical native contrast coding.
#'   A stored `prediction_grid` label alone does not prove that two models used
#'   identical reference data and weights. No shared-reference claim is made.
#'
#' @return A `patchwork` object. For multiple models its `cdi_comparison`
#'   attribute contains compact display tables and the common axis settings,
#'   without fitted models or posterior draws.
#' @seealso [plot.influ_diag()], [plot_compare()], [plot_index_ratio()]
#' @examples
#' data(lobsters_per_pot)
#' full <- glm(lobsters ~ year + month + depth + soak, poisson(),
#'   data = lobsters_per_pot)
#' recent_data <- droplevels(subset(lobsters_per_pot,
#'   as.integer(as.character(year)) >= 2007))
#' recent <- update(full, data = recent_data)
#' diagnostics <- list(Full = influ(full, focus = "year"),
#'   Recent = influ(recent, focus = "year"))
#' plot_cdi(diagnostics, term = "month")
#' @md
#' @export
plot_cdi <- function(fits, labels = NULL, year = NULL, term = NULL,
                     component = NULL, years = c("union", "overlap"),
                     coefficient_reference = "centred", coefficient_scale = "auto",
                     coefficient_order = "original", order_by = NULL,
                     colours = NULL, ...) {
  years <- match.arg(years)
  coefficient_reference <- match.arg(coefficient_reference, c("centred", "model"))
  coefficient_scale <- match.arg(coefficient_scale, c("auto", "link"))
  coefficient_order <- match.arg(coefficient_order,
    c("original", "ascending", "descending"))
  saved <- inherits(fits, "influ_diag") || (is.list(fits) && length(fits) > 0L &&
    all(vapply(fits, inherits, logical(1), "influ_diag")))
  if (saved && length(list(...))) {
    stop("Arguments in `...` cannot recalculate saved diagnostics; calculate new diagnostics with influ() first.", call. = FALSE)
  }
  diagnostics <- .as_diagnostic_list(fits, year, ...)
  labels <- .cdi_model_labels(diagnostics, labels)
  if (!is.null(year) && (!is.character(year) || length(year) != 1L ||
      is.na(year) || any(vapply(diagnostics, function(d) !identical(d$focus, year), logical(1))))) {
    stop("`year` must match the focus in every saved diagnostic.", call. = FALSE)
  }
  if (length(diagnostics) == 1L) {
    if (!is.null(colours)) {
      stop("`colours` is a multiple-model option; a single CDI retains its established purple styling.", call. = FALSE)
    }
    return(.plot_influ_cdi(diagnostics[[1L]], term, component,
      coefficient_reference, coefficient_scale, coefficient_order))
  }
  if (coefficient_reference == "model") {
    stop("Multiple-model CDI plots require centred coefficients: identical native model coding cannot be verified from compact diagnostics.", call. = FALSE)
  }
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required for CDI plots.", call. = FALSE)
  }
  prepared <- .cdi_multiple_data(diagnostics, labels, term, component, years,
    coefficient_reference, coefficient_scale, coefficient_order, order_by)
  colours <- .cdi_model_colours(labels, colours)
  coefficient_plot <- .cdi_multiple_coefficients(prepared, colours)
  distribution_with_legend <- .cdi_multiple_distribution(prepared, colours)
  legend_plot <- patchwork::wrap_elements(
    full = .cdi_size_legend(distribution_with_legend), clip = FALSE)
  distribution_plot <- distribution_with_legend + ggplot2::theme(
    legend.position = "none", axis.text.x = ggplot2::element_text(angle = prepared$angle),
    plot.margin = ggplot2::margin(t = 1, r = 1, unit = "mm"))
  influence_plot <- .cdi_multiple_influence(prepared, colours)
  caption <- "Coefficients and influence retain each model's own centring reference; not a shared-reference comparison."
  out <- coefficient_plot + legend_plot + distribution_plot + influence_plot +
    patchwork::plot_layout(nrow = 2, ncol = 2, heights = c(1, 2), widths = c(2, 1)) +
    patchwork::plot_annotation(caption = caption,
      theme = ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0)))
  # Keep only compact tables and scalar settings, not the draw-time guide state.
  prepared$axis_state <- NULL
  prepared$colours <- colours
  attr(out, "cdi_comparison") <- prepared
  out
}

.cdi_model_labels <- function(diagnostics, labels) {
  if (is.null(labels)) {
    labels <- names(diagnostics)
    if (is.null(labels) || anyNA(labels) || any(!nzchar(trimws(labels)))) {
      labels <- paste("Model", seq_along(diagnostics))
    }
  }
  if (!is.character(labels) || length(labels) != length(diagnostics) || anyNA(labels) ||
      any(!nzchar(trimws(labels))) || anyDuplicated(trimws(labels))) {
    stop("`labels` must contain one unique, non-empty label per diagnostic.", call. = FALSE)
  }
  trimws(labels)
}

.cdi_model_colours <- function(labels, colours) {
  if (is.null(colours)) {
    colours <- if (length(labels) <= 4L) {
      c("#6A3D9A", "#E66101", "#1B9E77", "#377EB8")[seq_along(labels)]
    } else grDevices::hcl.colors(length(labels), "Dark 3")
  }
  if (!is.character(colours) || length(colours) != length(labels) || anyNA(colours)) {
    stop("Supply one valid colour per model.", call. = FALSE)
  }
  if (!is.null(names(colours))) {
    if (anyDuplicated(names(colours)) || !setequal(names(colours), labels)) {
      stop("Named `colours` must match the model labels exactly.", call. = FALSE)
    }
    colours <- colours[labels]
  }
  valid <- tryCatch({ grDevices::col2rgb(colours); TRUE }, error = function(e) FALSE)
  if (!valid) stop("Supply one valid colour per model.", call. = FALSE)
  stats::setNames(unname(colours), labels)
}

.cdi_selected_rows <- function(x, term, component) {
  coefficients <- x$coefficients[x$coefficients$term == term, , drop = FALSE]
  if (!is.null(component)) coefficients <- coefficients[
    coefficients$component == component, , drop = FALSE]
  components <- unique(coefficients$component)
  if (!nrow(coefficients)) {
    stop("Every model must contain CDI coefficient summaries for term '", term,
      "' and the selected component.", call. = FALSE)
  }
  if (length(components) != 1L) {
    stop("A CDI plot displays one component at a time; select `component` explicitly.", call. = FALSE)
  }
  composition <- x$composition[x$composition$term == term &
    x$composition$component == components, , drop = FALSE]
  influence <- x$influence[x$influence$term == term &
    x$influence$component == components & x$influence$scale != "link", , drop = FALSE]
  if (!nrow(composition) || !nrow(influence)) {
    stop("Every model must contain CDI composition and influence for the selected term and component.", call. = FALSE)
  }
  if (anyNA(coefficients$level) || anyNA(composition$term_level) || anyNA(influence$level) ||
      anyNA(composition$level) || anyDuplicated(coefficients$level) ||
      anyDuplicated(influence$level) || anyDuplicated(composition[c("level", "term_level")])) {
    stop("CDI levels must be non-missing and unique within each displayed model cell.", call. = FALSE)
  }
  if (!all(composition$term_level %in% coefficients$level) ||
      !all(composition$level %in% influence$level)) {
    stop("CDI composition levels must match the stored coefficients and influence years.", call. = FALSE)
  }
  if (any(!is.finite(composition$proportion)) ||
      any(composition$proportion < 0 | composition$proportion > 1)) {
    stop("CDI composition proportions must be finite and between zero and one.", call. = FALSE)
  }
  if (any(is.infinite(unlist(influence[c("estimate", "lower", "upper")], use.names = FALSE)))) {
    stop("CDI influence estimates and intervals must be finite or missing.", call. = FALSE)
  }
  list(coefficients = coefficients, composition = composition, influence = influence)
}

.cdi_year_axis <- function(levels) {
  labels <- unique(as.character(levels))
  number <- suppressWarnings(as.numeric(labels))
  numeric <- all(is.finite(number)) && !anyDuplicated(number)
  if (numeric) {
    labels <- labels[order(number)]
    number <- sort(number)
    # Preserve empty calendar-year positions rather than joining neighbouring
    # observed rows. The guard avoids huge axes for non-annual numeric codes.
    if (all(number == round(number)) && diff(range(number)) <= 10000L) {
      full <- seq.int(min(number), max(number))
      if (all(as.character(number) == labels)) labels <- as.character(full)
    }
  } else {
    fishing <- regmatches(labels, regexec("^([0-9]{4})([-/])([0-9]{2}|[0-9]{4})$", labels))
    if (all(lengths(fishing) == 4L)) {
      start <- as.integer(vapply(fishing, `[[`, character(1), 2L))
      separator <- vapply(fishing, `[[`, character(1), 3L)
      end_label <- vapply(fishing, `[[`, character(1), 4L)
      width <- nchar(end_label)
      expected <- ifelse(width == 2L, (start + 1L) %% 100L, start + 1L)
      canonical <- length(unique(separator)) == 1L && length(unique(width)) == 1L &&
        all(as.integer(end_label) == expected) && !anyDuplicated(start) &&
        diff(range(start)) <= 10000L
      if (canonical) {
        full <- seq.int(min(start), max(start))
        last <- if (width[1L] == 2L) sprintf("%02d", (full + 1L) %% 100L) else {
          as.character(full + 1L)
        }
        labels <- paste0(full, separator[1L], last)
      }
    }
  }
  list(labels = labels, positions = seq_along(labels))
}

.cdi_strata_signature <- function(x, term) {
  if (term == "random_effects") {
    group <- x$metadata$lme4$random_cdi_group %||%
      x$metadata$glmmTMB$random_cdi_group
    if (!is.character(group) || length(group) != 1L || is.na(group) || !nzchar(group)) {
      stop("Aggregate random_effects CDI cannot be overlaid without verified single random-intercept group and level IDs; recalculate supported diagnostics or select an identified term.", call. = FALSE)
    }
    return(list(kind = "random_intercept", group = group))
  }
  if (term %in% c("spatial_field", "spatiotemporal_field", "iid_random_effects",
      "smooths", "time_varying_effects", "spatially_varying_effects")) {
    stop("Aggregate latent-field CDI cannot be overlaid without identified, shared categorical strata; field values are not stratum IDs.", call. = FALSE)
  }
  list(kind = "named_term", term = term)
}

.cdi_multiple_data <- function(diagnostics, labels, term, component, years,
                               coefficient_reference, coefficient_scale,
                               coefficient_order, order_by) {
  if (is.null(term)) term <- unique(diagnostics[[1L]]$influence$term)[1L]
  if (!is.character(term) || length(term) != 1L || is.na(term) || !nzchar(term)) {
    stop("A CDI plot displays one term at a time.", call. = FALSE)
  }
  if (!is.null(component) && (!is.character(component) || length(component) != 1L ||
      is.na(component) || !nzchar(component))) {
    stop("A CDI plot displays one component at a time.", call. = FALSE)
  }
  focus <- vapply(diagnostics, `[[`, character(1), "focus")
  if (length(unique(focus)) != 1L) {
    stop("Compared CDI diagnostics must use the same focus variable.", call. = FALSE)
  }
  selected <- lapply(diagnostics, .cdi_selected_rows, term = term, component = component)
  strata <- lapply(diagnostics, .cdi_strata_signature, term = term)
  if (!all(vapply(strata, identical, logical(1), strata[[1L]]))) {
    stop("Compared CDI diagnostics must use the same identified grouping variable and strata.", call. = FALSE)
  }
  components <- vapply(selected, function(d) unique(d$coefficients$component), character(1))
  if (length(unique(components)) != 1L) {
    stop("Compared CDI diagnostics must use the same component; encounter, positive, and combined effects cannot be mixed.", call. = FALSE)
  }
  displays <- lapply(selected, function(d) .cdi_plot_coefficients(
    d$coefficients, term, coefficient_reference, coefficient_scale))
  if (any(vapply(displays, function(d)
      any(is.infinite(unlist(d$data[c("lower", "upper")], use.names = FALSE))), logical(1)))) {
    stop("CDI coefficient intervals must be finite or missing.", call. = FALSE)
  }
  orientation <- lapply(seq_along(selected), function(i) {
    coefficients <- selected[[i]]$coefficients
    list(ratio = displays[[i]]$ratio, label = displays[[i]]$label,
      scale = unique(coefficients$cdi_scale),
      link = if (displays[[i]]$ratio) "log response" else unique(coefficients$link),
      complement = unique(coefficients$complement),
      influence_scale = unique(selected[[i]]$influence$scale))
  })
  if (any(vapply(orientation, function(o) any(lengths(o) != 1L), logical(1))) ||
      !all(vapply(orientation, identical, logical(1), orientation[[1L]]))) {
    stop("Compared CDI effects must use compatible links, scales, and response orientations.", call. = FALSE)
  }
  term_levels <- unique(unlist(lapply(selected, function(d)
    as.character(d$coefficients$level)), use.names = FALSE))
  if (any(grepl("^[[(].*,.*[])]$", term_levels))) {
    stop("Continuous CDI bins cannot be safely matched from compact interval labels; calculate diagnostics using shared categorical strata first.", call. = FALSE)
  }
  if (coefficient_order != "original") {
    if (is.null(order_by)) {
      stop("Supply `order_by` to identify the model used for coefficient ordering.", call. = FALSE)
    }
    index <- if (is.character(order_by) && length(order_by) == 1L) match(order_by, labels) else order_by
    if (!is.numeric(index) || length(index) != 1L || !is.finite(index) ||
        index != floor(index) || index < 1L || index > length(labels)) {
      stop("`order_by` must identify one model by its label or number.", call. = FALSE)
    }
    d <- displays[[index]]$data
    ranked <- order(d$estimate, decreasing = coefficient_order == "descending", method = "radix")
    ranked_levels <- as.character(d$level[ranked])
    term_levels <- c(ranked_levels, setdiff(term_levels, ranked_levels))
  } else if (!is.null(order_by)) {
    stop("`order_by` is only used for ascending or descending coefficient ordering.", call. = FALSE)
  }
  available <- lapply(selected, function(d) as.character(d$influence$level))
  years_available <- if (years == "overlap") Reduce(intersect, available) else {
    unique(unlist(available, use.names = FALSE))
  }
  if (!length(years_available)) stop("The selected CDI diagnostics have no overlapping years.", call. = FALSE)
  axis <- .cdi_year_axis(years_available)
  offsets <- (seq_along(labels) - (length(labels) + 1) / 2) * (0.7 / length(labels))
  tables <- lapply(seq_along(selected), function(i) {
    coefficients <- displays[[i]]$data
    composition <- selected[[i]]$composition
    influence <- selected[[i]]$influence
    composition <- composition[composition$level %in% years_available &
      composition$proportion > 0, , drop = FALSE]
    influence <- influence[influence$level %in% years_available, , drop = FALSE]
    coefficients$category <- match(as.character(coefficients$level), term_levels)
    coefficients$x <- coefficients$category + offsets[i]
    composition$category <- match(as.character(composition$term_level), term_levels)
    composition$x <- composition$category + offsets[i]
    composition$year_position <- match(as.character(composition$level), axis$labels)
    influence$year_position <- match(as.character(influence$level), axis$labels)
    influence <- influence[order(influence$year_position), , drop = FALSE]
    # Missing or non-finite estimates break segments even if every year's row
    # exists in an older saved object. No unavailable value becomes zero.
    finite <- is.finite(influence$estimate)
    influence$segment <- cumsum(c(TRUE, diff(influence$year_position) != 1L |
      !utils::head(finite, -1L) | !utils::tail(finite, -1L)))
    for (table in c("coefficients", "composition", "influence")) {
      d <- get(table)
      d$Model <- factor(labels[i], levels = labels)
      assign(table, d)
    }
    list(coefficients = coefficients, composition = composition, influence = influence)
  })
  list(coefficients = do.call(rbind, lapply(tables, `[[`, "coefficients")),
    composition = do.call(rbind, lapply(tables, `[[`, "composition")),
    influence = do.call(rbind, lapply(tables, `[[`, "influence")),
    labels = labels, offsets = stats::setNames(offsets, labels),
    term = term,
    term_label = if (identical(strata[[1L]]$kind, "random_intercept")) strata[[1L]]$group else term,
    term_levels = term_levels, focus = focus[1L],
    year_levels = axis$labels, year_positions = axis$positions, years = years,
    component = components[1L], ratio = displays[[1L]]$ratio,
    strata = strata[[1L]],
    coefficient_label = displays[[1L]]$label,
    coefficient_reference = coefficient_reference,
    references = stats::setNames(vapply(diagnostics, function(d)
      d$metadata$reference %||% "unverified", character(1)), labels),
    interval_probabilities = stats::setNames(lapply(diagnostics, function(d)
      d$uncertainty$probs), labels),
    reference_verified_shared = FALSE,
    angle = if (max(nchar(term_levels)) <= 6L) 0 else 45,
    axis_state = new.env(parent = emptyenv()))
}

.cdi_category_scale <- function(d, top = FALSE) {
  ggplot2::scale_x_continuous(limits = c(.5, length(d$term_levels) + .5),
    breaks = seq_along(d$term_levels), labels = d$term_levels,
    expand = ggplot2::expansion(mult = 0), position = if (top) "top" else "bottom",
    guide = .cdi_axis_guide(d$angle, d$axis_state))
}

.cdi_multiple_coefficients <- function(d, colours) {
  p <- ggplot2::ggplot(d$coefficients,
    ggplot2::aes(x = .data$x, y = .data$estimate, colour = .data$Model)) +
    ggplot2::geom_hline(yintercept = if (d$ratio) 1 else 0,
      linetype = 3, colour = "grey45") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
      width = min(.18, .5 / length(d$labels)), na.rm = TRUE) +
    ggplot2::geom_point(size = 1.8) + .cdi_category_scale(d, top = TRUE) +
    ggplot2::scale_colour_manual(values = colours, limits = d$labels, drop = FALSE) +
    ggplot2::labs(x = NULL, y = d$coefficient_label) + ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = d$angle),
      plot.margin = ggplot2::margin(b = 1, r = 1, unit = "mm"))
  if (d$ratio) p <- p + ggplot2::scale_y_log10()
  p
}

.cdi_multiple_distribution <- function(d, colours) {
  maximum <- max(d$composition$proportion, 0)
  if (maximum == 0) maximum <- 1
  ggplot2::ggplot(d$composition, ggplot2::aes(x = .data$x,
      y = .data$year_position, size = .data$proportion, colour = .data$Model)) +
    ggplot2::geom_point(alpha = .65) + .cdi_category_scale(d) +
    ggplot2::scale_y_continuous(limits = c(.5, length(d$year_levels) + .5),
      breaks = d$year_positions, labels = d$year_levels,
      expand = ggplot2::expansion(mult = 0)) +
    ggplot2::scale_colour_manual(values = colours, limits = d$labels, drop = FALSE) +
    ggplot2::scale_size_area(max_size = 8, limits = c(0, maximum),
      breaks = .cdi_proportion_breaks) +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1,
      override.aes = list(size = 3, alpha = 1)),
      size = ggplot2::guide_legend(ncol = 1, title.position = "top", order = 2,
        override.aes = list(colour = "grey35", alpha = .8))) +
    ggplot2::labs(x = d$term_label, y = d$focus, colour = "Model", size = "Proportion") +
    ggplot2::theme_bw()
}

.cdi_multiple_influence <- function(d, colours) {
  scale <- unique(d$influence$scale)
  ggplot2::ggplot(d$influence, ggplot2::aes(x = .data$year_position,
      y = .data$estimate, colour = .data$Model,
      group = interaction(.data$Model, .data$segment))) +
    ggplot2::geom_hline(yintercept = if (scale == "ratio") 1 else 0,
      linetype = 3, colour = "grey45") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
      width = .18, alpha = .65, na.rm = TRUE) +
    ggplot2::geom_line(na.rm = TRUE) + ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::scale_x_continuous(limits = c(.5, length(d$year_levels) + .5),
      breaks = d$year_positions, labels = d$year_levels, position = "top",
      expand = ggplot2::expansion(mult = 0)) +
    ggplot2::scale_colour_manual(values = colours, limits = d$labels, drop = FALSE) +
    ggplot2::coord_flip() + ggplot2::labs(x = d$focus, y = "Influence") +
    ggplot2::theme_bw() + ggplot2::theme(legend.position = "none",
      plot.margin = ggplot2::margin(t = 1, l = 1, unit = "mm"))
}
