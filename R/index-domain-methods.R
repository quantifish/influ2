#' @rdname index_domain
#' @param x,object An `influ_domain` result from `index_domain()`.
#' @param what Stored table to extract or summarise: `"coverage"` (default),
#'   `"decomposition"`, `"omission"`, `"sampled_only"`, `"partition"`, or
#'   `"trend"`. Index comparisons include the full-domain baseline and a
#'   `Series` column; their stored metadata are attached as `index_metadata`.
#' @param row.names,optional Passed to [as.data.frame()].
#' @param ... Reserved for future use.
#' @export
as.data.frame.influ_domain <- function(x, row.names = NULL, optional = FALSE,
    ..., what = c("coverage", "decomposition", "omission", "sampled_only",
      "partition", "trend")) {
  what <- match.arg(what)
  if (what %in% c("omission", "sampled_only")) {
    indices <- c(list("Full index" = x$baseline), x[[what]])
    if (is.null(names(indices)) || any(!nzchar(names(indices)))) {
      stop("Stored index comparisons must have named series.", call. = FALSE)
    }
    out <- do.call(rbind, lapply(seq_along(indices), function(i) {
      d <- indices[[i]]$table
      d$Series <- names(indices)[i]
      d
    }))
    rownames(out) <- NULL
    attr(out, "index_metadata") <- lapply(indices, function(z) z$metadata)
  } else {
    out <- x[[what]]
    if (is.null(out)) out <- data.frame()
  }
  as.data.frame(out, row.names = row.names, optional = optional)
}

#' @rdname index_domain
#' @export
summary.influ_domain <- function(object, ...,
    what = c("coverage", "decomposition", "omission", "sampled_only",
      "partition", "trend")) {
  as.data.frame(object, what = match.arg(what), ...)
}

#' @rdname index_domain
#' @export
print.influ_domain <- function(x, ...) {
  cat("Index domain diagnostic\n")
  cat("Backend:", x$metadata$backend, "\n")
  cat("Years:", nrow(x$baseline$table), "\n")
  cat("Sampled definitions:", paste(names(x$sampled_only), collapse = ", "), "\n")
  if (!is.null(x$metadata$omission_definition)) {
    cat("Omission definition:", x$metadata$omission_definition, "\n")
  }
  cat("Omission scenarios:", paste(names(x$omission), collapse = ", "), "\n")
  if (length(x$metadata$caveats)) {
    cat("Caveats:\n", paste0("  ", x$metadata$caveats, collapse = "\n"), "\n",
      sep = "")
  }
  print(as.data.frame(x, what = "coverage"), ...)
  invisible(x)
}

#' @rdname index_domain
#' @param type Plot `"sampled_only"` (default), `"coverage"`,
#'   `"decomposition"`, `"omission"`, `"partition"`, `"share"`, or `"map"`.
#' @param show_probs Display stored pointwise intervals when available.
#' @param coords Two coordinate column names in retained cell contributions.
#'   If `NULL`, use `X`/`Y`, `x`/`y`, or `lon`/`lat`, when available.
#' @param years,terms,components Optional selectors for map years, terms, and
#'   components, matching the stored labels.
#' @details Plotting and table extraction reuse stored results; they do not
#'   predict again or refit a model. Sampled-only comparisons divide every
#'   displayed series and its interval by the same geometric mean of the full
#'   index over years with finite positive estimates in every displayed series.
#'   This display-only normalisation preserves differences in level; the lower
#'   panel shows coverage, not uncertainty in the sampling process. Unavailable
#'   years remain gaps. Other index panels retain the stored index scale.
#'   Decomposition ribbons show stored spatial contribution quantiles
#'   (`Q10` to `Q90`), not parameter-uncertainty intervals.
#'
#'   Omission is a prediction counterfactual, not a refitted reduced model.
#'   Contributions are additive on the link scale, not on the response scale.
#'   A sampled-only domain can change between years and is not a fixed-domain
#'   abundance index. Classification depends on the declared mapping and
#'   sampling thresholds, not on interpolation or a mesh boundary. When no
#'   sampling rule is supplied, the default half-grid-spacing distance rule
#'   warns: it is a convenience convention, not an inferred survey footprint.
#'   All three sampled-only definitions can be compared while omission uses
#'   the single stored `omission_definition` (by default `"cell_time"`).
#'   Year-effect omission uses the first year as its baseline. Covariate
#'   omission defaults to the sampled-year mean link contribution as its
#'   reference; this is not prediction at a mean response-scale covariate.
#' @return Table methods return a data frame, with index metadata attached for
#'   index comparisons. `print()` invisibly returns its input. Plot methods
#'   return a ggplot, or a patchwork for sampled-only comparisons.
#' @export
plot.influ_domain <- function(x,
    type = c("sampled_only", "coverage", "decomposition", "omission",
      "partition", "share", "map"), show_probs = TRUE, coords = NULL,
    years = NULL, terms = NULL, components = NULL, ...) {
  type <- match.arg(type)
  if (type %in% c("sampled_only", "omission")) {
    d <- as.data.frame(x, what = type)
    if (type == "sampled_only") d <- .domain_common_scale(d)
    p <- .domain_series_plot(d, "Mean", "Series", "Qlower", "Qupper",
      show_probs) +
      ggplot2::labs(y = if (type == "sampled_only") {
        "Index / common full-index geometric mean"
      } else "Index (stored scale)", title = if (type == "omission") {
        "Prediction counterfactuals (not refits)"
      } else "Full and sampled-only indices")
    if (type == "omission") return(p)
    coverage <- if (is.data.frame(x$domains) && nrow(x$domains)) {
      .domain_series_plot(x$domains, "share", "definition", show_probs = FALSE) +
        ggplot2::labs(y = "Sampled-domain weight share")
    } else plot(x, type = "coverage")
    return(patchwork::wrap_plots(p, coverage,
      ncol = 1, heights = c(2, 1)))
  }
  if (type == "map") {
    return(.domain_map(x, coords, years, terms, components))
  }
  d <- if (type == "share") x$partition else x[[type]]
  if (!is.data.frame(d) || !nrow(d)) {
    stop("No stored data for the requested domain plot.", call. = FALSE)
  }
  if (type == "coverage") {
    return(.domain_series_plot(d, "share", "class", show_probs = FALSE) +
      ggplot2::labs(y = "Reference-weight coverage", colour = "Domain class"))
  }
  if (type == "decomposition") {
    p <- .domain_series_plot(d, "mean", "class", "Q10", "Q90", show_probs) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
      ggplot2::facet_wrap(ggplot2::vars(.data$term, .data$component),
        scales = "free_y") +
      ggplot2::labs(y = "Mean link-scale contribution",
        subtitle = "Additive on the link scale, not the response scale")
    return(p)
  }
  if (type == "share") {
    full <- data.frame(Year = x$baseline$table$Year, class = "Full index",
      share = 1, share_lower = 1, share_upper = 1)
    columns <- intersect(names(d), names(full))
    d <- rbind(d[, columns, drop = FALSE], full[, columns, drop = FALSE])
    return(.domain_series_plot(d, "share", "class", "share_lower",
      "share_upper", show_probs) +
      ggplot2::labs(y = "Share of full index"))
  }
  full <- x$baseline$table
  full$class <- "Full index"
  columns <- intersect(names(d), names(full))
  d <- rbind(d[, columns, drop = FALSE], full[, columns, drop = FALSE])
  .domain_series_plot(d, "Mean", "class", "Qlower", "Qupper", show_probs) +
    ggplot2::labs(y = "Contribution to full index (stored scale)")
}

#' @rdname index_domain
#' @export
autoplot.influ_domain <- function(object, ...) plot(object, ...)

.domain_common_scale <- function(d) {
  series <- unique(d$Series)
  eligible <- lapply(series, function(z) {
    rows <- d$Series == z & is.finite(d$Mean) & d$Mean > 0
    as.character(d$Year[rows])
  })
  common <- Reduce(intersect, eligible)
  if (!length(common)) {
    stop("No common years with finite positive indices for display normalisation.",
      call. = FALSE)
  }
  rows <- d$Series == "Full index" & as.character(d$Year) %in% common
  divisor <- exp(mean(log(d$Mean[rows])))
  for (column in intersect(c("Mean", "SD", "Median", "Qlower", "Qupper"),
      names(d))) d[[column]] <- d[[column]] / divisor
  d
}

.domain_series_plot <- function(d, value, group, lower = NULL, upper = NULL,
    show_probs = TRUE) {
  labels <- .cdi_year_axis(d$Year)$labels
  numeric_years <- suppressWarnings(as.numeric(labels))
  d$.year_position <- match(as.character(d$Year), labels)
  numeric_axis <- all(is.finite(numeric_years)) && !anyDuplicated(numeric_years)
  d$.year <- if (numeric_axis) as.numeric(as.character(d$Year)) else {
    factor(d$Year, levels = labels)
  }
  grouping <- intersect(c(group, "term", "component"), names(d))
  key <- do.call(interaction, c(d[grouping], list(drop = TRUE)))
  d$.segment <- d$.ribbon <- integer(nrow(d))
  bounded <- !is.null(lower) && !is.null(upper) &&
    all(c(lower, upper) %in% names(d))
  for (level in unique(key)) {
    rows <- which(key == level)
    rows <- rows[order(d$.year_position[rows])]
    gap <- c(TRUE, diff(d$.year_position[rows]) != 1L)
    finite <- is.finite(d[[value]][rows])
    d$.segment[rows] <- cumsum(gap | !finite |
      c(TRUE, !utils::head(finite, -1L)))
    finite <- finite & if (bounded) {
      is.finite(d[[lower]][rows]) & is.finite(d[[upper]][rows])
    } else FALSE
    d$.ribbon[rows] <- cumsum(gap | !finite |
      c(TRUE, !utils::head(finite, -1L)))
  }
  d$.group <- key
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$.year,
    y = .data[[value]], colour = .data[[group]], fill = .data[[group]],
    group = interaction(.data$.group, .data$.segment)))
  if (isTRUE(show_probs) && bounded) {
    p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[[lower]],
      ymax = .data[[upper]],
      group = interaction(.data$.group, .data$.ribbon)),
      alpha = 0.18, colour = NA, na.rm = TRUE)
  }
  p <- p + ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(na.rm = TRUE) + ggplot2::theme_bw() +
    ggplot2::labs(x = "Year", colour = NULL, fill = NULL)
  if (!numeric_axis) {
    p <- p + ggplot2::scale_x_discrete(limits = labels, drop = FALSE)
  }
  p
}

.domain_map <- function(x, coords, years, terms, components) {
  d <- x$retained_cells
  if (!is.data.frame(d) || !nrow(d)) {
    stop("Map plots require retained cell contributions; recalculate with cell retention.",
      call. = FALSE)
  }
  if (is.null(coords)) {
    for (candidate in list(c("X", "Y"), c("x", "y"), c("lon", "lat"))) {
      if (all(candidate %in% names(d))) {
        coords <- candidate
        break
      }
    }
  }
  if (!is.character(coords) || length(coords) != 2L || anyNA(coords) ||
      anyDuplicated(coords) || !all(coords %in% names(d)) ||
      !all(vapply(d[coords], is.numeric, logical(1)))) {
    stop("Supply two numeric coordinate columns in `coords` for retained cells.",
      call. = FALSE)
  }
  selectors <- list(Year = years, term = terms, component = components)
  for (column in names(selectors)) {
    selected <- selectors[[column]]
    if (is.null(selected)) next
    if (anyNA(selected) || !length(selected) ||
        any(!as.character(selected) %in% as.character(d[[column]]))) {
      stop("Map selectors must match stored year, term, and component labels.",
        call. = FALSE)
    }
    d <- d[as.character(d[[column]]) %in% as.character(selected), , drop = FALSE]
  }
  if (!nrow(d)) stop("No retained cells match the map selectors.", call. = FALSE)
  ggplot2::ggplot(d, ggplot2::aes(x = .data[[coords[1]]],
    y = .data[[coords[2]]], colour = .data$contribution,
    shape = factor(.data$sampled))) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Year, .data$term, .data$component)) +
    ggplot2::coord_equal() +
    ggplot2::scale_colour_gradient2(low = "#2166AC", mid = "white",
      high = "#B2182B", midpoint = 0) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = coords[1], y = coords[2],
      colour = "Link-scale\ncontribution", shape = "Sampled",
      subtitle = "Full reference domain; prediction contributions, not refits")
}
