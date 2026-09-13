#' Plot calculated CPUE indices
#'
#' Display assessment indices without refitting or recalculating predictions.
#' @md
#'
#' @param x,object An [influ_index][cpue_index] object.
#' @param show_probs Show the stored pointwise uncertainty interval.
#' @param type `"index"` (default), `"correlation"`, or `"covariance"`. The
#'   latter two display the stored annual uncertainty matrix as a heatmap.
#' @param scale For matrix displays, `"log"` (default) or `"response"`.
#' @param ... Reserved for future use; unused.
#' @return A ggplot object.
#' @seealso [index_vcov()], [index_table()], [cpue_index()], [integrate_index()], [plot_compare()]
#' @export
plot_index <- function(x, show_probs = TRUE, ...,
    type = c("index", "correlation", "covariance"), scale = c("log", "response")) {
  if (!inherits(x, "influ_index")) {
    stop("Calculate an `influ_index` with `cpue_index()` or `integrate_index()` before plotting.", call. = FALSE)
  }
  type <- match.arg(type)
  scale <- match.arg(scale)
  if (type != "index") return(.plot_index_matrix(x, type, scale))
  .plot_cpue_indices(list(x), labels = "Index", show_probs = show_probs)
}

.plot_index_matrix <- function(x, type, scale) {
  covariance <- index_vcov(x, scale = scale)
  if (type == "correlation") {
    if (any(diag(covariance) <= 0)) {
      stop("Correlation is undefined for an index with zero variance; use a covariance plot instead.", call. = FALSE)
    }
    covariance <- stats::cov2cor(covariance)
  }
  years <- rownames(covariance)
  d <- expand.grid(row = years, column = years, stringsAsFactors = FALSE)
  d$value <- as.vector(covariance)
  d$row <- factor(d$row, levels = rev(years))
  d$column <- factor(d$column, levels = years)
  breaks <- years[unique(round(seq(1, length(years), length.out = min(8L, length(years)))))]
  limit <- if (type == "correlation") 1 else max(abs(covariance))
  if (limit == 0) limit <- 1
  ggplot2::ggplot(d, ggplot2::aes(x = .data$column, y = .data$row, fill = .data$value)) +
    ggplot2::geom_tile() +
    ggplot2::coord_fixed() +
    ggplot2::scale_x_discrete(breaks = breaks, expand = c(0, 0)) +
    ggplot2::scale_y_discrete(breaks = breaks, expand = c(0, 0)) +
    ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
      midpoint = 0, limits = c(-limit, limit)) +
    ggplot2::labs(x = "Year", y = "Year", fill = if (type == "correlation") {
      "Correlation"
    } else paste0("Covariance\n(", scale, " scale)"),
      title = paste("Annual index", type),
      subtitle = paste(if (scale == "log") "Log-index" else "Response-scale index",
        "uncertainty; not residual autocorrelation"))
}

#' @rdname plot_index
#' @export
plot.influ_index <- function(x, ...) plot_index(x, ...)

#' @rdname plot_index
#' @export
autoplot.influ_index <- function(object, ...) plot_index(object, ...)

.plot_cpue_indices <- function(fits, labels = NULL, show_probs = TRUE) {
  if (!all(vapply(fits, inherits, logical(1), "influ_index"))) {
    stop("Do not mix calculated CPUE indices with fitted models or influence diagnostics.", call. = FALSE)
  }
  reference <- fits[[1]]$metadata
  for (field in c("method", "scale", "units", "year", "rescale")) {
    if (!all(vapply(fits, function(x) identical(x$metadata[[field]], reference[[field]]), logical(1)))) {
      stop("Compared CPUE indices must agree on method, scale, units, year, and rescaling target.", call. = FALSE)
    }
  }
  if (identical(reference$method, "integrated")) {
    for (field in c("area_units", "response_units", "total_area", "catchability")) {
      if (!all(vapply(fits, function(x) identical(x$metadata[[field]], reference[[field]]), logical(1)))) {
        stop("Compared integrated indices must agree on area, response units, and catchability convention.", call. = FALSE)
      }
    }
  }
  if (!all(vapply(fits, function(x) identical(x$metadata$random_effects,
      reference$random_effects), logical(1)))) {
    warning("Compared indices use different random-effect conventions; check that their reference populations answer the intended comparison.", call. = FALSE)
  }
  if (is.null(labels)) {
    labels <- names(fits)
    if (is.null(labels) || any(!nzchar(labels))) labels <- paste("Model", seq_along(fits))
  }
  if (!is.character(labels) || length(labels) != length(fits) || anyNA(labels) ||
      any(!nzchar(trimws(labels))) || anyDuplicated(trimws(labels))) {
    stop("Supply one unique, non-empty label per index.", call. = FALSE)
  }
  d <- do.call(rbind, lapply(seq_along(fits), function(i) {
    data <- fits[[i]]$table
    data$Model <- trimws(labels[i])
    data
  }))
  d$x <- .plot_level(d$Year)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$Mean,
    colour = .data$Model, fill = .data$Model, group = .data$Model))
  if (isTRUE(show_probs) && any(is.finite(d$Qlower) & is.finite(d$Qupper))) {
    p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$Qlower, ymax = .data$Qupper),
      alpha = 0.18, colour = NA, na.rm = TRUE)
  }
  ylabel <- if (reference$method == "year_effect") {
    paste("Year-effect index (", reference$scale, ")", sep = "")
  } else if (reference$method == "integrated") {
    if (reference$scale == "ratio") "Relative area-integrated index" else {
      paste0("Area-integrated index", if (!is.null(reference$units)) paste0(" (", reference$units, ")"))
    }
  } else if (reference$scale == "ratio") "Relative standardised CPUE" else {
    paste0("Standardised CPUE", if (!is.null(reference$units)) paste0(" (", reference$units, ")"))
  }
  p <- p + ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::labs(x = reference$year, y = ylabel, colour = NULL, fill = NULL)
  if (all(d$Mean >= 0) && reference$scale %in% c("response", "ratio")) {
    p <- p + ggplot2::scale_y_continuous(limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.05)))
  }
  if (length(fits) == 1L) p <- p + ggplot2::guides(colour = "none", fill = "none")
  p
}
