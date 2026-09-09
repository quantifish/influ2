#' Plot calculated CPUE indices
#'
#' Display assessment indices without refitting or recalculating predictions.
#' @md
#'
#' @param x,object An [influ_index][cpue_index] object.
#' @param show_probs Show the stored pointwise uncertainty interval.
#' @param ... Reserved for future use; unused.
#' @return A ggplot object.
#' @seealso [cpue_index()], [plot_compare()]
#' @export
plot_index <- function(x, show_probs = TRUE, ...) {
  if (!inherits(x, "influ_index")) {
    stop("Calculate an `influ_index` with `cpue_index()` before plotting.", call. = FALSE)
  }
  .plot_cpue_indices(list(x), labels = "Index", show_probs = show_probs)
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
