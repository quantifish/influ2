# Select a constant stride, anchored at the first category. Do not append an
# off-stride endpoint: that would reintroduce irregular gaps or crowded labels.
.cdi_regular_indices <- function(position_mm, label_width_mm, gap_mm = 1.5) {
  n <- length(position_mm)
  if (n < 2L) return(seq_len(n))
  spacing <- max(label_width_mm) + gap_mm
  for (stride in seq_len(n)) {
    indices <- seq.int(1L, n, by = stride)
    if (all(diff(position_mm[indices]) >= spacing)) return(indices)
  }
}

# Two guides share the final text styles. Both are built before either is
# drawn, so each can allow for the larger of the upper/lower label widths.
.cdi_axis_guide <- function(angle, state) {
  ggplot2::ggproto(NULL, ggplot2::guide_axis(angle = angle),
    cdi_state = state,
    build_labels = function(self, key, elements, params) {
      grobs <- ggplot2::GuideAxis$build_labels(key, elements, params)
      for (i in seq_along(grobs)) {
        g <- grobs[[i]]
        if (!inherits(g, "titleGrob") || length(g$children) != 1L ||
            !inherits(g$children[[1L]], "text")) next
        text <- g$children[[1L]]
        self$cdi_state$templates[[params$position]] <- text
        g$cdi_text <- text
        g$cdi_state <- self$cdi_state
        g$cdi_position <- params$position
        class(g) <- c("influ_cdi_axis_labels", class(g))
        grobs[[i]] <- g
      }
      grobs
    }
  )
}

# Measurement must be deferred until grid has the actual axis viewport, not
# when plot() is called (which may precede ggsave() or a device resize).
#' @importFrom grid makeContent
#' @export
makeContent.influ_cdi_axis_labels <- function(x) {
  text <- x$cdi_text
  templates <- x$cdi_state$templates
  widths <- vapply(templates, function(g) {
    # grobWidth(textGrob(labels)) measures only the first label. Measure each
    # separately so a later, wider category is not inadvertently clipped.
    max(vapply(seq_along(g$label), function(i) {
      probe <- grid::textGrob(g$label[i], gp = g$gp, rot = g$rot)
      grid::convertWidth(grid::grobWidth(probe), "mm", valueOnly = TRUE)
    }, numeric(1)))
  }, numeric(1))
  positions <- grid::convertX(text$x, "mm", valueOnly = TRUE)
  indices <- .cdi_regular_indices(positions, widths)
  text$label[-indices] <- ""
  x$children[[1L]] <- text
  # Compact draw-time metadata also lets tests verify resize behaviour without
  # relying on device-specific raster pixels or changing the diagnostic data.
  x$cdi_state$drawn[[x$cdi_position]] <- list(
    indices = indices, labels = x$cdi_text$label[indices],
    positions_mm = positions, label_width_mm = max(widths)
  )
  x
}
