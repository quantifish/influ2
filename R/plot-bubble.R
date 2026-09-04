#' Bubble plot of sampling composition
#'
#' Summarises the number or proportion of records in each combination of two
#' grouping variables and displays the result as a bubble plot.
#'
#' @param df A data frame.
#' @param group Character vector naming the vertical and horizontal grouping
#'   variables, in that order.
#' @param sort_order Optional ordering for the horizontal grouping variable.
#' @param sum_by One of `"raw"`, `"all"`, `"row"`, or `"column"`.
#' @param fill A fixed colour, or the name of a column in `df` used to colour
#'   bubbles.
#' @param alpha Bubble transparency.
#' @param xlab,ylab,zlab Axis and size-legend labels.
#' @param ... Reserved for future plotting options.
#'
#' @return A [ggplot2::ggplot()] object.
#' @examples
#' data(lobsters_per_pot)
#' plot_bubble(
#'   lobsters_per_pot,
#'   group = c("year", "month"),
#'   fill = "purple4"
#' )
#' @export
plot_bubble <- function(df, group = c("fishing_year", "vessel"),
                        sort_order = NULL, sum_by = "raw", fill = "purple",
                        alpha = 0.5, ylab = NULL, xlab = NULL, zlab = "N", ...) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }
  if (length(group) != 2L || !all(group %in% names(df))) {
    stop("`group` must name exactly two columns in `df`.", call. = FALSE)
  }

  aliases <- c(rows = "row", y = "row", col = "column", cols = "column",
               columns = "column", x = "column")
  if (sum_by %in% names(aliases)) sum_by <- unname(aliases[sum_by])
  sum_by <- match.arg(sum_by, c("raw", "all", "row", "column"))

  colour_by <- length(fill) == 1L && fill %in% names(df)
  grouping <- if (colour_by) unique(c(group, fill)) else group
  values <- df[grouping]
  values[[group[2]]] <- as.factor(values[[group[2]]])
  counts <- stats::aggregate(
    rep.int(1, nrow(values)),
    by = values,
    FUN = sum
  )
  names(counts)[ncol(counts)] <- "size"

  if (sum_by == "all") {
    counts$size <- counts$size / sum(counts$size)
  } else if (sum_by %in% c("row", "column")) {
    margin <- group[if (sum_by == "row") 1L else 2L]
    totals <- stats::ave(counts$size, counts[[margin]], FUN = sum)
    counts$size <- counts$size / totals
  }

  if (!is.null(sort_order)) {
    counts[[group[2]]] <- factor(counts[[group[2]]], levels = sort_order)
  }

  if (colour_by) {
    mapping <- ggplot2::aes(
      x = .data[[group[2]]],
      y = .data[[group[1]]],
      size = .data$size,
      colour = .data[[fill]],
      fill = .data[[fill]]
    )
  } else {
    mapping <- ggplot2::aes(
      x = .data[[group[2]]],
      y = .data[[group[1]]],
      size = .data$size
    )
  }

  plot <- ggplot2::ggplot(counts, mapping)
  if (colour_by) {
    plot <- plot +
      ggplot2::geom_point(alpha = alpha, shape = 16) +
      ggplot2::geom_point(shape = 1)
  } else {
    plot <- plot +
      ggplot2::geom_point(alpha = alpha, shape = 16, colour = fill) +
      ggplot2::geom_point(shape = 1, colour = fill)
  }

  plot +
    ggplot2::labs(
      x = xlab %||% group[2],
      y = ylab %||% group[1],
      size = zlab
    ) +
    ggplot2::scale_size(range = c(0, 10)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}
