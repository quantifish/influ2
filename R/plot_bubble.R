#' A bubble plot
#' 
#' @param df a data frame
#' @param group the groups to plot
#' @param fill the colour to use in the plot
#' @param alpha the alpha level of the bubbles
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param zlab the z axis label
#' @return a ggplot object
#' @import ggplot2
#' @import dplyr
#' @export
#' 
plot_bubble <- function(df, group = c("fishing_year", "vessel"), fill = "purple", alpha = 0.5, ylab = NULL, xlab = NULL, zlab = "N") {

  df <- df %>%
    mutate_at(vars(matches(group[2])), factor)

  if (fill %in% names(df)) {
    group <- c(group, fill)
    df1 <- df %>%
      group_by(.dots = group) %>%
      summarise(size = n()) %>%
      mutate(size = ifelse(size == 0, NA, size))

    p <- ggplot(data = df1, aes_string(x = factor(group[2]), y = group[1], fill = fill, colour = fill)) +
      geom_point(aes(size = size), alpha = alpha, shape = 16) +
      geom_point(aes(size = size), shape = 1)
  } else {
    df1 <- df %>%
      group_by(.dots = group) %>%
      summarise(size = n()) %>%
      mutate(size = ifelse(size == 0, NA, size))

    p <- ggplot(data = df1, aes_string(x = as.character(group[2]), y = group[1])) +
      geom_point(aes(size = size), alpha = alpha, shape = 16, colour = fill) +
      geom_point(aes(size = size), shape = 1, colour = fill)
  }

  p <- p + 
    labs(x = xlab, y = ylab, size = zlab) +
    theme_bw() +
    scale_size(range = c(0, 10)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(p)
}
