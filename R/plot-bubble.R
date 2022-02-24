#' Data extent plot
#' 
#' Generates a bubble plot for a data set, generally used for plotting database tables.
#' 
#' @param data A \code{data.frame} containing at least two columns.
#' @param xvar The name of the temporal column (e.g., year).
#' @param yvar The names of the columns in the \code{data.frame} to plot.
#' @param ... Further arguments passed to nothing.
#' @return a ggplot object
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
#' @import ggplot2
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @export
#' 
plot_data_extent <- function(data, xvar, yvar) {
  
  df <- data %>%
    select(xvar, all_of(yvar)) %>%
    mutate(across(yvar, ~ ifelse(is.na(.x), 0, 1))) %>%
    group_by_at(all_of(xvar)) %>%
    summarise(across(yvar, ~ ifelse(sum(.x) == 0, 0, sum(.x) / n()))) %>%
    mutate(across(yvar, ~ ifelse(.x == 0, NA, .x))) %>%
    pivot_longer(cols = yvar)
  
  df$name <- factor(df$name, levels = rev(yvar))
  
  df1 <- df %>%
    mutate(value = 1)
  
  p <- ggplot(data = df) +
    geom_point(data = df1, aes(x = .data$name, y = !!sym(xvar), size = .data$value), colour = "red") +
    geom_point(aes(x = .data$name, y = !!sym(xvar), size = .data$value)) +
    scale_size(limits = c(1e-6, 1)) +
    coord_flip() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(size = "Proportion")
  
  return(p)
}


#' Bubble plot
#' 
#' Generates a bubble plot for a data set.
#' 
#' @param df A \code{data.frame} containing at least two columns.
#' @param group The names of the columns in the \code{data.frame} to plot.
#' @param sort_order A character vector containing the order of varables.
#' @param sum_by Sum to 1 by row, sum to 1 by column, sum to 1 across all data, or raw. The size of the bubbles will be the same for all and raw, but the legend will change from numbers of records to a proportion.
#' @param fill the colour to use in the plot, can either be a colour or a factor to colour by.
#' @param alpha the alpha level of the bubbles.
#' @param xlab the x axis label.
#' @param ylab the y axis label.
#' @param zlab the z axis label.
#' @param ... Further arguments passed to nothing.
#' @return a ggplot object
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
#' @import ggplot2
#' @import dplyr
#' @export
#' 
plot_bubble <- function(df, 
                        group = c("fishing_year", "vessel"), 
                        sort_order = NULL,
                        sum_by = "raw", 
                        fill = "purple", alpha = 0.5, 
                        ylab = NA, xlab = NA, zlab = "N", ...) {
  
  if (!is.data.frame(df)) stop("df is not an object of data.frame.")

  df <- df %>%
    mutate_at(vars(matches(group[2])), factor)

  if (fill %in% names(df)) {
    group <- c(group, fill)
  }
  
  if (sum_by %in% c("row", "rows", "y")) {
    df0 <- df %>%
      group_by_at(all_of(group)) %>%
      summarise(n = n())
    df1 <- df0 %>%
      group_by_at(all_of(group[1])) %>%
      summarise(nsum = sum(.data$n)) %>%
      right_join(df0, by = group[1]) %>%
      mutate(size = .data$n / .data$nsum) %>%
      mutate(size = ifelse(.data$size == 0, NA, .data$size))
  } else if (sum_by %in% c("col", "cols", "column", "columns", "x")) {
    df0 <- df %>%
      group_by(.dots = group) %>%
      summarise(n = n())
    df1 <- df0 %>%
      group_by_at(all_of(group[2])) %>%
      summarise(nsum = sum(.data$n)) %>%
      right_join(df0, by = group[2]) %>%
      mutate(size = .data$n / .data$nsum) %>%
      mutate(size = ifelse(.data$size == 0, NA, .data$size)) %>%
      ungroup()
  } else if (sum_by %in% c("raw", "all")) {
    df1 <- df %>%
      group_by_at(all_of(group)) %>%
      summarise(size = n()) %>%
      mutate(size = ifelse(.data$size == 0, NA, .data$size)) %>%
      ungroup()
  }

  if (sum_by == "all") {
    df1$size <- df1$size / sum(df1$size)
  }
  
  df1 <- df1 %>% 
    mutate(variable = data.frame(df1)[,group[2]])

  if (!is.null(sort_order)) {
    df1$variable <- factor(df1$variable, levels = sort_order)
  }
  
  if (fill %in% names(df)) {
    p <- ggplot(data = df1, aes_string(x = group[2], y = group[1], fill = fill, colour = fill)) +
      geom_point(aes(size = .data$size), alpha = alpha, shape = 16) +
      geom_point(aes(size = .data$size), shape = 1)
  } else {
    p <- ggplot(data = df1, aes_string(x = group[2], y = group[1])) +
      geom_point(aes(size = .data$size), alpha = alpha, shape = 16, colour = fill) +
      geom_point(aes(size = .data$size), shape = 1, colour = fill)
  }
  
  if (is.na(xlab)) xlab <- group[2]
  if (is.na(ylab)) ylab <- group[1]
  
  p <- p + 
    labs(x = xlab, y = ylab, size = zlab) +
    scale_size(range = c(0, 10)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(p)
}
