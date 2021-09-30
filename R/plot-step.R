#' A Bayesian version of the step-plot
#' 
#' This requires that all steps be run using brms and then provided as a list of model fits.
#' 
#' @param fits a list of model fits in the order that you want to compare them
#' @param year the year or time label
#' @param fill the colour of the credible interval ribbon
#' @param probs the quantiles to plot
#' @param show_probs plot the quantiles or not
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
#' @import brms
#' @import ggplot2
#' @import dplyr
#' @export
#' 
plot_step <- function(fits, year = "year", fill = "purple",
                      probs = c(0.25, 0.75), show_probs = TRUE) {
  
  m <- length(fits)
  
  fout <- list()
  for (i in 1:m) {
    fout[[i]] <- get_index(fit = fits[[i]], year = year, probs = probs)
  }
  
  df <- NULL
  df_dash <- NULL
  df_grey <- NULL
  for (i in 1:m) {
    df <- rbind(df, fout[[i]])
    if (i > 1) {
      xx <- fout[[i - 1]] %>% mutate(Model = fout[[i]]$Model, line = i)
      df_dash <- rbind(df_dash, xx)
    }
    if (i > 2) {
      xx <- fout[[i - 2]] %>% mutate(Model = fout[[i]]$Model, line = i)
      df_grey <- rbind(df_grey, xx) # bug - these needs to include all prevous models in grey (i.e. if 4 models are provided)
    }
  }
  
  p <- ggplot(data = df) +
    geom_line(data = df_grey, aes(x = .data$Year, y = .data$Median, group = .data$Model), colour = "grey", linetype = "solid") +
    geom_line(data = df_dash, aes(x = .data$Year, y = .data$Median, group = 1), colour = "black", linetype = "dashed")
  
  if (show_probs) {
    p <- p + geom_ribbon(data = df, aes(x = .data$Year, ymin = .data$Qlower, ymax = .data$Qupper, group = 1), alpha = 0.3, colour = NA, fill = fill)
  }
  
  p <- p + 
    geom_line(data = df, aes(x = .data$Year, y = .data$Median, group = 1)) +
    geom_point(data = df, aes(x = .data$Year, y = .data$Median)) +
    facet_wrap(Model ~ ., ncol = 1, strip.position = "top") +
    labs(x = NULL, y = "Index") +
    scale_fill_manual(values = c(NA, "black")) +
    scale_colour_manual(values = c("black", "black")) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_shape_manual(values = c(NA, 19)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    theme_bw() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.y = unit(0, "lines"))
  
  return(p)
}
