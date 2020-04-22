#' Bayesian version of the CDI plot
#' 
#' @param fit a model fit
#' @param group the groups to plot
#' @param hurdle if a hurdle model then use the hurdle
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param colour the colour to use in the plot
#' @return a ggplot object
#' @importFrom gtable is.gtable gtable_filter
#' @import ggplot2
#' @import dplyr
#' @import patchwork
#' @export
#' 
plot_bayesian_cdi <- function(fit,
                              group = c("fishing_year", "area"),
                              hurdle = FALSE,
                              xlab = "Month", 
                              ylab = "Fishing year", 
                              colour = "purple") {

  # Model data
  data <- fit$data %>%
    mutate_at(vars(matches(group[2])), factor)

  # Posterior samples of coefficients
  coefs <- get_coefs(fit = fit, var = group[2], normalise = TRUE, hurdle = hurdle)
  
  # Transform the coefficients using the link function - this might be better placed in get-coefs.R
  if (fit$family$family == "lognormal") {
    if (fit$family$link == "identity") {
      coefs <- coefs %>% mutate(value = exp(.data$value))
    } else {
      stop("This link function for the lognormal family has not been coded in influ2 yet - please update the plot-cdi.R function.")
    }
  } else if (fit$family$family == "Gamma") {
    if (fit$family$link == "inverse") {
      coefs <- coefs %>% mutate(value = 1.0 / .data$value)
    } else if (fit$family$link == "identity") {
      coefs <- coefs %>% mutate(value = .data$value)
    } else if (fit$family$link == "log") {
      coefs <- coefs %>% mutate(value = exp(.data$value))
    } else {
      stop("This link function for the Gamma family has not been coded in influ2 yet - please update the plot-cdi.R function.")
    }
  } else {
    stop("This family has not been coded in influ2 yet - please update the plot-cdi.R function.")
  }

  # Influence
  influ <- get_influ(fit = fit, group = group, hurdle = hurdle)
  
  if (nrow(fit$ranef) > 0) {
    ylab1 <- "Coefficient"
  } else {
    ylab1 <- "Relative coefficient"
  }

  # Extract the legend on its own
  g2 <- function(a.gplot) {
    if (!is.gtable(a.gplot))
      a.gplot <- ggplotGrob(a.gplot)
    gtable_filter(a.gplot, 'guide-box', fixed = TRUE)
  }

  # Build the plot
  sp <- 0.05
  
  # The coefficients (top-left)
  p1 <- ggplot(data = coefs, aes(x = factor(.data$variable), y = .data$value)) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_violin(colour = colour, fill = colour, alpha = 0.5, draw_quantiles = 0.5, scale = "width") +
    labs(x = NULL, y = ylab1) +
    scale_x_discrete(position = "top") +
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), plot.margin = margin(b = sp, r = sp, unit = "cm"))
  
  # The bubble plot (bottom-left) and the legend for the bubble plot (top-right)
  p3a <- plot_bubble(df = data, group = group, xlab = xlab, ylab = ylab, zlab = "", fill = colour)
  p2 <- g2(p3a)
  p3 <- p3a +
    theme(legend.position = "none", plot.margin = margin(t = sp, r = sp, unit = "cm"), axis.text.x = element_text(angle = 45, hjust = 1))

  # The influence plot (bottom-right)
  p4 <- ggplot(data = influ, aes_string(x = as.character(group[1]))) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_violin(aes(y = exp(.data$delta)), colour = colour, fill = colour, alpha = 0.5, draw_quantiles = 0.5, scale = "width") +
    coord_flip() +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = "Influence") +
    theme_bw() +
    theme(legend.position = "none", plot.margin = margin(t = sp, l = sp, unit = "cm"))
  
  p1 + p2 + p3 + p4 + plot_layout(nrow = 2, ncol = 2, heights = c(1, 2), widths = c(2, 1))
}
