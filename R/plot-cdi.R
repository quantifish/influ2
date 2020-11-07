#' Bayesian version of the CDI plot
#' 
#' The CDI plot presents the coefficients for the variable of interest (top-left panel), the spread of the data 
#' (bottom-left panel), and the influence statistic (bottom-right panel).
#' 
#' @param fit An object of class \code{brmsfit}.
#' @param xfocus The column name of the variable to be plotted on the x axis. This column name must match one of the
#'   column names in the \code{data.frame} that was passed to \code{brm} as the \code{data} argument.
#' @param yfocus The column name of the variable to be plotted on the y axis. This column name must match one of the
#'   column names in the \code{data.frame} that was passed to \code{brm} as the \code{data} argument. This is generally the
#'   temporal variable in a generalised linear model (e.g. year).
#' @param hurdle If a hurdle model then use the hurdle.
#' @param xlab The x axis label.
#' @param ylab The y axis label.
#' @param colour The colour to use in the plot.
#' @param p_margin The margin between panels on the plot. This is passed to \code{margin} within \code{theme}.
#' @param legend To show the legend or not.
#' @param sum_by Sum to 1 by row, sum to 1 by column, sum to 1 across all data, or raw. The size of the bubbles will be 
#'   the same for all and raw, but the legend will change from numbers of records to a proportion.
#' @param ... Further arguments passed to nothing.
#' @return a \code{ggplot} object.
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
#' @seealso \code{\link{get_coefs}}, \code{\link{get_influ}}, \code{\link{plot_bubble}}
#' 
#' @importFrom gtable is.gtable gtable_filter
#' @importFrom stats poly
#' @importFrom tidyselect all_of
#' @import ggplot2
#' @import dplyr
#' @import patchwork
#' @export
#' 
plot_bayesian_cdi <- function(fit, xfocus = "area", yfocus = "fishing_year",
                              xlab = "Month",  ylab = "Fishing year", hurdle = FALSE,
                              colour = "purple", p_margin = 0.05, legend = TRUE, sum_by = "row", ...) {
  
  if (!is.brmsfit(fit)) stop("fit is not an object of class brmsfit.")
  
  # Identify the type of variable we are dealing with
  type <- id_var_type(fit = fit, xfocus = xfocus, hurdle = hurdle)
  
  # Posterior samples of coefficients
  if (type %in% c("fixed_effect", "random_effect")) {
    coefs <- get_coefs(fit = fit, var = xfocus, hurdle = hurdle)
  } else {
    coefs <- get_marginal(fit = fit, var = xfocus) # this would plot the marginal/conditional effect, but if it is a hurdle model it ignores the hurdle bit
    # coefs <- get_coefs_raw(fit = fit, var = xfocus)
  }
  
  # Model data
  if (is.numeric(coefs$variable)) {
    data <- fit$data %>%
      select(all_of(c(yfocus, xfocus)))
    length.out <- 15
    dmin <- min(data[,xfocus])
    dmax <- max(data[,xfocus])
    breaks <- seq(dmin, dmax, length.out = length.out)
    midpoints <- breaks[-length(breaks)] + diff(breaks) / 2
    data[,xfocus] <- cut(data[,xfocus], breaks = breaks, labels = sprintf("%.2f", round(midpoints, 2)), include.lowest = TRUE)
  } else {
    data <- fit$data
  }
  
  # Influence
  influ <- get_influ2(fit = fit, group = c(yfocus, xfocus), hurdle = hurdle)
  
  # Extract the legend on its own
  g2 <- function(a.gplot) {
    if (!is.gtable(a.gplot))
      a.gplot <- ggplotGrob(a.gplot)
    gtable_filter(a.gplot, 'guide-box', fixed = TRUE)
  }
  
  # The bubble plot (bottom-left) and the legend for the bubble plot (top-right)
  p3a <- plot_bubble(df = data, group = c(yfocus, xfocus), sum_by = sum_by, xlab = xlab, ylab = ylab, zlab = "", fill = colour)
  p2 <- g2(p3a)
  p3 <- p3a + theme(legend.position = "none", plot.margin = margin(t = p_margin, r = p_margin, unit = "cm"), 
                    axis.text.x = element_text(angle = 45, hjust = 1))
  
  # The coefficients (top-left)
  p1 <- ggplot(data = coefs, aes(x = .data$variable, y = .data$value)) +
    labs(x = NULL, y = "Conditional effect") +
    theme_bw() +
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          plot.margin = margin(b = p_margin, r = p_margin, unit = "cm"))
  if (is.numeric(coefs$variable)) {
    p3 <- p3 + scale_x_discrete(expand = expansion(mult = 0.05))
    p1 <- p1 +
      stat_summary(geom = "ribbon", alpha = 0.5, fill = colour, 
                   fun.min = function(x) quantile(x, probs = 0.025), 
                   fun.max = function(x) quantile(x, probs = 0.975)) +
      stat_summary(fun = "median", geom = "line", colour = colour) +
      scale_x_continuous(position = "top", breaks = midpoints, minor_breaks = NULL, expand = expansion(mult = 0.05)) +
      coord_cartesian(xlim = c(midpoints[1], midpoints[length(midpoints)]))
  } else {
    p1 <- p1 +
      geom_violin(colour = colour, fill = colour, alpha = 0.5, draw_quantiles = 0.5, scale = "width") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_x_discrete(position = "top")# +
      # scale_x_discrete(position = "top", breaks = midpoints, minor_breaks = NULL, expand = expansion(mult = 0.05)) +
      # coord_cartesian(xlim = c(midpoints[1], midpoints[length(midpoints)]))
  }
  
  # The influence plot (bottom-right)
  p4 <- ggplot(data = influ, aes_string(x = as.character(yfocus))) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_violin(aes(y = exp(.data$delta)), colour = colour, fill = colour, alpha = 0.5, draw_quantiles = 0.5, scale = "width") +
    # geom_violin(aes(y = .data$delta), colour = colour, fill = colour, alpha = 0.5, draw_quantiles = 0.5, scale = "width") +
    coord_flip() +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = "Influence") +
    theme_bw() +
    theme(legend.position = "none", plot.margin = margin(t = p_margin, l = p_margin, unit = "cm"))
  
  if (legend) {
    p <- p1 + p2 + p3 + p4 + plot_layout(nrow = 2, ncol = 2, heights = c(1, 2), widths = c(2, 1))
  } else {
    pv <- ggplot() + theme_void()
    p <- p1 + pv + p3 + p4 + plot_layout(nrow = 2, ncol = 2, heights = c(1, 2), widths = c(2, 1))
  }
  
  return(p)
}


#' Bayesian version of the CDI plot (depreciated)
#' 
#' @param fit a model fit
#' @param xfocus The column name of the variable to be plotted on the x axis. This column name must match one of the
#'   column names in the \code{data.frame} that was passed to \code{brm} as the \code{data} argument.
#' @param yfocus The column name of the variable to be plotted on the y axis. This column name must match one of the
#'   column names in the \code{data.frame} that was passed to \code{brm} as the \code{data} argument. This is generally the
#'   temporal variable in a generalised linear model (e.g. year).
#' @param hurdle if a hurdle model then use the hurdle
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param colour the colour to use in the plot
#' @return a ggplot object
#' 
#' @importFrom gtable is.gtable gtable_filter
#' @importFrom stats poly
#' @import ggplot2
#' @import dplyr
#' @import patchwork
#' @export
#' 
plot_bayesian_cdi2 <- function(fit,
                               xfocus = "area", yfocus = "fishing_year",
                              hurdle = FALSE,
                              xlab = "Month", 
                              ylab = "Fishing year", 
                              colour = "purple") {

  # Posterior samples of coefficients
  coefs <- get_coefs(fit = fit, var = xfocus, normalise = TRUE, hurdle = hurdle)
  n_iterations <- max(coefs$iteration)
  
  get_midpoint <- function(cut_label) {
    mean(as.numeric(unlist(strsplit(gsub("\\(|\\)|\\[|\\]", "", as.character(cut_label)), ","))))
  }
  
  # Model data
  is_poly <- FALSE
  if (any(grepl("poly", coefs$variable))) {
    is_poly <- TRUE
    data <- fit$data %>%
      select(-starts_with("poly"))
    dmin <- min(data[,xfocus])
    dmax <- max(data[,xfocus])
    data[,xfocus] <- cut(data[,xfocus], breaks = seq(dmin, dmax, length.out = 20), include.lowest = TRUE)
    # breaks <- unique(quantile(data[,xfocus], probs = seq(0, 1, length.out = 15)))
    # data[,xfocus] <- cut(data[,xfocus], breaks = breaks, include.lowest = TRUE)
    data[,xfocus] <- sapply(data[,xfocus], get_midpoint)

    z <- poly(fit$data[,xfocus], 3)
    x_new <- data.frame(id = 1:length(unique(data[,xfocus])), variable = sort(unique(data[,xfocus])))
    x_poly <- poly(x_new$variable, 3, coefs = attr(z, "coefs"))

    # Do the matrix multiplication
    Xbeta <- matrix(NA, nrow = n_iterations, ncol = nrow(x_poly))
    for (i in 1:n_iterations) {
      Xbeta[i,] <- x_poly %*% filter(coefs, .data$iteration == i)$value
    }
    coefs <- melt(Xbeta, varnames = c("iteration", "id")) %>%
      left_join(x_new, by = "id") %>%
      select(-id)
  } else if (length(unique(coefs$variable)) == 1) {
    data <- fit$data# %>%
      # select(xfocus)
    dmin <- min(data[,xfocus])
    dmax <- max(data[,xfocus])
    data[,xfocus] <- cut(data[,xfocus], breaks = seq(dmin, dmax, length.out = 20), include.lowest = TRUE)
    data[,xfocus] <- sapply(data[,xfocus], get_midpoint)
    
    x_new <- data.frame(id = 1:length(unique(data[,xfocus])), variable = sort(unique(data[,xfocus])))
    Xbeta <- matrix(NA, nrow = n_iterations, ncol = nrow(x_new))
    for (i in 1:n_iterations) {
      Xbeta[i,] <- as.matrix(x_new$variable) %*% filter(coefs, .data$iteration == i)$value
    }
    coefs <- melt(Xbeta, varnames = c("iteration", "id")) %>%
      left_join(x_new, by = "id") %>%
      select(-id)    
  } else {
    data <- fit$data %>%
      mutate_at(vars(matches(xfocus)), factor)
  }
  
  # Influence
  influ <- get_influ(fit = fit, group = c(yfocus, xfocus), hurdle = hurdle)
  
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
  p1 <- ggplot(data = coefs, aes(x = factor(.data$variable), y = exp(.data$value))) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_violin(colour = colour, fill = colour, alpha = 0.5, draw_quantiles = 0.5, scale = "width") +
    labs(x = NULL, y = ylab1) +
    scale_x_discrete(position = "top") +
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), plot.margin = margin(b = sp, r = sp, unit = "cm"))
  
  # The bubble plot (bottom-left) and the legend for the bubble plot (top-right)
  p3a <- plot_bubble(df = data, group = c(yfocus, xfocus), sum_by = "row", xlab = xlab, ylab = ylab, zlab = "", fill = colour)
  p2 <- g2(p3a)
  p3 <- p3a +
    theme(legend.position = "none", plot.margin = margin(t = sp, r = sp, unit = "cm"), axis.text.x = element_text(angle = 45, hjust = 1))

  # The influence plot (bottom-right)
  p4 <- ggplot(data = influ, aes_string(x = as.character(yfocus))) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    # geom_violin(aes(y = .data$delta), colour = colour, fill = colour, alpha = 0.5, draw_quantiles = 0.5, scale = "width") +
    geom_violin(aes(y = exp(.data$delta)), colour = colour, fill = colour, alpha = 0.5, draw_quantiles = 0.5, scale = "width") +
    coord_flip() +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = "Influence") +
    theme_bw() +
    theme(legend.position = "none", plot.margin = margin(t = sp, l = sp, unit = "cm"))
  
  p1 + p2 + p3 + p4 + plot_layout(nrow = 2, ncol = 2, heights = c(1, 2), widths = c(2, 1))
}
