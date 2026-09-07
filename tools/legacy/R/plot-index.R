#' Plot the standardised and unstandardised indices
#' 
#' In this plot the unstandardised indices is the geometric mean of the data.
#' 
#' @param fit An object of class \code{brmsfit}.
#' @param year the year or time label.
#' @param fill the fill colour for the percentiles.
#' @param probs The percentiles to be computed by the \code{quantile} function.
#' @param rescale the index of the series to rescale to. If set to NULL then no rescaling is done.
#' @param show_unstandardised show the unstandardised series or not.
#' @return a \code{ggplot} object.
#' @importFrom stats fitted
#' @import brms
#' @import ggplot2
#' @import dplyr
#' @export
#' 
plot_index <- function(fit, 
                       year = NULL, 
                       fill = "purple", 
                       probs = c(0.25, 0.75),
                       rescale = 1,
                       show_unstandardised = TRUE) {
  
  if (!is.brmsfit(fit)) stop("fit is not an object of class brmsfit.")
  
  if (is.null(year)) {
    year <- get_first_term(fit = fit)
  }
  
  # Get the standardised series
  fout <- get_index(fit = fit, year = year, probs = probs, rescale = rescale) %>%
    mutate(model = "Standardised")
  
  # Get the unstandardised series
  unstd <- get_unstandarsied(fit = fit, year = year, rescale = rescale) %>%
    mutate(model = "Unstandardised")
  
  df <- bind_rows(fout, unstd)
  
  df$model <- factor(df$model, levels = c("Unstandardised", "Standardised"))
  
  if (!show_unstandardised) {
    df <- df %>% filter(.data$model != "Unstandardised")
    scale_col <- fill
    scale_lin <- "solid"
  } else {
    scale_col <- c("grey", fill)
    scale_lin <- c("dashed", "solid")
  }
  
  p <- ggplot(data = df, aes(x = .data$Year, y = .data$Median, group = .data$model)) +
    # geom_ribbon(aes(ymin = .data$Qlower, ymax = .data$Qupper, fill = .data$model), alpha = 0.5, colour = NA) +
    geom_ribbon(data = df %>% filter(.data$model != "Unstandardised"), aes(ymin = .data$Qlower, ymax = .data$Qupper), alpha = 0.5, colour = NA, fill = fill) +
    geom_line(aes(colour = .data$model, linetype = .data$model)) +
    geom_point(aes(colour = .data$model)) +
    labs(x = NULL, y = "Index") +
    # scale_fill_manual(values = c("grey", fill)) + scale_fill_manual(values = fill) +
    scale_colour_manual(values = scale_col) +
    scale_linetype_manual(values = scale_lin) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    theme_bw() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank(), legend.key.width = unit(2, "cm")) +
    guides(color = guide_legend(override.aes = list(fill = NA)))

  return(p)
}
