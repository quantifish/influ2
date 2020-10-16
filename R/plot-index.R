#' Plot the standardised and unstandardised indices
#' 
#' In this plot the unstandardised indices is the geometric mean of the data.
#' 
#' @param fit An object of class \code{brmsfit}.
#' @param year the year or time label.
#' @param fill the fill colour for the percentiles.
#' @param probs The percentiles to be computed by the \code{quantile} function.
#' @return a \code{ggplot} object.
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
#' @importFrom stats fitted
#' @import brms
#' @import ggplot2
#' @import dplyr
#' @export
#' 
plot_index <- function(fit, year = "Year", fill = "purple", probs = c(0.25, 0.75)) {
  
  if (!is.brmsfit(fit)) stop("fit is not an object of class brmsfit.")

  fout <- get_index(fit = fit, year = year, probs = probs) %>%
    mutate(model = "Standardised")
  
  if (fit$family$family == "bernoulli" | grepl("hurdle", fit$family$family)) {
    prop <- data.frame(y = fit$data[,1], year = fit$data[,year]) %>%
      mutate(y = ifelse(.data$y > 0, 1, 0)) %>%
      group_by(year) %>%
      summarise(p = sum(.data$y) / n())
    unstd <- data.frame(y = fit$data[,1], year = fit$data[,year]) %>%
      filter(.data$y > 0) %>%
      group_by(year) %>%
      summarise(cpue = exp(mean(log(.data$y)))) %>%
      left_join(prop, by = "year") %>%
      mutate(cpue = .data$cpue * .data$p)
  } else {
    unstd <- data.frame(y = fit$data[,1], year = fit$data[,year]) %>%
      group_by(year) %>%
      summarise(cpue = exp(mean(log(.data$y))))
  }
  
  df1 <- fout %>%
    mutate(Estimate = NA, Est.Error = NA, Qlower = NA, Q50 = unstd$cpue / geo_mean(unstd$cpue), Qupper = NA, model = "Unstandardised")
  
  df <- rbind(fout, df1)
  df$model <- factor(df$model, levels = c("Unstandardised", "Standardised"))
  
  p <- ggplot(data = df, aes(x = .data$Year, y = .data$Q50, group = .data$model)) +
    geom_ribbon(aes(ymin = .data$Qlower, ymax = .data$Qupper, fill = .data$model), alpha = 0.5, colour = NA) +
    geom_line(aes(colour = .data$model, linetype = .data$model)) +
    geom_point(aes(colour = .data$model)) +
    labs(x = NULL, y = "Index") +
    scale_colour_manual(values = c("grey", fill)) +
    scale_fill_manual(values = c("grey", fill)) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    theme_bw() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank(), legend.key.width = unit(2, "cm")) +
    guides(color = guide_legend(override.aes = list(fill = NA)))
  
  return(p)
}
