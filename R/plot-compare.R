#' Compare CPUE indices
#' 
#' @param fits a list of objects of class \code{brmsfit} in the order that you want to compare them.
#' @param labels some optional labels for the fits that will be shown in the legend instead of the model formula.
#' @param year the year or time label in the fitted model (e.g., year).
#' @param probs the quantiles to plot. Defaults to 25\% and 75\% quantiles or 50\% credible interval (CI).
#' @param show_probs plot the quantiles as ribbons or not.
#' @param rescale How to re-scale the series. Choose from "raw" to retain the raw series, "unstandardised" to re-scale to the geometric mean of the unstandardised series, or a number to re-scale by. 
#' @param rescale_series the index of the series to rescale to if one series is longer/shorter than the others.
#' @return a \code{ggplot} object.
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
#' @import brms
#' @import ggplot2
#' @importFrom stringi stri_trim_right
#' @export
#' 
plot_compare <- function(fits, labels = NULL, year = "year", 
                         probs = c(0.25, 0.75), show_probs = TRUE, 
                         rescale = "raw", rescale_series = NULL) {
  
  # Extract the indices for each model fit as a list
  df0 <- list()
  for (i in 1:length(fits)) {
    fout <- get_index(fit = fits[[i]], year = year, probs = probs, rescale = rescale)
    if (is.null(labels)) {
      str <- as.character(fits[[i]]$formula)[1]
      left1 <- stri_trim_right(str = str, pattern = "[\u007E]", negate = FALSE)
      fout$model <- substr(str, nchar(left1) + 2, nchar(str))
    } else {
      fout$model <- labels[i]
    }
    df0[[i]] <- fout
  }
  
  # If one model series is shorter or longer (e.g., fewer years) then can rescale to one series or the other
  if (!is.null(rescale_series)) {
    for (i in 1:length(fits)) {
      if (i != rescale_series) {
        fout <- df0[[i]]
        df1 <- df0[[rescale_series]] %>% filter(.data$Year %in% fout$Year)
        gm <- geo_mean(df1$Mean)
        fout$Mean <- fout$Mean / geo_mean(fout$Mean) * gm
        fout$Qlower <- fout$Qlower / geo_mean(fout$Median) * gm
        fout$Qupper <- fout$Qupper / geo_mean(fout$Median) * gm
        fout$Median <- fout$Median / geo_mean(fout$Median) * gm
        df0[[i]] <- fout
      }
    }
  }

  # Change from list to data.frame
  df <- bind_rows(df0)

  # Generate the plot
  p <- ggplot(data = df)
  
  if (show_probs) {
    p <- p + geom_ribbon(data = df, aes(x = .data$Year, ymin = .data$Qlower, ymax = .data$Qupper, group = .data$model, fill = .data$model), alpha = 0.3, colour = NA)
  }
  
  p <- p + 
    geom_line(data = df, aes(x = .data$Year, y = .data$Median, colour = .data$model, group = .data$model)) +
    labs(x = NULL, y = "Index") +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.y = unit(0, "lines"))
  
  return(p)
}
