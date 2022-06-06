#' Compare indices
#' 
#' @param fits a list of objects of class \code{brmsfit} in the order that you want to compare them.
#' @param labels the labels for the fits.
#' @param year the year or time label.
#' @param probs the quantiles to plot.
#' @param show_probs plot the quantiles or not.
#' @param rescale the index of the series to rescale to.
#' 
#' @import brms
#' @import ggplot2
#' @importFrom stringi stri_trim_right
#' @export
#' 
plot_compare <- function(fits, labels = NULL, year = "year", probs = c(0.25, 0.75), show_probs = TRUE, rescale = NULL) {
  
  df0 <- list()
  df <- NULL
  
  # Extract the indices for each model fit
  for (i in 1:length(fits)) {
    # yrs <- sort(unique(fits[[i]]$data[,year]))
    # n <- length(yrs)
    # # Create newdata for prediction (using fitted)
    # newdata <- fits[[i]]$data %>% slice(rep(1, n))
    # for (j in 1:ncol(newdata)) {
    #   x <- fits[[i]]$data[,j]
    #   newdata[,j] <- ifelse(is.numeric(x), mean(x), NA)
    # }
    # newdata[,year] <- yrs
    # fout <- data.frame(fitted(object = fits[[i]], newdata = newdata, probs = c(probs[1], 0.5, probs[2])))
    fout <- get_index(fit = fits[[i]], year = year, probs = probs, rescale = 1)
    
    if (is.null(labels)) {
      str <- as.character(fits[[i]]$formula)[1]
      left1 <- stri_trim_right(str = str, pattern = "[\u007E]", negate = FALSE)
      fout$model <- substr(str, nchar(left1) + 2, nchar(str))
    } else {
      fout$model <- labels[i]
    }
    # fout$year <- yrs
    # fout$Qlower <- fout$Qlower / geo_mean(fout$Median)
    # fout$Qupper <- fout$Qupper / geo_mean(fout$Median)
    # fout$Median <- fout$Median / geo_mean(fout$Median)
    df0[[i]] <- fout
  }
  
  # If one model fit is shorter or longer (e.g., fewer years) then can rescale to one or the other
  if (!is.null(rescale)) {
    for (i in 1:length(fits)) {
      if (i != rescale) {
        fout <- df0[[i]]
        
        df1 <- df0[[rescale]] %>%
          filter(.data$Year %in% fout$Year)
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
  for (i in 1:length(fits)) {
    df <- rbind(df, df0[[i]])
  }
  
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
