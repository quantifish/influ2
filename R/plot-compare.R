#' Compare indices
#' 
#' @param fits a list of objects of class \code{brmsfit} in the order that you want to compare them.
#' @param labels the labels for the fits.
#' @param year the year or time label.
#' @param probs the quantiles to plot.
#' @param show_probs plot the quantiles or not.
#' 
#' @import brms
#' @import ggplot2
#' @export
#' 
plot_compare <- function(fits, labels = NULL,
                         year = "year", probs = c(0.25, 0.75), show_probs = TRUE) {
  
  m <- length(fits)
  df <- NULL
  
  for (i in 1:m) {
    # yrs <- sort(unique(fits[[i]]$data[,year]))
    # n <- length(yrs)
    # 
    # # Create newdata for prediction (using fitted)
    # newdata <- fits[[i]]$data %>% slice(rep(1, n))
    # for (j in 1:ncol(newdata)) {
    #   x <- fits[[i]]$data[,j]
    #   newdata[,j] <- ifelse(is.numeric(x), mean(x), NA)
    # }
    # newdata[,year] <- yrs
    # 
    # fout <- data.frame(fitted(object = fits[[i]], newdata = newdata, probs = c(probs[1], 0.5, probs[2])))
    fout <- get_index(fit = fits[[i]], year = year, probs = probs)
    
    if (is.null(labels)) {
      fout$model <- as.character(fits[[i]]$formula)[1]
    } else {
      fout$model <- labels[i]
    }
    # fout$year <- yrs
    # fout$Qlower <- fout$Qlower / geo_mean(fout$Q50)
    # fout$Qupper <- fout$Qupper / geo_mean(fout$Q50)
    # fout$Q50 <- fout$Q50 / geo_mean(fout$Q50)
    df <- rbind(df, fout)
  }
  
  p <- ggplot(data = df)
  if (show_probs) {
    p <- p + geom_ribbon(data = df, aes(x = .data$Year, ymin = .data$Qlower, ymax = .data$Qupper, group = .data$model, fill = .data$model), alpha = 0.3, colour = NA)
  }
  p <- p + 
    geom_line(data = df, aes(x = .data$Year, y = .data$Q50, colour = .data$model, group = .data$model)) +
    labs(x = NULL, y = "Index") +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.y = unit(0, "lines"))
  
  return(p)
}
