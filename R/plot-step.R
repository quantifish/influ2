#' Compare indices
#' 
#' @param fits a list of model fits in the order that you want to compare them
#' @param labels the labels for the fits
#' @param year the year or time label
#' @param probs the quantiles to plot
#' @param show_probs plot the quantiles or not
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
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.y = unit(0, "lines"))
  
  return(p)
}


#' A Bayesian version of the step-plot
#' 
#' This requires that all steps be run using brms and then provided as a list of model fits.
#' 
#' @param fits a list of model fits in the order that you want to compare them
#' @param year the year or time label
#' @param probs the quantiles to plot
#' @param show_probs plot the quantiles or not
#' @import brms
#' @import ggplot2
#' @import dplyr
#' @export
#' 
plot_step <- function(fits, year = "year", probs = c(0.25, 0.75), show_probs = TRUE) {
  
  m <- length(fits)
  
  df <- NULL
  df_grey <- NULL
  fout <- list()
  
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

    fout[[i]] <- get_index(fit = fits[[i]], year = year, probs = probs) %>%
      mutate(colour = "a")
    
    # fout[[i]] <- fitted(object = fits[[i]], newdata = newdata, probs = c(probs[1], 0.5, probs[2])) %>% 
    #   data.frame() %>%
    #   # mutate(Qlower = .data$Qlower / geo_mean(.), Q50 = .data$Q50 / geo_mean(.), Qupper = .data$Qupper / geo_mean(.)) %>%
    #   mutate(model = as.character(fits[[i]]$formula)[1], year = yrs, colour = "a")
    # fout[[i]]$Qlower <- fout[[i]]$Qlower / geo_mean(fout[[i]]$Q50)
    # fout[[i]]$Qupper <- fout[[i]]$Qupper / geo_mean(fout[[i]]$Q50)
    # fout[[i]]$Q50 <- fout[[i]]$Q50 / geo_mean(fout[[i]]$Q50)
    
    if (i > 2) {
      xx <- fout[[i - 2]] %>% mutate(Model = fout[[i]]$Model, line = i)
      df_grey <- rbind(df_grey, xx)
    }
    if (i > 1) f2 <- fout[[i]] %>% mutate(Q50 = f1$Q50, colour = "b")
    f1 <- fout[[i]]
    if (i > 1) fout[[i]] <- rbind(fout[[i]], f2)
    
    df <- rbind(df, fout[[i]])
  }
  
  df$colour <- factor(df$colour, levels = c("b", "a"))
  
  p <- ggplot(data = df)
  p <- p + geom_line(data = df_grey, aes(x = .data$Year, y = .data$Q50, group = .data$line), colour = "grey", linetype = "solid")
  if (show_probs) {
    p <- p + geom_ribbon(data = df, aes(x = .data$Year, ymin = .data$Qlower, ymax = .data$Qupper, group = .data$colour, fill = .data$colour), alpha = 0.3, colour = NA)
  }
  p <- p + 
    geom_line(data = df, aes(x = .data$Year, y = .data$Q50, colour = .data$colour, group = .data$colour, linetype = .data$colour)) +
    geom_point(data = df, aes(x = .data$Year, y = .data$Q50, colour = .data$colour, shape = .data$colour)) +
    facet_wrap(Model ~ ., ncol = 1, strip.position = "top") +
    labs(x = NULL, y = "Index") +
    scale_fill_manual(values = c(NA, "black")) +
    scale_colour_manual(values = c("black", "black")) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_shape_manual(values = c(NA, 19)) +
    theme_bw() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.y = unit(0, "lines"))
  p
}
