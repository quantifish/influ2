#' Geometric mean
#' 
#' @param a a vector
#' @export
#' 
geo_mean <- function(a) {
  prod(a)^(1.0 / length(a))
}


#' Get the standardised indices
#' 
#' Get the standardised indices each year with associated uncertainty as a table.
#' 
#' @param fit a list of model fits in the order that you want to compare them
#' @param year the year or time label
#' @param probs the quantiles to plot
#' @param do_plot return a plot instead of a data frame
#' @importFrom stats fitted
#' @import brms
#' @import ggplot2
#' @import patchwork
#' @import dplyr
#' @export
#' 
get_index <- function(fit, year = "year", probs = c(0.025, 0.975), do_plot = FALSE) {
  # std <- get_coefs(fit = fit, var = year)
  yrs <- sort(unique(fit$data[,year]))
  n <- length(yrs)
  
  # Create newdata for prediction (using fitted)
  newdata <- fit$data %>% slice(rep(1, n))
  for (j in 1:ncol(newdata)) {
    x <- fit$data[,j]
    newdata[,j] <- ifelse(is.numeric(x), mean(x), NA)
  }
  newdata[,year] <- yrs
  
  # CV = SD / mu
  fout1 <- fitted(object = fit, newdata = newdata, probs = c(probs[1], 0.5, probs[2])) %>% 
    data.frame() %>%
    rename(Qlower = 3, Qupper = 5) %>%
    mutate(CV = .data$Est.Error / .data$Estimate, Year = yrs) %>%
    mutate(Model = as.character(fit$formula)[1], Distribution = as.character(fit$family)[1], Link = as.character(fit$family)[2])
  
  p1 <- ggplot(fout1, aes(x = .data$Year)) +
    geom_pointrange(aes(y = .data$Q50, ymin = .data$Qlower, ymax = .data$Qupper)) +
    geom_pointrange(aes(y = .data$Estimate, ymin = .data$Estimate - .data$Est.Error, ymax = .data$Estimate + .data$Est.Error), colour = "red", alpha = 0.5)
  
  fout <- fout1
  fout$Estimate <- fout$Estimate / geo_mean(fout$Estimate)
  fout$Est.Error <- fout$CV * fout$Estimate # SD = CV * mu
  
  fout$Qlower <- fout$Qlower / geo_mean(fout$Q50)
  fout$Qupper <- fout$Qupper / geo_mean(fout$Q50)
  fout$Q50 <- fout$Q50 / geo_mean(fout$Q50)
  
  p2 <- ggplot(fout, aes(x = .data$Year)) +
    geom_pointrange(aes(y = .data$Q50, ymin = .data$Qlower, ymax = .data$Qupper)) +
    geom_pointrange(aes(y = .data$Estimate, ymin = .data$Estimate - .data$Est.Error, ymax = .data$Estimate + .data$Est.Error), colour = "red", alpha = 0.5)
  
  if (do_plot) {
    return(p1 + p2)
  } else {
    return(fout)
  }
}


#' Plot the standardised and unstandardised indices
#' 
#' In this plot the unstandardised indices is the geometric mean of the data.
#' 
#' @param fit a list of model fits in the order that you want to compare them
#' @param year the year or time label
#' @param fill the fill colour for the probs
#' @param probs the quantiles to plot
#' @importFrom stats fitted
#' @import brms
#' @import ggplot2
#' @import dplyr
#' @export
#' 
plot_index <- function(fit, year = "Year", fill = "purple", probs = c(0.25, 0.75)) {
  # yrs <- sort(unique(fit$data[,year]))
  # n <- length(yrs)
  # 
  # # Create newdata for prediction (using fitted)
  # newdata <- fit$data %>% slice(rep(1, n))
  # for (j in 1:ncol(newdata)) {
  #   x <- fit$data[,j]
  #   newdata[,j] <- ifelse(is.numeric(x), mean(x), NA)
  # }
  # newdata[,year] <- yrs
  # 
  # fout <- fitted(object = fit, newdata = newdata, probs = c(probs[1], 0.5, probs[2])) %>% 
  #   data.frame() %>%
  #   # mutate(Qlower = .data$Qlower / geo_mean(.), Q50 = .data$Q50 / geo_mean(.), Qupper = .data$Qupper / geo_mean(.)) %>%
  #   mutate(model = "Standardised", year = yrs)
  # fout$Qlower <- fout$Qlower / geo_mean(fout$Q50)
  # fout$Qupper <- fout$Qupper / geo_mean(fout$Q50)
  # fout$Q50 <- fout$Q50 / geo_mean(fout$Q50)
  fout <- get_index(fit = fit, year = year, probs = probs) %>%
    mutate(model = "Standardised")
  
  unstd <- data.frame(y = fit$data[,1], year = fit$data[,year]) %>%
    group_by(year) %>%
    summarise(cpue = exp(mean(log(.data$y))))
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
    theme_bw() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank(), legend.key.width = unit(2, "cm")) +
    guides(color = guide_legend(override.aes = list(fill = NA)))
  
  return(p)
}
