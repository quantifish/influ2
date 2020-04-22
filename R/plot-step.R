utils::globalVariables(".")


#' Geometric mean
#' 
#' @param a a vector
#' @export
#' 
geo_mean <- function(a) {
  prod(a)^(1.0 / length(a))
}


#' Get model coefficients
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
index_plot <- function(fit, year = "Year", fill = "purple", probs = c(0.25, 0.75)) {
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
  
  fout <- fitted(object = fit, newdata = newdata, probs = c(probs[1], 0.5, probs[2])) %>% 
    data.frame() %>%
    # mutate(Q25 = .data$Q25 / geo_mean(.), Q50 = .data$Q50 / geo_mean(.), Q75 = .data$Q75 / geo_mean(.)) %>%
    mutate(model = "Standardised", year = yrs)
  fout$Q25 <- fout$Q25 / geo_mean(fout$Q50)
  fout$Q75 <- fout$Q75 / geo_mean(fout$Q50)
  fout$Q50 <- fout$Q50 / geo_mean(fout$Q50)
  
  unstd <- data.frame(y = fit$data[,1], year = fit$data[,year]) %>%
    group_by(year) %>%
    summarise(cpue = exp(mean(log(.data$y))))# %>%
    # mutate(cpue = .data$cpue / geo_mean(.))
  df1 <- fout %>%
    mutate(Estimate = NA, Est.Error = NA, Q25 = NA, Q50 = unstd$cpue / geo_mean(unstd$cpue), Q75 = NA, model = "Unstandardised")
  
  df <- rbind(fout, df1)
  df$model <- factor(df$model, levels = c("Unstandardised", "Standardised"))
  
  p <- ggplot(data = df, aes(x = .data$year, y = .data$Q50, group = .data$model)) +
    geom_ribbon(aes(ymin = .data$Q25, ymax = .data$Q75, fill = .data$model), alpha = 0.5, colour = NA) +
    geom_line(aes(colour = .data$model, linetype = .data$model)) +
    geom_point(aes(colour = .data$model)) +
    labs(x = NULL, y = "Index") +
    scale_colour_manual(values = c("grey", fill)) +
    scale_fill_manual(values = c("grey", fill)) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    theme_bw() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank(), legend.key.width = unit(2, "cm")) +
    guides(color = guide_legend(override.aes = list(fill = NA)))
  p
}


#' Get model coefficients
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
step_plot <- function(fits, year = "year", probs = c(0.25, 0.75), show_probs = TRUE) {
  
  m <- length(fits)
  
  df <- NULL
  df_grey <- NULL
  fout <- list()
  
  for (i in 1:m) {
    yrs <- sort(unique(fits[[i]]$data[,year]))
    n <- length(yrs)
    
    # Create newdata for prediction (using fitted)
    newdata <- fits[[i]]$data %>% slice(rep(1, n))
    for (j in 1:ncol(newdata)) {
      x <- fits[[i]]$data[,j]
      newdata[,j] <- ifelse(is.numeric(x), mean(x), NA)
    }
    newdata[,year] <- yrs

    fout[[i]] <- fitted(object = fits[[i]], newdata = newdata, probs = c(probs[1], 0.5, probs[2])) %>% 
      data.frame() %>%
      # mutate(Q25 = .data$Q25 / geo_mean(.), Q50 = .data$Q50 / geo_mean(.), Q75 = .data$Q75 / geo_mean(.)) %>%
      mutate(model = as.character(fits[[i]]$formula)[1], year = yrs, colour = "a")
    fout[[i]]$Q25 <- fout[[i]]$Q25 / geo_mean(fout[[i]]$Q50)
    fout[[i]]$Q75 <- fout[[i]]$Q75 / geo_mean(fout[[i]]$Q50)
    fout[[i]]$Q50 <- fout[[i]]$Q50 / geo_mean(fout[[i]]$Q50)
    
    if (i > 2) {
      xx <- fout[[i - 2]] %>% mutate(model = fout[[i]]$model, line = i)
      df_grey <- rbind(df_grey, xx)
    }
    if (i > 1) f2 <- fout[[i]] %>% mutate(Q50 = f1$Q50, colour = "b")
    f1 <- fout[[i]]
    if (i > 1) fout[[i]] <- rbind(fout[[i]], f2)
    
    df <- rbind(df, fout[[i]])
  }
  
  df$colour <- factor(df$colour, levels = c("b", "a"))
  
  p <- ggplot(data = df)
  p <- p + geom_line(data = df_grey, aes(x = .data$year, y = .data$Q50, group = .data$line), colour = "grey", linetype = "solid")
  if (show_probs) {
    p <- p + geom_ribbon(data = df, aes(x = .data$year, ymin = .data$Q25, ymax = .data$Q75, group = .data$colour, fill = .data$colour), alpha = 0.3, colour = NA)
  }
  p <- p + 
    geom_line(data = df, aes(x = .data$year, y = .data$Q50, colour = .data$colour, group = .data$colour, linetype = .data$colour)) +
    geom_point(data = df, aes(x = .data$year, y = .data$Q50, colour = .data$colour, shape = .data$colour)) +
    facet_wrap(model ~ ., ncol = 1, strip.position = "top") +
    labs(x = NULL, y = "Index") +
    scale_fill_manual(values = c(NA, "black")) +
    scale_colour_manual(values = c("black", "black")) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_shape_manual(values = c(NA, 19)) +
    theme_bw() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.y = unit(0, "lines"))
  p
}
