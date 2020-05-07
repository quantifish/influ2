#' Bayesian version of the quantile-quantile plot
#' 
#' This is similar to plot(fit, 2). It works for different distributions.
#' 
#' @param fit An object of class \code{brmsfit}.
#' @param probs numeric vector of length two, representing probabilities. Corresponding quantile pairs define the line drawn.
#' @return a ggplot object
#' @importFrom stats residuals qqplot quantile ppoints qnorm qgamma
#' @import ggplot2
#' @export
#' 
plot_qq <- function(fit, probs = c(0.25, 0.75)) {
  # Extract the residuals
  res <- residuals(fit)
  res <- res[,"Estimate"]
  
  # Generate points from distribution
  if (fit$family$family %in% c("lognormal", "gaussian")) {
    dis <- qnorm(p = ppoints(n = length(res)))
    x <- qnorm(p = probs)
  } else if (fit$family$family == "gamma") {
    shape <- posterior_summary(as.data.frame(fit)$shape)[,"Estimate"]
    dis <- qgamma(p = ppoints(n = length(res)), shape = shape)
    x <- qgamma(p = probs, shape = shape)
  } else {
    stop("Poop - this family has not been coded yet.")
  }
  
  # Generate qq points
  xy <- as.data.frame(qqplot(x = dis, y = res, plot.it = FALSE))
  
  # Generate qq line (stolen from the qqline function)
  y <- quantile(res, probs, type = 7, na.rm = TRUE)
  slope <- diff(y) / diff(x)
  int <- y[1L] - slope * x[1L]
  
  ggplot(data = xy) +
    geom_abline(intercept = int, slope = slope, linetype = "dashed") +
    geom_point(aes(x = x, y = y), shape = 1) +
    labs(x = "Theoretical quantiles", y = "Sample quantiles") +
    theme_bw()
}


#' Plots predicted values versus residuals
#' 
#' This plots predicted values against residuals including uncertainty, using the fitted and residual functions on a brmsfit object.
#' 
#' @param fit An object of class \code{brmsfit}.
#' @param trend show a loess smoother or linear line
#' @return a ggplot object
#' @importFrom stats fitted residuals
#' @import ggplot2
#' @import dplyr
#' @export
#' 
plot_predicted_residuals <- function(fit, trend = "loess") {
  # Extract predicted values
  pred <- fitted(fit) %>% data.frame()
  names(pred) <- paste0("pred.", names(pred))
  
  # Extract residuals
  resid <- residuals(fit) %>% data.frame()
  names(resid) <- paste0("resid.", names(resid))
  
  p <- ggplot(data = cbind(resid, pred, fit$data), aes(x = .data$pred.Estimate, y = .data$resid.Estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_errorbarh(aes(xmax = .data$pred.Q2.5, xmin = .data$pred.Q97.5, height = 0), alpha = 0.75) +
    geom_pointrange(aes(ymin = .data$resid.Q2.5, ymax = .data$resid.Q97.5), alpha = 0.75) +
    labs(x = "Predicted values", y = "Residuals") +
    theme_bw()
  if (trend == "loess") {
    p <- p + geom_smooth(method = "loess", se = FALSE, formula = y ~ x)
  }
  if (trend %in% c("lm", "linear")) {
    p <- p + geom_smooth(method = "lm", se = FALSE, formula = y ~ x)
  }  
  return(p)
}


#' Plots predicted values versus residuals
#' 
#' This plots predicted values against residuals including uncertainty, using the fitted and residual functions on a brmsfit object.
#' 
#' @param fit An object of class \code{brmsfit}.
#' @param data a data frame with the same dimensions as the model data
#' @param year the year column
#' @return a ggplot object
#' @importFrom stats fitted residuals
#' @import ggplot2
#' @import dplyr
#' @export
#' 
plot_implied_residuals <- function(fit, data = NULL, year = "Year") {
  # Get the data
  if (is.null(data)) {
    data <- fit$data
  }
  
  # Extract predicted values
  idx <- get_index(fit, year = year)
  mean(idx$Estimate)
  idx$Estimate <- idx$Estimate - mean(idx$Estimate)
  mean(idx$Estimate)
  
  # Extract residuals
  resid <- residuals(fit) %>% data.frame()
  # names(resid) <- paste0("resid.", names(resid))
  resid <- cbind(data, resid) %>%
    group_by(.data$Year, .data$Area) %>%
    summarise(residual = mean(.data$Estimate))

  ires <- left_join(idx, resid, by = year) %>%
    mutate(implied = .data$Estimate + .data$residual)

  p <- ggplot(data = ires, aes(x = .data$Year, y = .data$implied)) +
    geom_line(data = idx, aes(x = .data$Year, y = .data$Estimate), group = 1, colour = "grey") +
    geom_line(group = 1, colour = "purple") +
    labs(x = NULL, y = "Residuals") +
    facet_wrap(.data$Area ~ ., ncol = 2) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(p)
}
