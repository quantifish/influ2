#' Bayesian version of the quantile-quantile plot
#' 
#' This is similar to plot(fit_glm, 2). It works for different distributions.
#' 
#' @param fit a model fit
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
#' @param fit a brmsfit object
#' @param loess show a loess smoother
#' @return a ggplot object
#' @importFrom stats fitted residuals
#' @import ggplot2
#' @import dplyr
#' @export
#' 
plot_predicted_residuals <- function(fit, loess = TRUE) {
  # Extract predicted values
  pred <- fitted(fit) %>% data.frame()
  names(pred) <- paste0("pred.", names(pred))
  
  # Extract residuals
  resid <- residuals(fit) %>% data.frame()
  names(resid) <- paste0("resid.", names(resid))
  
  p <- ggplot(data = cbind(resid, pred), aes(x = .data$pred.Estimate, y = .data$resid.Estimate)) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_errorbarh(aes(xmax = .data$pred.Q2.5, xmin = .data$pred.Q97.5, height = 0), alpha = 0.75) +
    geom_pointrange(aes(ymin = .data$resid.Q2.5, ymax = .data$resid.Q97.5), alpha = 0.75) +
    labs(x = "Predicted values", y = "Residuals") +
    theme_bw()
  if (loess) {
    p <- p + geom_smooth(method = "loess", se = FALSE, formula = y ~ x)
  }
  p
}
