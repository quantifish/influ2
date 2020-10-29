#' Bayesian version of the quantile-quantile plot
#' 
#' This is similar to plot(fit, 2). It works for different distributions.
#' 
#' @param fit An object of class \code{brmsfit}.
#' @param probs numeric vector of length two, representing probabilities. Corresponding quantile pairs define the line drawn.
#' @return a ggplot object
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
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
