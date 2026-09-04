#' Bayesian version of the quantile-quantile plot
#' 
#' This is similar to plot(fit, 2). It works for different distributions.
#' 
#' https://www.r-bloggers.com/the-many-uses-of-q-q-plots/
#' https://github.com/cran/car/blob/master/R/qqPlot.R
#' https://www.seascapemodels.org/rstats/2017/10/06/qqplot-non-normal-glm.html
#' https://mjskay.github.io/tidybayes/articles/tidybayes-residuals.html
#' 
#' @param fit An object of class \code{brmsfit}.
#' @param probs numeric vector of length two, representing probabilities. Corresponding quantile pairs define the line drawn.
#' @return a ggplot object
#' @importFrom stats residuals qqplot quantile ppoints qnorm qgamma
#' @import ggplot2
#' @export
#' 
plot_qq2 <- function(fit, probs = c(0.25, 0.75)) {
  n <- nrow(fit$data)
  
  # Extract the residuals
  res <- residuals(fit, summary = FALSE)
  iter <- nrow(res)
  dis <- matrix(NA, nrow(res), ncol(res))
  x <- matrix(NA, nrow(res), 2)
  # res <- residuals(fit)
  # res <- res[,"Estimate"]
  # dim(res)
  
  # Generate points from distribution
  if (fit$family$family %in% c("lognormal", "gaussian")) {
    dis <- qnorm(p = ppoints(n = length(res)))
    x <- qnorm(p = probs)
  } else if (fit$family$family == "gamma") {
    # shape <- posterior_summary(as.data.frame(fit)$shape)[,"Estimate"]
    shape <- as.data.frame(fit)$shape
    for (i in 1:iter) {
      dis[i,] <- qgamma(p = ppoints(n = length(res[i,])), shape = shape[i])
      x[i,] <- qgamma(p = probs, shape = shape[i])
    }
  } else {
    stop("Error: this family has not been coded yet.")
  }
  
  xy <- NULL
  for (i in 1:iter) {
    # Generate qq points
    qq_pts <- as.data.frame(qqplot(x = dis[i,], y = res[i,], plot.it = FALSE)) %>%
      mutate(iteration = i, data = 1:n)
    xy <- rbind(xy, qq_pts)
    
    # Generate qq line (stolen from the qqline function)
    # y <- quantile(res[i,], probs, type = 7, na.rm = TRUE)
    # slope <- diff(y) / diff(x[i,])
    # int <- y[1L] - slope * x[1L]
  }
  
  xy1 <- xy %>%
    group_by(.data$data) %>%
    summarise(x = mean(x), y = mean(y))
  
  ggplot(data = xy1, aes(x = .data$x, y = .data$y)) +
    geom_abline(intercept = int, slope = slope, linetype = "dashed") +
    geom_point(aes(), shape = 1) +
    labs(x = "Theoretical quantiles", y = "Sample quantiles") +
    theme_bw()
}




plot_implied_residuals2 <- function(fit, data = NULL, year = "Year") {
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
