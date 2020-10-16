#' Get the standardised indices
#' 
#' Get the standardised indices each year with associated uncertainty and return either as a table or a ggplot.
#' 
#' @param fit An object of class \code{brmsfit}.
#' @param year The year or time label (e.g. year, Year, fishing_year, etc).
#' @param probs The percentiles to be computed by the \code{quantile} function.
#' @param rescale How to rescale the series.
#' @param do_plot Return a \code{ggplot} object instead of a \code{data.frame}.
#' @return a \code{data.frame} or a \code{ggplot} object.
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
#' @importFrom stats fitted
#' @import brms
#' @import ggplot2
#' @import patchwork
#' @import dplyr
#' @export
#' 
get_index <- function(fit, year = "year", probs = c(0.025, 0.975), rescale = "one", do_plot = FALSE) {
  
  if (!is.brmsfit(fit)) stop("fit is not an object of class brmsfit.")
  
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
  
  # fout1 <- fitted(object = fit, newdata = newdata, probs = c(probs[1], 0.5, probs[2]), re_formula = NA)
  # newdata <- newdata[,1:5]
  # newdata <- expand.grid(cpue = 1, period = unique(celr5$period), area2 = NA, vessel = NA, month = NA, "period:area2" = NA)
  # names(newdata) <- names(fit5$data)
  # head(fit$data)
  # head(newdata)
  # fout1 <- fitted(object = fit, newdata = newdata)
  # fout1 <- fitted(object = fit4, newdata = newdata, probs = c(probs[1], 0.5, probs[2]))
  # fout1 <- conditional_effects(x = fit, effects = "period")[[1]]
  # conditions <- data.frame(area2 = c("916", "917", "933"))
  # fout1 <- conditional_effects(x = fit, effects = "period", conditions = conditions)[[1]]
  # fout1 <- posterior_epred(object = fit, newdata = newdata, probs = c(probs[1], 0.5, probs[2]))
  # fout1 <- posterior_predict(object = fit, newdata = newdata)
  # fout1 <- fitted(object = fit)
  # pred1 <- predict(fit, newdata = newdata, re_formula = NULL, allow_new_levels = TRUE)
  
  # Get the predicted CPUE by year
  fout1 <- fitted(object = fit, newdata = newdata, probs = c(probs[1], 0.5, probs[2]), re_formula = NA) %>% 
    data.frame() %>%
    rename(Qlower = 3, Qupper = 5) %>%
    mutate(CV = .data$Est.Error / .data$Estimate, Year = yrs) %>% # CV = SD / mu
    mutate(Model = as.character(fit$formula)[1], Distribution = as.character(fit$family)[1], Link = as.character(fit$family)[2])
  
  p1 <- ggplot(data = fout1, aes(x = .data$Year)) +
    geom_pointrange(aes(y = .data$Q50, ymin = .data$Qlower, ymax = .data$Qupper)) +
    geom_pointrange(aes(y = .data$Estimate, ymin = .data$Estimate - .data$Est.Error, ymax = .data$Estimate + .data$Est.Error), colour = "red", alpha = 0.5)
  
  # Rescale the predicted CPUE. The options are:
  # 1. raw - don't rescale
  # 2. one - rescale so that the series has a geometric mean of one
  # 3. unstandardised - rescale to the geometric mean of the unstandardised series
  # 4. a user defined number
  fout <- fout1
  if (rescale == "one") {
    fout$Estimate <- fout$Estimate / geo_mean(fout$Estimate)
    fout$Qlower <- fout$Qlower / geo_mean(fout$Q50)
    fout$Qupper <- fout$Qupper / geo_mean(fout$Q50)
    fout$Q50 <- fout$Q50 / geo_mean(fout$Q50)
  } else if (rescale == "unstandardised") {
    unstd <- data.frame(y = fit$data[,1], year = fit$data[,year]) %>%
      group_by(year) %>%
      summarise(cpue = exp(mean(log(.data$y))))
    fout$Estimate <- fout$Estimate / geo_mean(fout$Estimate) * geo_mean(unstd$cpue)
    fout$Qlower <- fout$Qlower / geo_mean(fout$Q50) * geo_mean(unstd$cpue)
    fout$Qupper <- fout$Qupper / geo_mean(fout$Q50) * geo_mean(unstd$cpue)
    fout$Q50 <- fout$Q50 / geo_mean(fout$Q50) * geo_mean(unstd$cpue)
  } else if (is.numeric(rescale)) {
    fout$Estimate <- fout$Estimate / geo_mean(fout$Estimate) * rescale
    fout$Qlower <- fout$Qlower / geo_mean(fout$Q50) * rescale
    fout$Qupper <- fout$Qupper / geo_mean(fout$Q50) * rescale
    fout$Q50 <- fout$Q50 / geo_mean(fout$Q50) * rescale
  }
  
  fout$Est.Error <- fout$CV * fout$Estimate # SD = CV * mu
  
  p2 <- ggplot(fout, aes(x = .data$Year)) +
    geom_pointrange(aes(y = .data$Q50, ymin = .data$Qlower, ymax = .data$Qupper)) +
    geom_pointrange(aes(y = .data$Estimate, ymin = .data$Estimate - .data$Est.Error, ymax = .data$Estimate + .data$Est.Error), colour = "red", alpha = 0.5)
  
  if (do_plot) {
    return(p1 + p2)
  } else {
    return(fout)
  }
}
