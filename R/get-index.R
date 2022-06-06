#' Logit
#' 
#' @return a \code{data.frame} or a \code{ggplot} object.
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
#' @export
#' 
logit <- qlogis


#' Logistic
#' 
#' @return a \code{data.frame} or a \code{ggplot} object.
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
#' @export
#' 
logistic <- plogis


#' Get the unstandardised indices
#' 
#' @param fit An object of class \code{brmsfit}.
#' @param year The year or time label (e.g. year, Year, fishing_year, etc).
#' @param rescale How to rescale the series.
#' @return a \code{data.frame} or a \code{ggplot} object.
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
#' @import brms
#' @import dplyr
#' @export
#' 
get_unstandarsied <- function(fit, year = "year", rescale = 1) {

  if (!is.brmsfit(fit)) stop("fit is not an object of class brmsfit.")
  
  if (fit$family$family == "bernoulli" | grepl("hurdle", fit$family$family)) {
    prop <- data.frame(y = fit$data[,1], Year = fit$data[,year]) %>%
      mutate(y = ifelse(.data$y > 0, 1, 0)) %>%
      group_by(.data$Year) %>%
      summarise(p = sum(.data$y) / n())
    unstd <- data.frame(y = fit$data[,1], Year = fit$data[,year]) %>%
      filter(.data$y > 0) %>%
      group_by(.data$Year) %>%
      summarise(cpue = exp(mean(log(.data$y)))) %>%
      left_join(prop, by = "Year") %>%
      mutate(cpue = .data$cpue * .data$p)
  } else {
    unstd <- data.frame(y = fit$data[,1], Year = fit$data[,year]) %>%
      group_by(.data$Year) %>%
      summarise(cpue = exp(mean(log(.data$y))))
  }
  
  gm <- geo_mean(unstd$cpue)
  
  fout <- unstd %>%
    mutate(Mean = .data$cpue, Median = .data$cpue) %>%
    select(-.data$cpue)
  
  # Rescale the series
  if (rescale == "raw") {
    # nothing to do
  } else if (is.numeric(rescale)) {
    fout$Mean <- fout$Mean / gm * rescale
    fout$Median <- fout$Median / gm * rescale
  }
  
  return(fout)
}


#' Get the standardised indices
#' 
#' Get the standardised indices each year with associated uncertainty and return either as a table or a ggplot.
#' 
#' @param fit An object of class \code{brmsfit}.
#' @param year The year or time label (e.g. year, Year, fishing_year, etc).
#' @param probs The percentiles to be computed by the \code{quantile} function.
#' @param rescale How to re-scale the series. Choose from "raw" to retain the raw series, "unstandardised" to re-scale to the geometric mean of the unstandardised series, or a number to re-scale by. 
#' @param do_plot Return a \code{ggplot} object instead of a \code{data.frame}.
#' @param ... Additional parameters passed to \code{fitted}.
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
get_index <- function(fit, year = "year", probs = c(0.025, 0.975), rescale = 1, do_plot = FALSE, ...) {
  
  if (!is.brmsfit(fit)) stop("fit is not an object of class brmsfit.")
  
  # std <- get_coefs(fit = fit, var = year)
  yrs <- sort(unique(fit$data[,year]))
  n <- length(yrs)
  
  # Create newdata for prediction (using fitted)
  newdata <- fit$data %>% slice(rep(1, n))
  for (j in 1:ncol(newdata)) {
    x <- fit$data[,j]
    if (is.numeric(x)) {
      if (is.integer(x)) {
        newdata[,j] <- round(mean(x))
      }  else {
        newdata[,j] <- mean(x)
      }
    } else {
      newdata[,j] <- NA
    }
    # newdata[,j] <- ifelse(is.numeric(x) & !is.integer(x), mean(x), NA) working on monotonic vars
    # newdata[,j] <- ifelse(is.numeric(x), mean(x), NA)
  }
  newdata[,year] <- yrs
  newdata$pots <- 1
  head(newdata)
  
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
  fout1 <- fitted(object = fit, newdata = newdata, probs = c(probs[1], 0.5, probs[2]), re_formula = NA, ...) %>% 
    data.frame() %>%
    rename(Qlower = 3, Qupper = 5) %>% # this renames the 3rd and the 5th columns
    mutate(CV = .data$Est.Error / .data$Estimate) %>% # CV = SD / mu
    mutate(Year = yrs) %>%
    mutate(Model = as.character(fit$formula)[1], Distribution = as.character(fit$family)[1], Link = as.character(fit$family)[2])
  
  # Rescale the predicted CPUE. The options are:
  # 1. raw - don't rescale
  # 2. one - rescale so that the series has a geometric mean of one
  # 3. unstandardised - rescale to the geometric mean of the unstandardised series
  # 4. a user defined number
  fout <- fout1
  if (rescale == "raw") {
    # nothing to do
  } else if (rescale == "unstandardised") {
    unstd <- get_unstandarsied(fit = fit, year = year, rescale = "raw")
    gm <- geo_mean(unstd$Estimate)
    
    fout$Estimate <- fout$Estimate / geo_mean(fout$Estimate) * gm
    fout$Qlower <- fout$Qlower / geo_mean(fout$Q50) * gm
    fout$Qupper <- fout$Qupper / geo_mean(fout$Q50) * gm
    fout$Q50 <- fout$Q50 / geo_mean(fout$Q50) * gm
  } else if (is.numeric(rescale)) {
    fout$Estimate <- fout$Estimate / geo_mean(fout$Estimate) * rescale
    fout$Qlower <- fout$Qlower / geo_mean(fout$Q50) * rescale
    fout$Qupper <- fout$Qupper / geo_mean(fout$Q50) * rescale
    fout$Q50 <- fout$Q50 / geo_mean(fout$Q50) * rescale
  }
  fout$Est.Error <- fout$CV * fout$Estimate # SD = CV * mu
  
  if (do_plot) {
    p1 <- ggplot(data = fout1, aes(x = .data$Year)) +
      geom_errorbar(aes(y = .data$Q50, ymin = .data$Qlower, ymax = .data$Qupper)) +
      geom_point(aes(y = .data$Q50)) +
      geom_errorbar(aes(y = .data$Estimate, ymin = .data$Estimate - .data$Est.Error, ymax = .data$Estimate + .data$Est.Error), colour = "red", alpha = 0.75) +
      geom_point(aes(y = .data$Estimate), colour = "red", alpha = 0.75) +
      theme_bw()
    
    p2 <- ggplot(fout, aes(x = .data$Year)) +
      geom_errorbar(aes(y = .data$Q50, ymin = .data$Qlower, ymax = .data$Qupper)) +
      geom_point(aes(y = .data$Q50)) +
      geom_errorbar(aes(y = .data$Estimate, ymin = .data$Estimate - .data$Est.Error, ymax = .data$Estimate + .data$Est.Error), colour = "red", alpha = 0.75) +
      geom_point(aes(y = .data$Estimate), colour = "red", alpha = 0.75) +
      theme_bw()
    return(p1 + p2)
  } else {
    # Rename and reorder columns
    fout <- fout %>%
      rename(Mean = .data$Estimate, SD = .data$Est.Error, Median = .data$Q50) %>%
      mutate(Qlow = .data$Qlower, Qup = .data$Qupper) %>%
      rename_with(~paste0("Q", probs[1] * 100), .data$Qlow) %>%
      rename_with(~paste0("Q", probs[2] * 100), .data$Qup) %>%
      relocate(.data$Year, .data$Mean, .data$SD, .data$CV)
    
    return(fout)
  }
}
