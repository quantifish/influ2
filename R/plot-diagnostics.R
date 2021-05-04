#' Plots predicted values versus residuals
#' 
#' This plots predicted values against residuals including uncertainty, using the fitted and residual functions on a brmsfit object.
#' 
#' @param fit An object of class \code{brmsfit}.
#' @param data a data frame with the same dimensions as the model data
#' @param year the year column
#' @param groups the grouping/facet variable
#' @return a ggplot object
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
#' @importFrom stats fitted residuals
#' @import ggplot2
#' @import dplyr
#' @export
#' 
plot_implied_residuals <- function(fit, data = NULL, year = "Year", groups = "Species") {
  grp <- c(year, groups)
  grp2 <- c("Year", groups)
  
  # Get the data
  if (is.null(data)) {
    data <- fit$data
  }
  data <- data %>%
    mutate(id = 1:n()) %>%
    rename(Year = all_of(year))

  # Extract predicted values and normalise
  idx <- get_index(fit, year = year)
  idx$Estimate <- idx$Estimate - mean(idx$Estimate)

  # Extract residuals
  ires <- residuals(fit, summary = FALSE) %>% 
    melt() %>%
    rename(iteration = .data$Var1, id = .data$Var2, residual = .data$value) %>%
    left_join(data, by = "id") %>% 
    left_join(idx, by = "Year") %>%
    mutate(implied = .data$Estimate + .data$residual) %>%
    group_by_at(grp2) %>%
    summarise(Estimate = mean(.data$implied), Qlower = quantile(.data$implied, probs = 0.05), Qupper = quantile(.data$implied, probs = 0.95))

  p <- ggplot(data = ires, aes(x = .data$Year, y = .data$Estimate)) +
    geom_line(data = idx, aes(x = .data$Year, y = .data$Estimate), group = 1, linetype = "dashed") +
    geom_pointrange(aes(min = .data$Qlower, max = .data$Qupper), colour = "purple") +
    geom_line(group = 1, colour = "purple") +
    labs(x = NULL, y = "Residuals") +
    facet_wrap(as.formula(paste("~", groups)), ncol = 2) +
    theme_bw()
  
  return(p)
}


#' Plots predicted values versus residuals
#' 
#' This plots predicted values against residuals including uncertainty, using the fitted and residual functions on a brmsfit object.
#' 
#' @param fit An object of class \code{brmsfit}.
#' @param trend show a loess smoother or linear line
#' @param type The type of the residuals, either "ordinary" or "pearson".
#' @return a ggplot object
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
#' @importFrom stats fitted residuals
#' @import ggplot2
#' @import dplyr
#' @export
#' 
plot_predicted_residuals <- function(fit, trend = "loess", type = "pearson") {
  # Extract predicted values
  pred <- fitted(fit) %>% 
    data.frame()
  names(pred) <- paste0("pred.", names(pred))
  
  # Extract residuals
  resid <- residuals(fit, type = type) %>% 
    data.frame()
  names(resid) <- paste0("resid.", names(resid))
  df <- cbind(resid, pred, fit$data)
  
  p <- ggplot(data = df, aes(x = .data$pred.Estimate, y = .data$resid.Estimate)) +
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
