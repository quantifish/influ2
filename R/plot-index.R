#' Plot the standardised and unstandardised indices
#' 
#' In this plot the unstandardised indices is the geometric mean of the data.
#' 
#' @param fit An object of class \code{brmsfit}.
#' @param year the year or time label.
#' @param fill the fill colour for the percentiles.
#' @param probs The percentiles to be computed by the \code{quantile} function.
#' @return a \code{ggplot} object.
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
#' @importFrom stats fitted
#' @import brms
#' @import ggplot2
#' @import dplyr
#' @export
#' 
plot_index <- function(fit, year = "Year", fill = "purple", probs = c(0.25, 0.75)) {
  
  if (!is.brmsfit(fit)) stop("fit is not an object of class brmsfit.")

  # Get the standardised series
  fout <- get_index(fit = fit, year = year, probs = probs) %>%
    mutate(model = "Standardised")
  
  # Get the unstandardised series
  unstd <- get_unstandarsied(fit = fit, year = year, rescale = "one") %>%
    mutate(model = "Unstandardised")
  
  df <- bind_rows(fout, unstd)
  df$model <- factor(df$model, levels = c("Unstandardised", "Standardised"))
  
  p <- ggplot(data = df, aes(x = .data$Year, y = .data$Q50, group = .data$model)) +
    geom_ribbon(aes(ymin = .data$Qlower, ymax = .data$Qupper, fill = .data$model), alpha = 0.5, colour = NA) +
    geom_line(aes(colour = .data$model, linetype = .data$model)) +
    geom_point(aes(colour = .data$model)) +
    labs(x = NULL, y = "Index") +
    scale_colour_manual(values = c("grey", fill)) +
    scale_fill_manual(values = c("grey", fill)) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    theme_bw() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank(), legend.key.width = unit(2, "cm")) +
    guides(color = guide_legend(override.aes = list(fill = NA)))
  
  return(p)
}


#' Plot the hurdle and positive components
#' 
#' @param fit An object of class \code{brmsfit}.
#' @param year the year or time label.
#' @param fill the fill colour for the percentiles.
#' @param probs The percentiles to be computed by the \code{quantile} function.
#' @return a \code{ggplot} object.
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
#' @importFrom stats fitted
#' @import brms
#' @import ggplot2
#' @import dplyr
#' @export
#' 
plot_hurdle <- function(fit, year = "Year", fill = "purple", probs = c(0.25, 0.75)) {
  
  if (!is.brmsfit(fit)) stop("fit is not an object of class brmsfit.")
  
  yrs <- sort(unique(fit$data[,year]))
  n <- length(yrs)
  
  # Create newdata for prediction (using fitted)
  newdata <- fit$data %>% slice(rep(1, n))
  for (j in 1:ncol(newdata)) {
    x <- fit$data[,j]
    newdata[,j] <- ifelse(is.numeric(x), mean(x), NA)
  }
  newdata[,year] <- yrs
  
  # Get the positive component
  mu <- fitted(object = fit, newdata = newdata, re_formula = NA, dpar = "mu") %>% 
    cbind(newdata) %>%
    rename(Qlower = 3, Qupper = 4) %>% # this renames the 3rd and the 5th columns
    mutate(model = "Lognormal") %>%
    mutate(Estimate = exp(.data$Estimate), Qlower = exp(.data$Qlower), Qupper = exp(.data$Qupper))
  mu$Estimate <- mu$Estimate / geo_mean(mu$Estimate)
  mu$Qlower <- mu$Qlower / geo_mean(mu$Q50)
  mu$Qupper <- mu$Qupper / geo_mean(mu$Q50)
  # mu$Q50 <- mu$Q50 / geo_mean(mu$Q50)
  # head(mu)
  
  # Get the hurdle component
  hu <- fitted(object = fit, newdata = newdata, re_formula = NA, dpar = "hu") %>% 
    cbind(newdata) %>%
    rename(Qlower = 3, Qupper = 4) %>% # this renames the 3rd and the 5th columns
    mutate(model = "Hurdle")# %>%
    # mutate(Estimate = inv_logit(Estimate), Qlower = inv_logit(Qlower), Qupper = inv_logit(Qupper))
  hu$Estimate <- hu$Estimate / geo_mean(hu$Estimate)
  hu$Qlower <- hu$Qlower / geo_mean(hu$Q50)
  hu$Qupper <- hu$Qupper / geo_mean(hu$Q50)
  # head(hu)
  
  # Get the combined series
  # bt <- fitted(object = fit, newdata = newdata, re_formula = NA) %>% 
  #   cbind(newdata) %>%
  #   rename(Qlower = 3, Qupper = 4) %>% # this renames the 3rd and the 5th columns
  #   mutate(model = "Combined")
  # head(bt)
  bt <- get_index(fit = fit, year = year, probs = probs) %>%
    mutate(model = "Combined")
  bt[year] <- bt$Year
  
  df <- bind_rows(mu, hu, bt)
  # df$model <- factor(df$model, levels = c("Unstandardised", "Standardised"))
  
  p <- ggplot(data = df, aes(x = .data$year, y = .data$Estimate, group = .data$model)) +
    geom_ribbon(aes(ymin = .data$Qlower, ymax = .data$Qupper, fill = .data$model), alpha = 0.5, colour = NA) +
    geom_line(aes(colour = .data$model, linetype = .data$model)) +
    geom_point(aes(colour = .data$model)) +
    labs(x = NULL, y = "Index") +
    # scale_colour_manual(values = c("grey", fill)) +
    # scale_fill_manual(values = c("grey", fill)) +
    # scale_linetype_manual(values = c("dashed", "solid")) +
    # scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    theme_bw() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank(), legend.key.width = unit(2, "cm")) +
    guides(color = guide_legend(override.aes = list(fill = NA)))
  
  return(p)
}