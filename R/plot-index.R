#' Plot the standardised and unstandardised indices
#' 
#' In this plot the unstandardised indices is the geometric mean of the data.
#' 
#' @param fit An object of class \code{brmsfit}.
#' @param year the year or time label.
#' @param fill the fill colour for the percentiles.
#' @param probs The percentiles to be computed by the \code{quantile} function.
#' @param rescale the index of the series to rescale to. If set to NULL then no rescaling is done.
#' @param show_unstandardised show the unstandardised series or not.
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
plot_index <- function(fit, 
                       year = "Year", 
                       fill = "purple", 
                       probs = c(0.25, 0.75),
                       rescale = 1,
                       show_unstandardised = TRUE) {
  
  if (!is.brmsfit(fit)) stop("fit is not an object of class brmsfit.")

  # Get the standardised series
  fout <- get_index(fit = fit, year = year, probs = probs, rescale = rescale) %>%
    mutate(model = "Standardised")
  
  # Get the unstandardised series
  unstd <- get_unstandarsied(fit = fit, year = year, rescale = rescale) %>%
    mutate(model = "Unstandardised")
  
  df <- bind_rows(fout, unstd)
  
  df$model <- factor(df$model, levels = c("Unstandardised", "Standardised"))
  
  if (!show_unstandardised) {
    df <- df %>% filter(model != "Unstandardised")
    scale_col <- fill
    scale_lin <- "solid"
  } else {
    scale_col <- c("grey", fill)
    scale_lin <- c("dashed", "solid")
  }
  
  p <- ggplot(data = df, aes(x = .data$Year, y = .data$Median, group = .data$model)) +
    # geom_ribbon(aes(ymin = .data$Qlower, ymax = .data$Qupper, fill = .data$model), alpha = 0.5, colour = NA) +
    geom_ribbon(data = df %>% filter(.data$model != "Unstandardised"), aes(ymin = .data$Qlower, ymax = .data$Qupper), alpha = 0.5, colour = NA, fill = fill) +
    geom_line(aes(colour = .data$model, linetype = .data$model)) +
    geom_point(aes(colour = .data$model)) +
    labs(x = NULL, y = "Index") +
    # scale_fill_manual(values = c("grey", fill)) + scale_fill_manual(values = fill) +
    scale_colour_manual(values = scale_col) +
    scale_linetype_manual(values = scale_lin) +
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
plot_hurdle <- function(fit, year = "Year", fill = "purple", probs = c(0.025, 0.975)) {
  
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
  mu <- fitted(object = fit, newdata = newdata, probs = c(probs[1], 0.5, probs[2]), re_formula = NA, dpar = "mu") %>% 
    cbind(newdata) %>%
    rename(Qlower = 3, Qupper = 5) %>% # this renames the 3rd and the 5th columns
    mutate(model = "Positive") %>%
    mutate(Estimate = exp(.data$Estimate), Q50 = exp(.data$Q50), Qlower = exp(.data$Qlower), Qupper = exp(.data$Qupper))
  # mu$Estimate <- mu$Estimate / geo_mean(mu$Estimate)
  mu$Qlower <- mu$Qlower / geo_mean(mu$Q50)
  mu$Qupper <- mu$Qupper / geo_mean(mu$Q50)
  mu$Q50 <- mu$Q50 / geo_mean(mu$Q50)

  # Get the hurdle component
  hu <- fitted(object = fit, newdata = newdata, probs = c(probs[1], 0.5, probs[2]), re_formula = NA, dpar = "hu") %>% 
    cbind(newdata) %>%
    rename(Qlower = 3, Qupper = 5) %>% # this renames the 3rd and the 5th columns
    mutate(model = "Hurdle")# %>%
    # mutate(Estimate = inv_logit(Estimate), Qlower = inv_logit(Qlower), Qupper = inv_logit(Qupper))
  # hu$Estimate <- hu$Estimate / geo_mean(hu$Estimate)
  hu$Qlower <- hu$Qlower / geo_mean(hu$Q50)
  hu$Qupper <- hu$Qupper / geo_mean(hu$Q50)
  hu$Q50 <- hu$Q50 / geo_mean(hu$Q50)
  
  # Get the combined series
  # bt <- fitted(object = fit, newdata = newdata, probs = c(probs[1], 0.5, probs[2]), re_formula = NA) %>%
  #   cbind(newdata) %>%
  #   rename(Qlower = 3, Qupper = 5) %>% # this renames the 3rd and the 5th columns
  #   mutate(model = "Combined")
  # bt$Qlower <- bt$Qlower / geo_mean(bt$Q50)
  # bt$Qupper <- bt$Qupper / geo_mean(bt$Q50)
  # bt$Q50 <- bt$Q50 / geo_mean(bt$Q50)
  bt <- get_index(fit = fit, year = year, probs = probs, rescale = 1) %>%
    mutate(model = "Combined")
  bt[year] <- bt$Year
  
  df <- bind_rows(mu, hu, bt)
  # df$model <- factor(df$model, levels = c("Hurdle", "Positive", "Combined"))
  
  p <- ggplot(data = df, aes(x = .data$year, y = .data$Q50, group = .data$model)) +
    geom_ribbon(aes(ymin = .data$Qlower, ymax = .data$Qupper, fill = .data$model), alpha = 0.5, colour = NA) +
    geom_line(aes(colour = .data$model, linetype = .data$model)) +
    geom_point(aes(colour = .data$model)) +
    labs(x = NULL, y = "Index") +
    # scale_colour_manual(values = c("grey", fill)) +
    # scale_fill_manual(values = c("grey", fill)) +
    # scale_linetype_manual(values = c("dashed", "solid")) +
    # scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    theme_bw() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank(), legend.key.width = unit(2, "cm")) +
    guides(color = guide_legend(override.aes = list(fill = NA)))

    return(p)
}