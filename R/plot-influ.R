#' Plot the influence metric for all variables in a model
#' 
#' The influence metric indicates how much of a standardisation effect the model is having.
#' 
#' @param fit An object of class \code{brmsfit}.
#' @param year the year variable label.
#' @param fill the colour to use in the plot.
#' @param hurdle if a hurdle model then use the hurdle
#' @return a \code{ggplot} object.
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
#' @seealso \code{\link{get_influ}}
#' 
#' @examples
#' \dontrun{
#' data(epilepsy)
#' epilepsy$Age <- factor(epilepsy$Age)
#' fit1 <- brm(count ~ Age + (1|patient), data = epilepsy, family = poisson(), iter = 100)
#' summary(fit1)
#' plot_influ(fit = fit1, year = "Age")
#' }
#' 
#' @import ggplot2
#' @export
#' 
plot_influ <- function(fit, year = "fishing_year", fill = "purple", hurdle = FALSE) {
  
  if (!is.brmsfit(fit)) stop("fit is not an object of class brmsfit.")
  
  # Extract the models variable names
  x1 <- gsub(paste0(as.character(fit$formula)[4], " ~ "), "", as.character(fit$formula)[1])
  x2 <- strsplit(x1, split = " + ", fixed = TRUE)[[1]]
  x <- x2[x2 != year]
  x <- gsub("\\(1 \\| ", "", x)
  x <- gsub("\\)", "", x)
  
  df <- NULL
  for (i in 1:length(x)) {
    inf1 <- get_influ2(fit = fit, group = c(year, x[i]), hurdle = hurdle) %>% 
      mutate(variable = x[i])
    
    df <- rbind(df, inf1)
  }
  
  # Order the factor levels
  df$variable <- factor(df$variable, levels = x)
  
  p <- ggplot(data = df, aes_string(x = year)) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_violin(aes(y = exp(.data$delta)), fill = fill, colour = fill, alpha = 0.5, draw_quantiles = 0.5, scale = "width") +
    facet_wrap(variable ~ ., ncol = 1, strip.position = "top") +
    labs(x = NULL, y = "Influence") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.y = unit(0, "lines"))
  
  return(p)
}
