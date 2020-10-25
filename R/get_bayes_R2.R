#' Get Bayesian R2 for a list of models
#' 
#' Calculates the Bayesian R2 for a list of brms models using the \code{add_criterion} function.
#' 
#' @param fits a list of model fits in the order that you want to compare them
#' @param ... additional parameters passed to \code{add_criterion}
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
#' @importFrom brms add_criterion
#' @import dplyr
#' @export
#' 
get_bayes_R2 <- function(fits, ...) {
  
  df <- NULL
  
  for (i in 1:length(fits)) {
    fit <- fits[[i]]
    if (!is.brmsfit(fit)) stop("fit is not an object of class brmsfit.")
    fit <- add_criterion(x = fit, criterion = "bayes_R2", ...)
    rdf <- data.frame(R2 = fit$criteria$bayes_R2) %>%
      mutate(Model = as.character(fit$formula)[1], Distribution = as.character(fit$family)[1], Link = as.character(fit$family)[2])
    df <- rbind(df, rdf)
  }
  
  df <- df %>% 
    group_by(.data$Model, .data$Distribution, .data$Link) %>% 
    summarise(R2 = mean(.data$R2)) %>%
    arrange(-.data$R2)
  
  df$diff <- c(0, diff(df$R2))
  
  return(df)
}
