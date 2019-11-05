#' Get the influence metric
#' 
#' @param fit a model fit
#' @param group the variable to obtain
#' @return a data frame
#' @importFrom reshape2 melt
#' @importFrom readr parse_number
#' @importFrom stats as.formula model.matrix quantile
#' @import dplyr
#' @export
#' 
get_influ <- function(fit, group = c("fishing_year", "area")) {
  
  # Model data
  data <- fit$data %>%
    mutate_at(vars(matches(group[2])), factor) %>%
    mutate(id = 1:n())
  y <- names(data)[1]
  
  # Posterior of coefficients
  coefs <- get_coefs(fit = fit, var = group[2], normalise = TRUE)
  n_iterations <- max(coefs$iteration)
  
  if (nrow(fit$ranef) > 0) {
    X <- model.matrix(as.formula(paste0(y, " ~ 0 + ", group[2])), data = data)
  } else {
    X <- model.matrix(as.formula(paste0(y, " ~ ", group[2])), data = data)
  }
  
  # Do the matrix multiplication
  Xbeta <- matrix(NA, nrow = n_iterations, ncol = nrow(data))
  for (i in 1:n_iterations) {
    Xbeta[i,] <- X %*% filter(coefs, .data$iteration == i)$value
  }
  
  influ_var <- melt(Xbeta) %>%
    rename(iteration = Var1, id = Var2) %>%
    left_join(data, by = "id")
  influ_rho <- influ_var %>%
    group_by(iteration) %>%
    summarise(rho = mean(value))
  influ_delta <- left_join(influ_var, influ_rho, by = "iteration") %>%
    group_by(.dots = c("iteration", group[1])) %>%
    summarise(delta = mean(value - rho))
  influ <- influ_delta %>%
    group_by(.dots = group[1]) %>%
    summarise(estimate = mean(exp(delta)), lower = quantile(exp(delta), probs = 0.05), upper = quantile(exp(delta), probs = 0.95))
  
  return(influ_delta)
}
