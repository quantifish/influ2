#' Plot the influence metric for all variables in a model
#' 
#' @param fit a model fit
#' @param year the year variable label
#' @param fill the colour to use in the plot
#' @import ggplot2
#' @export
#' 
plot_influ <- function(fit, year = "fishing_year", fill = "purple") {
  # Extract the models variable names
  x1 <- gsub(paste0(as.character(fit$formula)[4], " ~ "), "", as.character(fit$formula)[1])
  x2 <- strsplit(x1, split = " + ", fixed = TRUE)[[1]]
  x <- x2[x2 != year]
  
  df <- NULL
  for (i in 1:length(x)) {
    inf1 <- get_influ(fit, group = c(year, x[i])) %>% mutate(variable = x[i])
    df <- rbind(df, inf1)
  }
  df$variable <- factor(df$variable, levels = x)
  
  ggplot(data = df, aes_string(x = year)) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_violin(aes(y = exp(.data$delta)), fill = fill, colour = fill, alpha = 0.5, draw_quantiles = 0.5, scale = "width") +
    facet_wrap(variable ~ ., ncol = 1, strip.position = "top") +
    labs(x = NULL, y = "Influence") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.y = unit(0, "lines"))
}

#' Get the influence metric
#' 
#' @param fit a model fit
#' @param group the variable to obtain
#' @param hurdle if a hurdle model then use the hurdle
#' @return a data frame
#' @importFrom reshape2 melt
#' @importFrom readr parse_number
#' @importFrom stats as.formula model.matrix quantile
#' @import dplyr
#' @export
#' 
get_influ <- function(fit, group = c("fishing_year", "area"), hurdle = FALSE) {
  
  # Model data
  data <- fit$data %>%
    mutate_at(vars(matches(group[2])), factor) %>%
    mutate(id = 1:n())
  y <- names(data)[1]
  
  # Posterior of coefficients
  # coefs <- get_coefs(fit = fit, var = group[2], normalise = TRUE, hurdle = hurdle)
  coefs <- get_coefs(fit = fit, var = group[2], normalise = FALSE, hurdle = hurdle)
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
    rename(iteration = .data$Var1, id = .data$Var2) %>%
    left_join(data, by = "id")
  influ_rho <- influ_var %>%
    group_by(.data$iteration) %>%
    summarise(rho = mean(.data$value))
  influ_delta <- left_join(influ_var, influ_rho, by = "iteration") %>%
    group_by(.dots = c("iteration", group[1])) %>%
    summarise(delta = mean(.data$value - .data$rho))
  # influ <- influ_delta %>%
  #   group_by(.dots = group[1]) %>%
  #   summarise(estimate = mean(exp(.data$delta)), lower = quantile(exp(.data$delta), probs = 0.05), upper = quantile(exp(.data$delta), probs = 0.95))
  
  return(influ_delta)
}
