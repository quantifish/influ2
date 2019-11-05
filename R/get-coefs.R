#' Get model coefficients
#' 
#' @param fit a model fit
#' @param var the variable to obtain
#' @param normalise normalise to mean of zero
#' @return a data frame
#' @importFrom reshape2 melt
#' @importFrom readr parse_number
#' @importFrom nlme fixef ranef
#' @importFrom brms posterior_samples
#' @import dplyr
#' @export
#' 
get_coefs <- function(fit, var = "area", normalise = TRUE) {
  
  if (nrow(fit$ranef) > 0) {
    # Group-level effects
    # eff <- ranef(fit, groups = var, probs = c(0.05, 0.95))[[1]][,,1] %>%
    #   data.frame() %>%
    #   mutate(variable = rownames(.))
    
    coefs <- posterior_samples(fit, pars = paste0("r_", var)) %>%
      mutate(iteration = 1:n()) %>%
      melt(id.vars = "iteration") %>%
      mutate(variable = paste0(var, parse_number(as.character(.data$variable))))
  } else {
    # Population-level effects
    # eff <- fixef(fit, probs = c(0.05, 0.95)) %>%
    #   data.frame() %>%
    #   mutate(variable = rownames(.)) %>%
    #   filter(grepl(var, variable))
    # pars <- gsub(var, "", eff$variable)
    # vars <- unique(fit$data[,var])
    # e2 <- data.frame(t(rep(0, ncol(eff) - 1)), as.character(paste0(var, vars[!vars %in% pars])))
    # names(e2) <- names(eff)
    # eff <- rbind(e2, eff)
    
    ps <- posterior_samples(fit, pars = var) %>%
      mutate(iteration = 1:n()) %>%
      melt(id.vars = "iteration") %>%
      mutate(variable = paste0(var, parse_number(as.character(.data$variable))))

    if (normalise) {
      # Get the missing variable and normalise
      data <- fit$data
      data[,var] <- paste0(var, data[,var])
      ps0 <- data.frame(iteration = 1:max(ps$iteration),
                        variable = unique(data[,var])[!unique(data[,var]) %in% unique(ps$variable)],
                        value = 0)
      ps1 <- rbind(ps0, ps)
      mean_coefs <- ps1 %>% 
        group_by(.data$iteration) %>% 
        summarise(mean_coef = mean(.data$value))
      coefs <- left_join(ps1, mean_coefs, by = "iteration") %>%
        mutate(value = .data$value - .data$mean_coef)
    } else {
      coefs <- ps
    }
  }
  
  # Arrange by vessel coefficient if vessel chosen
  # if (str_detect(group[2], regex("vessel", ignore_case = TRUE))) {
  #   eff <- eff %>%
  #     arrange(estimate) %>%
  #     mutate(variable = parse_number(as.character(variable)))
  #   data$vessel <- factor(data$vessel, levels = eff$variable)
  #   coefs$variable <- factor(coefs$variable, levels = eff$variable)
  # }
  
  # return(eff)
  return(coefs)
}
