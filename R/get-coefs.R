#' Get model coefficients new version
#' 
#' @param fit An object of class \code{brmsfit}.
#' @param var The variable to obtain.
#' @return A \code{data.frame}.
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
#' @importFrom reshape2 melt
#' @importFrom readr parse_number
#' @importFrom nlme fixef ranef
#' @importFrom brms posterior_samples
#' @import dplyr
#' @export
#' 
get_marginal <- function(fit, var = "area") {
  
  if (!is.brmsfit(fit)) stop("fit is not an object of class brmsfit.")
  
  # Create newdata for prediction (using fitted)
  data <- fit$data
  if (!is.numeric(data[,var])) {
    levs <- unique(data[,var])
    n <- length(levs)
  } else {
    levs <- seq(min(data[,var]), max(data[,var]), length.out = 100)
    n <- length(levs)
  }
  newdata <- data %>% slice(rep(1, n))
  for (j in 1:ncol(newdata)) {
    x <- data[,j]
    newdata[,j] <- ifelse(is.numeric(x), mean(x), NA)
  }
  newdata[,var] <- levs    
  newdata$id <- 1:n
  
  # Posterior samples of coefficients
  coefs <- posterior_epred(object = fit, newdata = newdata) %>%
    melt(varnames = c("iteration", "id")) %>%
    left_join(newdata, by = "id") %>%
    rename(variable = var)

  return(coefs)
}


#' Get raw coefficients
#' 
#' @param fit An object of class \code{brmsfit}.
#' @param var The variable to obtain.
#' @return A \code{data.frame}.
#' 
#' @importFrom reshape2 melt
#' @importFrom brms posterior_samples
#' @import dplyr
#' @export
#' 
get_coefs_raw <- function(fit, var = "area") {
  
  if (!is.brmsfit(fit)) stop("fit is not an object of class brmsfit.")
  
  if (nrow(fit$ranef) > 0) {
    ps <- posterior_samples(fit, pars = paste0("r_", var)) %>%
      mutate(iteration = 1:n()) %>%
      melt(id.vars = "iteration") %>%
      mutate(variable = gsub(".*\\[|\\]", "", .data$variable)) %>%
      mutate(variable = gsub(",Intercept", "", .data$variable))
  } else {
    ps <- posterior_samples(fit, pars = var) %>%
      mutate(iteration = 1:n()) %>%
      melt(id.vars = "iteration") %>%
      mutate(variable = gsub("b_", "", .data$variable))
  }
  
  # Get the missing variable and normalise
  # if (nrow(fit$ranef) == 0 & normalise & !is_poly & length(unique(ps$variable)) != 1) {
  #   data <- fit$data
  #   data[,var] <- paste0(var, data[,var])
  #   ps0 <- data.frame(iteration = 1:max(ps$iteration),
  #                     variable = unique(data[,var])[!unique(data[,var]) %in% unique(ps$variable)],
  #                     value = 0)
  #   ps1 <- rbind(ps0, ps)
  #   mean_coefs <- ps1 %>% 
  #     group_by(.data$iteration) %>% 
  #     summarise(mean_coef = mean(.data$value))
  #   coefs <- left_join(ps1, mean_coefs, by = "iteration") %>%
  #     mutate(value = .data$value - .data$mean_coef) %>%
  #     select(-.data$mean_coef)
  # } else if (nrow(fit$ranef) == 0 & !normalise & !is_poly & length(unique(ps$variable)) != 1) {
  #   data <- fit$data
  #   data[,var] <- paste0(var, data[,var])
  #   ps0 <- data.frame(iteration = 1:max(ps$iteration),
  #                     variable = unique(data[,var])[!unique(data[,var]) %in% unique(ps$variable)],
  #                     value = 0)
  #   coefs <- rbind(ps0, ps)
  # } else {
    coefs <- ps
  # }
  
  return(coefs)
}


#' Get model coefficients
#' 
#' @param fit An object of class \code{brmsfit}.
#' @param var The variable to obtain.
#' @param normalise Normalise to have a mean of zero.
#' @param hurdle If a hurdle model then use the hurdle.
#' @param transform if the coefficients should be transformed using the link function.
#' @return A \code{data.frame}.
#' 
#' @importFrom reshape2 melt
#' @importFrom readr parse_number
#' @importFrom nlme fixef ranef
#' @importFrom brms posterior_samples
#' @import dplyr
#' @export
#' 
get_coefs <- function(fit, var = "area", normalise = TRUE, hurdle = FALSE, transform = FALSE) {
  
  if (!is.brmsfit(fit)) stop("fit is not an object of class brmsfit.")
  
  type <- id_var_type(fit = fit, xfocus = var, hurdle = hurdle)
  
  is_poly <- FALSE
  
  if (any(grepl("\\(1 \\|", var))) {
    # Group-level effects
    # eff <- ranef(fit, groups = var, probs = c(0.05, 0.95))[[1]][,,1] %>%
    #   data.frame() %>%
    #   mutate(variable = rownames(.))
    
    ps <- posterior_samples(fit, pars = paste0("r_", var)) %>%
      mutate(iteration = 1:n()) %>%
      melt(id.vars = "iteration") %>%
      mutate(variable = gsub(".*\\[|\\]", "", .data$variable)) %>%
      mutate(variable = gsub(",Intercept", "", .data$variable))
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
      mutate(variable = gsub("b_", "", .data$variable)) %>%
      mutate(variable = gsub("r_", "", .data$variable)) %>%
      filter(!str_detect(variable, "sd_"))
      #mutate(variable = paste0(var, gregexpr("[[:digit:]]+", .data$variable)))
      #mutate(variable = paste0(var, parse_number(as.character(.data$variable))))
    unique(ps$variable)
    #head(ps)
  }
  
  # Check to see if this is a polynomial
  if (any(grepl("poly", ps$variable))) {
    # ps <- ps %>%
    #   mutate(variable = gsub("poly", "", .data$variable))
    is_poly <- TRUE
    # order_poly <- unique(sub(".*?(\\d).*", "\\1", ps$variable))
  }
  
  # If it is a hurdle model then choose whether to plot the hurdle component or the positive distribution component
  if (any(grepl("hu", ps$variable))) {
    if (hurdle) {
      ps <- ps %>%
        filter(grepl("hu", .data$variable)) %>%
        mutate(variable = gsub("hu_", "", .data$variable))
    } else {
      ps <- ps %>%
        filter(!grepl("hu", .data$variable))
    }
  }
  
  # Get the missing variable and normalise
  if (type == "random_effect") {
    coefs <- ps
  } else if (!any(grepl("\\(1 \\|", var)) & normalise & !is_poly & length(unique(ps$variable)) != 1) {
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
      mutate(value = .data$value - .data$mean_coef) %>%
      select(-.data$mean_coef)
  } else if (nrow(fit$ranef) == 0 & !normalise & !is_poly & length(unique(ps$variable)) != 1) {
    data <- fit$data
    data[,var] <- paste0(var, data[,var])
    ps0 <- data.frame(iteration = 1:max(ps$iteration),
                      variable = unique(data[,var])[!unique(data[,var]) %in% unique(ps$variable)],
                      value = 0)
    coefs <- rbind(ps0, ps)
  } else {
    coefs <- ps
  }

  # Arrange by vessel coefficient if vessel chosen
  # if (str_detect(group[2], regex("vessel", ignore_case = TRUE))) {
  #   eff <- eff %>%
  #     arrange(estimate) %>%
  #     mutate(variable = parse_number(as.character(variable)))
  #   data$vessel <- factor(data$vessel, levels = eff$variable)
  #   coefs$variable <- factor(coefs$variable, levels = eff$variable)
  # }
  
  # Transform the coefficients using the link function
  # if (transform & !is_poly) {
  #   if (fit$family$family %in% c("lognormal", "hurdle_lognormal")) {
  #     if (fit$family$link == "identity") {
  #       coefs <- coefs %>% mutate(value = exp(.data$value))
  #     } else {
  #       stop("This link function for the lognormal family has not been coded in influ2 yet - please update the plot-cdi.R function.")
  #     }
  #   } else if (fit$family$family %in% c("gamma", "hurdle_gamma")) {
  #     if (fit$family$link == "inverse") {
  #       coefs <- coefs %>% mutate(value = 1.0 / .data$value)
  #     } else if (fit$family$link == "identity") {
  #       coefs <- coefs %>% mutate(value = .data$value)
  #     } else if (fit$family$link == "log") {
  #       coefs <- coefs %>% mutate(value = exp(.data$value))
  #     }
  #   } else if (fit$family$family %in% c("bernoulli")) {
  #     if (fit$family$link == "logit") {
  #       coefs <- coefs %>% mutate(value = exp(.data$value) / (1.0 + exp(.data$value)))
  #     }
  #   } else {
  #     stop("This family has not been coded in influ2 yet - please update the plot-cdi.R function.")
  #   }
  # }
  
  return(coefs)
}
