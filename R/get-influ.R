#' Get the influence metric
#' 
#' @param fit An object of class \code{brmsfit}.
#' @param group the variable to obtain
#' @param hurdle if a hurdle model then use the hurdle
#' @return a \code{data.frame}.
#' @importFrom reshape2 melt
#' @importFrom readr parse_number
#' @importFrom stringr str_split str_detect
#' @importFrom stats as.formula model.matrix quantile
#' @import dplyr
#' @export
#' 
get_influ2 <- function(fit, 
                       group = c("fishing_year", "area"), 
                       hurdle = FALSE) {
  
  if (!is.brmsfit(fit)) stop("fit is not an object of class brmsfit.")
  
  # Model data
  data <- fit$data %>%
    mutate(id = 1:n())
  y <- names(data)[1]
  
  # Identify the type of variable we are dealing with
  type <- id_var_type(fit = fit, xfocus = group[2], hurdle = hurdle)
  
  # Posterior of coefficients
  if (type == "fixed_effect") {
    # coefs <- get_marginal(fit = fit, var = group[2])
    # mean_coefs <- coefs %>% 
    #   group_by(.data$iteration) %>% 
    #   summarise(mean_coef = geo_mean(.data$value))
    # coefs <- left_join(coefs, mean_coefs, by = "iteration") %>%
    #   mutate(value = .data$value / .data$mean_coef) %>%
    #   select(-.data$mean_coef)
    # xxx <- coefs %>% group_by(variable) %>% summarise(value = mean(value))
    # geo_mean(xxx$value)
    coefs <- get_coefs(fit = fit, var = group[2], hurdle = hurdle)
  } else {
    coefs <- get_coefs_raw(fit = fit, var = group[2])
  }
  
  if (type == "random_effect") {
    X <- model.matrix(as.formula(paste0(y, " ~ 0 + ", group[2])), data = data)
  } else if (type == "linear") {
    X <- model.matrix(as.formula(paste0(y, " ~ 0 + ", group[2])), data = data)
  } else if (type == "fixed_effect") {
    X <- model.matrix(as.formula(paste0(y, " ~ 0 + ", group[2])), data = data)
  } else if (type == "polynomial") {
    X <- model.matrix(as.formula(paste0(y, " ~ 0 + poly(", group[2], ", 3)")), data = data)
    colnames(X) <- gsub("[^[:alnum:]]", "", colnames(X))
  } else if (type == "spline") {
    # m <- gam(as.formula(fit$formula), data = data)
    # model.matrix(~ ns(Duration, 5), data = data) %>% head()
    # model.matrix(~ s(Duration, 5), data = data) %>% head()
    # smooth.construct(object = s(Duration, k = 10), data = data, knots = NULL)
    # parse_bf(as.formula(fit$formula))
    sdata <- standata(fit)
    X <- sdata$Xs
    Z <- sdata$Zs_1_1
    s_coefs <- coefs %>%
      filter(!str_detect(.data$variable, "bs_|sds_"))
    coefs <- coefs %>%
      filter(str_detect(.data$variable, "bs_"))
  }

  # Do the matrix multiplication
  n_iterations <- max(coefs$iteration)
  Xbeta <- matrix(NA, nrow = n_iterations, ncol = nrow(data))
  for (i in 1:n_iterations) {
    Xbeta[i,] <- X %*% filter(coefs, .data$iteration == i)$value
  }
  
  # Another matrix multiplication if there is a spline
  if (type == "spline") {
    Xbeta2 <- matrix(NA, nrow = n_iterations, ncol = nrow(data))
    for (i in 1:n_iterations) {
      Xbeta2[i,] <- Z %*% filter(s_coefs, .data$iteration == i)$value
    }
    Xbeta <- Xbeta + Xbeta2
  }
  
  influ_var <- melt(Xbeta) %>%
    rename(iteration = .data$Var1, id = .data$Var2) %>%
    left_join(data, by = "id")
  influ_rho <- influ_var %>%
    group_by(.data$iteration) %>%
    summarise(rho = mean(.data$value))
  influ_delta <- left_join(influ_var, influ_rho, by = "iteration") %>%
    group_by_at(all_of(c("iteration", group[1]))) %>%
    summarise(delta = mean(.data$value - .data$rho))
  # influ <- influ_delta %>%
  #   group_by(.dots = group[1]) %>%
  #   summarise(estimate = mean(exp(.data$delta)), lower = quantile(exp(.data$delta), probs = 0.05), upper = quantile(exp(.data$delta), probs = 0.95))
  
  return(influ_delta)
}


#' Get the influence metric
#' 
#' @param fit a model fit
#' @param group the variable to obtain
#' @param hurdle if a hurdle model then use the hurdle
#' @return a \code{data.frame}
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
#' @importFrom reshape2 melt
#' @importFrom readr parse_number
#' @importFrom stats as.formula model.matrix quantile
#' @import dplyr
#' @export
#' 
get_influ <- function(fit, group = c("fishing_year", "area"), hurdle = FALSE) {
  
  if (!is.brmsfit(fit)) stop("fit is not an object of class brmsfit.")
  
  # Posterior of coefficients
  coefs <- get_coefs(fit = fit, var = group[2], normalise = FALSE, hurdle = hurdle)
  
  # Model data
  n_iterations <- max(coefs$iteration)
  is_poly <- FALSE
  if (any(grepl("poly", coefs$variable)) | length(unique(coefs$variable)) == 1) {
    data <- fit$data %>%
      mutate(id = 1:n())
    y <- names(data)[1]
    if (any(grepl("poly", coefs$variable))) is_poly <- TRUE
  } else {
    data <- fit$data %>%
      mutate_at(vars(matches(group[2])), factor) %>%
      mutate(id = 1:n())
    y <- names(data)[1]
  }
  
  if (nrow(fit$ranef) > 0 & !is_poly) {
    X <- model.matrix(as.formula(paste0(y, " ~ 0 + ", group[2])), data = data)
  } else if (!is_poly & length(unique(coefs$variable)) != 1) {
    X <- model.matrix(as.formula(paste0(y, " ~ ", group[2])), data = data)
  } else if (!is_poly) {
    X <- model.matrix(as.formula(paste0(y, " ~ 0 + ", group[2])), data = data)
  } else {
    X <- model.matrix(as.formula(paste0(y, " ~ 0 + poly(", group[2], ", 3)")), data = data)
    colnames(X) <- gsub("[^[:alnum:]]", "", colnames(X))
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
    group_by_at(all_of(c("iteration", group[1]))) %>%
    summarise(delta = mean(.data$value - .data$rho))
  # influ <- influ_delta %>%
  #   group_by(.dots = group[1]) %>%
  #   summarise(estimate = mean(exp(.data$delta)), lower = quantile(exp(.data$delta), probs = 0.05), upper = quantile(exp(.data$delta), probs = 0.95))
  
  return(influ_delta)
}
