#' Get table of various criterion
#' 
#' Calculates various criterion for a list of brms models using the \code{add_criterion} function.
#' 
#' @param fits a list of model fits in the order that you want to compare them
#' @param ... additional parameters passed to \code{add_criterion}
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
#' @import brms
#' @import dplyr
#' @export
#' 
table_criterion <- function(fits, ...) {
  
  n <- length(fits)
  
  df_names <- NULL
  for (i in 1:n) {
    fit <- fits[[i]]
    df <- data.frame(id = i,
                     Model = as.character(fit$formula)[1], 
                     Distribution = as.character(fit$family)[1], 
                     Link = as.character(fit$family)[2])
    df_names <- rbind(df_names, df)
  }
  
  df_loo <- list()
  for (i in 1:n) {
    if (!is.brmsfit(fits[[i]])) stop("fit is not an object of class brmsfit.")
    fits[[i]] <- add_criterion(x = fits[[i]], criterion = c("bayes_R2", "loo"), model_name = df_names$id[i], ...)
    df_loo[[i]] <- fits[[i]]$criteria$loo
  }
  lout <- data.frame(loo_compare(df_loo))
  lout$id <- as.integer(rownames(lout))

  df_r2 <- NULL
  for (i in 1:n) {
    fit <- fits[[i]]
    df1 <- data.frame(R2 = fit$criteria$bayes_R2) %>%
      mutate(id = df_names$id[i])
    df_r2 <- rbind(df_r2, df1)
  }
  
  df_all <- df_r2 %>% 
    left_join(df_names, by = "id") %>%
    group_by(.data$id, .data$Model, .data$Distribution, .data$Link) %>% 
    summarise(R2 = mean(.data$R2)) %>%
    left_join(lout, by = "id") %>%
    arrange(-.data$elpd_diff)
    # mutate(R2_diff = c(0, diff(R2))) %>%
  df_all$R2_diff <- c(0, diff(df_all$R2))
  df_all <- df_all %>%
    relocate(.data$R2_diff, .after = .data$R2) %>%
    ungroup() %>%
    select(-.data$id)
  
  return(df_all)
}


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
