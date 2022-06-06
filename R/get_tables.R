#' Get table of various criterion
#' 
#' Calculates various criterion for a list of brms models using the \code{add_criterion} function.
#' 
#' @param fits a list of model fits in the order that you want to compare them
#' @param criterion the criterion to use
#' @param sort to sort the table with the best models at the top
#' @param ... additional parameters passed to \code{add_criterion}
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
#' @import brms
#' @import dplyr
#' @importFrom rstan get_elapsed_time get_num_upars
#' @importFrom lubridate seconds_to_period hour minute second
#' @export
#' 
table_criterion <- function(fits, criterion = c("loo", "loo_R2", "bayes_R2", "log_lik"), sort = TRUE, ...) {
  
  n <- length(fits)
  
  for (i in 1:n) {
    if (!is.brmsfit(fits[[i]])) stop("fit is not an object of class brmsfit.")
  }
  
  # Get the formula, distribution, link function, run time, and number of divergent transitions for each model
  df_names <- NULL
  for (i in 1:n) {
    fit <- fits[[i]]
    
    nuts <- nuts_params(fit)
    divergent <- nuts %>% filter(.data$Parameter %in% "divergent__")
    td <- seconds_to_period(max(rowSums(get_elapsed_time(fit$fit))))

    df <- data.frame(id = i,
                     Model = gsub(".*\\~ ", "", as.character(fit$formula)[1]), 
                     Distribution = as.character(fit$family)[1], 
                     Link = as.character(fit$family)[2],
                     # n_pars = get_num_upars(fit$fit),
                     n_divergent = sum(divergent$Value),
                     max_time = sprintf('%02d:%02d:%02d', hour(td), minute(td), round(second(td))))
    df_names <- rbind(df_names, df)
  }

  # Get the criterion if required
  # for (i in 1:n) {
  #   fits[[i]] <- add_criterion(x = fits[[i]], criterion = criterion, model_name = df_names$id[i], overwrite = TRUE)
  # }
  
  if ("loo" %in% criterion) {
    list_loo <- list()
    for (i in 1:n) {
      list_loo[[i]] <- fits[[i]]$criteria$loo
    }
    df_loo <- data.frame(loo_compare(list_loo))
    df_loo$id <- loo_compare_order(list_loo)
    df_loo$model_name <- find_model_names(list_loo)[df_loo$id]
    df_loo <- df_loo %>%
      relocate(.data$p_loo, .after = .data$se_looic) %>%
      relocate(.data$se_p_loo, .after = .data$p_loo) %>%
      mutate(across(.data$se_diff:.data$se_looic, ~ format(round(.x, digits = 0), nsmall = 0))) %>%
      mutate(across(.data$p_loo:.data$se_p_loo, ~ format(round(.x, digits = 1), nsmall = 1)))
  }
  
  if ("loo_R2" %in% criterion) {
    df_loo_R2 <- NULL
    for (i in 1:n) {
      fit <- fits[[i]]
      df1 <- loo_R2(fit) %>%
        data.frame() %>%
        mutate(id = df_names$id[i]) %>%
        rename(loo_R2 = .data$Estimate, se_loo_R2 = .data$Est.Error, Q2.5_loo_R2 = .data$Q2.5, Q97.5_loo_R2 = .data$Q97.5) %>%
        mutate(across(.data$loo_R2:.data$Q97.5_loo_R2, ~ format(round(.x, digits = 3), nsmall = 3))) %>%
        select(-.data$Q2.5_loo_R2, -.data$Q97.5_loo_R2)
      df_loo_R2 <- rbind(df_loo_R2, df1)
    }
  }

  if ("bayes_R2" %in% criterion) {
    df_bayes_R2 <- NULL
    for (i in 1:n) {
      fit <- fits[[i]]
      df1 <- bayes_R2(fit) %>%
        data.frame() %>%
        mutate(id = df_names$id[i]) %>%
        rename(bayes_R2 = .data$Estimate, se_bayes_R2 = .data$Est.Error, Q2.5_bayes_R2 = .data$Q2.5, Q97.5_bayes_R2 = .data$Q97.5) %>%
        mutate(across(bayes_R2:Q97.5_bayes_R2, ~ format(round(.x, digits = 3), nsmall = 3))) %>%
        select(-.data$Q2.5_bayes_R2, -.data$Q97.5_bayes_R2)
      df_bayes_R2 <- rbind(df_bayes_R2, df1)
    }
  }
  
  # if ("log_lik" %in% criterion) {
  #   df_ll <- NULL
  #   for (i in 1:n) {
  #     fit <- fits[[i]]
  #     ll <- rowSums(log_lik(fit))
  #     df1 <- data.frame(id = df_names$id[i]) %>%
  #       rename(log_lik = mean(ll), se_log_lik = sd(ll))
  #     df_ll <- rbind(df_ll, df1)
  #   }
  # }
  
  # Combine the tables
  df_all <- df_names
  if ("loo" %in% criterion) df_all <- df_all %>% left_join(df_loo, by = "id")
  if ("loo_R2" %in% criterion)  df_all <- df_all %>% left_join(df_loo_R2, by = "id")
  if ("bayes_R2" %in% criterion)  df_all <- df_all %>% left_join(df_bayes_R2, by = "id")
  # if ("log_lik" %in% criterion)  df_all <- df_all %>% left_join(df_ll, by = "id")
  
  # Sort by elpd if wanted
  if (sort && "loo" %in% criterion) {
    df_all <- df_all %>% arrange(-as.numeric(.data$elpd_diff))
  } else {
    df_all <- df_all %>% arrange(.data$id)
  }
  
  df_all <- df_all %>%
    mutate(elpd_diff = format(round(elpd_diff, digits = 0), nsmall = 0)) %>%
    relocate(.data$model_name, .after = id) %>%
    relocate(.data$Distribution, .after = .data$model_name) %>%
    relocate(.data$Link, .after = .data$Distribution) %>%
    relocate(.data$n_divergent, .after = last_col()) %>%
    relocate(.data$max_time, .after = last_col())

  return(df_all)
}


loo_compare_order <- function(loos) {
  tmp <- sapply(loos, function(x) {
    est <- x$estimates
    setNames(c(est), nm = c(rownames(est), paste0("se_", rownames(est))))
  })
  colnames(tmp) <- find_model_names(loos)
  rnms <- rownames(tmp)
  ord <- order(tmp[grep("^elpd", rnms), ], decreasing = TRUE)
  ord
}

find_model_names <- function(x) {
  stopifnot(is.list(x))
  out_names <- character(length(x))
  
  names1 <- names(x)
  names2 <- lapply(x, "attr", "model_name", exact = TRUE)
  names3 <- lapply(x, "[[", "model_name")
  names4 <- paste0("model", seq_along(x))
  
  for (j in seq_along(x)) {
    if (isTRUE(nzchar(names1[j]))) {
      out_names[j] <- names1[j]
    } else if (length(names2[[j]])) {
      out_names[j] <- names2[[j]]
    } else if (length(names3[[j]])) {
      out_names[j] <- names3[[j]]
    } else {
      out_names[j] <- names4[j]
    }
  }
  
  return(out_names)
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
