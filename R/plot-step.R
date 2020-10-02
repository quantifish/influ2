glm_term_table <- function(mod_list) {
  n <- length(mod_list)
  
  Term <- rep("null", n)
  DF <- rep(0, n)
  Deviance <- rep(NA, n)
  AIC <- rep(NA, n)
  r2 <- rep(NA, n)
  Final <- rep(TRUE, n)
  
  for (i in 1:n) {
    Deviance[i] <- round(mod_list[[i]]$deviance, 0)
    AIC[i] <- round(mod_list[[i]]$aic, 0)
    r2[i] <- 1 - (mod_list[[i]]$deviance / mod_list[[1]]$deviance)
    if (i > 1) {
      tt <- as.character(mod_list[[i]]$terms[[3]])
      Term[i] <- tt[length(tt)]
      DF[i] <- (mod_list[[i]]$df.null - mod_list[[i]]$df.residual) - (mod_list[[i - 1]]$df.null - mod_list[[i - 1]]$df.residual)
      # If difference in r2 is greater than 1% then term is accepted into the model
      if ((r2[i] - r2[i - 1]) > 0.01) {
        Final[i] <- TRUE
      } else{
        Final[i] <- FALSE
      }
    }
  }
  return(data.frame(Term, DF, Deviance, AIC, r2 = sprintf("%.3f", round(r2, 3)), Final))
}

glm_step_plot <- function(data, mod_list, ibest = 5) {
  n <- length(mod_list)
  ny <- length(unique(data$year))
  df <- NULL
  for (i in 1:n) {
    if (mod_list[[i]]$family$family == "binomial") {
      cpue = exp(c(0, mod_list[[i]]$coefficients[2:ny])) / (1 + exp(c(0, mod_list[[i]]$coefficients[2:ny])))
      ylab <- "Probability of capture"
    } else {
      cpue <- exp(c(0, mod_list[[i]]$coefficients[2:ny]))
      cpue <- cpue / geo_mean(cpue)
      ylab <- "Relative CPUE"
    }
    df1 <- data.frame(year = as.character(sort(unique(data$year))), 
                      cpue = cpue, 
                      model = as.character(format(mod_list[[i]]$formula)),
                      lty = 2)
    if (i == ibest) df1$lty = 1
    df <- rbind(df, df1)
  }
  
  ggplot(data = df) +
    geom_line(aes(x = .data$year, y = .data$cpue, color = .data$model, group = .data$model, linetype = factor(.data$lty))) +
    labs(x = "Fishing year", y = ylab) +
    # coord_cartesian(ylim = c(0.5, 3)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") +
    guides(linetype = FALSE, color = guide_legend(nrow = 3, byrow = TRUE, title = NULL))
}


#' A Bayesian version of the step-plot
#' 
#' This requires that all steps be run using brms and then provided as a list of model fits.
#' 
#' @param fits a list of model fits in the order that you want to compare them
#' 
#' @importFrom brms add_criterion
#' @import dplyr
#' @export
#' 
get_bayes_R2 <- function(fits) {
  df <- NULL
  for (i in 1:length(fits)) {
    fit <- fits[[i]]
    if (!is.brmsfit(fit)) stop("fit is not an object of class brmsfit.")
    fit <- add_criterion(x = fit, criterion = "bayes_R2")
    rdf <- data.frame(R2 = fit$criteria$bayes_R2) %>%
      mutate(Model = as.character(fit$formula)[1], Distribution = as.character(fit$family)[1], Link = as.character(fit$family)[2])
    df <- rbind(df, rdf)
  }
  df <- df %>% 
    group_by(.data$Model, .data$Distribution, .data$Link) %>% 
    summarise(R2 = mean(.data$R2))
  df$diff <- c(0, diff(df$R2))
  return(df)
}


#' A Bayesian version of the step-plot
#' 
#' This requires that all steps be run using brms and then provided as a list of model fits.
#' 
#' @param fits a list of model fits in the order that you want to compare them
#' @param year the year or time label
#' @param probs the quantiles to plot
#' @param show_probs plot the quantiles or not
#' 
#' @import brms
#' @import ggplot2
#' @import dplyr
#' @export
#' 
plot_step <- function(fits, year = "year", probs = c(0.25, 0.75), show_probs = TRUE) {
  
  m <- length(fits)
  
  fout <- list()
  for (i in 1:m) {
    fout[[i]] <- get_index(fit = fits[[i]], year = year, probs = probs)
  }
  
  df <- NULL
  df_dash <- NULL
  df_grey <- NULL
  for (i in 1:m) {
    df <- rbind(df, fout[[i]])
    if (i > 1) {
      xx <- fout[[i - 1]] %>% mutate(Model = fout[[i]]$Model, line = i)
      df_dash <- rbind(df_dash, xx)
    }
    if (i > 2) {
      xx <- fout[[i - 2]] %>% mutate(Model = fout[[i]]$Model, line = i)
      df_grey <- rbind(df_grey, xx) # bug - these needs to include all prevous models in grey (i.e. if 4 models are provided)
    }
  }
  
  p <- ggplot(data = df) +
    geom_line(data = df_grey, aes(x = .data$Year, y = .data$Q50, group = .data$Model), colour = "grey", linetype = "solid") +
    geom_line(data = df_dash, aes(x = .data$Year, y = .data$Q50, group = 1), colour = "black", linetype = "dashed")
  if (show_probs) {
    p <- p + geom_ribbon(data = df, aes(x = .data$Year, ymin = .data$Qlower, ymax = .data$Qupper, group = 1), alpha = 0.3, colour = NA)
  }
  p <- p + 
    geom_line(data = df, aes(x = .data$Year, y = .data$Q50, group = 1)) +
    geom_point(data = df, aes(x = .data$Year, y = .data$Q50)) +
    facet_wrap(Model ~ ., ncol = 1, strip.position = "top") +
    labs(x = NULL, y = "Index") +
    scale_fill_manual(values = c(NA, "black")) +
    scale_colour_manual(values = c("black", "black")) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_shape_manual(values = c(NA, 19)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    theme_bw() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.y = unit(0, "lines"))
  p
  return(p)
}
