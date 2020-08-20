<<<<<<< HEAD
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
    geom_line(aes(year, cpue, color = model, group = model, linetype = factor(lty))) +
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
    fit <- add_criterion(fit, criterion = "bayes_R2")
    rdf <- data.frame(R2 = fit$criteria$bayes_R2) %>%
      mutate(Model = as.character(fit$formula)[1], Distribution = as.character(fit$family)[1], Link = as.character(fit$family)[2])
    df <- rbind(df, rdf)
  }
  df <- df %>% 
    group_by(Model, Distribution, Link) %>% 
    summarise(R2 = mean(R2))
  df$diff <- c(0, diff(df$R2))
  return(df)
}


=======
>>>>>>> 30b0ad1c4f3de7cadc84c1343f232fa40c9166c5
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
  
  df <- NULL
  df_grey <- NULL
  fout <- list()
  
  for (i in 1:m) {
<<<<<<< HEAD
    fout[[i]] <- get_index(fit = fits[[i]], year = year, probs = probs) %>% mutate(colour = "a")
  }
  
  for (i in 2:m) {
    
  }
  
  
  
  
  #   if (i > 2) {
  #     xx <- fout[[i - 2]] %>% mutate(Model = fout[[i]]$Model, line = i)
  #     df_grey <- rbind(df_grey, xx)
  #   }
  #   if (i > 1) f2 <- fout[[i]] %>% mutate(Q50 = f1$Q50, colour = "b")
  #   f1 <- fout[[i]]
  #   if (i > 1) fout[[i]] <- rbind(fout[[i]], f2)
  #   
  #   df <- rbind(df, fout[[i]])
  # }
  
=======
    # yrs <- sort(unique(fits[[i]]$data[,year]))
    # n <- length(yrs)
    # 
    # # Create newdata for prediction (using fitted)
    # newdata <- fits[[i]]$data %>% slice(rep(1, n))
    # for (j in 1:ncol(newdata)) {
    #   x <- fits[[i]]$data[,j]
    #   newdata[,j] <- ifelse(is.numeric(x), mean(x), NA)
    # }
    # newdata[,year] <- yrs

    fout[[i]] <- get_index(fit = fits[[i]], year = year, probs = probs) %>%
      mutate(colour = "a")
    
    # fout[[i]] <- fitted(object = fits[[i]], newdata = newdata, probs = c(probs[1], 0.5, probs[2])) %>% 
    #   data.frame() %>%
    #   # mutate(Qlower = .data$Qlower / geo_mean(.), Q50 = .data$Q50 / geo_mean(.), Qupper = .data$Qupper / geo_mean(.)) %>%
    #   mutate(model = as.character(fits[[i]]$formula)[1], year = yrs, colour = "a")
    # fout[[i]]$Qlower <- fout[[i]]$Qlower / geo_mean(fout[[i]]$Q50)
    # fout[[i]]$Qupper <- fout[[i]]$Qupper / geo_mean(fout[[i]]$Q50)
    # fout[[i]]$Q50 <- fout[[i]]$Q50 / geo_mean(fout[[i]]$Q50)
    
    if (i > 2) {
      xx <- fout[[i - 2]] %>% mutate(Model = fout[[i]]$Model, line = i)
      df_grey <- rbind(df_grey, xx)
    }
    if (i > 1) f2 <- fout[[i]] %>% mutate(Q50 = f1$Q50, colour = "b")
    f1 <- fout[[i]]
    if (i > 1) fout[[i]] <- rbind(fout[[i]], f2)
    
    df <- rbind(df, fout[[i]])
  }
  
>>>>>>> 30b0ad1c4f3de7cadc84c1343f232fa40c9166c5
  df$colour <- factor(df$colour, levels = c("b", "a"))
  
  p <- ggplot(data = df)
  p <- p + geom_line(data = df_grey, aes(x = .data$Year, y = .data$Q50, group = .data$line), colour = "grey", linetype = "solid")
  if (show_probs) {
    p <- p + geom_ribbon(data = df, aes(x = .data$Year, ymin = .data$Qlower, ymax = .data$Qupper, group = .data$colour, fill = .data$colour), alpha = 0.3, colour = NA)
  }
  p <- p + 
    geom_line(data = df, aes(x = .data$Year, y = .data$Q50, colour = .data$colour, group = .data$colour, linetype = .data$colour)) +
    geom_point(data = df, aes(x = .data$Year, y = .data$Q50, colour = .data$colour, shape = .data$colour)) +
    facet_wrap(Model ~ ., ncol = 1, strip.position = "top") +
    labs(x = NULL, y = "Index") +
    scale_fill_manual(values = c(NA, "black")) +
    scale_colour_manual(values = c("black", "black")) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_shape_manual(values = c(NA, 19)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    theme_bw() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.y = unit(0, "lines"))
  
  return(p)
}
