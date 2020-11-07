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


#' Identify the variable type
#' 
#' @param fit An object of class \code{brmsfit}.
#' @param xfocus the x
#' @param hurdle if hurdle or not
#' @return The geometric mean of the vector.
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
#' @export
#' 
id_var_type <- function(fit, xfocus, hurdle = FALSE) {
  
  if (!is.brmsfit(fit)) stop("fit is not an object of class brmsfit.")
  
  if (hurdle) {
    form_split <- str_split(as.character(fit$formula)[2], " \\+ ")[[1]]
    form_var <- form_split[grepl(xfocus, form_split)]
  } else {
    form_split <- str_split(as.character(fit$formula)[1], " \\+ ")[[1]]
    form_var <- form_split[grepl(xfocus, form_split)]
  }

  if (!is.numeric(fit$data[,xfocus]) & !any(grepl("\\(1 \\|", form_var))) {
    type <- "fixed_effect"
  } else if (!is.numeric(fit$data[,xfocus]) & any(grepl("\\(1 \\|", form_var))) {
    type <- "random_effect"
  } else if (is.numeric(fit$data[,xfocus]) & any(grepl("poly\\(", form_var))) {
    type <- "polynomial"
  } else if (is.numeric(fit$data[,xfocus]) & any(grepl("s\\(", form_var))) {
    type <- "spline"
  } else if (is.numeric(fit$data[,xfocus]) & any(grepl("t2\\(", form_var))) {
    type <- "spline"
  } else if (is.numeric(fit$data[,xfocus])) {
    type <- "linear"
  }
  
  return(type)
}


#' Geometric mean
#' 
#' @param a a vector.
#' @return The geometric mean of the vector.
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
#' @export
#' 
geo_mean <- function(a) {
  prod(a)^(1.0 / length(a))
}


#' Inverse logit
#' 
#' @param a a vector.
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' @export
#' 
inv_logit <- function(a) {
  exp(a) / (exp(a) + 1)
}


#' logit
#' 
#' @param p a vector.
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' @export
#' 
logit <- function(p) {
  log(p / (1 - p))
}
