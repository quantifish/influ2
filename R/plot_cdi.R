# Bayesian version of the CDI plot
#
#
plot_bayesian_cdi <- function(fit,
                              group = c("fishing_year", "area"),
                              xlab = "Month", ylab = "Fishing year", colour = "purple") {

  # If it is a random-effect then "try" ranef, else use fixef
  is_ranef <- TRUE
  eff <- try(
    ranef(fit, groups = group[2], probs = c(0.05, 0.95))[[1]][,,1] %>%
      data.frame() %>%
      rename(estimate = Estimate, lower = Q5, upper = Q95) %>%
      mutate(variable = rownames(.)),
    silent = TRUE
  )
  if (class(eff) == "try-error") {
    eff <- fixef(fit, probs = c(0.05, 0.95)) %>%
      data.frame() %>%
      rename(estimate = Estimate, lower = Q5, upper = Q95) %>%
      mutate(variable = rownames(.)) %>%
      filter(grepl(group[2], variable))
    pars <- gsub(group[2], "", eff$variable)
    vars <- unique(fit$data[,group[2]])
    e2 <- data.frame(t(rep(0, ncol(eff)-1)), as.character(paste0(group[2], vars[!vars %in% pars])))
    names(e2) <- names(eff)
    eff <- rbind(e2, eff)
    is_ranef <- FALSE
  }
  # head(eff)


  # Model data
  data <- fit$data %>%
    mutate_at(vars(matches(group[2])), factor) %>%
    mutate(id = 1:n())
  #class(data$vessel)
  head(data)


  if (is_ranef) {
    ps <- posterior_samples(fit, pars = paste0("r_", group[2])) %>%
      mutate(iteration = 1:n()) %>%
      melt(id.vars = "iteration") %>%
      mutate(variable = parse_number(as.character(variable)))
    n_iterations <- max(ps$iteration)
    coefs <- ps
    X <- model.matrix(as.formula(paste0("cpue ~ 0 + ", group[2])), data = data)
    ylab1 <- "Coefficient"
  } else {
    ps <- posterior_samples(fit, pars = group[2]) %>%
      mutate(iteration = 1:n()) %>%
      melt(id.vars = "iteration") %>%
      mutate(variable = parse_number(as.character(variable)))
    n_iterations <- max(ps$iteration)
    # Find the missing variable
    ps1 <- rbind(data.frame(iteration = 1:n_iterations,
                           variable = unique(data[,group[2]])[!unique(data[,group[2]]) %in% unique(ps$variable)],
                           value = 0), ps)
    mean_coefs <- ps1 %>% group_by(iteration) %>% summarise(mean_coef = mean(value))
    coefs <- left_join(ps1, mean_coefs, by = "iteration") %>%
      mutate(value = value - mean_coef)
    X <- model.matrix(as.formula(paste0("cpue ~ ", group[2])), data = data)
    ylab1 <- "Relative coefficient"
  }
  tail(coefs)
  dim(coefs)


  # Arrange by vessel coefficient if vessel chosen
  if (str_detect(group[2], regex("vessel", ignore_case = TRUE))) {
    eff <- eff %>%
      arrange(estimate) %>%
      mutate(variable = parse_number(as.character(variable)))
    data$vessel <- factor(data$vessel, levels = eff$variable)
    coefs$variable <- factor(coefs$variable, levels = eff$variable)
  }


  # Do the matrix multiplication
  Xbeta <- matrix(NA, nrow = n_iterations, ncol = nrow(fit$data))
  for (i in 1:n_iterations) {
    Xbeta[i,] <- X %*% filter(coefs, iteration == i)$value
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


  # Two methods for extracting the legend on its own
  g1 <- function(a.gplot){
    if (!gtable::is.gtable(a.gplot))
      a.gplot <- ggplotGrob(a.gplot)
    leg <- which(sapply(a.gplot$grobs, function(x) x$name) == "guide-box")
    a.gplot$grobs[[leg]]
  }
  g2 <- function(a.gplot){
    if (!gtable::is.gtable(a.gplot))
      a.gplot <- ggplotGrob(a.gplot)
    gtable::gtable_filter(a.gplot, 'guide-box', fixed=TRUE)
  }


  # Build the plot
  sp <- 0.05
  p1 <- ggplot(coefs, aes(x = factor(variable), y = exp(value))) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_violin(colour = colour, fill = colour, alpha = 0.5, draw_quantiles = 0.5, scale = "width") +
    labs(x = NULL, y = ylab1) +
    scale_x_discrete(position = "top") +
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), plot.margin = margin(b = sp, r = sp, unit = "cm"))
  p3a <- plot_bubble(df = data, group = group, xlab = xlab, ylab = ylab, blab = "", fill = fill)
  p2 <- g1(p3a)
  p3 <- p3a +
    theme(legend.position = "none", plot.margin = margin(t = sp, r = sp, unit = "cm"), axis.text.x = element_text(angle = 45, hjust = 1))
  # p2 <- ggplot() +
  #   geom_blank() +
  #   theme_void()
  p4 <- ggplot(data = influ_delta, aes_string(x = as.character(group[1]))) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_violin(aes(y = exp(delta)), colour = colour, fill = colour, alpha = 0.5, draw_quantiles = 0.5, scale = "width") +
    coord_flip() +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = "Influence") +
    theme_bw() +
    theme(legend.position = "none", plot.margin = margin(t = sp, l = sp, unit = "cm"))
  p1 + p2 + p3 + p4 + plot_layout(nrow = 2, ncol = 2, heights = c(1, 2), widths = c(2, 1))
}
