plot_implied_residuals2 <- function(fit, data = NULL, year = "Year") {
  # Get the data
  if (is.null(data)) {
    data <- fit$data
  }
  
  # Extract predicted values
  idx <- get_index(fit, year = year)
  mean(idx$Estimate)
  idx$Estimate <- idx$Estimate - mean(idx$Estimate)
  mean(idx$Estimate)
  
  # Extract residuals
  resid <- residuals(fit) %>% data.frame()
  # names(resid) <- paste0("resid.", names(resid))
  resid <- cbind(data, resid) %>%
    group_by(.data$Year, .data$Area) %>%
    summarise(residual = mean(.data$Estimate))
  
  ires <- left_join(idx, resid, by = year) %>%
    mutate(implied = .data$Estimate + .data$residual)
  
  p <- ggplot(data = ires, aes(x = .data$Year, y = .data$implied)) +
    geom_line(data = idx, aes(x = .data$Year, y = .data$Estimate), group = 1, colour = "grey") +
    geom_line(group = 1, colour = "purple") +
    labs(x = NULL, y = "Residuals") +
    facet_wrap(.data$Area ~ ., ncol = 2) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}
