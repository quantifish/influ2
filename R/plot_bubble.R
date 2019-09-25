# Bubble plot
#
plot_bubble <- function(df, group = c("fishing_year", "vessel"), fill = "purple", alpha = 0.5, ylab = NULL, xlab = NULL, blab = "N") {

  for (i in 2:ncol(df)) {
    df[,i] <- factor(df[,i])
  }

  if (fill %in% names(df)) {
    group <- c(group, fill)
    df1 <- df %>%
      group_by(.dots = group) %>%
      summarise(size = n()) %>%
      mutate(size = ifelse(size == 0, NA, size))

    p <- ggplot(data = df1, aes_string(x = factor(group[2]), y = group[1], fill = fill, colour = fill)) +
      geom_point(aes(size = size), alpha = alpha, shape = 16) +
      geom_point(aes(size = size), shape = 1)
  } else {
    df1 <- df %>%
      group_by(.dots = group) %>%
      summarise(size = n()) %>%
      mutate(size = ifelse(size == 0, NA, size))

    p <- ggplot(data = df1, aes_string(x = as.character(group[2]), y = group[1])) +
      geom_point(aes(size = size), alpha = alpha, shape = 16, colour = fill) +
      geom_point(aes(size = size), shape = 1, colour = fill)
  }

  p <- p + labs(x = xlab, y = ylab, size = blab) +
    theme_bw() +
    scale_size(range = c(0, 10)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(p)
}
