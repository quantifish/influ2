# Only result inspection belongs here: the child session sources this file,
# but never receives a fitted model, reference grid, or fixture constructor.
saved_plot_signature <- function(p) {
  if (inherits(p, "patchwork")) {
    return(list(
      class = class(p),
      layout = p$patches$layout,
      annotation = p$patches$annotation,
      panels = lapply(seq_len(length(p)), function(i) saved_plot_signature(p[[i]]))
    ))
  }
  built <- ggplot2::ggplot_build(p)
  list(
    class = class(p),
    data = built$data,
    labels = as.list(built$plot$labels),
    facets = built$layout$layout,
    ranges = lapply(built$layout$panel_params, function(panel) {
      list(x = panel$x.range, y = panel$y.range,
        x_labels = if (is.function(panel$x$get_labels)) panel$x$get_labels() else NULL,
        y_labels = if (is.function(panel$y$get_labels)) panel$y$get_labels() else NULL)
    })
  )
}

saved_result_view <- function(x) {
  if (inherits(x, "influ_diag")) {
    tables <- list(effects = influ2::influ_effects(x),
      composition = influ2::influ_composition(x), indices = influ2::influ_indices(x),
      metrics = influ2::influ_metrics(x), summary = summary(x))
    plots <- list(influence = plot(x), index = plot(x, type = "index"),
      cdi = plot(x, type = "cdi"))
  } else if (inherits(x, "influ_residuals")) {
    tables <- x[c("observations", "qq", "ecdf", "observed_ecdf", "calibration", "groups")]
    plots <- list(overview = plot(x), qq = plot(x, type = "qq"),
      distribution = plot(x, type = "distribution"))
  } else if (inherits(x, "influ_index")) {
    tables <- list(index = as.data.frame(x), reporting = influ2::index_table(x),
      covariance = influ2::index_vcov(x, "response"),
      log_covariance = influ2::index_vcov(x))
    plots <- list(index = influ2::plot_index(x),
      correlation = influ2::plot_index(x, type = "correlation"))
  } else if (inherits(x, "influ_steps")) {
    tables <- list(indices = influ2::influ_indices(x), summary = summary(x))
    plots <- list(steps = influ2::plot_step(x))
  } else stop("Unexpected saved-result class")
  signatures <- lapply(plots, saved_plot_signature)
  # Actually draw composite layouts and legends, not just their ggplot layers.
  for (p in plots) print(p)
  list(class = class(x), tables = tables,
    printed = capture.output(print(x)), plots = signatures)
}

saved_result_has_live_state <- function(x) {
  if (is.environment(x) || is.function(x) ||
      typeof(x) %in% c("externalptr", "weakref") || inherits(x, "connection")) {
    return(TRUE)
  }
  # Inspect attributes too: a formula can hide its fitting environment there.
  children <- c(if (is.list(x) || is.pairlist(x) || is.call(x) || is.expression(x))
    as.list(x), attributes(x))
  any(vapply(children, saved_result_has_live_state, logical(1)))
}
