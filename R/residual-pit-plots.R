# Panel selection and the optional bayesplot bridge operate only on saved
# diagnostics. Neither layer calls the simulation adapters or changes the fit.
.resid_select_panels <- function(x, panels, response_diagnostic) {
  if (is.null(panels)) panels <- c("qq", "fitted", "year", "auto")
  choices <- c("qq", "fitted", "year", "distribution", "calibration",
    "calibration_groups", "pit_ecdf", "pit_ecdf_diff", "auto")
  if (!is.character(panels) || !is.null(dim(panels)) || length(panels) != 4L ||
      anyNA(panels) || any(!panels %in% choices)) {
    stop("`panels` must contain exactly four names chosen from: ",
      paste(choices, collapse = ", "), ".", call. = FALSE)
  }
  if ("auto" %in% panels) panels[panels == "auto"] <- .resid_response_panel(x, response_diagnostic)
  unname(panels)
}

.resid_panel_caption <- function(x, panels) {
  normal <- panels %in% c("qq", "fitted", "year")
  lines <- character()
  if (any(normal)) {
    positions <- LETTERS[which(normal)]
    label <- if (identical(positions, c("A", "B", "C"))) "Panels A-C" else
      paste(if (length(positions) == 1L) "Panel" else "Panels", paste(positions, collapse = ", "))
    lines <- paste0(label, ": simulation-based PIT residuals on the normal scale (qnorm(PIT)).")
  }
  descriptions <- c(distribution = "observed versus simulated response ECDF.",
    calibration = "response probability calibration.",
    calibration_groups = "grouped observed-minus-predicted response proportions.",
    pit_ecdf = "PIT ECDF on the uniform scale.",
    pit_ecdf_diff = "PIT ECDF minus the uniform reference.")
  for (i in which(!normal)) {
    lines <- c(lines, paste0("Panel ", LETTERS[i], ": ", descriptions[[panels[i]]]))
  }
  paste(c(lines, paste(x$metadata$backend, "|", x$metadata$nsim,
    "simulations |", x$metadata$scheme)), collapse = "\n")
}

.resid_bayesplot_available <- function() {
  requireNamespace("bayesplot", quietly = TRUE) &&
    utils::packageVersion("bayesplot") >= "1.16.0"
}

.plot_residual_pit <- function(x, difference, grid_size) {
  .resid_integer(grid_size, "pit_grid_size", 2L)
  if (grid_size > 1000L) stop("`pit_grid_size` must be at most 1000.", call. = FALSE)
  pit <- x$observations$pit
  if (!is.numeric(pit) || !is.null(dim(pit)) || length(pit) < 3L ||
      length(pit) != nrow(x$observations) || any(!is.finite(pit) | pit < 0 | pit > 1)) {
    stop("PIT plots require one finite stored `observations$pit` value in [0, 1] per observation; recalculate the diagnostic if these are missing.", call. = FALSE)
  }
  level <- x$metadata$level
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1) {
    stop("PIT plots require a stored `metadata$level` strictly between zero and one.", call. = FALSE)
  }
  if (!.resid_bayesplot_available()) {
    stop("PIT ECDF plots require optional package 'bayesplot' >= 1.16.0. Install or update it to use these panels; the default overview does not require it.", call. = FALSE)
  }
  p <- bayesplot::ppc_pit_ecdf(pit = pit, K = grid_size, prob = level,
    plot_diff = difference, method = "independent", interpolate_adj = FALSE)
  reference <- if (difference) ggplot2::geom_hline(yintercept = 0,
    colour = "grey40", linetype = 3) else ggplot2::geom_abline(intercept = 0,
    slope = 1, colour = "grey40", linetype = 3)
  p <- p + reference + ggplot2::theme_get() + ggplot2::labs(
    title = if (difference) "PIT ECDF difference" else "PIT distribution check",
    subtitle = paste0(format(100 * level, trim = TRUE),
      "% simultaneous iid-uniform reference\nExploratory fitted-data ranks; not a calibrated test"),
    x = "Simulation-based PIT value", y = if (difference) "PIT ECDF minus uniform CDF" else "PIT cumulative probability")
  attr(p, "pit_reference") <- list(method = "independent", interpolated = FALSE,
    level = level, grid_size = grid_size, difference = difference)
  p
}
