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

# Keep bayesplot's public independent-reference calculation, but align its
# limits to the grid used by that calculation. N09 identified a mismatch in
# 1.16.0: limits for (1:K)/K were drawn on seq(0, 1, length.out = K).
# Do not guess about future layouts, or shift already aligned limits twice.
.resid_align_pit_plot <- function(p, pit, grid_size, difference,
    version = as.character(utils::packageVersion("bayesplot"))) {
  unsupported <- function() stop("Cannot safely align the PIT reference from bayesplot ",
    version, ". Its plot layout or reference grid is not recognised; please report this to influ2. The default overview is unaffected.", call. = FALSE)
  d <- ggplot2::ggplot_build(p)$data
  same <- function(a, b) isTRUE(all.equal(a, b, tolerance = 1e-12, check.attributes = FALSE))
  if (length(d) != 3L || length(p$layers) != 3L ||
      !all(vapply(p$layers, inherits, logical(1), "Layer")) ||
      !all(vapply(p$layers, function(layer) inherits(layer$geom, "GeomStep"), logical(1))) ||
      !all(vapply(d, function(z) is.numeric(z$x) && is.numeric(z$y) &&
        length(z$x) > 0L && all(is.finite(z$x)) && all(is.finite(z$y)), logical(1)))) unsupported()
  native_grid <- d[[1]]$x
  if (!all(vapply(d, function(z) same(z$x, native_grid), logical(1))) ||
      !same(d[[3]]$y, stats::ecdf(pit)(native_grid))) unsupported()
  upper <- d[[1]]$y
  lower <- d[[2]]$y
  if (any(lower < 0 | upper > 1 | lower > upper) ||
      any(diff(lower) < 0 | diff(upper) < 0) ||
      !same(utils::tail(lower, 1L), 1) || !same(utils::tail(upper, 1L), 1) ||
      !same(c(lower, upper) * length(pit), round(c(lower, upper) * length(pit)))) unsupported()
  grid <- (0:grid_size) / grid_size
  alignment <- "already_aligned"
  if (same(native_grid, grid)) {
    if (!same(c(lower[1L], upper[1L]), c(0, 0))) unsupported()
  } else {
    if (same(native_grid, seq(0, 1, length.out = grid_size)) &&
        identical(version, "1.16.0")) {
      alignment <- "corrected_1.16.0"
    } else if (!same(native_grid, grid[-1L])) unsupported()
    lower <- c(0, lower)
    upper <- c(0, upper)
  }
  # The zero-endpoint limits are exactly zero under the continuous uniform
  # reference. Keep any observed PIT mass at zero: never jitter or discard it.
  values <- list(upper, lower, stats::ecdf(pit)(grid))
  for (i in seq_along(values)) {
    p$layers[[i]]$data <- data.frame(x = grid, y = values[[i]] - difference * grid)
    p$layers[[i]]$mapping$x <- ggplot2::aes(x = .data$x)$x
    p$layers[[i]]$mapping$y <- ggplot2::aes(y = .data$y)$y
  }
  attr(p, "pit_alignment") <- list(bayesplot_version = version, alignment = alignment)
  p
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
    plot_diff = FALSE, method = "independent", interpolate_adj = FALSE)
  p <- .resid_align_pit_plot(p, pit, grid_size, difference)
  alignment <- attr(p, "pit_alignment")
  reference <- if (difference) ggplot2::geom_hline(yintercept = 0,
    colour = "grey40", linetype = 3) else ggplot2::geom_abline(intercept = 0,
    slope = 1, colour = "grey40", linetype = 3)
  p <- p + reference + ggplot2::theme_get() + ggplot2::labs(
    title = if (difference) "PIT ECDF difference" else "PIT distribution check",
    subtitle = paste0(format(100 * level, trim = TRUE),
      "% simultaneous iid-uniform reference\nExploratory fitted-data ranks; not a calibrated test"),
    x = "Simulation-based PIT value", y = if (difference) "PIT ECDF minus uniform CDF" else "PIT cumulative probability")
  attr(p, "pit_reference") <- c(list(method = "independent", interpolated = FALSE,
    level = level, grid_size = grid_size, evaluation_points = grid_size + 1L,
    difference = difference), alignment)
  p
}
