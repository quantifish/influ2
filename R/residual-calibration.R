# Probability calibration is calculated alongside the existing residuals. The
# original fitted probabilities, not noisy simulation means, define fixed bins.
.resid_probability_bins <- function(probability, bins, min_n, tolerance = 1e-8) {
  .resid_integer(bins, "calibration_bins", 1L)
  .resid_integer(min_n, "calibration_min_n", 1L)
  if (!is.numeric(probability) || !length(probability) ||
      any(!is.finite(probability) | probability < 0 | probability > 1)) {
    stop("Calibration requires finite fitted probabilities between zero and one.", call. = FALSE)
  }
  n <- length(probability)
  ord <- order(probability)
  p <- probability[ord]
  # Anchor each near-tie run to its first value (avoid chained near-ties).
  run <- integer(n)
  k <- 1L
  anchor <- p[1L]
  for (i in seq_len(n)) {
    if (p[i] - anchor > tolerance) {
      k <- k + 1L
      anchor <- p[i]
    }
    run[i] <- k
  }
  counts <- tabulate(run)
  cumulative <- cumsum(counts)
  wanted <- min(bins, max(1L, floor(n / min_n)), length(counts))
  cuts <- if (wanted > 1L) unique(vapply(seq_len(wanted - 1L), function(j) {
    which.min(abs(cumulative[-length(cumulative)] - j * n / wanted))
  }, integer(1))) else integer()
  # Remove boundaries next to under-supported bins; identical p never split.
  repeat {
    sizes <- diff(c(0, cumulative[cuts], n))
    small <- which(sizes < min_n)
    if (!length(cuts) || !length(small)) break
    j <- small[1L]
    boundary <- if (j == 1L) 1L else if (j == length(sizes)) j - 1L else {
      if (sizes[j - 1L] <= sizes[j + 1L]) j - 1L else j
    }
    cuts <- cuts[-boundary]
  }
  result <- integer(n)
  result[ord] <- findInterval(run - 1L, cuts) + 1L
  result
}

.resid_calibration_table <- function(ids, observed, probability, trials, min_n) {
  levels <- sort(unique(ids))
  rows <- lapply(levels, function(id) which(ids == id))
  tab <- do.call(rbind, lapply(seq_along(rows), function(i) {
    j <- rows[[i]]
    denominator <- sum(trials[j])
    data.frame(group = as.character(levels[i]), n = length(j), trials = denominator,
      predicted = sum(probability[j] * trials[j]) / denominator,
      observed = sum(observed[j]) / denominator,
      min_probability = min(probability[j]), max_probability = max(probability[j]),
      sparse = length(j) < min_n, lower = NA_real_, median = NA_real_, upper = NA_real_)
  }))
  list(table = tab, rows = rows)
}

.resid_calibration_setup <- function(adapter, bins, min_n, groups, nsim) {
  if (is.null(adapter$probability)) {
    if (!is.null(groups)) stop("`calibration_groups` requires an encounter or binomial response.", call. = FALSE)
    return(NULL)
  }
  p <- adapter$probability
  trials <- adapter$trials
  if (length(p) != length(adapter$observed) || length(trials) != length(p) ||
      any(!is.finite(trials) | trials < 1 | trials != round(trials))) {
    stop("Fitted probabilities and known trial counts must match the observed rows.", call. = FALSE)
  }
  if (any(adapter$observed < 0 | adapter$observed > trials |
      abs(adapter$observed - round(adapter$observed)) > 1e-7)) {
    stop("Observed calibration responses must be integer successes within the known trials.", call. = FALSE)
  }
  bin <- .resid_probability_bins(p, bins, min_n)
  result <- .resid_calibration_table(bin, adapter$observed, p, trials, min_n)
  result$bin <- bin
  result$simulated <- matrix(NA_real_, nrow(result$table), nsim)
  result$groups <- NULL
  if (!is.null(groups)) {
    if (!is.character(groups) || !length(groups) || anyNA(groups) ||
        anyDuplicated(groups) || !all(groups %in% names(adapter$data))) {
      stop("`calibration_groups` must name distinct columns in the aligned model data.", call. = FALSE)
    }
    values <- adapter$data[groups]
    if (any(vapply(values, function(x) !is.atomic(x) || !is.null(dim(x)) || anyNA(x), logical(1)))) {
      stop("Calibration group columns must contain non-missing atomic labels.", call. = FALSE)
    }
    if (any(groups %in% adapter$response_variables)) {
      stop("Do not define calibration groups from the observed response.", call. = FALSE)
    }
    ids <- as.integer(do.call(interaction, c(unname(values), list(drop = TRUE, lex.order = TRUE))))
    grouped <- .resid_calibration_table(ids, adapter$observed, p, trials, min_n)
    first <- vapply(grouped$rows, `[`, integer(1), 1L)
    grouped$table$group <- apply(values[first, , drop = FALSE], 1L, paste, collapse = " / ")
    grouped$columns <- values[first, , drop = FALSE]
    rownames(grouped$columns) <- NULL
    # Sparse groups remain visible, but no unreliable-looking envelope is drawn.
    # At most n/min_n group-by-simulation summaries need temporary storage.
    grouped$supported <- which(!grouped$table$sparse)
    grouped$simulated <- matrix(NA_real_, length(grouped$supported), nsim)
    result$groups <- grouped
  }
  result$settings <- list(requested_bins = bins, actual_bins = nrow(result$table),
    min_n = min_n, constant = diff(range(p)) <= 1e-8, groups = groups,
    prediction_type = adapter$prediction_type,
    conditioning = adapter$scheme, tolerance = 1e-8)
  result
}

.resid_calibration_update <- function(calibration, sim, id, trials) {
  if (is.null(calibration)) return(NULL)
  if (any(sim < 0 | sim > trials | abs(sim - round(sim)) > 1e-7)) {
    stop("Calibration simulations must be success counts within known trial totals.", call. = FALSE)
  }
  calibration$simulated[, id] <- vapply(calibration$rows, function(j) {
    sum(sim[j]) / sum(trials[j])
  }, numeric(1))
  g <- calibration$groups
  if (!is.null(g) && length(g$supported)) {
    g$simulated[, id] <- vapply(g$rows[g$supported], function(j) {
      sum(sim[j]) / sum(trials[j])
    }, numeric(1))
    calibration$groups <- g
  }
  calibration
}

.resid_calibration_finish <- function(calibration, level) {
  if (is.null(calibration)) return(NULL)
  intervals <- function(sims) t(apply(sims, 1L, stats::quantile,
    probs = c((1 - level) / 2, 0.5, (1 + level) / 2), names = FALSE))
  columns <- c("lower", "median", "upper")
  calibration$table[columns] <- intervals(calibration$simulated)
  g <- calibration$groups
  if (!is.null(g)) {
    if (length(g$supported)) g$table[g$supported, columns] <- intervals(g$simulated)
    g <- list(table = g$table, columns = g$columns)
  }
  list(bins = calibration$table, groups = g, settings = calibration$settings,
    level = level, envelope = "Pointwise predictive envelope at fixed fitted-probability bins; not a calibration-curve confidence interval")
}

.resid_response_panel <- function(x, requested) {
  if (requested != "auto") return(requested)
  kind <- x$metadata$response_kind
  if (is.null(kind)) {
    warning("This older diagnostic has no response-type metadata; retaining the distribution panel. Recalculate with influ_residuals() for automatic encounter calibration.", call. = FALSE)
    return("distribution")
  }
  if (kind == "bernoulli") "calibration" else "distribution"
}

.plot_residual_calibration <- function(x, grouped = FALSE) {
  kind <- x$metadata$response_kind
  if (!is.null(kind) && !kind %in% c("bernoulli", "grouped_binomial")) {
    stop("Calibration requires an encounter/Bernoulli or known-trial binomial response, not a positive or combined catch response.", call. = FALSE)
  }
  c <- x$calibration
  if (is.null(c)) {
    stop("This diagnostic has no fitted-probability calibration summaries. Recalculate with influ_residuals(); simulation-averaged responses are not substituted for fitted probabilities.", call. = FALSE)
  }
  d <- if (grouped) c$groups$table else c$bins
  if (is.null(d)) stop("Calculate `influ_residuals(..., calibration_groups = c('year', 'target'))` first, using your scientific grouping columns.", call. = FALSE)
  has_band <- all(c("lower", "upper") %in% names(d)) && any(is.finite(d$lower) & is.finite(d$upper))
  if (!has_band) warning("No stored simulation envelope is available; plotting observed proportions without invented uncertainty.", call. = FALSE)
  note <- if (has_band) paste0(100 * c$level, "% pointwise predictive envelope; exploratory") else "Exploratory; no stored predictive envelope"
  if (isTRUE(c$settings$constant) && !grouped) note <- paste(note, "One probability: overall frequency only", sep = "\n")
  if (any(d$sparse)) note <- paste(note, paste0("Sparse support: n < ", c$settings$min_n), sep = "\n")
  if (grouped) {
    d$position <- seq_len(nrow(d))
    d$deviation <- d$observed - d$predicted
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$position, y = .data$deviation)) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "grey40")
    if (has_band) p <- p + ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$lower - .data$predicted,
        ymax = .data$upper - .data$predicted), colour = "grey60", linewidth = 1, na.rm = TRUE)
    p <- p + ggplot2::scale_x_continuous(breaks = d$position, labels = d$group) +
      ggplot2::labs(title = "Grouped encounter checks", subtitle = note,
        x = paste(c$settings$groups, collapse = " / "), y = "Observed minus predicted proportion") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  } else {
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$predicted, y = .data$observed)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey40")
    if (has_band) p <- p + ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper), colour = "grey60", linewidth = 1, na.rm = TRUE)
    p <- p + ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
      ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
      ggplot2::labs(title = "Encounter probability calibration", subtitle = note,
        x = "Mean predicted probability", y = "Observed proportion")
  }
  p <- p + ggplot2::geom_point(ggplot2::aes(size = .data$n), colour = "purple4", alpha = 0.8) +
    ggplot2::scale_size_area(max_size = 5, name = "Observations", breaks = .resid_count_breaks(d$n)) +
    ggplot2::theme(legend.position = "bottom", legend.key.width = grid::unit(1.2, "cm"))
  if (any(d$sparse)) p <- p + ggplot2::geom_point(data = d[d$sparse, , drop = FALSE],
    shape = 4, size = 2, colour = "black")
  if (identical(kind, "grouped_binomial") && !grouped) {
    p <- p + ggplot2::labs(title = "Binomial probability calibration",
      x = "Trial-weighted predicted success probability", y = "Observed successes / trials")
  }
  p
}

.resid_count_breaks <- function(n) {
  b <- pretty(range(n), n = 3)
  b <- b[b >= min(n) & b <= max(n) & b > 0]
  if (!length(b)) unique(n) else b
}
