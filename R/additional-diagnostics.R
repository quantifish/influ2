.comparison_focus <- function(model, focus = NULL) {
  if (!is.null(focus)) return(focus)
  if (inherits(model, "influ_diag")) return(model$focus)
  frm <- tryCatch(stats::formula(model), error = function(e) NULL)
  if (inherits(frm, "brmsformula")) frm <- frm$formula
  if (is.null(frm)) {
    stop("Supply `year =` when a model's formula cannot be recovered.", call. = FALSE)
  }
  predictors <- all.vars(frm[[length(frm)]])
  if (!length(predictors)) {
    stop("The model has no predictor that can be used as the focus.", call. = FALSE)
  }
  predictors[1]
}

.as_diagnostic_list <- function(fits, focus = NULL, ...) {
  fitted_classes <- c("influ_diag", "glm", "gam", "glmmTMB", "brmsfit", "sdmTMB", "tinyVAST")
  if (any(inherits(fits, fitted_classes))) fits <- list(fits)
  if (!is.list(fits) || !length(fits)) {
    stop("`fits` must be a fitted model, an influ_diag, or a non-empty list of them.", call. = FALSE)
  }
  lapply(fits, function(model) {
    if (inherits(model, "influ_diag")) return(model)
    influ(model, focus = .comparison_focus(model, focus), ...)
  })
}

.comparison_index_data <- function(fits, labels = NULL, focus = NULL, ...) {
  diagnostics <- .as_diagnostic_list(fits, focus, ...)
  if (is.null(labels)) {
    labels <- vapply(seq_along(diagnostics), function(i) {
      model <- if (is.list(fits) && !any(inherits(fits, c(
        "influ_diag", "glm", "gam", "glmmTMB", "brmsfit", "sdmTMB", "tinyVAST"
      )))) fits[[i]] else fits
      frm <- tryCatch(stats::formula(model), error = function(e) NULL)
      formula_label <- if (is.null(frm)) paste0(diagnostics[[i]]$backend, " ", i) else {
        paste(deparse(frm), collapse = " ")
      }
      family <- diagnostics[[i]]$family
      paste0(formula_label, " [", family$family, "(", family$link, ")]")
    }, character(1))
    labels <- make.unique(labels, sep = " #")
  }
  if (!is.character(labels) || length(labels) != length(diagnostics) ||
      anyNA(labels) || any(!nzchar(trimws(labels))) ||
      anyDuplicated(trimws(labels))) {
    stop("`labels` must contain one unique, non-empty label per diagnostic.", call. = FALSE)
  }
  labels <- trimws(labels)

  out <- lapply(seq_along(diagnostics), function(i) {
    d <- influ_indices(diagnostics[[i]])
    d <- d[d$series == "standardised", , drop = FALSE]
    if (!nrow(d)) stop("Every diagnostic must contain a standardised index.", call. = FALSE)
    priority <- c("unconditional_mean", "conditional")
    chosen <- priority[priority %in% d$component][1]
    if (is.na(chosen)) chosen <- unique(d$component)[1]
    d <- unique(d[d$component == chosen, , drop = FALSE])
    d$Model <- labels[i]
    d
  })
  out <- do.call(rbind, out)
  scales <- unique(out$scale)
  if (length(scales) != 1L || is.na(scales) ||
      !scales %in% c("ratio", "difference", "link")) {
    stop(
      "Compared indices must use the same index scale; ratio, difference, and link scales cannot be mixed.",
      call. = FALSE
    )
  }
  out
}

.geometric_mean <- function(x) {
  if (any(!is.finite(x)) || any(x <= 0)) {
    stop("Index rescaling requires finite, positive values.", call. = FALSE)
  }
  exp(mean(log(x)))
}

.rescale_comparison_indices <- function(data, rescale = "raw", rescale_series = NULL) {
  models <- unique(data$Model)
  if (is.numeric(rescale) && length(rescale) == 1L) {
    if (!is.finite(rescale) || rescale <= 0) {
      stop("Numeric `rescale` must be finite and positive.", call. = FALSE)
    }
    for (model in models) {
      keep <- data$Model == model
      multiplier <- rescale / .geometric_mean(data$estimate[keep])
      columns <- c("estimate", "std_error", "lower", "upper")
      data[keep, columns] <- data[keep, columns] * multiplier
    }
  } else if (!identical(rescale, "raw")) {
    stop("`rescale` must be `\"raw\"` or a positive numeric value.", call. = FALSE)
  }

  if (!is.null(rescale_series)) {
    if (length(rescale_series) != 1L || !rescale_series %in% seq_along(models)) {
      stop("`rescale_series` must identify one supplied series.", call. = FALSE)
    }
    reference_model <- models[rescale_series]
    for (model in setdiff(models, reference_model)) {
      common <- intersect(
        data$level[data$Model == reference_model],
        data$level[data$Model == model]
      )
      if (!length(common)) {
        stop(
          "Compared index series must share at least one focus level when ",
          "`rescale_series` is used.",
          call. = FALSE
        )
      }
      reference <- data$estimate[data$Model == reference_model & data$level %in% common]
      target <- data$estimate[data$Model == model & data$level %in% common]
      multiplier <- .geometric_mean(reference) / .geometric_mean(target)
      keep <- data$Model == model
      columns <- c("estimate", "std_error", "lower", "upper")
      data[keep, columns] <- data[keep, columns] * multiplier
    }
  }
  data
}

#' Compare standardised CPUE indices
#'
#' Compares indices from any fitted models supported by [influ()] or from
#' pre-computed [influ_diag] objects. Models are reduced to the same index
#' schema before plotting.
#'
#' @param fits A fitted model, an [influ_diag], or a list of either. Alternatively,
#'   supply calculated [cpue_index()] objects, without mixing input types.
#' @param labels Optional unique, non-empty model labels. Repeated automatically
#'   generated labels are disambiguated with numeric suffixes.
#' @param year Optional focus-variable name. It is inferred when omitted.
#' @param probs Interval probabilities used when diagnostics must be calculated.
#' @param show_probs Show uncertainty ribbons.
#' @param rescale `"raw"`, or a positive numeric geometric mean.
#' @param rescale_series Optional series number supplying the common scale over
#'   overlapping focus levels.
#' @param ... Arguments passed to [influ()] when `fits` contains models.
#'
#' @details Compared indices must be on the same scale: response ratios,
#'   response differences, or link-scale contrasts. Ratio plots start at zero;
#'   difference and link-scale plots retain negative values. Inputs should
#'   describe comparable responses and focus effects; matching scales alone
#'   does not establish that the fitted models answer the same question.
#'   Fitted-model inputs retain their existing year-effect-contrast meaning.
#'   To compare expected-response indices, supply `cpue_index()` results with
#'   comparable reference populations and response definitions. Their stored
#'   intervals are used; set rescaling during calculation, not during plotting.
#'
#' @return A [ggplot2::ggplot()] object.
#' @export
plot_compare <- function(fits, labels = NULL, year = NULL,
                         probs = c(0.25, 0.75), show_probs = TRUE,
                         rescale = "raw", rescale_series = NULL, ...) {
  calculated <- if (inherits(fits, "influ_index")) list(fits) else fits
  if (is.list(calculated) && any(vapply(calculated, inherits, logical(1), "influ_index"))) {
    if (!identical(rescale, "raw") || !is.null(rescale_series) || !is.null(year) ||
        !missing(probs) || length(list(...))) {
      stop("For calculated CPUE indices, set year, intervals, and rescaling in `cpue_index()`; plots use the stored values.", call. = FALSE)
    }
    return(.plot_cpue_indices(calculated, labels, show_probs))
  }
  data <- .comparison_index_data(
    fits, labels = labels, focus = year, probs = probs, ...
  )
  data <- .rescale_comparison_indices(data, rescale, rescale_series)
  data$x <- .plot_level(data$level)

  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$x, y = .data$estimate,
      colour = .data$Model, fill = .data$Model, group = .data$Model
    )
  )
  if (isTRUE(show_probs)) {
    plot <- plot + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
      alpha = 0.18, colour = NA, na.rm = TRUE
    )
  }
  plot <- plot +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(x = diagnostics_focus(data), y = switch(unique(data$scale),
      ratio = "Standardised index", difference = "Year-effect difference",
      link = "Year-effect contrast (link scale)")) +
    ggplot2::theme_bw()
  if (identical(unique(data$scale), "ratio")) {
    plot <- plot + ggplot2::scale_y_continuous(
      limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.05))
    )
  }
  plot
}

diagnostics_focus <- function(data) {
  focus <- unique(data$focus)
  if (length(focus) == 1L) focus else "Focus"
}

#' Display the effect of sequential model-standardisation steps
#'
#' Plot supplied fits, calculated diagnostics, or a stored [influ_steps] object.
#' Set `refit = TRUE` with one original model to fit a sequence internally.
#' The displayed quantities are year-effect contrasts, including when spatial
#' processes are changed between fits; they are not area-integrated indices.
#'
#' @inheritParams influ_steps
#' @param fill Colour used for the current model's interval.
#' @param show_probs Show each step's uncertainty interval. These are not
#'   intervals for differences between models.
#' @md
#'
#' @return A [ggplot2::ggplot()] object.
#' @export
plot_step <- function(fits, labels = NULL, year = NULL, fill = "purple4",
                      probs = c(0.025, 0.975), show_probs = TRUE,
                      steps = NULL, refit = FALSE, component = NULL,
                      keep_fits = FALSE, refit_args = list(), ...) {
  sequence <- influ_steps(
    fits, labels = labels, year = year, probs = probs, steps = steps,
    refit = refit, component = component, keep_fits = keep_fits,
    refit_args = refit_args, ...
  )
  current <- sequence$indices
  models <- sequence$steps$label
  history <- do.call(rbind, lapply(seq_along(models), function(i) {
    do.call(rbind, lapply(seq_len(i), function(j) {
      d <- current[current$Model == models[j], , drop = FALSE]
      d$Panel <- models[i]
      d$Step <- if (j == i) "Current" else if (j == i - 1L) "Previous" else "Earlier"
      d
    }))
  }))
  history$x <- .plot_level(history$level)
  history$Panel <- factor(history$Panel, levels = models)

  plot <- ggplot2::ggplot(history)
  if (isTRUE(show_probs)) {
    intervals <- history[history$Step == "Current", , drop = FALSE]
    plot <- plot + ggplot2::geom_ribbon(
      data = intervals,
      ggplot2::aes(x = .data$x, ymin = .data$lower, ymax = .data$upper),
      fill = fill, alpha = 0.18, colour = NA, na.rm = TRUE
    )
  }
  plot <- plot +
    ggplot2::geom_line(
      ggplot2::aes(
        x = .data$x, y = .data$estimate,
        group = interaction(.data$Model, .data$Step),
        colour = .data$Step, linetype = .data$Step
      )
    ) +
    ggplot2::geom_point(
      data = history[history$Step == "Current", , drop = FALSE],
      ggplot2::aes(x = .data$x, y = .data$estimate)
    ) +
    ggplot2::facet_wrap(~Panel, ncol = 1) +
    ggplot2::scale_colour_manual(values = c(
      Current = "black", Previous = "black", Earlier = "grey65"
    )) +
    ggplot2::scale_linetype_manual(values = c(
      Current = "solid", Previous = "dashed", Earlier = "solid"
    )) +
    ggplot2::labs(x = sequence$focus, y = switch(sequence$metadata$scale,
      ratio = "Year-effect ratio", difference = "Year-effect difference",
      "Year-effect contrast")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")
  if (identical(sequence$metadata$scale, "ratio")) {
    plot <- plot + ggplot2::scale_y_continuous(
      limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.05))
    )
  }
  plot
}

.check_brms_criterion_fits <- function(fits) {
  compact <- vapply(fits, function(fit) {
    is.null(fit$fit) && !is.null(fit$influ2_draws)
  }, logical(1))
  if (any(compact)) {
    stop(
      "Bayesian R-squared and model criteria require the original complete brmsfit, ",
      "not a compact influence-only fixture. These helpers use an existing fit ",
      "and do not run MCMC; influ() can still use the compact fixture.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Summarise Bayesian R-squared for brms models
#'
#' @param fits A `brmsfit` or list of `brmsfit` objects.
#' @param probs Lower and upper interval probabilities.
#' @param ... Arguments passed to [brms::bayes_R2()].
#'
#' @details Requires an original complete `brmsfit`, not a compact influence-only
#'   fixture shipped with influ2. The helper summarises the existing fit and
#'   does not run MCMC.
#'
#' @return A data frame with one row per model.
#' @export
get_bayes_R2 <- function(fits, probs = c(0.025, 0.975), ...) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("Package 'brms' is required for Bayesian R-squared.", call. = FALSE)
  }
  if (inherits(fits, "brmsfit")) fits <- list(fits)
  if (!is.list(fits) || !length(fits) ||
      !all(vapply(fits, inherits, logical(1), "brmsfit"))) {
    stop("`fits` must contain only brmsfit objects.", call. = FALSE)
  }
  .check_brms_criterion_fits(fits)
  probs <- .validate_probs(probs)
  out <- lapply(fits, function(fit) {
    draws <- as.numeric(brms::bayes_R2(fit, summary = FALSE, ...))
    interval <- stats::quantile(draws, probs = probs, names = FALSE)
    data.frame(
      Model = paste(deparse(stats::formula(fit)), collapse = " "),
      Distribution = fit$family$family,
      Link = fit$family$link,
      R2 = mean(draws),
      SD = stats::sd(draws),
      lower = interval[1],
      upper = interval[2],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, out)
  out <- out[order(out$R2, decreasing = TRUE), , drop = FALSE]
  out$difference <- c(0, diff(out$R2))
  rownames(out) <- NULL
  out
}

#' Plot the completeness of variables through time
#'
#' @param data A data frame.
#' @param xvar Name of the temporal or grouping column.
#' @param yvar Names of columns whose non-missing coverage is displayed.
#'
#' @details For each variable and observed time level, bubble area represents
#'   the proportion of records whose value is not `NA`. A zero response is an
#'   observed value, not missing data. Cells with zero completeness are blank;
#'   neither a bubble nor a reference marker is drawn. For positive proportions,
#'   fixed-size red markers sit underneath the dark completeness bubbles.
#'   Records with a missing `xvar` are excluded.
#'   Completeness is calculated separately for each variable among the records
#'   supplied; it is not sampling intensity, measurement accuracy, or the
#'   proportion of joint complete cases across all selected variables.
#'
#' @examples
#' data(lobsters_per_pot)
#' coverage_example <- lobsters_per_pot
#' example_year <- as.integer(as.character(coverage_example$year))
#' record_number <- seq_len(nrow(coverage_example))
#' coverage_example$soak[example_year < 2005] <- NA_real_
#' coverage_example$soak[
#'   example_year >= 2005 & example_year < 2010 & record_number %% 2 == 0
#' ] <- NA_real_
#' coverage_example$depth[
#'   example_year < 2010 & record_number %% 4 == 0
#' ] <- NA_real_
#' plot_data_extent(coverage_example, "year", c("lobsters", "depth", "soak"))
#'
#' @return A [ggplot2::ggplot()] object.
#' @export
plot_data_extent <- function(data, xvar, yvar) {
  if (!is.data.frame(data) || length(xvar) != 1L || !xvar %in% names(data) ||
      !length(yvar) || any(!yvar %in% names(data))) {
    stop("`xvar` and `yvar` must name columns in `data`.", call. = FALSE)
  }
  observed_time <- !is.na(data[[xvar]])
  if (!any(observed_time)) {
    stop("`xvar` must contain at least one non-missing value.", call. = FALSE)
  }
  time_levels <- .focus_info(data[observed_time, , drop = FALSE], xvar)$levels
  out <- do.call(rbind, lapply(yvar, function(variable) {
    proportion <- vapply(time_levels, function(level) {
      keep <- !is.na(data[[xvar]]) & data[[xvar]] == level
      if (!any(keep)) NA_real_ else mean(!is.na(data[[variable]][keep]))
    }, numeric(1))
    data.frame(
      time = time_levels,
      variable = variable,
      proportion = proportion,
      stringsAsFactors = FALSE
    )
  }))
  out$variable <- factor(out$variable, levels = rev(yvar))
  out$time <- .plot_level(out$time)
  present <- out[!is.na(out$proportion) & out$proportion > 0, , drop = FALSE]

  ggplot2::ggplot(
    out,
    ggplot2::aes(x = .data$variable, y = .data$time)
  ) +
    # Retain the full grid, including completely missing variables and years.
    ggplot2::geom_blank() +
    ggplot2::geom_point(
      data = present, size = 4, colour = "firebrick2", alpha = 0.55
    ) +
    ggplot2::geom_point(
      ggplot2::aes(size = .data$proportion),
      data = present, colour = "grey15", na.rm = TRUE
    ) +
    ggplot2::scale_size_area(
      limits = c(0, 1), breaks = c(0.25, 0.5, 0.75, 1), max_size = 8
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = xvar, size = "Proportion present") +
    ggplot2::theme_bw()
}
