.comparison_focus <- function(model, focus = NULL) {
  if (!is.null(focus)) return(focus)
  if (inherits(model, "influ_diag")) return(model$focus)
  frm <- tryCatch(stats::formula(model), error = function(e) NULL)
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
  }
  if (length(labels) != length(diagnostics)) {
    stop("`labels` must have one value per diagnostic.", call. = FALSE)
  }

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
  do.call(rbind, out)
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
#' @param fits A fitted model, an [influ_diag], or a list of either.
#' @param labels Optional model labels.
#' @param year Optional focus-variable name. It is inferred when omitted.
#' @param probs Interval probabilities used when diagnostics must be calculated.
#' @param show_probs Show uncertainty ribbons.
#' @param rescale `"raw"`, or a positive numeric geometric mean.
#' @param rescale_series Optional series number supplying the common scale over
#'   overlapping focus levels.
#' @param ... Arguments passed to [influ()] when `fits` contains models.
#'
#' @return A [ggplot2::ggplot()] object.
#' @export
plot_compare <- function(fits, labels = NULL, year = NULL,
                         probs = c(0.25, 0.75), show_probs = TRUE,
                         rescale = "raw", rescale_series = NULL, ...) {
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
  plot +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_y_continuous(
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::labs(x = diagnostics_focus(data), y = "Standardised index") +
    ggplot2::theme_bw()
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

#' Summarise Bayesian R-squared for BRMS models
#'
#' @param fits A `brmsfit` or list of `brmsfit` objects.
#' @param probs Lower and upper interval probabilities.
#' @param ... Arguments passed to [brms::bayes_R2()].
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

#' Compare BRMS model criteria
#'
#' Calculates selected public BRMS criteria without inspecting the underlying
#' Stan object. This keeps `rstan` out of the package's mandatory dependencies.
#'
#' @param fits A `brmsfit` or list of `brmsfit` objects.
#' @param criterion Any of `"loo"`, `"loo_R2"`, `"bayes_R2"`, and
#'   `"log_lik"`.
#' @param sort Sort models by expected log predictive density, or Bayesian
#'   R-squared when LOO is not requested.
#' @param ... Arguments passed to the requested BRMS criterion functions.
#'
#' @return A data frame with one row per model.
#' @export
table_criterion <- function(fits,
                            criterion = c("loo", "loo_R2", "bayes_R2"),
                            sort = TRUE, ...) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("Package 'brms' is required for model criteria.", call. = FALSE)
  }
  allowed <- c("loo", "loo_R2", "bayes_R2", "log_lik")
  if (!length(criterion) || any(!criterion %in% allowed)) {
    stop("Unknown `criterion`; see `?table_criterion`.", call. = FALSE)
  }
  if (inherits(fits, "brmsfit")) fits <- list(fits)
  if (!is.list(fits) || !length(fits) ||
      !all(vapply(fits, inherits, logical(1), "brmsfit"))) {
    stop("`fits` must contain only brmsfit objects.", call. = FALSE)
  }

  rows <- lapply(seq_along(fits), function(i) {
    fit <- fits[[i]]
    row <- data.frame(
      id = i,
      Model = paste(deparse(stats::formula(fit)), collapse = " "),
      Distribution = fit$family$family,
      Link = fit$family$link,
      stringsAsFactors = FALSE
    )
    if ("loo" %in% criterion) {
      value <- fit$criteria$loo
      if (is.null(value)) value <- brms::loo(fit, ...)
      row$elpd_loo <- value$estimates["elpd_loo", "Estimate"]
      row$se_elpd_loo <- value$estimates["elpd_loo", "SE"]
      row$p_loo <- value$estimates["p_loo", "Estimate"]
      row$looic <- value$estimates["looic", "Estimate"]
    }
    if ("loo_R2" %in% criterion) {
      value <- as.data.frame(brms::loo_R2(fit, ...))
      row$loo_R2 <- value$Estimate[1]
      row$se_loo_R2 <- value$Est.Error[1]
    }
    if ("bayes_R2" %in% criterion) {
      value <- as.data.frame(brms::bayes_R2(fit, ...))
      row$bayes_R2 <- value$Estimate[1]
      row$se_bayes_R2 <- value$Est.Error[1]
    }
    if ("log_lik" %in% criterion) {
      value <- rowSums(brms::log_lik(fit, ...))
      row$log_lik <- mean(value)
      row$se_log_lik <- stats::sd(value)
    }
    row
  })
  out <- do.call(rbind, rows)
  if (isTRUE(sort)) {
    order_by <- if ("elpd_loo" %in% names(out)) "elpd_loo" else {
      if ("bayes_R2" %in% names(out)) "bayes_R2" else NULL
    }
    if (!is.null(order_by)) out <- out[order(out[[order_by]], decreasing = TRUE), ]
  }
  rownames(out) <- NULL
  out
}

#' Plot the completeness of variables through time
#'
#' @param data A data frame.
#' @param xvar Name of the temporal or grouping column.
#' @param yvar Names of columns whose non-missing coverage is displayed.
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

  ggplot2::ggplot(
    out,
    ggplot2::aes(x = .data$variable, y = .data$time)
  ) +
    ggplot2::geom_point(size = 4, colour = "firebrick2", alpha = 0.55) +
    ggplot2::geom_point(
      ggplot2::aes(size = .data$proportion),
      colour = "grey15", na.rm = TRUE
    ) +
    ggplot2::scale_size_area(limits = c(0, 1), max_size = 8) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = xvar, size = "Proportion present") +
    ggplot2::theme_bw()
}

.residual_estimate <- function(model, type = "pearson") {
  if (inherits(model, "brmsfit")) {
    if (!requireNamespace("brms", quietly = TRUE)) {
      stop("Package 'brms' is required for BRMS residuals.", call. = FALSE)
    }
    value <- stats::residuals(model, type = type)
  } else {
    value <- stats::residuals(model, type = type)
  }
  if (is.matrix(value) || is.data.frame(value)) {
    column <- if ("Estimate" %in% colnames(value)) "Estimate" else colnames(value)[1]
    value <- value[, column]
  }
  as.numeric(value)
}

.focus_link_effect <- function(model, data, focus) {
  diagnostic <- if (inherits(model, "influ_diag")) model else {
    influ(model, focus = focus, data = data, uncertainty = "none")
  }
  d <- influ_effects(diagnostic)
  d <- d[d$scale == "link", , drop = FALSE]
  terms <- unique(d$term)
  focus_terms <- terms[vapply(terms, function(term) {
    focus %in% all.vars(tryCatch(
      stats::as.formula(paste("~", term)),
      error = function(e) stats::as.formula("~ 1")
    ))
  }, logical(1))]
  if (!length(focus_terms)) {
    stop("The diagnostic does not contain a link-scale focus effect.", call. = FALSE)
  }
  if (length(focus_terms) > 1L) {
    stop(
      "Implied residuals are ambiguous when multiple model terms contain ",
      "the focus variable. Fit a main-effects model, or calculate the ",
      "interaction-specific baseline explicitly.",
      call. = FALSE
    )
  }
  d <- d[d$term == focus_terms[1], , drop = FALSE]
  priority <- c("unconditional_mean", "conditional")
  component <- priority[priority %in% d$component][1]
  if (!is.na(component)) d <- d[d$component == component, , drop = FALSE]
  unique(d[c("level", "estimate")])
}

#' Plot implied residual coefficients
#'
#' Implied residual coefficients mimic a focus-by-group interaction that was
#' not fitted. For each stratum, the mean standardised residual is added to the
#' normalised focus coefficient. This follows the definition used in New
#' Zealand inshore CPUE reports. Error bars show one standard error of the
#' standardised residuals.
#'
#' @references Starr, P. J., and Kendrick, T. H. (2019). *FLA 1 Fishery
#'   Characterisation and CPUE*. New Zealand Fisheries Assessment Report
#'   2019/09, Figure O.9. See also Middleton, D. A. J. (2025). *A Rapid Update
#'   of CPUE for the Snapper Fishery in SNA 2 to 2024*. New Zealand Fisheries
#'   Assessment Report 2025/32, Appendix C.
#'
#' @param fit A fitted model supported by [influ()].
#' @param data Optional original model data.
#' @param year Name of the focus variable.
#' @param groups Name of the categorical variable used for panels.
#' @param type Residual type passed to the fitted model's `residuals()` method.
#' @param min_n Minimum records required in a focus-by-group stratum.
#' @param colour Colour used for implied coefficients.
#'
#' @return A [ggplot2::ggplot()] object.
#' @export
plot_implied_residuals <- function(fit, data = NULL, year = "year",
                                   groups = "area", type = "pearson",
                                   min_n = 10L, colour = "purple4") {
  if (inherits(fit, "influ_diag")) {
    stop("Supply the fitted model, because observation residuals are required.", call. = FALSE)
  }
  data <- .resolve_influ_data(fit, data)
  if (!all(c(year, groups) %in% names(data))) {
    stop("`year` and `groups` must name columns in the model data.", call. = FALSE)
  }
  if (anyNA(data[c(year, groups)])) {
    stop("`year` and `groups` must not contain missing values.", call. = FALSE)
  }
  if (!is.numeric(min_n) || length(min_n) != 1L || !is.finite(min_n) ||
      min_n < 1 || min_n != floor(min_n)) {
    stop("`min_n` must be a positive whole number.", call. = FALSE)
  }
  residual <- .residual_estimate(fit, type)
  if (length(residual) != nrow(data)) {
    stop("The model returned a different number of residuals than data rows.", call. = FALSE)
  }
  baseline <- .focus_link_effect(fit, data, year)
  work <- data.frame(
    focus = as.character(data[[year]]),
    group = as.character(data[[groups]]),
    residual = residual,
    stringsAsFactors = FALSE
  )
  key <- interaction(work$focus, work$group, drop = TRUE, lex.order = TRUE)
  strata <- do.call(rbind, lapply(split(seq_len(nrow(work)), key), function(i) {
    values <- work$residual[i]
    data.frame(
      level = work$focus[i[1]],
      group = work$group[i[1]],
      n = sum(is.finite(values)),
      residual = mean(values, na.rm = TRUE),
      std_error = stats::sd(values, na.rm = TRUE) / sqrt(sum(is.finite(values))),
      stringsAsFactors = FALSE
    )
  }))
  strata <- merge(strata, baseline, by = "level", all.x = TRUE)
  strata$implied <- strata$estimate + strata$residual
  strata$lower <- strata$implied - strata$std_error
  strata$upper <- strata$implied + strata$std_error
  strata <- strata[strata$n >= min_n & is.finite(strata$implied), , drop = FALSE]
  strata$x <- .plot_level(strata$level)
  baseline$x <- .plot_level(baseline$level)

  ggplot2::ggplot(
    strata,
    ggplot2::aes(x = .data$x, y = .data$implied, group = 1)
  ) +
    ggplot2::geom_line(
      data = baseline,
      ggplot2::aes(x = .data$x, y = .data$estimate, group = 1),
      inherit.aes = FALSE, colour = "grey65", linewidth = 0.7
    ) +
    ggplot2::geom_point(
      data = baseline,
      ggplot2::aes(x = .data$x, y = .data$estimate),
      inherit.aes = FALSE, colour = "grey65", size = 1.3
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
      width = 0.15, colour = colour
    ) +
    ggplot2::geom_line(colour = colour) +
    ggplot2::geom_point(ggplot2::aes(size = .data$n), colour = colour) +
    ggplot2::facet_wrap(~group) +
    ggplot2::labs(
      x = year, y = "Residual implied coefficient", size = "Records"
    ) +
    ggplot2::theme_bw()
}

.fitted_estimate <- function(model) {
  value <- stats::fitted(model)
  if (is.matrix(value) || is.data.frame(value)) {
    column <- if ("Estimate" %in% colnames(value)) "Estimate" else colnames(value)[1]
    value <- value[, column]
  }
  as.numeric(value)
}

#' Plot predicted values against residuals
#'
#' @param fit A fitted model.
#' @param trend One of `"loess"`, `"lm"`, `"linear"`, or `"none"`.
#' @param type Residual type passed to the fitted model.
#'
#' @return A [ggplot2::ggplot()] object.
#' @export
plot_predicted_residuals <- function(fit, trend = "loess", type = "pearson") {
  predicted <- .fitted_estimate(fit)
  residual <- .residual_estimate(fit, type)
  if (length(predicted) != length(residual)) {
    stop("Fitted values and residuals have different lengths.", call. = FALSE)
  }
  data <- data.frame(predicted = predicted, residual = residual)
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$predicted, y = .data$residual)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = 3, colour = "grey45") +
    ggplot2::geom_point(shape = 1, alpha = 0.45) +
    ggplot2::labs(x = "Predicted values", y = "Residuals") +
    ggplot2::theme_bw()
  if (trend == "loess") {
    plot <- plot + ggplot2::geom_smooth(
      method = "loess", se = FALSE, formula = y ~ x
    )
  } else if (trend %in% c("lm", "linear")) {
    plot <- plot + ggplot2::geom_smooth(
      method = "lm", se = FALSE, formula = y ~ x
    )
  } else if (trend != "none") {
    stop("`trend` must be `\"loess\"`, `\"lm\"`, `\"linear\"`, or `\"none\"`.", call. = FALSE)
  }
  plot
}

#' Quantile-quantile plot of model residuals
#'
#' Displays the fitted model's standardised residuals against normal
#' quantiles. For non-Gaussian models this is a screening diagnostic, and
#' should be complemented by simulation-based residual checks.
#'
#' @param fit A fitted model.
#' @param probs Two probabilities defining the reference line.
#' @param type Residual type passed to the fitted model.
#'
#' @return A [ggplot2::ggplot()] object.
#' @export
plot_qq <- function(fit, probs = c(0.25, 0.75), type = "pearson") {
  probs <- .validate_probs(probs)
  residual <- sort(.residual_estimate(fit, type))
  residual <- residual[is.finite(residual)]
  if (length(residual) < 2L) {
    stop("At least two finite residuals are required for a Q-Q plot.", call. = FALSE)
  }
  theoretical <- stats::qnorm(stats::ppoints(length(residual)))
  x <- stats::qnorm(probs)
  y <- stats::quantile(residual, probs, names = FALSE)
  slope <- diff(y) / diff(x)
  intercept <- y[1] - slope * x[1]
  data <- data.frame(theoretical = theoretical, residual = residual)

  ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$theoretical, y = .data$residual)
  ) +
    ggplot2::geom_abline(
      intercept = intercept, slope = slope, linetype = 3, colour = "grey45"
    ) +
    ggplot2::geom_point(shape = 1) +
    ggplot2::labs(x = "Theoretical quantiles", y = "Sample quantiles") +
    ggplot2::theme_bw()
}
