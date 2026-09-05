.resolve_influ_data <- function(model, data = NULL) {
  if (!is.null(data)) return(as.data.frame(data))

  model_frame <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  call_data <- tryCatch(model$call$data, error = function(e) NULL)
  if (!is.null(call_data)) {
    formula_environment <- tryCatch(
      environment(stats::formula(model)),
      error = function(e) parent.frame()
    )
    original <- tryCatch(
      eval(call_data, envir = formula_environment, enclos = parent.frame()),
      error = function(e) NULL
    )
    if (is.data.frame(original)) {
      original <- as.data.frame(original)
      if (is.null(model_frame) || nrow(original) == nrow(model_frame)) {
        return(original)
      }
      used_rows <- row.names(model_frame)
      if (length(used_rows) == nrow(model_frame) &&
          all(used_rows %in% row.names(original))) {
        return(original[used_rows, , drop = FALSE])
      }
    }
  }

  if (!is.null(model_frame)) return(as.data.frame(model_frame))

  if (!is.null(model$data)) return(as.data.frame(model$data))
  if (!is.null(model$frame)) return(as.data.frame(model$frame))

  stop(
    "The model data could not be recovered. Supply the original data with ",
    "`data =`.",
    call. = FALSE
  )
}

.resolve_influ_weights <- function(data, weights = NULL) {
  if (is.null(weights)) return(rep(1, nrow(data)))
  if (is.character(weights) && length(weights) == 1L) {
    if (!weights %in% names(data)) {
      stop("Weight column '", weights, "' was not found in `data`.", call. = FALSE)
    }
    weights <- data[[weights]]
  }
  if (!is.numeric(weights) || length(weights) != nrow(data)) {
    stop("`weights` must be numeric with one value per observation.", call. = FALSE)
  }
  if (any(!is.finite(weights)) || any(weights < 0) || sum(weights) <= 0) {
    stop("`weights` must be finite, non-negative, and have a positive sum.", call. = FALSE)
  }
  as.numeric(weights)
}

.focus_info <- function(data, focus) {
  if (!is.character(focus) || length(focus) != 1L || !focus %in% names(data)) {
    stop("`focus` must name one column in the model data.", call. = FALSE)
  }
  value <- data[[focus]]
  if (!length(value)) {
    stop("The model data contain no observations.", call. = FALSE)
  }
  if (anyNA(value)) {
    stop("The focus variable contains missing values.", call. = FALSE)
  }
  if ((is.numeric(value) || inherits(value, c("Date", "POSIXt"))) &&
      any(!is.finite(value))) {
    stop("The focus variable contains non-finite values.", call. = FALSE)
  }
  levels <- if (is.factor(value)) {
    levels(droplevels(value))
  } else if (is.numeric(value) || inherits(value, c("Date", "POSIXt"))) {
    as.character(sort(unique(value)))
  } else {
    unique(as.character(value))
  }
  list(value = as.character(value), levels = levels)
}

.validate_probs <- function(probs) {
  if (!is.numeric(probs) || length(probs) != 2L ||
      any(!is.finite(probs)) || any(probs <= 0 | probs >= 1) ||
      probs[1] >= probs[2]) {
    stop(
      "`probs` must contain two increasing probabilities between zero and one.",
      call. = FALSE
    )
  }
  as.numeric(probs)
}

.validate_ndraws <- function(ndraws, allow_null = FALSE) {
  if (is.null(ndraws) && isTRUE(allow_null)) return(NULL)
  if (!is.numeric(ndraws) || length(ndraws) != 1L ||
      !is.finite(ndraws) || ndraws < 1 || ndraws != floor(ndraws)) {
    stop("`ndraws` must be a positive whole number.", call. = FALSE)
  }
  as.integer(ndraws)
}

.weighted_col_mean <- function(x, w) {
  if (!nrow(x)) return(rep(NA_real_, ncol(x)))
  if (inherits(x, "Matrix")) {
    return(as.numeric(Matrix::colSums(x * w) / sum(w)))
  }
  colSums(x * w) / sum(w)
}

.term_contrast <- function(x, focus_info, weights, reference = NULL,
                           reference_weights = NULL) {
  if (is.null(reference)) {
    overall <- .weighted_col_mean(x, weights)
  } else {
    overall <- .weighted_col_mean(reference, reference_weights)
  }
  out <- matrix(NA_real_, nrow = length(focus_info$levels), ncol = ncol(x))
  rownames(out) <- focus_info$levels
  colnames(out) <- colnames(x)
  for (i in seq_along(focus_info$levels)) {
    keep <- focus_info$value == focus_info$levels[i]
    if (sum(weights[keep]) <= 0) {
      stop(
        "Every focus level must have a positive total weight; level '",
        focus_info$levels[i], "' does not.",
        call. = FALSE
      )
    }
    out[i, ] <- .weighted_col_mean(x[keep, , drop = FALSE], weights[keep]) - overall
  }
  out
}

.resolve_reference <- function(data, X, reference_data = NULL,
                               reference_X = NULL, reference_weights = NULL) {
  if (is.null(reference_data) && is.null(reference_X)) {
    return(list(
      data = data,
      X = X,
      weights = rep(1, nrow(data)),
      label = "observed",
      explicit = FALSE
    ))
  }
  if (is.null(reference_data) || is.null(reference_X)) {
    stop("`reference_data` and its model matrix must be supplied together.", call. = FALSE)
  }
  reference_data <- as.data.frame(reference_data)
  if (!is.matrix(reference_X) && !inherits(reference_X, "Matrix")) {
    reference_X <- as.matrix(reference_X)
  }
  if (nrow(reference_X) != nrow(reference_data) || ncol(reference_X) != ncol(X)) {
    stop("The reference model matrix does not conform to `reference_data` or the fitted model matrix.", call. = FALSE)
  }
  list(
    data = reference_data,
    X = reference_X,
    weights = .resolve_influ_weights(reference_data, reference_weights),
    label = "prediction_grid",
    explicit = TRUE
  )
}

.draw_mvn <- function(n, mean, sigma, seed = NULL) {
  n <- .validate_ndraws(n)
  if (!is.null(seed)) set.seed(seed)
  sigma <- (sigma + t(sigma)) / 2
  eig <- eigen(sigma, symmetric = TRUE)
  tol <- max(abs(eig$values)) * sqrt(.Machine$double.eps)
  values <- pmax(eig$values, 0)
  if (any(eig$values < -tol)) {
    warning("The coefficient covariance matrix was not positive semi-definite; negative eigenvalues were truncated.", call. = FALSE)
  }
  z <- matrix(stats::rnorm(n * length(mean)), nrow = n)
  root <- sweep(t(eig$vectors), 1L, sqrt(values), "*")
  sweep(z %*% root, 2L, mean, "+")
}

.compact_contrast <- function(contribution, data, focus, weights,
                              reference_contribution = NULL,
                              reference_weights = NULL) {
  focus_info <- .focus_info(data, focus)
  contribution <- matrix(contribution, ncol = 1L)
  if (is.null(reference_contribution)) {
    reference_contribution <- contribution
    reference_weights <- weights
  }
  as.numeric(.term_contrast(
    contribution,
    focus_info,
    weights,
    matrix(reference_contribution, ncol = 1L),
    reference_weights
  ))
}

.summarise_vector <- function(x, probs) {
  c(
    estimate = mean(x),
    std_error = stats::sd(x),
    lower = unname(stats::quantile(x, probs[1], names = FALSE)),
    upper = unname(stats::quantile(x, probs[2], names = FALSE))
  )
}

.empty_uncertainty <- function(n) {
  list(
    std_error = rep(NA_real_, n),
    lower = rep(NA_real_, n),
    upper = rep(NA_real_, n)
  )
}

.effect_rows <- function(focus, levels, term, component, scale, estimate,
                         std_error, lower, upper, method) {
  data.frame(
    focus = focus,
    level = as.character(levels),
    term = term,
    component = component,
    scale = scale,
    estimate = as.numeric(estimate),
    std_error = as.numeric(std_error),
    lower = as.numeric(lower),
    upper = as.numeric(upper),
    method = method,
    stringsAsFactors = FALSE
  )
}

.component_influence <- function(B, beta, vcov, beta_draws, eta_reference,
                                 eta_reference_draws, family_spec, focus,
                                 levels, term, component, method, probs) {
  link_estimate <- as.numeric(B %*% beta)
  link_draws <- NULL

  if (!is.null(beta_draws)) {
    link_draws <- beta_draws %*% t(B)
    link_stats <- t(apply(link_draws, 2L, .summarise_vector, probs = probs))
    link_estimate <- link_stats[, "estimate"]
    link_uncertainty <- list(
      std_error = link_stats[, "std_error"],
      lower = link_stats[, "lower"],
      upper = link_stats[, "upper"]
    )
  } else if (!is.null(vcov)) {
    se <- sqrt(pmax(0, rowSums((B %*% vcov) * B)))
    z <- stats::qnorm(probs)
    link_uncertainty <- list(
      std_error = se,
      lower = link_estimate + z[1] * se,
      upper = link_estimate + z[2] * se
    )
  } else {
    link_uncertainty <- .empty_uncertainty(length(link_estimate))
  }

  link_rows <- .effect_rows(
    focus, levels, term, component, "link", link_estimate,
    link_uncertainty$std_error, link_uncertainty$lower,
    link_uncertainty$upper, method
  )

  natural_scale <- family_spec$natural_scale
  if (!is.null(link_draws)) {
    if (natural_scale == "ratio") {
      natural_draws <- exp(link_draws)
    } else {
      natural_draws <- sweep(link_draws, 1L, eta_reference_draws, "+")
      natural_draws <- .link_inverse(natural_draws, family_spec$link) -
        .link_inverse(eta_reference_draws, family_spec$link)
      if (isTRUE(family_spec$complement)) natural_draws <- -natural_draws
    }
    natural_stats <- t(apply(natural_draws, 2L, .summarise_vector, probs = probs))
    natural_estimate <- natural_stats[, "estimate"]
    natural_uncertainty <- list(
      std_error = natural_stats[, "std_error"],
      lower = natural_stats[, "lower"],
      upper = natural_stats[, "upper"]
    )
  } else {
    natural_estimate <- .effect_transform(link_estimate, family_spec, eta_reference)
    lower <- .effect_transform(link_uncertainty$lower, family_spec, eta_reference)
    upper <- .effect_transform(link_uncertainty$upper, family_spec, eta_reference)
    z_width <- diff(stats::qnorm(probs))
    natural_uncertainty <- list(
      std_error = ifelse(
        is.finite(lower) & is.finite(upper),
        abs(upper - lower) / z_width,
        NA_real_
      ),
      lower = pmin(lower, upper),
      upper = pmax(lower, upper)
    )
  }

  natural_rows <- .effect_rows(
    focus, levels, term, component, natural_scale, natural_estimate,
    natural_uncertainty$std_error, natural_uncertainty$lower,
    natural_uncertainty$upper, method
  )

  list(
    rows = rbind(link_rows, natural_rows),
    link_draws = link_draws,
    natural_draws = if (exists("natural_draws", inherits = FALSE)) natural_draws else NULL
  )
}

.term_source_variable <- function(term, data) {
  if (term %in% names(data)) return(term)
  vars <- tryCatch(all.vars(stats::as.formula(paste("~", term))), error = function(e) character())
  vars <- intersect(vars, names(data))
  if (length(vars)) vars[1] else NULL
}

.term_level_values <- function(data, term, contribution, bins = 20L) {
  source <- .term_source_variable(term, data)
  value <- if (is.null(source)) contribution else data[[source]]

  if (is.numeric(value) && length(unique(value)) > bins) {
    breaks <- unique(stats::quantile(value, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE))
    if (length(breaks) > 2L) {
      value <- cut(value, breaks = breaks, include.lowest = TRUE, ordered_result = TRUE)
    }
  }
  as.character(value)
}

.term_composition <- function(data, focus, focus_info, term, contribution,
                              weights, bins = 20L) {
  value <- .term_level_values(data, term, contribution, bins)
  keep <- weights > 0 & !is.na(value) & is.finite(contribution)
  value <- value[keep]
  contribution <- contribution[keep]
  weights <- weights[keep]
  focus_value <- focus_info$value[keep]

  key <- interaction(focus_value, value, drop = TRUE, lex.order = TRUE)
  rows <- split(seq_along(key), key)
  out <- do.call(rbind, lapply(rows, function(i) {
    data.frame(
      focus = focus,
      level = focus_value[i[1]],
      term = term,
      term_level = value[i[1]],
      n = length(i),
      weight = sum(weights[i]),
      effect = stats::weighted.mean(contribution[i], weights[i]),
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  totals <- stats::ave(out$weight, out$level, FUN = sum)
  out$proportion <- out$weight / totals
  out
}

.coefficient_summary <- function(composition, method = "none") {
  if (!nrow(composition)) return(data.frame())
  key <- interaction(composition$term, composition$term_level, drop = TRUE)
  rows <- split(seq_len(nrow(composition)), key)
  out <- do.call(rbind, lapply(rows, function(i) {
    data.frame(
      term = composition$term[i[1]],
      level = composition$term_level[i[1]],
      estimate = stats::weighted.mean(composition$effect[i], composition$weight[i]),
      std_error = NA_real_,
      lower = NA_real_,
      upper = NA_real_,
      method = method,
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}

.coefficient_term_summary <- function(data, term, X, beta, weights,
                                      beta_draws = NULL, vcov = NULL,
                                      method = "none", probs = c(0.025, 0.975),
                                      bins = 20L) {
  contribution <- as.numeric(X %*% beta)
  value <- .term_level_values(data, term, contribution, bins)
  keep <- weights > 0 & !is.na(value) & is.finite(contribution)
  value <- value[keep]
  X <- X[keep, , drop = FALSE]
  weights <- weights[keep]
  rows <- split(seq_along(value), factor(value, levels = unique(value)))
  out <- do.call(rbind, lapply(names(rows), function(level) {
    i <- rows[[level]]
    design <- .weighted_col_mean(X[i, , drop = FALSE], weights[i])
    estimate <- sum(design * beta)
    if (!is.null(beta_draws)) {
      stats <- .summarise_vector(as.numeric(beta_draws %*% design), probs)
    } else if (!is.null(vcov)) {
      se <- sqrt(max(0, as.numeric(t(design) %*% vcov %*% design)))
      z <- stats::qnorm(probs)
      stats <- c(
        estimate = estimate,
        std_error = se,
        lower = estimate + z[1] * se,
        upper = estimate + z[2] * se
      )
    } else {
      stats <- c(estimate = estimate, std_error = NA, lower = NA, upper = NA)
    }
    data.frame(
      term = term,
      level = level,
      estimate = unname(stats["estimate"]),
      std_error = unname(stats["std_error"]),
      lower = unname(stats["lower"]),
      upper = unname(stats["upper"]),
      method = method,
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}

.metric_values <- function(link) {
  position <- seq_along(link)
  slope <- if (length(link) > 1L) {
    stats::cov(position, link) / stats::var(position)
  } else {
    NA_real_
  }
  c(
    overall = exp(mean(abs(link))) - 1,
    trend = if (is.finite(slope)) exp(slope) - 1 else NA_real_
  )
}

.term_metrics <- function(term, component, link_estimate, link_draws = NULL,
                          method = "none", probs = c(0.025, 0.975)) {
  point <- .metric_values(link_estimate)
  do.call(rbind, lapply(names(point), function(metric) {
    if (!is.null(link_draws)) {
      values <- apply(link_draws, 1L, function(z) .metric_values(z)[metric])
      stats <- .summarise_vector(values, probs)
    } else {
      stats <- c(
        estimate = point[[metric]], std_error = NA_real_,
        lower = NA_real_, upper = NA_real_
      )
    }
    data.frame(
      term = term,
      component = component,
      metric = metric,
      estimate = unname(stats["estimate"]),
      std_error = unname(stats["std_error"]),
      lower = unname(stats["lower"]),
      upper = unname(stats["upper"]),
      method = method,
      stringsAsFactors = FALSE
    )
  }))
}

.nominal_indices <- function(data, response, focus, focus_info, weights,
                             probs = c(0.025, 0.975)) {
  if (is.null(response) || !response %in% names(data)) return(data.frame())
  y <- data[[response]]
  if (!is.numeric(y)) return(data.frame())

  out <- do.call(rbind, lapply(focus_info$levels, function(level) {
    keep <- focus_info$value == level & is.finite(y) & weights > 0
    level_y <- y[keep]
    level_weights <- weights[keep]
    if (!length(level_y)) {
      est <- se <- NA_real_
    } else {
      weight_sum <- sum(level_weights)
      squared_weight_sum <- sum(level_weights^2)
      effective_n <- weight_sum^2 / squared_weight_sum
      est <- stats::weighted.mean(level_y, level_weights)
      variance_denominator <- weight_sum - squared_weight_sum / weight_sum
      variance <- if (variance_denominator > 0) {
        sum(level_weights * (level_y - est)^2) / variance_denominator
      } else {
        NA_real_
      }
      se <- sqrt(variance / effective_n)
    }
    z <- stats::qnorm(probs)
    data.frame(
      focus = focus,
      level = as.character(level),
      series = "nominal",
      estimate = est,
      std_error = se,
      lower = est + z[1] * se,
      upper = est + z[2] * se,
      scale = "response",
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}

.standardised_indices <- function(influence, focus, focus_terms) {
  if (!length(focus_terms)) return(data.frame())
  if (length(focus_terms) > 1L) {
    warning(
      "A standardised index was not created because multiple model terms ",
      "contain the focus variable. Use a model-specific prediction grid to ",
      "define how interactions should be marginalised.",
      call. = FALSE
    )
    return(data.frame())
  }
  natural <- influence[
    influence$term %in% focus_terms & influence$scale != "link",
    , drop = FALSE
  ]
  if (!nrow(natural)) return(data.frame())
  natural <- natural[natural$term == focus_terms[1], , drop = FALSE]
  data.frame(
    focus = focus,
    level = natural$level,
    series = "standardised",
    estimate = natural$estimate,
    std_error = natural$std_error,
    lower = natural$lower,
    upper = natural$upper,
    scale = natural$scale,
    stringsAsFactors = FALSE
  )
}

.focus_terms <- function(term_columns, focus, data) {
  names(term_columns)[vapply(names(term_columns), function(term) {
    focus %in% (all.vars(tryCatch(
      stats::as.formula(paste("~", term)),
      error = function(e) stats::as.formula("~ 1")
    )))
  }, logical(1))]
}

.save_derived_draws <- function(draws, retain, draws_path = NULL) {
  if (retain == "summary") return(list(draws = NULL, path = NULL))
  if (retain == "derived_draws") return(list(draws = draws, path = NULL))
  if (retain == "disk") {
    if (is.null(draws_path)) {
      stop("`draws_path` is required when `retain = \"disk\"`.", call. = FALSE)
    }
    saveRDS(draws, draws_path, compress = FALSE)
    return(list(draws = NULL, path = normalizePath(draws_path, mustWork = TRUE)))
  }
  stop("Unknown retention mode '", retain, "'.", call. = FALSE)
}

.influ_linear_engine <- function(backend, model, data, response, focus,
                                 family_spec, X, beta, vcov, term_columns,
                                 uncertainty = "auto", retain = "summary",
                                 probs = c(0.025, 0.975), weights = NULL,
                                 reference_data = NULL, reference_X = NULL,
                                 reference_weights = NULL,
                                 component = "conditional", beta_draws = NULL,
                                 ndraws = 1000L, seed = NULL,
                                 draws_path = NULL, keep_model = FALSE,
                                 notes = character()) {
  uncertainty <- match.arg(
    uncertainty,
    c("auto", "none", "analytic", "posterior", "simulation")
  )
  retain <- match.arg(retain, c("summary", "derived_draws", "disk"))
  probs <- .validate_probs(probs)
  data <- as.data.frame(data)
  weights <- .resolve_influ_weights(data, weights)
  focus_info <- .focus_info(data, focus)

  if (!is.matrix(X) && !inherits(X, "Matrix")) X <- as.matrix(X)
  beta <- as.numeric(beta)
  if (nrow(X) != nrow(data) || ncol(X) != length(beta)) {
    stop("The model matrix, coefficient vector, and model data do not conform.", call. = FALSE)
  }
  if (!is.null(vcov)) {
    vcov <- as.matrix(vcov)
    if (!all(dim(vcov) == c(length(beta), length(beta)))) {
      stop("The coefficient covariance matrix does not conform to the model matrix.", call. = FALSE)
    }
  }

  if (uncertainty == "auto") {
    uncertainty <- if (!is.null(beta_draws)) "posterior" else if (!is.null(vcov)) "analytic" else "none"
  }
  if (uncertainty == "posterior" && is.null(beta_draws)) {
    stop("Posterior uncertainty requires coefficient draws.", call. = FALSE)
  }
  if (uncertainty == "analytic" && is.null(vcov)) {
    stop("Analytic uncertainty requires a coefficient covariance matrix.", call. = FALSE)
  }
  if (uncertainty == "simulation") {
    if (is.null(vcov)) stop("Simulation uncertainty requires a coefficient covariance matrix.", call. = FALSE)
    beta_draws <- .draw_mvn(ndraws, beta, vcov, seed = seed)
  }
  if (uncertainty == "none") {
    vcov <- NULL
    beta_draws <- NULL
  } else if (!uncertainty %in% c("analytic")) {
    vcov <- NULL
  }

  if (!is.null(beta_draws)) {
    beta_draws <- as.matrix(beta_draws)
    if (ncol(beta_draws) != length(beta)) {
      stop("Coefficient draws do not conform to the model matrix.", call. = FALSE)
    }
  }

  reference <- .resolve_reference(
    data, X, reference_data, reference_X, reference_weights
  )

  reference_design <- if (reference$explicit) {
    .weighted_col_mean(reference$X, reference$weights)
  } else {
    .weighted_col_mean(X, weights)
  }
  eta_reference <- sum(reference_design * beta)
  eta_reference_draws <- if (!is.null(beta_draws)) {
    as.numeric(beta_draws %*% reference_design)
  } else {
    NULL
  }

  effect_list <- list()
  composition_list <- list()
  coefficient_list <- list()
  metric_list <- list()
  retained_list <- list()
  method <- switch(
    uncertainty,
    none = "none",
    analytic = "analytic covariance",
    posterior = "posterior draws",
    simulation = "joint coefficient simulation"
  )

  for (term in names(term_columns)) {
    columns <- term_columns[[term]]
    columns <- columns[columns >= 1L & columns <= ncol(X)]
    if (!length(columns)) next

    B_term <- .term_contrast(
      X[, columns, drop = FALSE], focus_info, weights,
      reference = if (reference$explicit) reference$X[, columns, drop = FALSE] else NULL,
      reference_weights = if (reference$explicit) reference$weights else NULL
    )
    B <- matrix(0, nrow = nrow(B_term), ncol = ncol(X))
    B[, columns] <- B_term

    component_result <- .component_influence(
      B = B,
      beta = beta,
      vcov = vcov,
      beta_draws = beta_draws,
      eta_reference = eta_reference,
      eta_reference_draws = eta_reference_draws,
      family_spec = family_spec,
      focus = focus,
      levels = focus_info$levels,
      term = term,
      component = component,
      method = method,
      probs = probs
    )
    effect_list[[term]] <- component_result$rows
    metric_list[[term]] <- .term_metrics(
      term = term,
      component = component,
      link_estimate = component_result$rows$estimate[
        component_result$rows$scale == "link"
      ],
      link_draws = component_result$link_draws,
      method = method,
      probs = probs
    )

    if (!is.null(component_result$link_draws)) {
      draw_names <- paste(component, term, focus_info$levels, "link", sep = "|")
      colnames(component_result$link_draws) <- draw_names
      retained_list[[paste0(term, "_link")]] <- component_result$link_draws
      natural_names <- paste(component, term, focus_info$levels, family_spec$natural_scale, sep = "|")
      colnames(component_result$natural_draws) <- natural_names
      retained_list[[paste0(term, "_natural")]] <- component_result$natural_draws
    }

    contribution <- as.numeric(X[, columns, drop = FALSE] %*% beta[columns])
    composition_list[[term]] <- .term_composition(
      data, focus, focus_info, term, contribution, weights
    )
    coefficient_list[[term]] <- .coefficient_term_summary(
      data = data,
      term = term,
      X = X[, columns, drop = FALSE],
      beta = beta[columns],
      weights = weights,
      beta_draws = if (is.null(beta_draws)) NULL else beta_draws[, columns, drop = FALSE],
      vcov = if (is.null(vcov)) NULL else vcov[columns, columns, drop = FALSE],
      method = method,
      probs = probs
    )
  }

  if (!length(effect_list)) {
    stop("No model terms could be mapped to coefficient columns.", call. = FALSE)
  }

  influence <- do.call(rbind, effect_list)
  rownames(influence) <- NULL
  composition <- do.call(rbind, composition_list)
  rownames(composition) <- NULL
  composition$component <- component
  coefficients <- do.call(rbind, coefficient_list)
  rownames(coefficients) <- NULL
  if (nrow(coefficients)) coefficients$component <- component
  metrics <- do.call(rbind, metric_list)
  rownames(metrics) <- NULL

  derived_draws <- if (length(retained_list)) do.call(cbind, retained_list) else NULL
  stored <- .save_derived_draws(derived_draws, retain, draws_path)

  indices <- rbind(
    .nominal_indices(data, response, focus, focus_info, weights, probs),
    .standardised_indices(influence, focus, .focus_terms(term_columns, focus, data))
  )
  rownames(indices) <- NULL
  if (nrow(indices)) indices$component <- component

  new_influ_diag(
    backend = backend,
    family = family_spec,
    focus = focus,
    influence = influence,
    coefficients = coefficients,
    composition = composition,
    indices = indices,
    metrics = metrics,
    uncertainty = list(
      method = method,
      probs = probs,
      ndraws = if (is.null(beta_draws)) 0L else nrow(beta_draws)
    ),
    retained = list(
      mode = retain,
      path = stored$path,
      n_derived_draws = if (is.null(derived_draws)) 0L else nrow(derived_draws),
      n_derived_estimands = if (is.null(derived_draws)) 0L else ncol(derived_draws)
    ),
    metadata = list(
      call = match.call(),
      response = response,
      n_observations = nrow(data),
      terms = names(term_columns),
      reference = reference$label,
      n_reference = nrow(reference$data),
      notes = notes
    ),
    draws = stored$draws,
    model = if (isTRUE(keep_model)) model else NULL
  )
}

.influ_precomputed_engine <- function(backend, model, data, focus,
                                      family_spec, term_contrasts,
                                      term_contributions, beta,
                                      beta_draws = NULL, vcov = NULL,
                                      uncertainty = "posterior",
                                      retain = "summary",
                                      probs = c(0.025, 0.975),
                                      weights = NULL,
                                      component = "conditional",
                                      eta_reference = 0,
                                      eta_reference_draws = NULL,
                                      reference = "observed",
                                      draws_path = NULL,
                                      keep_model = FALSE,
                                      notes = character()) {
  retain <- match.arg(retain, c("summary", "derived_draws", "disk"))
  uncertainty <- match.arg(
    uncertainty,
    c("none", "analytic", "posterior", "simulation")
  )
  probs <- .validate_probs(probs)
  if (!length(term_contrasts) || !identical(names(term_contrasts), names(term_contributions))) {
    stop("Precomputed contrasts and contributions must be matching named lists.", call. = FALSE)
  }

  data <- as.data.frame(data)
  weights <- .resolve_influ_weights(data, weights)
  focus_info <- .focus_info(data, focus)
  beta <- as.numeric(beta)
  if (!is.null(beta_draws)) beta_draws <- as.matrix(beta_draws)
  if (uncertainty == "none") {
    beta_draws <- NULL
    vcov <- NULL
    eta_reference_draws <- NULL
  }
  if (uncertainty == "posterior" && is.null(beta_draws)) {
    stop("Posterior uncertainty requires coefficient draws.", call. = FALSE)
  }

  method <- switch(
    uncertainty,
    none = "none",
    analytic = "analytic covariance",
    posterior = "posterior draws",
    simulation = "joint coefficient simulation"
  )
  effects <- list()
  compositions <- list()
  retained_draws <- list()
  metric_rows <- list()

  for (term in names(term_contrasts)) {
    B <- as.matrix(term_contrasts[[term]])
    if (nrow(B) != length(focus_info$levels) || ncol(B) != length(beta)) {
      stop("A precomputed term contrast does not conform to its coefficients.", call. = FALSE)
    }
    result <- .component_influence(
      B = B,
      beta = beta,
      vcov = vcov,
      beta_draws = beta_draws,
      eta_reference = eta_reference,
      eta_reference_draws = eta_reference_draws,
      family_spec = family_spec,
      focus = focus,
      levels = focus_info$levels,
      term = term,
      component = component,
      method = method,
      probs = probs
    )
    effects[[term]] <- result$rows
    metric_rows[[term]] <- .term_metrics(
      term = term,
      component = component,
      link_estimate = result$rows$estimate[result$rows$scale == "link"],
      link_draws = result$link_draws,
      method = method,
      probs = probs
    )
    compositions[[term]] <- .term_composition(
      data = data,
      focus = focus,
      focus_info = focus_info,
      term = term,
      contribution = term_contributions[[term]],
      weights = weights
    )

    if (!is.null(result$link_draws)) {
      colnames(result$link_draws) <- paste(
        component, term, focus_info$levels, "link", sep = "|"
      )
      colnames(result$natural_draws) <- paste(
        component, term, focus_info$levels, family_spec$natural_scale,
        sep = "|"
      )
      retained_draws[[paste0(term, "_link")]] <- result$link_draws
      retained_draws[[paste0(term, "_natural")]] <- result$natural_draws
    }
  }

  influence <- do.call(rbind, effects)
  rownames(influence) <- NULL
  composition <- do.call(rbind, compositions)
  rownames(composition) <- NULL
  composition$component <- component
  coefficients <- .coefficient_summary(composition)
  if (nrow(coefficients)) coefficients$component <- component
  derived_draws <- if (length(retained_draws)) do.call(cbind, retained_draws) else NULL
  stored <- .save_derived_draws(derived_draws, retain, draws_path)
  metrics <- do.call(rbind, metric_rows)
  rownames(metrics) <- NULL

  new_influ_diag(
    backend = backend,
    family = family_spec,
    focus = focus,
    influence = influence,
    coefficients = coefficients,
    composition = composition,
    metrics = metrics,
    uncertainty = list(
      method = method,
      probs = probs,
      ndraws = if (is.null(beta_draws)) 0L else nrow(beta_draws)
    ),
    retained = list(
      mode = retain,
      path = stored$path,
      n_derived_draws = if (is.null(derived_draws)) 0L else nrow(derived_draws),
      n_derived_estimands = if (is.null(derived_draws)) 0L else ncol(derived_draws)
    ),
    metadata = list(
      response = NULL,
      n_observations = nrow(data),
      terms = names(term_contrasts),
      reference = reference,
      notes = notes
    ),
    draws = stored$draws,
    model = if (isTRUE(keep_model)) model else NULL
  )
}

.response_mean_from_eta <- function(eta, family_spec) {
  if (identical(family_spec$family, "lognormal")) return(exp(eta))
  .link_inverse(eta, family_spec$link)
}

.summarise_draw_matrix <- function(draws, method, probs) {
  if (method == "none") {
    return(cbind(
      estimate = as.numeric(draws[1, ]),
      std_error = NA_real_,
      lower = NA_real_,
      upper = NA_real_
    ))
  }
  t(apply(draws, 2L, .summarise_vector, probs = probs))
}

.diag_from_term_draws <- function(backend, model, data, focus, family_spec,
                                  term_draws, point_contributions,
                                  eta_reference_draws, weights = NULL,
                                  component = "conditional",
                                  retain = "summary",
                                  probs = c(0.025, 0.975),
                                  draws_path = NULL,
                                  keep_model = FALSE,
                                  method = "joint precision simulation",
                                  response = NULL,
                                  reference = "observed",
                                  notes = character()) {
  if (!length(term_draws) || !identical(names(term_draws), names(point_contributions))) {
    stop("Term draws and point contributions must be matching named lists.", call. = FALSE)
  }
  data <- as.data.frame(data)
  weights <- .resolve_influ_weights(data, weights)
  focus_info <- .focus_info(data, focus)
  n_draws <- nrow(term_draws[[1]])
  if (length(eta_reference_draws) != n_draws) {
    stop("Reference-predictor draws do not conform to the term draws.", call. = FALSE)
  }

  effects <- list()
  compositions <- list()
  coefficients <- list()
  metrics <- list()
  retained <- list()
  identity <- diag(length(focus_info$levels))
  for (term in names(term_draws)) {
    draws <- as.matrix(term_draws[[term]])
    if (!all(dim(draws) == c(n_draws, length(focus_info$levels)))) {
      stop("A compact term-draw matrix has incompatible dimensions.", call. = FALSE)
    }
    result <- .component_influence(
      B = identity,
      beta = colMeans(draws),
      vcov = NULL,
      beta_draws = draws,
      eta_reference = mean(eta_reference_draws),
      eta_reference_draws = eta_reference_draws,
      family_spec = family_spec,
      focus = focus,
      levels = focus_info$levels,
      term = term,
      component = component,
      method = method,
      probs = probs
    )
    effects[[term]] <- result$rows
    compositions[[term]] <- .term_composition(
      data, focus, focus_info, term, point_contributions[[term]], weights
    )
    coefficients[[term]] <- .coefficient_summary(compositions[[term]], method)
    metrics[[term]] <- .term_metrics(
      term, component,
      result$rows$estimate[result$rows$scale == "link"],
      result$link_draws,
      method, probs
    )
    colnames(result$link_draws) <- paste(
      component, term, focus_info$levels, "link", sep = "|"
    )
    colnames(result$natural_draws) <- paste(
      component, term, focus_info$levels, family_spec$natural_scale, sep = "|"
    )
    retained[[paste0(term, "_link")]] <- result$link_draws
    retained[[paste0(term, "_natural")]] <- result$natural_draws
  }

  influence <- do.call(rbind, effects)
  composition <- do.call(rbind, compositions)
  coefficients <- do.call(rbind, coefficients)
  metric_table <- do.call(rbind, metrics)
  rownames(influence) <- rownames(composition) <- rownames(coefficients) <- rownames(metric_table) <- NULL
  composition$component <- component
  coefficients$component <- component
  derived_draws <- do.call(cbind, retained)
  stored <- .save_derived_draws(derived_draws, retain, draws_path)

  new_influ_diag(
    backend = backend,
    family = family_spec,
    focus = focus,
    influence = influence,
    coefficients = coefficients,
    composition = composition,
    metrics = metric_table,
    uncertainty = list(method = method, probs = probs, ndraws = n_draws),
    retained = list(
      mode = retain,
      path = stored$path,
      n_derived_draws = n_draws,
      n_derived_estimands = ncol(derived_draws)
    ),
    metadata = list(
      response = response,
      n_observations = nrow(data),
      terms = names(term_draws),
      reference = reference,
      notes = notes
    ),
    draws = stored$draws,
    model = if (isTRUE(keep_model)) model else NULL
  )
}

.two_part_combined_diag <- function(backend, model, data, response, focus,
                                    family_spec, main_projection,
                                    probability_projection,
                                    probability_is_zero = TRUE,
                                    weights = NULL,
                                    retain = "summary",
                                    probs = c(0.025, 0.975),
                                    draws_path = NULL,
                                    keep_model = FALSE,
                                    notes = character()) {
  retain <- match.arg(retain, c("summary", "derived_draws", "disk"))
  n_draws <- length(main_projection$eta_reference)
  if (length(probability_projection$eta_reference) != n_draws) {
    stop("The two model components do not contain matching joint draws.", call. = FALSE)
  }
  focus_info <- .focus_info(data, focus)
  terms <- union(
    names(main_projection$term_deltas),
    names(probability_projection$term_deltas)
  )
  if (!length(terms)) return(NULL)
  method <- main_projection$method

  p0 <- .link_inverse(
    probability_projection$eta_reference,
    probability_projection$family_spec$link
  )
  if (probability_is_zero) p0 <- 1 - p0
  mu0 <- .response_mean_from_eta(
    main_projection$eta_reference,
    main_projection$family_spec
  )
  baseline <- p0 * mu0
  if (any(!is.finite(baseline)) || any(baseline <= 0)) {
    stop(
      "The two-part model produced a non-positive or non-finite reference ",
      "mean, so unconditional influence ratios cannot be calculated.",
      call. = FALSE
    )
  }
  rows <- list()
  retained_draws <- list()
  metric_rows <- list()

  for (term in terms) {
    main_delta <- main_projection$term_deltas[[term]]
    probability_delta <- probability_projection$term_deltas[[term]]
    if (is.null(main_delta)) {
      main_delta <- matrix(0, nrow = n_draws, ncol = length(focus_info$levels))
    }
    if (is.null(probability_delta)) {
      probability_delta <- matrix(0, nrow = n_draws, ncol = length(focus_info$levels))
    }

    mu1 <- .response_mean_from_eta(
      sweep(main_delta, 1L, main_projection$eta_reference, "+"),
      main_projection$family_spec
    )
    p1 <- .link_inverse(
      sweep(
        probability_delta,
        1L,
        probability_projection$eta_reference,
        "+"
      ),
      probability_projection$family_spec$link
    )
    if (probability_is_zero) p1 <- 1 - p1
    ratio_draws <- sweep(p1 * mu1, 1L, baseline, "/")
    if (any(!is.finite(ratio_draws)) || any(ratio_draws <= 0)) {
      stop(
        "The two-part model produced non-positive or non-finite ",
        "unconditional influence ratios.",
        call. = FALSE
      )
    }
    link_draws <- log(ratio_draws)
    ratio_stats <- .summarise_draw_matrix(ratio_draws, method, probs)
    link_stats <- .summarise_draw_matrix(link_draws, method, probs)

    rows[[term]] <- rbind(
      .effect_rows(
        focus, focus_info$levels, term, "unconditional_mean", "link",
        link_stats[, "estimate"], link_stats[, "std_error"],
        link_stats[, "lower"], link_stats[, "upper"], method
      ),
      .effect_rows(
        focus, focus_info$levels, term, "unconditional_mean", "ratio",
        ratio_stats[, "estimate"], ratio_stats[, "std_error"],
        ratio_stats[, "lower"], ratio_stats[, "upper"], method
      )
    )
    metric_rows[[term]] <- .term_metrics(
      term, "unconditional_mean",
      link_stats[, "estimate"],
      if (method == "none") NULL else link_draws,
      method, probs
    )

    if (method != "none") {
      colnames(link_draws) <- paste(
        "unconditional_mean", term, focus_info$levels, "link", sep = "|"
      )
      colnames(ratio_draws) <- paste(
        "unconditional_mean", term, focus_info$levels, "ratio", sep = "|"
      )
      retained_draws[[paste0(term, "_link")]] <- link_draws
      retained_draws[[paste0(term, "_ratio")]] <- ratio_draws
    }
  }

  influence <- do.call(rbind, rows)
  rownames(influence) <- NULL
  derived_draws <- if (length(retained_draws)) {
    do.call(cbind, retained_draws)
  } else {
    NULL
  }
  stored <- .save_derived_draws(derived_draws, retain, draws_path)
  metrics <- do.call(rbind, metric_rows)
  rownames(metrics) <- NULL
  nominal <- .nominal_indices(
    data, response, focus, focus_info,
    .resolve_influ_weights(data, weights), probs
  )
  focus_terms <- terms[vapply(terms, function(term) {
    focus %in% all.vars(tryCatch(
      stats::as.formula(paste("~", term)),
      error = function(e) stats::as.formula("~ 1")
    ))
  }, logical(1))]
  indices <- rbind(
    nominal,
    .standardised_indices(influence, focus, focus_terms)
  )
  if (nrow(indices)) indices$component <- "unconditional_mean"

  new_influ_diag(
    backend = backend,
    family = family_spec,
    focus = focus,
    influence = influence,
    indices = indices,
    metrics = metrics,
    uncertainty = list(
      method = method,
      probs = probs,
      ndraws = if (method == "none") 0L else n_draws
    ),
    retained = list(
      mode = retain,
      path = stored$path,
      n_derived_draws = if (method == "none") 0L else n_draws,
      n_derived_estimands = if (is.null(derived_draws)) 0L else ncol(derived_draws)
    ),
    metadata = list(
      response = response,
      n_observations = nrow(data),
      terms = terms,
      reference = main_projection$reference %||% "observed",
      notes = notes
    ),
    draws = stored$draws,
    model = if (isTRUE(keep_model)) model else NULL
  )
}

.combine_influ_diags <- function(diags, backend, family_spec, focus,
                                 model = NULL, keep_model = FALSE,
                                 notes = character()) {
  diags <- Filter(Negate(is.null), diags)
  if (!length(diags)) stop("No component diagnostics were supplied.", call. = FALSE)

  bind_table <- function(name) {
    pieces <- lapply(diags, `[[`, name)
    pieces <- Filter(function(x) is.data.frame(x) && nrow(x), pieces)
    if (!length(pieces)) return(data.frame())
    out <- do.call(rbind, pieces)
    rownames(out) <- NULL
    out
  }

  draw_pieces <- lapply(diags, `[[`, "draws")
  draw_pieces <- Filter(Negate(is.null), draw_pieces)
  if (length(unique(vapply(draw_pieces, nrow, integer(1)))) > 1L) {
    stop(
      "Component diagnostics must retain the same number of joint draws.",
      call. = FALSE
    )
  }
  draws <- if (length(draw_pieces)) do.call(cbind, draw_pieces) else NULL

  methods <- unique(vapply(diags, function(x) x$uncertainty$method, character(1)))
  modes <- unique(vapply(diags, function(x) x$retained$mode, character(1)))
  combined_mode <- if ("disk" %in% modes) {
    "disk"
  } else if ("derived_draws" %in% modes) {
    "derived_draws"
  } else {
    "summary"
  }

  new_influ_diag(
    backend = backend,
    family = family_spec,
    focus = focus,
    influence = bind_table("influence"),
    coefficients = bind_table("coefficients"),
    composition = bind_table("composition"),
    indices = bind_table("indices"),
    metrics = bind_table("metrics"),
    uncertainty = list(
      method = paste(methods, collapse = "; "),
      probs = diags[[1]]$uncertainty$probs,
      ndraws = max(vapply(diags, function(x) x$uncertainty$ndraws, numeric(1)))
    ),
    retained = list(
      mode = combined_mode,
      path = NULL,
      n_derived_draws = if (is.null(draws)) 0L else nrow(draws),
      n_derived_estimands = if (is.null(draws)) 0L else ncol(draws)
    ),
    metadata = list(
      response = diags[[1]]$metadata$response,
      n_observations = diags[[1]]$metadata$n_observations,
      terms = unique(unlist(lapply(diags, function(x) x$metadata$terms))),
      reference = paste(unique(vapply(diags, function(x) x$metadata$reference %||% "observed", character(1))), collapse = "; "),
      notes = unique(c(notes, unlist(lapply(diags, function(x) x$metadata$notes))))
    ),
    draws = draws,
    model = if (isTRUE(keep_model)) model else NULL
  )
}
