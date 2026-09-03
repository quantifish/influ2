.resolve_influ_data <- function(model, data = NULL) {
  if (!is.null(data)) return(as.data.frame(data))

  out <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  if (!is.null(out)) return(as.data.frame(out))

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
  if (anyNA(value)) {
    stop("The focus variable contains missing values.", call. = FALSE)
  }
  levels <- if (is.factor(value)) {
    levels(droplevels(value))
  } else {
    unique(as.character(value))
  }
  list(value = as.character(value), levels = levels)
}

.weighted_col_mean <- function(x, w) {
  if (!nrow(x)) return(rep(NA_real_, ncol(x)))
  colSums(x * w) / sum(w)
}

.term_contrast <- function(x, focus_info, weights) {
  overall <- .weighted_col_mean(x, weights)
  out <- matrix(NA_real_, nrow = length(focus_info$levels), ncol = ncol(x))
  rownames(out) <- focus_info$levels
  colnames(out) <- colnames(x)
  for (i in seq_along(focus_info$levels)) {
    keep <- focus_info$value == focus_info$levels[i]
    out[i, ] <- .weighted_col_mean(x[keep, , drop = FALSE], weights[keep]) - overall
  }
  out
}

.normal_quantile <- function(level) {
  stats::qnorm((1 + level) / 2)
}

.draw_mvn <- function(n, mean, sigma, seed = NULL) {
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
                                 levels, term, component, method, probs,
                                 confidence_level) {
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
    z <- .normal_quantile(confidence_level)
    link_uncertainty <- list(
      std_error = se,
      lower = link_estimate - z * se,
      upper = link_estimate + z * se
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
    z <- .normal_quantile(confidence_level)
    natural_uncertainty <- list(
      std_error = ifelse(
        is.finite(lower) & is.finite(upper),
        abs(upper - lower) / (2 * z),
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

.term_composition <- function(data, focus, focus_info, term, contribution,
                              weights, bins = 20L) {
  source <- .term_source_variable(term, data)
  value <- if (is.null(source)) contribution else data[[source]]

  if (is.numeric(value) && length(unique(value)) > bins) {
    breaks <- unique(stats::quantile(value, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE))
    if (length(breaks) > 2L) {
      value <- cut(value, breaks = breaks, include.lowest = TRUE, ordered_result = TRUE)
    }
  }
  value <- as.character(value)

  key <- interaction(focus_info$value, value, drop = TRUE, lex.order = TRUE)
  rows <- split(seq_along(key), key)
  out <- do.call(rbind, lapply(rows, function(i) {
    data.frame(
      focus = focus,
      level = focus_info$value[i[1]],
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

.coefficient_summary <- function(composition) {
  if (!nrow(composition)) return(data.frame())
  key <- interaction(composition$term, composition$term_level, drop = TRUE)
  rows <- split(seq_len(nrow(composition)), key)
  out <- do.call(rbind, lapply(rows, function(i) {
    data.frame(
      term = composition$term[i[1]],
      level = composition$term_level[i[1]],
      estimate = stats::weighted.mean(composition$effect[i], composition$weight[i]),
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}

.nominal_indices <- function(data, response, focus, focus_info, weights) {
  if (is.null(response) || !response %in% names(data)) return(data.frame())
  y <- data[[response]]
  if (!is.numeric(y)) return(data.frame())

  out <- do.call(rbind, lapply(focus_info$levels, function(level) {
    keep <- focus_info$value == level
    est <- stats::weighted.mean(y[keep], weights[keep])
    se <- stats::sd(y[keep]) / sqrt(sum(keep))
    data.frame(
      focus = focus,
      level = as.character(level),
      series = "nominal",
      estimate = est,
      std_error = se,
      lower = est - stats::qnorm(0.975) * se,
      upper = est + stats::qnorm(0.975) * se,
      scale = "response",
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}

.standardised_indices <- function(influence, focus, focus_terms) {
  if (!length(focus_terms)) return(data.frame())
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
                                 component = "conditional", beta_draws = NULL,
                                 ndraws = 1000L, seed = NULL,
                                 draws_path = NULL, keep_model = FALSE,
                                 notes = character()) {
  uncertainty <- match.arg(
    uncertainty,
    c("auto", "none", "analytic", "posterior", "simulation")
  )
  retain <- match.arg(retain, c("summary", "derived_draws", "disk"))
  if (length(probs) != 2L || any(probs <= 0 | probs >= 1) || probs[1] >= probs[2]) {
    stop("`probs` must contain two increasing probabilities between zero and one.", call. = FALSE)
  }
  confidence_level <- probs[2] - probs[1]

  data <- as.data.frame(data)
  weights <- .resolve_influ_weights(data, weights)
  focus_info <- .focus_info(data, focus)

  X <- as.matrix(X)
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

  eta_reference <- sum(.weighted_col_mean(X, weights) * beta)
  eta_reference_draws <- if (!is.null(beta_draws)) {
    as.numeric(beta_draws %*% .weighted_col_mean(X, weights))
  } else {
    NULL
  }

  effect_list <- list()
  composition_list <- list()
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

    B_term <- .term_contrast(X[, columns, drop = FALSE], focus_info, weights)
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
      probs = probs,
      confidence_level = confidence_level
    )
    effect_list[[term]] <- component_result$rows

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
  }

  if (!length(effect_list)) {
    stop("No model terms could be mapped to coefficient columns.", call. = FALSE)
  }

  influence <- do.call(rbind, effect_list)
  rownames(influence) <- NULL
  composition <- do.call(rbind, composition_list)
  rownames(composition) <- NULL
  composition$component <- component
  coefficients <- .coefficient_summary(composition)
  if (nrow(coefficients)) coefficients$component <- component

  derived_draws <- if (length(retained_list)) do.call(cbind, retained_list) else NULL
  stored <- .save_derived_draws(derived_draws, retain, draws_path)

  indices <- rbind(
    .nominal_indices(data, response, focus, focus_info, weights),
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
      reference = "observed",
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
  draws <- if (length(draw_pieces)) do.call(cbind, draw_pieces) else NULL

  methods <- unique(vapply(diags, function(x) x$uncertainty$method, character(1)))
  modes <- unique(vapply(diags, function(x) x$retained$mode, character(1)))

  new_influ_diag(
    backend = backend,
    family = family_spec,
    focus = focus,
    influence = bind_table("influence"),
    coefficients = bind_table("coefficients"),
    composition = bind_table("composition"),
    indices = bind_table("indices"),
    uncertainty = list(
      method = paste(methods, collapse = "; "),
      probs = diags[[1]]$uncertainty$probs,
      ndraws = max(vapply(diags, function(x) x$uncertainty$ndraws, numeric(1)))
    ),
    retained = list(
      mode = paste(modes, collapse = "; "),
      path = NULL,
      n_derived_draws = if (is.null(draws)) 0L else nrow(draws),
      n_derived_estimands = if (is.null(draws)) 0L else ncol(draws)
    ),
    metadata = list(
      response = diags[[1]]$metadata$response,
      n_observations = diags[[1]]$metadata$n_observations,
      terms = unique(unlist(lapply(diags, function(x) x$metadata$terms))),
      reference = "observed",
      notes = unique(c(notes, unlist(lapply(diags, function(x) x$metadata$notes))))
    ),
    draws = draws,
    model = if (isTRUE(keep_model)) model else NULL
  )
}
