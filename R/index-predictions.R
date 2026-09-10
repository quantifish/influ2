.index_prediction_guards <- function(model, backend) {
  if (backend == "brms") {
    if (!requireNamespace("brms", quietly = TRUE) ||
        !requireNamespace("posterior", quietly = TRUE)) {
      stop("Packages 'brms' and 'posterior' are required.", call. = FALSE)
    }
    if (is.null(model$fit) || inherits(model$formula, "mvbrmsformula")) {
      stop("Use an original complete, univariate brms fit, not a compact influence fixture.", call. = FALSE)
    }
    frm <- stats::formula(model)$formula
    lhs <- frm[[2]]
    if (is.call(lhs) && identical(lhs[[1]], as.name("|"))) {
      additions <- all.names(lhs[[3]], functions = TRUE)
      if (any(!additions %in% c("trials", all.vars(lhs[[3]]))) || !is.name(lhs[[2]])) {
        stop("brms response additions other than trials are not supported for CPUE indices.", call. = FALSE)
      }
    }
    forms <- c(list(frm), lapply(model$formula$pforms, function(x) {
      if (inherits(x, "brmsformula")) x$formula else x
    }))
    calls <- unlist(lapply(forms, function(f) all.names(f, functions = TRUE)))
    if (!is.null(model$autocor) ||
        any(calls %in% c("gp", "ar", "ma", "arma", "cosy", "sar", "car", "fcor", "unstr"))) {
      stop("brms autocorrelation and Gaussian-process predictions require a dedicated joint prediction adapter.", call. = FALSE)
    }
  } else {
    frm <- stats::formula(model)
    if (!is.null(model$call$offset)) {
      stop("Put exposure offsets inside the model formula before calculating an index; fit-call offsets are not supported.", call. = FALSE)
    }
  }
  response <- frm[[2]]
  if (!is.name(response) && !identical(response[[1]], as.name("cbind")) &&
      !(backend == "brms" && identical(response[[1]], as.name("|")))) {
    stop("Use a model of the natural response, not a transformed response, for a CPUE index.", call. = FALSE)
  }
  if (backend == "glmmTMB") {
    if (isTRUE(model$modelInfo$REML) ||
        length(model$modelInfo$reTrms$zi$cnms)) {
      stop("glmmTMB response indices currently require ML and no zero-component random effects.", call. = FALSE)
    }
    if (!isTRUE(model$sdr$pdHess) || model$fit$convergence != 0) {
      stop("The glmmTMB fit must have converged with a positive-definite Hessian.", call. = FALSE)
    }
  }
}

.index_reference_predictors <- function(model, backend, year, reference_data) {
  forms <- if (backend == "glmmTMB") {
    model$modelInfo$allForm[c("formula", "ziformula", "dispformula")]
  } else if (backend == "brms") {
    c(list(model$formula$formula), lapply(model$formula$pforms, function(x) {
      if (inherits(x, "brmsformula")) x$formula else x
    }))
  } else list(stats::formula(model))
  required <- unique(unlist(lapply(forms, function(f) {
    if (!inherits(f, "formula")) return(character())
    all.vars(f[[length(f)]])
  })))
  if (backend == "glmmTMB") {
    required <- unique(unlist(lapply(c("cond", "zi", "disp"), function(component) {
      term <- stats::terms(model, component = component)
      if (is.null(term)) character() else all.vars(stats::delete.response(term))
    })))
  }
  if (backend == "brms") {
    parsed <- brms::brmsterms(model$formula)
    required <- unique(unlist(lapply(parsed$dpars, function(part) {
      # Do not remove a group name also used as a population-level predictor.
      # Exact lookup: for distributional terms without random effects, $re
      # can partially match the character-valued $resp field.
      union(setdiff(all.vars(part$allvars), part[["re"]][["group"]]),
        unlist(lapply(part[c("fe", "sm")], all.vars)))
    })))
    lhs <- model$formula$formula[[2]]
    if (is.call(lhs) && identical(lhs[[1]], as.name("|"))) {
      required <- union(required, all.vars(lhs[[3]]))
    }
  }
  missing <- setdiff(required, c(year, names(reference_data)))
  if (length(missing)) {
    stop("Supply all reference predictors explicitly; missing: ",
      paste(missing, collapse = ", "), ".", call. = FALSE)
  }
}

.index_check_prediction <- function(x, n) {
  if (!is.numeric(x) || length(dim(x)) > 2L ||
      (is.matrix(x) && ncol(x) != 1L) || length(x) != n || any(!is.finite(x))) {
    stop("Native prediction must return one finite expected response per reference row.", call. = FALSE)
  }
  as.numeric(x)
}

.index_check_covariance <- function(covariance, n) {
  if (!is.matrix(covariance) || !identical(dim(covariance), c(n, n)) ||
      any(!is.finite(covariance))) {
    stop("A finite joint parameter covariance is required for index uncertainty.", call. = FALSE)
  }
  covariance <- (covariance + t(covariance)) / 2
  e <- eigen(covariance, symmetric = TRUE, only.values = TRUE)$values
  if (any(e < -sqrt(.Machine$double.eps) * max(abs(e)))) {
    stop("The parameter covariance is not positive semi-definite.", call. = FALSE)
  }
  covariance
}

.index_frequentist <- function(model, backend, years, batches, newdata, weights, uncertainty, batch_size) {
  beta <- if (backend == "glmmTMB") model$fit$par else stats::coef(model)
  if (any(!is.finite(beta))) {
    stop("Rank-deficient or non-finite model coefficients are not supported for response indices.", call. = FALSE)
  }
  covariance <- NULL
  if (uncertainty) {
    covariance <- if (backend == "glmmTMB") model$sdr$cov.fixed else {
      if (backend == "gam") stats::vcov(model, unconditional = !is.null(model$Vc)) else stats::vcov(model)
    }
    covariance <- .index_check_covariance(as.matrix(covariance), length(beta))
    if (!identical(rownames(covariance), names(beta))) {
      stop("Parameter covariance order does not match the fitted coefficients.", call. = FALSE)
    }
  }
  predict_mean <- function(d, parameters = beta) {
    .index_check_prediction(stats::predict(model, newdata = d,
      type = "response", re.form = NA, newparams = parameters), nrow(d))
  }
  estimate <- numeric(length(years))
  gradient <- matrix(0, length(years), length(beta))
  # Batch across years as well as reference rows, avoiding a native TMB
  # prediction setup per year when a small reference profile is supplied.
  n_reference <- length(weights)
  block_size <- batch_size
  n_prediction <- length(years) * n_reference
  for (first in seq(1, n_prediction, by = block_size)) {
      index <- seq.int(first, min(n_prediction, first + block_size - 1L))
      i <- as.integer((index - 1L) %/% n_reference + 1L)
      rows <- as.integer((index - 1L) %% n_reference + 1L)
      d <- newdata(i, rows)
      w <- weights[rows]
      add <- function(value) {
        answer <- numeric(length(years))
        sums <- rowsum(matrix(value, ncol = 1L), i, reorder = FALSE)
        answer[as.integer(rownames(sums))] <- sums[, 1]
        answer
      }
      if (backend == "glmmTMB") {
        estimate <- estimate + add(predict_mean(d) * w)
        if (uncertainty) {
          for (j in seq_along(beta)) {
            h <- .Machine$double.eps^(1 / 3) * max(1, abs(beta[j]))
            upper <- lower <- beta
            upper[j] <- upper[j] + h
            lower[j] <- lower[j] - h
            gradient[, j] <- gradient[, j] +
              add((predict_mean(d, upper) - predict_mean(d, lower)) * w) / (2 * h)
          }
        }
      } else {
        eta <- .index_check_prediction(stats::predict(model, newdata = d, type = "link"), nrow(d))
        family <- stats::family(model)
        mu <- .index_check_prediction(family$linkinv(eta), nrow(d))
        estimate <- estimate + add(mu * w)
        if (uncertainty) {
          design <- if (backend == "gam") {
            stats::predict(model, newdata = d, type = "lpmatrix")
          } else .glm_reference_matrix(model, d)
          if (!identical(colnames(design), names(beta))) {
            stop("Prediction design does not match fitted coefficients.", call. = FALSE)
          }
          sums <- rowsum(design * (w * family$mu.eta(eta)), i, reorder = FALSE)
          chosen <- as.integer(rownames(sums))
          gradient[chosen, ] <- gradient[chosen, , drop = FALSE] + sums
        }
      }
  }
  if (any(!is.finite(estimate)) || any(!is.finite(gradient))) {
    stop("Index predictions or derivatives are non-finite.", call. = FALSE)
  }
  list(estimate = estimate, covariance = if (uncertainty) {
    gradient %*% covariance %*% t(gradient)
  } else NULL)
}

.index_brms <- function(model, years, batches, newdata, weights, ndraws, draw_batch_size) {
  available <- posterior::ndraws(model)
  n <- min(ndraws, available)
  if (n < 2L) stop("At least two existing posterior draws are required.", call. = FALSE)
  ids <- unique(as.integer(round(seq(1, available, length.out = n))))
  draw_batches <- split(seq_along(ids), ceiling(seq_along(ids) / draw_batch_size))
  annual <- matrix(0, length(ids), length(years), dimnames = list(as.character(ids), years))
  for (i in seq_along(years)) {
    for (rows in batches) {
      d <- newdata(i, rows)
      for (k in draw_batches) {
        predictions <- brms::posterior_epred(model, newdata = d,
          draw_ids = ids[k], re_formula = NA, allow_new_levels = FALSE)
        if (!is.matrix(predictions) ||
            !identical(dim(predictions), c(length(k), length(rows))) ||
            any(!is.finite(predictions))) {
          stop("brms must return a finite draw-by-row matrix for a univariate expected response.", call. = FALSE)
        }
        annual[k, i] <- annual[k, i] + as.numeric(predictions %*% weights[rows])
      }
    }
  }
  list(draws = annual, draw_ids = ids)
}
