# Hurdles have observed component membership. Zero-inflated count mixtures do
# not: retain zeros in their full mixture likelihood for every adjustment.
.implied_joint_component <- function(kind, component) {
  allowed <- if (kind == "hurdle") c("encounter", "positive", "combined", "zero_inflation") else
    c("conditional", "positive", "zero_inflation", "combined")
  if (is.null(component) || !component %in% allowed) {
    stop(if (kind == "hurdle") {
      "A joint hurdle fit requires explicit component = 'encounter', 'positive', 'zero_inflation', or 'combined'."
    } else {
      "A zero-inflated mixture requires component = 'conditional' (or 'positive'), 'zero_inflation', or 'combined'. Its mixture gate is not an observed encounter response."
    }, call. = FALSE)
  }
}

.implied_joint_baseline <- function(a, groups, baseline, year_term, X, beta, terms, smooths = character()) {
  # A constant gate has a real, centred baseline of zero. No year coefficient
  # is invented; the local year/group shift is still estimated from likelihood.
  annual <- year_term %||% a$year
  vars <- lapply(names(terms), function(x) all.vars(stats::as.formula(paste("~", x))))
  if (!any(vapply(vars, function(x) annual %in% x, logical(1))) && !annual %in% smooths) {
    if (!is.null(year_term)) stop("The selected year_term is absent from this component.", call. = FALSE)
    chosen <- if (baseline == "year_group") which(vapply(vars, identical, logical(1), groups)) else integer()
    columns <- unique(unlist(terms[chosen], use.names = FALSE))
    value <- if (length(columns)) drop(X[, columns, drop = FALSE] %*% beta[columns]) else rep(0, nrow(a$data))
    a$baseline <- value - mean(value)
    a$baseline_terms <- names(terms)[chosen]
    a$baseline_group_present <- length(chosen) > 0L
    a$year_term <- NULL
    return(a)
  }
  .implied_native_baseline(a, groups, baseline, year_term, X, beta, terms, smooths)
}

.implied_glmmtmb_joint_adapter <- function(model, data, year, groups, baseline, component, year_term) {
  .require_model_backend(model)
  if (isTRUE(model$modelInfo$REML) || !isTRUE(model$sdr$pdHess) || model$fit$convergence != 0L) {
    stop("Implied effects require a converged ML glmmTMB fit with a positive-definite Hessian.", call. = FALSE)
  }
  fam <- stats::family(model)
  family <- .implied_family_name(fam$family)
  if (!family %in% c("Gamma", "poisson", "nbinom2", "tweedie", "truncated_poisson", "truncated_nbinom2")) {
    stop("Joint glmmTMB implied effects require Gamma or truncated Poisson/NB2 hurdles, or zero-inflated Poisson/NB2/Tweedie mixtures.", call. = FALSE)
  }
  kind <- if (family == "Gamma" || startsWith(family, "truncated_")) "hurdle" else "zero_inflated"
  .implied_joint_component(kind, component)
  a <- .implied_native_data(model, data, year, groups, stats::formula(model), family)
  .implied_native_family(family, fam$link, a$observed, joint = TRUE)
  weights <- stats::model.weights(model$frame)
  if (!is.null(weights) && any(weights != 1 | !is.finite(weights))) stop("Non-unit case weights are not supported for implied effects.", call. = FALSE)
  alignment <- model; alignment$na.action <- attr(model$frame, "na.action")
  get <- function(type) as.numeric(.align_observation_values(stats::predict(model,
    type = type, re.form = NULL), model$frame, "Native joint predictors", alignment))
  gate <- get("zlink")
  a$eta <- cbind(-gate, get("link"))
  a$dispersion <- get("disp")
  if (family == "Gamma") a$dispersion <- a$dispersion^2
  if (family %in% c("poisson", "truncated_poisson")) a$dispersion[] <- 1
  a$extra <- list()
  if (family == "tweedie") a$extra$power <- .implied_tweedie_power(model, fam, "glmmTMB")
  a$backend <- "glmmTMB"; a$positive_family <- family
  a$joint_kind <- kind; a$family <- paste(kind, family, sep = "_")
  a$component <- component
  if (any(!is.finite(a$eta)) || any(!is.finite(a$dispersion) | a$dispersion <= 0)) {
    stop("Finite native predictors and positive dispersion are required.", call. = FALSE)
  }
  if (component == "combined") {
    if (!is.null(year_term)) stop("Combined implied responses do not use a year-term baseline.", call. = FALSE)
    return(a)
  }
  part <- if (component %in% c("encounter", "zero_inflation")) "zi" else "cond"
  X <- stats::model.matrix(model, component = part)
  beta <- glmmTMB::fixef(model)[[part]]
  labels <- attr(stats::terms(model, component = part), "term.labels")
  assignments <- attr(X, "assign")
  terms <- stats::setNames(lapply(seq_along(labels), function(j) which(assignments == j)), labels)
  if (part == "zi" && component == "encounter") beta <- -beta
  .implied_joint_baseline(a, groups, baseline, year_term, X, beta, terms)
}

.implied_density <- function(y, eta, dispersion, family, extra = list()) {
  base <- sub("^truncated_", "", family)
  value <- switch(base,
    poisson = stats::dpois(y, exp(eta), log = TRUE),
    nbinom2 = stats::dnbinom(y, size = dispersion, mu = exp(eta), log = TRUE),
    Gamma = stats::dgamma(y, shape = 1 / dispersion, rate = exp(-eta) / dispersion, log = TRUE),
    lognormal = stats::dlnorm(y, eta - dispersion^2 / 2, dispersion, log = TRUE),
    tweedie = {
      if (!is.null(extra$tweedie_constant)) {
        extra$tweedie_constant + .implied_tweedie_meanpart(y, eta, dispersion, extra$power)
      } else {
        if (!requireNamespace("mgcv", quietly = TRUE)) stop("Package 'mgcv' is required for the native Tweedie density.", call. = FALSE)
        mgcv::ldTweedie(y, mu = exp(eta), p = extra$power, phi = dispersion)[, 1L]
      }
    }, stop("Unsupported joint implied-effect density.", call. = FALSE))
  if (startsWith(family, "truncated_")) value <- value -
    .implied_log1mexp(.implied_logzero(eta, dispersion, base, extra))
  value
}

.implied_joint_loglik <- function(theta, y, eta, dispersion, family, kind, extra = list()) {
  gate <- eta[, 1L] + theta[1L]
  amount <- eta[, 2L] + theta[2L]
  positive <- y > 0
  if (kind == "hurdle") {
    return(.implied_loglik(0, as.numeric(positive), gate, 1, "binomial") +
      sum(.implied_density(y[positive], amount[positive], dispersion[positive], family,
        .implied_extra_subset(extra, which(positive)))))
  }
  lp <- stats::plogis(gate, log.p = TRUE)
  value <- numeric(length(y))
  value[positive] <- lp[positive] + .implied_density(y[positive], amount[positive], dispersion[positive], family,
    .implied_extra_subset(extra, which(positive)))
  lzero <- .implied_logzero(amount[!positive], dispersion[!positive], family,
    .implied_extra_subset(extra, which(!positive)))
  z1 <- stats::plogis(-gate[!positive], log.p = TRUE)
  z2 <- lp[!positive] + lzero
  largest <- pmax(z1, z2)
  value[!positive] <- largest + log(exp(z1 - largest) + exp(z2 - largest))
  sum(value)
}

.implied_joint_logmean <- function(theta, eta, dispersion, family, extra = list()) {
  .implied_logmean(stats::plogis(eta[, 1L] + theta[1L], log.p = TRUE) +
    .implied_positive_logmean(eta[, 2L] + theta[2L], dispersion, family, extra))
}

.implied_profile_function <- function(delta, ll, level) {
  maximum <- ll(delta); cutoff <- stats::qchisq(level, 1) / 2
  f <- function(x) maximum - ll(x) - cutoff
  vapply(c(-1, 1), function(sign) {
    width <- .25
    for (j in seq_len(12L)) {
      end <- delta + sign * width
      value <- f(end)
      if (!is.na(value) && value >= 0) return(stats::uniroot(f, sort(c(delta, end)), tol = 1e-9)$root)
      width <- 2 * width
    }
    sign * Inf
  }, numeric(1))
}

# Search all local minima on a modest deterministic grid, then refine them.
# Mixture likelihoods can be non-concave; a single optimise() is insufficient.
.implied_profile_gate <- function(target, ll, eta, dispersion, family, extra) {
  objective <- function(gate) {
    if (!startsWith(family, "truncated_")) {
      amount <- target - .implied_joint_logmean(c(gate, 0), eta, dispersion, family, extra)
    } else {
      constraint <- function(amount) .implied_joint_logmean(c(gate, amount), eta, dispersion, family, extra) - target
      bounds <- c(-30, 30)
      if (constraint(bounds[1L]) > 0 || constraint(bounds[2L]) < 0) return(Inf)
      amount <- stats::uniroot(constraint, bounds, tol = 1e-10)$root
    }
    value <- -ll(c(gate, amount))
    if (is.finite(value)) value else Inf
  }
  grid <- seq(-30, 30, length.out = 81L)
  values <- vapply(grid, objective, numeric(1))
  candidates <- which(is.finite(values) & values <= c(Inf, utils::head(values, -1L)) &
    values <= c(utils::tail(values, -1L), Inf))
  if (!length(candidates)) return(-Inf)
  refined <- vapply(candidates, function(j) stats::optimize(objective,
    grid[c(max(1L, j - 1L), min(length(grid), j + 1L))], tol = 1e-9)$objective, numeric(1))
  -min(c(values, refined))
}

.implied_joint <- function(a, groups, baseline, min_n, level, interval) {
  .implied_joint_component(a$joint_kind, a$component)
  time <- .focus_info(a$data, a$year); group <- .focus_info(a$data, groups)
  grid <- expand.grid(level = time$levels, group = group$levels, stringsAsFactors = FALSE)
  combined <- a$component == "combined"
  gate_part <- a$component %in% c("encounter", "zero_inflation")
  # Native zero-probability effects are the negative of membership log-odds.
  sign <- if (a$component == "zero_inflation") -1 else 1
  table <- do.call(rbind, lapply(seq_len(nrow(grid)), function(j) {
    cell <- grid[j, ]; i <- which(time$value == cell$level & group$value == cell$group)
    y <- a$observed[i]; n <- length(i); np <- sum(y > 0)
    eta <- a$eta[i, , drop = FALSE]; dispersion <- a$dispersion[i]
    extra <- .implied_extra_subset(a$extra, i)
    b <- if (!n) NA_real_ else if (combined) exp(.implied_joint_logmean(c(0, 0), eta, dispersion, a$positive_family, extra)) else mean(a$baseline[i])
    status <- if (!n) "empty" else if (n < min_n) "sparse" else if (!np) "boundary_zero" else "ok"
    if (status == "ok" && a$joint_kind == "hurdle") {
      if ((combined || !gate_part) && np < min_n) status <- "sparse_positive"
    }
    if (status == "ok" && (combined || gate_part) && np == n) status <- "boundary_one"
    shift <- estimate <- se <- lo <- hi <- d1 <- d2 <- NA_real_
    if (status == "ok") {
      ll <- function(theta) .implied_joint_loglik(theta, y, eta, dispersion, a$positive_family, a$joint_kind, extra)
      if (combined) {
        if (a$joint_kind == "hurdle") {
          gate <- .implied_shift(as.numeric(y > 0), eta[, 1L], rep(1, n), "binomial")
          amount <- .implied_shift(y[y > 0], eta[y > 0, 2L], dispersion[y > 0],
            a$positive_family, .implied_extra_subset(extra, which(y > 0)))
          theta <- c(gate$shift, amount$shift)
        } else {
          starts <- list(c(0, 0), c(-1, .5), c(1, -.5), c(-2, 1), c(2, -1))
          fits <- lapply(starts, function(start) stats::optim(start, function(theta) -ll(theta),
            method = "L-BFGS-B", lower = c(-30, -30), upper = c(30, 30),
            control = list(factr = 1e4, pgtol = 1e-8, maxit = 300)))
          fit <- fits[[which.min(vapply(fits, function(x) x$value, numeric(1)))]]
          theta <- fit$par
          if (fit$convergence != 0L) status <- "failed_optimisation"
        }
        d1 <- theta[1L]; d2 <- theta[2L]
        if (any(!is.finite(theta) | abs(theta) > 29.9)) status <- "boundary_component"
        if (status == "ok") {
          log_estimate <- .implied_joint_logmean(theta, eta, dispersion, a$positive_family, extra)
          estimate <- exp(log_estimate); shift <- log_estimate - log(b)
          H <- stats::optimHess(theta, function(t) -ll(t))
          h <- 1e-4
          gradient <- vapply(1:2, function(k) {
            step <- c(0, 0); step[k] <- h
            (.implied_joint_logmean(theta + step, eta, dispersion, a$positive_family, extra) -
              .implied_joint_logmean(theta - step, eta, dispersion, a$positive_family, extra)) / (2 * h)
          }, numeric(1))
          if (any(!is.finite(H)) || min(eigen(H, symmetric = TRUE, only.values = TRUE)$values) <= 0) status <- "unidentified"
          else se <- estimate * sqrt(drop(t(gradient) %*% solve(H, gradient)))
          if (status == "ok" && interval == "auto") {
            profile <- function(target) .implied_profile_gate(target, ll, eta, dispersion, a$positive_family, extra)
            bounds <- .implied_profile_function(log_estimate, profile, level)
            lo <- exp(bounds[1L]); hi <- exp(bounds[2L])
          }
        }
      } else {
        k <- if (gate_part) 1L else 2L
        scalar_ll <- function(delta) { theta <- c(0, 0); theta[k] <- sign * delta; ll(theta) }
        fit <- if (a$joint_kind == "hurdle") {
          if (gate_part) {
            fit <- .implied_shift(as.numeric(y > 0), eta[, 1L], rep(1, n), "binomial")
            fit$shift <- sign * fit$shift
            fit
          } else .implied_shift(y[y > 0], eta[y > 0, 2L], dispersion[y > 0],
            a$positive_family, .implied_extra_subset(extra, which(y > 0)))
        } else .implied_numeric_shift(scalar_ll)
        shift <- fit$shift; se <- fit$std_error; estimate <- b + shift
        if (!is.finite(shift)) status <- "boundary_component"
        if (status == "ok" && interval == "auto") {
          ci <- .implied_profile_function(shift, scalar_ll, level)
          lo <- b + ci[1L]; hi <- b + ci[2L]
        }
        if (k == 1L) d1 <- sign * shift else d2 <- shift
      }
    }
    used <- if (a$joint_kind == "hurdle" && !combined && !gate_part) np else n
    data.frame(level = cell$level, group = cell$group, n = used,
      n_positive = np, baseline = b, adjustment = shift, estimate = estimate,
      std_error = se, lower = lo, upper = hi, status = status,
      encounter_adjustment = d1, positive_adjustment = d2)
  }))
  metadata <- list(backend = a$backend, family = a$family,
    link = if (combined) "response" else if (gate_part) "logit" else "log",
    response = a$response, log_response = FALSE, year = a$year, year_levels = time$levels,
    groups = groups, group_levels = group$levels, method = "likelihood",
    baseline = if (combined) "observed_fitted_mean" else baseline,
    baseline_terms = a$baseline_terms %||% character(), baseline_group_present = a$baseline_group_present,
    centring = if (combined) "None: original stratum response means" else "Mean fixed terms over original fitted rows",
    conditioning = a$conditioning %||% "Original parameters, smooths, offsets, and fitted latent effects held fixed",
    interval = if (interval == "auto") "conditional_profile" else "none",
    level = if (interval == "auto") level else NA_real_, min_n = min_n,
    n = if (a$joint_kind == "hurdle" && !combined && !gate_part) sum(a$observed > 0) else length(a$observed),
    n_total = length(a$observed), n_positive = sum(a$observed > 0), component = a$component,
    joint_kind = a$joint_kind, positive_family = a$positive_family,
    power = a$extra$power,
    likelihood = if (a$joint_kind == "zero_inflated") "Full mixture likelihood; zeros are not assigned to a latent component" else "Observed hurdle membership and positive-response likelihood",
    estimate_scale = if (combined) "response" else "effect",
    adjustment_scale = if (combined) "Log ratio of implied to fitted stratum mean" else if (gate_part) "Log-odds" else "Log mean",
    std_error = if (combined) "Conditional delta-method SE on response scale; bars use profile likelihood" else "Conditional likelihood SE on effect scale",
    averaging = "Equal weights over original stratum rows; not a standardised index",
    year_term = a$year_term, reference = a$reference, draw_id = a$draw_id, format_version = 1L)
  structure(list(table = table, metadata = metadata), class = "influ_implied")
}
