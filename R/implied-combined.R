.implied_logmean <- function(x) {
  largest <- max(x)
  largest + log(mean(exp(x - largest)))
}

# Expected response over these original observation rows, retaining their
# covariates, exposure, and fitted latent effects. Not a reference-grid index.
.implied_delta_logmean <- function(encounter_shift, eta) {
  .implied_logmean(stats::plogis(eta[, 1L] + encounter_shift, log.p = TRUE) + eta[, 2L])
}

# Profile the derived scalar mean over two separately estimated shifts. The
# standard hurdle likelihood factorises. At any encounter shift inside its
# LR interval, the remaining log-likelihood budget gives exact bounds for the
# Gaussian lognormal shift. Extremising their derived means profiles out how
# the two components contribute, rather than imposing an equal-shift rule.
.implied_combined_limits <- function(y, eta, sigma, encounter, positive, level) {
  binary <- as.numeric(y > 0)
  cutoff <- stats::qchisq(level, 1) / 2
  best <- .implied_loglik(encounter, binary, eta[, 1L], 1, "binomial")
  interval <- .implied_profile(encounter, binary, eta[, 1L], rep(1, length(y)), "binomial", level)
  precision <- sum(1 / sigma[y > 0]^2)
  boundary <- function(delta, sign) {
    loss <- best - .implied_loglik(delta, binary, eta[, 1L], 1, "binomial")
    radius <- sqrt(2 * max(0, cutoff - loss) / precision)
    .implied_delta_logmean(delta, eta) + positive + sign * radius
  }
  # Check the whole interval and refine every local extremum, including ends.
  grid <- sort(unique(c(seq(interval[1L], interval[2L], length.out = 65L), encounter)))
  vapply(c(-1, 1), function(sign) {
    values <- vapply(grid, boundary, numeric(1), sign = sign)
    target <- -sign * values
    candidates <- which(target <= c(Inf, utils::head(target, -1L)) &
      target <= c(utils::tail(target, -1L), Inf))
    refined <- vapply(candidates, function(j) {
      bounds <- grid[c(max(1L, j - 1L), min(length(grid), j + 1L))]
      stats::optimize(function(d) -sign * boundary(d, sign), bounds,
        tol = 1e-9)$objective
    }, numeric(1))
    -sign * min(c(target, refined))
  }, numeric(1))
}

.implied_combined <- function(a, groups, min_n, level, interval) {
  time <- .focus_info(a$data, a$year)
  group <- .focus_info(a$data, groups)
  grid <- expand.grid(level = time$levels, group = group$levels, stringsAsFactors = FALSE)
  tab <- do.call(rbind, lapply(seq_len(nrow(grid)), function(j) {
    cell <- grid[j, ]
    i <- which(time$value == cell$level & group$value == cell$group)
    y <- a$observed[i]
    positive <- y > 0
    n <- length(i)
    np <- sum(positive)
    eta <- a$eta[i, , drop = FALSE]
    sigma <- a$dispersion[i]
    b <- if (n) exp(.implied_delta_logmean(0, eta)) else NA_real_
    status <- if (!n) "empty" else if (n < min_n) "sparse" else if (!np) {
      "empty_positive"
    } else if (np < min_n) "sparse_positive" else if (np == n) "boundary_one" else "ok"
    d1 <- d2 <- estimate <- adjustment <- se <- lo <- hi <- NA_real_
    if (status == "ok") {
      encounter <- .implied_shift(as.numeric(positive), eta[, 1L], rep(1, n), "binomial")
      pos <- .implied_shift(y[positive], eta[positive, 2L], sigma[positive], "lognormal")
      d1 <- encounter$shift
      d2 <- pos$shift
      log_estimate <- .implied_delta_logmean(d1, eta) + d2
      estimate <- exp(log_estimate)
      adjustment <- log_estimate - log(b)
      p <- stats::plogis(eta[, 1L] + d1)
      log_weight <- stats::plogis(eta[, 1L] + d1, log.p = TRUE) + eta[, 2L]
      weight <- exp(log_weight - max(log_weight))
      derivative <- sum(weight * (1 - p)) / sum(weight)
      se <- estimate * sqrt(derivative^2 * encounter$std_error^2 + pos$std_error^2)
      if (interval == "auto") {
        limits <- exp(.implied_combined_limits(y, eta, sigma, d1, d2, level))
        lo <- limits[1L]
        hi <- limits[2L]
      }
      if (any(!is.finite(c(b, estimate, se))) || b <= 0 || estimate <= 0 ||
          (interval == "auto" && any(!is.finite(c(lo, hi)) | c(lo, hi) <= 0))) {
        stop("Combined implied response exceeds the representable response scale; rescale the response units.", call. = FALSE)
      }
    }
    data.frame(level = cell$level, group = cell$group, n = n, n_positive = np,
      baseline = b, adjustment = adjustment, estimate = estimate, std_error = se,
      lower = lo, upper = hi, status = status,
      encounter_adjustment = d1, positive_adjustment = d2)
  }))
  metadata <- list(backend = a$backend, family = "delta_lognormal", link = "response",
    response = a$response, log_response = FALSE, year = a$year, year_levels = time$levels,
    groups = groups, group_levels = group$levels, method = "likelihood",
    baseline = "observed_fitted_mean", baseline_terms = character(),
    centring = "None: arithmetic means over each stratum's original observation rows",
    conditioning = "Original parameters, smooths, offsets, and fitted random effects held fixed",
    interval = if (interval == "auto") "conditional_profile" else "none",
    level = if (interval == "auto") level else NA_real_, min_n = min_n,
    n = length(a$observed), n_positive = sum(a$observed > 0), component = "combined",
    estimate_scale = "response", adjustment_scale = "log ratio of implied to fitted stratum mean",
    std_error = "Conditional delta-method SE on response scale; bars use profile likelihood",
    averaging = "Equal weights over original stratum rows, including zero observations; not a standardised index",
    dispersion = "Native fitted positive-component log-scale SD", format_version = 1L)
  structure(list(table = tab, metadata = metadata), class = "influ_implied")
}
