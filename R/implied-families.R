# Distribution parameters remain fixed at their native fitted values. Helpers
# accept row-specific extra parameters without retaining them in the result.
.implied_family_name <- function(x) {
  if (grepl("^Negative Binomial|^nbinom2$|^negbinomial$", x, ignore.case = TRUE)) return("nbinom2")
  if (grepl("^Tweedie|^tw$", x, ignore.case = TRUE)) return("tweedie")
  if (tolower(x) == "gamma") return("Gamma")
  if (x == "bernoulli") return("binomial")
  x
}

.implied_extra_subset <- function(extra, i) {
  if (is.null(extra)) return(list())
  lapply(extra, function(x) if (length(x) > 1L) x[i] else x)
}

.implied_tweedie_power <- function(model, family, backend) {
  p <- switch(backend,
    glmmTMB = as.numeric(glmmTMB::family_params(model)),
    sdmTMB = 1 + stats::plogis(model$parlist$thetaf),
    tinyVAST = 1 + stats::plogis(model$internal$parlist$log_sigma[2L]),
    if (is.function(family$getTheta)) family$getTheta(TRUE) else {
      # Fixed-power mgcv/statmod families keep p in their variance closure.
      e <- environment(family$variance)
      if (!is.null(e) && exists("p", e, inherits = FALSE)) get("p", e) else if (
        !is.null(e) && exists("var.power", e, inherits = FALSE)) get("var.power", e) else NA_real_
    })
  if (length(p) != 1L || !is.finite(p) || p <= 1 || p >= 2) {
    stop("Tweedie implied effects require one retained native power strictly between 1 and 2.", call. = FALSE)
  }
  as.numeric(p)
}

.implied_log1mexp <- function(x) {
  out <- numeric(length(x))
  small <- x < -log(2)
  out[small] <- log1p(-exp(x[small]))
  out[!small] <- log(-expm1(x[!small]))
  out
}

# With phi and power fixed, Tweedie's expensive series normaliser does not
# depend on the local mean shift. Cache one number per row, not simulations.
.implied_tweedie_meanpart <- function(y, eta, phi, p) {
  value <- -exp((2 - p) * eta) / ((2 - p) * phi)
  positive <- y > 0
  value[positive] <- value[positive] + y[positive] * exp((1 - p) * eta[positive]) /
    ((1 - p) * rep_len(phi, length(y))[positive])
  value
}

.implied_logzero <- function(eta, dispersion, family, extra = list()) {
  switch(family,
    poisson = -exp(eta),
    nbinom2 = -dispersion * (pmax(eta - log(dispersion), 0) + log1p(exp(-abs(eta - log(dispersion))))),
    tweedie = -exp((2 - extra$power) * eta) / ((2 - extra$power) * dispersion),
    Gamma = rep(-Inf, length(eta)),
    lognormal = rep(-Inf, length(eta)),
    stop("No validated zero-probability calculation for this joint family.", call. = FALSE))
}

.implied_positive_logmean <- function(eta, dispersion, family, extra = list()) {
  if (startsWith(family, "truncated_")) {
    return(eta - .implied_log1mexp(.implied_logzero(eta, dispersion,
      sub("^truncated_", "", family), extra)))
  }
  eta
}

.implied_numeric_shift <- function(ll) {
  width <- 1
  for (j in seq_len(10L)) {
    fit <- stats::optimize(ll, c(-width, width), maximum = TRUE, tol = 1e-10)
    if (abs(fit$maximum) < .95 * width) {
      h <- 1e-3
      curvature <- -(ll(fit$maximum + h) - 2 * fit$objective + ll(fit$maximum - h)) / h^2
      if (!is.finite(curvature) || curvature <= 0) stop("Local implied-effect curvature is not positive.", call. = FALSE)
      return(list(shift = fit$maximum, std_error = 1 / sqrt(curvature)))
    }
    if (width >= 32) return(list(shift = sign(fit$maximum) * Inf, std_error = NA_real_))
    width <- width * 2
  }
  stop("Could not bracket a finite local implied effect.", call. = FALSE)
}
