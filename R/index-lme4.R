.index_lme4 <- function(model, years, newdata, weights, uncertainty, batch_size,
    response_transform) {
  info <- .lme4_check(model, response_transform)
  beta <- lme4::fixef(model)
  V <- if (uncertainty) .index_check_covariance(as.matrix(stats::vcov(model)), length(beta)) else NULL
  if (!is.null(V) && !identical(rownames(V), names(beta))) stop("Native lme4 covariance does not match fixed-effect coefficients.", call. = FALSE)
  estimate <- numeric(length(years))
  gradient <- matrix(0, length(years), length(beta))
  n <- length(years) * length(weights)
  for (first in seq.int(1L, n, by = batch_size)) {
    k <- seq.int(first, min(n, first + batch_size - 1L))
    i <- as.integer((k - 1L) %/% length(weights) + 1L)
    rows <- as.integer((k - 1L) %% length(weights) + 1L)
    d <- newdata(i, rows)
    eta <- .index_check_prediction(stats::predict(model, newdata = d,
      re.form = NA, type = "link"), nrow(d))
    if (info$transform == "log") {
      mu <- exp(eta + stats::sigma(model)^2 / 2)
      derivative <- mu
    } else {
      mu <- info$family$linkinv(eta)
      derivative <- info$family$mu.eta(eta)
    }
    sums <- rowsum(matrix(mu * weights[rows], ncol = 1L), i, reorder = FALSE)
    at <- as.integer(rownames(sums))
    estimate[at] <- estimate[at] + sums[, 1L]
    if (uncertainty) {
      g <- rowsum(.lme4_X(model, d) * (derivative * weights[rows]), i, reorder = FALSE)
      gradient[at, ] <- gradient[at, , drop = FALSE] + g
    }
  }
  if (any(!is.finite(estimate)) || any(!is.finite(gradient))) stop("lme4 index predictions or gradients are non-finite.", call. = FALSE)
  list(estimate = estimate, covariance = if (uncertainty) gradient %*% V %*% t(gradient) else NULL)
}
