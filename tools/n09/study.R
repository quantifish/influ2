# Developer-only N09 study; no package algorithms or defaults are changed.
n09_settings <- function() {
  list(n = 480L, nrep = 100L, nsim = 499L, high_nsim = 1999L,
    sensitivity_ids = 1:10, batch_size = 50L, grid_size = 100L,
    pit_grid_size = 100L, level = 0.95, phi = 4,
    year_effect = c(0, .2, .35, .3, .1, -.1), beta = .7,
    design_seed = 120926L, band_seed = 90912L, band_replicates = 10000L)
}

n09_seed <- function(backend, id, purpose) {
  stopifnot(backend %in% c("glmmTMB", "sdmTMB"), id >= 1, id <= 1003,
    purpose %in% c("data", "analytic", "oracle", "primary", "second"))
  as.integer(1000000 + match(backend, c("glmmTMB", "sdmTMB")) * 100000 +
    id * 10 + match(purpose, c("data", "analytic", "oracle", "primary", "second")))
}

n09_design <- function(backend, settings = n09_settings()) {
  set.seed(settings$design_seed)
  if (backend == "glmmTMB") {
    d <- expand.grid(year = 2011:2016, vessel = factor(1:20), haul = 1:4)
    d <- d[order(d$year, d$vessel, d$haul), ]
  } else {
    stations <- data.frame(station = 1:80, X = runif(80, 0, 100), Y = runif(80, 0, 100))
    d <- stations[rep(seq_len(80), 6), ]
    d$year <- rep(2011:2016, each = 80)
  }
  d$x <- rnorm(nrow(d)) + .25 * (d$year - mean(d$year))
  rownames(d) <- paste0("row", seq_len(nrow(d)))
  stopifnot(nrow(d) == settings$n)
  d
}

n09_generate <- function(backend, id, design, mesh = NULL, settings = n09_settings()) {
  d <- design
  seed <- n09_seed(backend, id, "data")
  if (backend == "glmmTMB") {
    set.seed(seed)
    effects <- rnorm(nlevels(d$vessel), sd = .7)
    d$latent <- effects[as.integer(d$vessel)]
    d$mu <- exp(log(5) + settings$year_effect[match(d$year, 2011:2016)] +
      settings$beta * d$x + d$latent)
    d$response <- rnbinom(nrow(d), mu = d$mu, size = settings$phi)
  } else {
    generated <- sdmTMB::simulate_new(~ factor(year) + x, data = d,
      time = "year", mesh = mesh, family = sdmTMB::nbinom2(),
      B = c(log(5), settings$year_effect[-1], settings$beta),
      range = 30, sigma_O = .45, sigma_E = .6, phi = settings$phi,
      spatiotemporal = "iid", seed = seed)
    stopifnot(nrow(generated) == nrow(d), identical(generated$year, d$year),
      isTRUE(all.equal(generated$X, d$X)), isTRUE(all.equal(generated$Y, d$Y)))
    d$mu <- as.numeric(generated$mu)
    d$response <- as.numeric(generated$observed)
    d$omega <- as.numeric(generated$omega_s)
    d$epsilon <- as.numeric(generated$epsilon_st)
  }
  stopifnot(all(is.finite(d$mu) & d$mu > 0), all(d$response >= 0),
    all(d$response == floor(d$response)))
  d
}

n09_bands <- function(n = 480L, K = 100L, level = .95) {
  p <- bayesplot::ppc_pit_ecdf(pit = (seq_len(n) - .5) / n,
    K = K, prob = level, method = "independent", interpolate_adj = FALSE)
  b <- ggplot2::ggplot_build(p)$data
  # Validate the public plot's structure rather than call private band code.
  stopifnot(length(b) == 3L, nrow(b[[1]]) == K,
    identical(b[[1]]$x, b[[2]]$x), identical(b[[1]]$x, b[[3]]$x),
    all(b[[2]]$y <= b[[1]]$y))
  data.frame(x = b[[1]]$x, lower = b[[2]]$y, upper = b[[1]]$y,
    interval_grid = seq_len(K) / K)
}

n09_pit_metrics <- function(pit, bands, level = .95) {
  stopifnot(is.numeric(pit), length(pit) >= 3, all(is.finite(pit)),
    all(pit > 0 & pit < 1))
  n <- length(pit)
  u <- sort(pit)
  distance <- max(seq_len(n) / n - u, u - (seq_len(n) - 1) / n)
  F <- findInterval(bands$x, u) / n
  z <- qnorm(pit)
  tail <- (1 - level) / 2
  low <- qnorm(qbeta(tail, seq_len(n), n + 1 - seq_len(n)))
  high <- qnorm(qbeta(1 - tail, seq_len(n), n + 1 - seq_len(n)))
  c(dkw_distance = distance,
    dkw_crossing = as.numeric(distance > sqrt(log(2 / (1 - level)) / (2 * n))),
    displayed_band_crossing = as.numeric(any(F < bands$lower | F > bands$upper)),
    qq_outside_fraction = mean(sort(z) < low | sort(z) > high),
    residual_mean = mean(z), residual_sd = sd(z))
}

n09_neighbours <- function(data, k = 4L) {
  if (!all(c("X", "Y") %in% names(data))) return(NULL)
  # Each pair is within the same year; symmetrise a four-nearest-neighbour graph.
  edges <- lapply(split(seq_len(nrow(data)), data$year), function(ids) {
    distance <- as.matrix(dist(data[ids, c("X", "Y")]))
    diag(distance) <- Inf
    pairs <- do.call(rbind, lapply(seq_along(ids), function(i) {
      j <- order(distance[i, ])[seq_len(k)]
      cbind(ids[i], ids[j])
    }))
    unique(rbind(pairs, pairs[, 2:1]))
  })
  do.call(rbind, edges)
}

n09_spatial_score <- function(residual, data, edges) {
  if (is.null(edges)) return(NA_real_)
  centred <- residual - ave(residual, data$year, FUN = mean)
  if (sum(centred^2) == 0) return(NA_real_)
  length(residual) / nrow(edges) *
    sum(centred[edges[, 1]] * centred[edges[, 2]]) / sum(centred^2)
}

n09_metrics <- function(checks, data, bands, edges = NULL) {
  stopifnot(identical(checks$observations$row, rownames(data)))
  r <- checks$observations$residual
  F <- findInterval(checks$ecdf$response, sort(data$response)) / nrow(data)
  c(n09_pit_metrics(checks$observations$pit, bands, checks$metadata$level),
    fitted_spearman = cor(r, checks$observations$predicted, method = "spearman"),
    covariate_spearman = cor(r, data$x, method = "spearman"),
    year_rms = sqrt(mean(tapply(r, data$year, mean)^2)),
    response_ecdf_outside_fraction = mean(F < checks$ecdf$lower | F > checks$ecdf$upper),
    spatial_score = n09_spatial_score(r, data, edges))
}

n09_capture <- function(expr) {
  warnings <- character()
  start <- proc.time()[[3]]
  result <- tryCatch(withCallingHandlers(expr, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = identity)
  list(value = result, error = if (inherits(result, "error")) conditionMessage(result) else "",
    warnings = paste(unique(warnings), collapse = " | "), seconds = proc.time()[[3]] - start)
}

n09_fit <- function(backend, scenario, data, mesh = NULL) {
  if (backend == "glmmTMB") {
    glmmTMB::glmmTMB(if (scenario == "omit_x") response ~ factor(year) + (1 | vessel) else
      response ~ factor(year) + x + (1 | vessel), data = data,
      family = glmmTMB::nbinom2(), control = glmmTMB::glmmTMBControl(parallel = 1L))
  } else {
    sdmTMB::sdmTMB(if (scenario == "omit_x") response ~ factor(year) else
      response ~ factor(year) + x, data = data, mesh = mesh, time = "year",
      spatial = "on", spatiotemporal = if (scenario == "omit_st") "off" else "iid",
      family = sdmTMB::nbinom2(), silent = TRUE)
  }
}

n09_fit_status <- function(captured, backend) {
  if (nzchar(captured$error)) return(list(valid = FALSE, convergence = NA_integer_,
    pd_hessian = FALSE, max_gradient = NA_real_, loglik = NA_real_))
  f <- captured$value
  report <- if (backend == "glmmTMB") f$sdr else f$sd_report
  code <- if (backend == "glmmTMB") f$fit$convergence else f$model$convergence
  ll <- as.numeric(logLik(f))
  gradient <- report$gradient.fixed
  list(valid = identical(as.integer(code), 0L) && isTRUE(report$pdHess) && is.finite(ll),
    convergence = as.integer(code), pd_hessian = isTRUE(report$pdHess),
    max_gradient = if (length(gradient)) max(abs(gradient)) else NA_real_, loglik = ll)
}

n09_wilson <- function(successes, n, level = .95) {
  stopifnot(length(n) == 1L, n >= 0, successes >= 0, successes <= n)
  if (!n) return(c(rate = NA_real_, lower = NA_real_, upper = NA_real_))
  z <- qnorm(1 - (1 - level) / 2)
  p <- successes / n
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  radius <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  c(rate = p, lower = max(0, centre - radius), upper = min(1, centre + radius))
}

n09_band_audit <- function(bands, settings = n09_settings()) {
  set.seed(settings$band_seed)
  flags <- replicate(settings$band_replicates, {
    u <- sort(runif(settings$n))
    F <- findInterval(bands$x, u) / settings$n
    distance <- max(seq_len(settings$n) / settings$n - u,
      u - (seq_len(settings$n) - 1) / settings$n)
    aligned <- findInterval(bands$interval_grid, u) / settings$n
    c(displayed = any(F < bands$lower | F > bands$upper),
      aligned = any(aligned < bands$lower | aligned > bands$upper),
      dkw = distance > sqrt(log(2 / (1 - settings$level)) / (2 * settings$n)))
  })
  do.call(rbind, lapply(rownames(flags), function(method) {
    hits <- sum(flags[method, ])
    data.frame(method, n = ncol(flags), crossings = hits,
      as.list(n09_wilson(hits, ncol(flags))))
  }))
}
