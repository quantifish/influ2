# IV01 developer-only functions. No production simulation runs when sourced.
iv_settings <- function() list(nrep = 100L, min_n = 10L, level = .95,
  years = 2011:2016, seasons = c("Early season", "Mid-season", "Late season"),
  vessels = 12L, phi = 4, vessel_sd = .4, signal_size = .6,
  strong_shift = .3, design_seed = 912601L)

iv_counts <- function(sampling) {
  if (sampling == "balanced") return(matrix(36L, 6L, 3L))
  stopifnot(identical(sampling, "uneven"))
  matrix(c(108, 24, 12, 80, 24, 9, 12, 4, 9,
    0, 36, 60, 12, 9, 96, 9, 24, 120), 6L, 3L, byrow = TRUE)
}

iv_design <- function(sampling, settings = iv_settings()) {
  set.seed(settings$design_seed + match(sampling, c("balanced", "uneven")))
  grid <- expand.grid(year = settings$years, season = settings$seasons,
    stringsAsFactors = FALSE)
  grid$n <- as.vector(iv_counts(sampling))
  d <- grid[rep(seq_len(nrow(grid)), grid$n), c("year", "season")]
  d$year <- factor(d$year, levels = settings$years)
  d$season <- factor(d$season, levels = settings$seasons)
  # Equal vessel representation per balanced cell; deterministic irregular
  # representation in small cells. Allocation is independent of responses.
  d$vessel <- factor(unlist(lapply(grid$n, function(n)
    sample(rep(seq_len(settings$vessels), length.out = n)))), levels = seq_len(settings$vessels))
  d$depth_z <- rnorm(nrow(d))
  rownames(d) <- paste0(sampling, "-", seq_len(nrow(d)))
  d
}

iv_seed <- function(sampling, signal, id) {
  stopifnot(sampling %in% c("balanced", "uneven"), signal %in% c("null", "trend"),
    length(id) == 1L, id %in% c(1:100, 1001:1003))
  9300000L + match(sampling, c("balanced", "uneven")) * 100000L +
    match(signal, c("null", "trend")) * 10000L + as.integer(id)
}

iv_generate <- function(design, sampling, signal, id, settings = iv_settings()) {
  set.seed(iv_seed(sampling, signal, id))
  d <- design
  year_effect <- c(-.2, -.1, .1, .25, .1, .2)[as.integer(d$year)]
  season_effect <- c(-.2, 0, .2)[as.integer(d$season)]
  vessel_effect <- rnorm(settings$vessels, sd = settings$vessel_sd)
  d$baseline_true <- year_effect + season_effect
  d$baseline_true <- d$baseline_true - mean(d$baseline_true)
  d$eta_additive <- log(4) + year_effect + season_effect + .3 * d$depth_z +
    vessel_effect[as.integer(d$vessel)]
  d$injected <- if (signal == "null") rep(0, nrow(d)) else settings$signal_size *
    seq(-1, 1, length.out = 6)[as.integer(d$year)] * (as.integer(d$season) - 2)
  d$mu_true <- exp(d$eta_additive + d$injected)
  d$response <- rnbinom(nrow(d), mu = d$mu_true, size = settings$phi)
  d
}

iv_capture <- function(expr) {
  warnings <- character()
  error <- ""
  start <- proc.time()[[3L]]
  value <- tryCatch(withCallingHandlers(expr, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = function(e) { error <<- conditionMessage(e); NULL })
  list(value = value, error = error, warnings = paste(unique(warnings), collapse = " | "),
    seconds = proc.time()[[3L]] - start)
}

iv_fit <- function(d) glmmTMB::glmmTMB(
  response ~ year + season + depth_z + (1 | vessel),
  family = glmmTMB::nbinom2(), data = d, REML = FALSE)

iv_status <- function(captured) {
  fit <- captured$value
  if (is.null(fit)) return(list(valid = FALSE, convergence = NA_integer_,
    pd_hessian = FALSE, loglik = NA_real_, dispersion = NA_real_))
  ll <- as.numeric(stats::logLik(fit))
  list(valid = fit$fit$convergence == 0L && isTRUE(fit$sdr$pdHess) && is.finite(ll),
    convergence = fit$fit$convergence, pd_hessian = isTRUE(fit$sdr$pdHess),
    loglik = ll, dispersion = stats::sigma(fit))
}

iv_grid <- function(d, signal, settings = iv_settings()) {
  grid <- expand.grid(level = as.character(settings$years), group = settings$seasons,
    stringsAsFactors = FALSE)
  grid$n <- vapply(seq_len(nrow(grid)), function(j)
    sum(d$year == grid$level[j] & d$season == grid$group[j]), 0L)
  grid$injected <- if (signal == "null") 0 else settings$signal_size *
    seq(-1, 1, length.out = 6)[match(grid$level, settings$years)] *
    (match(grid$group, settings$seasons) - 2)
  grid
}

# Expected likelihood target: independent optimiser, not the package's score
# solver, with stable log(phi + exp(eta + delta)) arithmetic.
iv_target <- function(mu_true, eta, phi) {
  phi <- rep_len(phi, length(eta))
  stopifnot(length(mu_true) == length(eta), length(eta) > 0L,
    all(is.finite(c(mu_true, eta, phi))), all(mu_true > 0), all(phi > 0))
  # Row-specific exact shifts bracket the unique expected-score root.
  bounds <- range(log(mu_true) - eta)
  if (diff(bounds) < 1e-12) return(mean(bounds))
  objective <- function(delta) {
    z <- eta + delta
    a <- pmax(log(phi), z)
    log_sum <- a + log(exp(log(phi) - a) + exp(z - a))
    -sum(mu_true * z - (mu_true + phi) * log_sum)
  }
  stats::optimize(objective, interval = bounds, tol = 1e-10)$minimum
}

iv_oracle <- function(d, signal, settings = iv_settings()) {
  grid <- iv_grid(d, signal, settings)
  shift <- getFromNamespace(".implied_shift", "influ2")
  profile <- getFromNamespace(".implied_profile", "influ2")
  rows <- lapply(seq_len(nrow(grid)), function(j) {
    cell <- grid[j, ]
    i <- which(d$year == cell$level & d$season == cell$group)
    status <- if (!length(i)) "empty" else if (length(i) < settings$min_n) "sparse" else "ok"
    delta <- lo <- hi <- NA_real_
    if (status == "ok") {
      delta <- shift(d$response[i], d$eta_additive[i], rep(settings$phi, length(i)), "nbinom2")$shift
      if (!is.finite(delta)) status <- "boundary_zero" else {
        ci <- profile(delta, d$response[i], d$eta_additive[i], rep(settings$phi, length(i)),
          "nbinom2", settings$level)
        lo <- ci[1L]
        hi <- ci[2L]
      }
    }
    data.frame(cell, status, adjustment = delta, lower_shift = lo, upper_shift = hi,
      target = cell$injected, baseline = if (length(i)) mean(d$baseline_true[i]) else NA_real_)
  })
  do.call(rbind, rows)
}

iv_fitted <- function(fit, d, signal, settings = iv_settings()) {
  before <- list(par = fit$obj$env$last.par.best, data = fit$obj$env$data,
    rng = .Random.seed, eta = predict(fit, type = "link", re.form = NULL))
  result <- influ2::implied_effects(fit, groups = "season", year = "year",
    min_n = settings$min_n, level = settings$level)
  after <- list(par = fit$obj$env$last.par.best, data = fit$obj$env$data,
    rng = .Random.seed, eta = predict(fit, type = "link", re.form = NULL))
  stopifnot(identical(before, after))
  grid <- iv_grid(d, signal, settings)
  table <- result$table
  stopifnot(identical(table[c("level", "group", "n")], grid[c("level", "group", "n")]))
  eta <- as.numeric(before$eta)
  phi <- as.numeric(predict(fit, type = "disp", re.form = NULL))
  target <- vapply(seq_len(nrow(grid)), function(j) {
    i <- which(d$year == grid$level[j] & d$season == grid$group[j])
    if (!length(i)) return(NA_real_)
    iv_target(d$mu_true[i], eta[i], phi[i])
  }, 0.0)
  cells <- data.frame(grid, status = table$status, adjustment = table$adjustment,
    lower_shift = table$lower - table$baseline, upper_shift = table$upper - table$baseline,
    target, baseline = table$baseline)
  list(cells = cells, result = result)
}

iv_metrics <- function(cells, settings = iv_settings()) {
  ok <- cells$status == "ok" & is.finite(cells$adjustment) & is.finite(cells$target)
  ci <- ok & is.finite(cells$lower_shift) & is.finite(cells$upper_shift)
  strong <- ci & abs(cells$injected) >= settings$strong_shift
  excluded <- cells$lower_shift > 0 | cells$upper_shift < 0
  average <- function(x) if (length(x)) mean(x) else NA_real_
  err <- cells$adjustment[ok] - cells$target[ok]
  list(cells = nrow(cells), usable = sum(ok), intervals = sum(ci),
    empty = sum(cells$status == "empty"), sparse = sum(cells$status == "sparse"),
    boundary = sum(cells$status == "boundary_zero"), strong_cells = sum(strong),
    bias = average(err), rmse = sqrt(average(err^2)),
    containment = average(cells$lower_shift[ci] <= cells$target[ci] & cells$upper_shift[ci] >= cells$target[ci]),
    zero_exclusion = average(excluded[ci]), any_zero_exclusion = if (any(ci)) any(excluded[ci]) else NA,
    width = average(cells$upper_shift[ci] - cells$lower_shift[ci]),
    strong_direction = average(sign(cells$adjustment[strong]) == sign(cells$injected[strong])),
    strong_exclusion = average(excluded[strong]),
    target_injection_rmse = sqrt(average((cells$target[ok] - cells$injected[ok])^2)))
}

iv_groups <- function(data, fields) split(data, interaction(data[fields], drop = TRUE, lex.order = TRUE))

iv_summary <- function(metrics) {
  measures <- c("bias", "rmse", "containment", "zero_exclusion", "any_zero_exclusion",
    "width", "strong_direction", "strong_exclusion", "target_injection_rmse")
  do.call(rbind, lapply(iv_groups(metrics, c("sampling", "signal", "route")), function(d) {
    out <- d[1L, c("sampling", "signal", "route")]
    out$attempted <- nrow(d)
    out$successful <- sum(d$success)
    for (name in measures) {
      x <- d[[name]][d$success & is.finite(d[[name]])]
      out[[name]] <- if (length(x)) mean(x) else NA_real_
      out[[paste0(name, "_n")]] <- length(x)
      out[[paste0(name, "_mcse")]] <- if (length(x) > 1L) stats::sd(x) / sqrt(length(x)) else NA_real_
    }
    for (name in c("cells", "usable", "intervals", "empty", "sparse", "boundary", "strong_cells"))
      out[[name]] <- sum(d[[name]][d$success])
    out
  }))
}
