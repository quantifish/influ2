# Reproduce the residual article's compact Bayesian example from a complete fit.
# Reuse the Gaussian fit described in model-criteria-example.R. Sourcing this
# file and calling this function never fit a model or run MCMC.
write_brms_residual_example <- function(fit,
    path = "inst/extdata/brms-residual-example.rds") {
  stopifnot(inherits(fit, "brmsfit"), !is.null(fit$fit),
    identical(fit$family$family, "gaussian"),
    identical(paste(deparse(fit$formula$formula), collapse = " "), "y ~ year + x"))
  set.seed(20260907)
  d <- data.frame(year = factor(rep(2001:2005, each = 30)),
    x = rnorm(150) + rep(seq(-0.5, 0.5, length.out = 5), each = 30))
  d$y <- 2 + c(-0.2, -0.1, 0.1, 0.2, 0.3)[d$year] +
    0.7 * d$x + rnorm(150, sd = 0.6)
  stopifnot(isTRUE(all.equal(fit$data[names(d)], d,
    check.attributes = FALSE)))
  draws <- posterior::as_draws_array(fit)
  stopifnot(posterior::nchains(draws) == 4L)
  convergence <- posterior::summarise_draws(draws,
    "rhat", "ess_bulk", "ess_tail")
  stopifnot(max(convergence$rhat, na.rm = TRUE) < 1.01,
    min(convergence$ess_bulk, na.rm = TRUE) > 400,
    min(convergence$ess_tail, na.rm = TRUE) > 400)
  sampler <- brms::nuts_params(fit)
  stopifnot(!any(sampler$Parameter == "divergent__" & sampler$Value > 0))

  checks <- influ2::influ_residuals(fit, nsim = 500, batch_size = 25,
    grid_size = 201, seed = 20260910)
  # Separate, small set of whole posterior predictive replicates for an overlay.
  # Store their ECDF values, not the fitted model or prediction matrix.
  set.seed(20260911)
  draw_ids <- sample.int(brms::ndraws(fit), 20L, replace = FALSE)
  yrep <- brms::posterior_predict(fit, draw_ids = draw_ids,
    sort = FALSE, cores = 1L)
  stopifnot(identical(dim(yrep), c(20L, nrow(d))), all(is.finite(yrep)),
    isTRUE(all.equal(checks$observations$observed, d$y)))
  overlay <- do.call(rbind, lapply(seq_len(nrow(yrep)), function(i) {
    data.frame(replicate = i, response = checks$ecdf$response,
      probability = as.numeric(stats::ecdf(yrep[i, ])(checks$ecdf$response)))
  }))
  result <- list(checks = checks, overlay = overlay, metadata = list(
    data_seed = 20260907L, seed = 20260910L, overlay_seed = 20260911L,
    overlay_draw_ids = draw_ids, n = nrow(d), formula = "y ~ year + x",
    family = "gaussian", chains = 4L, posterior_draws = brms::ndraws(fit),
    max_rhat = max(convergence$rhat, na.rm = TRUE),
    min_ess_bulk = min(convergence$ess_bulk, na.rm = TRUE),
    min_ess_tail = min(convergence$ess_tail, na.rm = TRUE), divergences = 0L,
    brms_version = as.character(packageVersion("brms")),
    influ2_version = as.character(packageVersion("influ2")),
    source = "data-raw/brms-residual-example.R",
    fitting_source = "data-raw/model-criteria-example.R"))
  saveRDS(result, path, compress = "xz")
  invisible(result)
}
