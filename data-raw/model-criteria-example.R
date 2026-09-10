# Reproduce the compact model-comparison vignette table from a complete fit.
# Sourcing this file does not fit a model. Call the function explicitly.
# Pass an existing fit to avoid MCMC; otherwise the small model is fitted once.
write_brms_criteria_example <- function(fit = NULL,
    path = "inst/extdata/brms-criteria-example.rds") {
  set.seed(20260907)
  d <- data.frame(year = factor(rep(2001:2005, each = 30)),
    x = rnorm(150) + rep(seq(-0.5, 0.5, length.out = 5), each = 30))
  d$y <- 2 + c(-0.2, -0.1, 0.1, 0.2, 0.3)[d$year] +
    0.7 * d$x + rnorm(150, sd = 0.6)
  if (is.null(fit)) {
    fit <- brms::brm(y ~ year + x, data = d, family = gaussian(),
      prior = brms::set_prior("normal(0, 2)", class = "b"),
      backend = "rstan", chains = 4, cores = 2, iter = 2000, warmup = 1000,
      seed = 20260907, refresh = 0)
  }
  stopifnot(inherits(fit, "brmsfit"), !is.null(fit$fit),
    isTRUE(all.equal(fit$data[names(d)], d, check.attributes = FALSE)))
  diagnostics <- posterior::summarise_draws(posterior::as_draws_array(fit),
    "rhat", "ess_bulk", "ess_tail")
  stopifnot(max(diagnostics$rhat, na.rm = TRUE) < 1.01)
  glm_fit <- glm(y ~ year + x, data = d, family = gaussian())
  native_loo <- brms::loo(fit)
  fit$criteria$loo <- native_loo
  started <- proc.time()
  result <- influ2::table_criterion(list(GLM = glm_fit, brms = fit),
    criterion = c("auto", "loo_R2"))
  stopifnot(isTRUE(all.equal(result$AIC[1], AIC(glm_fit))),
    isTRUE(all.equal(result$looic[2], unname(native_loo$estimates["looic", "Estimate"]))),
    is.na(result$AIC[2]), is.na(result$looic[1]))
  saveRDS(list(table = result, metadata = list(seed = 20260907L,
    n = nrow(d), family = "gaussian", formula = "y ~ year + x",
    posterior_draws = brms::ndraws(fit),
    max_rhat = max(diagnostics$rhat, na.rm = TRUE),
    comparison_seconds = unname((proc.time() - started)["elapsed"]),
    brms_version = as.character(packageVersion("brms")),
    loo_version = as.character(packageVersion("loo")),
    source = "data-raw/model-criteria-example.R")), path, compress = "xz")
  invisible(result)
}
