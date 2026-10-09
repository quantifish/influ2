# One-time native posterior fixtures for the expanded observation likelihoods.
# Tests and vignettes read these small fixtures and never compile or run MCMC.
library(brms)
set.seed(9102026)
d <- expand.grid(year = factor(2011:2013), area = factor(c("A", "B")), record = 1:40)
eta <- 1 + .2 * as.numeric(d$year) + .35 * (d$area == "B")
mu <- exp(eta)
present <- rbinom(nrow(d), 1, plogis(.1 + .2 * as.numeric(d$year)))
d$gamma <- present * rgamma(nrow(d), 3, rate = 3 / mu)
d$poisson <- present * pmax(1, rpois(nrow(d), mu))
d$nb <- present * pmax(1, rnbinom(nrow(d), size = 2, mu = mu))
d$zip <- present * rpois(nrow(d), mu)
d$zinb <- present * rnbinom(nrow(d), size = 2, mu = mu)
d$tw <- mgcv::rTweedie(mu, p = 1.5, phi = .8)
tw <- custom_family("tweedie", dpars = c("mu", "phi", "p"),
  links = c("log", "log", "identity"), lb = c(NA, 0, 1), ub = c(NA, NA, 2), type = "real",
  log_lik = function(i, prep) {
    get <- function(x) as.numeric(brms::get_dpar(prep, x, i = i))
    mgcv::ldTweedie(prep$data$Y[i], get("mu"), get("p"), get("phi"))[, 1L]
  })
stan <- stanvar(scode = '
  real tweedie_lpdf(real y, real mu, real phi, real p) {
    real lambda = pow(mu, 2-p) / (phi * (2-p));
    if (y == 0) return -lambda;
    real alpha = (2-p)/(p-1);
    real scale = phi * (p-1) * pow(mu, p-1);
    vector[150] terms;
    for (k in 1:150) terms[k] = poisson_lpmf(k | lambda) + gamma_lpdf(y | k*alpha, 1/scale);
    return log_sum_exp(terms);
  }', block = "functions")
specs <- list(
  hurdle_gamma = list(bf(gamma ~ year + area, hu ~ year + area), hurdle_gamma()),
  hurdle_poisson = list(bf(poisson ~ year + area, hu ~ year + area), hurdle_poisson()),
  hurdle_negbinomial = list(bf(nb ~ year + area, hu ~ year + area), hurdle_negbinomial()),
  zero_inflated_poisson = list(bf(zip ~ year + area, zi ~ year + area), zero_inflated_poisson()),
  zero_inflated_negbinomial = list(bf(zinb ~ year + area, zi ~ year + area), zero_inflated_negbinomial()),
  tweedie = list(bf(tw ~ year + area, phi = .8, p = 1.5), tw)
)
out <- "tests/testthat/fixtures/brms-implied"
full <- "data-raw/brms-implied-full"
dir.create(full, recursive = TRUE, showWarnings = FALSE)
for (name in names(specs)) {
  spec <- specs[[name]]
  complete <- file.path(full, paste0(name, ".rds"))
  message("Fitting joint fixture: ", name)
  fit <- if (file.exists(complete)) readRDS(complete) else brm(spec[[1L]], family = spec[[2L]],
    data = d, backend = "rstan", chains = 2, cores = 2, iter = 2000, warmup = 1000,
    seed = 9102026, init = 0, refresh = 0,
    stanvars = if (name == "tweedie") stan else NULL,
    control = list(adapt_delta = .99, max_treedepth = 12))
  diagnostics <- posterior::summarise_draws(posterior::as_draws_array(fit), "rhat", "ess_bulk")
  divergences <- sum(vapply(rstan::get_sampler_params(fit$fit, inc_warmup = FALSE),
    function(x) sum(x[, "divergent__"]), numeric(1)))
  stopifnot(max(diagnostics$rhat, na.rm = TRUE) < 1.02,
    min(diagnostics$ess_bulk, na.rm = TRUE) > 100, divergences == 0)
  saveRDS(fit, complete, compress = "xz")
  sim <- fit$fit@sim
  keep <- unique(round(seq(1, sim$n_save[1L] - sim$warmup2[1L], length.out = 64)))
  for (ch in seq_along(sim$samples)) {
    samples <- sim$samples[[ch]]; index <- sim$warmup2[ch] + keep
    values <- lapply(samples, function(x) x[index]); attrs <- attributes(samples)
    attrs$sampler_params <- lapply(attrs$sampler_params, function(x) x[index])
    attributes(values) <- attrs; sim$samples[[ch]] <- values
  }
  sim$iter <- 64L; sim$warmup <- 0L; sim$thin <- 1L
  sim$n_save[] <- 64L; sim$warmup2[] <- 0L
  sim$permutation <- rep(list(seq_len(64L)), sim$chains)
  fit$fit@sim <- sim
  fit$fit@stanmodel@dso <- methods::new("cxxdso")
  fit$fit@.MISC <- new.env(parent = emptyenv())
  attr(fit, "implied_fixture") <- list(seed = 9102026L, retained_draws = 128L,
    max_rhat = max(diagnostics$rhat, na.rm = TRUE),
    min_ess = min(diagnostics$ess_bulk, na.rm = TRUE), divergences = divergences,
    brms_version = as.character(packageVersion("brms")))
  if (name == "tweedie") {
    # Keep only namespace-qualified code, not the interactive global workspace.
    fit$family$log_lik <- tw$log_lik
    environment(fit$family$log_lik) <- baseenv()
    fit$family$env <- baseenv()
    fit$formula$family <- fit$family
  }
  path <- file.path(out, paste0(name, ".rds"))
  saveRDS(fit, path, compress = "xz")
  stopifnot(isTRUE(all.equal(brms::log_lik(readRDS(path), draw_ids = 17, cores = 1),
    brms::log_lik(fit, draw_ids = 17, cores = 1))))
}
