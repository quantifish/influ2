# Reproduce the native brms posterior fixtures once; never run MCMC in tests
# or during a vignette build. Requires a working local rstan toolchain.
library(brms)
set.seed(16092026)
d <- expand.grid(year = factor(2011:2013), area = factor(c("A", "B")), record = 1:40)
d$x <- runif(nrow(d), -1, 1)
d$year_scaled <- as.numeric(d$year) - 2
d$vessel <- factor(sample(1:12, nrow(d), TRUE))
d$effort <- runif(nrow(d), -.4, .4)
re <- rnorm(12, sd = .5)[d$vessel]
eta <- .7 + .2 * d$year_scaled + .3 * (d$area == "B") + .4 * d$x + re + d$effort
d$normal <- eta + rnorm(nrow(d), sd = .6)
d$positive <- rlnorm(nrow(d), eta, .6)
d$gamma <- rgamma(nrow(d), shape = 3, rate = 3 / exp(eta))
d$count <- rpois(nrow(d), exp(eta))
d$nb <- rnbinom(nrow(d), mu = exp(eta), size = 3)
d$binary <- rbinom(nrow(d), 1, plogis(eta))
d$delta <- rbinom(nrow(d), 1, plogis(.6 + .3 * d$year_scaled + re)) * d$positive
specs <- list(
  hurdle = list(bf(delta ~ year + area + s(x, k = 4) + offset(effort) + (1 | vessel),
    hu ~ year_scaled + area + x + (1 | vessel), sigma ~ x), hurdle_lognormal()),
  hurdle_constant = list(bf(delta ~ year + area + x + offset(effort) + (1 | vessel)), hurdle_lognormal()),
  gaussian = list(bf(normal ~ year + area + x + offset(effort) + (1 | vessel)), gaussian()),
  lognormal = list(bf(positive ~ year + area + x + offset(effort) + (1 | vessel)), lognormal()),
  Gamma = list(bf(gamma ~ year + area + x + offset(effort) + (1 | vessel)), Gamma("log")),
  poisson = list(bf(count ~ year + area + x + offset(effort) + (1 | vessel)), poisson()),
  nbinom2 = list(bf(nb ~ year + area + x + offset(effort) + (1 | vessel)), negbinomial()),
  bernoulli = list(bf(binary ~ year + area + x + (1 | vessel)), bernoulli())
)
out <- "tests/testthat/fixtures/brms-implied"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
full <- "data-raw/brms-implied-full"
dir.create(full, recursive = TRUE, showWarnings = FALSE)
for (name in names(specs)) {
  path <- file.path(out, paste0(name, ".rds"))
  complete_path <- file.path(full, paste0(name, ".rds"))
  message("Fitting ", name)
  spec <- specs[[name]]
  priors <- c(set_prior("normal(0, 2)", class = "b"), set_prior("exponential(2)", class = "sd"))
  if (name == "hurdle") priors <- c(priors,
    set_prior("exponential(2)", class = "sd", dpar = "hu"),
    set_prior("exponential(2)", class = "sds"),
    set_prior("normal(0, 1)", class = "b", dpar = "sigma"),
    set_prior("normal(0, 2)", class = "b", dpar = "hu"))
  if (name == "nbinom2") priors <- c(priors, set_prior("gamma(2, 0.5)", class = "shape"))
  fit <- if (file.exists(complete_path)) readRDS(complete_path) else brm(spec[[1]], data = d, family = spec[[2]], backend = "rstan",
    prior = priors, init = 0,
    chains = 4, cores = 2, iter = 2000, warmup = 1000, seed = 16092026,
    control = list(adapt_delta = .999, max_treedepth = 12), refresh = 0)
  diagnostic <- posterior::summarise_draws(posterior::as_draws_array(fit),
    "rhat", "ess_bulk", "ess_tail")
  print(c(max_rhat = max(diagnostic$rhat, na.rm = TRUE),
    min_ess = min(diagnostic$ess_bulk, na.rm = TRUE)))
  sampler <- rstan::get_sampler_params(fit$fit, inc_warmup = FALSE)
  divergences <- sum(vapply(sampler, function(s) sum(s[, "divergent__"]), numeric(1)))
  stopifnot(max(diagnostic$rhat, na.rm = TRUE) < 1.02,
    min(diagnostic$ess_bulk, na.rm = TRUE) > 100, divergences == 0)
  saveRDS(fit, complete_path, compress = "xz")
  # Keep 64 evenly spaced joint draws from each chain, with no warmup. This
  # is a numerical prediction fixture, not a scientific posterior summary.
  sim <- fit$fit@sim
  keep <- unique(round(seq(1, sim$n_save[1] - sim$warmup2[1], length.out = 64)))
  for (ch in seq_along(sim$samples)) {
    samples <- sim$samples[[ch]]
    index <- sim$warmup2[ch] + keep
    values <- lapply(samples, function(x) x[index])
    attrs <- attributes(samples)
    attrs$sampler_params <- lapply(attrs$sampler_params, function(x) x[index])
    attributes(values) <- attrs
    sim$samples[[ch]] <- values
  }
  sim$iter <- 64L; sim$warmup <- 0L; sim$thin <- 1L
  sim$n_save[] <- 64L; sim$warmup2[] <- 0L
  sim$permutation <- rep(list(seq_len(64L)), sim$chains)
  fit$fit@sim <- sim
  native <- log_lik(fit, draw_ids = c(1, 17), cores = 1)
  # The compiled C++ module is not needed for native predictions/log_lik.
  fit$fit@stanmodel@dso <- methods::new("cxxdso")
  fit$fit@.MISC <- new.env(parent = emptyenv())
  attr(fit, "implied_fixture") <- list(seed = 16092026L,
    max_rhat = max(diagnostic$rhat, na.rm = TRUE),
    min_ess = min(diagnostic$ess_bulk, na.rm = TRUE),
    divergences = divergences, retained_draws = 256L,
    brms_version = as.character(packageVersion("brms")), native_log_lik = native)
  saveRDS(fit, path, compress = "xz")
  stopifnot(isTRUE(all.equal(log_lik(readRDS(path), draw_ids = c(1, 17), cores = 1), native)))
}
