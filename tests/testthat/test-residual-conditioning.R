test_that("conditioning choices are explicit and backend-specific", {
  defaults <- c(glm = "fitted", gam = "fitted", glmmTMB = "new_effects",
    sdmTMB = "fitted", tinyVAST = "fitted", brms = "posterior_predictive")
  for (backend in names(defaults)) {
    expect_identical(.resid_conditioning(backend, "backend_default"), unname(defaults[backend]))
    expect_identical(.resid_conditioning(backend, unname(defaults[backend])), unname(defaults[backend]))
    for (bad in list(NULL, NA_character_, 1, "conditional", c("fitted", "new_effects"))) {
      expect_error(.resid_conditioning(backend, bad), "Unsupported `conditioning`")
    }
  }
  expect_error(.resid_conditioning("brms", "fitted"), "posterior_predictive")
  expect_error(.resid_conditioning("gam", "conditional_draw"), "Unsupported")
  expect_error(.resid_conditioning("tinyVAST", "new_effects"), "Unsupported")
  fit <- glm(count ~ mined, data = data.frame(count = 1:30, mined = rep(0:1, 15)), family = poisson())
  a <- influ_residuals(fit, data = data.frame(count = 1:30, mined = rep(0:1, 15), year = rep(1:3, 10)), year = "year", nsim = 20)
  b <- influ_residuals(fit, data = data.frame(count = 1:30, mined = rep(0:1, 15), year = rep(1:3, 10)), year = "year", nsim = 20, conditioning = "fitted")
  expect_identical(a$observations, b$observations)
  expect_identical(a$metadata$conditioning, "fitted")
  expect_identical(b$metadata$conditioning_requested, "fitted")
})

test_that("glmmTMB conditional simulations reuse fitted effects without mutating the fit", {
  skip_if_not_installed("glmmTMB")
  d <- glmmTMB::Salamanders
  d$year <- factor(rep(2000:2003, length.out = nrow(d)))
  f <- glmmTMB::glmmTMB(count ~ year + mined + (1 | site), data = d,
    family = glmmTMB::nbinom2())
  original <- f$obj$env$data
  par <- f$obj$env$last.par.best
  set.seed(47)
  rng <- .Random.seed
  a <- influ_residuals(f, nsim = 40, batch_size = 1, seed = 19, conditioning = "fitted")
  expect_identical(.Random.seed, rng)
  b <- influ_residuals(f, nsim = 40, batch_size = 17, seed = 19, conditioning = "fitted")
  expect_identical(a$observations, b$observations)
  expect_identical(f$obj$env$data, original)
  expect_identical(f$obj$env$last.par.best, par)
  expect_match(plot(a)$patches$annotation$caption, "conditional on fitted random effects")
  default <- influ_residuals(f, nsim = 20, seed = 19)
  explicit <- influ_residuals(f, nsim = 20, seed = 19, conditioning = "new_effects")
  expect_identical(default$observations, explicit$observations)
  expect_false(isTRUE(all.equal(default$observations$pit, a$observations$pit)))

  # Independent reference: native public simulation with native fixed-effect
  # simulation controls. Restore the test fit's settings even on failure.
  set.seed(19)
  seeds <- sample.int(.Machine$integer.max, 40, replace = TRUE)
  native_copy <- unserialize(serialize(f, NULL))
  glmmTMB::set_simcodes(native_copy$obj, "fix")
  sims <- vapply(seeds, function(seed) as.numeric(simulate(native_copy, nsim = 1, seed = seed)[[1]]), numeric(nrow(d)))
  expect_equal(a$observations$predicted, rowMeans(sims), tolerance = 1e-12)
  set.seed(19)
  sample.int(.Machine$integer.max, 40, replace = TRUE)
  u <- runif(nrow(d))
  expect_equal(a$observations$pit,
    (rowSums(sims < d$count) + u * (rowSums(sims == d$count) + 1)) / 41)
  expect_identical(f$obj$env$data, original)
  changed <- unserialize(serialize(f, NULL))
  glmmTMB::set_simcodes(changed$obj, "fix")
  settings <- changed$obj$env$data
  restored <- influ_residuals(changed, nsim = 20, seed = 19, conditioning = "new_effects")
  expect_identical(restored$observations, default$observations)
  expect_identical(changed$obj$env$data, settings)
})

test_that("conditional glmmTMB binomial and zero-inflation effects are held fixed", {
  skip_if_not_installed("glmmTMB")
  set.seed(64)
  d <- expand.grid(year = factor(1:4), vessel = factor(1:20), id = 1:8)
  d$trials <- 5L
  u <- rnorm(20, sd = 0.9)
  d$success <- rbinom(nrow(d), 5, plogis(u[as.integer(d$vessel)]))
  f <- glmmTMB::glmmTMB(cbind(success, trials - success) ~ year + (1 | vessel), data = d, family = binomial())
  a <- influ_residuals(f, nsim = 20, conditioning = "fitted")
  expect_equal(a$observations$trials, d$trials)
  expect_equal(a$observations$probability, as.numeric(predict(f, type = "response")))
  d$y <- rbinom(nrow(d), 1, plogis(u[as.integer(d$vessel)])) * rpois(nrow(d), 5)
  z <- glmmTMB::glmmTMB(y ~ year + (1 | vessel), ziformula = ~ (1 | vessel), family = poisson(), data = d)
  before <- z$obj$env$data
  prepared <- .resid_tmb_object(z, "glmmTMB", "fitted")
  expect_true(all(vapply(prepared$obj$env$data$terms, function(x) x$simCode == 1L, logical(1))))
  expect_true(all(vapply(prepared$obj$env$data$termszi, function(x) x$simCode == 1L, logical(1))))
  expect_identical(z$obj$env$data, before)
  expect_identical(influ_residuals(z, nsim = 20, conditioning = "fitted")$metadata$component, "combined")
})

conditioning_sdm_fixture <- function(family = binomial()) {
  d <- as.data.frame(sdmTMB::pcod_2011)
  mesh <- sdmTMB::make_mesh(d, c("X", "Y"), n_knots = 15)
  sdmTMB::sdmTMB(if (isTRUE(family$delta)) density ~ depth_scaled else present ~ depth_scaled,
    data = d, mesh = mesh, time = "year", spatial = "on", spatiotemporal = "iid",
    family = family, silent = TRUE)
}

test_that("sdmTMB shares one joint field draw across batches with native probabilities", {
  skip_if_not_installed("sdmTMB")
  f <- conditioning_sdm_fixture()
  original <- f$tmb_obj$env$last.par.best
  original_data <- f$tmb_obj$env$data
  set.seed(71)
  rng <- .Random.seed
  a <- influ_residuals(f, nsim = 24, batch_size = 1, seed = 58, conditioning = "conditional_draw")
  b <- influ_residuals(f, nsim = 24, batch_size = 11, seed = 58, conditioning = "conditional_draw")
  expect_identical(.Random.seed, rng)
  expect_identical(a$observations, b$observations)
  expect_identical(a$calibration, b$calibration)
  expect_identical(f$tmb_obj$env$last.par.best, original)
  expect_identical(f$tmb_obj$env$data, original_data)
  expect_identical(a$metadata$conditioning, "conditional_draw")
  expect_match(a$metadata$prediction_type, "same single joint")

  # Native MC supplies the same joint conditional draw; native simulate()
  # accepts that draw explicitly. Neither the reference nor ours draws a
  # fresh field per response batch.
  set.seed(58)
  sampled <- f$tmb_obj$env$MC(n = 1, keep = TRUE, antithetic = FALSE)
  par <- original
  par[f$tmb_obj$env$lrandom()] <- as.numeric(attr(sampled, "samples"))
  seeds <- sample.int(.Machine$integer.max, 24, replace = TRUE)
  u <- runif(nrow(f$data))
  sims <- vapply(seeds, function(seed) as.numeric(simulate(f, nsim = 1,
    seed = seed, mcmc_samples = matrix(par, ncol = 1), silent = TRUE)), numeric(nrow(f$data)))
  expect_equal(a$observations$predicted, rowMeans(sims), tolerance = 1e-12)
  expect_equal(a$observations$pit, (rowSums(sims < f$data$present) +
    u * (rowSums(sims == f$data$present) + 1)) / 25)
  expected_p <- plogis(f$tmb_obj$report(par)$eta_i[, 1])
  expect_equal(a$observations$probability, as.numeric(expected_p), tolerance = 1e-12)
  expect_false(isTRUE(all.equal(a$observations$probability, predict(f, type = "response")$est)))
  expect_lt(as.numeric(object.size(a)), as.numeric(object.size(sims)))
  expect_false(any(c("model", "par", "draw", "simulations") %in% names(a)))
  invalid <- f
  invalid$sd_report$pdHess <- FALSE
  expect_error(influ_residuals(invalid, conditioning = "conditional_draw", nsim = 20), "positive-definite")
  set.seed(7)
  rng <- .Random.seed
  expect_error(influ_residuals(invalid, conditioning = "conditional_draw", nsim = 20), "positive-definite")
  expect_identical(.Random.seed, rng)
  new <- influ_residuals(f, nsim = 20, conditioning = "new_effects")
  expect_match(new$metadata$scheme, "fitted smooths held fixed")
  expect_identical(new$observations, influ_residuals(f, nsim = 20, batch_size = 1,
    conditioning = "new_effects")$observations)
  expect_equal(new$observations$probability, predict(f, type = "response")$est)
})

test_that("sdmTMB delta components keep the same field and correct encounter links", {
  skip_if_not_installed("sdmTMB")
  for (type in c("standard", "poisson-link")) {
    f <- conditioning_sdm_fixture(sdmTMB::delta_gamma(type = type))
    for (scheme in c("conditional_draw", "new_effects")) {
      a <- influ_residuals(f, nsim = 20, seed = 59, conditioning = scheme)
      b <- influ_residuals(f, nsim = 20, seed = 59, conditioning = scheme, component = "encounter")
      c <- influ_residuals(f, nsim = 20, seed = 59, conditioning = scheme, component = "positive")
      expect_identical(a$metadata$component, "combined")
      expect_identical(c$observations$observed, f$data$density[f$data$density > 0])
      expect_identical(c$observations$row, rownames(f$data)[f$data$density > 0])
      expect_true(all(b$observations$probability >= 0 & b$observations$probability <= 1))
      set.seed(59)
      adapter <- .resid_adapter(f, NULL, 20, conditioning = scheme)
      whole <- adapter$simulate(1:20)
      positive <- adapter$simulate_component(1:20, component = 2L)
      expect_equal(b$observations$predicted, rowMeans(whole > 0))
      expect_equal(c$observations$predicted, rowMeans(positive[f$data$density > 0, , drop = FALSE]))
      if (scheme == "conditional_draw") {
        set.seed(59)
        p <- .resid_tmb_object(f, "sdmTMB", scheme)
        lambda <- f$family[[1]]$linkinv(p$obj$report(p$par)$eta_i[, 1])
        expected <- if (type == "standard") lambda else -expm1(-lambda)
        expect_equal(b$observations$probability, expected)
      }
    }
  }
})

conditioning_tiny_fixture <- function(family = "poisson") {
  set.seed(7)
  d <- data.frame(time = rep(1:3, each = 80), var = "density", dist = family,
    x = runif(240), ycoord = runif(240))
  eta <- 3 * sin(d$x * 5)
  d$response <- switch(family,
    poisson = rpois(240, exp(eta)),
    binomial = rbinom(240, 1, plogis(eta)),
    delta = rbinom(240, 1, plogis(eta)) * rgamma(240, 3, 1))
  families <- switch(family, poisson = list(poisson = poisson()),
    binomial = list(binomial = binomial()), delta = list(delta = tinyVAST::delta_gamma()))
  mesh <- fmesher::fm_mesh_2d(d[c("x", "ycoord")], cutoff = .2)
  tinyVAST::tinyVAST(response ~ factor(time), data = d, family = families,
    spatial_domain = mesh, space_term = "density <-> density, spatial_sd",
    space_columns = c("x", "ycoord"))
}

test_that("tinyVAST single field sampling agrees with native TMB and preserves the fit", {
  skip_if_not_installed("tinyVAST")
  skip_if_not_installed("fmesher")
  f <- conditioning_tiny_fixture()
  original <- f$obj$env$last.par.best
  before <- f$obj$env$data
  a <- influ_residuals(f, nsim = 24, seed = 92, batch_size = 1, conditioning = "conditional_draw")
  b <- influ_residuals(f, nsim = 24, seed = 92, batch_size = 17, conditioning = "conditional_draw")
  expect_identical(a$observations, b$observations)
  expect_identical(f$obj$env$last.par.best, original)
  expect_identical(f$obj$env$data, before)
  set.seed(92)
  draw <- f$obj$env$MC(n = 1, keep = TRUE, antithetic = FALSE)
  par <- original
  par[f$obj$env$lrandom()] <- as.numeric(attr(draw, "samples"))
  seeds <- sample.int(.Machine$integer.max, 24, replace = TRUE)
  sims <- vapply(seeds, function(s) {set.seed(s); f$obj$simulate(par)$y_i}, numeric(nrow(f$data)))
  expect_equal(a$observations$predicted, rowMeans(sims), tolerance = 1e-12)
  expect_identical(a$metadata$conditioning, "conditional_draw")
  no_field <- tinyVAST::tinyVAST(response ~ factor(time), data = f$data,
    family = list(poisson = poisson()), spatial_domain = NULL)
  expect_error(influ_residuals(no_field, nsim = 20, conditioning = "conditional_draw"), "requires latent effects")
})

test_that("tinyVAST Bernoulli and delta calibration uses the sampled predictor", {
  skip_if_not_installed("tinyVAST")
  skip_if_not_installed("fmesher")
  for (family in c("binomial", "delta")) {
    f <- conditioning_tiny_fixture(family)
    part <- if (family == "delta") "encounter" else "auto"
    a <- influ_residuals(f, nsim = 20, seed = 12, conditioning = "conditional_draw", component = part)
    set.seed(12)
    prepared <- .resid_tmb_object(f, "tinyVAST", "conditional_draw")
    report <- prepared$obj$report(prepared$par)
    expected <- if (family == "delta") plogis(report$p_i) else report$mu_i
    expect_equal(a$observations$probability, as.numeric(expected))
    expect_true(all(is.finite(a$calibration$bins$predicted)))
    expect_identical(a$observations, influ_residuals(f, nsim = 20, seed = 12,
      conditioning = "conditional_draw", component = part, batch_size = 3)$observations)
  }
})

test_that("GAM explicit defaults preserve existing simulation targets", {
  skip_if_not_installed("mgcv")
  set.seed(8)
  d <- data.frame(year = factor(rep(1:4, each = 30)), x = runif(120))
  d$response <- rpois(120, exp(d$x))
  g <- mgcv::gam(response ~ year + s(x, k = 4), data = d, family = poisson())
  expect_identical(influ_residuals(g, nsim = 20)$observations,
    influ_residuals(g, nsim = 20, conditioning = "fitted")$observations)
})

test_that("native conditioning guards reject incomplete fits and changed interfaces", {
  skip_if_not_installed("TMB")
  expect_error(.resid_tmb_object(list(), "glmmTMB", "fitted"), "complete, unprofiled")
  e <- list(parList = function(...) list(a = 0, b = 1), last.par.best = c(a = 0, b = 1),
    lfixed = function() c(TRUE, FALSE), lrandom = function() c(FALSE, TRUE),
    random = 2L, data = list(sim_re = 0:5, sim_obs = 1), map = list(), DLL = "test")
  f <- list(tmb_obj = list(env = e), last.par.best = e$last.par.best,
    sd_report = list(pdHess = TRUE), model = list(convergence = 0L))
  bad <- f
  bad$tmb_obj$env$profile <- "a"
  expect_error(.resid_tmb_object(bad, "sdmTMB", "conditional_draw"), "unprofiled")
  bad <- f
  bad$tmb_obj$env$data$sim_re <- 0:2
  expect_error(.resid_tmb_object(bad, "sdmTMB", "conditional_draw"), "Unsupported sdmTMB")
  bad <- list(obj = list(env = e), fit = list(parfull = e$last.par.best))
  bad$obj$env$data$terms <- list(list())
  expect_error(.resid_tmb_object(bad, "glmmTMB", "fitted"), "validated simulation controls")
  mock <- list(env = list(last.par = c(wrong = 0)))
  testthat::local_mocked_bindings(MakeADFun = function(...) mock, .package = "TMB")
  expect_error(.resid_tmb_object(f, "sdmTMB", "conditional_draw"), "parameter order")
  mock <- list(env = list(last.par = e$last.par.best, lrandom = e$lrandom,
    random = e$random, MC = function(...) structure(0, samples = NA_real_)),
    fn = function(...) 0)
  bad <- f
  bad$reml <- TRUE
  expect_error(.resid_tmb_object(bad, "sdmTMB", "conditional_draw"), "ML, not REML")
  expect_error(.resid_tmb_object(f, "sdmTMB", "conditional_draw"), "latent-effect sampling failed")
})
