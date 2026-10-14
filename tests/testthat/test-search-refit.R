search_refit_data <- function() {
  set.seed(833)
  d <- data.frame(year = factor(rep(2011:2013, each = 90)), x = runif(270, -1, 1),
    z = rnorm(270), vessel = factor(rep(1:15, 18)), effort = runif(270, 1, 3),
    w = rep(c(1, 2, 1), 90), keep = rep(c(TRUE, TRUE, FALSE), 90))
  eta <- 1 + .15 * as.numeric(d$year) + .4 * d$x + rnorm(15, 0, .5)[d$vessel]
  d$y <- rpois(270, exp(eta))
  d$positive <- exp(eta + rnorm(270, 0, .5))
  d$encounter <- as.integer(d$y > 0)
  d$X <- runif(270); d$Y <- runif(270)
  rownames(d) <- paste0("row", seq_len(nrow(d)))
  d
}

test_that("search locks original rows, weights, trials, and both offsets", {
  d <- search_refit_data(); d$x[1] <- NA
  f <- glm(y ~ year * x + z + offset(log(effort)), data = d,
    family = poisson(), subset = keep, weights = w, offset = z / 10)
  old <- f; rng <- .Random.seed
  lock <- .search_locked_data(f, formula(f), "glm")
  expect_identical(lock$row_names, rownames(model.frame(f)))
  expect_equal(lock$weights, lock$data$w)
  plan <- .search_refit_args(f, "glm")
  reduced <- .search_refit_one(f, ~year + x, plan$args, lock, "glm")
  manual <- glm(y ~ year + x + offset(log(effort)), data = lock$data,
    family = poisson(), weights = w, offset = z / 10)
  expect_equal(coef(reduced), coef(manual))
  expect_equal(model.offset(model.frame(reduced)), log(lock$data$effort) + lock$data$z / 10)
  expect_true(.search_fit_status(reduced, "glm")$eligible)
  expect_identical(f, old); expect_identical(.Random.seed, rng)
  expect_identical(.search_locked_data(f, formula(f), "glm", d[nrow(d):1, ])$data, lock$data)
  bad <- d; bad$y <- bad$y + 1
  expect_error(.search_locked_data(f, formula(f), "glm", bad), "reproduce")
  bad <- d; bad$w <- bad$w + 1
  expect_error(.search_locked_data(f, formula(f), "glm", bad), "weights")
  bad <- d; rownames(bad) <- seq_len(nrow(bad))
  expect_error(.search_locked_data(f, formula(f), "glm", bad), "align")
  expect_error(.search_refit_one(f, ~year + offset(2 * log(effort)), plan$args, lock, "glm"), "offset")
  expect_error(.search_refit_one(f, positive ~year, plan$args, lock, "glm"), "response")
  d$success <- rbinom(nrow(d), 8, .4)
  b <- glm(cbind(success, 8 - success) ~year + x, family = binomial(), data = d)
  l <- .search_locked_data(b, formula(b), "glm")
  r <- .search_refit_one(b, ~year, list(), l, "glm")
  expect_identical(model.response(model.frame(b)), model.response(model.frame(r)))
})

test_that("search refit arguments reject likelihood and process overrides", {
  d <- search_refit_data(); f <- glm(y ~year + x, family = poisson(), data = d)
  for (nm in c("family", "formula", "ziformula", "dispformula", "mesh", "priors", "spatial", "map", "method")) {
    expect_error(.search_refit_args(f, "glm", setNames(list(1), nm)), "structure")
  }
  expect_error(.search_refit_args(f, "glm", list(data = d)), "overriding")
  expect_error(.search_refit_args(f, "glm", list(control = list(map = list()))), "setting")
  expect_error(.search_refit_args(f, "glm", list(control = list(getsd = FALSE))), "disable")
  expect_error(.search_refit_args(f, "glm", list(control = list(conv_check = "skip"))), "skip")
  expect_identical(.search_refit_args(f, "glm", list(control = glm.control(maxit = 80)))$args$control$maxit, 80)
  bad <- f; bad$converged <- FALSE
  expect_false(.search_fit_status(bad, "glm")$eligible)
  bad <- f; bad$coefficients[1] <- NA
  expect_identical(.search_fit_status(bad, "glm")$flags, "invalid_coefficients")
})

test_that("MASS negative-binomial search re-estimates size", {
  skip_if_not_installed("MASS")
  d <- search_refit_data()
  d$y <- rnbinom(nrow(d), mu = exp(1 + .5 * d$x), size = 2)
  f <- MASS::glm.nb(y ~ year + x + z, data = d)
  lock <- .search_locked_data(f, formula(f), "glm")
  r <- .search_refit_one(f, ~year + x, .search_refit_args(f, "glm")$args, lock, "glm")
  native <- MASS::glm.nb(y ~year + x, data = d)
  expect_equal(r$theta, native$theta, tolerance = 1e-7)
  expect_true(.search_fit_status(r, "glm")$eligible)
  expect_equal(as.numeric(logLik(r)), as.numeric(logLik(native)))
})

test_that("GAM search retains smooth and smoothing-selection semantics", {
  skip_if_not_installed("mgcv")
  d <- search_refit_data()
  f <- mgcv::gam(y ~year + s(x, k = 4) + z, family = poisson(), data = d, method = "REML")
  a <- .search_refit_args(f, "gam")
  expect_false(a$needs_original_refit); expect_match(a$notes, "conditional AIC")
  lock <- .search_locked_data(f, formula(f), "gam")
  r <- .search_refit_one(f, ~year + s(x, k = 4), a$args, lock, "gam")
  expect_identical(r$method, f$method); expect_length(r$smooth, 1L)
  expect_true(.search_fit_status(r, "gam")$eligible)
  expect_equal(AIC(r), AIC(mgcv::gam(y ~year + s(x, k = 4), family = poisson(), data = d, method = "REML")))
})

test_that("GAM native estimated and fixed family state never leaks across refits", {
  skip_if_not_installed("mgcv")
  set.seed(839)
  d <- data.frame(year = factor(rep(2011:2014, each = 80)),
    x = runif(320, -1, 1), z = rnorm(320))
  mu <- exp(1 + .9 * sin(3 * d$x) + .1 * as.numeric(d$year))
  cases <- list(nb = list(y = rnbinom(320, mu = mu, size = 1.8), family = mgcv::nb()),
    fixed_nb = list(y = rnbinom(320, mu = mu, size = 1.8), family = mgcv::nb(theta = 1.8)),
    tweedie = list(y = mgcv::rTweedie(mu = mu, p = 1.5, phi = .8), family = mgcv::tw()),
    gamma = list(y = rgamma(320, shape = 3, scale = mu / 3), family = Gamma("log")))
  for (case in cases) {
    d$y <- case$y
    m <- withr::with_package("mgcv", mgcv::gam(y ~year + s(x, k = 5) + z,
      data = d, family = case$family, method = "REML"))
    old_aic <- AIC(m); old_scale <- m$sig2
    old_theta <- if (is.function(m$family$getTheta)) m$family$getTheta(TRUE) else NULL
    old_n_theta <- m$family$n.theta
    old_variance <- if (is.function(m$family$variance) && length(formals(m$family$variance)) == 1L)
      m$family$variance(rep(5, 3)) else NULL
    r <- .search_refit_one(m, ~year + z, list(),
      .search_locked_data(m, formula(m), "gam"), "gam")
    expect_equal(AIC(m), old_aic)
    expect_identical(m$sig2, old_scale)
    expect_identical(m$family$n.theta, old_n_theta)
    if (!is.null(old_theta)) {
      expect_identical(m$family$getTheta(TRUE), old_theta)
      expect_false(identical(environment(m$family$getTheta), environment(r$family$getTheta)))
      expect_identical(r$family$n.theta, old_n_theta)
      if (identical(old_n_theta, 0)) expect_equal(r$family$getTheta(TRUE), old_theta)
    }
    if (!is.null(old_variance)) expect_equal(m$family$variance(rep(5, 3)), old_variance)
    expect_true(.search_fit_status(r, "gam")$eligible)
  }
})

test_that("glmmTMB search freezes secondary likelihood formulas and converts REML", {
  skip_if_not_installed("glmmTMB")
  d <- search_refit_data()
  d$y <- rnbinom(nrow(d), mu = exp(1 + .2 * as.numeric(d$year) + .6 * d$x +
    rnorm(15, 0, .5)[d$vessel]), size = 1.2) * rbinom(nrow(d), 1, plogis(.5 + .5 * d$x))
  f <- glmmTMB::glmmTMB(y ~year + x + z + (1 | vessel), data = d,
    family = glmmTMB::nbinom2(), ziformula = ~x, dispformula = ~year)
  old <- f$obj$env$last.par.best; rng <- .Random.seed
  lock <- .search_locked_data(f, formula(f), "glmmTMB")
  control <- glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 600, eval.max = 800))
  r <- .search_refit_one(f, ~year + x + (1 | vessel),
    .search_refit_args(f, "glmmTMB", list(control = control))$args, lock, "glmmTMB")
  expect_identical(deparse(formula(r, component = "zi")), deparse(formula(f, component = "zi")))
  expect_identical(deparse(formula(r, component = "disp")), deparse(formula(f, component = "disp")))
  expect_equal(model.matrix(r, component = "zi"), model.matrix(f, component = "zi"))
  expect_equal(model.matrix(r, component = "disp"), model.matrix(f, component = "disp"))
  expect_identical(f$obj$env$last.par.best, old); expect_identical(.Random.seed, rng)
  expect_true(.search_fit_status(r, "glmmTMB")$eligible)
  saved <- unserialize(serialize(f, NULL))
  expect_false(.search_fit_status(saved, "glmmTMB")$eligible)
  expect_error(.search_refit_one(saved, ~year, list(), lock, "glmmTMB"), "live native")
  g <- glmmTMB::glmmTMB(log(positive) ~year + x + (1 | vessel), data = d, REML = TRUE)
  a <- .search_refit_args(g, "glmmTMB")
  expect_true(a$needs_original_refit); expect_identical(a$args$REML, FALSE)
  l <- .search_locked_data(g, formula(g), "glmmTMB")
  rr <- .search_refit_one(g, ~year + (1 | vessel), a$args, l, "glmmTMB")
  expect_false(rr$modelInfo$REML); expect_true(g$modelInfo$REML)
})

test_that("search rejects positional fixed-parameter and random-parameter maps", {
  skip_if_not_installed("glmmTMB")
  set.seed(32)
  d <- data.frame(year = factor(rep(2020:2022, each = 40)), x = rnorm(120), z = rnorm(120))
  d$y <- rpois(120, exp(1 + .8 * d$x))
  fit <- glmmTMB::glmmTMB(y ~year + x, data = d, family = poisson(),
    start = list(beta = c(1, 0, 0, .8)), map = list(beta = factor(c(1, 2, 3, NA))))
  expect_equal(unname(glmmTMB::fixef(fit)$cond[["x"]]), .8)
  expect_error(.search_refit_args(fit, "glmmTMB"), "positional parameter maps")
  expect_error(search_cpue(fit, year = "year", scope = ~year + z,
    direction = "backward", refit = TRUE), "positional parameter maps")
  expect_error(.search_refit_one(fit, ~year + z, list(),
    .search_locked_data(fit, formula(fit), "glmmTMB"), "glmmTMB"), "positional parameter maps")
  d$vessel <- factor(rep(1:12, 10))
  random <- glmmTMB::glmmTMB(y ~year + x + (1 | vessel), data = d, family = poisson(),
    start = list(theta = log(.5)), map = list(theta = factor(NA)))
  expect_error(.search_refit_args(random, "glmmTMB"), "positional parameter maps")
  expect_error(.search_refit_args(list(control = list(map = list(b_j = factor(NA)))), "sdmTMB"), "positional parameter maps")
  tiny <- list(internal = list(control = list(tmb_map = list(alpha_j = factor(NA)))))
  expect_error(.search_refit_args(tiny, "tinyVAST"), "positional parameter maps")
})

test_that("glmmTMB negative-binomial family environments remain private to each fit", {
  skip_if_not_installed("glmmTMB")
  set.seed(734)
  d <- data.frame(year = factor(rep(1:4, 80)), x = rnorm(320), z = rnorm(320))
  d$y <- rnbinom(320, size = 1.6, mu = exp(1 + .9 * d$x))
  m <- glmmTMB::glmmTMB(y ~year + x + z, data = d, family = glmmTMB::nbinom2())
  f <- family(m); old <- f$variance(5); old_sigma <- sigma(m)
  r <- .search_refit_one(m, ~year + z, list(),
    .search_locked_data(m, formula(m), "glmmTMB"), "glmmTMB")
  expect_equal(f$variance(5), old)
  expect_equal(family(m)$variance(5), old)
  expect_identical(sigma(m), old_sigma)
  expect_false(identical(environment(f$variance), environment(family(r)$variance)))
  expect_true(.search_fit_status(r, "glmmTMB")$eligible)
  expect_equal(as.numeric(family(r)$variance(5)), 5 + 25 / sigma(r))
})

test_that("lme4 search has explicit ML and negative-binomial size policies", {
  skip_if_not_installed("lme4")
  d <- search_refit_data()
  f <- lme4::lmer(log(positive) ~year + x + z + (1 | vessel), data = d, REML = TRUE)
  a <- .search_refit_args(f, "merMod")
  expect_true(a$needs_original_refit); expect_identical(a$args$REML, FALSE)
  l <- .search_locked_data(f, formula(f), "merMod")
  r <- .search_refit_one(f, ~year + x + (1 | vessel), a$args, l, "merMod")
  expect_false(lme4::isREML(r)); expect_true(lme4::isREML(f))
  expect_true(.search_fit_status(r, "merMod")$eligible)
  bad <- r; bad@optinfo$conv$opt <- NULL
  expect_false(.search_fit_status(bad, "merMod")$eligible)
  expect_identical(.search_fit_status(bad, "merMod")$flags, "missing_convergence")
  d$y <- rnbinom(nrow(d), size = 2, mu = exp(1 + .4 * d$x))
  nb <- suppressWarnings(lme4::glmer.nb(y ~x + z + (1 | vessel), data = d))
  expect_error(.search_refit_args(nb, "merMod"), "explicit reestimate")
  a <- .search_refit_args(nb, "merMod", list(reestimate_nb = TRUE))
  expect_match(a$notes, "re-estimated")
  l <- .search_locked_data(nb, formula(nb), "merMod")
  rng <- .Random.seed
  r <- suppressWarnings(.search_refit_one(nb, ~x + (1 | vessel), a$args, l, "merMod"))
  expect_s4_class(r, "glmerMod"); expect_identical(.Random.seed, rng)
})

test_that("sdmTMB search retains native mesh, weights, offsets, and delta pair", {
  skip_if_not_installed("sdmTMB")
  d <- search_refit_data(); mesh <- sdmTMB::make_mesh(d, c("X", "Y"), n_knots = 8)
  f <- sdmTMB::sdmTMB(y ~year + x + z, data = d, mesh = mesh,
    family = poisson(), spatial = "off", spatiotemporal = "off", offset = log(d$effort), weights = d$w)
  a <- .search_refit_args(f, "sdmTMB")
  l <- .search_locked_data(f, f$formula, "sdmTMB"); rng <- .Random.seed
  supplied <- .search_locked_data(f, f$formula, "sdmTMB", d[nrow(d):1, ])
  expect_identical(supplied$data, l$data)
  mode <- f$tmb_obj$env$last.par.best
  r <- .search_refit_one(f, ~year + x, a$args, l, "sdmTMB")
  expect_identical(r$spde, f$spde); expect_equal(r$offset, f$offset)
  expect_equal(r$tmb_data$weights_i, f$tmb_data$weights_i)
  expect_true(.search_fit_status(r, "sdmTMB")$eligible)
  expect_identical(f$tmb_obj$env$last.par.best, mode); expect_identical(.Random.seed, rng)
  g <- sdmTMB::sdmTMB(log(positive) ~year + x + z, data = d, mesh = mesh,
    family = gaussian(), spatial = "off", spatiotemporal = "off", reml = TRUE)
  ml <- .search_refit_args(g, "sdmTMB")
  expect_true(ml$needs_original_refit); expect_identical(ml$args$reml, FALSE)
  reduced <- .search_refit_one(g, ~year + x, ml$args,
    .search_locked_data(g, g$formula, "sdmTMB"), "sdmTMB")
  expect_false(reduced$reml); expect_true(g$reml)
  expect_true(.search_fit_status(reduced, "sdmTMB")$eligible)
  d$delta <- d$positive * rbinom(nrow(d), 1, .7)
  f <- sdmTMB::sdmTMB(list(delta ~year + x, delta ~year + x + z),
    data = d, mesh = mesh, family = sdmTMB::delta_lognormal(), spatial = "off", spatiotemporal = "off")
  l <- .search_locked_data(f, f$formula, "sdmTMB")
  r <- .search_refit_one(f, list(~year, ~year + x), .search_refit_args(f, "sdmTMB")$args, l, "sdmTMB")
  expect_length(r$formula, 2L); expect_true(r$family$delta)
})

test_that("tinyVAST search preserves stored process controls and native weights", {
  skip_if_not_installed("tinyVAST")
  d <- search_refit_data()
  f <- tinyVAST::tinyVAST(y ~year + x + z, data = d, spatial_domain = NULL, family = poisson(),
    weights = d$w, control = tinyVAST::tinyVASTcontrol(calculate_deviance_explained = FALSE))
  l <- .search_locked_data(f, f$formula, "tinyVAST"); rng <- .Random.seed; mode <- f$obj$env$last.par.best
  a <- .search_refit_args(f, "tinyVAST", list(control = list(iter.max = 3000)))
  expect_identical(a$args$control$calculate_deviance_explained, FALSE)
  r <- .search_refit_one(f, ~year + x, a$args, l, "tinyVAST")
  expect_equal(r$tmb_inputs$tmb_data$weights_i, f$tmb_inputs$tmb_data$weights_i)
  expect_true(.search_fit_status(r, "tinyVAST")$eligible)
  expect_identical(f$obj$env$last.par.best, mode); expect_identical(.Random.seed, rng)
  g <- tinyVAST::tinyVAST(log(positive) ~year + x + z, data = d, spatial_domain = NULL,
    family = gaussian(), control = tinyVAST::tinyVASTcontrol(reml = TRUE,
      calculate_deviance_explained = FALSE))
  ml <- .search_refit_args(g, "tinyVAST")
  reduced <- .search_refit_one(g, ~year + x, ml$args,
    .search_locked_data(g, g$formula, "tinyVAST"), "tinyVAST")
  expect_false(reduced$internal$control$reml); expect_true(g$internal$control$reml)
  expect_true(.search_fit_status(reduced, "tinyVAST")$eligible)
  d$delta <- d$positive * rbinom(nrow(d), 1, .7)
  delta_formula <- ~year + x + z
  joint <- tinyVAST::tinyVAST(delta ~year + x + z, data = d, spatial_domain = NULL,
    family = tinyVAST::delta_lognormal(), delta_options = list(formula = delta_formula),
    control = tinyVAST::tinyVASTcontrol(calculate_deviance_explained = FALSE))
  delta_formula <- ~1
  joint_reduced <- .search_refit_one(joint, ~year + x,
    .search_refit_args(joint, "tinyVAST")$args,
    .search_locked_data(joint, joint$formula, "tinyVAST"), "tinyVAST")
  expect_identical(deparse(joint_reduced$internal$delta_formula), deparse(joint$internal$delta_formula))
  expect_equal(joint_reduced$tmb_inputs$tmb_data$X2_ij, joint$tmb_inputs$tmb_data$X2_ij)
  expect_true(.search_fit_status(joint_reduced, "tinyVAST")$eligible)
  a <- .search_refit_args(structure(list(internal = list(control = tinyVAST::tinyVASTcontrol(reml = TRUE))), class = "tinyVAST"), "tinyVAST")
  expect_true(a$needs_original_refit); expect_false(a$args$control$reml)
})

test_that("sdmTMB refits freeze mutable latent-process calls and binomial trials", {
  skip_if_not_installed("sdmTMB")
  set.seed(61)
  d <- data.frame(year = rep(2020:2025, each = 40), x = rnorm(240), z = rnorm(240),
    X = runif(240), Y = runif(240))
  slope <- c(0, .7, -.5, 1.1, .2, -.8)[d$year - 2019]
  d$y <- rpois(nrow(d), exp(1 + (.7 + slope) * d$x))
  mesh <- sdmTMB::make_mesh(d, c("X", "Y"), n_knots = 8)
  tv <- ~0 + x
  tv_type <- "rw0"
  m <- sdmTMB::sdmTMB(y ~factor(year) + x + z, data = d, mesh = mesh, family = poisson(),
    spatial = "off", spatiotemporal = "off", time = "year", time_varying = tv, time_varying_type = tv_type)
  tv <- ~0 + z
  tv_type <- "ar1"
  mode <- m$tmb_obj$env$last.par.best; rng <- .Random.seed
  r <- .search_refit_one(m, ~factor(year) + x, list(),
    .search_locked_data(m, m$formula, "sdmTMB"), "sdmTMB")
  expect_identical(deparse(r$time_varying), deparse(m$time_varying))
  expect_equal(r$tmb_data$X_rw_ik, m$tmb_data$X_rw_ik)
  expect_identical(r$tmb_data$random_walk, m$tmb_data$random_walk)
  expect_identical(r$tmb_data$share_range, m$tmb_data$share_range)
  expect_identical(m$tmb_obj$env$last.par.best, mode); expect_identical(.Random.seed, rng)
  expect_true(.search_fit_status(m, "sdmTMB")$eligible)
  expect_true(.search_fit_status(r, "sdmTMB")$eligible)
  missing <- m; missing$time_varying <- NULL
  expect_error(.search_sdmTMB_settings(missing, list()), "Cannot recover")
  svc <- ~0 + x
  d$y <- rpois(nrow(d), exp(1 + (.7 + 2.5 * (d$X - .5) + 2 * (d$Y - .5)) * d$x))
  spatial <- sdmTMB::sdmTMB(y ~factor(year) + x + z, data = d, mesh = mesh, family = poisson(),
    spatial = "off", spatiotemporal = "off", spatial_varying = svc)
  svc <- ~0 + z
  reduced <- .search_refit_one(spatial, ~factor(year) + x, list(),
    .search_locked_data(spatial, spatial$formula, "sdmTMB"), "sdmTMB")
  expect_identical(deparse(reduced$spatial_varying_formula), deparse(spatial$spatial_varying_formula))
  expect_equal(reduced$tmb_data$z_i, spatial$tmb_data$z_i)
  d$trials <- rep(8, nrow(d))
  d$success <- rbinom(nrow(d), d$trials, plogis(.2 + .4 * d$x))
  d$proportion <- d$success / d$trials
  bin <- sdmTMB::sdmTMB(proportion ~factor(year) + x + z, data = d, mesh = mesh,
    family = binomial(), weights = d$trials, spatial = "off", spatiotemporal = "off")
  locked <- .search_locked_data(bin, bin$formula, "sdmTMB", d)
  expect_equal(locked$weights, d$trials)
  reduced <- .search_refit_one(bin, ~factor(year) + x, list(), locked, "sdmTMB")
  expect_equal(reduced$tmb_data$size, bin$tmb_data$size)
  expect_equal(reduced$tmb_data$y_i, bin$tmb_data$y_i)
  expect_true(.search_fit_status(reduced, "sdmTMB")$eligible)
})

test_that("brms search sends a complete distributional formula without sampling in the test", {
  skip_if_not_installed("brms")
  d <- search_refit_data()
  f <- structure(list(data = d, formula = brms::bf(positive ~year + x + z, sigma ~x),
    prior = brms::set_prior("normal(0, 2)", class = "b"), file = "cache",
    stan_args = list(file = "cache")), class = "brmsfit")
  captured <- NULL
  testthat::local_mocked_bindings(update = function(object, ...) {
    captured <<- c(list(object = object), list(...))
    object
  }, .package = "stats")
  l <- .search_locked_data(f, f$formula$formula, "brmsfit")
  a <- .search_refit_args(f, "brmsfit", list(chains = 2, iter = 400))
  r <- .search_refit_one(f, ~year + x, a$args, l, "brmsfit")
  expect_identical(captured$formula.$pforms, f$formula$pforms)
  expect_identical(captured$object$prior, f$prior)
  expect_null(captured$object$file); expect_identical(captured$file_refit, "always")
  expect_identical(captured$newdata, f$data); expect_s3_class(r, "brmsfit")
  expect_error(.search_refit_args(f, "brmsfit", list(prior = f$prior)), "structure")
  expect_error(.search_refit_args(f, "brmsfit", list(recompile = FALSE)), "recompilation")
  expect_error(.search_refit_args(f, "brmsfit", list(control = list(.95))), "uniquely named")
  f$prior <- brms::set_prior("normal(0, 2)", class = "b", coef = "z")
  f$family <- brms::lognormal()
  expect_error(.search_refit_one(f, ~year + x, a$args, l, "brmsfit"), "explicit brms prior")
  saved <- readRDS(test_path("fixtures", "brms-implied", "gaussian.rds"))
  status <- .search_fit_status(saved, "brmsfit")
  expect_type(status$eligible, "logical"); expect_length(status$eligible, 1L)
  expect_false(grepl("unavailable", status$detail))
})
