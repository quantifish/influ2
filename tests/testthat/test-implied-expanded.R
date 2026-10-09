expanded_implied_data <- function() {
  set.seed(9102026)
  d <- expand.grid(year = factor(2011:2013), area = factor(c("A", "B")), record = 1:55)
  d$x <- runif(nrow(d), -1, 1)
  d$effort <- runif(nrow(d), -.3, .3)
  d$eta <- 1 + .2 * as.numeric(d$year) + .35 * (d$area == "B") + .3 * d$x + d$effort
  d$binary <- rbinom(nrow(d), 1, plogis(d$eta - 1))
  d$gamma <- rgamma(nrow(d), shape = 3, rate = 3 / exp(d$eta))
  d$poisson <- rpois(nrow(d), exp(d$eta))
  d$nbinom2 <- rnbinom(nrow(d), size = 2, mu = exp(d$eta))
  if (requireNamespace("mgcv", quietly = TRUE)) d$tweedie <- mgcv::rTweedie(exp(d$eta), 1.5, .8)
  rownames(d) <- paste0("row", seq_len(nrow(d)))
  d
}

test_that("ordinary binomial effects use native probabilities and known trials", {
  d <- expanded_implied_data()
  set.seed(9); d$success <- rbinom(nrow(d), 8, plogis(d$eta - 1)); d$failure <- 8 - d$success
  fits <- list(glm = glm(binary ~ year + area + x + offset(effort), data = d, family = binomial()),
    grouped_glm = glm(cbind(success, failure) ~ year + area + x + offset(effort), data = d, family = binomial()))
  if (requireNamespace("mgcv", quietly = TRUE)) fits$gam <- mgcv::gam(binary ~ year + area + s(x, k = 4) + offset(effort), data = d, family = binomial())
  if (requireNamespace("glmmTMB", quietly = TRUE)) fits$glmmTMB <- glmmTMB::glmmTMB(binary ~ year + area + x + offset(effort), data = d, family = binomial())
  for (name in names(fits)) {
    m <- fits[[name]]; z <- implied_effects(m, year = "year", groups = "area")
    eta <- as.numeric(predict(m, type = "link"))
    y <- if (name == "grouped_glm") d$success else d$binary
    size <- if (name == "grouped_glm") 8 else 1
    for (j in seq_len(nrow(z$table))) {
      cell <- z$table[j, ]; i <- d$year == cell$level & d$area == cell$group
      ll <- function(delta) sum(dbinom(y[i], size, plogis(eta[i] + delta), log = TRUE))
      optimum <- optimize(ll, c(-10, 10), maximum = TRUE, tol = 1e-10)
      expect_equal(cell$adjustment, optimum$maximum, tolerance = 1e-6)
      expect_equal(2 * (ll(cell$adjustment) - ll(cell$lower - cell$baseline)), qchisq(.95, 1), tolerance = 1e-6)
      expect_equal(2 * (ll(cell$adjustment) - ll(cell$upper - cell$baseline)), qchisq(.95, 1), tolerance = 1e-6)
      h <- 1e-3
      curvature <- -(ll(cell$adjustment + h) - 2 * ll(cell$adjustment) + ll(cell$adjustment - h)) / h^2
      expect_equal(1 / cell$std_error^2, curvature, tolerance = 1e-5)
    }
    expect_equal(z$metadata$link, "logit")
  }
  zero <- d; zero$binary[] <- 0
  a <- .implied_shift(rep(0, 10), rep(0, 10), 1, "binomial")
  b <- .implied_shift(rep(8, 10), rep(0, 10), 1, "binomial", list(trials = rep(8, 10)))
  expect_equal(a$shift, -Inf); expect_equal(b$shift, Inf)
})

test_that("Tweedie shifts and profiles agree with independent native densities", {
  skip_if_not_installed("mgcv")
  d <- expanded_implied_data()
  families <- list(glm = mgcv::Tweedie(p = 1.5), gam = mgcv::Tweedie(p = 1.5))
  if (requireNamespace("glmmTMB", quietly = TRUE)) families$glmmTMB <- glmmTMB::tweedie()
  if (requireNamespace("sdmTMB", quietly = TRUE)) families$sdmTMB <- sdmTMB::tweedie()
  if (requireNamespace("tinyVAST", quietly = TRUE)) families$tinyVAST <- tinyVAST::tweedie()
  for (backend in names(families)) {
    Family <- families[[backend]]
    formula <- tweedie ~ year + area + x + offset(effort)
    m <- switch(backend, glm = glm(formula, data = d, family = Family),
      gam = mgcv::gam(formula, data = d, family = Family),
      glmmTMB = glmmTMB::glmmTMB(formula, data = d, family = Family),
      sdmTMB = sdmTMB::sdmTMB(tweedie ~ year + area + x, offset = d$effort, data = d, family = Family, spatial = "off", silent = TRUE),
      tinyVAST = tinyVAST::tinyVAST(formula, data = d, family = Family, spatial_domain = NULL,
        control = tinyVAST::tinyVASTcontrol(calculate_deviance_explained = FALSE)))
    a <- .implied_adapter(m, NULL, "year", "area", "year_group")
    z <- implied_effects(m, year = "year", groups = "area")
    expect_true(all(z$table$status == "ok"))
    expect_equal(z$metadata$power, a$extra$power)
    for (j in seq_len(nrow(z$table))) {
      cell <- z$table[j, ]; i <- d$year == cell$level & d$area == cell$group
      ll <- function(delta) sum(mgcv::ldTweedie(d$tweedie[i], exp(a$eta[i] + delta),
        p = a$extra$power, phi = a$dispersion[i])[, 1])
      expect_lt(abs(cell$adjustment - optimize(ll, c(-5, 5), maximum = TRUE, tol = 1e-10)$maximum), 1e-6)
      expect_equal(2 * (ll(cell$adjustment) - ll(cell$lower - cell$baseline)), qchisq(.95, 1), tolerance = 1e-6)
      expect_equal(2 * (ll(cell$adjustment) - ll(cell$upper - cell$baseline)), qchisq(.95, 1), tolerance = 1e-6)
    }
    if (backend %in% c("sdmTMB", "tinyVAST")) {
      native <- .resid_tmb_object(m, backend, "fitted")
      get_ll <- function(par) { r <- native$obj$report(par); -sum(if (backend == "sdmTMB") r$jnll_obs else r$negloglik_i) }
      expect_equal(.implied_loglik(0, a$observed, a$eta, a$dispersion, "tweedie", a$extra), get_ll(native$par), tolerance = 1e-8)
      shifted <- native$par
      k <- which(names(shifted) == if (backend == "sdmTMB") "b_j" else "alpha_j")[1L]
      shifted[k] <- shifted[k] + .2
      expect_equal(.implied_loglik(.2, a$observed, a$eta, a$dispersion, "tweedie", a$extra), get_ll(shifted), tolerance = 1e-8)
    }
  }
  expect_equal(.implied_shift(rep(0, 10), rep(1, 10), rep(.8, 10), "tweedie", list(power = 1.5))$shift, -Inf)
})

test_that("sdmTMB Gamma, Poisson, and NB2 retain native scales and offsets", {
  skip_if_not_installed("sdmTMB")
  d <- expanded_implied_data()
  for (name in c("gamma", "poisson", "nbinom2")) {
    d$y <- d[[name]]
    Family <- switch(name, gamma = Gamma("log"), poisson = poisson(), nbinom2 = sdmTMB::nbinom2())
    m <- sdmTMB::sdmTMB(y ~ year + area + x, data = d, offset = d$effort, family = Family, spatial = "off", silent = TRUE)
    original <- m$tmb_obj$env$last.par.best
    a <- .implied_adapter(m, NULL, "year", "area", "year_group")
    z <- implied_effects(m, year = "year", groups = "area")
    native <- .resid_tmb_object(m, "sdmTMB", "fitted")
    expect_equal(.implied_loglik(0, a$observed, a$eta, a$dispersion, a$family), -sum(native$obj$report(native$par)$jnll_obs), tolerance = 1e-8)
    shifted <- native$par; shifted[which(names(shifted) == "b_j")[1]] <- shifted[which(names(shifted) == "b_j")[1]] + .2
    expect_equal(.implied_loglik(.2, a$observed, a$eta, a$dispersion, a$family), -sum(native$obj$report(shifted)$jnll_obs), tolerance = 1e-8)
    if (name == "gamma") expect_equal(a$dispersion, rep(1 / exp(m$parlist$ln_phi), nrow(d)))
    expect_identical(m$tmb_obj$env$last.par.best, original)
    expect_true(all(z$table$status == "ok"))
  }
})

test_that("joint Gamma and count displays agree with native glmmTMB likelihoods", {
  skip_if_not_installed("glmmTMB")
  d <- expanded_implied_data()
  set.seed(347); present <- rbinom(nrow(d), 1, .55)
  families <- list(Gamma = glmmTMB::ziGamma("log"),
    poisson = poisson(), nbinom2 = glmmTMB::nbinom2(),
    truncated_poisson = glmmTMB::truncated_poisson(),
    truncated_nbinom2 = glmmTMB::truncated_nbinom2(), tweedie = glmmTMB::tweedie())
  for (name in names(families)) {
    base <- switch(name, Gamma = d$gamma, poisson = d$poisson, nbinom2 = d$nbinom2,
      truncated_poisson = pmax(1, d$poisson), truncated_nbinom2 = pmax(1, d$nbinom2), tweedie = d$tweedie)
    d$y <- present * base
    m <- glmmTMB::glmmTMB(y ~ year + area + x + offset(effort), data = d,
      ziformula = ~year + area, family = families[[name]])
    a <- .implied_adapter(m, NULL, "year", "area", "year_group", "combined")
    expect_equal(.implied_joint_loglik(c(0, 0), a$observed, a$eta, a$dispersion,
      a$positive_family, a$joint_kind, a$extra), as.numeric(logLik(m)), tolerance = 1e-7)
    z <- implied_effects(m, year = "year", groups = "area", component = "combined")
    if (name == "tweedie") expect_equal(z$metadata$power, a$extra$power)
    expect_equal(z$table$baseline, as.numeric(tapply(predict(m, type = "response"),
      interaction(d$year, d$area), mean)), tolerance = 1e-7)
    for (part in if (a$joint_kind == "hurdle") c("positive", "encounter") else c("conditional", "zero_inflation")) {
      single <- implied_effects(m, year = "year", groups = "area", component = part)
      expect_equal(single$metadata$likelihood, if (a$joint_kind == "hurdle") "Observed hurdle membership and positive-response likelihood" else "Full mixture likelihood; zeros are not assigned to a latent component")
      if (a$joint_kind == "zero_inflated") expect_equal(single$table$n, rep(55L, 6))
      expect_s3_class(ggplot2::ggplot_build(plot(single)), "ggplot_built")
    }
    good <- z$table$status == "ok"
    expect_true(any(good))
    expect_true(all(z$table$lower[good] < z$table$estimate[good] & z$table$upper[good] > z$table$estimate[good]))
    # Independent native-density likelihood and constrained response profile.
    # Check an interior cell, rather than testing only interval ordering.
    j <- which(good)[1L]; cell <- z$table[j, ]
    i <- d$year == cell$level & d$area == cell$group
    y <- d$y[i]; ep <- a$eta[i, 1L]; em <- a$eta[i, 2L]; disp <- a$dispersion[i]
    p <- y > 0
    density <- function(delta) {
      value <- switch(name,
        Gamma = dgamma(y[p], shape = 1 / disp[p], rate = exp(-em[p] - delta) / disp[p], log = TRUE),
        poisson = dpois(y[p], exp(em[p] + delta), log = TRUE),
        nbinom2 = dnbinom(y[p], size = disp[p], mu = exp(em[p] + delta), log = TRUE),
        truncated_poisson = dpois(y[p], exp(em[p] + delta), log = TRUE) - log(-expm1(-exp(em[p] + delta))),
        truncated_nbinom2 = dnbinom(y[p], size = disp[p], mu = exp(em[p] + delta), log = TRUE) - log1p(-dnbinom(0, size = disp[p], mu = exp(em[p] + delta))),
        tweedie = mgcv::ldTweedie(y[p], exp(em[p] + delta), a$extra$power, disp[p])[, 1L])
      value
    }
    ll <- function(theta) {
      pr <- plogis(ep + theta[1L]); mu <- exp(em + theta[2L])
      nonzero <- sum(log(pr[p]) + density(theta[2L]))
      f0 <- switch(name, Gamma = rep(0, sum(!p)), truncated_poisson = rep(0, sum(!p)),
        truncated_nbinom2 = rep(0, sum(!p)), poisson = dpois(0, mu[!p]),
        nbinom2 = dnbinom(0, size = disp[!p], mu = mu[!p]),
        tweedie = exp(-mu[!p]^(2 - a$extra$power) / ((2 - a$extra$power) * disp[!p])))
      nonzero + sum(log(1 - pr[!p] + pr[!p] * f0))
    }
    theta <- c(cell$encounter_adjustment, cell$positive_adjustment)
    expect_equal(.implied_joint_loglik(theta, y, a$eta[i, ], disp,
      a$positive_family, a$joint_kind, a$extra), ll(theta), tolerance = 1e-8)
    if (!startsWith(name, "truncated_")) {
      profile <- function(target) optimize(function(gate) {
        amount <- log(target / mean(plogis(ep + gate) * exp(em)))
        ll(c(gate, amount))
      }, c(-20, 20), maximum = TRUE, tol = 1e-10)$objective
      for (bound in c(cell$lower, cell$upper)) expect_equal(2 * (ll(theta) - profile(bound)), qchisq(.95, 1), tolerance = 1e-5)
    }
    expect_identical(unserialize(serialize(z, NULL)), z)
    expect_lt(as.numeric(object.size(z)), 30000)
  }
})

test_that("delta-Gamma is supported in both spatial backends", {
  d <- expanded_implied_data(); set.seed(423); d$y <- rbinom(nrow(d), 1, .6) * d$gamma
  for (backend in c("sdmTMB", "tinyVAST")) {
    if (!requireNamespace(backend, quietly = TRUE)) next
    Family <- if (backend == "sdmTMB") sdmTMB::delta_gamma() else tinyVAST::delta_gamma()
    m <- if (backend == "sdmTMB") sdmTMB::sdmTMB(y ~ year + area + x, data = d, family = Family, spatial = "off", silent = TRUE) else
      tinyVAST::tinyVAST(y ~ year + area + x, delta_options = list(formula = ~year + area + x), data = d, family = Family, spatial_domain = NULL,
        control = tinyVAST::tinyVASTcontrol(calculate_deviance_explained = FALSE))
    a <- .implied_adapter(m, NULL, "year", "area", "year_group", "combined")
    native <- .resid_tmb_object(m, backend, "fitted"); r <- native$obj$report(native$par)
    expect_equal(.implied_joint_loglik(c(0, 0), a$observed, a$eta, a$dispersion,
      a$positive_family, a$joint_kind), -sum(if (backend == "sdmTMB") r$jnll_obs else r$negloglik_i), tolerance = 1e-8)
    for (part in c("encounter", "positive", "combined")) {
      z <- implied_effects(m, year = "year", groups = "area", component = part)
      expect_true(all(z$table$status == "ok"))
      expect_s3_class(ggplot2::ggplot_build(plot(z)), "ggplot_built")
    }
  }
})

test_that("standalone truncated counts retain their conditional likelihood", {
  skip_if_not_installed("glmmTMB")
  d <- expanded_implied_data()
  for (name in c("truncated_poisson", "truncated_nbinom2")) {
    d$y <- pmax(1, d[[sub("^truncated_", "", name)]])
    Family <- if (name == "truncated_poisson") glmmTMB::truncated_poisson() else glmmTMB::truncated_nbinom2()
    m <- glmmTMB::glmmTMB(y ~ year + area + x, data = d, family = Family)
    a <- .implied_adapter(m, NULL, "year", "area", "year_group")
    expect_equal(.implied_loglik(0, a$observed, a$eta, a$dispersion, name), as.numeric(logLik(m)), tolerance = 1e-8)
    z <- implied_effects(m, year = "year", groups = "area")
    expect_true(all(z$table$status == "ok"))
    expect_equal(exp(.implied_positive_logmean(a$eta, a$dispersion, name)),
      as.numeric(predict(m, type = "response")), tolerance = 1e-8)
    i <- d$year == z$table$level[1] & d$area == z$table$group[1]
    ll <- function(delta) .implied_loglik(delta, a$observed[i], a$eta[i], a$dispersion[i], name)
    for (end in c(z$table$lower[1], z$table$upper[1])) {
      expect_equal(2 * (ll(z$table$adjustment[1]) - ll(end - z$table$baseline[1])), qchisq(.95, 1), tolerance = 1e-6)
    }
    expect_equal(.implied_shift(rep(1, 10), rep(0, 10), rep(2, 10), name)$shift, -Inf)
  }
})

test_that("constant gates and joint boundary cells are explicit", {
  skip_if_not_installed("glmmTMB")
  d <- expanded_implied_data(); set.seed(329)
  d$y <- rbinom(nrow(d), 1, .6) * d$gamma
  m <- glmmTMB::glmmTMB(y ~ year + area + x, ziformula = ~1,
    data = d, family = glmmTMB::ziGamma("log"))
  z <- implied_effects(m, year = "year", groups = "area", component = "encounter", interval = "none")
  expect_equal(z$table$baseline, rep(0, 6))
  expect_null(z$metadata$year_term)
  expect_error(implied_effects(m, year = "year", groups = "area", component = "encounter", year_term = "year"), "absent")
  expect_error(implied_effects(m, year = "year", groups = "area", component = "combined", method = "traditional"), "Joint implied effects")
  a <- .implied_adapter(m, NULL, "year", "area", "year_group", "combined")
  i <- d$year == levels(d$year)[1] & d$area == "A"
  a$observed[i] <- 0
  a$observed[d$year == levels(d$year)[2] & d$area == "A"] <- 1
  a$data$area <- factor(a$data$area, levels = c("A", "B", "C"))
  boundary <- .implied_joint(a, "area", "year_group", 10L, .95, "none")
  expect_equal(boundary$table$status[1:2], c("boundary_zero", "boundary_one"))
  expect_true(all(boundary$table$status[boundary$table$group == "C"] == "empty"))
  sparse <- .implied_joint(a, "area", "year_group", 100L, .95, "none")
  expect_true(all(sparse$table$status[sparse$table$n > 0] == "sparse"))
  # With no zero responses, an extra-zero gate is at its zero-probability
  # boundary. Do not attempt a flat numerical optimisation and abort the plot.
  a$joint_kind <- "zero_inflated"; a$positive_family <- "poisson"
  a$observed[a$observed > 0] <- pmax(1, round(a$observed[a$observed > 0]))
  boundary <- .implied_joint(a, "area", "year_group", 10L, .95, "none")
  expect_equal(boundary$table$status[1:2], c("boundary_zero", "boundary_one"))
  bad <- m; bad$modelInfo$family$family <- "lognormal"
  expect_error(.implied_glmmtmb_joint_adapter(bad, NULL, "year", "area", "year_group", "combined", NULL), "Joint glmmTMB implied effects require")
})
