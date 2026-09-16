implied_brms_fixture <- function(name = "hurdle") {
  skip_if_not_installed("brms")
  skip_if_not_installed("rstan")
  readRDS(test_path("fixtures", "brms-implied", paste0(name, ".rds")))
}

test_that("a native constant brms hurdle probability is not a logit predictor", {
  m <- implied_brms_fixture("hurdle_constant")
  for (id in list(NULL, 17L)) {
    p <- brms::prepare_predictions(m, point_estimate = if (is.null(id)) "mean" else NULL, draw_ids = id)
    expect_false(is.list(p$dpars$hu))
    a <- .implied_adapter(m, NULL, "year", "area", "year_group", "combined", draw_id = id)
    hu <- as.numeric(brms::get_dpar(p, "hu"))
    expect_equal(a$eta[, 1], rep(-qlogis(hu), nrow(m$data)))
    y <- m$data$delta; positive <- y > 0
    ll <- .implied_loglik(0, as.numeric(positive), a$eta[, 1], 1, "binomial") +
      .implied_loglik(0, y[positive], a$eta[positive, 2], a$dispersion[positive], "lognormal")
    expect_equal(ll, sum(brms::log_lik(p, cores = 1)))
    z <- implied_effects(m, year = "year", groups = "area", component = "combined", draw_id = id)
    native_mean <- as.numeric(brms::posterior_epred(m,
      point_estimate = if (is.null(id)) "mean" else NULL, draw_ids = id))
    expect_equal(z$table$baseline, as.numeric(tapply(native_mean, interaction(m$data$year, m$data$area), mean)))
  }
  expect_error(implied_effects(m, year = "year", groups = "area", component = "encounter"), "additive fixed year")
})

test_that("Gaussian log-response brms effects retain traditional equivalence", {
  m <- implied_brms_fixture("gaussian")
  # Exactly the same likelihood and posterior: exp(Y) with log on the LHS
  # equals the original Gaussian Y. This is not a new fitted posterior.
  m$data$normal <- exp(m$data$normal)
  m$formula$formula[[2]] <- quote(log(normal))
  a <- implied_effects(m, year = "year", groups = "area", interval = "descriptive")
  b <- implied_effects(m, year = "year", groups = "area", method = "traditional")
  expect_equal(a$table, b$table)
  expect_true(a$metadata$log_response)
  expect_error(implied_effects(m, year = "year", groups = "area", method = "traditional",
    traditional_scale = "standardised"), "plain Gaussian GLM")
})

test_that("brms single-response adapters reproduce native point and draw likelihoods", {
  for (name in c("gaussian", "lognormal", "Gamma", "poisson", "nbinom2", "bernoulli")) {
    m <- implied_brms_fixture(name)
    for (id in list(NULL, 17L)) {
      set.seed(231); rng <- .Random.seed
      a <- .implied_adapter(m, NULL, "year", "area", "year_group", draw_id = id)
      z <- implied_effects(m, year = "year", groups = "area", draw_id = id)
      expect_identical(.Random.seed, rng)
      p <- brms::prepare_predictions(m, point_estimate = if (is.null(id)) "mean" else NULL,
        draw_ids = id, re_formula = NULL)
      native <- sum(brms::log_lik(p, cores = 1))
      expect_equal(.implied_loglik(0, a$observed, a$eta, a$dispersion, a$family), native, tolerance = 1e-9)
      # Independent native likelihood after a shift, not merely agreement at zero.
      mu <- brms::get_dpar(p, "mu")
      p$dpars$mu <- if (a$family %in% c("gaussian", "lognormal")) mu + .2 else if (a$family == "binomial") {
        plogis(qlogis(mu) + .2)
      } else mu * exp(.2)
      expect_equal(.implied_loglik(.2, a$observed, a$eta, a$dispersion, a$family),
        sum(brms::log_lik(p, cores = 1)), tolerance = 1e-9)
      for (j in seq_len(nrow(z$table))) {
        cell <- z$table[j, ]; i <- a$data$year == cell$level & a$data$area == cell$group
        ll <- function(shift) .implied_loglik(shift, a$observed[i], a$eta[i], a$dispersion[i], a$family)
        expect_lt(abs(cell$adjustment - optimize(ll, c(-5, 5), maximum = TRUE, tol = 1e-9)$maximum), 1e-6)
        for (bound in c(cell$lower, cell$upper)) expect_equal(
          2 * (ll(cell$adjustment) - ll(bound - cell$baseline)), qchisq(.95, 1), tolerance = 1e-6)
      }
      expect_equal(z$table, implied_effects(m, data = m$data[nrow(m$data):1, ],
        year = "year", groups = "area", draw_id = id)$table)
      expect_lt(as.numeric(object.size(z)), 20000)
      expect_match(plot(z)$labels$caption, "not Bayesian credible")
      expect_s3_class(ggplot2::ggplot_build(plot(z)), "ggplot_built")
    }
    expect_equal(brms::log_lik(m, draw_ids = c(1, 17), cores = 1), attr(m, "implied_fixture")$native_log_lik)
  }
})

test_that("brms hurdle effects preserve component orientation and native log-SD", {
  m <- implied_brms_fixture()
  before <- serialize(m, NULL)
  for (id in list(NULL, 17L)) {
    p <- brms::prepare_predictions(m, point_estimate = if (is.null(id)) "mean" else NULL, draw_ids = id)
    mu <- as.numeric(brms::get_dpar(p, "mu"))
    sigma <- as.numeric(brms::get_dpar(p, "sigma"))
    hu <- as.numeric(brms::get_dpar(p, "hu"))
    positive <- implied_effects(m, groups = "area", year = "year", component = "positive", draw_id = id)
    encounter <- implied_effects(m, groups = "area", year = "year", year_term = "year_scaled", component = "encounter", draw_id = id)
    joint <- implied_effects(m, groups = "area", year = "year", component = "combined", draw_id = id)
    a <- .implied_adapter(m, NULL, "year", "area", "year_group", "combined", draw_id = id)
    y <- m$data$delta; pos <- y > 0
    expect_gt(diff(range(sigma)), .001)
    expect_equal(a$eta[, 1], qlogis(1 - hu))
    expect_equal(a$eta[, 2], mu + sigma^2 / 2)
    ll <- .implied_loglik(0, as.numeric(pos), a$eta[, 1], 1, "binomial") +
      .implied_loglik(0, y[pos], a$eta[pos, 2], sigma[pos], "lognormal")
    expect_equal(ll, sum(brms::log_lik(p, cores = 1)))
    for (j in seq_len(nrow(joint$table))) {
      cell <- joint$table[j, ]; i <- m$data$year == cell$level & m$data$area == cell$group
      ip <- i & pos
      d1 <- encounter$table$adjustment[j]; d2 <- positive$table$adjustment[j]
      expect_equal(d2, sum((log(y[ip]) - mu[ip]) / sigma[ip]^2) / sum(1 / sigma[ip]^2))
      expect_equal(cell$estimate, mean(plogis(qlogis(1 - hu[i]) + d1) * exp(mu[i] + sigma[i]^2 / 2 + d2)))
      expect_equal(cell$baseline, mean((1 - hu[i]) * exp(mu[i] + sigma[i]^2 / 2)))
      expect_equal(cell$n_positive, sum(ip))
      # Native brms density after both selected component shifts.
      shifted <- p
      shifted$dpars$mu <- matrix(mu + ifelse(i, d2, 0), nrow = 1)
      shifted$dpars$hu <- matrix(plogis(qlogis(hu) - ifelse(i, d1, 0)), nrow = 1)
      expected <- sum(dbinom(as.numeric(pos[i]), 1, plogis(a$eta[i, 1] + d1), log = TRUE)) +
        sum(dlnorm(y[ip], mu[ip] + d2, sigma[ip], log = TRUE))
      expect_equal(sum(brms::log_lik(shifted, cores = 1)[1, i]), expected)
      profile <- function(target) optimize(function(delta) {
        dpos <- log(target / mean(plogis(a$eta[i, 1] + delta) * exp(a$eta[i, 2])))
        sum(dbinom(as.numeric(pos[i]), 1, plogis(a$eta[i, 1] + delta), log = TRUE)) +
          sum(dlnorm(y[ip], mu[ip] + dpos, sigma[ip], log = TRUE))
      }, c(-15, 15), maximum = TRUE, tol = 1e-10)$objective
      for (bound in c(cell$lower, cell$upper)) expect_equal(2 * (expected - profile(bound)), qchisq(.95, 1), tolerance = 1e-5)
    }
    # The hu baseline must be negated to describe encounter, on ALL rows.
    X <- brms::standata(m)$X_hu
    b <- as.numeric(p$dpars$hu$fe$b)
    columns <- which(attr(X, "assign") %in% c(1, 2))
    baseline <- drop(X[, columns, drop = FALSE] %*% -b[columns]); baseline <- baseline - mean(baseline)
    expect_equal(encounter$table$baseline, as.numeric(tapply(baseline,
      interaction(m$data$year, m$data$area), mean)))
    expect_equal(positive$metadata$n_excluded, sum(!pos))
    expect_equal(joint$metadata$reference, if (is.null(id)) "posterior_mean_parameters" else "joint_posterior_draw")
    expect_s3_class(ggplot2::ggplot_build(plot(joint)), "ggplot_built")
  }
  expect_identical(serialize(m, NULL), before)
})

test_that("brms implied effects fail explicitly for unsupported or ambiguous requests", {
  m <- implied_brms_fixture()
  calc <- function(model = m, ...) implied_effects(model, groups = "area", year = "year", component = "positive", ...)
  expect_error(implied_effects(m, groups = "area", year = "year"), "explicit component")
  expect_error(calc(draw_id = 0), "draw_id")
  expect_error(calc(draw_id = .5), "draw_id")
  expect_error(calc(draw_id = Inf), "draw_id")
  expect_error(calc(draw_id = posterior::ndraws(m) + 1), "exceeds")
  expect_error(calc(year_term = "x"), "constant within")
  expect_error(implied_effects(m, groups = "area", year = "year", component = "combined", year_term = "year"), "baseline")
  expect_error(implied_effects(m, groups = "area", year = "year", component = "encounter"), "year_term")
  bad <- m; bad$fit <- NULL
  expect_error(calc(bad), "complete retained brms")
  bad <- m; bad$family$link <- "log"
  expect_error(calc(bad), "identity")
  bad <- m$data; bad$delta[1] <- bad$delta[1] + 1
  expect_error(calc(data = bad), "match|differ")
  expect_error(calc(method = "traditional"), "Gaussian")
  expect_error(implied_effects(m, groups = "delta", year = "year", component = "positive"), "response")
  one <- implied_brms_fixture("poisson")
  expect_error(implied_effects(one, groups = "area", year = "year", component = "positive"), "single supported")
  one$family$family <- "zero_inflated_poisson"
  expect_error(implied_effects(one, groups = "area", year = "year"), "Supported native")
  for (f in list(brms::bf(delta | weights(x) ~ year + area),
      brms::bf(delta | trunc(lb = .1) ~ year + area),
      brms::bf(delta ~ year + gp(x)), brms::bf(delta ~ year + ar(time = year_scaled)))) {
    bad <- m; bad$formula <- f
    expect_error(calc(bad), "without response additions")
  }
  bad <- implied_brms_fixture("lognormal"); bad$family$link <- "log"
  expect_error(implied_effects(bad, year = "year", groups = "area"), "identity link")
  expect_output(print(calc()), "Posterior-mean")
  expect_output(print(calc(draw_id = 17)), "Joint posterior draw 17")
  expect_match(plot(calc(draw_id = 17))$labels$caption, "Fixed joint posterior draw 17")
  expect_identical(calc(draw_id = 17)$metadata$draw_id, 17)
})
