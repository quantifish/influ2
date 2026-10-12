lme4_fixture <- local({
  cache <- new.env(parent = emptyenv())
  function(kind = "poisson") {
    skip_if_not_installed("lme4", "1.1.36")
    if (exists(kind, cache, inherits = FALSE)) return(cache[[kind]])
    set.seed(194)
    d <- expand.grid(year = factor(1:4), month = factor(1:6), replicate = 1:15)
    d$depth <- runif(nrow(d), -1, 1) + .15 * as.integer(d$year)
    d$soak <- runif(nrow(d), .5, 2)
    eta <- 1 + .16 * as.integer(d$year) + .4 * d$depth + .2 * d$soak +
      c(-.8, -.4, -.1, .2, .5, .9)[d$month]
    mu <- exp(eta)
    d$y <- switch(kind,
      poisson = rpois(nrow(d), mu), nbinom2 = rnbinom(nrow(d), mu = mu, size = 3),
      Gamma = rgamma(nrow(d), shape = 5, scale = mu / 5),
      binomial = rbinom(nrow(d), 1, plogis(eta - 2)),
      trials = rbinom(nrow(d), 5, plogis(eta - 2)),
      lognormal = exp(eta + rnorm(nrow(d), sd = .35)),
      gaussian = eta + rnorm(nrow(d), sd = .35),
      slope = eta + c(-.5, -.3, -.1, .1, .3, .5)[d$month] * d$depth + rnorm(nrow(d), sd = .35))
    d$trials <- 5
    d$failure <- 5 - d$y
    if (kind == "nbinom2") m <- lme4::glmer.nb(y ~ year + depth + soak + (1 | month), data = d) else {
      if (kind %in% c("gaussian", "lognormal", "slope")) {
        f <- if (kind == "lognormal") log(y) ~ year + depth + soak + (1 | month) else {
          if (kind == "slope") y ~ year + depth + soak + (depth | month) else y ~ year + depth + soak + (1 | month)
        }
        m <- lme4::lmer(f, data = d, REML = FALSE)
      } else {
        f <- if (kind == "trials") cbind(y, failure) ~ year + depth + soak + (1 | month) else y ~ year + depth + soak + (1 | month)
        fam <- switch(kind, poisson = poisson(), Gamma = Gamma(link = "log"), binomial(), trials = binomial())
        m <- lme4::glmer(f, family = fam, data = d)
      }
    }
    cache[[kind]] <- list(model = m, data = d, transform = if (kind == "lognormal") "log" else "identity")
    cache[[kind]]
  }
})

test_that("lme4 native predictions and joint covariance underpin shared influence/CDI", {
  for (kind in c("poisson", "nbinom2", "Gamma", "binomial", "gaussian", "lognormal", "slope", "trials")) {
    f <- lme4_fixture(kind)
    m <- f$model
    x <- influ(m, focus = "year", response_transform = f$transform)
    expect_s3_class(x, "influ_diag")
    expect_identical(x$backend, "lme4")
    expect_null(x$model)
    expect_null(x$draws)
    expect_true(all(c("conditional", "random_effects") %in% x$influence$component))
    expect_true(all(is.finite(x$influence$estimate)))
    V <- as.matrix(vcov(m, full = TRUE))
    q <- length(lme4::getME(m, "b"))
    expect_equal(.lme4_joint_covariance(m), V)
    native_eta <- as.numeric(predict(m, type = "link", re.form = NULL))
    expect_equal(x$metadata$lme4$reference_predictor, mean(native_eta))
    Z <- lme4::getME(m, "Z")
    B <- .term_contrast(Z, .focus_info(f$data, "year"), rep(1, nrow(f$data)))
    rows <- subset(x$influence, term == "random_effects" & scale == "link")
    expect_equal(rows$estimate, as.numeric(B %*% lme4::getME(m, "b")))
    expect_equal(rows$std_error, sqrt(pmax(0, rowSums((B %*% V[seq_len(q), seq_len(q), drop = FALSE]) * B))))
    if (kind %in% c("binomial", "trials")) {
      natural <- subset(x$influence, term == "random_effects" & scale == "difference")
      expect_equal(natural$estimate, plogis(mean(native_eta) + rows$estimate) - plogis(mean(native_eta)))
    }
    expect_s3_class(plot(x, type = "cdi", term = "random_effects"), "patchwork")
    if (kind != "slope") {
      random_coefficients <- subset(x$coefficients, term == "random_effects")
      expect_setequal(random_coefficients$level, levels(f$data$month))
      expect_setequal(subset(x$composition, term == "random_effects")$term_level, levels(f$data$month))
      expect_identical(x$metadata$lme4$random_cdi_group, "month")
    }
    expect_s3_class(plot(x, type = "cdi", term = "depth"), "patchwork")
    expect_s3_class(plot(x, type = "components"), "ggplot")
    expect_s3_class(plot_compare(list(A = x, B = x)), "ggplot")
    expect_s3_class(cpue_index(m, method = "year_effect", response_transform = f$transform), "influ_index")
    expect_true(as.numeric(object.size(x)) < 1e6)
  }
})

test_that("lme4 reference grids preserve polynomial bases and random-level subsets", {
  f <- lme4_fixture("slope")
  m <- update(f$model, . ~ year + poly(depth, 2) + soak + (depth | month), data = f$data)
  grid <- f$data[c(1, 3, 8, 17), c("year", "depth", "soak", "month")]
  X <- .lme4_X(m, grid)
  Z <- .lme4_Z(m, f$data, grid)
  eta <- as.numeric(X %*% lme4::fixef(m) + Z %*% lme4::getME(m, "b"))
  expect_equal(eta, as.numeric(predict(m, newdata = grid, re.form = NULL)))
  x <- influ(m, focus = "year", data = f$data, reference_data = grid)
  expect_identical(x$metadata$reference, "prediction_grid")
  bad <- grid
  bad$month <- "new vessel"
  expect_error(influ(m, focus = "year", data = f$data, reference_data = bad), "new levels")
  expect_error(influ(m, focus = "year", data = f$data, reference_data = grid[c("year", "depth", "soak")]), "random-effect predictors|month")
})

test_that("joint fixed/random influence simulations share draws and restore caller RNG", {
  f <- lme4_fixture()
  set.seed(74)
  rng <- .Random.seed
  x <- influ(f$model, focus = "year", uncertainty = "simulation",
    ndraws = 60, seed = 9, retain = "derived_draws")
  expect_identical(.Random.seed, rng)
  expect_equal(x, influ(f$model, focus = "year", uncertainty = "simulation",
    ndraws = 60, seed = 9, retain = "derived_draws"))
  expected <- .draw_mvn(60, c(as.numeric(lme4::getME(f$model, "b")), lme4::fixef(f$model)), .lme4_joint_covariance(f$model), 9)
  q <- length(lme4::getME(f$model, "b"))
  X <- lme4::getME(f$model, "X")
  depth <- which(attr(X, "assign") == match("depth", attr(terms(f$model), "term.labels")))
  B <- .term_contrast(X[, depth, drop = FALSE], .focus_info(f$data, "year"), rep(1, nrow(f$data)))
  expect_equal(x$draws[, grepl("conditional\\|depth\\|.*\\|link$", colnames(x$draws)), drop = FALSE],
    expected[, q + depth, drop = FALSE] %*% t(B), ignore_attr = TRUE)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  disk <- influ(f$model, focus = "year", uncertainty = "simulation", ndraws = 60,
    seed = 9, retain = "disk", draws_path = path)
  expect_null(disk$draws)
  expect_equal(readRDS(path), x$draws)
})

test_that("lme4 standardised and integrated index covariance follows native predictions", {
  for (kind in c("poisson", "nbinom2", "Gamma", "binomial", "gaussian", "lognormal", "trials")) {
    f <- lme4_fixture(kind)
    grid <- data.frame(depth = c(0, .5), soak = c(1, 1.2))
    x <- cpue_index(f$model, reference_data = grid, reference_weights = c(1, 3), response_transform = f$transform)
    d <- grid[rep(1:2, 4), ]
    d$year <- factor(rep(1:4, each = 2), levels = 1:4)
    native <- predict(f$model, newdata = d, re.form = NA, type = "response")
    if (kind == "lognormal") native <- exp(native + sigma(f$model)^2 / 2)
    expected <- as.numeric(rowsum(native * rep(c(.25, .75), 4), d$year))
    expect_equal(x$table$Mean, expected)
    expect_identical(x$metadata$backend, "lme4")
    expect_match(x$metadata$random_effects, "not population-marginal")
    expect_match(x$metadata$variance_parameters, "not propagated")
    expect_equal(diag(index_vcov(x, "response")), x$table$SD^2, ignore_attr = TRUE)
    expect_equal(cpue_index(f$model, reference_data = grid, reference_weights = c(1, 3), batch_size = 1,
      response_transform = f$transform)$table, x$table, tolerance = 1e-10)
    total <- integrate_index(f$model, grid, area = c(1, 3), area_units = "km^2",
      response_units = "response/km^2", response_transform = f$transform)
    expect_equal(total$table$Mean, 4 * x$table$Mean)
    expect_equal(index_vcov(total, "response"), 16 * index_vcov(x, "response"))
    expect_s3_class(plot_index(total), "ggplot")
    expect_s3_class(plot_compare(list(A = x, B = x)), "ggplot")
    expect_s3_class(plot_index(x, type = "covariance"), "ggplot")
    expect_true(all(is.na(x$table$Median)))
    expect_null(cpue_index(f$model, reference_data = grid, uncertainty = "none", response_transform = f$transform)$covariance$response)
  }
})

test_that("lme4 PIT and DHARMa simulations preserve native draws, batching, and trials", {
  for (kind in c("poisson", "nbinom2", "Gamma", "binomial", "trials", "gaussian", "lognormal")) {
    f <- lme4_fixture(kind)
    for (conditioning in c("fitted", "new_effects")) {
      a <- .resid_lme4_adapter(f$model, f$data, NULL, "auto", conditioning, f$transform)
      set.seed(5)
      actual <- a$simulate(1:3)
      set.seed(5)
      expected <- vapply(1:3, function(i) {
        v <- simulate(f$model, nsim = 1, newdata = f$data, re.form = if (conditioning == "fitted") NULL else NA,
          weights = if (kind == "trials") f$data$trials else rep(1, nrow(f$data)))[[1]]
        if (kind == "trials") v <- v[, 1]
        if (kind == "lognormal") v <- exp(v)
        as.numeric(v)
      }, numeric(nrow(f$data)))
      expect_equal(actual, expected, ignore_attr = TRUE)
      set.seed(12)
      rng <- .Random.seed
      x <- influ_residuals(f$model, nsim = 24, batch_size = 6, seed = 18,
        conditioning = conditioning, response_transform = f$transform, groups = "month")
      expect_identical(.Random.seed, rng)
      y <- influ_residuals(f$model, nsim = 24, batch_size = 1, seed = 18,
        conditioning = conditioning, response_transform = f$transform, groups = "month")
      expect_equal(x$observations, y$observations)
      # The frozen shared engine chooses ECDF knots from the first batch.
      # Native response vectors, ranks, fitted means, and calibration do not
      # change with batch size; ECDF representations can use different knots.
      expect_equal(x$calibration, y$calibration)
      expect_s3_class(plot(x), "patchwork")
      expect_s3_class(plot(x, type = "qq"), "ggplot")
      expect_s3_class(plot_grouped_residuals(x, groups = "month", min_n = 2), "ggplot")
      expect_s3_class(plot_predicted_residuals(x), "ggplot")
      if (kind == "trials") {
        expect_identical(x$metadata$response_kind, "grouped_binomial")
        expect_equal(x$observations$observed, f$data$y)
      }
      if (requireNamespace("DHARMa", quietly = TRUE)) {
        h <- influ_residuals(f$model, nsim = 24, seed = 18, conditioning = conditioning,
          response_transform = f$transform, residual_method = "dharma", retain_dharma = TRUE)
        expect_s3_class(h$dharma, "DHARMa")
        expect_equal(h$observations$pit, h$dharma$scaledResiduals)
        expect_equal(h$dharma$observedResponse, a$observed)
      }
    }
  }
})

test_that("lme4 implied effects use native likelihood parameters, not residual scores", {
  for (kind in c("poisson", "nbinom2", "Gamma", "binomial", "gaussian", "lognormal", "trials")) {
    f <- lme4_fixture(kind)
    x <- implied_effects(f$model, groups = "month", min_n = 3, interval = "none", response_transform = f$transform)
    expect_identical(x$metadata$backend, "lme4")
    expect_s3_class(plot(x), "ggplot")
    expect_s3_class(plot_implied_residuals(f$model, groups = "month", min_n = 3,
      interval = "none", response_transform = f$transform), "ggplot")
    a <- .implied_adapter(f$model, NULL, NULL, "month", "year_group", response_transform = f$transform)
    expect_equal(a$eta, as.numeric(predict(f$model, type = "link")))
    if (kind == "Gamma") expect_equal(a$dispersion, rep(sigma(f$model)^2, nrow(f$data)))
    if (kind == "nbinom2") expect_equal(a$dispersion, rep(lme4::getME(f$model, "glmer.nb.theta"), nrow(f$data)))
    if (kind == "lognormal") {
      traditional <- implied_effects(f$model, groups = "month", min_n = 3,
        method = "traditional", interval = "none", response_transform = "log")
      expect_equal(x$table$adjustment, traditional$table$adjustment)
      expect_equal(x$table$estimate, traditional$table$estimate)
    }
  }
})

test_that("lme4 step refits retain random terms and make NB dispersion intent explicit", {
  f <- lme4_fixture()
  steps <- influ_steps(f$model, refit = TRUE, uncertainty = "none", keep_fits = TRUE)
  expect_s3_class(steps, "influ_steps")
  expect_s3_class(plot(steps), "ggplot")
  expect_identical(length(steps$fits), 3L)
  for (m in steps$fits) expect_equal(length(lme4::getME(m, "b")), 6)
  n <- lme4_fixture("nbinom2")
  expect_error(influ_steps(n$model, refit = TRUE), "reestimate_nb")
  for (option in c(FALSE, TRUE)) {
    z <- influ_steps(n$model, refit = TRUE, uncertainty = "none",
      steps = list(Reduced = ~ year + depth + (1 | month), Full = ~ .),
      refit_args = list(reestimate_nb = option), keep_fits = TRUE)
    expect_s3_class(z, "influ_steps")
    if (!option) expect_equal(lme4::getME(z$fits[[1]], "glmer.nb.theta"), lme4::getME(n$model, "glmer.nb.theta"))
  }
  expect_s3_class(influ_steps(list(A = f$model, B = f$model)), "influ_steps")
})

test_that("lme4 criteria keep native ML/REML values and never substitute cAIC", {
  f <- lme4_fixture("gaussian")
  reml <- update(f$model, REML = TRUE, data = f$data)
  x <- table_criterion(list(ML = f$model, REML = reml), criterion = c("auto", "cAIC", "loo"))
  expect_equal(x$AIC, c(AIC(f$model), AIC(reml)))
  expect_equal(x$df, c(attr(logLik(f$model), "df"), attr(logLik(reml), "df")))
  expect_true(all(is.na(x$cAIC)))
  expect_true(all(is.na(x$looic)))
  expect_true(is.na(x$AIC_group[2]))
  expect_true(all(is.na(x$delta_AIC)))
  expect_match(x$notes[2], "REML")
  expect_equal(as.numeric(logLik(reml)), x$logLik[2])
})

test_that("lme4 unsupported structures, mismatched data, and dependencies fail explicitly", {
  f <- lme4_fixture()
  d <- f$data
  d$y[1] <- d$y[1] + 1
  expect_error(influ(f$model, focus = "year", data = d), "fitted model frame|fitted response")
  d <- f$data
  d$depth[1] <- d$depth[1] + 1
  expect_error(influ_residuals(f$model, data = d), "fitted model frame|fitted design")
  expect_error(influ_residuals(f$model, conditioning = "conditional_draw"), "fitted.*new_effects")
  expect_error(influ(f$model, focus = "year", response_transform = "log"), "Gaussian lmer")
  expect_error(influ_residuals(f$model, trial_counts = "trials"), "binomial")
  expect_error(influ_residuals(f$model, component = "combined"), "joint")
  logfit <- lme4_fixture("lognormal")
  expect_error(influ(logfit$model, focus = "year"), "Declare")
  expect_error(cpue_index(logfit$model, reference_data = data.frame(depth = 0, soak = 1)), "Declare")
  expect_error(cpue_index(f$model, reference_data = data.frame(depth = 0)), "soak")
  expect_error(cpue_index(f$model, reference_data = data.frame(depth = 0, soak = 1), retain = "draws"), "Draw retention")
  bad <- f$model
  bad@optinfo$conv$opt <- 1L
  expect_error(influ(bad, focus = "year"), "converged")
  expect_error(.lme4_check(structure(list(), class = "merMod")), "lmer.*glmer")
  local_mocked_bindings(.model_backend_available = function(package) FALSE)
  expect_error(influ(f$model, focus = "year"), "Package 'lme4'")
  expect_error(cpue_index(f$model), "Package 'lme4'")
  expect_error(influ_residuals(f$model), "Package 'lme4'")
})

test_that("lme4 omitted rows, offsets, weighted trials, and native state stay aligned", {
  f <- lme4_fixture("trials")
  d <- f$data
  d$proportion <- d$y / d$trials
  m <- lme4::glmer(proportion ~ year + depth + soak + (1 | month),
    weights = trials, family = binomial(), data = d)
  expect_error(influ_residuals(m, nsim = 20), "trial_counts")
  x <- influ_residuals(m, nsim = 24, trial_counts = "trials", conditioning = "fitted")
  expect_equal(x$observations$observed, d$y)
  expect_equal(x$observations$trials, d$trials)
  a <- .resid_lme4_adapter(m, d, "trials", "auto", "fitted", "identity")
  set.seed(27)
  sims <- a$simulate(1:10)
  expect_true(all(sims >= 0 & sims <= d$trials & sims == round(sims)))
  expect_true(any(sims > 0 & sims < d$trials))
  expect_error(influ_residuals(m, trial_counts = "depth"), "must match")
  expect_error(implied_effects(m, groups = "month"), "Non-unit")

  p <- lme4_fixture()
  d <- p$data
  d$exposure <- rep(c(.5, 1, 2), length.out = nrow(d))
  d$depth[c(2, 7)] <- NA_real_
  fit <- lme4::glmer(y ~ year + depth + soak + offset(log(exposure)) + (1 | month),
    family = poisson(), data = d, na.action = na.exclude)
  before <- list(beta = lme4::fixef(fit), b = lme4::getME(fit, "b"),
    theta = lme4::getME(fit, "theta"), call = getCall(fit), frame = model.frame(fit))
  checks <- influ_residuals(fit, nsim = 24, data = d, conditioning = "fitted")
  expect_identical(checks$observations$row, rownames(model.frame(fit)))
  expect_equal(nrow(checks$observations), nrow(d) - 2)
  grid <- data.frame(depth = 0, soak = 1, exposure = c(.5, 2))
  index <- cpue_index(fit, reference_data = grid)
  expect_true(all(is.finite(index$table$SD)))
  implied <- implied_effects(fit, data = d, groups = "month", interval = "none")
  expect_equal(sum(implied$table$n), nrow(model.frame(fit)))
  steps <- influ_steps(fit, refit = TRUE, uncertainty = "none", keep_fits = TRUE)
  expect_equal(vapply(steps$fits, nobs, numeric(1)), rep(nrow(model.frame(fit)), length(steps$fits)), ignore_attr = TRUE)
  after <- list(beta = lme4::fixef(fit), b = lme4::getME(fit, "b"),
    theta = lme4::getME(fit, "theta"), call = getCall(fit), frame = model.frame(fit))
  expect_equal(after, before)
})

test_that("lme4 contrasts and crossed random effects preserve native reference predictions", {
  f <- lme4_fixture("gaussian")
  d <- f$data
  d$vessel <- factor(rep(1:10, length.out = nrow(d)))
  m <- lme4::lmer(y ~ year + depth + soak + (1 | month) + (1 | vessel),
    data = d, contrasts = list(year = "contr.sum"))
  grid <- d[c(3, 37, 80), c("year", "depth", "soak", "month", "vessel")]
  X <- .lme4_X(m, grid)
  Z <- .lme4_Z(m, d, grid)
  expect_equal(as.numeric(X %*% lme4::fixef(m) + Z %*% lme4::getME(m, "b")),
    as.numeric(predict(m, newdata = grid)), tolerance = 1e-10)
  expect_s3_class(influ(m, focus = "year", data = d, reference_data = grid), "influ_diag")
  expect_s3_class(cpue_index(m, reference_data = grid[c("depth", "soak")]), "influ_index")
  changed <- d
  changed$month <- factor(rev(as.character(changed$month)), levels = levels(d$month))
  expect_error(influ_residuals(m, data = changed), "fitted model frame|random-effect design")
})
