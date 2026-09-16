implied_tiny_fixture <- local({
  value <- NULL
  function() {
    skip_if_not_installed("tinyVAST")
    if (!is.null(value)) return(value)
    set.seed(1239)
    d <- expand.grid(year = factor(2011:2013), area = factor(c("A", "B")), record = 1:65)
    d$x <- runif(nrow(d), -1, 1)
    d$effort <- runif(nrow(d), -.4, .4)
    d$vessel <- factor(sample(1:12, nrow(d), TRUE))
    d$year_scaled <- as.numeric(d$year) - 2
    re <- rnorm(12, sd = .6)[d$vessel]
    eta <- .8 + .25 * d$year_scaled + .3 * (d$area == "B") + .8 * sin(3 * d$x) + re + d$effort
    d$response <- rbinom(nrow(d), 1, plogis(.6 + .2 * d$year_scaled + re)) * rlnorm(nrow(d), eta - .7^2 / 2, .7)
    d$group <- as.character(d$area)
    d$group[d$record <= 4] <- "zero"
    d$group[d$record > 4 & d$record <= 8] <- "one"
    d$response[d$group == "zero"] <- 0
    d$response[d$group == "one"] <- 1
    rownames(d) <- paste0("obs", seq_len(nrow(d)))
    m <- tinyVAST::tinyVAST(response ~ year_scaled + area + x + s(vessel, bs = "re"),
      delta_options = list(formula = ~year + area + s(x, k = 4) + offset(effort) + s(vessel, bs = "re")),
      family = tinyVAST::delta_lognormal(), data = d, spatial_domain = NULL,
      control = tinyVAST::tinyVASTcontrol(calculate_deviance_explained = FALSE))
    value <<- list(model = m, data = d)
    value
  }
})

test_that("tinyVAST delta likelihoods and local shifts preserve fitted effects", {
  f <- implied_tiny_fixture(); m <- f$model; d <- f$data
  mode <- m$obj$env$last.par.best; rng <- .Random.seed
  pos <- implied_effects(m, groups = "area", year = "year", component = "positive")
  enc <- implied_effects(m, groups = "area", year = "year", year_term = "year_scaled", component = "encounter")
  joint <- implied_effects(m, groups = "area", year = "year", component = "combined")
  expect_identical(m$obj$env$last.par.best, mode)
  expect_identical(.Random.seed, rng)
  r <- m$obj$report(mode); sigma <- exp(m$internal$parlist$log_sigma)
  y <- d$response; ip <- y > 0
  native <- sum(dbinom(as.numeric(ip), 1, plogis(r$p_i), log = TRUE)) +
    sum(dlnorm(y[ip], r$p2_i[ip] - sigma^2 / 2, sigma, log = TRUE))
  expect_equal(native, -sum(r$negloglik_i), tolerance = 1e-10)
  X <- m$tmb_inputs$tmb_data$X2_ij; beta <- m$internal$parlist$alpha2_j
  fixed <- as.numeric(X %*% beta)
  expect_gt(max(abs(r$p2_i - fixed - d$effort)), .1)
  for (j in seq_len(nrow(joint$table))) {
    cell <- joint$table[j, ]; i <- d$year == cell$level & d$area == cell$group
    positive <- i & ip
    d1 <- enc$table$adjustment[j]; d2 <- pos$table$adjustment[j]
    expect_equal(d2, mean(log(y[positive]) - r$p2_i[positive] + sigma^2 / 2))
    expect_equal(cell$estimate, mean(plogis(r$p_i[i] + d1) * exp(r$p2_i[i] + d2)))
    expect_equal(cell$n_positive, sum(positive))
    ll <- function(t) sum(dbinom(as.numeric(ip[i]), 1, plogis(r$p_i[i] + t[1]), log = TRUE)) +
      sum(dlnorm(y[positive], r$p2_i[positive] + t[2] - sigma^2 / 2, sigma, log = TRUE))
    mle <- optim(c(0, 0), function(t) -ll(t), method = "BFGS", control = list(reltol = 1e-12))$par
    expect_equal(c(d1, d2), mle, tolerance = 1e-5)
    profile <- function(target) optimize(function(e) {
      p <- log(target / mean(plogis(r$p_i[i] + e) * exp(r$p2_i[i])))
      ll(c(e, p))
    }, c(-15, 15), maximum = TRUE, tol = 1e-10)$objective
    for (bound in c(cell$lower, cell$upper)) expect_equal(2 * (ll(c(d1, d2)) - profile(bound)), qchisq(.95, 1), tolerance = 1e-5)
  }
  expect_equal(pos$table, implied_effects(m, data = d[nrow(d):1, ], year = "year", groups = "area", component = "positive")$table)
  restored <- unserialize(serialize(m, NULL))
  expect_equal(pos$table, implied_effects(restored, year = "year", groups = "area", component = "positive")$table)
  expect_s3_class(ggplot2::ggplot_build(plot(joint)), "ggplot_built")
  expect_lt(as.numeric(object.size(joint)), 25000)
})

test_that("tinyVAST single response families match their native likelihood", {
  skip_if_not_installed("tinyVAST")
  set.seed(179)
  d <- expand.grid(year = factor(1:3), area = factor(c("A", "B")), record = 1:45)
  d$x <- runif(nrow(d)); d$effort <- runif(nrow(d), -.3, .3)
  eta <- .5 + .1 * as.numeric(d$year) + .3 * (d$area == "B") + .2 * d$x + d$effort
  families <- list(gaussian = gaussian(), Gamma = Gamma("log"),
    poisson = poisson(), nbinom2 = tinyVAST::nbinom2(),
    lognormal = tinyVAST::lognormal(), binomial = binomial())
  responses <- list(gaussian = rnorm(nrow(d), eta, .6),
    Gamma = rgamma(nrow(d), shape = 3, scale = exp(eta) / 3),
    poisson = rpois(nrow(d), exp(eta)), nbinom2 = rnbinom(nrow(d), size = 3, mu = exp(eta)),
    lognormal = rlnorm(nrow(d), eta - .6^2 / 2, .6), binomial = rbinom(nrow(d), 1, plogis(eta)))
  for (name in names(families)) {
    d$response <- responses[[name]]
    m <- tinyVAST::tinyVAST(response ~ year + area + x + offset(effort), data = d,
      family = families[[name]], spatial_domain = NULL,
      control = tinyVAST::tinyVASTcontrol(calculate_deviance_explained = FALSE))
    a <- .implied_adapter(m, NULL, "year", "area", "year_group")
    z <- implied_effects(m, year = "year", groups = "area")
    expect_equal(.implied_loglik(0, d$response, a$eta, a$dispersion, a$family),
      -sum(m$obj$report(m$obj$env$last.par.best)$negloglik_i), tolerance = 1e-8)
    native <- .resid_tmb_object(m, "tinyVAST", "fitted")
    # Intercept changes only the mean predictor, holding all nuisance values fixed.
    shifted <- native$par; column <- which(names(shifted) == "alpha_j")[1]
    shifted[column] <- shifted[column] + .2
    expect_equal(.implied_loglik(.2, d$response, a$eta, a$dispersion, a$family),
      -sum(native$obj$report(shifted)$negloglik_i), tolerance = 1e-8)
    for (j in seq_len(nrow(z$table))) {
      cell <- z$table[j, ]; i <- d$year == cell$level & d$area == cell$group
      ll <- function(shift) .implied_loglik(shift, d$response[i], a$eta[i], a$dispersion[i], a$family)
        expect_lt(abs(cell$adjustment - optimize(ll, c(-5, 5), maximum = TRUE, tol = 1e-9)$maximum), 1e-6)
      expect_equal(2 * (ll(cell$adjustment) - ll(cell$lower - cell$baseline)), qchisq(.95, 1), tolerance = 1e-6)
    }
  }
})

test_that("tinyVAST support and ambiguity guards are explicit", {
  f <- implied_tiny_fixture(); m <- f$model
  calc <- function(model = m, ...) implied_effects(model, groups = "group", year = "year", component = "positive", ...)
  z <- calc()
  expect_true(all(subset(z$table, group == "zero")$status == "empty"))
  expect_true(all(subset(z$table, group == "one")$status == "sparse"))
  expect_false(z$metadata$baseline_group_present)
  z <- implied_effects(m, groups = "group", year = "year", component = "combined", min_n = 3)
  expect_true(all(subset(z$table, group == "zero")$status == "empty_positive"))
  expect_true(all(subset(z$table, group == "one")$status == "boundary_one"))
  expect_error(implied_effects(m, groups = "area", year = "year"), "explicit component")
  expect_error(calc(draw_id = 1), "only to brms")
  expect_error(calc(year_term = "x"), "constant within")
  bad <- m; bad$sdrep$pdHess <- FALSE
  expect_error(calc(bad), "positive-definite")
  bad <- m; bad$internal$control$reml <- TRUE
  expect_error(calc(bad), "ML")
  bad <- m; bad$internal$family[[1]]$type <- "poisson_link_delta"
  expect_error(calc(bad), "standard delta-lognormal")
  bad <- m; bad$tmb_inputs$tmb_data$y_i[1] <- -10
  expect_error(calc(bad), "likelihood rows")
  bad <- m; bad$tmb_inputs$tmb_data$weights_i[1] <- 2
  expect_error(calc(bad), "weights")
  bad <- m; bad$tmb_inputs$tmb_data$c_i <- c(0, 1)
  expect_error(calc(bad), "one response")
  bad <- m; bad$internal$parlist <- NULL
  expect_error(calc(bad), "complete retained")
  expect_error(calc(data = f$data[-1, ]), "row names|match")
  expect_error(calc(method = "traditional"), "Gaussian")
})

test_that("tinyVAST implied effects retain non-zero spatial and yearly fields", {
  skip_if_not_installed("tinyVAST")
  skip_if_not_installed("fmesher")
  set.seed(101)
  nx <- 7L; nt <- 6L
  correlation <- exp(-.5 * abs(outer(1:nx, 1:nx, "-")))
  covariance <- kronecker(correlation, correlation)
  sp <- t(chol(.7^2 * covariance + diag(1e-8, nx^2)))
  st <- t(chol(.5^2 * covariance + diag(1e-8, nx^2)))
  epsilon <- t(replicate(nt, as.numeric(st %*% rnorm(nx^2))))
  for (i in 2:nt) epsilon[i, ] <- .45 * epsilon[i - 1, ] + sqrt(1 - .45^2) * epsilon[i, ]
  omega <- as.numeric(sp %*% rnorm(nx^2))
  eta <- 1 + outer(seq(-.15, .15, length.out = nt), rep(1, nx^2)) + outer(rep(1, nt), omega) + epsilon
  d <- expand.grid(time = 1:nt, x = 1:nx, ycoord = 1:nx)
  d$count <- rpois(nrow(d), exp(as.vector(eta)))
  d$var <- "density"; d$dist <- "poisson"
  probability <- .45 + .50 * exp(-(d$x - (1 + (d$time - 1) * (nx - 1) / (nt - 1)))^2 / 8)
  d <- d[runif(nrow(d)) < probability, ]
  d$year <- factor(d$time)
  d$area <- factor(ifelse(d$x <= 3, "west", "east"))
  mesh <- fmesher::fm_mesh_2d(d[c("x", "ycoord")], cutoff = 1)
  m <- tinyVAST::tinyVAST(count ~ year, data = d, family = list(poisson = poisson()), spatial_domain = mesh,
    space_term = "density <-> density, spatial_sd",
    spacetime_term = "density -> density, 1, rho\n density <-> density, 0, spatiotemporal_sd",
    space_columns = c("x", "ycoord"))
  expect_true(m$sdrep$pdHess)
  expect_gt(max(abs(m$internal$parlist$omega_sc)), .01)
  expect_gt(max(abs(m$internal$parlist$epsilon_stc)), .1)
  mode <- m$obj$env$last.par.best; rng <- .Random.seed
  a <- .implied_adapter(m, NULL, "year", "area", "year_group")
  z <- implied_effects(m, year = "year", groups = "area", min_n = 3)
  expect_identical(m$obj$env$last.par.best, mode)
  expect_identical(.Random.seed, rng)
  r <- m$obj$report(mode)
  expect_equal(a$eta, as.numeric(r$p_i))
  expect_equal(.implied_loglik(0, d$count, a$eta, a$dispersion, "poisson"), -sum(r$negloglik_i))
  expect_gt(max(abs(a$eta - drop(m$tmb_inputs$tmb_data$X_ij %*% m$internal$parlist$alpha_j))), .1)
  for (j in which(z$table$status == "ok")) {
    cell <- z$table[j, ]; i <- d$year == cell$level & d$area == cell$group
    expect_equal(cell$adjustment, log(sum(d$count[i]) / sum(exp(r$p_i[i]))))
  }
})
