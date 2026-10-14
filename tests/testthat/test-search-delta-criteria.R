search_delta_criteria_fixture <- function() {
  skip_if_not_installed("sdmTMB")
  set.seed(20109)
  d <- data.frame(year = factor(rep(2011:2013, each = 60)),
    x = rnorm(180), z = rnorm(180), X = runif(180), Y = runif(180))
  d$delta <- rlnorm(180, .8 + .4 * d$x, .4) * rbinom(180, 1, .7)
  d$w <- rep(c(1, 2), 90)
  mesh <- sdmTMB::make_mesh(d, c("X", "Y"), n_knots = 8)
  original <- sdmTMB::sdmTMB(list(delta ~year + x, delta ~year + x + z),
    data = d, mesh = mesh, family = sdmTMB::delta_lognormal(),
    spatial = "off", spatiotemporal = "off")
  reduced <- .search_refit_one(original, list(~year + x, ~year + x),
    .search_refit_args(original, "sdmTMB")$args,
    .search_locked_data(original, original$formula, "sdmTMB"), "sdmTMB")
  list(data = d, original = original, reduced = reduced, mesh = mesh)
}

test_that("native delta criteria verify observed response and duplicated weights", {
  f <- search_delta_criteria_fixture()
  mode <- f$original$tmb_obj$env$last.par.best; rng <- .Random.seed
  a <- .criterion_sdmTMB_frame(f$original, f$original$formula[[1L]])
  b <- .criterion_sdmTMB_frame(f$reduced, f$reduced$formula[[1L]])
  expect_identical(a$response, "delta"); expect_identical(b$response, "delta")
  expect_equal(as.numeric(a$y), f$data$delta)
  expect_equal(a$weights, rep(1, nrow(f$data)))
  expect_identical(a$weights, b$weights)
  tab <- table_criterion(list(Full = f$original, Reduced = f$reduced),
    criterion = c("aic", "loglik"))
  expect_identical(tab$AIC_group[1L], tab$AIC_group[2L])
  expect_false(anyNA(tab$delta_AIC))
  expect_equal(tab$AIC, c(AIC(f$original), AIC(f$reduced)))
  expect_identical(f$original$tmb_obj$env$last.par.best, mode)
  expect_identical(.Random.seed, rng)
  weighted <- sdmTMB::sdmTMB(f$original$formula, data = f$data, mesh = f$mesh,
    family = sdmTMB::delta_lognormal(), spatial = "off", spatiotemporal = "off",
    weights = f$data$w)
  frame <- .criterion_sdmTMB_frame(weighted, weighted$formula[[1L]])
  expect_equal(frame$weights, f$data$w)
  tab <- table_criterion(list(Unweighted = f$original, Weighted = weighted), criterion = "aic")
  expect_false(identical(tab$AIC_group[1L], tab$AIC_group[2L]))
  expect_true(all(is.na(tab$delta_AIC)))
})

test_that("unverified delta provenance never falls back to raw first column", {
  f <- search_delta_criteria_fixture(); m <- f$original; n <- nrow(m$data)
  bad <- m; bad$tmb_data$weights_i <- rep(1, 2L * n)
  bad$tmb_data$weights_i[n + 1L] <- 2
  expect_null(.criterion_sdmTMB_frame(bad, bad$formula[[1L]]))
  bad_data <- m; bad_data$data$delta <- bad_data$data$delta * 2
  expect_null(.criterion_sdmTMB_frame(bad_data, bad_data$formula[[1L]]))
  bad_length <- m; bad_length$tmb_data$weights_i <- numeric(1)
  expect_null(.criterion_sdmTMB_frame(bad_length, bad_length$formula[[1L]]))
  for (fit in list(bad, bad_data, bad_length)) {
    context <- new.env(parent = emptyenv()); context$notes <- list()
    row <- .criterion_model.sdmTMB(fit, "aic", context, list())
    expect_null(row$frame); expect_true(is.na(row$response)); expect_false(row$eligible)
    tab <- table_criterion(list(Valid = m, Invalid = fit), criterion = "aic")
    expect_true(is.na(tab$AIC_group[2L]))
    expect_match(tab$notes[2L], "could not be verified")
  }
})

test_that("sdmTMB binomial comparisons retain successes, failures, and trials", {
  skip_if_not_installed("sdmTMB")
  set.seed(327)
  d <- data.frame(year = factor(rep(1:3, 50)), x = rnorm(150), z = rnorm(150),
    X = runif(150), Y = runif(150), trials = rep(8L, 150))
  d$success <- rbinom(150, d$trials, plogis(.2 + .4 * d$x))
  mesh <- sdmTMB::make_mesh(d, c("X", "Y"), n_knots = 8)
  m <- sdmTMB::sdmTMB(cbind(success, trials - success) ~year + x + z,
    data = d, mesh = mesh, family = binomial(), spatial = "off", spatiotemporal = "off")
  r <- .search_refit_one(m, ~year + x, list(),
    .search_locked_data(m, m$formula, "sdmTMB"), "sdmTMB")
  frame <- .criterion_sdmTMB_frame(m, m$formula[[1L]])
  expect_true(is.matrix(frame$y))
  expect_equal(as.numeric(frame$y[, 1L]), d$success)
  expect_equal(as.numeric(rowSums(frame$y)), d$trials)
  tab <- table_criterion(list(Full = m, Reduced = r), "aic")
  expect_false(anyNA(tab$delta_AIC))
  expect_identical(tab$AIC_group[1L], tab$AIC_group[2L])
  expect_equal(tab$AIC, c(AIC(m), AIC(r)))
  out <- search_cpue(m, direction = "backward", refit = TRUE)
  expect_true(all(out$candidates$eligible))
  bad <- m; bad$data$trials <- 9
  expect_null(.criterion_sdmTMB_frame(bad, bad$formula[[1L]]))
  changed <- sdmTMB::sdmTMB(cbind(success, trials - success) ~year + x,
    data = transform(d, trials = 9L), mesh = mesh, family = binomial(),
    spatial = "off", spatiotemporal = "off")
  separated <- table_criterion(list(Original = m, Changed_trials = changed), "aic")
  expect_true(all(is.na(separated$delta_AIC)))
  d$proportion <- d$success / d$trials
  proportion <- sdmTMB::sdmTMB(proportion ~year + x + z, data = d, mesh = mesh,
    family = binomial(), weights = d$trials, spatial = "off", spatiotemporal = "off")
  out <- search_cpue(proportion, direction = "backward", refit = TRUE)
  expect_true(all(out$candidates$eligible))
  d$endpoint <- rbinom(nrow(d), 1, plogis(.2 + .4 * d$x))
  endpoint <- sdmTMB::sdmTMB(endpoint ~year + x + z, data = d, mesh = mesh,
    family = binomial(), weights = d$trials, spatial = "off", spatiotemporal = "off")
  expect_true(.search_fit_status(endpoint, "sdmTMB")$eligible)
  frame <- .criterion_sdmTMB_frame(endpoint, endpoint$formula[[1L]])
  expect_equal(as.numeric(frame$y), d$endpoint)
  expect_equal(frame$weights, d$trials)
  expect_true(all(search_cpue(endpoint, direction = "backward", refit = TRUE)$candidates$eligible))
  zero <- d; zero$trials[1:10] <- 0L; zero$success[1:10] <- 0L
  zero_trial <- sdmTMB::sdmTMB(cbind(success, trials - success) ~year + x + z,
    data = zero, mesh = mesh, family = binomial(), spatial = "off", spatiotemporal = "off")
  expect_true(.search_fit_status(zero_trial, "sdmTMB")$eligible)
  frame <- .criterion_sdmTMB_frame(zero_trial, zero_trial$formula[[1L]])
  expect_equal(as.numeric(rowSums(frame$y)), zero$trials)
  expect_true(all(search_cpue(zero_trial, direction = "backward", refit = TRUE)$candidates$eligible))
})
