test_that("brms residuals preserve joint draw identities, known trials, and batching", {
  skip_if_not_installed("brms")
  set.seed(395)
  d <- data.frame(year = factor(rep(1:3, each = 12)), x = seq(-1, 1, length.out = 36))
  probability <- plogis(outer(seq(-0.3, 0.3, length.out = 60), d$x, "+"))
  simulations <- matrix(0, 60, nrow(d))
  seen <- list()
  testthat::local_mocked_bindings(
    ndraws = function(object) 60L,
    standata = function(object, ...) list(trials = object$data$n),
    posterior_predict = function(object, draw_ids, sort, cores) {
      expect_false(sort)
      expect_identical(cores, 1L)
      seen[[length(seen) + 1L]] <<- draw_ids
      simulations[draw_ids, , drop = FALSE]
    },
    posterior_epred = function(object, draw_ids, sort, ...) {
      sweep(probability[draw_ids, , drop = FALSE], 2, object$data$n, "*")
    }, .package = "brms")
  for (trials in c(1, 5)) {
    d$n <- trials
    d$y <- rbinom(nrow(d), trials, plogis(d$x))
    simulations <- matrix(rbinom(length(probability), trials, probability), nrow = 60)
    formula <- if (trials == 1) brms::bf(y ~ year + x, family = brms::bernoulli()) else {
      brms::bf(y | trials(n) ~ year + x, family = brms::brmsfamily("binomial"))
    }
    model <- structure(list(formula = formula, family = formula$family,
      data = d, fit = TRUE), class = "brmsfit")
    # Only native posterior extraction is mocked; routing, row alignment,
    # draw selection, compact accumulation, and calibration run end to end.
    set.seed(51)
    ids <- sample.int(60, 20, replace = FALSE)
    randomiser <- runif(nrow(d))
    expected <- t(simulations[ids, , drop = FALSE])
    seen <- list()
    result <- influ_residuals(model, nsim = 20, seed = 51, batch_size = 7)
    expect_identical(unlist(seen), ids)
    expect_equal(result$observations$observed, d$y)
    expect_equal(result$observations$trials, d$n)
    expect_equal(result$observations$probability, colMeans(probability[ids, , drop = FALSE]))
    expect_equal(result$observations$predicted, rowMeans(expected))
    expect_equal(result$observations$pit,
      (rowSums(expected < d$y) + randomiser * (rowSums(expected == d$y) + 1)) / 21)
    expect_match(result$metadata$prediction_type, "Posterior mean")
    expect_identical(result$metadata$response_kind,
      if (trials == 1) "bernoulli" else "grouped_binomial")
    whole <- influ_residuals(model, nsim = 20, seed = 51, batch_size = 20)
    expect_identical(result$observations, whole$observations)
    expect_identical(result$calibration, whole$calibration)
    expect_null(result$draws)
    expect_null(result$model)
    expect_error(influ_residuals(model, nsim = 61), "exceeds available posterior draws")
    model$formula <- brms::bf(y | weights(n) ~ year + x, family = brms::brmsfamily("binomial"))
    expect_error(influ_residuals(model, nsim = 20), "Only brms.*trials")
    model$formula <- brms::bf(log(y) ~ year + x)
    expect_error(influ_residuals(model, nsim = 20), "single named observed response")
  }
})

test_that("brms hurdle residuals distinguish combined catches from encounters", {
  skip_if_not_installed("brms")
  d <- data.frame(year = factor(rep(1:3, each = 12)), y = rep(c(0, 1, 3), 12))
  model <- structure(list(data = d, fit = TRUE,
    formula = brms::bf(y ~ year, family = brms::hurdle_lognormal()),
    family = brms::hurdle_lognormal()), class = "brmsfit")
  simulated <- outer(1:40, 1:36, function(a, b) ifelse((a + b) %% 3 == 0, 0, (a + b) / 10))
  testthat::local_mocked_bindings(
    ndraws = function(object) 40L,
    posterior_predict = function(object, draw_ids, ...) simulated[draw_ids, , drop = FALSE],
    posterior_epred = function(object, draw_ids, dpar, ...) {
      expect_identical(dpar, "hu")
      matrix(1 / 3, length(draw_ids), 36)
    }, .package = "brms")
  combined <- influ_residuals(model, nsim = 20, seed = 41)
  encounter <- influ_residuals(model, nsim = 20, seed = 41, component = "encounter")
  expect_identical(combined$metadata$response_kind, "combined")
  expect_equal(combined$observations$observed, d$y)
  expect_null(combined$calibration)
  expect_identical(encounter$metadata$response_kind, "bernoulli")
  expect_equal(encounter$observations$observed, as.numeric(d$y > 0))
  expect_equal(encounter$observations$probability, rep(2 / 3, nrow(d)))
  expect_error(influ_residuals(model, nsim = 20, component = "positive"), "separate positive fit")
})
