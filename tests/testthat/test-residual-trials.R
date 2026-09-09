trial_fixture <- function() {
  data <- expand.grid(year = factor(2018:2020), visit = seq_len(20))
  data$x <- seq(-1, 1, length.out = nrow(data))
  data$trials <- rep(c(4L, 8L, 12L), length.out = nrow(data))
  data$success <- rep(c(1L, 4L, 8L, 2L, 6L, 5L), length.out = nrow(data))
  data$proportion <- data$success / data$trials
  data$encounter <- factor(rep(c("absent", "present", "present", "absent", "present"),
                               length.out = nrow(data)))
  data$case_weight <- rep(2, nrow(data))
  data
}

test_that("binomial factor responses simulate the observed success level", {
  data <- trial_fixture()
  model <- glm(encounter ~ year + x, data = data, family = binomial())
  adapter <- .resid_adapter(model, data = NULL, nsim = 4)
  expect_identical(adapter$observed, as.numeric(data$encounter == "present"))
  expect_equal(adapter$trials, rep(1, nrow(data)))
  expect_identical(adapter$response_kind, "bernoulli")
  set.seed(928)
  expected <- matrix(rbinom(nrow(data) * 4, size = 1, prob = fitted(model)), nrow(data))
  set.seed(928)
  expect_equal(adapter$simulate(1:4), expected)
  weighted <- glm(encounter ~ year + x, data = data, family = binomial(), weights = case_weight)
  expect_error(.resid_adapter(weighted, NULL, 4), "Weighted factor responses")
})

test_that("trial metadata prevents confusing grouped trials and case weights", {
  data <- trial_fixture()
  fit <- glm(cbind(success, trials - success) ~ year + x,
              family = binomial(), data = data)
  adapter <- .resid_adapter(fit, data, 4, trial_counts = "trials")
  expect_equal(adapter$observed, data$success)
  expect_equal(as.numeric(adapter$trials), data$trials)
  expect_identical(adapter$response_kind, "grouped_binomial")
  expect_error(.resid_adapter(fit, data, 4, component = "encounter"),
               "requires one trial per observation")
  expect_error(.resid_adapter(fit, data, 4, component = "positive"),
               "not a positive-catch component")
  expect_error(.resid_adapter(fit, data, 4, component = "combined"),
               "requires a joint delta")
  weighted <- glm(cbind(success, trials - success) ~ year + x,
                  family = binomial(), data = data, weights = case_weight)
  expect_error(.resid_adapter(weighted, data, 4), "Additional binomial case weights")
  data$other_trials <- data$trials + 1
  expect_error(.resid_adapter(fit, data, 4, trial_counts = "other_trials"),
               "must match the fitted binomial trial counts")
  for (trials in list(NA_character_, "absent", 3, c("trials", "success"))) {
    expect_error(.resid_adapter(fit, data, 4, trial_counts = trials),
                 "must name the known trial-count column")
  }
  count <- glm(success ~ year + x, family = poisson(), data = data)
  expect_error(.resid_adapter(count, data, 4, trial_counts = "trials"),
               "interpretation of GLM/GAM/glmmTMB binomial trial weights only")
  expect_error(.resid_adapter(count, data, 4, component = "positive"),
               "requires an explicitly supported positive-response family")
  expect_error(.resid_adapter(count, data, 4, component = "encounter"),
               "not a count or continuous family")
})

test_that("native glmmTMB grouped-binomial simulations retain success counts", {
  skip_if_not_installed("glmmTMB")
  data <- trial_fixture()
  fit <- glmmTMB::glmmTMB(cbind(success, trials - success) ~ year + x,
                          family = binomial(), data = data)
  adapter <- .resid_adapter(fit, data, 5)
  expect_equal(adapter$observed, data$success)
  expect_equal(as.numeric(adapter$trials), data$trials)
  expect_equal(adapter$probability, as.numeric(predict(fit, type = "response")))
  set.seed(329)
  expected <- vapply(simulate(fit, nsim = 5), function(x) as.numeric(x[, 1]),
                     numeric(nrow(data)))
  set.seed(329)
  expect_equal(adapter$simulate(1:5), expected)
  weighted <- glmmTMB::glmmTMB(cbind(success, trials - success) ~ year + x,
                               family = binomial(), data = data, weights = case_weight)
  expect_error(.resid_adapter(weighted, data, 5),
               "case weights are not trial counts for a two-column response")
})

test_that("GAM negative-binomial simulations use the native family generator", {
  skip_if_not_installed("mgcv")
  set.seed(295)
  data <- trial_fixture()
  data$catch <- rnbinom(nrow(data), mu = exp(1 + data$x), size = 2)
  fit <- mgcv::gam(catch ~ year + s(x, k = 4), data = data,
                   family = mgcv::nb(), method = "REML")
  adapter <- .resid_adapter(fit, NULL, 3)
  set.seed(321)
  expected <- vapply(1:3, function(i) {
    fit$family$rd(fitted(fit), wt = rep(1, nrow(data)), scale = fit$sig2)
  }, numeric(nrow(data)))
  set.seed(321)
  expect_equal(adapter$simulate(1:3), expected)
  expect_equal(adapter$observed, data$catch)
  expect_identical(adapter$response_kind, "distribution")
})
