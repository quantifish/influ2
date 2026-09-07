negbin_step_data <- function() {
  set.seed(831)
  data <- data.frame(
    year = factor(rep(2001:2005, each = 100)),
    x = stats::rnorm(500), z = stats::rnorm(500),
    effort = stats::runif(500, 0.8, 2),
    weight = rep(c(1, 2), 250), keep = rep(c(TRUE, TRUE, FALSE, TRUE), 125)
  )
  data$y <- stats::rnbinom(nrow(data), size = 2.5,
    mu = data$effort * exp(1 + 0.1 * as.numeric(data$year) + data$x + 0.5 * data$z))
  data
}

test_that("negative-binomial stages re-estimate theta and reuse the original fit", {
  skip_if_not_installed("MASS")
  data <- negbin_step_data()
  model <- MASS::glm.nb(y ~ year + x + z, data = data)
  # A stored unqualified call must work without attaching MASS.
  model$call[[1L]] <- quote(glm.nb)
  result <- influ_steps(model, year = "year", refit = TRUE, keep_fits = TRUE)
  expect_identical(result$steps$status,
    c("refitted", "refitted", "reused original"))
  expect_identical(result$fits[[3L]], model)
  expect_true(all(vapply(result$fits, inherits, logical(1), "negbin")))
  expect_identical(result$fits[[1L]]$call[[1L]], quote(MASS::glm.nb))
  expect_true(all(vapply(result$fits, function(fit) {
    isTRUE(fit$converged) && is.null(fit$th.warn) && is.finite(fit$theta) && fit$theta > 0
  }, logical(1))))

  manual <- list(
    MASS::glm.nb(y ~ year, data = data),
    MASS::glm.nb(y ~ year + x, data = data)
  )
  for (i in seq_along(manual)) {
    expect_equal(result$fits[[i]]$theta, manual[[i]]$theta, tolerance = 1e-5)
    expect_equal(stats::coef(result$fits[[i]]), stats::coef(manual[[i]]),
      tolerance = 1e-5)
    expect_equal(stats::vcov(result$fits[[i]]), stats::vcov(manual[[i]]),
      tolerance = 1e-5)
    expected <- influ_indices(influ(manual[[i]], focus = "year"))
    expected <- expected[expected$series == "standardised", , drop = FALSE]
    actual <- result$indices[result$indices$step_id == i, names(expected), drop = FALSE]
    rownames(expected) <- rownames(actual) <- NULL
    expect_equal(actual, expected, tolerance = 1e-5)
    expect_gt(abs(result$fits[[i]]$theta / model$theta - 1), 0.1)
  }
  expect_true(all(is.finite(result$indices$lower) & is.finite(result$indices$upper)))
})

test_that("negative-binomial refits retain formula exposure, weights, and locked rows", {
  skip_if_not_installed("MASS")
  data <- negbin_step_data()
  data$x[c(2, 7)] <- NA_real_
  model <- MASS::glm.nb(y ~ year + x + z + offset(log(effort)),
    data = data, weights = weight, subset = keep)
  result <- .step_refit_models(model, "year")
  manual <- MASS::glm.nb(y ~ year + offset(log(effort)),
    data = result$data, weights = weight)

  expect_identical(result$fits[[3L]], model)
  expect_true(all(vapply(result$fits, stats::nobs, numeric(1)) == stats::nobs(model)))
  expect_equal(result$fits[[1L]]$theta, manual$theta, tolerance = 1e-5)
  expect_equal(stats::coef(result$fits[[1L]]), stats::coef(manual), tolerance = 1e-5)
  for (fit in result$fits) {
    frame <- stats::model.frame(fit)
    expect_equal(stats::model.offset(frame), log(result$data$effort))
    expect_equal(stats::model.weights(frame), result$data$weight)
  }
})

test_that("negative-binomial stages reject incomplete dispersion convergence", {
  skip_if_not_installed("MASS")
  data <- negbin_step_data()
  model <- MASS::glm.nb(y ~ year + x + z, data = data)
  failed <- model
  failed$th.warn <- "alternation limit reached"
  expect_error(.step_refit_models(failed, "year", steps = list(Full = formula(failed))),
    "incomplete negative-binomial dispersion convergence")
  for (theta in list(NULL, NA_real_, Inf, 0, -1, c(1, 2))) {
    failed <- model
    failed$theta <- theta
    expect_error(.step_refit_models(failed, "year", steps = list(Full = formula(failed))),
      "finite positive negative-binomial dispersion")
  }
})
