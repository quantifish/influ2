test_that("weighted references retain zero-weight exclusions and explicit matrices", {
  d <- data.frame(year = c("B", "A", "B"), w = c(1, 3, 0))
  X <- cbind(intercept = 1, x = c(2, 6, NA))
  w <- influ2:::.resolve_influ_weights(d, "w")
  expect_equal(w, c(1, 3, 0))
  expect_equal(influ2:::.weighted_col_mean(X, w), c(intercept = 1, x = 5))
  expect_error(influ2:::.resolve_influ_weights(d, "absent"), "was not found")
  expect_true(all(is.na(influ2:::.weighted_col_mean(X[FALSE, ], numeric()))))
  expect_error(influ2:::.weighted_col_mean(X, rep(1, 3)), "positive-weight rows")
  expect_error(influ2:::.weighted_col_mean(X, rep(0, 3)), "positive-weight rows")

  reference <- influ2:::.resolve_reference(d, X, d, as.data.frame(X), "w")
  expect_identical(reference$X, X)
  expect_equal(reference$weights, w)
  expect_true(reference$explicit)
  expect_identical(reference$label, "prediction_grid")
  expect_error(influ2:::.resolve_reference(d, X, reference_data = d), "supplied together")
  expect_error(influ2:::.resolve_reference(d, X, reference_X = X), "supplied together")
  expect_error(influ2:::.resolve_reference(d, X, d, X[-1, ]), "does not conform")
  expect_error(influ2:::.resolve_reference(d, X, d, X[, 1, drop = FALSE]), "does not conform")
})

test_that("data recovery and focus ordering have explicit fallback contracts", {
  d <- data.frame(year = c(2002, 2000, 2001), catch = 1:3)
  expect_identical(influ2:::.resolve_influ_data(list(data = d)), d)
  expect_identical(influ2:::.resolve_influ_data(list(frame = d)), d)
  expect_error(influ2:::.resolve_influ_data(list()), "data could not be recovered")
  expect_identical(influ2:::.focus_info(d, "year")$levels, as.character(2000:2002))
  dates <- as.Date(c("2020-02-01", "2020-01-01", "2020-02-01"))
  expect_identical(influ2:::.focus_info(data.frame(year = dates), "year")$levels,
                   c("2020-01-01", "2020-02-01"))
  d$year <- c("B", "A", "B")
  expect_identical(influ2:::.focus_info(d, "year")$levels, c("B", "A"))
  d$year <- factor(d$year, levels = c("A", "B", "unused"))
  expect_identical(influ2:::.focus_info(d, "year")$levels, c("A", "B"))
  expect_error(influ2:::.focus_info(d[FALSE, ], "year"), "no observations")
  expect_error(influ2:::.focus_info(data.frame(year = c(1, NA)), "year"), "missing values")
  expect_error(influ2:::.focus_info(data.frame(year = c(1, Inf)), "year"), "non-finite")
})

test_that("the linear engine validates coefficient and uncertainty dimensions", {
  fixture <- bentley_fixture()
  model <- fixture$model
  X <- stats::model.matrix(model)
  args <- list(backend = "glm", model = model, data = fixture$data,
    response = "catch", focus = "year",
    family_spec = influ2:::.new_family_spec("poisson", "log", "glm"),
    X = X, beta = stats::coef(model), vcov = stats::vcov(model),
    term_columns = influ2:::.glm_term_columns(model, X))
  run <- function(...) {
    supplied <- list(...)
    current <- args
    current[names(supplied)] <- supplied
    do.call(influ2:::.influ_linear_engine, current)
  }
  expect_equal(run(X = as.data.frame(X))$influence, run()$influence)
  expect_error(run(X = X[-1, ]), "do not conform")
  expect_error(run(beta = args$beta[-1]), "do not conform")
  expect_error(run(vcov = diag(2)), "covariance matrix does not conform")
  expect_error(run(uncertainty = "posterior"), "requires coefficient draws")
  expect_error(run(uncertainty = "analytic", vcov = NULL), "requires a coefficient covariance")
  expect_error(run(uncertainty = "simulation", vcov = NULL), "requires a coefficient covariance")
  expect_error(run(uncertainty = "posterior", beta_draws = matrix(1, 3, 2)), "draws do not conform")
  expect_error(run(term_columns = list(absent = integer())), "No model terms")
  expect_equal(run(vcov = NULL)$influence, run(uncertainty = "none")$influence)
  expect_error(influ2:::.save_derived_draws(matrix(1), "disk"), "draws_path.*required")
  expect_error(influ2:::.save_derived_draws(matrix(1), "unknown"), "Unknown retention mode")
})

test_that("singular joint draws preserve dependence and warn on negative eigenvalues", {
  draw <- influ2:::.draw_mvn(20, c(1, 2), matrix(c(1, 2, 2, 4), 2), seed = 482)
  expect_equal(draw[, 2], 2 * draw[, 1], tolerance = 1e-12)
  expect_identical(draw, influ2:::.draw_mvn(20, c(1, 2), matrix(c(1, 2, 2, 4), 2), seed = 482))
  expect_warning(truncated <- influ2:::.draw_mvn(20, c(1, 2), diag(c(1, -0.1)), seed = 482),
                 "negative eigenvalues were truncated")
  expect_equal(truncated[, 2], rep(2, 20))
  expect_true(all(is.finite(truncated)))
  expect_error(influ2:::.index_check_covariance(diag(c(1, -0.1)), 2L), "not positive semi-definite")
  for (bad in list(diag(3), matrix(NA_real_, 2, 2), 1:4)) {
    expect_error(influ2:::.index_check_covariance(bad, 2L), "finite joint parameter covariance")
  }
  asymmetric <- matrix(c(2, 0.2, 0.4, 1), 2)
  expect_equal(influ2:::.index_check_covariance(asymmetric, 2L),
               (asymmetric + t(asymmetric)) / 2)
})

test_that("nominal summaries distinguish missing catch from zero catch", {
  d <- data.frame(year = factor(c("missing", "single", "zero", "zero")),
                  catch = c(NA_real_, 5, 0, 0))
  summary <- influ2:::.nominal_indices(d, "catch", "year",
    influ2:::.focus_info(d, "year"), rep(1, 4))
  expect_true(is.na(summary$estimate[summary$level == "missing"]))
  expect_equal(summary$estimate[summary$level == "single"], 5)
  expect_true(is.na(summary$std_error[summary$level == "single"]))
  expect_equal(summary$estimate[summary$level == "zero"], 0)
  expect_equal(summary$std_error[summary$level == "zero"], 0)
  d$catch <- as.character(d$catch)
  expect_identical(influ2:::.nominal_indices(d, "catch", "year",
    influ2:::.focus_info(d, "year"), rep(1, 4)), data.frame())
})

test_that("prediction boundaries reject malformed native outputs", {
  for (bad in list(c(1, NA), c(1, Inf), "1", matrix(1, 1, 2), array(1, c(1, 1, 2)), 1)) {
    expect_error(influ2:::.index_check_prediction(bad, 2L), "one finite expected response")
  }
  expect_equal(influ2:::.index_check_prediction(matrix(c(1, 2), ncol = 1), 2L), c(1, 2))
})
