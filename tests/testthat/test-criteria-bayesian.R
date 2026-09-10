criteria_brms_fixture <- function(shift = 0, id = 1L) {
  set.seed(324)
  d <- data.frame(y = rnorm(150), x = rnorm(150))
  log_lik <- vapply(d$y, function(y) dnorm(y, rnorm(500, shift, 0.03), 1, log = TRUE), numeric(500))
  structure(list(id = id, formula = brms::bf(y ~ x), data = d,
    family = brms::brmsfamily("gaussian"), criteria = list(loo = loo::loo(log_lik))), class = "brmsfit")
}

test_that("brms criteria retain native LOO values and paired uncertainty", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")
  a <- criteria_brms_fixture()
  b <- criteria_brms_fixture(0.2, 2L)
  native <- loo::loo_compare(list(A = a$criteria$loo, B = b$criteria$loo))
  native_names <- if ("model" %in% colnames(native)) native[["model"]] else rownames(native)
  native_order <- match(c("A", "B"), native_names)
  testthat::local_mocked_bindings(
    loo = function(...) stop("Cached LOO must be reused"),
    bayes_R2 = function(...) matrix(c(0.6, 0.04), 1, dimnames = list(NULL, c("Estimate", "Est.Error"))),
    .package = "brms")
  result <- table_criterion(list(A = a, B = b))
  expect_identical(result$Model, c("A", "B"))
  expect_equal(result$looic, c(a$criteria$loo$estimates["looic", "Estimate"], b$criteria$loo$estimates["looic", "Estimate"]))
  expect_equal(result$elpd_diff, unname(native[native_order, "elpd_diff"]))
  expect_equal(result$se_diff, unname(native[native_order, "se_diff"]))
  expect_equal(result$delta_looic, -2 * result$elpd_diff)
  expect_equal(result$pareto_k_bad, c(0, 0))
  expect_equal(result$bayes_R2, c(0.6, 0.6))
  expect_equal(result$se_bayes_R2, c(0.04, 0.04))
  expect_true(all(is.na(result$df)))
  expect_equal(result$nobs, c(150, 150))
  expect_identical(table_criterion(list(B = b, A = a), sort = TRUE)$Model, native_names)
  reversed <- table_criterion(list(B = b, A = a))
  expect_equal(reversed$elpd_diff, rev(result$elpd_diff))
  expect_equal(reversed$se_diff, rev(result$se_diff))
  expect_null(attr(result, "loo"))
  expect_false(any(vapply(result, is.list, logical(1))))
  changed <- b; changed$data$y[1] <- changed$data$y[1] + 1
  expect_true(all(is.na(table_criterion(list(a, changed))$elpd_diff)))
  changed <- b; changed$criteria$loo$diagnostics$pareto_k[1] <- 1.1
  x <- table_criterion(list(a, changed))
  expect_true(is.finite(x$looic[2]))
  expect_true(is.na(x$looic_group[2]))
  expect_equal(x$pareto_k_bad[2], 1)
  expect_match(x$notes[2], "Unreliable Pareto-k")
  changed <- b; changed$criteria$loo$diagnostics$pareto_k <- NULL
  expect_match(table_criterion(changed)$notes, "diagnostics unavailable")
  changed <- b; changed$criteria$loo$pointwise <- changed$criteria$loo$pointwise[-1, ]
  x <- table_criterion(changed)
  expect_match(x$notes, "rows do not match")
  expect_true(is.na(x$looic_group))
})

test_that("mixed Bayesian and frequentist tables keep distinct criteria", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")
  b <- criteria_brms_fixture()
  g <- glm(y ~ x, data = b$data, family = gaussian())
  testthat::local_mocked_bindings(
    bayes_R2 = function(...) matrix(c(0.6, 0.04), 1, dimnames = list(NULL, c("Estimate", "Est.Error"))),
    loo_R2 = function(...) matrix(c(0.5, 0.05), 1, dimnames = list(NULL, c("Estimate", "Est.Error"))),
    log_lik = function(...) matrix(-2, 40, 150), .package = "brms")
  x <- table_criterion(list(Bayes = b, GLM = g), c("auto", "AIC", "BIC", "cAIC", "logLik", "deviance", "loo_R2", "log_lik"))
  expect_true(is.na(x$AIC[1]))
  expect_equal(x$AIC[2], AIC(g))
  expect_true(is.na(x$looic[2]))
  expect_true(is.finite(x$looic[1]))
  expect_equal(x$loo_R2[1], 0.5)
  expect_equal(x$log_lik[1], -300)
  expect_equal(x$se_log_lik[1], 0)
  expect_match(x$notes[1], "Not an ML fit")
  expect_match(x$notes[2], "not applicable")
  expect_warning(table_criterion(list(b, g), sort = TRUE), "not sorted")
  # Response additions are reported without pretending to align trials or subsets.
  b$formula <- brms::bf(y | weights(x) ~ 1)
  x <- table_criterion(b)
  expect_true(is.na(x$looic_group))
  expect_match(x$notes, "Response additions")
})

test_that("native Bayesian warnings and failures remain visible without refits", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")
  b <- criteria_brms_fixture()
  b$criteria <- list()
  testthat::local_mocked_bindings(
    loo = function(...) stop("native LOO calculation failed"),
    bayes_R2 = function(...) {
      warning("native R-squared caveat")
      matrix(c(0.6, 0.04), 1, dimnames = list(NULL, c("Estimate", "Est.Error")))
    }, .package = "brms")
  x <- table_criterion(b)
  expect_true(is.na(x$looic))
  expect_equal(x$bayes_R2, 0.6)
  expect_match(x$notes, "native LOO calculation failed")
  expect_match(x$notes, "native R-squared caveat")
  expect_setequal(attr(x, "criteria_notes")$status, c("error", "warning"))
  expect_error(table_criterion(b, reloo = TRUE), "refitting")
  expect_error(table_criterion(b, relo = TRUE), "refitting")
  expect_error(table_criterion(b, moment_match = TRUE), "refitting")
  expect_error(table_criterion(b, resp = "y"), "response/rows")
  expect_error(table_criterion(b, summary = FALSE), "response/rows")
})

test_that("old matrix-format LOO results retain non-contiguous model identities", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")
  a <- criteria_brms_fixture()
  b <- criteria_brms_fixture(0.2, 2L)
  g <- glm(y ~ x, data = a$data)
  native_compare <- loo::loo_compare
  testthat::local_mocked_bindings(loo_compare = function(x, ...) {
    result <- native_compare(x, ...)
    if ("model" %in% colnames(result)) {
      labels <- result$model
      result <- as.matrix(result[setdiff(names(result), "model")])
      rownames(result) <- labels
    }
    result
  }, .package = "loo")
  reference <- table_criterion(list(A = a, B = b), criterion = "loo")
  result <- table_criterion(list(B = b, GLM = g, A = a), criterion = "loo")
  expect_equal(result$elpd_diff[c(3, 1)], reference$elpd_diff)
  expect_equal(result$se_diff[c(3, 1)], reference$se_diff)
  expect_true(is.na(result$elpd_diff[2]))
  testthat::local_mocked_bindings(loo_compare = function(...) {
    matrix(0, 2, 2, dimnames = list(c("8", "9"), c("elpd_diff", "se_diff")))
  }, .package = "loo")
  expect_error(table_criterion(list(a, b), "loo"), "identities could not be aligned")
})

test_that("multiresponse R-squared is never silently reduced to its first row", {
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")
  b <- criteria_brms_fixture()
  testthat::local_mocked_bindings(
    bayes_R2 = function(...) matrix(c(0.6, 0.4, 0.04, 0.02), 2,
      dimnames = list(NULL, c("Estimate", "Est.Error"))), .package = "brms")
  x <- table_criterion(b, "bayes_R2")
  expect_true(is.na(x$bayes_R2))
  expect_match(x$notes, "Multiple response-specific")
})
