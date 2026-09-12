# Deterministic developer checks; the 400 production fits are never run here.
pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE)
source("tools/implied-validation/study.R")
library(testthat)

test_that("the fixed designs, seeds, and generator match the protocol", {
  balanced <- iv_design("balanced")
  uneven <- iv_design("uneven")
  expect_equal(nrow(balanced), 648L)
  expect_equal(nrow(uneven), 648L)
  expect_equal(unname(as.matrix(table(uneven$year, uneven$season))), unname(iv_counts("uneven")), ignore_attr = TRUE)
  expect_equal(sum(iv_counts("uneven") == 0), 1)
  expect_equal(sum(iv_counts("uneven") > 0 & iv_counts("uneven") < 10), 5)
  expect_identical(balanced, iv_design("balanced"))
  expect_equal(range(table(balanced$year, balanced$season, balanced$vessel)), c(3, 3))
  keys <- expand.grid(sampling = c("balanced", "uneven"), signal = c("null", "trend"), id = c(1:100, 1001:1003))
  seeds <- mapply(iv_seed, keys$sampling, keys$signal, keys$id)
  expect_false(anyDuplicated(seeds) > 0L)
  for (signal in c("null", "trend")) {
    d <- iv_generate(uneven, "uneven", signal, 1001)
    expect_identical(d, iv_generate(uneven, "uneven", signal, 1001))
    expect_equal(d$mu_true, exp(d$eta_additive + d$injected))
    expect_equal(mean(d$baseline_true), 0, tolerance = 1e-14)
    expect_true(all(d$response >= 0 & d$response == round(d$response)))
    grid <- iv_grid(d, signal)
    expect_equal(d$injected, grid$injected[match(paste(d$year, d$season), paste(grid$level, grid$group))])
    if (signal == "null") expect_true(all(d$injected == 0))
  }
})

test_that("the expected conditional target matches native NB likelihoods", {
  eta <- log(c(2, 4, 6))
  phi <- c(2, 3, 5)
  expect_equal(iv_target(exp(eta + .35), eta, phi), .35, tolerance = 1e-12)
  mu <- c(1.5, 5.5, 9)
  target <- iv_target(mu, eta, phi)
  score <- function(delta) sum(phi * (mu - exp(eta + delta)) / (phi + exp(eta + delta)))
  expect_equal(target, uniroot(score, c(-2, 2), tol = 1e-12)$root, tolerance = 1e-6)
  # Explicit expectation over native NB2 probabilities, with negligible omitted tail.
  values <- 0:qnbinom(1 - 1e-12, mu = max(mu), size = 2)
  prob <- lapply(seq_along(mu), function(i) dnbinom(values, mu = mu[i], size = 4))
  expected <- function(delta) sum(vapply(seq_along(mu), function(i)
    sum(prob[[i]] * dnbinom(values, mu = exp(eta[i] + delta), size = phi[i], log = TRUE)), 0.0))
  native <- optimize(expected, c(-2, 2), maximum = TRUE, tol = 1e-10)$maximum
  expect_equal(target, native, tolerance = 1e-6)
  expect_error(iv_target(0, 0, 4))
})

test_that("known-parameter shifts and profiles match independent native calculations", {
  d <- iv_generate(iv_design("balanced"), "balanced", "trend", 1002)
  result <- iv_oracle(d, "trend")
  for (j in c(1, 7, 18)) {
    cell <- result[j, ]
    i <- d$year == cell$level & d$season == cell$group
    ll <- function(delta) sum(dnbinom(d$response[i], mu = exp(d$eta_additive[i] + delta), size = 4, log = TRUE))
    opt <- optimize(ll, c(-5, 5), maximum = TRUE, tol = 1e-10)
    expect_equal(cell$adjustment, opt$maximum, tolerance = 1e-6)
    expect_equal(2 * (ll(cell$adjustment) - ll(cell$lower_shift)), qchisq(.95, 1), tolerance = 1e-6)
    expect_equal(2 * (ll(cell$adjustment) - ll(cell$upper_shift)), qchisq(.95, 1), tolerance = 1e-6)
  }
  uneven <- iv_generate(iv_design("uneven"), "uneven", "null", 1002)
  z <- iv_oracle(uneven, "null")
  expect_equal(table(z$status), c(empty = 1L, ok = 12L, sparse = 5L), ignore_attr = TRUE)
  uneven$response[] <- 0L
  zeros <- iv_oracle(uneven, "null")
  expect_equal(sum(zeros$status == "boundary_zero"), 12L)
  expect_true(all(zeros$adjustment[zeros$status == "boundary_zero"] == -Inf))
  expect_true(all(is.na(zeros$adjustment[zeros$status %in% c("empty", "sparse")])))
  stats <- iv_metrics(zeros)
  expect_equal(stats$usable, 0)
  expect_equal(stats$boundary, 12)
  expect_true(is.na(stats$containment))
})

test_that("the public diagnostic preserves the fit and independent targets", {
  d <- iv_generate(iv_design("uneven"), "uneven", "trend", 1003)
  run <- iv_capture(iv_fit(d))
  expect_true(iv_status(run)$valid)
  fit <- run$value
  result <- iv_fitted(fit, d, "trend")
  expect_s3_class(result$result, "influ_implied")
  eta <- predict(fit, type = "link", re.form = NULL)
  phi <- predict(fit, type = "disp", re.form = NULL)
  for (j in which(result$cells$status == "ok")) {
    c <- result$cells[j, ]
    i <- d$year == c$level & d$season == c$group
    score <- function(delta) sum(phi[i] * (d$mu_true[i] - exp(eta[i] + delta)) / (phi[i] + exp(eta[i] + delta)))
    expect_equal(c$target, uniroot(score, c(-5, 5), tol = 1e-12)$root, tolerance = 1e-6)
  }
  p <- plot(result$result)
  shown <- p$layers[[1L]]$data
  expect_true(all(shown$status == "ok" & shown$n >= 10))
  for (segment in split(shown, shown$segment))
    if (nrow(segment) > 1L) expect_true(all(diff(segment$position) == 1L))
  fail <- iv_capture(stop("intentional test"))
  expect_false(iv_status(fail)$valid)
  expect_match(fail$error, "intentional test")
  warning <- iv_capture(warning("record this warning"))
  expect_match(warning$warnings, "record this warning")
})

test_that("summaries use datasets, not cells, as Monte Carlo replicates", {
  d <- iv_generate(iv_design("balanced"), "balanced", "null", 1001)
  values <- iv_metrics(iv_oracle(d, "null"))
  one <- data.frame(sampling = "balanced", signal = "null", route = "known_parameters", success = TRUE, as.list(values))
  two <- one
  two$containment <- .5
  x <- rbind(one, two)
  s <- iv_summary(x)
  expect_equal(s$containment, mean(x$containment))
  expect_equal(s$containment_mcse, sd(x$containment) / sqrt(2))
  expect_equal(s$containment_n, 2)
  x$success[2] <- FALSE
  s <- iv_summary(x)
  expect_equal(s$successful, 1)
  expect_equal(s$containment, x$containment[1])
  expect_true(is.na(s$containment_mcse))
})
