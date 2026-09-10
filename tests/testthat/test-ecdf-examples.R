test_that("standalone ECDF reuses the overview panel without simulation", {
  skip_if_not_installed("glmmTMB")
  set.seed(312)
  d <- expand.grid(year = factor(2010:2013), month = factor(1:6), repeat_id = 1:8)
  d$x <- rnorm(nrow(d))
  month_effect <- rnorm(6, sd = 0.4)
  d$catch <- rnbinom(nrow(d), mu = exp(0.5 + 0.3 * d$x +
    month_effect[d$month]), size = 2)
  fit <- glmmTMB::glmmTMB(catch ~ year + x + (1 | month),
    family = glmmTMB::nbinom2(), data = d)
  expect_true(fit$sdr$pdHess)
  checks <- influ_residuals(fit, nsim = 40, seed = 22)
  original <- checks
  set.seed(29)
  rng <- .Random.seed
  local_mocked_bindings(.resid_adapter = function(...) stop("No simulation"))
  for (scale in c("identity", "log1p")) {
    standalone <- plot(checks, type = "distribution", response_scale = scale)
    overview <- plot(checks, response_scale = scale)[[4L]]
    expect_identical(standalone$data, overview$data)
    expect_identical(ggplot2::ggplot_build(standalone)$data,
      ggplot2::ggplot_build(overview)$data)
    expect_identical(ggplot2::ggplot_build(ggplot2::autoplot(checks,
      type = "distribution", response_scale = scale))$data,
      ggplot2::ggplot_build(standalone)$data)
    expect_identical(standalone$scales$get_scales("y")$limits, c(0, 1))
  }
  expect_equal(checks$observed_ecdf$probability[checks$observed_ecdf$response == 0],
    mean(d$catch == 0))
  expect_identical(checks, original)
  expect_identical(.Random.seed, rng)
})

test_that("saved brms predictive example is compact, aligned, and renderable", {
  example <- readRDS(system.file("extdata", "brms-residual-example.rds",
    package = "influ2"))
  x <- example$checks
  expect_s3_class(x, "influ_residuals")
  expect_identical(x$metadata$backend, "brms")
  expect_match(x$metadata$scheme, "Posterior predictive")
  expect_equal(x$metadata$nsim, 500)
  expect_equal(nrow(x$observations), 150)
  expect_equal(example$metadata$posterior_draws, 4000)
  expect_lt(example$metadata$max_rhat, 1.01)
  expect_equal(example$metadata$divergences, 0)
  expect_false(any(c("fit", "model", "draws", "yrep", "simulations") %in%
    c(names(example), names(x))))
  expect_lt(as.numeric(object.size(example)), 200000)
  expect_equal(x$observed_ecdf$probability,
    as.numeric(ecdf(x$observations$observed)(x$observed_ecdf$response)))
  expect_true(all(x$ecdf$lower <= x$ecdf$median & x$ecdf$median <= x$ecdf$upper))
  expect_true(all(x$ecdf$lower >= 0 & x$ecdf$upper <= 1))
  expect_true(all(vapply(x$ecdf[c("lower", "median", "upper")],
    function(y) all(diff(y) >= 0), logical(1))))
  expect_equal(length(unique(example$overlay$replicate)), 20)
  expect_equal(length(unique(example$metadata$overlay_draw_ids)), 20)
  for (curve in split(example$overlay, example$overlay$replicate)) {
    expect_equal(curve$response, x$ecdf$response)
    expect_true(all(diff(curve$probability) >= 0))
    expect_true(all(curve$probability >= 0 & curve$probability <= 1))
    expect_equal(curve$probability * 150, round(curve$probability * 150))
  }
  local_mocked_bindings(.resid_adapter = function(...) stop("No simulation"))
  expect_s3_class(plot(x, type = "distribution"), "ggplot")
  expect_identical(ggplot2::ggplot_build(plot(x, type = "distribution"))$data,
    ggplot2::ggplot_build(plot(x)[[4L]])$data)
  expect_error(plot(x, type = "distribution", response_scale = "log1p"),
    "non-negative")
})
