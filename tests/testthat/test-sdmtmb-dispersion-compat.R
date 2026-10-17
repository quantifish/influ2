test_that("native component dispersion accepts both delta layouts", {
  expect_equal(.sdmTMB_component_dispersion(c(1, .6), 2L), c(1, .6))
  expect_equal(.sdmTMB_component_dispersion(.6, 2L), c(1, .6))
  expect_equal(.sdmTMB_component_dispersion(log(.6), 2L, log = TRUE), c(0, log(.6)))
  expect_equal(.sdmTMB_component_dispersion(.6, 1L), .6)
})

test_that("observation dispersion preserves native row and component order", {
  expect_equal(.sdmTMB_observation_dispersion(list(phi = c(1, .6)), 3L, 2L),
    cbind(rep(1, 3), rep(.6, 3)))
  expect_equal(.sdmTMB_observation_dispersion(list(phi = .6), 3L, 2L),
    cbind(rep(1, 3), rep(.6, 3)))
  phi <- c(.4, .6, .8)
  expect_equal(.sdmTMB_observation_dispersion(list(phi_i = phi), 3L, 1L),
    matrix(phi, 3L, 1L))
  expect_equal(.sdmTMB_observation_dispersion(list(phi_i = phi), 3L, 2L),
    matrix(c(rep(1, 3), phi), 3L, 2L))
  expect_error(.sdmTMB_observation_dispersion(list(phi_i = phi[1:2]), 3L, 1L),
    "align with fitted rows")
  expect_error(.sdmTMB_observation_dispersion(list(phi = NA_real_), 3L, 1L),
    "finite and positive")
  expect_error(.sdmTMB_observation_dispersion(list(phi = c(1, 2)), 3L, 1L),
    "component-aligned")
})

test_that("sdmTMB dispersion regressions retain native Gamma likelihoods", {
  skip_if_not_installed("sdmTMB", "1.2.0")
  set.seed(912)
  d <- expand.grid(year = factor(1:3), area = factor(c("A", "B")), id = 1:30)
  d$x <- runif(nrow(d), -1, 1)
  mean <- exp(1 + .2 * as.integer(d$year) + .3 * d$x)
  shape <- exp(1 + .5 * d$x)
  d$response <- rgamma(nrow(d), shape = shape, rate = shape / mean)
  for (backend in c("rtmb", "tmb")) {
    m <- sdmTMB::sdmTMB(response ~ year + area + x, data = d,
      family = Gamma(link = "log"), dispformula = ~ x, spatial = "off", silent = TRUE,
      control = sdmTMB::sdmTMBcontrol(backend = backend))
    expect_true(m$sd_report$pdHess)
    a <- .implied_sdmtmb_adapter(m, NULL, "year", "area", "year_group", NULL)
    report <- m$tmb_obj$report(m$last.par.best)
    phi <- as.numeric(report$phi_i)
    expect_equal(a$dispersion, 1 / phi)
    native <- function(delta) sum(dgamma(d$response, shape = phi,
      rate = phi / exp(report$eta_i[, 1L] + delta), log = TRUE))
    expect_equal(.implied_loglik(0, a$observed, a$eta, a$dispersion, "Gamma"),
      native(0), tolerance = 1e-8)
    expect_equal(.implied_loglik(.2, a$observed, a$eta, a$dispersion, "Gamma"),
      native(.2), tolerance = 1e-8)
  }
})
