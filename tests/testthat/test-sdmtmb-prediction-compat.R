test_that("sdmTMB prediction objectives preserve the fitted backend and joint mode", {
  skip_if_not_installed("sdmTMB")
  set.seed(943)
  n <- 240L
  d <- data.frame(year = factor(rep(1:4, each = 60L)),
    group = factor(rep(letters[1:12], 20L)), x = stats::runif(n, -1, 1))
  effect <- stats::rnorm(12L, sd = 0.5)
  d$y <- stats::rpois(n, exp(0.7 + 0.3 * d$x +
    0.1 * as.integer(d$year) + effect[d$group]))
  backends <- if (exists("make_sdmTMB_adfun", asNamespace("sdmTMB"),
    inherits = FALSE)) c("rtmb", "tmb") else "tmb"
  for (backend in backends) {
    control <- if (length(backends) > 1L) {
      sdmTMB::sdmTMBcontrol(backend = backend)
    } else sdmTMB::sdmTMBcontrol()
    fit <- sdmTMB::sdmTMB(y ~ year + x + (1 | group), data = d,
      family = stats::poisson(), spatial = "off", control = control,
      silent = TRUE)
    grid <- d[c(2L, 67L, 128L, 192L), , drop = FALSE]
    expected <- stats::predict(fit, newdata = grid, return_tmb_report = TRUE)
    state <- lapply(c("last.par", "last.par.best", "last.par1", "last.par2",
      "value.best"), function(name) fit$tmb_obj$env[[name]])
    mode <- fit$last.par.best
    rng <- .Random.seed
    expect_no_warning(prediction <- .sdmTMB_prediction_object(fit, grid))
    expect_identical(prediction$obj$env$last.par.best, mode)
    expect_identical(prediction$obj$env$lrandom(), fit$tmb_obj$env$lrandom())
    expect_identical(prediction$obj$env$DLL, fit$tmb_obj$env$DLL)
    actual <- prediction$obj$report(mode)
    expect_equal(actual$proj_eta, expected$proj_eta, tolerance = 1e-8)
    expect_equal(actual$proj_rf, expected$proj_rf, tolerance = 1e-8)
    expect_equal(nrow(actual$proj_eta), nrow(grid))
    # Reporting an alternate full joint vector must not optimise or update
    # either the original fit or its fitted parameter list.
    draw <- mode
    draw[1L] <- draw[1L] + 0.03
    expect_true(all(is.finite(prediction$obj$report(draw)$proj_eta)))
    expect_identical(lapply(c("last.par", "last.par.best", "last.par1",
      "last.par2", "value.best"), function(name) fit$tmb_obj$env[[name]]), state)
    expect_identical(fit$last.par.best, mode)
    expect_identical(.Random.seed, rng)

    if (backend == "rtmb") {
      saved <- fit
      saved$backend <- NULL
      expect_identical(.sdmTMB_prediction_object(saved, grid)$obj$env$DLL,
        fit$tmb_obj$env$DLL)
    }
    # Explicit TMB freeing followed by Rcpp's RTMB finaliser would free the
    # same tape twice. Repeated creation, cleanup, and collection must remain
    # safe for both backends, without invalidating the original fitted tape.
    for (iteration in seq_len(3L)) {
      temporary <- .sdmTMB_prediction_object(fit, grid)
      expect_no_error(.index_free_tmb(temporary$obj))
      rm(temporary)
      invisible(gc())
      expect_true(all(is.finite(fit$tmb_obj$report(mode)$eta_i)))
    }

    changed <- fit
    changed$last.par.best[which(fit$tmb_obj$env$lfixed())[1L]] <-
      changed$last.par.best[which(fit$tmb_obj$env$lfixed())[1L]] + 0.5
    expect_error(.sdmTMB_prediction_object(changed, grid),
      "do not match the fitted joint mode")
    changed <- fit
    changed$tmb_random <- character()
    expect_error(.sdmTMB_prediction_object(changed, grid),
      "do not match the fitted joint mode")
  }
})
