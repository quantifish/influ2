test_that("sdmTMB conditioning clones retain native simulation semantics", {
  skip_if_not_installed("sdmTMB")
  d <- as.data.frame(sdmTMB::pcod_2011)
  mesh <- sdmTMB::make_mesh(d, c("X", "Y"), n_knots = 10)
  fit <- sdmTMB::sdmTMB(present ~ depth_scaled, data = d, mesh = mesh,
    time = "year", spatial = "on", spatiotemporal = "iid",
    family = binomial(), silent = TRUE)
  original_data <- fit$tmb_obj$env$data
  original_inputs <- fit$tmb_data
  original_par <- fit$tmb_obj$env$last.par.best

  # sdmTMB 1.1 uses a compiled TMB objective; 1.2 also supports RTMB,
  # whose env$data does not contain the native six simulation controls.
  # Both must reproduce the native fitted/new-effects simulation exactly.
  for (conditioning in c("fitted", "new_effects")) {
    prepared <- .resid_tmb_object(fit, "sdmTMB", conditioning)
    expect_identical(prepared$par, original_par)
    expect_equal(prepared$obj$report(prepared$par)$eta_i,
      fit$tmb_obj$report(original_par)$eta_i, tolerance = 1e-12)
    set.seed(71)
    actual <- as.numeric(prepared$obj$simulate(par = prepared$par,
      complete = FALSE)$y_i[, 1L])
    expected <- as.numeric(stats::simulate(fit, nsim = 1, seed = 71,
      type = "mle-eb", re_form = if (conditioning == "new_effects") NA else NULL,
      silent = TRUE))
    expect_identical(actual, expected)
    expect_identical(fit$tmb_obj$env$data, original_data)
    expect_identical(fit$tmb_data, original_inputs)
    expect_identical(fit$tmb_obj$env$last.par.best, original_par)
  }
})
