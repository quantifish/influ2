# Run after build-brms-fixtures.R. Uses the full validated posterior, not the
# thinned numerical-test fixture. No MCMC is performed here.
devtools::load_all(quiet = TRUE)
m <- readRDS("data-raw/brms-implied-full/hurdle.rds")
check <- posterior::summarise_draws(posterior::as_draws_array(m), "rhat", "ess_bulk")
stopifnot(max(check$rhat, na.rm = TRUE) < 1.02,
  min(check$ess_bulk, na.rm = TRUE) > 100)
out <- list(
  positive = implied_effects(m, year = "year", groups = "area", component = "positive"),
  encounter = implied_effects(m, year = "year", groups = "area", year_term = "year_scaled", component = "encounter"),
  combined = implied_effects(m, year = "year", groups = "area", component = "combined"),
  one_draw = implied_effects(m, year = "year", groups = "area", component = "positive", draw_id = 17),
  metadata = list(n = nrow(m$data), posterior_draws = posterior::ndraws(m),
    max_rhat = max(check$rhat, na.rm = TRUE), min_bulk_ESS = min(check$ess_bulk, na.rm = TRUE),
    reference = "Posterior-mean parameters; conditional likelihood intervals, not credible intervals",
    brms_version = as.character(packageVersion("brms")))
)
saveRDS(out, "inst/extdata/brms-implied-example.rds", compress = "xz")
write.csv(m$data[c("delta", "year", "area", "x", "effort", "vessel", "year_scaled")],
  "inst/extdata/brms-implied-data.csv", row.names = FALSE)
