# Fixed adapter inputs isolate the rank engine from changes in native simulators.
# Native model integrations are tested separately in the residual backend tests.
residual_baseline_case <- function(kind) {
  n <- 30L
  b <- 24L
  d <- data.frame(year = rep(c(2010, 2012, 2013), each = 10),
    fleet = rep(c("A", "B"), 15), response = rep(c(0, 1, 2, 3, 1), 6),
    row.names = paste0("operation-", seq_len(n)))
  sims <- outer(seq_len(n), seq_len(b), function(i, j) (i + j * 3) %% 7)
  backend <- c(distribution = "glm", continuous = "gam", combined = "glmmTMB",
    positive = "sdmTMB", bernoulli = "brms", grouped = "tinyVAST")[[kind]]
  probability <- trials <- NULL
  if (kind == "continuous") {
    d$response <- (d$response - 2) / 3
    sims <- (sims - 3) / 3
  }
  if (kind == "positive") {
    d$response <- d$response + 0.5
    sims <- sims + 0.5
  }
  if (kind %in% c("bernoulli", "grouped")) {
    trials <- if (kind == "bernoulli") rep(1, n) else rep(2:4, 10)
    probability <- rep(c(0.2, 0.5, 0.8), each = 10)
    d$response <- d$response %% (trials + 1)
    sims <- sims %% (trials + 1)
  }
  dimnames(sims) <- list(rownames(d), paste0("draw-", seq_len(b)))
  list(data = d, simulations = sims, adapter = list(backend = backend,
    observed = d$response, data = d, response = "response",
    scheme = "Frozen joint-simulation baseline", structure = "baseline response",
    family = "baseline family", trials = trials, probability = probability,
    prediction_type = if (is.null(probability)) NULL else "Frozen fitted probabilities",
    response_variables = "response",
    response_kind = switch(kind, bernoulli = "bernoulli", grouped = "grouped_binomial",
      combined = "combined", positive = "positive_continuous", "distribution"),
    component = switch(kind, combined = "combined", positive = "positive", "single")))
}

run_residual_baseline <- function(kind, batch_size) {
  case <- residual_baseline_case(kind)
  adapter <- case$adapter
  adapter$simulate <- function(ids) {
    # A native simulator may consume RNG even when these test values are fixed.
    runif(length(ids))
    case$simulations[, ids, drop = FALSE]
  }
  testthat::local_mocked_bindings(
    .resid_adapter = function(...) {
      # Model preparation can consume RNG before rank randomisers (e.g. brms).
      if (kind == "bernoulli") sample.int(100L, 24L)
      adapter
    },
    .resid_year = function(...) list(name = "year", source = "explicit",
      levels = c("2010", "2012", "2013")), .package = "influ2")
  influ2::influ_residuals("frozen adapter", data = case$data, year = "year",
    nsim = 24L, batch_size = batch_size, seed = 601L, grid_size = 25L,
    groups = "fleet", calibration_bins = 3L, calibration_min_n = 4L,
    calibration_groups = if (kind %in% c("bernoulli", "grouped")) c("year", "fleet") else NULL)
}
