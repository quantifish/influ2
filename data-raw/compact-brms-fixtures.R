# Reproduce the synthetic lobster data and compact the development BRMS
# fixtures used by tests and vignettes.
#
# A brmsfit serialises its Stan dynamic-library state, which is unnecessary for
# influence diagnostics and makes otherwise small fixtures tens of megabytes.
# The package backend recognises `influ2_draws`; `brms::standata()` continues to
# reconstruct design matrices from the retained formula, data, and basis.

simulate_lobsters_per_pot <- function(seed = 20260905L) {
  set.seed(seed)
  cells <- expand.grid(
    year = 2000:2017,
    month = 1:12,
    KEEP.OUT.ATTRS = FALSE
  )
  year_index <- cells$year - min(cells$year)
  seasonal_effort <- 1 + 0.28 * cos(2 * pi * (cells$month - 8) / 12)
  annual_effort <- 1 + 0.22 * sin(2 * pi * year_index / 7)
  cells$n <- stats::rpois(nrow(cells), 27 * seasonal_effort * annual_effort)

  # Deliberate holes and sparse cells make the composition diagnostics useful.
  holes <- (2 * year_index + 3 * cells$month) %% 17 == 0 |
    (cells$year <= 2002 & cells$month %in% c(4, 5)) |
    (cells$year >= 2015 & cells$month == 12)
  cells$n[holes] <- 0L
  row <- rep(seq_len(nrow(cells)), cells$n)
  year <- cells$year[row]
  month <- cells$month[row]
  observation_year <- year - min(cells$year)

  depth_mean <- 16 + 0.75 * observation_year +
    5 * cos(2 * pi * (month - 3) / 12)
  depth <- stats::rgamma(length(row), shape = 9, scale = depth_mean / 9)

  longer_soak <- stats::plogis(
    -0.8 + 0.08 * observation_year +
      0.35 * cos(2 * pi * (month - 7) / 12)
  )
  soak <- 24 + 24 * stats::rbinom(length(row), 1, longer_soak) +
    24 * stats::rbinom(length(row), 1, 0.07)
  soak <- pmax(2, soak + stats::rnorm(length(row), sd = 2.2))

  annual_abundance <- 0.18 * sin(2 * pi * observation_year / 9) -
    0.012 * observation_year
  seasonal_abundance <- 0.25 * cos(2 * pi * (month - 9) / 12)
  eta <- log(1.6) + annual_abundance + seasonal_abundance -
    0.0011 * (depth - 27)^2 + 0.004 * (soak - 24)
  lobsters <- stats::rnbinom(length(row), mu = exp(eta), size = 1.25)

  data.frame(
    lobsters = lobsters,
    year = factor(year, levels = 2000:2017),
    month = factor(sprintf("%02d", month), levels = sprintf("%02d", 1:12)),
    depth = depth,
    soak = soak
  )
}

simulate_hurdle_cpue <- function(seed = 42L, n = 1000L) {
  set.seed(seed)
  year_effect <- data.frame(
    year = 1995:2015,
    value = stats::rnorm(21)
  )
  sampled_year <- year_effect[
    sample(seq_len(nrow(year_effect)), size = n, replace = TRUE),
    ,
    drop = FALSE
  ]
  group <- sample(c("treat", "placebo"), size = n, replace = TRUE)
  group_effect <- ifelse(group == "treat", 0.9, 0)
  y <- (1 - stats::rbinom(n, size = 1, prob = 0.3)) *
    stats::rlnorm(
      n,
      meanlog = 2 + group_effect + sampled_year$value,
      sdlog = 0.2
    )
  data.frame(
    y = y,
    year = factor(sampled_year$year),
    group = group
  )
}

lobsters_per_pot <- simulate_lobsters_per_pot()
save(
  lobsters_per_pot,
  file = "data/lobsters_per_pot.rda",
  compress = "xz",
  version = 2
)

compact_brms_fixture <- function(path, ndraws = 200L, metadata = NULL) {
  model <- readRDS(path)
  draws <- if (!is.null(model$influ2_draws)) {
    model$influ2_draws
  } else {
    posterior::as_draws_matrix(model)
  }
  keep <- unique(round(seq(1, nrow(draws), length.out = min(ndraws, nrow(draws)))))
  model$influ2_draws <- draws[keep, , drop = FALSE]
  if (!is.null(metadata) && is.null(model$influ2_fixture)) {
    model$influ2_fixture <- metadata
  }
  model$fit <- NULL
  model$criteria <- list()
  data_terms <- attr(model$data, "terms")
  if (!is.null(data_terms)) {
    environment(data_terms) <- globalenv()
    attr(model$data, "terms") <- data_terms
  }
  saveRDS(model, path, compress = "xz")
  invisible(path)
}

refit_lobster_brms_fixture <- function(
    data = lobsters_per_pot,
    path = "inst/extdata/brms-fixtures/fit2.rds",
    seed = 20260905L) {
  if (!requireNamespace("brms", quietly = TRUE) ||
      !requireNamespace("posterior", quietly = TRUE)) {
    stop("Packages 'brms' and 'posterior' are required to refit the fixture.")
  }
  fit <- brms::brm(
    formula = stats::as.formula(
      "lobsters ~ year + (1 | month) + s(depth, k = 3)",
      env = globalenv()
    ),
    family = brms::negbinomial(),
    data = data,
    chains = 4,
    cores = 4,
    iter = 3000,
    warmup = 1500,
    seed = seed,
    refresh = 100,
    control = list(adapt_delta = 0.99, max_treedepth = 12)
  )
  environment(fit$formula$formula) <- globalenv()
  sampler <- brms::nuts_params(fit)
  if (any(sampler$Parameter == "divergent__" & sampler$Value > 0)) {
    stop("The lobster fixture fit contains divergent transitions.")
  }
  diagnostics <- posterior::summarise_draws(
    posterior::as_draws_array(fit),
    "rhat", "ess_bulk", "ess_tail"
  )
  if (any(diagnostics$rhat > 1.01, na.rm = TRUE) ||
      any(diagnostics$ess_bulk < 400, na.rm = TRUE) ||
      any(diagnostics$ess_tail < 400, na.rm = TRUE)) {
    stop(
      sprintf(
        paste(
          "The lobster fixture fit did not pass its gates:",
          "max R-hat %.3f, min bulk ESS %.0f, and min tail ESS %.0f."
        ),
        max(diagnostics$rhat, na.rm = TRUE),
        min(diagnostics$ess_bulk, na.rm = TRUE),
        min(diagnostics$ess_tail, na.rm = TRUE)
      )
    )
  }
  fit$influ2_fixture <- list(
    provenance = "Fully synthetic lobsters_per_pot data",
    seed = seed,
    chains = 4L,
    post_warmup_draws = 6000L,
    max_rhat = max(diagnostics$rhat, na.rm = TRUE),
    min_ess_bulk = min(diagnostics$ess_bulk, na.rm = TRUE),
    min_ess_tail = min(diagnostics$ess_tail, na.rm = TRUE)
  )
  saveRDS(fit, path)
  compact_brms_fixture(path)
}

refit_hurdle_brms_fixture <- function(
    data = simulate_hurdle_cpue(),
    path = "inst/extdata/brms-fixtures/m1.rds",
    seed = 42L) {
  if (!requireNamespace("brms", quietly = TRUE) ||
      !requireNamespace("posterior", quietly = TRUE)) {
    stop("Packages 'brms' and 'posterior' are required to refit the fixture.")
  }
  fit <- brms::brm(
    formula = brms::bf(y ~ year + group, hu ~ 1),
    family = brms::hurdle_lognormal(),
    data = data,
    chains = 4,
    cores = 4,
    iter = 2000,
    warmup = 1000,
    seed = seed,
    refresh = 100,
    control = list(adapt_delta = 0.99, max_treedepth = 12)
  )
  environment(fit$formula$formula) <- globalenv()
  environment(fit$formula$pforms$hu) <- globalenv()
  sampler <- brms::nuts_params(fit)
  if (any(sampler$Parameter == "divergent__" & sampler$Value > 0)) {
    stop("The hurdle fixture fit contains divergent transitions.")
  }
  diagnostics <- posterior::summarise_draws(
    posterior::as_draws_array(fit),
    "rhat", "ess_bulk", "ess_tail"
  )
  if (any(diagnostics$rhat > 1.01, na.rm = TRUE) ||
      any(diagnostics$ess_bulk < 400, na.rm = TRUE) ||
      any(diagnostics$ess_tail < 400, na.rm = TRUE)) {
    stop(
      sprintf(
        paste(
          "The hurdle fixture fit did not pass its gates:",
          "max R-hat %.3f, min bulk ESS %.0f, and min tail ESS %.0f."
        ),
        max(diagnostics$rhat, na.rm = TRUE),
        min(diagnostics$ess_bulk, na.rm = TRUE),
        min(diagnostics$ess_tail, na.rm = TRUE)
      )
    )
  }
  fit$influ2_fixture <- list(
    provenance = paste(
      "Fully synthetic hurdle CPUE data generated by",
      "simulate_hurdle_cpue()"
    ),
    seed = seed,
    chains = 4L,
    post_warmup_draws = 4000L,
    max_rhat = max(diagnostics$rhat, na.rm = TRUE),
    min_ess_bulk = min(diagnostics$ess_bulk, na.rm = TRUE),
    min_ess_tail = min(diagnostics$ess_tail, na.rm = TRUE)
  )
  saveRDS(fit, path)
  compact_brms_fixture(path)
}

if (identical(tolower(Sys.getenv("INFLU2_REFIT_BRMS_FIXTURE")), "true")) {
  refit_lobster_brms_fixture()
} else {
  compact_brms_fixture("inst/extdata/brms-fixtures/fit2.rds")
}
if (identical(tolower(Sys.getenv("INFLU2_REFIT_HURDLE_FIXTURE")), "true")) {
  refit_hurdle_brms_fixture()
} else {
  compact_brms_fixture(
    "inst/extdata/brms-fixtures/m1.rds",
    metadata = list(
      provenance = paste(
        "Fully synthetic hurdle CPUE data generated by",
        "simulate_hurdle_cpue()"
      ),
      seed = 42L
    )
  )
}
