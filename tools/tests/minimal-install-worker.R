# Called by tools/check-minimal-install.R, never by the installed package.
local({
  args <- commandArgs(trailingOnly = TRUE)
  stopifnot(length(args) == 2L, args[1L] %in% c("preflight", "produce", "check"))
  mode <- args[1L]
  config <- readRDS(args[2L])
  assert_isolated <- function() {
    expected <- normalizePath(c(config$library, .Library), winslash = "/")
    stopifnot(identical(.libPaths(), unique(expected)))
    available <- installed.packages()[, "Package"]
    leaked <- setdiff(available, config$allowed)
    if (length(leaked)) stop("Unexpected packages in minimal library: ", paste(leaked, collapse = ", "))
    for (package in config$forbidden) {
      stopifnot(!length(find.package(package, quiet = TRUE)),
        !requireNamespace(package, quietly = TRUE))
    }
    stopifnot(!any(config$forbidden %in% loadedNamespaces()))
  }
  if (mode != "produce") assert_isolated()
  if (mode == "preflight") {
    message("Minimal library isolated; optional packages unavailable: ",
      paste(config$forbidden, collapse = ", "))
    return(invisible(NULL))
  }
  # Producer uses precisely the staged dependency versions too.
  if (mode == "produce") .libPaths(c(config$library, .libPaths()))
  library("influ2", lib.loc = config$library)
  stopifnot(identical(normalizePath(getNamespaceInfo(asNamespace("influ2"), "path")),
    normalizePath(file.path(config$library, "influ2"))))
  source(file.path(config$source, "tests", "testthat", "helper-saved-results.R"), local = TRUE)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  data <- read.csv(system.file("extdata", "bentley-poisson-data.csv", package = "influ2"),
    colClasses = c(year = "factor", area = "factor", vessel = "factor"))
  make_core_results <- function() {
    full <- glm(catch ~ year + area + vessel, family = poisson(), data = data)
    reduced <- update(full, . ~ year + area, data = data)
    reference <- unique(data[c("area", "vessel")])
    list(influence = influ(full, focus = "year"),
      preview = influ(full, focus = "year", uncertainty = "none"),
      residuals = influ_residuals(full, nsim = 40, seed = 281),
      index = cpue_index(full, reference_data = reference),
      integrated = integrate_index(full, reference, area = seq_len(nrow(reference)),
        area_units = "km^2", response_units = "individuals/km^2"),
      steps = influ_steps(list(Reduced = reduced, Full = full), year = "year"))
  }
  if (mode == "produce") {
    # Native mixed-model fits plus stored Bayesian draws; no MCMC is run.
    stopifnot(requireNamespace("glmmTMB", quietly = TRUE),
      requireNamespace("brms", quietly = TRUE), requireNamespace("posterior", quietly = TRUE))
    full <- glmmTMB::glmmTMB(catch ~ year + area + (1 | vessel), poisson(), data = data)
    reduced <- glmmTMB::glmmTMB(catch ~ year + (1 | vessel), poisson(), data = data)
    stopifnot(full$sdr$pdHess, reduced$sdr$pdHess,
      full$fit$convergence == 0L, reduced$fit$convergence == 0L)
    fit <- readRDS(system.file("extdata", "brms-fixtures", "fit2.rds", package = "influ2"))
    bayesian <- readRDS(system.file("extdata", "brms-residual-example.rds", package = "influ2"))
    results <- c(make_core_results(), list(
      mixed_influence = influ(full, focus = "year"),
      mixed_residuals = influ_residuals(full, nsim = 40, seed = 281),
      mixed_index = cpue_index(full, reference_data = unique(data["area"])),
      mixed_steps = influ_steps(list(Reduced = reduced, Full = full), year = "year"),
      bayesian_influence = influ(fit, focus = "year", ndraws = 40, retain = "summary"),
      bayesian_residuals = bayesian$checks))
    stopifnot(!any(vapply(results, saved_result_has_live_state, logical(1))))
    saveRDS(results, config$results)
    saveRDS(lapply(results, saved_result_view), config$expected)
    message("Saved 12 compact results and their table/plot baselines with optional backends available.")
    return(invisible(NULL))
  }

  results <- readRDS(config$results)
  expected <- readRDS(config$expected)
  before <- results
  set.seed(907)
  rng <- .Random.seed
  views <- lapply(results, saved_result_view)
  stopifnot(identical(results, before), identical(.Random.seed, rng),
    !any(vapply(results, saved_result_has_live_state, logical(1))))
  for (name in names(results)) {
    comparison <- all.equal(views[[name]], expected[[name]], tolerance = 1e-12)
    if (!isTRUE(comparison)) stop("Restored result differs: ", name, ": ", paste(comparison, collapse = "; "))
  }
  message("All 12 saved results reproduce tables, labels, panel layouts, and plot coordinates without their backends.")

  fresh <- make_core_results()
  stopifnot(isTRUE(all.equal(fresh, results[names(fresh)], tolerance = 1e-12)),
    all(fresh$index$table$SD > 0), all(fresh$integrated$table$SD > 0),
    any(fresh$influence$influence$std_error > 0, na.rm = TRUE),
    any(fresh$residuals$ecdf$upper > fresh$residuals$ecdf$lower))
  invisible(lapply(fresh, saved_result_view))
  print(plot_compare(list(Standardised = fresh$index, Same_model = fresh$index)))
  message("Fresh core influence, preview, PIT residuals, standardised and integrated indices, and steps passed.")

  expect_dependency_error <- function(expr, text) {
    error <- tryCatch({ force(expr); NULL }, error = identity)
    if (!inherits(error, "error") || !grepl(text, conditionMessage(error), fixed = TRUE)) {
      stop("Expected a clear optional-dependency error containing: ", text,
        "; received: ", if (inherits(error, "error")) conditionMessage(error) else "no error")
    }
  }
  # Class-only sentinels test early dependency guards, not fitted-model validity.
  for (backend in c("glmmTMB", "sdmTMB", "tinyVAST")) {
    expect_dependency_error(influ(structure(list(), class = backend), focus = "year"),
      paste0("Package '", backend, "' is required for this model."))
  }
  expect_dependency_error(influ(structure(list(), class = "brmsfit"), focus = "year"),
    "Packages 'brms' and 'posterior' are required for this model.")
  for (model_class in c("glmmTMB", "sdmTMB", "tinyVAST", "brmsfit")) {
    model <- structure(list(), class = model_class)
    backend <- if (model_class == "brmsfit") "brms" else model_class
    required_message <- paste0("Package '", backend, "' is required")
    expect_dependency_error(influ_residuals(model, nsim = 20), required_message)
    expect_dependency_error(cpue_index(model, year = "year",
      reference_data = data.frame(area = "A")), required_message)
    expect_dependency_error(integrate_index(model, year = "year",
      reference_data = data.frame(area = "A"), area = 1,
      area_units = "km^2", response_units = "individuals/km^2"), required_message)
  }
  for (type in c("pit_ecdf", "pit_ecdf_diff")) {
    expect_dependency_error(plot(fresh$residuals, type = type),
      "PIT ECDF plots require optional package 'bayesplot'")
  }
  expect_dependency_error(plot(fresh$residuals,
    panels = c("qq", "fitted", "year", "pit_ecdf")),
    "PIT ECDF plots require optional package 'bayesplot'")
  assert_isolated()
  message("All 19 missing-package guards passed; optional packages remained unavailable throughout.")
})
