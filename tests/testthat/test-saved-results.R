expect_saved_results_restart <- function(results) {
  skip_if_not_installed("callr")
  skip_if_not_installed("pkgload")
  directory <- tempfile("influ2 saved results ")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  paths <- file.path(directory, paste0(names(results), ".rds"))
  names(paths) <- names(results)
  for (name in names(results)) {
    expect_false(saved_result_has_live_state(results[[name]]), info = name)
    saveRDS(results[[name]], paths[[name]])
  }
  original <- results
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  set.seed(401)
  rng <- .Random.seed
  expected <- lapply(results, saved_result_view)
  expect_identical(results, original)
  expect_identical(.Random.seed, rng)

  package_path <- normalizePath(getNamespaceInfo(asNamespace("influ2"), "path"),
    winslash = "/", mustWork = TRUE)
  restored <- callr::r(function(paths, helper, package_path, development) {
    if (development) {
      pkgload::load_all(package_path, export_all = FALSE, helpers = FALSE,
        attach_testthat = FALSE, quiet = TRUE)
    } else {
      library("influ2", lib.loc = dirname(package_path), character.only = TRUE)
    }
    source(helper, local = TRUE)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    objects <- lapply(paths, readRDS)
    before <- objects
    set.seed(907) # Different from the originating session: plots must not resimulate.
    rng <- .Random.seed
    views <- lapply(objects, saved_result_view)
    list(objects = objects, views = views, pid = Sys.getpid(),
      unchanged = identical(objects, before), rng_unchanged = identical(.Random.seed, rng),
      package_path = normalizePath(getNamespaceInfo(asNamespace("influ2"), "path"),
        winslash = "/", mustWork = TRUE),
      backend_namespaces = intersect(loadedNamespaces(),
        c("brms", "rstan", "glmmTMB", "sdmTMB", "tinyVAST")))
  }, args = list(paths = paths,
    helper = normalizePath(test_path("helper-saved-results.R"), winslash = "/"),
    package_path = package_path, development = pkgload::is_dev_package("influ2")),
    system_profile = FALSE, user_profile = FALSE, package = FALSE, timeout = 60)

  expect_false(identical(restored$pid, Sys.getpid()))
  expect_identical(restored$package_path, package_path)
  expect_length(restored$backend_namespaces, 0L)
  expect_true(restored$unchanged)
  expect_true(restored$rng_unchanged)
  for (name in names(results)) {
    expect_identical(restored$objects[[name]], results[[name]], info = name)
    expect_equal(restored$views[[name]], expected[[name]], tolerance = 1e-12,
      info = name)
  }
}

test_that("all four compact result types survive a fresh R session", {
  fixture <- bentley_fixture()
  full <- fixture$model
  reduced <- stats::update(full, . ~ year + area, data = fixture$data)
  reference <- unique(fixture$data[c("area", "vessel")])
  results <- list(
    influence = influ(full, focus = "year"),
    residuals = influ_residuals(full, nsim = 40, seed = 281),
    index = cpue_index(full, reference_data = reference),
    steps = influ_steps(list(Reduced = reduced, Full = full), year = "year")
  )
  expect_null(results$influence$model)
  expect_null(results$influence$draws)
  expect_null(results$index$draws)
  expect_null(results$steps$fits)
  expect_true(any(results$influence$influence$std_error > 0, na.rm = TRUE))
  expect_true(all(results$index$table$SD > 0))
  expect_true(any(results$steps$indices$std_error > 0, na.rm = TRUE))
  expect_true(any(results$residuals$ecdf$upper > results$residuals$ecdf$lower))
  expect_saved_results_restart(results)
})

test_that("mixed-model results reopen without loading glmmTMB", {
  skip_if_not_installed("glmmTMB")
  fixture <- bentley_fixture()
  full <- glmmTMB::glmmTMB(catch ~ year + area + (1 | vessel),
    family = poisson(), data = fixture$data)
  reduced <- glmmTMB::glmmTMB(catch ~ year + (1 | vessel),
    family = poisson(), data = fixture$data)
  expect_true(full$sdr$pdHess)
  expect_true(reduced$sdr$pdHess)
  expect_identical(full$fit$convergence, 0L)
  expect_identical(reduced$fit$convergence, 0L)
  results <- list(
    influence = influ(full, focus = "year"),
    residuals = influ_residuals(full, nsim = 40, seed = 281),
    index = cpue_index(full, reference_data = unique(fixture$data["area"])),
    steps = influ_steps(list(Reduced = reduced, Full = full), year = "year")
  )
  expect_saved_results_restart(results)
})

test_that("saved Bayesian residual summaries reopen without their fit or brms", {
  example <- readRDS(system.file("extdata", "brms-residual-example.rds",
    package = "influ2"))
  expect_identical(example$checks$metadata$backend, "brms")
  expect_saved_results_restart(list(residuals = example$checks))
})

test_that("Bayesian influence summaries reopen without posterior draws or brms", {
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  fit <- readRDS(system.file("extdata", "brms-fixtures", "fit2.rds", package = "influ2"))
  diagnostic <- influ(fit, focus = "year", ndraws = 40, retain = "summary")
  expect_identical(diagnostic$backend, "brms")
  expect_null(diagnostic$draws)
  expect_null(diagnostic$model)
  expect_true(any(diagnostic$coefficients$relative_upper >
    diagnostic$coefficients$relative_lower, na.rm = TRUE))
  expect_saved_results_restart(list(influence = diagnostic))
})

test_that("the compact-object guard detects hidden live state", {
  expect_false(saved_result_has_live_state(list(x = 1:3, label = "Year")))
  expect_true(saved_result_has_live_state(list(hidden = new.env(parent = emptyenv()))))
  expect_true(saved_result_has_live_state(list(formula = stats::as.formula("y ~ x"))))
  expect_true(saved_result_has_live_state(list(fun = function() NULL)))
  expect_true(saved_result_has_live_state(as.call(list(as.name("identity"),
    new.env(parent = emptyenv())))))
})
