# Rscript --vanilla tools/n09/run.R pilot /absolute/output/directory
# Rscript --vanilla tools/n09/run.R production /absolute/output/directory
local({
  args <- commandArgs(trailingOnly = TRUE)
  stopifnot(length(args) == 2L, args[1] %in% c("pilot", "production"))
  mode <- args[1]
  output <- args[2]
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  source("tools/n09/study.R", local = TRUE)
  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE)
  settings <- n09_settings()
  ids <- if (mode == "pilot") 1001:1003 else seq_len(settings$nrep)
  packages <- c("influ2", "glmmTMB", "sdmTMB", "TMB", "Matrix", "bayesplot", "ggplot2")
  metadata <- list(mode = mode, settings = settings,
    package_versions = setNames(vapply(packages, function(p) as.character(packageVersion(p)), ""), packages),
    runtime_md5 = tools::md5sum(sort(list.files("R", full.names = TRUE))),
    study_md5 = tools::md5sum(c("tools/n09/study.R", "tools/n09/run.R", "tools/n09/protocol.md")),
    R = R.version.string, source_commit = system2("git", c("rev-parse", "HEAD"), stdout = TRUE))
  config <- file.path(output, "configuration.rds")
  if (file.exists(config)) stopifnot(identical(readRDS(config), metadata)) else saveRDS(metadata, config)
  bands <- n09_bands(settings$n, settings$pit_grid_size, settings$level)
  saveRDS(bands, file.path(output, "bands.rds"))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  metric_names <- c(names(n09_pit_metrics((seq_len(settings$n) - .5) / settings$n, bands)),
    "fitted_spearman", "covariate_spearman", "year_rms", "response_ecdf_outside_fraction", "spatial_score")
  for (backend in c("glmmTMB", "sdmTMB")) {
    design <- n09_design(backend, settings)
    mesh <- if (backend == "sdmTMB") sdmTMB::make_mesh(design, c("X", "Y"),
      n_knots = 25L, seed = settings$design_seed) else NULL
    edges <- n09_neighbours(design)
    example_saved <- FALSE
    scenarios <- if (backend == "glmmTMB") c("full", "omit_x") else c("full", "omit_x", "omit_st")
    schemes <- if (backend == "glmmTMB") c("fitted", "new_effects") else
      c("fitted", "conditional_draw", "new_effects")
    for (id in ids) {
      checkpoint <- file.path(output, sprintf("%s-%04d.rds", backend, id))
      if (file.exists(checkpoint)) {
        example_saved <- example_saved || !is.null(readRDS(checkpoint)$example)
        next
      }
      started <- proc.time()[[3]]
      generated <- n09_capture(n09_generate(backend, id, design, mesh, settings))
      if (nzchar(generated$error)) stop("Data generation failed for ", backend, " ", id, ": ", generated$error)
      d <- generated$value
      rows <- fit_rows <- examples <- list()
      append_row <- function(scenario, conditioning, variant, nsim, seed, captured,
                             metrics = NULL, simulation_seed = NA_integer_) {
        values <- setNames(rep(NA_real_, length(metric_names)), metric_names)
        if (!is.null(metrics)) values[names(metrics)] <- metrics
        rows[[length(rows) + 1L]] <<- data.frame(backend, replicate = id, scenario,
          conditioning, variant, nsim, seed, simulation_seed,
          data_seed = n09_seed(backend, id, "data"), success = !nzchar(captured$error),
          error = captured$error, warnings = captured$warnings, seconds = captured$seconds,
          as.list(values), stringsAsFactors = FALSE)
      }
      set.seed(n09_seed(backend, id, "analytic"))
      lo <- pnbinom(d$response - 1, mu = d$mu, size = settings$phi)
      hi <- pnbinom(d$response, mu = d$mu, size = settings$phi)
      u <- lo + runif(nrow(d)) * (hi - lo)
      # The oracle is continuous randomised PIT; clipping protects only machine endpoints.
      u <- pmin(1 - .Machine$double.eps, pmax(.Machine$double.eps, u))
      append_row("oracle", "analytic_truth", "primary", 0L, n09_seed(backend, id, "analytic"),
        list(error = "", warnings = "", seconds = 0), n09_pit_metrics(u, bands))
      set.seed(n09_seed(backend, id, "oracle"))
      sims <- matrix(rnbinom(nrow(d) * settings$nsim,
        mu = rep(d$mu, settings$nsim), size = settings$phi), nrow(d))
      rownames(sims) <- rownames(d)
      oracle <- n09_capture(influ2::as_influ_residuals(sims, data = d,
        response = "response", year = "year", response_kind = "distribution",
        conditioning = "Known parameters and realised latent effects",
        batch_size = settings$batch_size, seed = n09_seed(backend, id, "analytic"),
        grid_size = settings$grid_size, level = settings$level))
      if (nzchar(oracle$error)) stop(oracle$error)
      append_row("oracle", "finite_truth", "primary", settings$nsim,
        n09_seed(backend, id, "analytic"), oracle, n09_metrics(oracle$value, d, bands, edges),
        simulation_seed = n09_seed(backend, id, "oracle"))
      rm(sims, oracle)
      all_valid <- TRUE
      for (scenario in scenarios) {
        captured <- n09_capture(n09_fit(backend, scenario, d, mesh))
        status <- n09_fit_status(captured, backend)
        fit_rows[[length(fit_rows) + 1L]] <- data.frame(backend, replicate = id, scenario,
          seed = n09_seed(backend, id, "data"), as.list(status),
          warnings = captured$warnings, error = captured$error, seconds = captured$seconds)
        if (!status$valid) {
          all_valid <- FALSE
          next
        }
        fit <- captured$value
        objective <- if (backend == "glmmTMB") fit$obj else fit$tmb_obj
        original <- list(par = objective$env$last.par.best, data = objective$env$data)
        variants <- if (id %in% settings$sensitivity_ids || mode == "pilot")
          c("primary", "second_seed", "more_simulations") else "primary"
        for (scheme in schemes) for (variant in variants) {
          nsim <- if (variant == "more_simulations") settings$high_nsim else settings$nsim
          seed <- n09_seed(backend, id, if (variant == "second_seed") "second" else "primary")
          checked <- n09_capture(influ2::influ_residuals(fit, year = "year",
            nsim = nsim, batch_size = settings$batch_size, grid_size = settings$grid_size,
            conditioning = scheme, seed = seed, level = settings$level))
          stopifnot(identical(original, list(par = objective$env$last.par.best, data = objective$env$data)))
          metrics <- if (!nzchar(checked$error)) n09_metrics(checked$value, d, bands, edges) else NULL
          append_row(scenario, scheme, variant, nsim, seed, checked, metrics)
          if (nzchar(checked$error)) all_valid <- FALSE
          if (!example_saved && variant == "primary" && !nzchar(checked$error)) {
            examples[[paste(scenario, scheme, sep = ":")]] <- checked$value
          }
        }
        rm(fit, objective, captured)
      }
      example <- if (!example_saved && all_valid) list(replicate = id, data = d, checks = examples) else NULL
      if (!is.null(example)) example_saved <- TRUE
      saveRDS(list(metrics = do.call(rbind, rows), fits = do.call(rbind, fit_rows), example = example,
        generation_warnings = generated$warnings), checkpoint, compress = "xz")
      message(sprintf("%s %s %d: %d/%d eligible fits, %d diagnostics, %.1f s", mode, backend, id,
        sum(vapply(fit_rows, function(x) x$valid, logical(1))), length(scenarios), length(rows),
        proc.time()[[3]] - started))
      gc(verbose = FALSE)
    }
  }
  capture.output(sessionInfo(), file = file.path(output, "session-info.txt"))
  message("Study run complete: ", normalizePath(output))
})
