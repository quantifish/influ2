# Rscript --vanilla tools/implied-validation/run.R pilot /absolute/output
# Rscript --vanilla tools/implied-validation/run.R production /absolute/output
local({
  args <- commandArgs(trailingOnly = TRUE)
  stopifnot(length(args) == 2L, args[1L] %in% c("pilot", "production"))
  mode <- args[1L]
  output <- args[2L]
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  source("tools/implied-validation/study.R", local = TRUE)
  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE)
  settings <- iv_settings()
  packages <- c("influ2", "glmmTMB", "TMB", "Matrix", "ggplot2")
  config <- list(mode = mode, settings = settings, R = R.version.string,
    package_versions = setNames(vapply(packages, function(p) as.character(packageVersion(p)), ""), packages),
    source_commit = system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
    runtime_md5 = tools::md5sum(sort(list.files("R", full.names = TRUE))),
    study_md5 = tools::md5sum(c("tools/implied-validation/study.R",
      "tools/implied-validation/run.R", "tools/implied-validation/protocol.md")))
  config_file <- file.path(output, "configuration.rds")
  if (file.exists(config_file)) stopifnot(identical(readRDS(config_file), config)) else saveRDS(config, config_file)
  designs <- setNames(lapply(c("balanced", "uneven"), iv_design), c("balanced", "uneven"))
  ids <- if (mode == "pilot") 1001:1003 else seq_len(settings$nrep)
  metrics_template <- iv_metrics(iv_oracle(iv_generate(designs$balanced, "balanced", "null", ids[1]), "null"))
  example_saved <- FALSE
  for (id in ids) {
    checkpoint <- file.path(output, sprintf("replicate-%04d.rds", id))
    if (file.exists(checkpoint)) {
      example_saved <- example_saved || !is.null(readRDS(checkpoint)$examples)
      next
    }
    started <- proc.time()[[3L]]
    fits <- metrics <- cells <- examples <- list()
    all_valid <- TRUE
    for (sampling in names(designs)) for (signal in c("null", "trend")) {
      key <- data.frame(replicate = id, sampling, signal, seed = iv_seed(sampling, signal, id))
      d <- iv_generate(designs[[sampling]], sampling, signal, id)
      oracle <- iv_capture(iv_oracle(d, signal))
      fit_run <- iv_capture(iv_fit(d))
      fit_status <- iv_status(fit_run)
      fits[[length(fits) + 1L]] <- data.frame(key, as.list(fit_status),
        seconds = fit_run$seconds, warnings = fit_run$warnings, error = fit_run$error)
      fitted <- if (fit_status$valid) iv_capture(iv_fitted(fit_run$value, d, signal)) else
        list(value = NULL, error = "Fit ineligible; diagnostic not attempted", warnings = "", seconds = 0)
      for (route in c("known_parameters", "fitted_model")) {
        captured <- if (route == "known_parameters") oracle else fitted
        success <- !nzchar(captured$error)
        value <- if (success) {
          if (route == "known_parameters") captured$value else captured$value$cells
        } else NULL
        stats <- if (success) iv_metrics(value) else lapply(metrics_template, function(x) NA_real_)
        metrics[[length(metrics) + 1L]] <- data.frame(key, route, success,
          warnings = captured$warnings, error = captured$error, seconds = captured$seconds, as.list(stats))
        if (success) cells[[length(cells) + 1L]] <- data.frame(key, route, value)
      }
      if (!fit_status$valid || nzchar(oracle$error) || nzchar(fitted$error)) all_valid <- FALSE
      if (!example_saved && !nzchar(fitted$error)) examples[[paste(sampling, signal, sep = ":")]] <-
        list(result = fitted$value$result, truth = fitted$value$cells[, c("level", "group", "injected", "target")])
      rm(fit_run, fitted, oracle, d)
    }
    chosen <- if (!example_saved && all_valid) list(replicate = id, cases = examples) else NULL
    if (!is.null(chosen)) example_saved <- TRUE
    result <- list(fits = do.call(rbind, fits), metrics = do.call(rbind, metrics),
      cells = do.call(rbind, cells), examples = chosen)
    saveRDS(result, checkpoint, compress = "xz")
    message(sprintf("%s %d: %d/4 eligible fits, %d/8 calculations, %.1f s",
      mode, id, sum(result$fits$valid), sum(result$metrics$success), proc.time()[[3L]] - started))
    gc(verbose = FALSE)
  }
  capture.output(sessionInfo(), file = file.path(output, "session-info.txt"))
  message("Run complete: ", normalizePath(output))
})
