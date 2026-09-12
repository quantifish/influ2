# Rscript --vanilla tools/implied-validation/report.R /absolute/run /absolute/new-result.rds
# Read all attempts; no fitting and no overwrite of a frozen result.
local({
  args <- commandArgs(trailingOnly = TRUE)
  stopifnot(length(args) == 2L, !file.exists(args[2L]))
  source("tools/implied-validation/study.R", local = TRUE)
  config <- readRDS(file.path(args[1L], "configuration.rds"))
  stopifnot(config$mode == "production", identical(config$settings, iv_settings()),
    identical(config$study_md5, tools::md5sum(names(config$study_md5))),
    identical(config$runtime_md5, tools::md5sum(names(config$runtime_md5))))
  files <- file.path(args[1L], sprintf("replicate-%04d.rds", seq_len(config$settings$nrep)))
  stopifnot(all(file.exists(files)))
  runs <- lapply(files, readRDS)
  fits <- do.call(rbind, lapply(runs, `[[`, "fits"))
  metrics <- do.call(rbind, lapply(runs, `[[`, "metrics"))
  cells <- do.call(rbind, lapply(runs, `[[`, "cells"))
  examples <- Filter(Negate(is.null), lapply(runs, `[[`, "examples"))
  stopifnot(nrow(fits) == 4L * config$settings$nrep, nrow(metrics) == 8L * config$settings$nrep,
    length(examples) == 1L, !anyDuplicated(fits[c("replicate", "sampling", "signal")]),
    !anyDuplicated(metrics[c("replicate", "sampling", "signal", "route")]),
    !anyDuplicated(cells[c("replicate", "sampling", "signal", "route", "level", "group")]))
  fit_summary <- do.call(rbind, lapply(iv_groups(fits, c("sampling", "signal")), function(d)
    data.frame(d[1, c("sampling", "signal")], attempted = nrow(d), eligible = sum(d$valid),
      errors = sum(nzchar(d$error)), warnings = sum(nzchar(d$warnings)),
      mean_dispersion = mean(d$dispersion[d$valid]), seconds = sum(d$seconds))))
  cell_summary <- do.call(rbind, lapply(iv_groups(cells, c("sampling", "signal", "route", "level", "group")), function(d) {
    ok <- d$status == "ok" & is.finite(d$adjustment)
    average <- function(x) if (length(x)) mean(x) else NA_real_
    data.frame(d[1, c("sampling", "signal", "route", "level", "group", "n", "injected")],
      usable = sum(ok), adjustment = average(d$adjustment[ok]), target = average(d$target[ok]),
      width = average((d$upper_shift - d$lower_shift)[ok]))
  }))
  result <- list(schema_version = 1L, metadata = config,
    report_md5 = tools::md5sum("tools/implied-validation/report.R"), fits = fits,
    metrics = metrics, cells = cells, fit_summary = fit_summary,
    summary = iv_summary(metrics), cell_summary = cell_summary, examples = examples[[1L]])
  saveRDS(result, args[2L], compress = "xz", version = 3)
  for (name in c("fits", "metrics", "fit_summary", "summary", "cell_summary"))
    write.csv(result[[name]], file.path(args[1L], paste0(name, ".csv")), row.names = FALSE)
  print(result$fit_summary, row.names = FALSE)
  print(result$summary[, c("sampling", "signal", "route", "successful", "bias", "rmse",
    "containment", "zero_exclusion", "any_zero_exclusion", "strong_direction", "width")], row.names = FALSE)
  message("Saved ", args[2L], " (", file.info(args[2L])$size, " bytes)")
})
