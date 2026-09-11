# Rscript --vanilla tools/n09/report.R /absolute/production/directory
# No model fitting: compile all attempts and the pre-specified example objects.
local({
  args <- commandArgs(trailingOnly = TRUE)
  stopifnot(length(args) == 1L)
  input <- args[1]
  source("tools/n09/study.R", local = TRUE)
  config <- readRDS(file.path(input, "configuration.rds"))
  stopifnot(config$mode == "production", identical(config$settings, n09_settings()),
    identical(config$study_md5, tools::md5sum(names(config$study_md5))),
    identical(config$runtime_md5, tools::md5sum(names(config$runtime_md5))))
  settings <- config$settings
  files <- unlist(lapply(c("glmmTMB", "sdmTMB"), function(b)
    file.path(input, sprintf("%s-%04d.rds", b, seq_len(settings$nrep)))))
  stopifnot(all(file.exists(files)))
  runs <- lapply(files, readRDS)
  metrics <- do.call(rbind, lapply(runs, `[[`, "metrics"))
  fits <- do.call(rbind, lapply(runs, `[[`, "fits"))
  examples <- Filter(Negate(is.null), lapply(runs, `[[`, "example"))
  stopifnot(length(examples) == 2L, nrow(fits) == 5L * settings$nrep,
    !anyDuplicated(metrics[c("backend", "replicate", "scenario", "conditioning", "variant")]),
    !anyDuplicated(fits[c("backend", "replicate", "scenario")]))
  names(examples) <- c("glmmTMB", "sdmTMB")
  generation_warnings <- data.frame(file = basename(files),
    warning = vapply(runs, `[[`, "", "generation_warnings"))
  primary <- metrics[metrics$variant == "primary", ]
  split_groups <- function(data, fields) split(data,
    interaction(data[fields], drop = TRUE, lex.order = TRUE))
  fit_summary <- do.call(rbind, lapply(split_groups(fits, c("backend", "scenario")), function(d) {
    data.frame(d[1, c("backend", "scenario")], attempted = nrow(d), eligible = sum(d$valid),
      warnings = sum(nzchar(d$warnings)), errors = sum(nzchar(d$error)),
      largest_gradient = if (all(is.na(d$max_gradient))) NA_real_ else max(d$max_gradient, na.rm = TRUE),
      seconds = sum(d$seconds))
  }))
  continuous <- c("dkw_distance", "residual_mean", "residual_sd", "qq_outside_fraction",
    "fitted_spearman", "covariate_spearman", "year_rms", "response_ecdf_outside_fraction", "spatial_score")
  summary <- do.call(rbind, lapply(split_groups(primary,
    c("backend", "scenario", "conditioning")), function(d) {
    good <- d[d$success, ]
    hits <- sum(good$dkw_crossing)
    means <- vapply(good[continuous], function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE), 0.0)
    data.frame(d[1, c("backend", "scenario", "conditioning")],
      datasets = settings$nrep, diagnostic_attempts = nrow(d), usable = nrow(good),
      diagnostic_failures = sum(!d$success), warnings = sum(nzchar(d$warnings)),
      crossings = hits, as.list(n09_wilson(hits, nrow(good))),
      displayed_crossings = sum(good$displayed_band_crossing), as.list(means))
  }))
  # Paired sensitivity summaries: do not count these reruns as new datasets.
  sensitivity <- metrics[metrics$variant != "primary", ]
  keys <- c("backend", "replicate", "scenario", "conditioning")
  paired <- merge(sensitivity, primary, by = keys, suffixes = c("", "_primary"), all.x = TRUE)
  stopifnot(nrow(paired) == nrow(sensitivity), !anyNA(paired$variant_primary))
  sensitivity_summary <- do.call(rbind, lapply(split_groups(paired,
    c("backend", "scenario", "conditioning", "variant")), function(d) {
    good <- d[d$success & d$success_primary, ]
    data.frame(d[1, c("backend", "scenario", "conditioning", "variant")],
      datasets = length(settings$sensitivity_ids), paired = nrow(good),
      flag_changes = sum(good$dkw_crossing != good$dkw_crossing_primary),
      mean_absolute_distance_change = mean(abs(good$dkw_distance - good$dkw_distance_primary)),
      maximum_absolute_distance_change = if (nrow(good))
        max(abs(good$dkw_distance - good$dkw_distance_primary)) else NA_real_,
      mean_absolute_sd_change = mean(abs(good$residual_sd - good$residual_sd_primary)))
  }))
  bands <- readRDS(file.path(input, "bands.rds"))
  audit <- n09_band_audit(bands, settings)
  # Public artefact contains no fitted models, response-simulation matrices,
  # private attachments, machine paths, or executable closures.
  result <- list(schema_version = 1L, metadata = config, metrics = metrics, fits = fits,
    fit_summary = fit_summary, summary = summary, sensitivity = sensitivity_summary,
    band_audit = audit, bands = bands, examples = examples,
    generation_warnings = generation_warnings)
  for (name in c("metrics", "fits", "fit_summary", "summary", "sensitivity", "band_audit"))
    write.csv(result[[name]], file.path(input, paste0(name, ".csv")), row.names = FALSE)
  target <- "inst/extdata/n09-validation.rds"
  saveRDS(result, target, compress = "xz", version = 3)
  print(fit_summary, row.names = FALSE)
  print(summary[, c("backend", "scenario", "conditioning", "usable", "crossings", "rate", "lower", "upper",
    "residual_sd", "covariate_spearman", "spatial_score")], row.names = FALSE)
  print(audit, row.names = FALSE)
  message("Saved ", target, " (", file.info(target)$size, " bytes)")
})
