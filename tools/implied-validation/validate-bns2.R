# Local-only integration check. Reads BNS fits and frames; never writes there.
# Run from the influ2 repository with the BNS CPUE project path as argument 1.
# Private figures/tables are written under ignored data-raw/, not distributed.
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
project <- normalizePath(args[1L], mustWork = TRUE)
pkgload::load_all(quiet = TRUE)
root <- file.path(project, "data/derived/bns_cpue_duration_s2_2026-09-08/spatial/bns2")
fit_path <- file.path(root, "fits/s2.rds")
prep_path <- file.path(root, "prepared.rds")
frame_root <- file.path(project, "data/derived/bns_cpue_duration_release_2026-09-08/frames")
frame_paths <- file.path(frame_root, paste0("wg2026_bns2_", c("bll", "trawl"), ".rds"))
protected <- c(fit_path, prep_path, frame_paths)
before <- tools::md5sum(protected)
m <- readRDS(fit_path)$fit
d <- m$data
prep <- readRDS(prep_path)$data
key <- function(x) {
  stopifnot(!anyNA(x[c("source_id", "event_key")]),
    !anyDuplicated(x[c("source_id", "event_key")]))
  paste(x$source_id, x$event_key, sep = ":")
}
stopifnot(identical(key(d), key(prep)))
common <- intersect(names(d), names(prep))
stopifnot(all(vapply(common, function(v) identical(d[[v]], prep[[v]]), logical(1))))
original <- do.call(rbind, lapply(frame_paths, function(path) {
  readRDS(path)$event$model_frame[, c("source_id", "event_key", "area", "response")]
}))
index <- match(key(d), key(original))
stopifnot(!anyNA(index), !anyDuplicated(index),
  identical(as.numeric(original$response[index]), as.numeric(d$catch_kg)),
  !anyNA(original$area[index]))
d$area <- factor(original$area[index])
# Keep all original fit columns and row names; never join by event_key alone.
stopifnot(identical(rownames(d), rownames(m$data)), nrow(d) == nrow(m$data))
out <- file.path("data-raw", "bns2-implied-review")
dir.create(out, recursive = TRUE, showWarnings = FALSE, mode = "0700")
saveRDS(d, file.path(out, "aligned-data.rds"))
original_mode <- m$tmb_obj$env$last.par.best
original_parameters <- serialize(m$parlist, NULL)
set.seed(830)
rng <- .Random.seed
native <- influ2:::.implied_sdmtmb_report(m)
positive <- m$data$catch_kg > 0
sigma <- native$phi[2L]
# Original native observation objective includes both components and the
# lognormal mean correction; no random-effect prior terms are added here.
ll <- sum(dbinom(as.numeric(positive), 1, plogis(native$eta_i[, 1]), log = TRUE)) +
  sum(dlnorm(m$data$catch_kg[positive], native$eta_i[positive, 2] - sigma^2 / 2,
    sigma, log = TRUE))
stopifnot(abs(-sum(native$jnll_obs) - ll) < 1e-6)
checks <- list()
for (group in c("area", "target")) {
  results <- list()
  for (component in c("encounter", "positive", "combined")) {
    timing <- system.time(result <- implied_effects(m, data = d, year = "year_factor",
      groups = group, component = component,
      year_term = if (component == "encounter") "year_scaled" else NULL))
    stopifnot(identical(m$tmb_obj$env$last.par.best, original_mode),
      identical(serialize(m$parlist, NULL), original_parameters), identical(.Random.seed, rng))
    results[[component]] <- result
    tab <- result$table
    okay <- tab$status == "ok"
    stopifnot(all(is.finite(tab$estimate[okay])),
      all(tab$lower[okay] < tab$estimate[okay]), all(tab$upper[okay] > tab$estimate[okay]))
    for (j in which(okay)) {
      cell <- tab[j, ]
      i <- d$year_factor == cell$level & d[[group]] == cell$group
      if (component == "positive") {
        i <- i & positive
        expected <- mean(log(d$catch_kg[i]) - native$eta_i[i, 2] + sigma^2 / 2)
        stopifnot(abs(cell$adjustment - expected) < 1e-10,
          abs(cell$std_error - sigma / sqrt(sum(i))) < 1e-10, cell$n == sum(i))
      } else if (component == "encounter") {
        score <- sum(as.numeric(positive[i]) - plogis(native$eta_i[i, 1] + cell$adjustment))
        stopifnot(abs(score) < 1e-6, cell$n == sum(i))
      } else {
        expected <- mean(plogis(native$eta_i[i, 1] + cell$encounter_adjustment) *
          exp(native$eta_i[i, 2] + cell$positive_adjustment))
        stopifnot(abs(cell$estimate - expected) < 1e-7 * max(1, expected))
      }
    }
    stem <- paste(component, group, sep = "-")
    saveRDS(result, file.path(out, paste0(stem, ".rds")))
    write.csv(tab, file.path(out, paste0(stem, ".csv")), row.names = FALSE)
    p <- plot(result, ncol = 3) + ggplot2::theme_bw()
    ggplot2::ggsave(file.path(out, paste0(stem, ".png")), p,
      width = 13, height = max(6, ceiling(length(result$metadata$group_levels) / 3) * 2.5), dpi = 120)
    checks[[stem]] <- data.frame(group = group, component = component,
      n = result$metadata$n, strata = nrow(tab), supported = sum(okay),
      elapsed_seconds = unname(timing["elapsed"]), bytes = as.numeric(object.size(result)))
  }
  # The combined result must use exactly the same separately estimated shifts.
  okay <- results$combined$table$status == "ok"
  stopifnot(isTRUE(all.equal(results$combined$table$encounter_adjustment[okay],
    results$encounter$table$adjustment[okay], tolerance = 1e-10)),
    isTRUE(all.equal(results$combined$table$positive_adjustment[okay],
      results$positive$table$adjustment[okay], tolerance = 1e-10)))
}
stopifnot(identical(before, tools::md5sum(protected)))
write.csv(do.call(rbind, checks), file.path(out, "validation.csv"), row.names = FALSE)
writeLines(c("PASS: all checks", paste("Native observation log-likelihood error:", -sum(native$jnll_obs) - ll),
  "Saved fit, preparation, and source frame hashes unchanged; no BNS files written.",
  paste("sdmTMB", as.character(packageVersion("sdmTMB")))), file.path(out, "validation.txt"))
print(do.call(rbind, checks))
