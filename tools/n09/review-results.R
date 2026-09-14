# Read-only numerical companion to interpretation-review.md. Run from repo root:
# Rscript --vanilla tools/n09/review-results.R
# No model fitting, response simulation, RNG use, package loading, or file writes.
local({
  paths <- c("inst/extdata/n09-validation.rds", "inst/extdata/implied-validation.rds")
  expected <- c("e47251546d8efa06d977a53b749b7071", "60a5ecb751c8c54e102af1f60daccf79")
  before <- tools::md5sum(paths)
  stopifnot(identical(unname(before), expected))
  n09 <- readRDS(paths[1L])
  iv01 <- readRDS(paths[2L])
  stopifnot(nrow(n09$fits) == 500L, all(n09$fits$valid),
    nrow(n09$metrics) == 1960L, all(n09$metrics$success),
    !any(nzchar(n09$metrics$error)), !any(nzchar(n09$metrics$warnings)),
    nrow(iv01$fits) == 400L, all(iv01$fits$valid),
    nrow(iv01$metrics) == 800L, all(iv01$metrics$success))
  m <- subset(n09$metrics, variant == "primary")
  fields <- c("backend", "scenario", "conditioning")
  measures <- c("residual_sd", "covariate_spearman", "spatial_score",
    "fitted_spearman", "year_rms", "response_ecdf_outside_fraction")
  key <- function(d, columns) do.call(paste, c(d[columns], sep = ":"))
  stopifnot(!anyDuplicated(key(m, c(fields, "replicate"))))
  groups <- split(m, key(m, fields))
  wilson <- function(hits, n) {
    p <- hits / n
    z <- qnorm(.975)
    centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
    radius <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
    c(lower = max(0, centre - radius), upper = min(1, centre + radius))
  }
  mean_finite <- function(v) if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE)
  primary <- do.call(rbind, lapply(groups, function(d) {
    stopifnot(nrow(d) == 100L, setequal(d$replicate, 1:100))
    hits <- sum(d$dkw_crossing)
    limits <- wilson(hits, nrow(d))
    means <- vapply(d[measures], mean_finite, numeric(1))
    s <- n09$summary[key(n09$summary, fields) == key(d[1L, ], fields), ]
    stopifnot(nrow(s) == 1L, s$usable == nrow(d), s$crossings == hits,
      isTRUE(all.equal(unname(unlist(s[c("lower", "upper")])), unname(limits))),
      isTRUE(all.equal(unname(unlist(s[measures])), unname(means))))
    data.frame(d[1L, fields], n = nrow(d), crossings = hits,
      lower_pct = 100 * limits[1L], upper_pct = 100 * limits[2L], as.list(means),
      row.names = NULL)
  }))

  # Exploratory paired contrasts added by this review, not pre-specified N09
  # hypothesis tests. Datasets, not observations or simulation draws, are units.
  comparisons <- data.frame(
    backend = c("glmmTMB", rep("sdmTMB", 4)),
    scenario = c("omit_x", "omit_x", rep("omit_st", 3)),
    conditioning = c("fitted", "fitted", "fitted", "conditional_draw", "new_effects"),
    metric = c(rep("covariate_spearman", 2), rep("spatial_score", 3)))
  paired <- do.call(rbind, lapply(seq_len(nrow(comparisons)), function(i) {
    spec <- comparisons[i, ]
    a <- m[m$backend == spec$backend & m$scenario == spec$scenario &
      m$conditioning == spec$conditioning, ]
    b <- m[m$backend == spec$backend & m$scenario == "full" &
      m$conditioning == spec$conditioning, ]
    p <- merge(a, b, by = "replicate", suffixes = c("_omit", "_full"))
    stopifnot(nrow(p) == 100L, setequal(p$replicate, 1:100))
    delta <- p[[paste0(spec$metric, "_omit")]] - p[[paste0(spec$metric, "_full")]]
    stopifnot(all(is.finite(delta)))
    data.frame(spec, n = length(delta), mean_difference = mean(delta),
      mcse = sd(delta) / sqrt(length(delta)))
  }))

  # Reconcile all paired sensitivity records, without pooling independent units.
  sensitivity <- n09$sensitivity
  for (i in seq_len(nrow(sensitivity))) {
    s <- sensitivity[i, ]
    d <- subset(n09$metrics, backend == s$backend & scenario == s$scenario &
      conditioning == s$conditioning & replicate <= 10L)
    p <- merge(subset(d, variant == "primary"), subset(d, variant == s$variant),
      by = "replicate", suffixes = c("_primary", "_extra"))
    stopifnot(nrow(p) == s$paired, all(p$success_primary & p$success_extra),
      s$flag_changes == sum(p$dkw_crossing_primary != p$dkw_crossing_extra),
      isTRUE(all.equal(s$mean_absolute_distance_change,
        mean(abs(p$dkw_distance_primary - p$dkw_distance_extra)))))
  }

  iv_fields <- c("sampling", "signal", "route")
  iv_measures <- c("containment", "zero_exclusion", "any_zero_exclusion",
    "strong_direction", "strong_exclusion", "target_injection_rmse")
  iv <- do.call(rbind, lapply(split(iv01$metrics, key(iv01$metrics, iv_fields)), function(d) {
    stopifnot(nrow(d) == 100L, setequal(d$replicate, 1:100))
    means <- vapply(d[iv_measures], mean_finite, numeric(1))
    s <- iv01$summary[key(iv01$summary, iv_fields) == key(d[1L, ], iv_fields), ]
    stopifnot(nrow(s) == 1L,
      isTRUE(all.equal(unname(unlist(s[iv_measures])), unname(means))))
    data.frame(d[1L, iv_fields], datasets = nrow(d), as.list(means), row.names = NULL)
  }))
  show <- function(title, data) {
    cat("\n", title, "\n", sep = "")
    print(data, row.names = FALSE, digits = 5)
  }
  show("N09: pooled reference crossings and structural summaries", primary)
  show("Exploratory paired differences: omitted minus full model", paired)
  show("N09: paired seed/count sensitivity", sensitivity[, c(fields, "variant",
    "paired", "flag_changes", "mean_absolute_distance_change")])
  show("IV01: means across independent datasets (proportions, not percentages)", iv)
  show("Frozen original independent-uniform band audit (not rerun)", n09$band_audit)
  core <- c("R/residual-engine.R", "R/residual-conditioning.R", "R/residual-simulation.R")
  stopifnot(identical(unname(tools::md5sum(core)), unname(n09$metadata$runtime_md5[core])),
    identical(before, tools::md5sum(paths)))
  cat("\nFrozen artefacts and rank/conditioning/simulation source hashes agree.\n",
    "Review checks passed; no fitted models or simulations were generated.\n", sep = "")
})
