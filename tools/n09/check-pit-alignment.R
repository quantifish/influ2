# Separate follow-up to the frozen N09 study. Run from the repository root:
# Rscript --vanilla tools/n09/check-pit-alignment.R
# No model fitting, file writes, private dependency helpers, or altered N09 data.
pkgload::load_all(".", quiet = TRUE)
stopifnot(requireNamespace("bayesplot", quietly = TRUE))
study <- readRDS("inst/extdata/n09-validation.rds")
checks <- study$examples$sdmTMB$checks[["full:fitted"]]
n <- 480L
K <- 100L
checks$observations <- data.frame(pit = (seq_len(n) - .5) / n)
p <- suppressMessages(plot(checks, type = "pit_ecdf", pit_grid_size = K))
d <- ggplot2::ggplot_build(p)$data
stopifnot(isTRUE(all.equal(d[[1]]$x, (0:K) / K)),
  isTRUE(all.equal(d[[1]]$y, c(0, study$bands$upper))),
  isTRUE(all.equal(d[[2]]$y, c(0, study$bands$lower))))
set.seed(90912)
flags <- replicate(10000L, {
  u <- sort(runif(n))
  original <- findInterval(study$bands$x, u) / n
  aligned <- findInterval(d[[1]]$x, u) / n
  distance <- max(seq_len(n) / n - u, u - (seq_len(n) - 1) / n)
  c(original = any(original < study$bands$lower | original > study$bands$upper),
    corrected = any(aligned < d[[2]]$y | aligned > d[[1]]$y),
    dkw = distance > sqrt(log(2 / .05) / (2 * n)))
})
result <- data.frame(reference = rownames(flags), samples = ncol(flags),
  crossings = rowSums(flags), rate = rowMeans(flags), row.names = NULL)
stopifnot(identical(as.integer(result$crossings), study$band_audit$crossings))
print(attr(p, "pit_reference"))
print(result)
cat("Matched the frozen N09 control without modifying its artefact or study scripts.\n")
cat("This is an independent-uniform plotting control, not fitted-model calibration.\n")
