# Run each storage mode in a separate process to compare peak RSS fairly:
# /usr/bin/time -l Rscript tools/benchmark-dharma-storage.R disk output-disk.rds
# /usr/bin/time -l Rscript tools/benchmark-dharma-storage.R memory output-memory.rds
# This maintainer benchmark is not part of CRAN tests or vignette execution.
local({
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 2L || !args[1L] %in% c("disk", "memory")) {
    stop("Supply storage ('disk' or 'memory') and a result .rds path.")
  }
  devtools::load_all(quiet = TRUE)
  set.seed(71014)
  n <- 25000L
  data <- data.frame(year = factor(rep(2001:2005, length.out = n)), x = runif(n))
  data$cpue <- rpois(n, exp(0.5 + 0.05 * as.integer(data$year) + 0.2 * data$x))
  fit <- glm(cpue ~ year + x, poisson(), data = data)
  gc()
  elapsed <- system.time(result <- influ2::influ_residuals(fit,
    nsim = 400L, batch_size = 10L, seed = 71014L, grid_size = 51L,
    residual_method = "dharma", residual_tests = "standard",
    dharma_storage = args[1L], dharma_max_mb = if (args[1L] == "disk") 8 else 256))["elapsed"]
  summary <- data.frame(storage = args[1L], nobs = n, nsim = 400L,
    elapsed_seconds = unname(elapsed),
    potential_full_matrix_mib = result$metadata$dharma_matrix_mb,
    working_response_estimate_mib = result$metadata$dharma_working_mb %||% NA_real_,
    saved_object_mib = as.numeric(object.size(result)) / 1024^2)
  # The saved object contains compact summaries, not the fitted model or cache.
  dir.create(dirname(args[2L]), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(summary = summary, result = result), args[2L])
  print(summary)
})
