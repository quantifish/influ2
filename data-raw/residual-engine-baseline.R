# Run only against the pre-refactor package at source 6c3d1c9.
# This is a frozen regression artefact, not a fixture to regenerate after edits.
stopifnot(as.character(packageVersion("influ2")) == "1.1.0")
stopifnot(!grepl(".resid_summarise", paste(deparse(body(influ2::influ_residuals)),
  collapse = "\n"), fixed = TRUE))
source("tests/testthat/helper-residual-baseline.R")
kinds <- c("distribution", "continuous", "combined", "positive", "bernoulli", "grouped")
cases <- expand.grid(kind = kinds, batch_size = c(1L, 7L, 24L),
  stringsAsFactors = FALSE)
results <- lapply(seq_len(nrow(cases)), function(i) {
  set.seed(808L)
  before <- .Random.seed
  x <- run_residual_baseline(cases$kind[i], cases$batch_size[i])
  stopifnot(identical(before, .Random.seed))
  list(result = x, bytes = as.numeric(object.size(x)))
})
saveRDS(list(source = "6c3d1c9", R = R.version.string, cases = cases,
  results = results), "tests/testthat/fixtures/residual-engine-baseline.rds", compress = "xz")
