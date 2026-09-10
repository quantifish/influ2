test_that("fitted-model calculations preserve the pre-refactor numerical baseline", {
  baseline <- readRDS(test_path("fixtures", "residual-engine-baseline.rds"))
  expect_identical(baseline$source, "6c3d1c9")
  for (i in seq_len(nrow(baseline$cases))) {
    set.seed(808L)
    rng <- .Random.seed
    x <- run_residual_baseline(baseline$cases$kind[i], baseline$cases$batch_size[i])
    expect_identical(.Random.seed, rng)
    # Allow only cross-platform floating-point roundoff in the frozen R maths.
    expect_equal(x, baseline$results[[i]]$result, tolerance = 1e-12)
    expect_lte(as.numeric(object.size(x)), baseline$results[[i]]$bytes + 1024)
    expect_false(any(c("simulations", "model", "less", "ties") %in% names(x)))
  }
})
