# Developer checks: Rscript --vanilla tools/n09/test-study.R
source("tools/n09/study.R")
library(testthat)
grDevices::pdf(NULL)

test_that("seed purposes, replicates, and backends have distinct seeds", {
  grid <- expand.grid(backend = c("glmmTMB", "sdmTMB"), id = c(1:100, 1001:1003),
    purpose = c("data", "analytic", "oracle", "primary", "second"), stringsAsFactors = FALSE)
  seeds <- mapply(n09_seed, grid$backend, grid$id, grid$purpose)
  expect_false(anyDuplicated(seeds) > 0L)
  expect_true(all(seeds > 0 & seeds < .Machine$integer.max))
})

test_that("data designs and graph preserve year and row alignment", {
  a <- n09_design("glmmTMB")
  b <- n09_design("sdmTMB")
  expect_equal(nrow(a), 480L)
  expect_equal(nrow(b), 480L)
  expect_equal(as.integer(table(a$year)), rep(80L, 6))
  expect_identical(n09_design("sdmTMB"), b)
  edges <- n09_neighbours(b)
  expect_true(all(b$year[edges[, 1]] == b$year[edges[, 2]]))
  expect_true(all(edges[, 1] != edges[, 2]))
  key <- paste(edges[, 1], edges[, 2])
  expect_true(all(paste(edges[, 2], edges[, 1]) %in% key))
  expect_null(n09_neighbours(a))
})

test_that("distribution metrics use exact two-sided ECDF distances", {
  u <- c(.11, .24, .38, .71, .92)
  bands <- data.frame(x = seq(0, 1, length.out = 20), lower = 0, upper = 1)
  got <- n09_pit_metrics(u, bands)
  manual <- max(abs(ecdf(u)(u) - u), abs((seq_along(u) - 1) / length(u) - u))
  expect_equal(unname(got["dkw_distance"]), manual)
  expect_equal(unname(got["displayed_band_crossing"]), 0)
  expect_equal(unname(got["residual_mean"]), mean(qnorm(u)))
  expect_equal(unname(got["residual_sd"]), sd(qnorm(u)))
  expect_equal(n09_pit_metrics(rev(u), bands), got)
  expect_equal(unname(n09_pit_metrics(seq(.80, .99, length.out = 100), bands)["dkw_crossing"]), 1)
  expect_error(n09_pit_metrics(c(0, .5, 1), bands))
})

test_that("Monte Carlo intervals and failure capture do not hide exclusions", {
  expect_equal(unname(n09_wilson(5, 100)["rate"]), .05)
  expect_equal(unname(n09_wilson(0, 100)["lower"]), 0)
  expect_equal(unname(n09_wilson(100, 100)["upper"]), 1)
  expect_true(all(is.na(n09_wilson(0, 0))))
  expect_error(n09_wilson(101, 100))
  x <- n09_capture({ warning("keep me"); stop("failed fit") })
  expect_equal(x$warnings, "keep me")
  expect_equal(x$error, "failed fit")
  expect_false(n09_fit_status(x, "glmmTMB")$valid)
})

test_that("stored band extraction reproduces the public bayesplot curves", {
  set.seed(391)
  u <- runif(480)
  bands <- n09_bands()
  p <- bayesplot::ppc_pit_ecdf(pit = u, K = 100, prob = .95,
    method = "independent", interpolate_adj = FALSE)
  native <- ggplot2::ggplot_build(p)$data
  expect_equal(bands$upper, native[[1]]$y)
  expect_equal(bands$lower, native[[2]]$y)
  expect_equal(findInterval(bands$x, sort(u)) / length(u), native[[3]]$y)
  expect_equal(unname(n09_pit_metrics(u, bands)["displayed_band_crossing"]),
    as.numeric(any(native[[3]]$y < native[[2]]$y | native[[3]]$y > native[[1]]$y)))
})

grDevices::dev.off()
