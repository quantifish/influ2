test_that("field selection preserves native family mean adjustments", {
  r <- list(proj_fe = matrix(c(1, 2), 2), proj_rf = matrix(c(0.4, -0.2), 2),
    proj_epsilon_st_A_vec = matrix(c(0.1, -0.1), 2))
  adjustment <- log(1.3)
  r$proj_eta <- r$proj_fe + r$proj_rf + adjustment
  expect_equal(.index_sdmtmb_eta(r, "all"), r$proj_eta)
  expect_equal(.index_sdmtmb_eta(r, "none"), r$proj_fe + adjustment)
  expect_equal(.index_sdmtmb_eta(r, "spatial"), r$proj_eta - r$proj_epsilon_st_A_vec)
  expect_equal(.index_sdmtmb_eta(r, "spatiotemporal"), r$proj_fe + adjustment + r$proj_epsilon_st_A_vec)
})

test_that("joint Gaussian draws retain the full covariance and parameter order", {
  skip_if_not_installed("Matrix")
  covariance <- matrix(c(1, 0.8, 0.8, 2), 2)
  mode <- c(a = 2, b = 3)
  info <- list(mode = mode, sd = list(jointPrecision = Matrix::Matrix(solve(covariance), sparse = TRUE)))
  obj <- list(env = list(last.par = mode, random = 2L))
  set.seed(2)
  sample <- t(.index_spatial_sampler(info, obj)(20000))
  expect_equal(unname(colMeans(sample)), unname(mode), tolerance = 0.02)
  expect_equal(unname(cov(sample)), covariance, tolerance = 0.025)
  obj$env$last.par <- rev(mode)
  expect_error(.index_spatial_sampler(info, obj), "parameter order")
})

test_that("sdmTMB delta field indices agree with native integration", {
  skip_if_not_installed("sdmTMB")
  data("pcod_2011", package = "sdmTMB")
  data("pcod_mesh_2011", package = "sdmTMB")
  m <- sdmTMB::sdmTMB(density ~ factor(year) + depth_scaled, data = pcod_2011,
    mesh = pcod_mesh_2011, family = sdmTMB::delta_gamma(), time = "year",
    spatial = "on", spatiotemporal = "iid", silent = TRUE)
  ref <- as.data.frame(pcod_2011)[c(1, 20, 50), c("X", "Y", "depth_scaled")]
  ref$area <- c(2, 3, 5)
  original <- m$tmb_obj$env$last.par.best
  set.seed(90)
  rng <- .Random.seed
  a <- cpue_index(m, reference_data = ref, ndraws = 30, retain = "draws", batch_size = 5, draw_batch_size = 7)
  expect_identical(.Random.seed, rng)
  expect_identical(m$tmb_obj$env$last.par.best, original)
  b <- cpue_index(m, reference_data = ref, ndraws = 30, retain = "draws", batch_size = 100, draw_batch_size = 13)
  expect_equal(a$table, b$table)
  expect_equal(a$draws, b$draws)
  expect_equal(dim(a$draws), c(30L, 4L))
  expect_true(all(is.finite(a$table$SD)))
  expect_null(cpue_index(m, reference_data = ref, uncertainty = "none")$draws)
  total <- integrate_index(m, ref, "area", area_units = "km^2", response_units = "kg/km^2", uncertainty = "none")
  nd <- do.call(rbind, lapply(total$table$Year, function(y) transform(ref, year = as.numeric(y))))
  native <- suppressMessages(sdmTMB::get_index(predict(m, nd, return_tmb_object = TRUE), area = nd$area, bias_correct = FALSE))
  expect_equal(total$table$Mean, native$est)
  report <- predict(m, nd, return_tmb_report = TRUE)
  for (fields in c("all", "spatial", "spatiotemporal", "none")) {
    eta <- switch(fields, all = report$proj_eta, spatial = report$proj_eta - report$proj_epsilon_st_A_vec,
      spatiotemporal = report$proj_fe + report$proj_epsilon_st_A_vec, none = report$proj_fe)
    expected <- plogis(eta[, 1]) * exp(eta[, 2])
    index <- cpue_index(m, reference_data = ref, spatial_fields = fields, uncertainty = "none")
    expect_equal(index$table$Mean, as.numeric(tapply(expected, nd$year, mean)))
  }
  for (seed in list(NA, -1, Inf, 1.2)) expect_error(cpue_index(m, reference_data = ref, seed = seed), "seed")
  expect_error(cpue_index(m, reference_data = ref, ndraws = 1), "two joint draws")
  invalid <- m
  invalid$sd_report$pdHess <- FALSE
  expect_error(cpue_index(invalid, reference_data = ref), "positive-definite")
  invalid <- m
  invalid$nonlocal_parsed <- list()
  expect_error(cpue_index(invalid, reference_data = ref), "Nonlocal")
})

test_that("sdmTMB offset predictions explicitly use the reference exposure", {
  skip_if_not_installed("sdmTMB")
  set.seed(12)
  d <- data.frame(year = factor(rep(1:3, each = 80)), x = runif(240), exposure = runif(240, 1, 3))
  d$catch <- rpois(240, d$exposure * exp(0.5 + d$x))
  m <- sdmTMB::sdmTMB(catch ~ year + x, data = d, offset = log(d$exposure), family = poisson(), spatial = "off", silent = TRUE)
  ref <- data.frame(x = 0.4, log_exposure = log(2))
  a <- cpue_index(m, reference_data = ref, uncertainty = "none")
  b <- cpue_index(m, reference_data = ref, uncertainty = "none", prediction_offset = "log_exposure")
  expect_equal(b$table$Mean, 2 * a$table$Mean)
  expect_equal(cpue_index(m, reference_data = ref, uncertainty = "none",
    prediction_offset = "log_exposure", spatial_fields = "none")$table$Mean, b$table$Mean)
  expect_error(cpue_index(m, reference_data = ref, prediction_offset = "absent"), "prediction_offset")
})

test_that("standard and Poisson-link delta indices use the combined native response", {
  skip_if_not_installed("sdmTMB")
  skip_if_not_installed("tinyVAST")
  set.seed(31)
  d <- data.frame(year = factor(rep(1:3, each = 100)), x = runif(300, -1, 1))
  d$time <- as.integer(d$year)
  d$catch <- rbinom(300, 1, plogis(0.3 + d$x)) * rgamma(300, shape = 3, scale = exp(1 + d$x) / 3)
  for (kind in c("standard", "poisson-link")) {
    models <- list(
      sdmTMB::sdmTMB(catch ~ year + x, data = d, family = sdmTMB::delta_gamma(type = kind), spatial = "off", silent = TRUE),
      tinyVAST::tinyVAST(catch ~ year + x, data = d, family = tinyVAST::delta_gamma(type = kind), spatial_domain = NULL))
    for (m in models) {
      ref <- data.frame(x = c(-0.5, 0.5))
      a <- cpue_index(m, year = "year", reference_data = ref, ndraws = 20)
      nd <- do.call(rbind, lapply(1:3, function(t) transform(ref, year = factor(t, levels = 1:3), time = t)))
      p <- if (inherits(m, "sdmTMB")) predict(m, nd, type = "response")$est else predict(m, nd, what = "mu_g")
      expect_equal(a$table$Mean, as.numeric(tapply(p, nd$year, mean)))
      expect_true(all(is.finite(a$table$SD)))
    }
  }
})

tiny_index_fixture <- function() {
  set.seed(101)
  nx <- 7L; nt <- 6L
  correlation <- exp(-0.5 * abs(outer(1:nx, 1:nx, "-")))
  covariance <- kronecker(correlation, correlation)
  sp <- t(chol(0.7^2 * covariance + diag(1e-8, nx^2)))
  st <- t(chol(0.5^2 * covariance + diag(1e-8, nx^2)))
  epsilon <- t(replicate(nt, as.numeric(st %*% rnorm(nx^2))))
  for (i in 2:nt) epsilon[i, ] <- 0.45 * epsilon[i - 1, ] + sqrt(1 - 0.45^2) * epsilon[i, ]
  omega <- as.numeric(sp %*% rnorm(nx^2))
  eta <- 1 + outer(seq(-0.15, 0.15, length.out = nt), rep(1, nx^2)) + outer(rep(1, nt), omega) + epsilon
  d <- expand.grid(time = 1:nt, x = 1:nx, ycoord = 1:nx)
  d$count <- rpois(nrow(d), exp(as.vector(eta)))
  d$var <- "density"; d$dist <- "poisson"
  probability <- 0.45 + 0.50 * exp(-(d$x - (1 + (d$time - 1) * (nx - 1) / (nt - 1)))^2 / 8)
  d <- d[runif(nrow(d)) < probability, ]
  d$year <- factor(d$time)
  mesh <- fmesher::fm_mesh_2d(d[c("x", "ycoord")], cutoff = 1)
  tinyVAST::tinyVAST(count ~ year, data = d, family = list(poisson = poisson()), spatial_domain = mesh,
    space_term = "density <-> density, spatial_sd",
    spacetime_term = "density -> density, 1, rho\n density <-> density, 0, spatiotemporal_sd",
    space_columns = c("x", "ycoord"))
}

test_that("tinyVAST spatial indices preserve fields, native time, and native totals", {
  skip_if_not_installed("tinyVAST")
  skip_if_not_installed("fmesher")
  m <- tiny_index_fixture()
  ref <- data.frame(x = c(1, 3, 7), ycoord = c(1, 4, 7), area = c(2, 3, 5))
  original <- m$obj$env$last.par.best
  a <- cpue_index(m, year = "year", reference_data = ref, ndraws = 30, retain = "draws", batch_size = 7, draw_batch_size = 11)
  b <- cpue_index(m, year = "year", reference_data = ref, ndraws = 30, retain = "draws", batch_size = 100, draw_batch_size = 19)
  expect_identical(m$obj$env$last.par.best, original)
  expect_equal(a$table, b$table)
  expect_equal(a$draws, b$draws)
  expect_true(all(is.finite(a$table$SD)))
  nd <- do.call(rbind, lapply(1:6, function(t) transform(ref, time = t, year = factor(t, levels = 1:6))))
  native <- predict(m, nd, what = "mu_g", se.fit = FALSE, bias.correct = FALSE)
  expect_equal(a$table$Mean, as.numeric(tapply(native, nd$time, mean)))
  total <- integrate_index(m, ref, "area", year = "year", area_units = "km^2", response_units = "fish/km^2", uncertainty = "none")
  # Native block-wise integration was introduced in tinyVAST 1.6.2.
  if ("block" %in% names(formals(tinyVAST::integrate_output))) {
    native <- tinyVAST::integrate_output(m, nd, area = nd$area, block = nd$time, getsd = FALSE, bias.correct = FALSE)
    expect_equal(total$table$Mean, native$Estimate)
  }
  none <- cpue_index(m, year = "year", reference_data = ref, spatial_fields = "none", uncertainty = "none")
  expect_false(isTRUE(all.equal(a$table$Mean, none$table$Mean)))
  expect_error(cpue_index(m, year = "year", reference_data = transform(ref, time = 1)), "fixed native time")
  expect_error(cpue_index(m, year = "year", reference_data = transform(ref, var = "other")), "fitted tinyVAST domain")
  expect_error(cpue_index(m, year = "year", reference_data = transform(ref, dist = "other")), "fitted tinyVAST family")
  relative <- cpue_index(m, year = "year", reference_data = ref, ndraws = 20, retain = "draws", rescale = 1)
  expect_equal(exp(rowMeans(log(relative$draws))), rep(1, 20), ignore_attr = TRUE)
})
