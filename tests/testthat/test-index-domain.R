domain_integration_fixture <- local({
  cache <- new.env(parent = emptyenv())
  function(backend, delta = FALSE) {
    key <- paste(backend, delta)
    if (exists(key, cache, inherits = FALSE)) return(cache[[key]])
    set.seed(271)
    if (backend == "sdmTMB") {
      data("pcod_2011", package = "sdmTMB", envir = environment())
      data("pcod_mesh_2011", package = "sdmTMB", envir = environment())
      model <- sdmTMB::sdmTMB(density ~ factor(year) + depth_scaled,
        data = pcod_2011, mesh = pcod_mesh_2011,
        family = sdmTMB::delta_gamma(), time = "year",
        spatial = "on", spatiotemporal = "iid", silent = TRUE)
      ref <- as.data.frame(pcod_2011)[c(1, 20, 50),
        c("X", "Y", "depth_scaled")]
      year <- "year"
    } else if (delta) {
      d <- data.frame(year = factor(rep(1:4, each = 100)),
        x = runif(400, -1, 1))
      d$catch <- rbinom(400, 1, plogis(0.5 + d$x)) *
        rgamma(400, shape = 3, scale = exp(1 + d$x) / 3)
      model <- tinyVAST::tinyVAST(catch ~ year + x, data = d,
        family = tinyVAST::delta_gamma(), spatial_domain = NULL)
      ref <- data.frame(x = c(-0.8, 0.1, 0.7))
      year <- "year"
    } else {
      nx <- 7L
      nt <- 6L
      grid <- expand.grid(x = 1:nx, ycoord = 1:nx)
      covariance <- exp(-as.matrix(dist(grid)) / 2)
      L <- t(chol(covariance + diag(1e-8, nrow(grid))))
      omega <- as.numeric(0.7 * L %*% rnorm(nrow(grid)))
      epsilon <- replicate(nt, as.numeric(0.6 * L %*% rnorm(nrow(grid))))
      d <- expand.grid(time = 1:nt, cell = seq_len(nrow(grid)))
      d$x <- grid$x[d$cell]
      d$ycoord <- grid$ycoord[d$cell]
      d$count <- rpois(nrow(d), exp(1 + 0.1 * d$time +
        omega[d$cell] + epsilon[cbind(d$cell, d$time)]))
      d$year <- factor(d$time)
      d$var <- "density"
      d$dist <- "poisson"
      mesh <- fmesher::fm_mesh_2d(grid, cutoff = 1)
      model <- tinyVAST::tinyVAST(count ~ year, data = d,
        family = list(poisson = poisson()), spatial_domain = mesh,
        space_term = "density <-> density, spatial_sd",
        spacetime_term = paste("density -> density, 1, rho",
          "density <-> density, 0, spatiotemporal_sd", sep = "\n"),
        space_columns = c("x", "ycoord"))
      ref <- data.frame(x = c(1, 3, 7), ycoord = c(1, 4, 7))
      year <- "year"
    }
    ref$area <- c(2, 3, 5)
    years <- if (is.factor(model$data[[year]])) {
      levels(model$data[[year]])
    } else sort(unique(as.character(model$data[[year]])))
    cache[[key]] <- list(model = model, reference = ref, year = year,
      years = years)
    cache[[key]]
  }
})

domain_all_sampled <- function(fixture) {
  matrix(TRUE, nrow(fixture$reference), length(fixture$years))
}

domain_expect_baselines <- function(fixture) {
  model <- fixture$model
  ref <- fixture$reference
  sampled <- domain_all_sampled(fixture)
  result <- index_domain(model, ref, year = fixture$year, sampled = sampled,
    reference_weights = ref$area, uncertainty = "none")
  expected <- cpue_index(model, reference_data = ref, year = fixture$year,
    reference_weights = ref$area, uncertainty = "none")
  expect_s3_class(result, "influ_domain")
  expect_s3_class(result$baseline, "influ_index")
  expect_equal(result$baseline$table$Mean, expected$table$Mean)
  expect_setequal(names(result$sampled_only),
    c("cell_time", "cell_ever", "cell_always"))
  for (index in result$sampled_only) {
    expect_s3_class(index, "influ_index")
    expect_equal(index$table$Mean, expected$table$Mean)
  }
  total <- index_domain(model, ref, year = fixture$year, sampled = sampled,
    area = "area", area_units = "km^2", response_units = "kg/km^2",
    uncertainty = "none")
  expected_total <- integrate_index(model, ref, "area", year = fixture$year,
    area_units = "km^2", response_units = "kg/km^2", uncertainty = "none")
  expect_equal(total$baseline$table$Mean, expected_total$table$Mean)
  for (index in total$sampled_only) {
    expect_equal(index$table$Mean, expected_total$table$Mean)
  }
  invisible(result)
}

test_that("sdmTMB delta domains reproduce CPUE and integrated indices", {
  skip_if_not_installed("sdmTMB")
  f <- domain_integration_fixture("sdmTMB", delta = TRUE)
  explicit <- domain_expect_baselines(f)
  expect_warning(heuristic <- index_domain(f$model, f$reference, year = f$year,
    reference_weights = f$reference$area, sampled_definition = "cell_time",
    uncertainty = "none"), "Default sampling distance")
  expect_equal(heuristic$baseline$table$Mean, explicit$baseline$table$Mean)
  expect_identical(heuristic$metadata$omission_definition, "cell_time")
})

test_that("univariate tinyVAST domains reproduce spatial CPUE and totals", {
  skip_if_not_installed("tinyVAST")
  skip_if_not_installed("fmesher")
  domain_expect_baselines(domain_integration_fixture("tinyVAST"))
})

test_that("univariate tinyVAST delta domains retain the combined response", {
  skip_if_not_installed("tinyVAST")
  domain_expect_baselines(domain_integration_fixture("tinyVAST", delta = TRUE))
})

test_that("sampled-only means renormalise weights and empty years stay unavailable", {
  skip_if_not_installed("sdmTMB")
  f <- domain_integration_fixture("sdmTMB", delta = TRUE)
  sampled <- domain_all_sampled(f)
  sampled[3, ] <- FALSE
  sampled[, 2] <- FALSE
  expect_warning(expect_warning(expect_warning(result <- index_domain(f$model, f$reference, year = f$year,
    sampled = sampled, reference_weights = f$reference$area,
    uncertainty = "none", min_years = 2), "Empty cell_time domain in year\\(s\\): 2013"),
    "Empty cell_always domain in year\\(s\\): 2011, 2013, 2015, 2017"),
    "fixed `cell_always` sampled domain is empty")
  expected <- cpue_index(f$model,
    reference_data = f$reference[1:2, ], year = f$year,
    reference_weights = f$reference$area[1:2], uncertainty = "none")
  keep <- seq_along(f$years) != 2
  expect_equal(result$sampled_only$cell_time$table$Mean[keep],
    expected$table$Mean[keep])
  expect_true(is.na(result$sampled_only$cell_time$table$Mean[2]))
  expect_equal(result$sampled_only$cell_ever$table$Mean, expected$table$Mean)
  expect_true(all(is.na(result$sampled_only$cell_always$table$Mean)))
  expect_warning(expect_warning(expect_warning(total <- index_domain(f$model, f$reference, year = f$year,
    sampled = sampled, area = "area", area_units = "km^2",
    response_units = "kg/km^2", uncertainty = "none"),
    "Empty cell_time domain in year\\(s\\): 2013"),
    "Empty cell_always domain in year\\(s\\): 2011, 2013, 2015, 2017"),
    "fixed `cell_always` sampled domain is empty")
  expect_equal(total$sampled_only$cell_time$table$Mean[2], 0)
  expect_equal(total$sampled_only$cell_always$table$Mean, rep(0, length(f$years)))
  expect_equal(total$sampled_only$cell_time$table$Mean[keep],
    sum(f$reference$area[1:2]) * expected$table$Mean[keep])
})

domain_expect_field_omissions <- function(fixture) {
  result <- index_domain(fixture$model, fixture$reference, year = fixture$year,
    sampled = domain_all_sampled(fixture),
    omit = c("spatial", "spatiotemporal"), where = "all",
    uncertainty = "none")
  for (term in c("spatial", "spatiotemporal")) {
    remaining <- if (term == "spatial") "spatiotemporal" else "spatial"
    expected <- cpue_index(fixture$model,
      reference_data = fixture$reference, year = fixture$year,
      spatial_fields = remaining, uncertainty = "none")
    expect_equal(result$omission[[paste0(term, ":all")]]$table$Mean,
      expected$table$Mean, tolerance = 1e-8)
  }
}

test_that("field omissions match native-backed sdmTMB field selection", {
  skip_if_not_installed("sdmTMB")
  domain_expect_field_omissions(domain_integration_fixture("sdmTMB", TRUE))
})

test_that("field omissions match native-backed tinyVAST field selection", {
  skip_if_not_installed("tinyVAST")
  skip_if_not_installed("fmesher")
  domain_expect_field_omissions(domain_integration_fixture("tinyVAST"))
})

test_that("class partitions share joint draws and batching preserves model and RNG", {
  skip_if_not_installed("sdmTMB")
  f <- domain_integration_fixture("sdmTMB", TRUE)
  sampled <- domain_all_sampled(f)
  sampled[2, 2] <- FALSE
  sampled[3, ] <- FALSE
  before <- list(best = f$model$tmb_obj$env$last.par.best,
    last = f$model$tmb_obj$env$last.par, mode = f$model$last.par.best,
    fixed = f$model$model$par)
  set.seed(2709)
  rng <- .Random.seed
  a <- index_domain(f$model, f$reference, year = f$year, sampled = sampled,
    reference_weights = f$reference$area, ndraws = 24, seed = 19,
    batch_size = 2, draw_batch_size = 5)
  expect_identical(.Random.seed, rng)
  b <- index_domain(f$model, f$reference, year = f$year, sampled = sampled,
    reference_weights = f$reference$area, ndraws = 24, seed = 19,
    batch_size = 100, draw_batch_size = 13)
  expect_identical(.Random.seed, rng)
  expect_identical(list(best = f$model$tmb_obj$env$last.par.best,
    last = f$model$tmb_obj$env$last.par, mode = f$model$last.par.best,
    fixed = f$model$model$par), before)
  expect_equal(a$baseline$table, b$baseline$table)
  expect_equal(a$baseline$draws, b$baseline$draws)
  expect_equal(a$sampled_only, b$sampled_only)
  expect_equal(a$partition, b$partition)
  expect_equal(a$partition_draws, b$partition_draws)
  expect_equal(a$decomposition, b$decomposition)
  expect_equal(a$trend, b$trend)
  expect_true(all(c("Year", "class", "Mean", "SD", "Qlower", "Qupper",
    "share", "share_lower", "share_upper") %in% names(a$partition)))
  expect_setequal(names(a$partition_draws),
    c("sampled", "previously_sampled", "never_sampled"))
  for (i in seq_along(f$years)) {
    rows <- as.character(a$partition$Year) == f$years[i]
    expect_equal(sum(a$partition$Mean[rows]), a$baseline$table$Mean[i])
    expect_equal(sum(a$partition$share[rows]), 1)
  }
  expect_equal(Reduce(`+`, a$partition_draws), a$baseline$draws,
    tolerance = 1e-8)
  for (cl in names(a$partition_draws)) {
    draws <- a$partition_draws[[cl]]
    expect_equal(dim(draws), c(24L, length(f$years)))
    rows <- match(f$years,
      as.character(a$partition$Year[a$partition$class == cl]))
    tab <- a$partition[a$partition$class == cl, ][rows, ]
    expect_equal(tab$SD, apply(draws, 2, sd), ignore_attr = TRUE)
    expect_equal(tab$Qlower, apply(draws, 2, quantile, probs = 0.025),
      ignore_attr = TRUE)
    expect_equal(tab$Qupper, apply(draws, 2, quantile, probs = 0.975),
      ignore_attr = TRUE)
  }
})

domain_expect_native_decomposition <- function(fixture, delta = FALSE) {
  f <- fixture
  result <- index_domain(f$model, f$reference, year = f$year,
    sampled = domain_all_sampled(f), uncertainty = "none",
    retain_cells = f$years, batch_size = 2)
  cells <- result$retained_cells
  expect_true(all(c("Year", "cell", "term", "component", "contribution") %in%
    names(cells)))
  expect_setequal(as.character(unique(cells$Year)), f$years)
  components <- unique(cells$component)
  expect_length(components, if (delta) 2L else 1L)
  nd <- do.call(rbind, lapply(f$years, function(y) {
    d <- f$reference
    d[[f$year]] <- f$model$data[[f$year]][
      match(y, as.character(f$model$data[[f$year]]))]
    if (inherits(f$model, "tinyVAST") && "time" %in% names(f$model$data)) {
      d$time <- f$model$data$time[
        match(y, as.character(f$model$data[[f$year]]))]
    }
    d
  }))
  native <- if (inherits(f$model, "sdmTMB")) {
    predict(f$model, nd, return_tmb_report = TRUE)$proj_eta
  } else {
    eta <- matrix(predict(f$model, nd, what = "p1_g", se.fit = FALSE,
      bias.correct = FALSE), ncol = 1L)
    if (delta) eta <- cbind(eta, predict(f$model, nd, what = "p2_g",
      se.fit = FALSE, bias.correct = FALSE))
    eta
  }
  reconstructed <- matrix(NA_real_, nrow(nd), length(components))
  for (j in seq_along(components)) {
    for (i in seq_len(nrow(nd))) {
      keep <- as.character(cells$Year) == as.character(nd[[f$year]][i]) &
        cells$cell == (i - 1L) %% nrow(f$reference) + 1L &
        cells$component == components[j]
      expect_true(any(keep))
      reconstructed[i, j] <- sum(cells$contribution[keep])
    }
  }
  expect_equal(unname(reconstructed), unname(as.matrix(native)),
    tolerance = 1e-8)
  populated <- result$decomposition$class %in% c("all", "sampled")
  expect_true(all(is.finite(result$decomposition$mean[populated])))
  expect_true(all(is.na(result$decomposition$mean[!populated])))
  expect_null(index_domain(f$model, f$reference, year = f$year,
    sampled = domain_all_sampled(f), uncertainty = "none")$retained_cells)
}

test_that("retained sdmTMB delta terms reconstruct both native link predictors", {
  skip_if_not_installed("sdmTMB")
  domain_expect_native_decomposition(domain_integration_fixture("sdmTMB", TRUE),
    delta = TRUE)
})

test_that("retained tinyVAST terms reconstruct the spatial native predictor", {
  skip_if_not_installed("tinyVAST")
  skip_if_not_installed("fmesher")
  domain_expect_native_decomposition(domain_integration_fixture("tinyVAST"))
})

test_that("retained tinyVAST delta terms reconstruct both native predictors", {
  skip_if_not_installed("tinyVAST")
  domain_expect_native_decomposition(domain_integration_fixture("tinyVAST", TRUE),
    delta = TRUE)
})

test_that("a known drifting surface exposes dependence on a never-sampled region", {
  skip_if_not_installed("sdmTMB")
  set.seed(2727)
  nt <- 6L
  grid <- expand.grid(x = 1:7, y = 1:7)
  grid$cell <- seq_len(nrow(grid))
  covariance <- exp(-as.matrix(dist(grid[c("x", "y")])) / 1.5)
  L <- t(chol(covariance + diag(1e-8, nrow(grid))))
  omega <- as.numeric(0.8 * L %*% rnorm(nrow(grid)))
  epsilon <- replicate(nt, as.numeric(0.7 * L %*% rnorm(nrow(grid))))
  eta <- outer(0.12 * (grid$x - 1), 0:(nt - 1)) +
    omega + epsilon + 2
  d <- expand.grid(year = seq_len(nt), cell = grid$cell[grid$x <= 4],
    replicate = 1:3)
  d$x <- grid$x[d$cell]
  d$y <- grid$y[d$cell]
  d$count <- rpois(nrow(d), exp(eta[cbind(d$cell, d$year)]))
  ref <- grid[grid$x <= 4 | grid$x >= 6, ]
  ref$area <- ifelse(ref$x >= 6, 10, 1)
  # Mesh support covers the declared prediction domain, but only western
  # observations enter the likelihood. Do not confuse mesh support with data.
  domain_mesh <- sdmTMB::make_mesh(grid, xy_cols = c("x", "y"), cutoff = 1.1)
  mesh <- sdmTMB::make_mesh(d, xy_cols = c("x", "y"), mesh = domain_mesh$mesh)
  model <- sdmTMB::sdmTMB(count ~ factor(year) + x:year, data = d,
    mesh = mesh, family = poisson(), time = "year", spatial = "on",
    spatiotemporal = "iid", silent = TRUE)
  result <- index_domain(model, ref, year = "year", sampled = "cell",
    reference_weights = ref$area, uncertainty = "none",
    trend_window = 3:6, unsampled_threshold = 0.5,
    retain_cells = c(1, 6), batch_size = nrow(ref) * nt)
  never <- result$partition$class == "never_sampled"
  expect_gt(mean(result$partition$share[never]), 0.5)
  expect_true(any(result$flags$high_unsampled_share))
  slope <- function(v) unname(coef(lm(log(v) ~ seq_along(v)))[2])
  full <- result$baseline$table$Mean
  sampled <- result$sampled_only$cell_time$table$Mean
  expect_gt(abs(slope(full) - slope(sampled)), 0.01)
  truth <- exp(eta[ref$cell, , drop = FALSE])
  truth_full <- colSums(truth * ref$area) / sum(ref$area)
  sampled_rows <- ref$x <= 4
  truth_sampled <- colMeans(truth[sampled_rows, , drop = FALSE])
  expect_gt(abs(slope(truth_full) - slope(truth_sampled)), 0.01)
  trend <- result$trend
  expect_setequal(unique(trend$window), c("all", "window"))
  full_row <- trend$series == "full" & trend$window == "all"
  sampled_row <- trend$series == "cell_time" & trend$window == "all"
  expect_equal(trend$slope[full_row], slope(full))
  expect_equal(trend$slope[sampled_row], slope(sampled))
  expect_equal(trend$slope_difference[sampled_row],
    slope(sampled) - slope(full))
  # Confirm that unsampled cells really have native field projections. This
  # does not assert recovery of the unknown true trajectory in those cells.
  native_data <- rbind(transform(ref, year = 1), transform(ref, year = 6))
  native <- predict(model, native_data, return_tmb_report = TRUE)
  unobserved <- native_data$x >= 6
  expect_gt(max(abs(native$proj_omega_s_A[unobserved, 1])), 1e-6)
  expect_gt(max(abs(native$proj_epsilon_st_A_vec[unobserved, 1])), 1e-6)
  for (term in c("spatial", "spatiotemporal")) {
    cells <- result$retained_cells[result$retained_cells$term == term, ]
    cells <- cells[order(match(cells$Year, c("1", "6")), cells$cell), ]
    expected <- native[[if (term == "spatial") "proj_omega_s_A" else
      "proj_epsilon_st_A_vec"]][, 1]
    expect_equal(cells$contribution[unobserved], expected[unobserved],
      tolerance = 1e-8)
  }
})
