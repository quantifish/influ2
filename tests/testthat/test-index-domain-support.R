.domain_support_mock <- function(backend = "sdmTMB") {
  data <- data.frame(year = c(1, 1, 1, 2, 2), cell = letters[1:5],
    response = c(2, NA, 0, 4, 1), row.names = paste0("event", 1:5))
  ids <- rownames(data)[c(1, 3, 4, 5)]
  design <- matrix(1, length(ids), 1, dimnames = list(ids, "(Intercept)"))
  td <- list(X_ij = if (backend == "sdmTMB") list(design) else design,
    weights_i = c(1, 0, 2, 1), size = rep(1, 4), size_i = rep(1, 4))
  model <- if (backend == "sdmTMB") {
    list(formula = list(response ~ 1), tmb_data = td)
  } else list(formula = response ~ 1, data = data, tmb_inputs = list(tmb_data = td))
  list(model = model, info = list(data = data), data = data, ids = ids)
}

test_that("native fitted row IDs, missing responses, and zero weights define support", {
  for (backend in c("sdmTMB", "tinyVAST")) {
    f <- .domain_support_mock(backend)
    observed <- .domain_sampling_data(f$model, backend, f$info)
    expect_identical(rownames(observed), f$ids[c(1, 3, 4)])
    expect_identical(observed$cell, c("a", "d", "e"))
    policy <- attr(observed, "domain_sampling_support")
    expect_identical(policy$stored_rows, 5L)
    expect_identical(policy$fitted_rows, 4L)
    expect_identical(policy$supported_rows, 3L)
    expect_identical(policy$excluded_missing_or_unfitted, 1L)
    expect_identical(policy$excluded_zero_weight_or_trials, 1L)
    expect_match(policy$policy, "observed response.*positive fitted likelihood")
    shuffled <- f$info
    shuffled$data <- shuffled$data[c(5, 3, 2, 4, 1), ]
    expect_identical(rownames(.domain_sampling_data(f$model, backend, shuffled)),
      rownames(observed))
  }
})

test_that("tinyVAST full-data weights are aligned through native fitted row IDs", {
  f <- .domain_support_mock("tinyVAST")
  f$model$tmb_inputs$tmb_data$weights_i <- c(1, 1, 0, 2, 1)
  f$model$tmb_inputs$tmb_data$size_i <- c(1, 1, 1, 1, 0)
  observed <- .domain_sampling_data(f$model, "tinyVAST", f$info)
  expect_identical(observed$cell, c("a", "d"))
  expect_identical(attr(observed, "domain_sampling_support")$excluded_zero_weight_or_trials, 2L)
})

test_that("repeated sdmTMB delta likelihood weights require matching components", {
  f <- .domain_support_mock()
  f$model$tmb_data$X_ij[[2]] <- f$model$tmb_data$X_ij[[1]]
  f$model$tmb_data$weights_i <- rep(f$model$tmb_data$weights_i, 2)
  observed <- .domain_sampling_data(f$model, "sdmTMB", f$info)
  expect_identical(observed$cell, c("a", "d", "e"))
  f$model$tmb_data$weights_i[8] <- 0
  expect_error(.domain_sampling_data(f$model, "sdmTMB", f$info),
    "component likelihood weights disagree")
})

test_that("zero native trials and non-finite observed values do not imply support", {
  f <- .domain_support_mock()
  f$model$tmb_data$weights_i[] <- 1
  f$model$tmb_data$size <- c(1, 0, 1, 1)
  f$info$data$response[4] <- Inf
  observed <- .domain_sampling_data(f$model, "sdmTMB", f$info)
  expect_identical(observed$cell, c("a", "e"))
  f$model$tmb_data$weights_i[] <- 0
  expect_identical(nrow(.domain_sampling_data(f$model, "sdmTMB", f$info)), 0L)
})

test_that("ambiguous native support alignment fails rather than guessing rows", {
  f <- .domain_support_mock()
  bad <- f$model
  rownames(bad$tmb_data$X_ij[[1]]) <- NULL
  expect_error(.domain_sampling_data(bad, "sdmTMB", f$info), "row identifiers")
  bad <- f$model
  rownames(bad$tmb_data$X_ij[[1]])[1] <- "absent"
  expect_error(.domain_sampling_data(bad, "sdmTMB", f$info), "row identifiers")
  bad <- f$model
  bad$tmb_data$X_ij[[2]] <- bad$tmb_data$X_ij[[1]][4:1, , drop = FALSE]
  expect_error(.domain_sampling_data(bad, "sdmTMB", f$info), "row identifiers")
  bad <- f$model
  bad$tmb_data$weights_i <- 1
  expect_error(.domain_sampling_data(bad, "sdmTMB", f$info), "likelihood weights.*align")
  bad <- f$model
  bad$tmb_data$weights_i[1] <- -1
  expect_error(.domain_sampling_data(bad, "sdmTMB", f$info), "malformed")
  bad <- f$model
  bad$tmb_data$size <- NULL
  expect_error(.domain_sampling_data(bad, "sdmTMB", f$info), "trial counts.*unavailable")
  bad <- f$model
  bad$formula <- list(log(response) ~ 1)
  expect_error(.domain_sampling_data(bad, "sdmTMB", f$info), "named natural-response")
  expect_error(.domain_sampling_data(f$model, "unknown", f$info), "unsupported")
})

test_that("GAM support uses the retained frame and handles trial and case weights", {
  skip_if_not_installed("mgcv")
  set.seed(283)
  data <- data.frame(year = rep(1:2, each = 30), x = runif(60),
    weight = rep(c(1, 1, 0), 20))
  data$y <- rpois(60, exp(0.5 + data$x))
  model <- mgcv::gam(y ~ factor(year) + s(x, k = 4), data = data,
    weights = weight, family = poisson())
  info <- list(data = stats::model.frame(model))
  observed <- .domain_sampling_data(model, "gam", info)
  expect_identical(rownames(observed), rownames(info$data)[data$weight > 0])
  shuffled <- info
  shuffled$data <- info$data[60:1, ]
  expect_error(.domain_sampling_data(model, "gam", shuffled), "different rows or ordering")
  data$success <- rbinom(60, 5, 0.5)
  data$failure <- 5 - data$success
  data$success[1] <- data$failure[1] <- 0
  model <- mgcv::gam(cbind(success, failure) ~ factor(year), data = data,
    family = binomial())
  observed <- .domain_sampling_data(model, "gam", list(data = stats::model.frame(model)))
  expect_identical(nrow(observed), 59L)
})

.domain_native_support_fixture <- function(backend) {
  set.seed(285)
  data <- data.frame(year = rep(1:2, each = 60),
    cell = rep(c(rep("observed", 40), rep("missing", 10), rep("zero_weight", 10)), 2),
    x = rnorm(120), row.names = paste0("event", 1:120))
  data$y <- rpois(120, exp(1 + 0.1 * data$x))
  data$y[data$cell == "missing"] <- NA
  data$Easting <- match(data$cell, c("observed", "missing", "zero_weight")) - 1
  data$Northing <- 0
  weight <- as.numeric(data$cell != "zero_weight")
  model <- if (backend == "sdmTMB") {
    sdmTMB::sdmTMB(y ~ factor(year) + x, data = data, family = poisson(),
      weights = weight, spatial = "off", spatiotemporal = "off", silent = TRUE)
  } else tinyVAST::tinyVAST(y ~ factor(year) + x, data = data,
    family = poisson(), weights = weight, spatial_domain = NULL)
  list(model = model, data = data,
    reference = data.frame(cell = c("observed", "missing", "zero_weight"), x = 0,
      Easting = 0:2, Northing = 0))
}

for (backend in c("sdmTMB", "tinyVAST")) local({
  selected <- backend
  test_that(paste(selected, "sampling ignores native missing responses and zero-weight rows"), {
    skip_if_not_installed(selected)
    f <- .domain_native_support_fixture(selected)
    info <- .index_spatial_info(f$model, selected, "year")
    observed <- .domain_sampling_data(f$model, selected, info)
    expect_identical(nrow(info$data), 120L)
    expect_identical(nrow(observed), 80L)
    expect_true(all(observed$cell == "observed"))
    expect_true(all(is.finite(observed$y)))
    sampling <- .domain_sampling(observed, f$reference, "year", 1:2,
      "cell", sampled_definition = "cell_time")
    expect_equal(unname(sampling$counts), matrix(c(40L, 0L, 0L), 3, 2))
    expect_equal(sampling$coverage$share[sampling$coverage$class == "never_sampled"],
      c(2 / 3, 2 / 3))
    coordinate_sampling <- .domain_sampling(observed, f$reference, "year", 1:2,
      list(coords = c("Easting", "Northing"), distance = 0),
      sampled_definition = "cell_time")
    expect_identical(coordinate_sampling$counts, sampling$counts)
    result <- index_domain(f$model, f$reference, year = "year", sampled = "cell",
      uncertainty = "none")
    expect_identical(result$counts, sampling$counts)
    expect_equal(result$coverage, sampling$coverage)
    expect_identical(result$metadata$sampling_support$supported_rows, 80L)
    expect_identical(result$baseline$metadata$sampling_support,
      result$metadata$sampling_support)
  })
})

test_that("native moving-coverage example plots despite an empty fixed core", {
  skip_if_not_installed("sdmTMB")
  set.seed(286)
  data <- data.frame(year = rep(1:2, each = 40),
    cell = rep(c("a", "b"), each = 40), x = rnorm(80))
  data$y <- rpois(80, exp(1 + 0.1 * data$x))
  model <- sdmTMB::sdmTMB(y ~ factor(year) + x, data = data,
    family = poisson(), spatial = "off", spatiotemporal = "off", silent = TRUE)
  ref <- data.frame(cell = c("a", "b"), x = 0)
  expect_warning(expect_warning(result <- index_domain(model, ref, year = "year",
    sampled = "cell",
    sampled_definition = c("cell_time", "cell_ever", "cell_always"),
    uncertainty = "none"), "fixed.*cell_always.*empty"), "Empty cell_always")
  expect_true(all(is.na(result$sampled_only$cell_always$table$Mean)))
  expect_warning(p <- plot(result), "cell_always.*coverage remains shown")
  expect_s3_class(p, "patchwork")
  expect_true(all(is.finite(p[[1]]$data$Mean[p[[1]]$data$Series == "Full index"])))
})
