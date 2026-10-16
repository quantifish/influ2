omission_index_fixture <- function(uncertainty = "auto") {
  draws <- if (uncertainty == "auto") {
    cbind(`2001` = c(1, 2, 4, 8), `2002` = c(3, 4, 8, 10))
  } else NULL
  .domain_index(c(`2001` = 3, `2002` = 6), draws,
    list(method = "standardised", backend = "sdmTMB", family = "poisson",
      link = "log", scale = "response", rescale = "raw", probs = c(0.1, 0.9),
      uncertainty = uncertainty, omit = "spatial"))
}

test_that("CPUE omissions delegate once with one domain target and all settings", {
  captured <- NULL
  calls <- 0L
  local_mocked_bindings(index_domain = function(...) {
    calls <<- calls + 1L
    captured <<- list(...)
    list(omission = list(omission_index_fixture()))
  })
  model <- structure(list(), class = "sdmTMB")
  ref <- data.frame(cell = c("a", "b"), x = c(1, 2))
  rule <- list(x = list(rule = "fixed", value = list(x = 3)))
  result <- cpue_index(model, year = "year", reference_data = ref,
    reference_weights = c(1, 3), omit_terms = "spatial",
    omit_where = "never_sampled", sampled = "cell",
    sampled_definition = "cell_ever", min_obs = 2, min_years = 0.5,
    covariate_reference = rule, probs = c(0.1, 0.9), ndraws = 37,
    seed = 9, batch_size = 7, draw_batch_size = 3, units = "fish",
    prediction_offset = "exposure", retain = "draws")
  expect_s3_class(result, "influ_index")
  expect_equal(calls, 1L)
  expect_identical(captured[[1]], model)
  expected <- list(reference_data = ref, year = "year", sampled = "cell",
    sampled_definition = "cell_ever", omission_definition = "cell_ever",
    min_obs = 2, min_years = 0.5,
    sampled_only = FALSE, reference_weights = c(1, 3), decompose = FALSE,
    omit = "spatial", where = "never_sampled", covariate_reference = rule,
    ndraws = 37, seed = 9, batch_size = 7, draw_batch_size = 3,
    uncertainty = "auto", probs = c(0.1, 0.9),
    prediction_offset = "exposure", units = "fish")
  expect_identical(captured[-1], expected)
  expect_identical(result, omission_index_fixture())
})

test_that("CPUE omission defaults select one target and preserve covariance", {
  captured <- NULL
  local_mocked_bindings(index_domain = function(...) {
    captured <<- list(...)
    list(omission = list(omission_index_fixture()))
  })
  result <- cpue_index(structure(list(), class = "tinyVAST"),
    reference_data = data.frame(x = 1:2), omit_terms = "spatial")
  expect_identical(captured$where, "unsampled")
  expect_identical(captured$sampled_definition, "cell_time")
  expect_identical(captured$omission_definition, "cell_time")
  expect_null(captured$sampled)
  expect_identical(captured$ndraws, 1000L)
  expect_null(result$draws)
  expect_identical(result$covariance, omission_index_fixture()$covariance)
  expect_identical(result$table, omission_index_fixture()$table)
})

test_that("CPUE omissions rescale paired draws before summaries and retention", {
  local_mocked_bindings(index_domain = function(...) {
    list(omission = list(omission_index_fixture()))
  })
  model <- structure(list(), class = "sdmTMB")
  args <- list(model = model, reference_data = data.frame(x = 1:2),
    omit_terms = "spatial", rescale = 2)
  result <- do.call(cpue_index, c(args, list(retain = "draws")))
  source <- omission_index_fixture()
  expected <- source$draws * (2 / exp(rowMeans(log(source$draws))))
  expect_equal(result$draws, expected)
  expect_equal(exp(rowMeans(log(result$draws))), rep(2, nrow(expected)),
    ignore_attr = TRUE)
  expect_equal(geo_mean(result$table$Mean), 2)
  expect_equal(result$table$SD, apply(expected, 2, sd), ignore_attr = TRUE)
  expect_equal(result$table$CV, result$table$SD / result$table$Mean)
  intervals <- apply(expected, 2, quantile, probs = c(0.1, 0.9))
  expect_equal(result$table$Qlower, intervals[1, ], ignore_attr = TRUE)
  expect_equal(result$table$Qupper, intervals[2, ], ignore_attr = TRUE)
  expect_equal(result$covariance$response, cov(expected))
  expect_equal(result$covariance$log, cov(log(expected)))
  expect_identical(result$metadata$scale, "ratio")
  expect_identical(result$metadata$rescale, 2)
  summary <- do.call(cpue_index, args)
  expect_null(summary$draws)
  expect_identical(summary$table, result$table)
  expect_identical(summary$covariance, result$covariance)
})

test_that("CPUE omission previews omit uncertainty and reject invalid rescaling", {
  local_mocked_bindings(index_domain = function(..., uncertainty) {
    list(omission = list(omission_index_fixture(uncertainty)))
  })
  model <- structure(list(), class = "sdmTMB")
  preview <- cpue_index(model, reference_data = data.frame(x = 1:2),
    omit_terms = "spatial", uncertainty = "none", rescale = 1)
  expect_equal(geo_mean(preview$table$Mean), 1)
  expect_true(all(is.na(preview$table$SD)))
  expect_null(preview$draws)
  expect_null(preview$covariance)
  expect_error(cpue_index(model, omit_terms = "spatial", uncertainty = "none",
    retain = "draws"), "Draw retention")
  for (bad in list(0, -1, NA_real_, Inf, "unit")) {
    expect_error(cpue_index(model, omit_terms = "spatial", rescale = bad),
      "positive finite")
  }
  local_mocked_bindings(index_domain = function(...) {
    index <- omission_index_fixture()
    index$draws[1, 1] <- 0
    list(omission = list(index))
  })
  expect_error(cpue_index(model, omit_terms = "spatial", rescale = 1),
    "positive indices")
})

test_that("CPUE omission guards reject unsupported and overlapping options", {
  model <- structure(list(), class = "sdmTMB")
  expect_error(cpue_index(list(), omit_terms = "spatial"),
    "standardised sdmTMB or tinyVAST")
  expect_error(cpue_index(model, method = "year_effect", omit_terms = "spatial"),
    "standardised sdmTMB or tinyVAST")
  for (bad in list(c("spatial", "year"), NA_character_, "", 1)) {
    expect_error(cpue_index(model, omit_terms = bad), "exactly one prediction term")
  }
  for (fields in c("spatial", "spatiotemporal", "none")) {
    expect_error(cpue_index(model, omit_terms = "spatial", spatial_fields = fields),
      "overlapping omission switches")
  }
  expect_error(cpue_index(model, omit_terms = "spatial", omit_where = "unknown"),
    "arg")
  expect_error(cpue_index(model, omit_terms = "spatial",
    sampled_definition = c("cell_time", "cell_ever")), "length 1")
  expect_error(cpue_index(model, omit_terms = "spatial", unused = TRUE),
    "Additional arguments")
  local_mocked_bindings(index_domain = function(...) stop("unknown native term"))
  expect_error(cpue_index(model, omit_terms = "unknown"), "unknown native term")
})

test_that("ordinary GLM CPUE indices do not enter omission machinery", {
  local_mocked_bindings(index_domain = function(...) stop("unexpected domain call"))
  data <- data.frame(year = factor(rep(1:3, each = 6)), x = rep(1:6, 3),
    catch = c(1, 2, 1, 3, 2, 4, 2, 3, 2, 4, 5, 3, 4, 3, 5, 4, 6, 7))
  model <- glm(catch ~ year + x, family = poisson(), data = data)
  reference <- data.frame(x = c(2, 5))
  ordinary <- cpue_index(model, year = "year", reference_data = reference)
  expect_identical(cpue_index(model, year = "year", reference_data = reference,
    omit_terms = NULL), ordinary)
  expect_identical(cpue_index(model, year = "year", reference_data = reference,
    omit_terms = character()), ordinary)
  native <- vapply(levels(data$year), function(y) {
    mean(predict(model, transform(reference, year = factor(y, levels = levels(data$year))),
      type = "response"))
  }, numeric(1))
  expect_equal(ordinary$table$Mean, unname(native))
  expect_error(cpue_index(model, year = "year", reference_data = reference,
    omit_terms = "x"), "standardised sdmTMB or tinyVAST")
})

test_that("CPUE omission agrees with real spatial domain counterfactuals", {
  skip_if_not_installed("sdmTMB")
  data("pcod_2011", package = "sdmTMB")
  data("pcod_mesh_2011", package = "sdmTMB")
  model <- sdmTMB::sdmTMB(density ~ factor(year) + depth_scaled,
    data = pcod_2011, mesh = pcod_mesh_2011, family = sdmTMB::delta_gamma(),
    time = "year", spatial = "on", spatiotemporal = "iid", silent = TRUE)
  ref <- as.data.frame(pcod_2011)[c(1, 20, 50), c("X", "Y", "depth_scaled")]
  years <- sort(unique(pcod_2011$year))
  sampled <- matrix(c(TRUE, FALSE, FALSE), nrow(ref), length(years))
  domain <- index_domain(model, ref, sampled = sampled,
    sampled_definition = "cell_time", sampled_only = FALSE, decompose = FALSE,
    omit = "spatial", where = "unsampled", ndraws = 20, seed = 13)
  index <- cpue_index(model, reference_data = ref, sampled = sampled,
    omit_terms = "spatial", ndraws = 20, seed = 13, retain = "draws")
  expect_identical(index, domain$omission[[1]])
  summary <- cpue_index(model, reference_data = ref, sampled = sampled,
    omit_terms = "spatial", ndraws = 20, seed = 13)
  expect_null(summary$draws)
  expect_equal(summary$table, index$table)
  expect_equal(summary$covariance, index$covariance)
})
