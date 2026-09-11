test_that("new residuals and indices explain missing optional model packages", {
  local_mocked_bindings(.model_backend_available = function(package) FALSE)
  for (model_class in c("glmmTMB", "sdmTMB", "tinyVAST", "brmsfit", "gam")) {
    model <- structure(list(), class = model_class)
    package <- switch(model_class, brmsfit = "brms", gam = "mgcv", model_class)
    message <- paste0("Package '", package, "' is required")
    expect_error(influ_residuals(model, nsim = 20), message, fixed = TRUE)
    expect_error(cpue_index(model, year = "year",
      reference_data = data.frame(area = "A")), message, fixed = TRUE)
    expect_error(integrate_index(model, year = "year",
      reference_data = data.frame(area = "A"), area = 1,
      area_units = "km^2", response_units = "individuals/km^2"), message, fixed = TRUE)
  }
})

test_that("backend requirements do not affect GLMs or saved compact results", {
  local_mocked_bindings(.model_backend_available = function(package) {
    stop("A compact result or GLM must not look up a model backend")
  })
  fixture <- bentley_fixture()
  diagnostic <- influ(fixture$model, focus = "year")
  expect_null(.require_model_backend(fixture$model))
  expect_null(.require_model_backend(diagnostic))
  expect_s3_class(cpue_index(diagnostic, year = "year", method = "year_effect"), "influ_index")
})

test_that("available native backends pass the early requirement guard", {
  seen <- character()
  local_mocked_bindings(.model_backend_available = function(package) {
    seen <<- c(seen, package)
    TRUE
  })
  for (model_class in c("glmmTMB", "sdmTMB", "tinyVAST", "brmsfit", "gam")) {
    expect_null(.require_model_backend(structure(list(), class = model_class)))
  }
  expect_identical(seen, c("glmmTMB", "sdmTMB", "tinyVAST", "brms", "mgcv"))
})
