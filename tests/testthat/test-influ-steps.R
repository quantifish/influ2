step_year_rows <- function(diagnostic, component = "conditional") {
  out <- diagnostic$indices[
    diagnostic$indices$series == "standardised" &
      diagnostic$indices$component == component,
    , drop = FALSE
  ]
  rownames(out) <- NULL
  out
}

test_that("supplied fits produce their existing year-effect contrasts without refitting", {
  fixture <- bentley_fixture()
  reduced <- stats::update(fixture$model, . ~ year + area, data = fixture$data)
  fits <- list(Reduced = reduced, Full = fixture$model)
  expected <- lapply(fits, function(fit) step_year_rows(influ(fit, focus = "year")))
  # A refit would evaluate these calls and fail; diagnostics still have all
  # fitted coefficients, matrices, and covariance needed for extraction.
  for (i in seq_along(fits)) fits[[i]]$call <- quote(stop("Unexpected model refit"))
  steps <- influ_steps(fits, year = "year")

  expect_s3_class(steps, "influ_steps")
  expect_identical(steps$metadata$estimand, "year_effect_contrast")
  expect_identical(steps$focus, "year")
  expect_null(steps$fits)
  expect_false(any(steps$steps$refitted))
  for (i in seq_along(expected)) {
    actual <- steps$indices[steps$indices$step_id == steps$steps$step_id[i], names(expected[[i]]), drop = FALSE]
    rownames(actual) <- NULL
    expect_equal(actual, expected[[i]])
  }

  # Verify the scientific quantity independently: exponentiated centred
  # year coefficients, rather than a grid-integrated response or abundance.
  X <- stats::model.matrix(fixture$model)
  year_columns <- which(attr(X, "assign") == 1L)
  contribution <- as.numeric(X[, year_columns, drop = FALSE] %*%
    stats::coef(fixture$model)[year_columns])
  contrast <- tapply(contribution, fixture$data$year, mean) - mean(contribution)
  actual <- steps$indices[steps$indices$step_id == steps$steps$step_id[2], ]
  expect_equal(actual$estimate, as.numeric(exp(contrast[actual$level])))
})

test_that("explicit refit stages agree with separately fitted GLM stages", {
  fixture <- bentley_fixture()
  stages <- list(Year = ~year, Area = ~year + area, Vessel = ~year + area + vessel)
  sequence <- influ_steps(
    fixture$model, year = "year", steps = stages, refit = TRUE,
    probs = c(0.1, 0.9), keep_fits = TRUE
  )
  manual <- lapply(stages, function(formula) {
    stats::glm(
      stats::update.formula(stats::formula(fixture$model), formula),
      family = stats::poisson(link = "log"), data = fixture$data
    )
  })

  expect_length(sequence$fits, length(stages))
  expect_equal(nrow(sequence$steps), length(stages))
  for (i in seq_along(stages)) {
    expected <- step_year_rows(influ(manual[[i]], focus = "year", probs = c(0.1, 0.9)))
    actual <- sequence$indices[sequence$indices$step_id == sequence$steps$step_id[i], names(expected), drop = FALSE]
    rownames(actual) <- NULL
    expect_equal(actual, expected, tolerance = 1e-8)
  }
})

test_that("supplied fit contrasts use fitted years after the source data changes", {
  fixture <- bentley_fixture()
  data <- fixture$data
  models <- list(
    Reduced = stats::glm(catch ~ year + area,
      family = stats::poisson(link = "log"), data = data),
    Full = stats::glm(catch ~ year + area + vessel,
      family = stats::poisson(link = "log"), data = data)
  )
  expected <- lapply(models, function(model) step_year_rows(influ(model, focus = "year")))
  data$year <- rev(data$year)
  single <- influ_steps(models[[2L]], year = "year")
  actual <- single$indices[names(expected[[2L]])]
  rownames(actual) <- NULL
  expect_equal(actual, expected[[2L]])

  multiple <- influ_steps(models, year = "year")
  for (i in seq_along(models)) {
    actual <- multiple$indices[multiple$indices$step_id == multiple$steps$step_id[i], names(expected[[i]]), drop = FALSE]
    rownames(actual) <- NULL
    expect_equal(actual, expected[[i]])
  }
  expect_false(any(multiple$steps$refitted))
})

test_that("supplied fits reject changed transformed years they cannot reconstruct", {
  fixture <- bentley_fixture()
  data <- fixture$data
  data$year <- as.numeric(as.character(data$year))
  model <- stats::glm(catch ~ factor(year) + area + vessel,
    family = stats::poisson(link = "log"), data = data)
  data$year <- rev(data$year)
  expect_error(influ_steps(model, year = "year"), "source data|reproduce")
})

test_that("automatic GLM stages reach the fitted full-model year effect", {
  fixture <- bentley_fixture()
  steps <- influ_steps(fixture$model, year = "year", refit = TRUE)
  expected <- step_year_rows(influ(fixture$model, focus = "year"))
  final_id <- tail(steps$steps$step_id, 1L)
  actual <- steps$indices[steps$indices$step_id == final_id, names(expected), drop = FALSE]
  rownames(actual) <- NULL

  expect_equal(actual, expected, tolerance = 1e-8)
  expect_null(steps$fits)
})

test_that("refitted steps retain the full model's complete-case observation set", {
  fixture <- bentley_fixture()
  data <- fixture$data
  data$x <- seq_len(nrow(data)) / nrow(data)
  data$x[seq.int(1L, nrow(data), by = 7L)] <- NA_real_
  full <- stats::glm(
    catch ~ year + area + x,
    family = stats::poisson(link = "log"), data = data, na.action = stats::na.exclude
  )
  used <- stats::model.frame(full)
  baseline <- stats::glm(
    catch ~ year, family = stats::poisson(link = "log"), data = used
  )
  steps <- influ_steps(
    full, year = "year", refit = TRUE,
    steps = list(Year = ~year, Full = ~year + area + x), keep_fits = TRUE
  )
  expected <- step_year_rows(influ(baseline, focus = "year"))
  actual <- steps$indices[steps$indices$step_id == steps$steps$step_id[1], names(expected), drop = FALSE]
  rownames(actual) <- NULL

  expect_equal(actual, expected, tolerance = 1e-8)
  expect_true(all(vapply(steps$fits, stats::nobs, numeric(1)) == stats::nobs(full)))
})

test_that("refit steps cannot silently use data changed after fitting", {
  fixture <- bentley_fixture()
  data <- fixture$data
  fitted <- stats::glm(
    catch ~ year + area + vessel,
    family = stats::poisson(link = "log"), data = data
  )
  original_frame <- stats::model.frame(fitted)
  baseline <- stats::glm(
    catch ~ year, family = stats::poisson(link = "log"), data = original_frame
  )
  data$catch <- data$catch + 1
  steps <- influ_steps(fitted, year = "year", refit = TRUE, keep_fits = TRUE)
  expected <- step_year_rows(influ(baseline, focus = "year"))
  actual <- steps$indices[steps$indices$step_id == steps$steps$step_id[1], names(expected), drop = FALSE]
  rownames(actual) <- NULL
  expect_equal(actual, expected, tolerance = 1e-8)
  expect_true(all(vapply(steps$fits, function(fit) {
    frame <- stats::model.frame(fit)
    identical(unname(stats::model.response(frame)), original_frame$catch) &&
      identical(rownames(frame), rownames(original_frame))
  }, logical(1))))
})

test_that("unrecoverable transformed source changes stop refitting", {
  fixture <- bentley_fixture()
  data <- fixture$data
  data$x <- 1 + seq_len(nrow(data)) / nrow(data)
  fitted <- stats::glm(
    catch ~ year + log(x), family = stats::poisson(link = "log"), data = data
  )
  data$x <- data$x * 2
  expect_error(influ_steps(fitted, year = "year", refit = TRUE), "source data|reproduce")
})

test_that("precomputed step plots reuse stored indices and intervals", {
  fixture <- bentley_fixture()
  diagnostic <- influ(fixture$model, focus = "year", probs = c(0.05, 0.85))
  prepared <- influ_steps(list(Full = diagnostic))
  expected <- step_year_rows(diagnostic)
  actual <- prepared$indices[names(expected)]
  rownames(actual) <- NULL
  expect_equal(actual, expected)
  expect_equal(influ_indices(prepared), prepared$indices)

  plotting <- plot_step(prepared)
  expect_s3_class(plotting, "ggplot")
  current <- plotting$data[plotting$data$Step == "Current", names(expected), drop = FALSE]
  rownames(current) <- NULL
  expect_equal(current, expected)
  expect_s3_class(plot(prepared), "ggplot")
  expect_s3_class(ggplot2::autoplot(prepared), "ggplot")
})

test_that("step labels and observation support cannot silently combine distinct fits", {
  fixture <- bentley_fixture()
  diagnostic <- influ(fixture$model, focus = "year")
  expect_error(
    influ_steps(list(diagnostic, diagnostic), labels = c("Same", "Same")),
    "unique|duplicat"
  )
  changed_data <- fixture$data
  changed_data$catch <- rev(changed_data$catch)
  changed_model <- stats::glm(
    catch ~ year + area + vessel,
    family = stats::poisson(link = "log"), data = changed_data
  )
  expect_error(
    influ_steps(list(Original = fixture$model, Changed = changed_model)),
    "data|response|observation|sample"
  )
})

test_that("step sequences reject incompatible focus, years, scales, and references", {
  fixture <- bentley_fixture()
  diagnostic <- influ(fixture$model, focus = "year")
  other_focus <- diagnostic
  other_focus$focus <- "period"
  other_focus$indices$focus <- "period"
  expect_error(influ_steps(list(diagnostic, other_focus)), "focus|year")

  other_years <- diagnostic
  other_years$indices$level[other_years$indices$level == other_years$indices$level[1]] <- "other"
  expect_error(influ_steps(list(diagnostic, other_years)), "level|year|support")

  gaussian <- stats::glm(
    catch ~ year + area + vessel, family = stats::gaussian(), data = fixture$data
  )
  expect_error(influ_steps(list(fixture$model, gaussian)), "scale|link|comparab")

  reference <- fixture$data[seq.int(1L, nrow(fixture$data), by = 3L), , drop = FALSE]
  grid <- influ(fixture$model, focus = "year", reference_data = reference)
  expect_error(influ_steps(list(diagnostic, grid)), "reference")
})

test_that("multiple component indices require an explicit choice", {
  fixture <- bentley_fixture()
  diagnostic <- influ(fixture$model, focus = "year")
  diagnostic$family$structure <- "hurdle"
  conditional <- diagnostic$indices
  conditional$component <- "positive"
  unconditional <- diagnostic$indices
  unconditional$component <- "unconditional_mean"
  diagnostic$indices <- rbind(conditional, unconditional)

  expect_error(influ_steps(list(Hurdle = diagnostic)), "component")
  selected <- influ_steps(list(Hurdle = diagnostic), component = "unconditional_mean")
  expect_identical(unique(selected$indices$component), "unconditional_mean")
  expect_equal(selected$indices$estimate, step_year_rows(diagnostic, "unconditional_mean")$estimate)
  expect_error(influ_steps(list(Hurdle = diagnostic), component = "absent"), "component")
})

test_that("supplied sdmTMB delta fits support explicit year-effect components", {
  skip_if_not_installed("sdmTMB")
  set.seed(417)
  n <- 120L
  data <- data.frame(
    year = factor(rep(1:3, each = n / 3)),
    x = stats::rnorm(n), X = stats::runif(n), Y = stats::runif(n)
  )
  eta <- 0.1 * as.integer(data$year) + 0.2 * data$x
  data$catch <- stats::rbinom(n, 1, stats::plogis(0.4 + eta)) *
    stats::rgamma(n, shape = 3, scale = exp(eta) / 3)
  mesh <- sdmTMB::make_mesh(data, c("X", "Y"), n_knots = 10)
  fitted <- sdmTMB::sdmTMB(
    catch ~ year + x, data = data, mesh = mesh,
    family = sdmTMB::delta_gamma(), spatial = "off",
    spatiotemporal = "off", silent = TRUE
  )
  expect_length(fitted$formula, 2L)
  diagnostic <- influ(fitted, focus = "year", uncertainty = "none")
  expected <- step_year_rows(diagnostic, "unconditional_mean")

  single <- influ_steps(fitted, year = "year",
    component = "unconditional_mean", uncertainty = "none")
  actual <- single$indices[names(expected)]
  rownames(actual) <- NULL
  expect_equal(actual, expected)
  expect_false(any(single$steps$refitted))

  repeated <- influ_steps(list(First = fitted, Second = fitted), year = "year",
    component = "unconditional_mean", uncertainty = "none")
  for (id in repeated$steps$step_id) {
    actual <- repeated$indices[repeated$indices$step_id == id, names(expected), drop = FALSE]
    rownames(actual) <- NULL
    expect_equal(actual, expected)
  }
  expect_null(repeated$fits)
})

test_that("Gaussian year-effect differences retain negative values", {
  fixture <- bentley_fixture()
  gaussian <- stats::glm(
    catch ~ year + area + vessel, family = stats::gaussian(), data = fixture$data
  )
  steps <- influ_steps(list(Gaussian = gaussian))
  expect_identical(unique(steps$indices$scale), "difference")
  expect_true(any(steps$indices$estimate < 0))
  plotted <- ggplot2::ggplot_build(plot_step(steps, show_probs = FALSE))
  expect_true(any(vapply(plotted$data, function(layer) {
    "y" %in% names(layer) && any(layer$y < 0, na.rm = TRUE)
  }, logical(1))))
})
