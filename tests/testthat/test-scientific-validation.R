test_that("GLM term contrasts and analytic uncertainty match direct algebra", {
  fixture <- bentley_fixture()
  model <- fixture$model
  data <- fixture$data
  weights <- seq_len(nrow(data))
  diagnostic <- influ(model, focus = "year", weights = weights)

  X <- stats::model.matrix(model)
  term_index <- match("area", attr(stats::terms(model), "term.labels"))
  columns <- which(attr(X, "assign") == term_index)
  levels <- levels(data$year)
  overall <- colSums(X[, columns, drop = FALSE] * weights) / sum(weights)
  B_term <- do.call(rbind, lapply(levels, function(level) {
    keep <- data$year == level
    colSums(X[keep, columns, drop = FALSE] * weights[keep]) /
      sum(weights[keep]) - overall
  }))
  B <- matrix(0, nrow = length(levels), ncol = ncol(X))
  B[, columns] <- B_term
  expected <- as.numeric(B %*% stats::coef(model))
  expected_se <- sqrt(diag(B %*% stats::vcov(model) %*% t(B)))

  observed <- subset(
    diagnostic$influence,
    term == "area" & scale == "link"
  )
  expect_equal(observed$estimate, expected, tolerance = 1e-12)
  expect_equal(observed$std_error, expected_se, tolerance = 1e-12)
})

test_that("joint coefficient simulation converges to analytic uncertainty", {
  fixture <- bentley_fixture()
  analytic <- influ(fixture$model, focus = "year", uncertainty = "analytic")
  simulated <- influ(
    fixture$model,
    focus = "year",
    uncertainty = "simulation",
    ndraws = 10000,
    seed = 938
  )
  analytic <- subset(
    analytic$influence,
    term == "area" & scale == "link"
  )
  simulated <- subset(
    simulated$influence,
    term == "area" & scale == "link"
  )

  expect_equal(simulated$estimate, analytic$estimate, tolerance = 0.01)
  expect_equal(simulated$std_error, analytic$std_error, tolerance = 0.01)
})

test_that("equivalent GLM, GAM, and glmmTMB fixed terms agree", {
  fixture <- bentley_fixture()
  glm_fit <- fixture$model

  skip_if_not_installed("mgcv")
  gam_fit <- mgcv::gam(
    catch ~ year + area + vessel,
    family = stats::poisson(link = "log"),
    data = fixture$data
  )
  glm_rows <- subset(
    influ(glm_fit, focus = "year", uncertainty = "none")$influence,
    scale == "link"
  )
  gam_rows <- subset(
    influ(gam_fit, focus = "year", uncertainty = "none")$influence,
    scale == "link"
  )
  comparison <- merge(
    glm_rows[c("level", "term", "estimate")],
    gam_rows[c("level", "term", "estimate")],
    by = c("level", "term")
  )
  expect_equal(comparison$estimate.x, comparison$estimate.y, tolerance = 1e-7)

  skip_if_not_installed("glmmTMB")
  mixed_fit <- glmmTMB::glmmTMB(
    catch ~ year + area + vessel,
    family = stats::poisson(link = "log"),
    data = fixture$data
  )
  mixed_rows <- subset(
    influ(mixed_fit, focus = "year", uncertainty = "none")$influence,
    component == "conditional" & scale == "link"
  )
  comparison <- merge(
    glm_rows[c("level", "term", "estimate")],
    mixed_rows[c("level", "term", "estimate")],
    by = c("level", "term")
  )
  expect_equal(comparison$estimate.x, comparison$estimate.y, tolerance = 1e-5)
})

test_that("natural-scale transformations cover all supported links", {
  eta <- 0.35
  delta <- c(-0.2, 0, 0.3)
  inverse <- list(
    identity = identity,
    log = exp,
    logit = stats::plogis,
    probit = stats::pnorm,
    cloglog = function(x) 1 - exp(-exp(x))
  )

  for (link in names(inverse)) {
    spec <- influ2:::.new_family_spec(
      if (link == "log") "poisson" else "binomial",
      link
    )
    observed <- influ2:::.effect_transform(delta, spec, eta)
    expected <- if (spec$natural_scale == "ratio") {
      exp(delta)
    } else {
      inverse[[link]](eta + delta) - inverse[[link]](eta)
    }
    expect_equal(observed, expected, tolerance = 1e-14)
  }
})

test_that("weight and reference-grid validation fails explicitly", {
  fixture <- bentley_fixture()
  model <- fixture$model
  reference <- fixture$data[1:4, , drop = FALSE]

  expect_error(influ(model, focus = "year", weights = -1), "one value")
  expect_error(
    influ(model, focus = "year", weights = rep(-1, nrow(fixture$data))),
    "non-negative"
  )
  expect_error(
    influ(model, focus = "year", reference_data = reference,
      reference_weights = c(1, 2)),
    "one value"
  )
  expect_error(influ(model, focus = "missing"), "must name one column")
  expect_error(
    influ(model, focus = "year", uncertainty = "simulation", ndraws = 0),
    "positive whole number"
  )
  expect_error(
    influ(model, focus = "year", probs = c(NA_real_, 0.9)),
    "increasing probabilities"
  )
})

test_that("numeric focus levels are ordered independently of row order", {
  set.seed(102)
  data <- data.frame(
    year = rep(c(2003, 2001, 2002), each = 30),
    x = stats::rnorm(90)
  )
  data$catch <- stats::rpois(
    nrow(data),
    exp(0.2 + 0.03 * (data$year - 2001) + 0.1 * data$x)
  )
  model <- stats::glm(
    catch ~ year + x,
    family = stats::poisson(),
    data = data
  )
  diagnostic <- influ(model, focus = "year")

  expect_identical(unique(diagnostic$influence$level), c("2001", "2002", "2003"))
  expect_identical(unique(diagnostic$indices$level), c("2001", "2002", "2003"))
})

test_that("analytic intervals honour asymmetric probabilities", {
  fixture <- bentley_fixture()
  probs <- c(0.1, 0.8)
  diagnostic <- influ(fixture$model, focus = "year", probs = probs)
  observed <- subset(
    diagnostic$influence,
    term == "area" & scale == "link"
  )

  expect_equal(
    observed$lower,
    observed$estimate + stats::qnorm(probs[1]) * observed$std_error,
    tolerance = 1e-12
  )
  expect_equal(
    observed$upper,
    observed$estimate + stats::qnorm(probs[2]) * observed$std_error,
    tolerance = 1e-12
  )
})

test_that("nominal index uncertainty uses weights and requested probabilities", {
  fixture <- bentley_fixture()
  weights <- seq_len(nrow(fixture$data))
  probs <- c(0.1, 0.8)
  diagnostic <- influ(
    fixture$model,
    focus = "year",
    weights = weights,
    probs = probs
  )
  observed <- subset(diagnostic$indices, series == "nominal")
  keep <- fixture$data$year == observed$level[1]
  y <- fixture$data$catch[keep]
  w <- weights[keep]
  weight_sum <- sum(w)
  squared_weight_sum <- sum(w^2)
  effective_n <- weight_sum^2 / squared_weight_sum
  estimate <- stats::weighted.mean(y, w)
  variance <- sum(w * (y - estimate)^2) /
    (weight_sum - squared_weight_sum / weight_sum)
  se <- sqrt(variance / effective_n)

  expect_equal(observed$estimate[1], estimate, tolerance = 1e-12)
  expect_equal(observed$std_error[1], se, tolerance = 1e-12)
  expect_equal(observed$lower[1], estimate + stats::qnorm(probs[1]) * se)
  expect_equal(observed$upper[1], estimate + stats::qnorm(probs[2]) * se)
})

test_that("zero total weight within a focus level fails explicitly", {
  fixture <- bentley_fixture()
  weights <- rep(1, nrow(fixture$data))
  weights[fixture$data$year == levels(fixture$data$year)[1]] <- 0

  expect_error(
    influ(fixture$model, focus = "year", weights = weights),
    "Every focus level must have a positive total weight"
  )
})

test_that("ambiguous focus interactions do not create a partial index", {
  fixture <- bentley_fixture()
  model <- stats::glm(
    catch ~ year * area + vessel,
    family = stats::poisson(),
    data = fixture$data
  )

  expect_warning(
    diagnostic <- influ(model, focus = "year"),
    "multiple model terms contain the focus variable"
  )
  expect_identical(unique(diagnostic$indices$series), "nominal")
})

test_that("two-part preview does not retain a synthetic draw", {
  data <- data.frame(
    y = c(0, 1, 2, 3),
    year = factor(c(1, 1, 2, 2))
  )
  projection <- function(spec, delta) {
    list(
      family_spec = spec,
      eta_reference = 0,
      term_deltas = list(`as.factor(year)` = matrix(delta, nrow = 1)),
      reference = "observed",
      method = "none"
    )
  }
  probability_spec <- influ2:::.new_family_spec("binomial", "logit")
  main_spec <- influ2:::.new_family_spec("gamma", "log")
  diagnostic <- influ2:::.two_part_combined_diag(
    backend = "test",
    model = NULL,
    data = data,
    response = "y",
    focus = "year",
    family_spec = main_spec,
    main_projection = projection(main_spec, c(-0.1, 0.1)),
    probability_projection = projection(probability_spec, c(-0.2, 0.2)),
    probability_is_zero = FALSE,
    retain = "derived_draws"
  )

  expect_null(influ_draws(diagnostic))
  expect_identical(diagnostic$retained$n_derived_draws, 0L)
  expect_identical(diagnostic$retained$n_derived_estimands, 0L)
  expect_true("standardised" %in% diagnostic$indices$series)
})

test_that("glmmTMB random effects retain joint simulated uncertainty", {
  skip_if_not_installed("glmmTMB")
  set.seed(73)
  data <- data.frame(
    year = factor(rep(1:4, each = 40)),
    vessel = factor(rep(1:20, each = 8)),
    x = stats::rnorm(160)
  )
  vessel_effect <- stats::rnorm(20, sd = 0.35)
  data$catch <- stats::rpois(
    nrow(data),
    exp(0.4 + 0.1 * as.numeric(data$year) + 0.2 * data$x +
      vessel_effect[data$vessel])
  )
  model <- glmmTMB::glmmTMB(
    catch ~ year + x + (1 | vessel),
    family = stats::poisson(),
    data = data
  )
  diagnostic <- influ(
    model,
    focus = "year",
    uncertainty = "simulation",
    retain = "derived_draws",
    ndraws = 60,
    seed = 9
  )
  random_rows <- subset(
    diagnostic$influence,
    component == "random_effects"
  )

  expect_true(all(random_rows$method == "joint coefficient simulation"))
  expect_true(all(is.finite(random_rows$std_error)))
  expect_equal(nrow(influ_draws(diagnostic)), 60)
})
