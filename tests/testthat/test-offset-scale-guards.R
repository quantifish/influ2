offset_scale_data <- function() {
  set.seed(717)
  data <- data.frame(
    year = factor(rep(1:4, each = 40)), x = stats::rnorm(160),
    exposure = stats::runif(160, 0.5, 3)
  )
  data$y <- stats::rpois(160, data$exposure *
    exp(0.5 + 0.2 * as.numeric(data$year) + 0.3 * data$x))
  data
}

test_that("offset detection distinguishes calls, formulas, and zero defaults", {
  expect_length(.influ_offset_sources(y ~ year + offset(log(e))), 1L)
  expect_length(.influ_offset_sources(y ~ year + stats::offset(log(e))), 1L)
  expect_length(.influ_offset_sources(y ~ year + offset), 0L)
  expect_length(.influ_offset_sources(y ~ year + rate + resp_rate), 0L)
  expect_length(.influ_offset_sources(y ~ year, quote(glm(offset = log(e)))), 1L)
  expect_length(.influ_offset_sources(y ~ year, quote(glm(offset = NULL))), 0L)
  expect_length(.influ_offset_sources(stored = rep(0, 4)), 0L)
  expect_length(.influ_offset_sources(stored = rep(2, 4)), 1L)
})

test_that("GLM log ratios retain offset fits without estimating offset terms", {
  data <- offset_scale_data()
  formula_fit <- stats::glm(y ~ year + x + offset(log(exposure)),
    family = stats::poisson(), data = data)
  call_fit <- stats::glm(y ~ year + x, offset = log(exposure),
    family = stats::poisson(), data = data)
  reference <- data[seq(1L, 160L, by = 4L), , drop = FALSE]
  for (grid in list(NULL, reference)) {
    a <- influ(formula_fit, "year", uncertainty = "none", reference_data = grid)
    b <- influ(call_fit, "year", uncertainty = "none", reference_data = grid)
    expect_equal(a$influence, b$influence)
    expect_setequal(unique(a$influence$term), c("year", "x"))
    effect <- subset(a$influence, term == "x" & scale == "ratio")
    delta <- vapply(effect$level, function(level) {
      mean(data$x[data$year == level]) - mean((grid %||% data)$x)
    }, numeric(1))
    expect_equal(effect$estimate, unname(exp(stats::coef(formula_fit)["x"] * delta)))
    expect_true(a$metadata$offset$detected)
    expect_match(a$metadata$nominal_scope, "not exposure-adjusted CPUE")
    nominal <- subset(a$indices, series == "nominal")
    expect_equal(nominal$estimate, as.numeric(tapply(data$y, data$year, mean)))
  }
})

test_that("identity differences cancel fixed offsets but probability effects do not", {
  data <- offset_scale_data()
  identity_fit <- stats::glm(y ~ year + x + offset(exposure),
    family = stats::gaussian(), data = data)
  diagnostic <- influ(identity_fit, "year", uncertainty = "none")
  effect <- subset(diagnostic$influence, term == "x" & scale == "difference")
  delta <- vapply(effect$level, function(level) {
    mean(data$x[data$year == level]) - mean(data$x)
  }, numeric(1))
  expect_equal(effect$estimate, unname(stats::coef(identity_fit)["x"] * delta))
  data$binary <- rep(c(0, 1, 1, 0, 1), length.out = nrow(data))
  for (link in c("logit", "probit", "cloglog")) {
    for (constant in c(FALSE, TRUE)) {
      data$fixed <- if (constant) 0.4 else log(data$exposure)
      fit <- stats::glm(binary ~ year + x, offset = fixed,
        family = stats::binomial(link), data = data)
      expect_error(influ(fit, "year"), "offset-aware reference calculation")
    }
  }
})

test_that("GAMs use the same safe offset boundary", {
  skip_if_not_installed("mgcv")
  data <- offset_scale_data()
  model <- mgcv::gam(y ~ year + s(x, k = 4) + offset(log(exposure)),
    family = stats::poisson(), data = data)
  diagnostic <- influ(model, "year", uncertainty = "none")
  expect_true(diagnostic$metadata$offset$detected)
  expect_false(any(grepl("offset", diagnostic$influence$term)))
  data$binary <- rep(c(0, 1, 1, 0, 1), length.out = nrow(data))
  probability <- mgcv::gam(binary ~ year + s(x, k = 4), offset = log(exposure),
    family = stats::binomial(), data = data)
  expect_error(influ(probability, "year"), "offset-aware reference calculation")
})

test_that("glmmTMB checks conditional, probability, and dispersion offsets", {
  skip_if_not_installed("glmmTMB")
  data <- offset_scale_data()
  conditional <- glmmTMB::glmmTMB(y ~ year + x + offset(log(exposure)),
    family = stats::poisson(), data = data)
  diagnostic <- influ(conditional, "year", uncertainty = "none")
  expect_true(diagnostic$metadata$offset$detected)
  expect_setequal(unique(diagnostic$influence$term), c("year", "x"))
  data$y[seq(1L, 160L, by = 4L)] <- 0
  for (zi_offset in c(FALSE, TRUE)) {
    model <- glmmTMB::glmmTMB(
      if (zi_offset) y ~ year + x else y ~ year + x + offset(log(exposure)),
      ziformula = if (zi_offset) ~ year + offset(log(exposure)) else ~ year,
      family = stats::poisson(), data = data
    )
    expect_error(influ(model, "year"), "hurdle/zero-inflated")
  }
  data$positive <- exp(0.2 + 0.1 * as.numeric(data$year) + data$x / 3)
  constant <- glmmTMB::glmmTMB(positive ~ year, family = glmmTMB::lognormal(),
    data = data)
  expect_s3_class(influ(constant, "year", uncertainty = "none"), "influ_diag")
  varying <- glmmTMB::glmmTMB(positive ~ year, dispformula = ~ x,
    family = glmmTMB::lognormal(), data = data)
  varying_diag <- influ(varying, "year", uncertainty = "none")
  expect_s3_class(varying_diag, "influ_diag")
  expect_match(varying_diag$metadata$lognormal$scope, "dispersion effects are not decomposed")
  effect <- subset(varying_diag$influence, term == "year" & scale == "ratio")
  fitted_mean <- stats::predict(varying, type = "response")
  relative_mean <- exp(as.numeric(tapply(log(fitted_mean), data$year, mean)) -
    mean(log(fitted_mean)))
  expect_equal(effect$estimate, relative_mean)
  identity_fit <- glmmTMB::glmmTMB(positive ~ year,
    family = glmmTMB::lognormal(link = "identity"), data = data)
  expect_error(influ(identity_fit, "year"), "log link for the arithmetic mean")
})

test_that("BRMS offset and sigma guards run before posterior extraction", {
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  data <- offset_scale_data()
  data$positive <- exp(data$x)
  check_formula <- function(formula, pattern) {
    model <- structure(list(formula = formula, family = formula$family,
      data = data), class = "brmsfit")
    expect_error(influ(model, "year"), pattern)
  }
  check_formula(brms::bf(y ~ year + offset(log(exposure)), hu ~ year,
    family = brms::hurdle_poisson()), "hurdle/zero-inflated")
  check_formula(brms::bf(y ~ year, hu ~ year + offset(log(exposure)),
    family = brms::hurdle_poisson()), "hurdle/zero-inflated")
  check_formula(brms::bf(positive ~ year, sigma ~ x,
    family = brms::lognormal()), "varying sigma/dispersion")
  check_formula(brms::bf(positive ~ year, sigma ~ (1 | year),
    family = brms::lognormal()), "varying sigma/dispersion")
  check_formula(brms::bf(positive ~ year, sigma ~ offset(x),
    family = brms::lognormal()), "varying sigma/dispersion")
  nonlinear <- brms::bf(positive ~ year, family = brms::lognormal()) +
    brms::nlf(sigma ~ exp(a)) + brms::lf(a ~ x)
  check_formula(nonlinear, "varying sigma/dispersion")
  check_formula(brms::bf(positive ~ year, family = brms::lognormal(link = "inverse")),
    "identity link for mu")

  for (formula in list(
    brms::bf(positive ~ year, family = brms::lognormal()),
    brms::bf(positive ~ year, sigma ~ 1, family = brms::lognormal()),
    brms::bf(positive ~ year, sigma = 0.5, family = brms::lognormal()))) {
    parsed <- brms::brmsterms(formula)
    expect_silent(.check_influ_lognormal_scale(
      .new_family_spec("lognormal", "identity"), parsed$dpars$sigma$formula))
  }
  rate_formula <- brms::bf(y | rate(exposure) ~ year + x, family = stats::poisson())
  standata <- brms::make_standata(rate_formula, data = data)
  expect_equal(as.numeric(standata$denom), data$exposure)
  sources <- .influ_offset_sources(rate_formula$formula)
  expect_length(sources, 1L)
  expect_silent(.check_influ_offset_scope(.new_family_spec("poisson", "log"), sources))
})

test_that("spatial backend entry points apply the offset guard", {
  skip_if_not_installed("sdmTMB")
  data <- offset_scale_data()
  model <- sdmTMB::sdmTMB(y ~ year + x, data = data, spatial = "off",
    offset = log(data$exposure), family = stats::poisson())
  diagnostic <- influ(model, "year", uncertainty = "none")
  expect_true(diagnostic$metadata$offset$detected)
  expect_false(any(grepl("offset", diagnostic$influence$term)))
  model$family <- sdmTMB::lognormal(link = "identity")
  expect_error(influ(model, "year"), "log link for the arithmetic mean")
  # Only the family declaration is changed: this deliberately incomplete
  # object must fail at the entry guard, before matrix/field calculations.
  model$family <- sdmTMB::delta_gamma()
  expect_error(influ(model, "year"), "hurdle/zero-inflated")
})

test_that("tinyVAST handles fixed offsets and rejects delta reference offsets", {
  skip_if_not_installed("tinyVAST")
  data <- offset_scale_data()
  data$var <- "y"
  data$dist <- "poisson"
  model <- tinyVAST::tinyVAST(y ~ year + x + offset(log(exposure)), data = data,
    family = list(poisson = stats::poisson()), spatial_domain = NULL)
  diagnostic <- influ(model, "year", uncertainty = "none")
  expect_true(diagnostic$metadata$offset$detected)
  expect_false(any(grepl("offset", diagnostic$influence$term)))
  model$internal$family <- list(poisson = tinyVAST::lognormal(link = "identity"))
  expect_error(influ(model, "year"), "log link for the arithmetic mean")
  model$internal$family <- list(poisson = tinyVAST::delta_gamma())
  expect_error(influ(model, "year"), "hurdle/zero-inflated")
  model$internal$gam_setup$offset[] <- 0
  model$tmb_inputs$tmb_data$offset_i[] <- 0
  model$formula <- y ~ year + x
  model$internal$delta_formula <- ~ year + offset(log(exposure))
  expect_error(influ(model, "year"), "hurdle/zero-inflated")
})
