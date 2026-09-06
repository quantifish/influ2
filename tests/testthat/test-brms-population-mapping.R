test_that("BRMS fixed-term labels exclude intervening random and smooth terms", {
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  fit <- readRDS(system.file(
    "extdata", "brms-fixtures", "fit2.rds", package = "influ2"
  ))
  observed <- .brms_population_matrix(fit)
  expect_identical(names(observed$term_columns), c("year", "soak"))
  expect_identical(colnames(observed$X)[observed$term_columns$soak], "soak")
  expect_equal(unname(observed$X[, observed$term_columns$soak]), fit$data$soak)

  reference <- fit$data[seq(1, nrow(fit$data), length.out = 50), , drop = FALSE]
  reference$soak <- seq(12, 72, length.out = nrow(reference))
  grid <- .brms_population_matrix(fit, newdata = reference)
  expect_identical(grid$term_columns, observed$term_columns)
  expect_equal(unname(grid$X[, grid$term_columns$soak]), reference$soak)

  ndraws <- 40L
  slope_draws <- as.numeric(.brms_draw_matrix(fit, "b_soak", ndraws))
  for (explicit in c(FALSE, TRUE)) {
    diagnostic <- influ(fit, focus = "year", ndraws = ndraws,
      reference_data = if (explicit) reference else NULL)
    conditional <- subset(diagnostic$influence, component == "conditional")
    expect_setequal(unique(conditional$term), c("year", "soak"))
    expect_false(any(grepl("\\|", conditional$term)))
    effect <- subset(conditional, term == "soak" & scale == "link")
    reference_mean <- mean(if (explicit) reference$soak else fit$data$soak)
    delta <- vapply(effect$level, function(year) {
      mean(fit$data$soak[fit$data$year == year]) - reference_mean
    }, numeric(1))
    projected <- outer(slope_draws, delta)
    expect_equal(effect$estimate, unname(colMeans(projected)))
    expect_equal(effect$lower, unname(apply(projected, 2L, stats::quantile, 0.025)))
    expect_equal(effect$upper, unname(apply(projected, 2L, stats::quantile, 0.975)))
    ratio <- subset(conditional, term == "soak" & scale == "ratio")
    expect_equal(ratio$estimate, unname(colMeans(exp(projected))))
  }
})

test_that("BRMS hurdle dpar labels use their own population-only formulas", {
  skip_if_not_installed("brms")
  set.seed(308)
  data <- data.frame(year = factor(rep(1:4, each = 20)),
    month = factor(rep(1:5, 16)), depth = stats::runif(80, 5, 40),
    soak = stats::runif(80, 12, 72), x = stats::rnorm(80))
  data$y <- ifelse(seq_len(80) %% 4 == 0, 0, exp(stats::rnorm(80)))
  formula <- brms::bf(
    y ~ year + (1 | month) + s(depth, k = 3) + soak + x,
    hu ~ year + (1 | month) + s(depth, k = 3) + x + soak,
    family = brms::hurdle_lognormal()
  )
  model <- list(formula = formula, data = data)
  reference_data <- data[1:30, , drop = FALSE]
  observed_standata <- brms::make_standata(formula, data = data)
  reference_standata <- brms::make_standata(formula, data = reference_data)
  testthat::local_mocked_bindings(standata = function(object, newdata = NULL, ...) {
    if (is.null(newdata)) observed_standata else reference_standata
  }, .package = "brms")
  for (reference in list(NULL, reference_data)) {
    mu <- .brms_population_matrix(model, newdata = reference)
    hu <- .brms_population_matrix(model, dpar = "hu", newdata = reference)
    expect_identical(names(mu$term_columns), c("year", "soak", "x"))
    expect_identical(names(hu$term_columns), c("year", "x", "soak"))
    expect_identical(colnames(mu$X)[mu$term_columns$soak], "soak")
    expect_identical(colnames(hu$X)[hu$term_columns$soak], "soak")
    expect_equal(unname(hu$X[, hu$term_columns$soak]), (reference %||% data)$soak)
  }
})

test_that("BRMS invalid population assignment metadata fails rather than mislabelling", {
  skip_if_not_installed("brms")
  model <- list(formula = brms::bf(y ~ year + (1 | month) + x,
    family = stats::gaussian()))
  matrix <- matrix(1, nrow = 3, ncol = 2)
  testthat::local_mocked_bindings(standata = function(...) list(X = matrix), .package = "brms")
  expect_error(.brms_population_matrix(model), "assignments do not match")
  attr(matrix, "assign") <- c(0, 3)
  expect_error(.brms_population_matrix(model), "assignments do not match")
  attr(matrix, "assign") <- 0L
  expect_error(.brms_population_matrix(model), "assignments do not match")
})
