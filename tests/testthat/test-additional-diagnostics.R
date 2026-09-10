test_that("model-neutral comparison and step plots share the index schema", {
  fixture <- bentley_fixture()
  full <- influ(fixture$model, focus = "year")
  reduced_model <- stats::glm(
    catch ~ year + area,
    family = stats::poisson(link = "log"),
    data = fixture$data
  )
  reduced <- influ(reduced_model, focus = "year")

  comparison <- plot_compare(
    list(full, reduced),
    labels = c("Full", "Reduced")
  )
  steps <- plot_step(
    list(reduced, full),
    labels = c("Reduced", "Full")
  )

  expect_s3_class(comparison, "ggplot")
  expect_setequal(unique(comparison$data$Model), c("Full", "Reduced"))
  expect_s3_class(steps, "ggplot")
  expect_setequal(unique(steps$data$Step), c("Current", "Previous"))
})

test_that("comparison labels distinguish identical formulas and different families", {
  fixture <- bentley_fixture()
  poisson <- fixture$model
  gaussian <- stats::glm(
    catch ~ year + area + vessel,
    family = stats::gaussian(link = "log"),
    data = fixture$data
  )

  comparison <- plot_compare(list(poisson, gaussian))

  expect_length(unique(comparison$data$Model), 2)
  expect_match(unique(comparison$data$Model)[1], "poisson\\(log\\)")
  expect_match(unique(comparison$data$Model)[2], "gaussian\\(log\\)")
})

test_that("data extent reports the observed proportion by focus level", {
  data <- data.frame(
    year = rep(1:2, each = 3),
    catch = c(1, NA, 3, NA, NA, 2),
    effort = c(1, 2, 3, 4, NA, 6)
  )
  plot <- plot_data_extent(data, "year", c("catch", "effort"))

  expect_s3_class(plot, "ggplot")
  catch <- plot$data[plot$data$variable == "catch", ]
  expect_equal(catch$proportion, c(2 / 3, 1 / 3))

  unordered <- data.frame(
    year = c(2002, 2000, 2001),
    catch = 1:3
  )
  expect_equal(
    as.character(plot_data_extent(unordered, "year", "catch")$data$time),
    as.character(2000:2002)
  )
  expect_error(
    plot_data_extent(data.frame(year = NA_real_, catch = 1), "year", "catch"),
    "non-missing"
  )
})

test_that("grouped residual means retain the generalised scale", {
  fixture <- bentley_fixture()
  checks <- influ_residuals(fixture$model, data = fixture$data, groups = "area", nsim = 50)
  p <- plot_implied_residuals(checks, groups = "area", min_n = 1)
  first <- p$data[1, ]
  keep <- as.character(checks$observations$year) == first$level &
    as.character(checks$groups$area) == first$group
  r <- checks$observations$residual[keep]
  expect_equal(first$residual, mean(r))
  expect_equal(first$std_error, sd(r) / sqrt(length(r)))
  expect_equal(first$lower, mean(r) - sd(r) / sqrt(length(r)))
  expect_equal(first$upper, mean(r) + sd(r) / sqrt(length(r)))
  expect_error(plot_implied_residuals(checks, groups = "area", min_n = 0), "integer of at least")
})

test_that("bubble plots reject inputs that would silently discard groups", {
  expect_error(plot_bubble(data.frame()), "at least one row")
  missing_group <- data.frame(year = c(1, NA), month = c(1, 2))
  expect_error(plot_bubble(missing_group, c("year", "month")), "missing")
  complete <- data.frame(year = 1:2, month = c("01", "02"))
  expect_error(
    plot_bubble(complete, c("year", "month"), sort_order = "01"),
    "every observed"
  )
  expect_error(
    plot_bubble(complete, c("year", "month"), alpha = 2),
    "between zero and one"
  )
})

test_that("predicted-residual plots and simulation-based Q-Q support GLMs", {
  model <- bentley_fixture()$model
  expect_s3_class(plot_predicted_residuals(model, trend = "none"), "ggplot")
  expect_s3_class(plot_predicted_residuals(model, trend = "lm"), "ggplot")
  expect_s3_class(plot(influ_residuals(model, nsim = 20), type = "qq"), "ggplot")
  expect_error(plot_predicted_residuals(model, trend = "bad"), "should be one of")
})

test_that("brms comparison helpers validate their inputs", {
  expect_error(get_bayes_R2(list(stats::lm(mpg ~ wt, mtcars))), "brmsfit")
  expect_error(table_criterion(list(stats::lm(mpg ~ wt, mtcars))), "brmsfit")
  expect_error(
    table_criterion(list(), criterion = "not-a-criterion"),
    "Unknown"
  )
  expect_error(get_bayes_R2(structure(list(), class = "brmsfit"), probs = c(0.8, 0.2)))
})

test_that("brms comparison helpers summarise public criterion interfaces", {
  skip_if_not_installed("brms")
  make_fit <- function(id, predictor) {
    structure(
      list(
        id = id,
        formula = brms::bf(stats::reformulate(predictor, response = "y")),
        family = brms::brmsfamily("gaussian"),
        criteria = list()
      ),
      class = "brmsfit"
    )
  }
  fits <- list(make_fit(1, "x"), make_fit(2, "z"))
  testthat::local_mocked_bindings(
    bayes_R2 = function(fit, summary = TRUE, ...) {
      draws <- c(0.35, 0.45, 0.55) + 0.1 * (fit$id - 1)
      if (!summary) return(draws)
      matrix(
        c(mean(draws), stats::sd(draws)),
        nrow = 1,
        dimnames = list(NULL, c("Estimate", "Est.Error"))
      )
    },
    loo = function(fit, ...) {
      estimate <- 10 + fit$id
      list(estimates = matrix(
        c(estimate, 1, 2, 0.2, -2 * estimate, 2),
        nrow = 3,
        byrow = TRUE,
        dimnames = list(
          c("elpd_loo", "p_loo", "looic"),
          c("Estimate", "SE")
        )
      ))
    },
    loo_R2 = function(fit, ...) {
      matrix(
        c(0.25 + fit$id / 10, 0.04),
        nrow = 1,
        dimnames = list(NULL, c("Estimate", "Est.Error"))
      )
    },
    log_lik = function(fit, ...) {
      matrix(-fit$id, nrow = 3, ncol = 4)
    },
    .package = "brms"
  )

  r2 <- get_bayes_R2(fits, probs = c(0.25, 0.75))
  expect_equal(r2$R2, c(0.55, 0.45))
  expect_equal(r2$difference, c(0, -0.1))

  criteria <- table_criterion(
    fits,
    criterion = c("loo", "loo_R2", "bayes_R2", "log_lik")
  )
  # Summary tables preserve input order, without ranking unverified fits.
  expect_equal(criteria$id, c(1, 2))
  expect_true(all(c(
    "elpd_loo", "loo_R2", "bayes_R2", "log_lik"
  ) %in% names(criteria)))
  expect_equal(criteria$log_lik, c(-4, -8))
})

test_that("comparison rescaling rejects invalid and non-overlapping scales", {
  fixture <- bentley_fixture()
  diagnostic <- influ(fixture$model, focus = "year")
  expect_error(plot_compare(diagnostic, rescale = 0), "finite and positive")

  second <- diagnostic
  second$indices$level <- paste0("other-", second$indices$level)
  expect_error(
    plot_compare(
      list(diagnostic, second),
      labels = c("First", "Second"),
      rescale_series = 1
    ),
    "share at least one focus level"
  )
})
