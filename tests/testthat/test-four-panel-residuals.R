four_panel_fixture <- function() {
  set.seed(901)
  d <- expand.grid(year = factor(c(2010, 2011, 2013, 2014)), repeat_id = 1:25)
  d$x <- rnorm(nrow(d))
  d$catch <- rpois(nrow(d), exp(0.3 + 0.1 * as.numeric(d$year) + 0.4 * d$x))
  d
}

test_that("year boxes encode sample size without crowding axis labels", {
  d <- four_panel_fixture()
  d <- d[d$year != "2011" | d$repeat_id <= 4, ]
  fit <- glm(catch ~ year + x, poisson(), data = d)
  result <- influ_residuals(fit, nsim = 20)
  p <- plot(result, type = "year")
  expect_identical(p$scales$get_scales("x")$labels, levels(d$year))
  boxes <- ggplot2::ggplot_build(p)$data[[3]]
  widths <- boxes$xmax - boxes$xmin
  expect_equal(widths / max(widths), sqrt(as.numeric(table(d$year)) / max(table(d$year))))
  expect_false(any(grepl("n=", p$scales$get_scales("x")$labels)))
})

test_that("compact ranks reproduce the exact simulation calculation", {
  d <- four_panel_fixture()
  fit <- glm(catch ~ year + x, poisson(), data = d)
  set.seed(23)
  before <- .Random.seed
  result <- influ_residuals(fit, nsim = 40, batch_size = 7, seed = 302)
  expect_identical(.Random.seed, before)
  set.seed(302)
  u <- runif(nrow(d))
  sims <- as.matrix(simulate(fit, nsim = 40))
  expected <- (rowSums(sims < d$catch) + u * (rowSums(sims == d$catch) + 1)) / 41
  expect_equal(result$observations$pit, expected, ignore_attr = TRUE)
  expect_equal(result$observations$predicted, rowMeans(sims), ignore_attr = TRUE)
  expect_equal(result$observations$residual, qnorm(expected), ignore_attr = TRUE)
  expect_identical(result$observations,
    influ_residuals(fit, nsim = 40, batch_size = 40, seed = 302)$observations)
  expect_identical(result, influ_residuals(fit, nsim = 40, batch_size = 7, seed = 302))
  expect_false(any(c("model", "simulations", "draws") %in% names(result)))
  expect_lt(as.numeric(object.size(result)), as.numeric(object.size(sims)) * 2)
  expect_output(print(result), "year-name detection")
  expect_s3_class(plot(result), "patchwork")
  for (p in c("qq", "fitted", "year", "distribution")) {
    expect_s3_class(plot(result, type = p), "ggplot")
    expect_s3_class(ggplot2::autoplot(result, type = p), "ggplot")
  }
  standalone_qq <- plot(result, type = "qq")
  overview_qq <- plot(result)[[1L]]
  expect_identical(standalone_qq$data, overview_qq$data)
  expect_identical(ggplot2::ggplot_build(standalone_qq)$data,
    ggplot2::ggplot_build(overview_qq)$data)
  expect_identical(plot(result, type = "distribution")$scales$get_scales("y")$limits, c(0, 1))
  expected_ecdf <- apply(sims, 2, function(s) ecdf(s)(result$ecdf$response))
  expect_equal(result$ecdf$median, apply(expected_ecdf, 1, median), ignore_attr = TRUE)
  expect_equal(result$ecdf$lower, apply(expected_ecdf, 1, quantile, 0.025), ignore_attr = TRUE)
  expect_error(plot(result, type = "wrong"), "arg")
})

test_that("overview labels distinguish normal-score PIT panels from response checks", {
  d <- four_panel_fixture()
  d$present <- as.integer(d$catch > 2)
  models <- list(
    distribution = glm(catch ~ year + x, poisson(), data = d),
    calibration = glm(present ~ year + x, binomial(), data = d)
  )
  for (fourth in names(models)) {
    result <- influ_residuals(models[[fourth]], nsim = 20, seed = 317)
    before <- result
    rng <- .Random.seed
    overview <- plot(result)
    caption <- overview$patches$annotation$caption
    expect_match(caption, "Panels A-C: simulation-based PIT residuals", fixed = TRUE)
    expect_match(caption, "normal scale (qnorm(PIT))", fixed = TRUE)
    expect_match(caption, if (fourth == "calibration")
      "Panel D: response probability calibration." else
      "Panel D: observed versus simulated response ECDF.", fixed = TRUE)
    expect_match(caption, result$metadata$scheme, fixed = TRUE)
    expect_match(caption, "20 simulations", fixed = TRUE)
    for (panel in c("qq", "fitted", "year")) {
      expect_identical(plot(result, type = panel)$labels$y, "Normal-score PIT residual")
    }
    expect_false(grepl("PIT", plot(result, type = fourth)$labels$y, fixed = TRUE))
    expect_identical(result, before)
    expect_identical(.Random.seed, rng)
  }
})

test_that("time selection recognises aliases, ordering, ambiguity, and overrides", {
  d <- four_panel_fixture()
  names(d)[1] <- "Fishing.Year"
  d$year <- d$Fishing.Year
  fit <- glm(catch ~ x + Fishing.Year, poisson(), data = d)
  expect_identical(influ_residuals(fit, nsim = 20)$metadata$year, "Fishing.Year")
  both <- glm(catch ~ year + Fishing.Year + x, poisson(), data = d)
  expect_error(influ_residuals(both, nsim = 20), "unambiguous time")
  expect_identical(influ_residuals(both, year = "year", nsim = 20)$metadata$year_source, "explicit")
  names(d)[1] <- "Period"
  fit <- glm(catch ~ Period + x, poisson(), data = d)
  expect_warning(result <- influ_residuals(fit, nsim = 20), "first formula term")
  expect_identical(levels(result$observations$year), c("2010", "2011", "2013", "2014"))
  p <- plot(result, type = "year")
  expect_equal(sort(unique(p$data$position)), c(2010, 2011, 2013, 2014))
  expect_error(influ_residuals(fit, year = "missing", nsim = 20), "must name one column")
  intercept <- glm(catch ~ 1, poisson(), data = d)
  expect_error(influ_residuals(intercept, nsim = 20), "unambiguous time")
  expect_identical(influ_residuals(intercept, data = d, year = "year", nsim = 20)$metadata$year, "year")
  ambiguous <- glm(catch ~ Period:x, poisson(), data = d)
  expect_error(influ_residuals(ambiguous, nsim = 20), "unambiguous time")
  d$Period <- as.character(d$Period)
  d$Period[1] <- NA
  expect_error(.resid_year(fit, d, "Period"), "non-missing")
  expect_error(.resid_year(fit, d, c("Period", "x")), "must name one column")
})

test_that("residual rows are retained after subset and na.exclude", {
  d <- four_panel_fixture()
  d$x[5] <- NA
  for (action in list(na.omit, na.exclude)) {
    fit <- glm(catch ~ year + x, poisson(), data = d,
      subset = repeat_id != 1, na.action = action)
    result <- influ_residuals(fit, data = d[nrow(d):1, ], nsim = 20)
    expect_identical(result$observations$row, rownames(fit$model))
    expect_equal(result$observations$observed, model.response(fit$model), ignore_attr = TRUE)
    bad <- d
    bad$catch[10] <- 999
    expect_error(influ_residuals(fit, data = bad, nsim = 20), "does not match")
  }
  no_frame <- glm(catch ~ year + x, poisson(), data = d, model = FALSE)
  expect_error(influ_residuals(no_frame, nsim = 20), "retained model frame")
})

test_that("binomial diagnostics consistently use successes", {
  d <- four_panel_fixture()
  d$trials <- rep(5:9, length.out = nrow(d))
  d$success <- rbinom(nrow(d), d$trials, plogis(d$x))
  count <- glm(cbind(success, trials - success) ~ year + x, binomial(), data = d)
  prop <- glm(I(success/trials) ~ year + x, weights = trials, binomial(), data = d)
  a <- influ_residuals(count, nsim = 20)
  b <- influ_residuals(prop, data = d, trial_counts = "trials", nsim = 20)
  expect_equal(a$observations, b$observations)
  expect_equal(a$observations$observed, d$success)
  expect_identical(a$metadata$response, "Successes")
  expect_equal(a$observations,
    influ_residuals(count, nsim = 20, batch_size = 1)$observations)
  d$present <- factor(rep(c(FALSE, TRUE), length.out = nrow(d)))
  fit <- glm(present ~ year + x, binomial(), data = d)
  expect_equal(influ_residuals(fit, nsim = 20)$observations$observed, as.numeric(d$present) - 1)
  weighted <- glm(cbind(success, trials - success) ~ year + x,
    binomial(), data = d, weights = rep(2, nrow(d)))
  expect_error(influ_residuals(weighted, nsim = 20), "case weights")
})

test_that("Gaussian, Gamma, NB, GAM NB, and GAM Tweedie simulations work", {
  d <- four_panel_fixture()
  d$positive <- exp(rnorm(nrow(d), 1 + d$x, 0.3))
  models <- list(glm(x ~ year, gaussian(), data = d),
    glm(positive ~ year + x, Gamma(link = "log"), data = d))
  if (requireNamespace("MASS", quietly = TRUE)) {
    models <- c(models, list(MASS::glm.nb(catch ~ year + x, data = d)))
  }
  if (requireNamespace("mgcv", quietly = TRUE)) {
    # mgcv's Tweedie family requires its helpers on the search path.
    attached <- "package:mgcv" %in% search()
    suppressPackageStartupMessages(library("mgcv", character.only = TRUE))
    if (!attached) on.exit(detach("package:mgcv"), add = TRUE)
    tweedie <- mgcv::tw()
    models <- c(models, list(
      mgcv::gam(catch ~ year + s(x, k = 4), family = mgcv::nb(), data = d),
      mgcv::gam(positive ~ year + s(x, k = 4), family = tweedie, data = d)))
  }
  for (fit in models) {
    result <- influ_residuals(fit, nsim = 20)
    expect_true(all(is.finite(result$observations$residual)))
    expect_true(all(result$observations$pit > 0 & result$observations$pit < 1))
  }
  result <- influ_residuals(models[[1]], nsim = 20)
  expect_error(plot(result, type = "distribution", response_scale = "log1p"), "non-negative")
})

test_that("glmmTMB native joint and binomial simulations are retained", {
  skip_if_not_installed("glmmTMB")
  d <- four_panel_fixture()
  # Give every model component genuine support. Fitting NB dispersion,
  # zero inflation, and random effects to the original small Poisson sample
  # put estimates on boundaries and gave platform-dependent Hessian warnings.
  d <- d[rep(seq_len(nrow(d)), each = 4), ]
  set.seed(1759)
  group_effect <- rnorm(25, sd = 0.7)
  mu <- exp(1.1 + 0.12 * as.numeric(d$year) + 0.4 * d$x +
    group_effect[d$repeat_id])
  d$catch <- ifelse(rbinom(nrow(d), 1, 0.25), 0,
    rnbinom(nrow(d), mu = mu, size = 2))
  fit <- glmmTMB::glmmTMB(catch ~ year + x + (1 | repeat_id),
    ziformula = ~1, family = glmmTMB::nbinom2(), data = d)
  expect_identical(fit$fit$convergence, 0L)
  expect_true(fit$sdr$pdHess)
  result <- influ_residuals(fit, nsim = 20, batch_size = 3)
  expect_match(result$metadata$scheme, "random effects resimulated")
  expect_true(all(is.finite(result$observations$pit)))
  d$success <- rbinom(nrow(d), 5, 0.3)
  fit <- glmmTMB::glmmTMB(cbind(success, 5 - success) ~ year + x,
    family = binomial(), data = d)
  expect_equal(influ_residuals(fit, nsim = 20)$observations$observed, d$success)
})

test_that("invalid inputs fail explicitly and RNG is restored after errors", {
  d <- four_panel_fixture()
  fit <- glm(catch ~ year + x, poisson(), data = d)
  for (arg in c("nsim", "batch_size", "grid_size", "seed")) {
    expect_error(do.call(influ_residuals, c(list(model = fit), setNames(list(NA_real_), arg))), "integer")
  }
  expect_error(influ_residuals(fit, level = 1), "between zero and one")
  expect_error(influ_residuals(fit, nsim = 19), "at least 20")
  before <- .Random.seed
  quasi <- glm(catch ~ year + x, quasipoisson(), data = d)
  expect_error(influ_residuals(quasi), "Quasi families")
  expect_identical(.Random.seed, before)
  expect_error(influ_residuals(list()), "No residual simulation adapter")
  weighted <- glm(catch ~ year + x, poisson(), data = d, weights = rep(2, nrow(d)))
  expect_error(influ_residuals(weighted), "weighted fits")
})

test_that("transformed time, offset, and non-syntactic names retain their meaning", {
  d <- four_panel_fixture()
  d$year <- as.numeric(as.character(d$year))
  d$effort <- seq(1, 3, length.out = nrow(d))
  fit <- glm(catch ~ factor(year) + x + offset(log(effort)), poisson(), data = d)
  result <- influ_residuals(fit, nsim = 20, seed = 55)
  expect_identical(result$metadata$year, "year")
  set.seed(55)
  runif(nrow(d))
  sims <- as.matrix(simulate(fit, nsim = 20))
  expect_equal(result$observations$predicted, rowMeans(sims), ignore_attr = TRUE)
  names(d)[1] <- "Fishing year"
  fit <- glm(catch ~ `Fishing year` + x, poisson(), data = d)
  expect_identical(influ_residuals(fit, nsim = 20)$metadata$year, "Fishing year")
  fit <- glm(catch ~ `Fishing year`:x, poisson(), data = d)
  expect_identical(influ_residuals(fit, nsim = 20)$metadata$year, "Fishing year")
})

test_that("compact and multivariate brms fits fail before simulation", {
  compact <- structure(list(influ2_draws = matrix(0, 20, 1)), class = "brmsfit")
  skip_if_not_installed("brms")
  expect_error(influ_residuals(compact), "complete brmsfit")
  multi <- structure(list(formula = structure(list(), class = "mvbrmsformula")), class = "brmsfit")
  expect_error(influ_residuals(multi), "one response")
})

test_that("four-panel display is visually stable", {
  skip_if_not_installed("vdiffr")
  previous_theme <- ggplot2::theme_set(ggplot2::theme_bw())
  on.exit(ggplot2::theme_set(previous_theme), add = TRUE)
  d <- four_panel_fixture()
  fit <- glm(catch ~ year + x, poisson(), data = d)
  vdiffr::expect_doppelganger("four-panel residual overview", function() {
    print(plot(influ_residuals(fit, nsim = 40, seed = 501)))
  })
})
