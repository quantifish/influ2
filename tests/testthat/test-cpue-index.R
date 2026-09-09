index_fixture <- function() {
  set.seed(29)
  d <- data.frame(year = factor(rep(c(2010, 2012, 2013), each = 100)),
    x = runif(300, -1, 1), exposure = runif(300, 0.5, 2))
  d$catch <- rpois(nrow(d), exp(0.3 + 0.2 * as.numeric(d$year) + 0.5 * d$x) * d$exposure)
  d
}

test_that("geo_mean is stable, explicit about zeros, and exported", {
  expect_equal(geo_mean(c(1, 4, 16)), 4)
  expect_equal(geo_mean(rep(1e200, 100)), 1e200)
  expect_equal(log(geo_mean(rep(1e-200, 100))), log(1e-200))
  expect_equal(geo_mean(c(0, 3)), 0)
  expect_true(is.na(geo_mean(c(0, NA))))
  expect_equal(geo_mean(c(1, 4, NA), na.rm = TRUE), 2)
  expect_true(is.nan(geo_mean(numeric())))
  expect_true(is.nan(geo_mean(NA_real_, na.rm = TRUE)))
  expect_error(geo_mean(c(-1, 4)), "non-negative")
  expect_error(geo_mean(Inf), "finite")
  expect_error(geo_mean("1"), "numeric vector")
  expect_error(geo_mean(matrix(1)), "numeric vector")
  expect_error(geo_mean(1, na.rm = NA), "TRUE or FALSE")
})

test_that("response indices reproduce weighted predictions and joint delta uncertainty", {
  d <- index_fixture()
  m <- glm(catch ~ year * x + offset(log(exposure)), poisson(), d)
  ref <- data.frame(x = c(-0.7, 0.4, 1), exposure = 1)
  w <- c(1, 3, 2)
  index <- cpue_index(m, reference_data = ref, reference_weights = w)
  expect_s3_class(index, "influ_index")
  tab <- as.data.frame(index)
  expect_true(all(c("Year", "Mean", "Median", "SD", "CV", "Qlower", "Qupper") %in% names(tab)))
  expect_identical(tab$Year, levels(d$year))
  gradient <- matrix(0, 3, length(coef(m)))
  for (i in seq_len(3)) {
    nd <- ref
    nd$year <- factor(levels(d$year)[i], levels = levels(d$year))
    pred <- predict(m, nd, type = "response")
    expected <- weighted.mean(pred, w)
    X <- model.matrix(delete.response(terms(m)), nd)
    gradient[i, ] <- colSums(X * (pred * w / sum(w)))
    expect_equal(tab$Mean[i], expected)
  }
  covariance <- gradient %*% vcov(m) %*% t(gradient)
  expect_equal(tab$SD, sqrt(diag(covariance)), ignore_attr = TRUE)
  expect_equal(tab$CV, tab$SD / tab$Mean)
  expect_true(all(is.na(tab$Median)))
  expect_null(index$draws)
  expect_false(any(c("model", "reference_data") %in% names(index)))
  expect_equal(index, cpue_index(m, method = "standardized",
    reference_data = ref, reference_weights = w))
  expect_equal(index$table, cpue_index(m, reference_data = ref,
    reference_weights = w, batch_size = 1)$table)
  scaled <- cpue_index(m, reference_data = ref, reference_weights = w, rescale = 1)
  a <- tab$Mean
  J <- (diag(3) - outer(a, 1 / a) / 3) / geo_mean(a)
  expect_equal(geo_mean(scaled$table$Mean), 1)
  expect_equal(scaled$table$SD, sqrt(diag(J %*% covariance %*% t(J))), ignore_attr = TRUE)
  expect_true(all(scaled$table$SD > 0))
  preview <- cpue_index(m, reference_data = ref, reference_weights = w, uncertainty = "none")
  expect_equal(preview$table$Mean, tab$Mean)
  expect_true(all(is.na(preview$table$SD)))
  expect_output(print(index), "CPUE index")
})

test_that("index guards prevent implicit references and ambiguous unsupported predictions", {
  d <- index_fixture()
  m <- glm(catch ~ year + x, poisson(), d)
  ref <- data.frame(x = c(0, 1))
  expect_error(cpue_index(m), "reference_data")
  expect_error(cpue_index(m, reference_data = d), "without the year")
  expect_error(cpue_index(m, reference_data = data.frame(other = 1)), "missing: x")
  expect_error(cpue_index(m, reference_data = data.frame(x = NA_real_)), "missing values")
  for (w in list(0, c(0, 0), c(-1, 2), c(NA, 2), c(Inf, 2))) {
    expect_error(cpue_index(m, reference_data = ref, reference_weights = w), "reference_weights")
  }
  expect_equal(cpue_index(m, reference_data = data.frame(x = c(0, NA)),
    reference_weights = c(1, 0))$table,
    cpue_index(m, reference_data = data.frame(x = 0))$table)
  expect_error(cpue_index(m, reference_data = ref, rescale = 0), "rescale")
  expect_error(cpue_index(m, reference_data = ref, retain = "draws"), "only for brms")
  expect_error(cpue_index(m, reference_data = ref, response = "something"), "Additional arguments")
  no_frame <- glm(catch ~ year + x, poisson(), d, model = FALSE)
  expect_error(cpue_index(no_frame, reference_data = ref), "raw year")
  transformed <- glm(log(catch + 1) ~ year + x, gaussian(), d)
  expect_error(cpue_index(transformed, reference_data = ref), "transformed response")
  off <- glm(catch ~ year + x, poisson(), d, offset = log(exposure))
  expect_error(cpue_index(off, reference_data = ref), "inside the model formula")
  aliased <- glm(catch ~ year + x + I(2 * x), poisson(), d)
  expect_error(cpue_index(aliased, reference_data = ref), "Rank-deficient")
  quasi <- glm(catch ~ year + x, quasipoisson(), d)
  expect_error(cpue_index(quasi, reference_data = ref), "Quasi")
  expect_error(cpue_index(structure(list(), class = "sdmTMB"), year = "year"), "year_effect")
})

test_that("year-effect extraction is explicitly distinct and preserves existing comparisons", {
  m <- bentley_fixture()$model
  diagnostic <- influ(m, focus = "year")
  index <- cpue_index(diagnostic, method = "year_effect")
  expected <- subset(influ_indices(diagnostic), series == "standardised")
  expect_equal(index$table$Mean, expected$estimate)
  expect_identical(index$metadata$method, "year_effect")
  expect_error(cpue_index(diagnostic, year = "area", method = "year_effect"), "match")
  expect_error(cpue_index(diagnostic, method = "year_effect", rescale = 1), "rescaling")
  preview <- cpue_index(diagnostic, method = "year_effect", uncertainty = "none")
  expect_true(all(is.na(preview$table$SD)))
  expect_s3_class(plot_compare(list(diagnostic, diagnostic), labels = c("A", "B")), "ggplot")
})

test_that("calculated indices plot without recalculation and reject incompatible inputs", {
  d <- index_fixture()
  a <- cpue_index(glm(catch ~ year + x, poisson(), d), reference_data = data.frame(x = 0))
  b <- cpue_index(glm(catch ~ year * x, poisson(), d), reference_data = data.frame(x = 0))
  for (p in list(plot_index(a), plot(a), ggplot2::autoplot(a),
      plot_compare(list(Main = a, Interaction = b)))) {
    expect_s3_class(p, "ggplot")
    expect_silent(ggplot2::ggplot_build(p))
  }
  expect_error(plot_index(d), "cpue_index")
  expect_error(plot_compare(list(a, d)), "Do not mix")
  expect_error(plot_compare(list(a, b), labels = c("A", "A")), "unique")
  expect_error(plot_compare(list(a, b), rescale = 1), "stored values")
  b$metadata$units <- "kg per set"
  expect_error(plot_compare(list(a, b)), "units")
})

test_that("Gaussian, Gamma, and binomial response targets match native predictions", {
  d <- index_fixture()
  d$z <- rnorm(nrow(d), -3 + d$x)
  d$positive <- exp(rnorm(nrow(d), d$x, 0.3))
  d$success <- rbinom(nrow(d), 5, plogis(d$x))
  fits <- list(glm(z ~ year + x, gaussian(), d),
    glm(positive ~ year + x, Gamma(link = "log"), d),
    glm(cbind(success, 5 - success) ~ year + x, binomial(), d))
  nd <- data.frame(year = levels(d$year), x = 0)
  for (m in fits) {
    a <- cpue_index(m, reference_data = data.frame(x = 0))
    expect_equal(a$table$Mean, as.numeric(predict(m, nd, type = "response")))
    expect_true(all(is.finite(a$table$SD)))
  }
  expect_true(all(is.na(cpue_index(fits[[1]], reference_data = data.frame(x = 0))$table$CV)))
})

test_that("GAM indices preserve smooth and offset predictions", {
  skip_if_not_installed("mgcv")
  d <- index_fixture()
  m <- mgcv::gam(catch ~ year + s(x, k = 5) + offset(log(exposure)),
    family = poisson(), method = "REML", data = d)
  ref <- data.frame(x = c(-0.5, 0.6), exposure = c(1, 2))
  a <- cpue_index(m, reference_data = ref)
  g <- do.call(rbind, lapply(levels(d$year), function(year) transform(ref, year = year)))
  p <- predict(m, g, type = "response")
  expect_equal(a$table$Mean, as.numeric(tapply(p, g$year, mean)))
  expect_equal(a$table, cpue_index(m, reference_data = ref, batch_size = 1)$table)
  expect_true(all(is.finite(a$table$SD)))
})

test_that("glmmTMB indices agree with native response estimates and uncertainty", {
  skip_if_not_installed("glmmTMB")
  d <- index_fixture()
  d$catch <- rnbinom(nrow(d), mu = exp(0.5 + 0.5 * d$x), size = 2)
  m <- glmmTMB::glmmTMB(catch ~ year + x, family = glmmTMB::nbinom2(), data = d)
  a <- cpue_index(m, reference_data = data.frame(x = 0.5))
  nd <- data.frame(year = levels(d$year), x = 0.5)
  native <- predict(m, nd, type = "response", re.form = NA, se.fit = TRUE)
  expect_equal(a$table$Mean, as.numeric(native$fit))
  expect_equal(a$table$SD, as.numeric(native$se.fit), tolerance = 1e-5)
  expect_equal(a$table, cpue_index(m, reference_data = data.frame(x = 0.5), batch_size = 1)$table)
  d$catch[seq(1, nrow(d), by = 3)] <- 0
  for (family in list(glmmTMB::nbinom2(), glmmTMB::truncated_nbinom2())) {
    m <- glmmTMB::glmmTMB(catch ~ year + x, ziformula = ~x, family = family, data = d)
    a <- cpue_index(m, reference_data = data.frame(x = 0.5))
    native <- predict(m, nd, type = "response", re.form = NA, se.fit = TRUE)
    expect_equal(a$table$Mean, as.numeric(native$fit))
    expect_equal(a$table$SD, as.numeric(native$se.fit), tolerance = 1e-4)
  }
})

test_that("brms batching preserves draw identities, weighted means, and normalisation", {
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  d <- index_fixture()
  # Test the aggregation contract with a deterministic native-prediction stub;
  # complete-fit backend agreement is additionally checked in release validation.
  model <- structure(list(data = d, formula = brms::bf(catch ~ year + x),
    family = brms::brmsfamily("poisson"), fit = TRUE), class = "brmsfit")
  testthat::local_mocked_bindings(ndraws = function(x) 12L, .package = "posterior")
  native <- function(object, newdata, draw_ids, re_formula, allow_new_levels) {
    expect_identical(re_formula, NA)
    expect_false(allow_new_levels)
    exp(outer(draw_ids / 10, as.numeric(newdata$year) + newdata$x, "+"))
  }
  testthat::local_mocked_bindings(posterior_epred = native, .package = "brms")
  ref <- data.frame(x = c(0, 0.3))
  a <- cpue_index(model, reference_data = ref, reference_weights = c(1, 3),
    ndraws = 10, batch_size = 1, draw_batch_size = 3, retain = "draws")
  b <- cpue_index(model, reference_data = ref, reference_weights = c(1, 3),
    ndraws = 10, batch_size = 100, draw_batch_size = 100, retain = "draws")
  expect_equal(a, b)
  expect_equal(dim(a$draws), c(10L, 3L))
  expect_equal(a$table$Mean, colMeans(a$draws), ignore_attr = TRUE)
  expect_equal(a$table$Median, apply(a$draws, 2, median), ignore_attr = TRUE)
  expect_equal(a$table$SD, apply(a$draws, 2, sd), ignore_attr = TRUE)
  scaled <- cpue_index(model, reference_data = ref, ndraws = 10,
    rescale = 1, retain = "draws")
  expect_equal(exp(rowMeans(log(scaled$draws))), rep(1, 10), ignore_attr = TRUE)
  expect_equal(scaled$table$SD, rep(0, 3), tolerance = 1e-12)
  summary <- cpue_index(model, reference_data = ref, ndraws = 10)
  preview <- cpue_index(model, reference_data = ref, ndraws = 10, uncertainty = "none")
  expect_equal(summary$table$Mean, preview$table$Mean)
  expect_true(all(is.na(preview$table$SD)))
  expect_null(summary$draws)
  model$fit <- NULL
  expect_error(cpue_index(model, reference_data = ref), "complete")
})

test_that("assessment index figures are visually stable", {
  skip_if_not_installed("vdiffr")
  previous_theme <- ggplot2::theme_set(ggplot2::theme_bw())
  on.exit(ggplot2::theme_set(previous_theme), add = TRUE)
  d <- index_fixture()
  a <- cpue_index(glm(catch ~ year + x, poisson(), d), reference_data = data.frame(x = 0))
  b <- cpue_index(glm(catch ~ year * x, poisson(), d), reference_data = data.frame(x = 0))
  vdiffr::expect_doppelganger("assessment CPUE index", plot_index(a))
  vdiffr::expect_doppelganger("assessment CPUE comparison", plot_compare(list(Main = a, Interaction = b)))
})
