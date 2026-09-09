integration_fixture <- function() {
  set.seed(409)
  d <- expand.grid(year = factor(2011:2013), cell = 1:80)
  d$x <- runif(nrow(d), -1, 1)
  d$y <- runif(nrow(d), -1, 1)
  d$density <- rgamma(nrow(d), shape = 4,
    scale = exp(1 + 0.2 * as.integer(d$year) + 0.5 * d$x - d$y^2) / 4)
  d
}

test_that("integration applies to GLMs and spatial GAMs, with joint uncertainty", {
  skip_if_not_installed("mgcv")
  d <- integration_fixture()
  models <- list(glm(density ~ year + x + y, Gamma(link = "log"), d),
    mgcv::gam(density ~ year + s(x, y, k = 12), Gamma(link = "log"), data = d, method = "REML"))
  ref <- data.frame(x = c(-0.7, 0.2, 0.8), y = c(0.5, -0.2, 0.6), area = c(2, 3, 5))
  for (m in models) {
    a <- integrate_index(m, ref, "area", area_units = "km^2", response_units = "kg/km^2", units = "kg")
    b <- cpue_index(m, reference_data = ref, reference_weights = ref$area)
    expect_s3_class(a, "influ_index")
    for (nm in c("Mean", "SD", "Qlower", "Qupper")) expect_equal(a$table[[nm]], 10 * b$table[[nm]])
    expect_equal(a$table$CV, b$table$CV)
    expect_equal(a$metadata$total_area, 10)
    expect_identical(a$metadata$method, "integrated")
    for (i in 1:3) {
      nd <- transform(ref, year = levels(d$year)[i])
      expect_equal(a$table$Mean[i], sum(ref$area * predict(m, nd, type = "response")), ignore_attr = TRUE)
    }
    twice <- integrate_index(m, ref, 2 * ref$area, area_units = "km^2", response_units = "kg/km^2")
    expect_equal(twice$table$Mean, 2 * a$table$Mean)
    expect_equal(twice$table$SD, 2 * a$table$SD)
    relative <- integrate_index(m, ref, "area", area_units = "km^2", response_units = "kg/km^2", rescale = 1)
    expect_equal(geo_mean(relative$table$Mean), 1)
    expect_equal(relative$table$SD, cpue_index(m, reference_data = ref, reference_weights = ref$area, rescale = 1)$table$SD)
    expect_s3_class(plot_index(a), "ggplot")
    expect_silent(ggplot2::ggplot_build(plot_compare(list(A = a, B = a))))
    expect_error(plot_compare(list(a, b)), "method")
  }
})

test_that("GLMs without space can integrate, but do not invent spatial variation", {
  d <- integration_fixture()
  m <- glm(density ~ year, Gamma(link = "log"), d)
  ref <- data.frame(cell = 1:3)
  a <- integrate_index(m, ref, c(2, 3, 5), area_units = "km^2", response_units = "kg/km^2")
  p <- predict(m, data.frame(year = levels(d$year)), type = "response", se.fit = TRUE)
  expect_equal(a$table$Mean, 10 * as.numeric(p$fit))
  expect_equal(a$table$SD, 10 * as.numeric(p$se.fit))
})

test_that("known conversions and within-cell averaging do not double-count area", {
  d <- integration_fixture()
  m <- glm(density ~ year + x + y, Gamma(link = "log"), d)
  ref <- data.frame(x = c(-0.5, 0.5), y = 0, area = c(2, 3), q = c(0.5, 2))
  a <- integrate_index(m, ref, "area", catchability = "q", area_units = "km^2", response_units = "kg/tow")
  b <- cpue_index(m, reference_data = ref, reference_weights = ref$area / ref$q)
  expect_equal(a$table$Mean, b$table$Mean * sum(ref$area / ref$q))
  repeated <- ref[rep(1:2, each = 4), ]
  repeated$season <- rep(1:4, 2)
  c <- integrate_index(m, repeated, "area", catchability = "q", averaging_weights = 1 / 4,
    area_units = "km^2", response_units = "kg/tow")
  expect_equal(a$table, c$table)
  expect_equal(c$metadata$total_area, 5)
  zero <- rbind(ref, data.frame(x = NA, y = NA, area = 0, q = 1))
  expect_equal(a$table, integrate_index(m, zero, "area", catchability = "q", area_units = "km^2", response_units = "kg/tow")$table)
})

test_that("integration refuses implicit areas, units, and invalid conversions", {
  d <- integration_fixture()
  m <- glm(density ~ year + x + y, Gamma(link = "log"), d)
  ref <- data.frame(x = 1:2, y = 0)
  expect_error(integrate_index(m, ref), "explicit cell areas")
  expect_error(integrate_index(m, ref, 1), "both.*units")
  call <- function(...) integrate_index(m, ref, area_units = "km^2", response_units = "kg/km^2", ...)
  for (area in list(0, -1, Inf, NA_real_, "absent", 1:3, matrix(1))) expect_error(call(area = area), "area|Area")
  for (q in list(0, -1, Inf, NA_real_, 1:3)) expect_error(call(area = 1, catchability = q), "catchability")
  expect_error(call(area = 1, averaging_weights = 0), "positive finite sum")
  expect_error(integrate_index(m, ref, 1, area_units = "", response_units = "kg/km^2"), "non-empty")
})

test_that("glmmTMB zero-inflated totals use the combined native expectation", {
  skip_if_not_installed("glmmTMB")
  d <- integration_fixture()
  d$catch <- rpois(nrow(d), exp(2 + d$x + 0.1 * as.integer(d$year)))
  d$catch[runif(nrow(d)) < 0.3] <- 0
  m <- glmmTMB::glmmTMB(catch ~ year + x, ziformula = ~x, family = poisson(), data = d)
  ref <- data.frame(x = c(-0.5, 0.5))
  a <- integrate_index(m, ref, c(3, 7), area_units = "km^2", response_units = "fish/km^2")
  for (i in 1:3) {
    p <- predict(m, transform(ref, year = levels(d$year)[i]), type = "response", re.form = NA)
    expect_equal(a$table$Mean[i], sum(c(3, 7) * p))
  }
  expect_true(all(is.finite(a$table$SD)))
})

test_that("brms integration aggregates paired posterior expectations, not coefficients", {
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  d <- integration_fixture()
  m <- structure(list(data = d, formula = brms::bf(density ~ year + x),
    family = brms::brmsfamily("Gamma", link = "log"), fit = TRUE), class = "brmsfit")
  testthat::local_mocked_bindings(ndraws = function(x) 12L, .package = "posterior")
  testthat::local_mocked_bindings(posterior_epred = function(object, newdata, draw_ids, ...) {
    exp(outer(draw_ids / 10, as.numeric(newdata$year) + newdata$x, "+"))
  }, .package = "brms")
  ref <- data.frame(x = c(0, 1))
  a <- integrate_index(m, ref, c(2, 3), area_units = "km^2", response_units = "kg/km^2",
    ndraws = 10, retain = "draws", batch_size = 1, draw_batch_size = 3)
  b <- integrate_index(m, ref, c(2, 3), area_units = "km^2", response_units = "kg/km^2",
    ndraws = 10, retain = "draws", batch_size = 20, draw_batch_size = 12)
  expect_equal(a, b)
  expected <- exp(outer(a$metadata$draw_ids / 10, 1:3, "+")) * (2 + 3 * exp(1))
  expect_equal(unname(a$draws), expected)
  expect_equal(a$table$Mean, colMeans(expected))
  expect_equal(a$table$Median, apply(expected, 2, median))
  relative <- integrate_index(m, ref, c(2, 3), area_units = "km^2", response_units = "kg/km^2", rescale = 1, retain = "draws")
  expect_equal(exp(rowMeans(log(relative$draws))), rep(1, 12), ignore_attr = TRUE)
})
