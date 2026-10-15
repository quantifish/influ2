cdi_group_fixture <- function() {
  set.seed(571)
  d <- expand.grid(year = factor(2010:2014), month = factor(sprintf("%02d", 1:6)),
    repeat_id = seq_len(12))
  d$x <- stats::runif(nrow(d))
  effect <- c(-.7, -.4, -.1, .2, .5, .8)
  d$cpue <- stats::rpois(nrow(d), exp(.8 + .1 * as.numeric(d$year) +
    effect[as.integer(d$month)] + .3 * d$x))
  d
}

test_that("glmmTMB single-intercept CDI retains native grouping identities", {
  skip_if_not_installed("glmmTMB")
  d <- cdi_group_fixture()
  m <- glmmTMB::glmmTMB(cpue ~ year + x + (1 | month), family = poisson(), data = d)
  expect_true(m$sdr$pdHess)
  diagnostic <- influ(m, focus = "year")
  coefficients <- subset(diagnostic$coefficients, term == "random_effects")
  composition <- subset(diagnostic$composition, term == "random_effects")
  influence <- subset(diagnostic$influence,
    term == "random_effects" & scale == "ratio")
  expect_identical(diagnostic$metadata$glmmTMB$random_cdi_group, "month")
  expect_setequal(coefficients$level, levels(d$month))
  expect_setequal(composition$term_level, levels(d$month))
  modes <- as.numeric(glmmTMB::getME(m, "b"))
  expect_equal(coefficients$estimate,
    modes[match(coefficients$level, levels(d$month))], tolerance = 1e-12)
  native_covariance <- .glmmTMB_random_covariance(m, length(modes))
  expect_equal(coefficients$std_error,
    sqrt(diag(native_covariance))[match(coefficients$level, levels(d$month))],
    tolerance = 1e-12)
  by_year <- tapply(modes[as.integer(d$month)], d$year, mean)
  expected <- exp(by_year - mean(modes[as.integer(d$month)]))
  expect_equal(influence$estimate, as.numeric(expected[match(influence$level,
    names(expected))]), tolerance = 1e-12)
  proportion <- prop.table(table(d$year, d$month), 1)
  expect_equal(composition$proportion,
    as.numeric(proportion[cbind(match(composition$level, rownames(proportion)),
      match(composition$term_level, colnames(proportion)))]))
  expect_s3_class(plot(diagnostic, type = "cdi", term = "random_effects"), "patchwork")
})

test_that("mixed-model CDI overlays align actual strata across unequal periods", {
  skip_if_not_installed("glmmTMB")
  d <- cdi_group_fixture()
  recent <- droplevels(subset(d, year %in% c("2012", "2014") & month != "06"))
  full <- glmmTMB::glmmTMB(cpue ~ year + x + (1 | month), poisson(), data = d)
  short <- glmmTMB::glmmTMB(cpue ~ year + x + (1 | month), poisson(), data = recent)
  diagnostics <- list(Full = influ(full, focus = "year"),
    Recent = influ(short, focus = "year"))
  original <- diagnostics
  p <- plot_cdi(diagnostics, term = "random_effects")
  display <- attr(p, "cdi_comparison")
  expect_identical(display$term_levels, levels(d$month))
  expect_identical(display$term_label, "month")
  expect_identical(display$year_levels, as.character(2010:2014))
  recent_coefficients <- subset(display$coefficients, Model == "Recent")
  expect_false("06" %in% recent_coefficients$level)
  expect_setequal(recent_coefficients$level, levels(recent$month))
  recent_bubbles <- subset(display$composition, Model == "Recent")
  expect_false(any(recent_bubbles$level %in% c("2010", "2011", "2013")))
  expect_identical(diagnostics, original)
  expect_silent(ggplot2::ggplotGrob(p))
  old <- diagnostics
  old$Recent$metadata$glmmTMB <- NULL
  expect_error(plot_cdi(old, term = "random_effects"), "group|provenance|aggregate")
})

test_that("complex glmmTMB random structures are not mislabelled as one grouping variable", {
  skip_if_not_installed("glmmTMB")
  d <- cdi_group_fixture()
  d$vessel <- factor(rep(letters[1:4], length.out = nrow(d)))
  m <- glmmTMB::glmmTMB(cpue ~ year + x + (1 | month) + (1 | vessel),
    poisson(), data = d)
  diagnostic <- influ(m, focus = "year", uncertainty = "none")
  expect_null(diagnostic$metadata$glmmTMB$random_cdi_group)
  expect_error(plot_cdi(list(A = diagnostic, B = diagnostic), term = "random_effects"),
    "group|provenance|aggregate")
})
