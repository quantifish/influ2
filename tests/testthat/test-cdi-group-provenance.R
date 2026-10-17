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

test_that("glmmTMB random-intercept CDI retains native grouping identities", {
  skip_if_not_installed("glmmTMB")
  d <- cdi_group_fixture()
  m <- glmmTMB::glmmTMB(cpue ~ year + x + (1 | month), family = poisson(), data = d)
  expect_true(m$sdr$pdHess)
  diagnostic <- influ(m, focus = "year")
  # The random-effect term is named after its grouping factor, as for brms.
  coefficients <- subset(diagnostic$coefficients, term == "month")
  composition <- subset(diagnostic$composition, term == "month")
  influence <- subset(diagnostic$influence,
    term == "month" & scale == "ratio")
  expect_identical(diagnostic$metadata$random_effect_terms,
    data.frame(term = "month", component = "random_effects", identified = TRUE))
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
  expect_s3_class(plot(diagnostic, type = "cdi", term = "month"), "patchwork")
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
  p <- plot_cdi(diagnostics, term = "month")
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
  # Aggregate terms saved before the split still need recorded provenance.
  old <- diagnostics
  for (i in seq_along(old)) {
    for (table in c("coefficients", "composition", "influence")) {
      rows <- old[[i]][[table]]$term == "month"
      old[[i]][[table]]$term[rows] <- "random_effects"
    }
    old[[i]]$metadata$random_effect_terms <- NULL
  }
  old$Full$metadata$glmmTMB$random_cdi_group <- "month"
  expect_error(plot_cdi(old, term = "random_effects"), "group|provenance|aggregate")
  old$Recent$metadata$glmmTMB$random_cdi_group <- "month"
  expect_s3_class(plot_cdi(old, term = "random_effects"), "patchwork")
})

test_that("several glmmTMB grouping factors form separate identified terms", {
  skip_if_not_installed("glmmTMB")
  d <- cdi_group_fixture()
  d$vessel <- factor(rep(letters[1:4], length.out = nrow(d)))
  m <- glmmTMB::glmmTMB(cpue ~ year + x + (1 | month) + (1 | vessel),
    poisson(), data = d)
  diagnostic <- influ(m, focus = "year", uncertainty = "none")
  terms <- diagnostic$metadata$random_effect_terms
  expect_setequal(terms$term, c("month", "vessel"))
  expect_true(all(terms$identified))
  expect_setequal(subset(diagnostic$coefficients, term == "month")$level,
    levels(d$month))
  expect_setequal(subset(diagnostic$coefficients, term == "vessel")$level,
    levels(d$vessel))
  p <- plot_cdi(list(A = diagnostic, B = diagnostic), term = "vessel")
  expect_identical(attr(p, "cdi_comparison")$term_levels, levels(d$vessel))

  # A term whose levels could not be recovered is never overlaid.
  unidentified <- diagnostic
  unidentified$metadata$random_effect_terms$identified[terms$term == "vessel"] <- FALSE
  expect_error(plot_cdi(list(A = unidentified, B = diagnostic), term = "vessel"),
    "no identified grouping-factor levels")
})
