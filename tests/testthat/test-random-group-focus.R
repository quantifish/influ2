test_that("random grouping levels preserve numeric focus order without binning", {
  data <- data.frame(year = rep(c(30:1), each = 2), x = seq_len(60))
  original <- data
  fitted <- factor(data$year)
  grouped <- .random_group_terms("year", 30L, list(year = fitted),
    data, "year", "test")

  expect_true(is.factor(grouped$data$year))
  expect_identical(levels(grouped$data$year), as.character(1:30))
  expect_identical(.focus_info(grouped$data, "year"), .focus_info(data, "year"))
  expect_identical(as.character(grouped$data$year), as.character(fitted))
  expect_identical(levels(.term_level_values(grouped$data, "year", data$x)),
    as.character(1:30))
  expect_identical(grouped$identified, c(year = TRUE))
  expect_identical(data, original)

  # Declared factor order, including non-chronological order, remains the
  # same in the fixed and random components; unused levels are not invented.
  data$year <- factor(data$year, levels = c(31, 30:1))
  grouped <- .random_group_terms("year", 30L,
    list(year = droplevels(data$year)), data, "year", "test")
  expect_identical(.focus_info(grouped$data, "year"), .focus_info(data, "year"))
  expect_identical(levels(grouped$data$year), as.character(30:1))
})

random_focus_fixture <- function() {
  set.seed(716)
  data <- data.frame(year = rep(1:30, each = 8), x = stats::rnorm(240))
  effects <- stats::rnorm(30, sd = .8)
  data$y <- .1 * data$year + .4 * data$x + effects[data$year] +
    stats::rnorm(nrow(data), sd = .3)
  # Put late years first, so chronological order cannot be inherited from
  # observation order, and use 1:30 to distinguish numeric from lexical order.
  data[rev(seq_len(nrow(data))), , drop = FALSE]
}

for (backend in c("lme4", "glmmTMB")) {
  local({
    backend <- backend
    test_that(paste(backend, "random focus CDI retains all native year levels"), {
      skip_if_not_installed(backend)
      data <- random_focus_fixture()
      recent <- subset(data, year >= 10)
      fit <- function(data) {
        if (backend == "lme4") {
          lme4::lmer(y ~ year + x + (1 | year), data = data)
        } else {
          glmmTMB::glmmTMB(y ~ year + x + (1 | year), data = data,
            family = stats::gaussian())
        }
      }
      full_model <- fit(data)
      recent_model <- fit(recent)
      diagnostic <- influ(full_model, focus = "year", uncertainty = "none")
      recent_diagnostic <- influ(recent_model, focus = "year", uncertainty = "none")

      coefficients <- subset(diagnostic$coefficients,
        term == "year" & component == "random_effects")
      composition <- subset(diagnostic$composition,
        term == "year" & component == "random_effects")
      influence <- subset(diagnostic$influence,
        term == "year" & component == "random_effects" & scale == "link")
      modes <- if (backend == "lme4") {
        lme4::ranef(full_model)$year
      } else {
        glmmTMB::ranef(full_model)$cond$year
      }
      expect_identical(coefficients$level, as.character(1:30))
      expect_equal(coefficients$estimate,
        modes[coefficients$level, "(Intercept)"], tolerance = 1e-10)
      expect_equal(nrow(composition), 30L)
      expect_identical(composition$term_level, composition$level)
      expect_equal(composition$proportion, rep(1, 30))
      expect_identical(influence$level, as.character(1:30))
      expect_equal(influence$estimate,
        modes[influence$level, "(Intercept)"] - mean(modes[, "(Intercept)"]),
        tolerance = 1e-10)
      expect_identical(unique(subset(diagnostic$influence,
        component == "conditional" & term == "year")$level), influence$level)
      expect_identical(diagnostic$metadata$random_effect_terms,
        data.frame(term = "year", component = "random_effects", identified = TRUE))
      expect_s3_class(plot(diagnostic, type = "cdi", term = "year",
        component = "random_effects"), "patchwork")

      diagnostics <- list(Full = diagnostic, Recent = recent_diagnostic)
      comparison <- plot_cdi(diagnostics, term = "year", component = "random_effects")
      display <- attr(comparison, "cdi_comparison")
      expect_identical(display$term_levels, as.character(1:30))
      expect_identical(display$year_levels, as.character(1:30))
      recent_coefficients <- subset(display$coefficients, Model == "Recent")
      recent_composition <- subset(display$composition, Model == "Recent")
      expect_identical(recent_coefficients$level, as.character(10:30))
      expect_setequal(recent_composition$term_level, as.character(10:30))
      expect_false(any(recent_composition$level %in% as.character(1:9)))
      expect_equal(recent_coefficients$category, 10:30)
      expect_silent(ggplot2::ggplotGrob(comparison))
    })
  })
}
