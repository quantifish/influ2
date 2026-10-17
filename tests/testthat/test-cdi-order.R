ordered_cdi_fixture <- function(random = FALSE, encounter = FALSE) {
  d <- expand.grid(year = factor(2019:2022), vessel = factor(c("V003", "V001", "V002")),
    replicate = 1:12)
  set.seed(20260921)
  eta <- c(V001 = 0.6, V002 = -0.6, V003 = 0.1)[as.character(d$vessel)] +
    0.03 * as.integer(d$year)
  d$response <- if (encounter) rbinom(nrow(d), 1, plogis(eta)) else
    rgamma(nrow(d), shape = 5, scale = exp(eta) / 5)
  family <- if (encounter) binomial() else Gamma(link = "log")
  fit <- if (random) mgcv::gam(response ~ year + s(vessel, bs = "re"),
    family = family, data = d) else
    stats::glm(response ~ year + vessel, family = family, data = d)
  influ(fit, focus = "year")
}

test_that("CDI ordering preserves values and aligns the category panels", {
  skip_if_not_installed("mgcv")
  for (random in c(FALSE, TRUE)) for (encounter in c(FALSE, TRUE)) {
    diagnostic <- ordered_cdi_fixture(random, encounter)
    before <- serialize(diagnostic, NULL)
    term <- if (random) "s(vessel)" else "vessel"
    original <- plot(diagnostic, type = "cdi", term = term)
    explicit <- plot(diagnostic, type = "cdi", term = term,
      coefficient_order = "original")
    expect_identical(original[[1]]$data, explicit[[1]]$data)
    expect_identical(original[[1]]$scales$get_scales("x")$limits,
      explicit[[1]]$scales$get_scales("x")$limits)
    for (direction in c("ascending", "descending")) {
      p <- plot(diagnostic, type = "cdi", term = term,
        coefficient_order = direction)
      expected <- as.character(original[[1]]$data$level[
        order(original[[1]]$data$estimate, decreasing = direction == "descending",
          method = "radix")])
      expect_identical(p[[1]]$scales$get_scales("x")$limits, expected)
      expect_identical(p[[3]]$scales$get_scales("x")$limits, expected)
      expect_identical(levels(p[[1]]$data$level), expected)
      expect_identical(levels(p[[3]]$data$term_level), expected)
      expect_identical(p[[1]]$data$estimate, original[[1]]$data$estimate)
      expect_identical(p[[1]]$data$lower, original[[1]]$data$lower)
      expect_identical(p[[1]]$data$upper, original[[1]]$data$upper)
      expect_identical(as.character(p[[1]]$data$level),
        as.character(original[[1]]$data$level))
      expect_identical(p[[3]]$data$proportion, original[[3]]$data$proportion)
      expect_identical(as.character(p[[3]]$data$term_level),
        as.character(original[[3]]$data$term_level))
      expect_identical(p[[4]]$data, original[[4]]$data)
      expect_identical(p[[4]]$scales$get_scales("x")$limits,
        original[[4]]$scales$get_scales("x")$limits)
      built <- ggplot2::ggplot_build(p[[1]])$data[[3]]
      values <- built$y[order(built$x)]
      expect_true(if (direction == "ascending") all(diff(values) >= 0) else all(diff(values) <= 0))
      bubble <- ggplot2::ggplot_build(p[[3]])$data[[1]]
      expect_equal(as.numeric(bubble$x), match(as.character(p[[3]]$data$term_level), expected))
    }
    expect_identical(serialize(diagnostic, NULL), before)
  }
})

test_that("CDI ordering uses the selected display scale and retains ties", {
  d <- ordered_cdi_fixture()
  rows <- which(d$coefficients$term == "vessel")
  d$coefficients$relative_estimate[rows] <- c(2, 1, 1)
  d$coefficients$centred_estimate[rows] <- c(-1, 1, 0)
  # Only the point estimates are used to determine order.
  for (scale in c("auto", "link")) {
    p <- plot(d, type = "cdi", term = "vessel", coefficient_scale = scale,
      coefficient_order = "ascending")
    expected <- d$coefficients$level[rows[if (scale == "auto") c(2, 3, 1) else c(1, 3, 2)]]
    expect_identical(p[[1]]$scales$get_scales("x")$limits, as.character(expected))
  }
  p <- ggplot2::autoplot(d, type = "cdi", term = "vessel", coefficient_order = "descending")
  expect_identical(p[[1]]$scales$get_scales("x")$limits,
    as.character(d$coefficients$level[rows]))
  expect_error(plot(d, type = "cdi", term = "vessel", coefficient_order = "unknown"),
    "arg.*should be one of")
})

test_that("model-coded CDI ordering is independent of the relative display scale", {
  diagnostic <- ordered_cdi_fixture()
  rows <- which(diagnostic$coefficients$term == "vessel")
  diagnostic$coefficients$estimate[rows] <- c(3, 1, 1)
  diagnostic$coefficients$relative_estimate[rows] <- c(1, 2, 3)
  diagnostic$coefficients$centred_estimate[rows] <- c(-1, 0, 1)
  original <- as.character(diagnostic$coefficients$level[rows])
  for (scale in c("auto", "link")) {
    for (direction in c("ascending", "descending")) {
      p <- ggplot2::autoplot(diagnostic, type = "cdi", term = "vessel",
        coefficient_reference = "model", coefficient_scale = scale,
        coefficient_order = direction)
      expected <- original[if (direction == "ascending") c(2, 3, 1) else 1:3]
      expect_identical(p[[1]]$scales$get_scales("x")$limits, expected)
      expect_identical(p[[3]]$scales$get_scales("x")$limits, expected)
      expect_identical(p[[1]]$data$estimate, c(3, 1, 1))
      expect_identical(p[[3]]$scales$get_scales("y")$limits,
        p[[4]]$scales$get_scales("x")$limits)
    }
  }
  # Sorting is a CDI option, not a change to any other plot's data or axes.
  for (type in c("influence", "components", "index")) {
    original_plot <- plot(diagnostic, type = type)
    ordered_plot <- plot(diagnostic, type = type, coefficient_order = "descending")
    expect_identical(original_plot$data, ordered_plot$data)
    expect_identical(original_plot$labels, ordered_plot$labels)
  }
})

test_that("CDI ordering is selected within a single component", {
  diagnostic <- ordered_cdi_fixture()
  primary <- unique(diagnostic$coefficients$component)
  expect_length(primary, 1L)
  other <- diagnostic
  for (table in c("coefficients", "composition", "influence")) {
    other[[table]]$component <- "alternative"
  }
  rows <- which(other$coefficients$term == "vessel")
  other$coefficients$relative_estimate[rows] <- c(3, 2, 1)
  for (table in c("coefficients", "composition", "influence")) {
    diagnostic[[table]] <- rbind(diagnostic[[table]], other[[table]])
  }
  before <- serialize(diagnostic, NULL)
  expect_error(plot(diagnostic, type = "cdi", term = "vessel",
    coefficient_order = "ascending"), "one component")
  for (component in c(primary, "alternative")) {
    rows <- diagnostic$coefficients$term == "vessel" &
      diagnostic$coefficients$component == component
    coefficients <- diagnostic$coefficients[rows, ]
    p <- plot(diagnostic, type = "cdi", term = "vessel", component = component,
      coefficient_order = "ascending")
    expected <- as.character(coefficients$level[order(coefficients$relative_estimate,
      method = "radix")])
    expect_identical(p[[1]]$scales$get_scales("x")$limits, expected)
    expect_identical(p[[3]]$scales$get_scales("x")$limits, expected)
    expect_identical(unique(p[[1]]$data$component), component)
    expect_identical(unique(p[[3]]$data$component), component)
    expect_identical(unique(p[[4]]$data$component), component)
  }
  expect_identical(serialize(diagnostic, NULL), before)
})

ordered_mixed_cdi_fixture <- function(backend) {
  set.seed(20261012)
  d <- expand.grid(year = factor(2019:2022), vessel = factor(sprintf("V%02d", 1:6)),
    replicate = 1:15)
  d$depth <- runif(nrow(d))
  d$response <- 2 + .1 * as.integer(d$year) + .3 * d$depth +
    c(.9, -.8, .4, -.3, .1, -.6)[d$vessel] + rnorm(nrow(d), sd = .2)
  fit <- if (backend == "lme4") {
    lme4::lmer(response ~ year + depth + (1 | vessel), data = d, REML = FALSE)
  } else glmmTMB::glmmTMB(response ~ year + depth + (1 | vessel), data = d)
  influ(fit, focus = "year")
}

check_ordered_mixed_cdi <- function(diagnostic, term = "vessel") {
  before <- serialize(diagnostic, NULL)
  original <- plot(diagnostic, type = "cdi", term = term)
  for (direction in c("ascending", "descending")) {
    p <- plot(diagnostic, type = "cdi", term = term, coefficient_order = direction)
    expected <- as.character(original[[1]]$data$level[
      order(original[[1]]$data$estimate, decreasing = direction == "descending",
        method = "radix")])
    expect_identical(p[[1]]$scales$get_scales("x")$limits, expected)
    expect_identical(p[[3]]$scales$get_scales("x")$limits, expected)
    expect_identical(p[[1]]$data$estimate, original[[1]]$data$estimate)
    expect_identical(p[[3]]$data$proportion, original[[3]]$data$proportion)
    expect_identical(p[[4]]$data, original[[4]]$data)
    expect_s3_class(ggplot2::ggplotGrob(p[[1]]), "gtable")
    expect_s3_class(ggplot2::ggplotGrob(p[[3]]), "gtable")
  }
  expect_identical(serialize(diagnostic, NULL), before)
}

test_that("native lme4 random-effect CDI supports ordered categories", {
  skip_if_not_installed("lme4", "1.1.36")
  check_ordered_mixed_cdi(ordered_mixed_cdi_fixture("lme4"))
})

test_that("native glmmTMB random-effect CDI supports ordered categories", {
  skip_if_not_installed("glmmTMB")
  check_ordered_mixed_cdi(ordered_mixed_cdi_fixture("glmmTMB"))
})

test_that("saved brms group-level CDI uses the same ordering without refitting", {
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  fit <- readRDS(system.file("extdata", "brms-fixtures", "fit2.rds", package = "influ2"))
  diagnostic <- influ(fit, focus = "year")
  check_ordered_mixed_cdi(diagnostic, term = "month")
  p <- plot(diagnostic, type = "cdi", term = "month", coefficient_order = "ascending")
  expected <- p[[1]]$scales$get_scales("x")$limits
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path, width = 10, height = 7)
  on.exit({ grDevices::dev.off(); unlink(path) })
  print(p)
  drawn <- p[[1]]$scales$get_scales("x")$guide$cdi_state$drawn
  expect_identical(drawn$top$labels, expected)
  expect_identical(drawn$bottom$labels, expected)
  expect_identical(drawn$top$indices, drawn$bottom$indices)
  expect_equal(drawn$top$positions_mm, drawn$bottom$positions_mm)
})
