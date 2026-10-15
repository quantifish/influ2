cdi_multiple_fixture <- function() {
  d <- expand.grid(year = factor(c(2000, 2001, 2003, 2004)),
    area = factor(c("A", "B", "C")), replicate = 1:4)
  d$y <- 1L + (as.integer(d$year) * as.integer(d$area) + d$replicate) %% 7L
  full <- stats::glm(y ~ year + area, data = d, family = poisson())
  recent_data <- droplevels(subset(d, year != "2000" & area != "B"))
  recent <- stats::glm(y ~ year + area, data = recent_data, family = poisson())
  list(Full = influ(full, focus = "year"), Recent = influ(recent, focus = "year"))
}

test_that("a single public CDI delegates without altering the established display", {
  d <- cdi_multiple_fixture()$Full
  direct <- plot(d, type = "cdi", term = "area")
  wrapped <- plot_cdi(d, term = "area")
  expect_s3_class(wrapped, "patchwork")
  expect_null(attr(wrapped, "cdi_comparison"))
  for (i in c(1L, 3L, 4L)) {
    expect_equal(direct[[i]]$data, wrapped[[i]]$data)
    expect_equal(ggplot2::ggplot_build(direct[[i]])$data,
      ggplot2::ggplot_build(wrapped[[i]])$data)
  }
  expect_equal(plot_cdi(list(One = d), term = "area",
    coefficient_reference = "model")[[1L]]$data,
    plot(d, type = "cdi", term = "area", coefficient_reference = "model")[[1L]]$data)
  expect_error(plot_cdi(d, term = "area", colours = "red"), "multiple-model option")
})

test_that("overlaid CDI uses fixed matching model offsets in every category cell", {
  diagnostics <- cdi_multiple_fixture()
  before <- serialize(diagnostics, NULL)
  p <- plot_cdi(diagnostics, term = "area")
  d <- attr(p, "cdi_comparison")
  expect_identical(serialize(diagnostics, NULL), before)
  expect_identical(d$labels, c("Full", "Recent"))
  expect_identical(d$term_levels, c("A", "B", "C"))
  expect_equal(unname(d$offsets), c(-.175, .175))
  for (model in d$labels) {
    a <- subset(d$coefficients, Model == model)
    b <- subset(d$composition, Model == model)
    expect_equal(a$x - a$category, rep(d$offsets[[model]], nrow(a)))
    expect_equal(b$x - b$category, rep(d$offsets[[model]], nrow(b)))
    expect_equal(b$x, a$x[match(b$term_level, a$level)])
    stored <- subset(diagnostics[[model]]$coefficients, term == "area")
    expect_equal(a$estimate, stored$relative_estimate)
    expect_equal(a$lower, stored$relative_lower)
    expect_equal(a$upper, stored$relative_upper)
  }
  expect_false(any(d$coefficients$Model == "Recent" & d$coefficients$level == "B"))
  expect_false(any(d$composition$Model == "Recent" & d$composition$term_level == "B"))
  expect_identical(p[[1]]$scales$get_scales("x")$breaks,
    p[[3]]$scales$get_scales("x")$breaks)
  expect_identical(p[[1]]$scales$get_scales("x")$labels,
    p[[3]]$scales$get_scales("x")$labels)
  expect_identical(p[[1]]$scales$get_scales("x")$position, "top")
  expect_identical(p[[4]]$scales$get_scales("x")$position, "top")
  expect_identical(p[[3]]$scales$get_scales("y")$breaks,
    p[[4]]$scales$get_scales("x")$breaks)
  expect_identical(p[[3]]$scales$get_scales("y")$labels,
    p[[4]]$scales$get_scales("x")$labels)
  expect_match(p$patches$annotation$caption, "own centring reference")
  expect_false(d$reference_verified_shared)
  expect_equal(d$interval_probabilities, lapply(diagnostics, function(x) x$uncertainty$probs))
  expect_null(d$model)
  expect_null(d$draws)
  expect_null(d$axis_state)
})

test_that("year union and overlap preserve chronological gaps without bridging lines", {
  diagnostics <- cdi_multiple_fixture()
  union <- attr(plot_cdi(diagnostics, term = "area"), "cdi_comparison")
  overlap <- attr(plot_cdi(diagnostics, term = "area", years = "overlap"), "cdi_comparison")
  expect_identical(union$year_levels, as.character(2000:2004))
  expect_identical(overlap$year_levels, as.character(2001:2004))
  expect_false("2002" %in% union$influence$level)
  expect_false("2002" %in% union$composition$level)
  expect_false(any(union$composition$Model == "Recent" & union$composition$level == "2000"))
  for (model in union$labels) {
    d <- subset(union$influence, Model == model)
    expect_identical(d$segment, cumsum(c(TRUE, diff(d$year_position) != 1L)))
    expect_equal(d$estimate,
      subset(diagnostics[[model]]$influence, term == "area" & scale == "ratio")$estimate)
    expect_equal(subset(overlap$coefficients, Model == model)$estimate,
      subset(union$coefficients, Model == model)$estimate)
  }
  reversed <- diagnostics
  reversed[[1]]$influence <- reversed[[1]]$influence[nrow(reversed[[1]]$influence):1L, ]
  expect_identical(attr(plot_cdi(reversed, term = "area"),
    "cdi_comparison")$year_levels, union$year_levels)
  disjoint <- diagnostics
  disjoint$Recent$influence$level <- as.character(as.integer(disjoint$Recent$influence$level) + 20)
  disjoint$Recent$composition$level <- as.character(as.integer(disjoint$Recent$composition$level) + 20)
  expect_error(plot_cdi(disjoint, term = "area", years = "overlap"), "no overlapping years")
})

test_that("non-finite influence values split lines without inventing zeroes", {
  diagnostics <- cdi_multiple_fixture()
  rows <- diagnostics$Full$influence$term == "area" &
    diagnostics$Full$influence$level == "2001"
  diagnostics$Full$influence$estimate[rows] <- NA_real_
  d <- attr(plot_cdi(diagnostics, term = "area"), "cdi_comparison")$influence
  full <- subset(d, Model == "Full")
  expect_true(is.na(full$estimate[full$level == "2001"]))
  expect_equal(length(unique(full$segment)), 3L)
})

test_that("calendar and fishing-year labels only expand when unambiguous", {
  expect_identical(.cdi_year_axis(c("2003", "2000"))$labels, as.character(2000:2003))
  expect_identical(.cdi_year_axis(c("2003/04", "2000/01"))$labels,
    c("2000/01", "2001/02", "2002/03", "2003/04"))
  expect_identical(.cdi_year_axis(c("1999-2000", "2002-2003"))$labels,
    c("1999-2000", "2000-2001", "2001-2002", "2002-2003"))
  expect_identical(.cdi_year_axis(c("1998/99", "2000/01"))$labels,
    c("1998/99", "1999/00", "2000/01"))
  for (labels in list(c("Before", "After"), c("2001/04", "2003/04"),
      c("2000/01", "2003-04"), c("02000", "02003"), c("1", "01"))) {
    expect_identical(.cdi_year_axis(labels)$labels, labels)
  }
})

test_that("model palettes and bubble proportions share single consistent scales", {
  diagnostics <- cdi_multiple_fixture()
  colours <- c(Recent = "orange", Full = "purple4")
  p <- plot_cdi(diagnostics, term = "area", colours = colours)
  d <- attr(p, "cdi_comparison")
  expect_identical(d$colours, c(Full = "purple4", Recent = "orange"))
  for (i in c(1L, 3L, 4L)) {
    scale <- p[[i]]$scales$get_scales("colour")
    expect_identical(scale$limits, d$labels)
    expect_equal(unname(scale$palette(2)), c("purple4", "orange"))
  }
  scale <- p[[3]]$scales$get_scales("size")
  expect_equal(scale$limits, c(0, max(d$composition$proportion)))
  expect_lte(length(scale$breaks(scale$limits)), 4L)
  # The extracted top-right legend contains both Model and Proportion guides.
  legend <- attr(p[[2]], "grobs")$full
  expect_s3_class(legend, "gtable")
  expect_equal(sum(legend$layout$name == "guides"), 2L)
  expect_identical(p[[3]]$guides$guides$size$params$ncol, 1)
  expect_error(plot_cdi(diagnostics, term = "area", colours = "red"), "one valid colour")
  expect_error(plot_cdi(diagnostics, term = "area", colours = c("invalid-colour", "red")), "valid colour")
  expect_error(plot_cdi(diagnostics, term = "area", colours = c(A = "red", B = "blue")), "match the model labels")
  expect_error(plot_cdi(diagnostics, term = "area", colours = c(Full = "red", Full = "blue")), "match the model labels")
})

test_that("explicit coefficient ordering uses one model and retains absent levels", {
  diagnostics <- cdi_multiple_fixture()
  rows <- diagnostics$Recent$coefficients$term == "area"
  diagnostics$Recent$coefficients$relative_estimate[rows] <- c(3, 1)
  p <- plot_cdi(diagnostics, term = "area", coefficient_order = "ascending", order_by = "Recent")
  d <- attr(p, "cdi_comparison")
  expect_identical(d$term_levels, c("C", "A", "B"))
  expect_equal(subset(d$coefficients, Model == "Recent")$category, c(2, 1))
  expect_identical(attr(plot_cdi(diagnostics, term = "area", coefficient_order = "descending",
    order_by = 2), "cdi_comparison")$term_levels, c("A", "C", "B"))
  expect_error(plot_cdi(diagnostics, term = "area", coefficient_order = "ascending"), "Supply `order_by`")
  for (invalid in list("missing", c(1, 2), NA_real_, Inf, .5, 0, 3)) {
    expect_error(plot_cdi(diagnostics, term = "area", coefficient_order = "ascending",
      order_by = invalid), "identify one model")
  }
  expect_error(plot_cdi(diagnostics, term = "area", order_by = "Full"), "only used")
})

test_that("multi-model CDI refuses incompatible semantics and unverifiable coding", {
  diagnostics <- cdi_multiple_fixture()
  expect_error(plot_cdi(diagnostics, term = "area", coefficient_reference = "model"), "coding cannot be verified")
  expect_error(plot_cdi(diagnostics, term = c("area", "year")), "one term")
  expect_error(plot_cdi(diagnostics, term = "missing"), "Every model must contain CDI coefficient")
  expect_error(plot_cdi(diagnostics, term = "area", component = c("conditional", "positive")), "one component")
  expect_error(plot_cdi(diagnostics, term = "area", year = "fyear"), "match the focus")
  expect_error(plot_cdi(diagnostics, term = "area", labels = c("A", " A ")), "unique")
  expect_error(plot_cdi(diagnostics, term = "area", labels = "A"), "unique")
  expect_error(plot_cdi(diagnostics, term = "area", labels = c("A", NA)), "unique")
  expect_error(plot_cdi(diagnostics, term = "area", reference_data = data.frame()), "cannot recalculate saved")
  for (field in c("component", "complement", "link", "cdi_scale")) {
    changed <- diagnostics
    if (field == "component") {
      for (table in c("coefficients", "composition", "influence")) {
        changed$Recent[[table]]$component <- "positive"
      }
    } else changed$Recent$coefficients[[field]] <- switch(field,
      complement = TRUE, link = "identity", cdi_scale = "link")
    # A natural log-response ratio can originate from a lognormal identity link.
    if (field == "link") {
      expect_s3_class(plot_cdi(changed, term = "area"), "patchwork")
      expect_error(plot_cdi(changed, term = "area", coefficient_scale = "link"), "compatible links")
    } else expect_error(plot_cdi(changed, term = "area"), "same component|compatible links")
  }
  changed <- diagnostics
  changed$Recent$focus <- "fyear"
  expect_error(plot_cdi(changed, term = "area"), "same focus")
  changed <- diagnostics
  rows <- changed$Recent$influence$term == "area" & changed$Recent$influence$scale == "ratio"
  changed$Recent$influence$scale[rows] <- "difference"
  expect_error(plot_cdi(changed, term = "area"), "compatible links")
})

test_that("bin labels and malformed summary cells fail explicitly", {
  diagnostics <- cdi_multiple_fixture()
  changed <- diagnostics
  changed$Full$coefficients$level[changed$Full$coefficients$term == "area"] <- c("[0,1]", "(1,2]", "(2,3]")
  rows <- changed$Full$composition$term == "area"
  changed$Full$composition$term_level[rows] <- c("[0,1]", "(1,2]", "(2,3]")[
    match(changed$Full$composition$term_level[rows], c("A", "B", "C"))]
  expect_error(plot_cdi(changed, term = "area"), "Continuous CDI bins")
  changed <- diagnostics
  row <- which(changed$Full$composition$term == "area")[1L]
  changed$Full$composition$proportion[row] <- -1
  expect_error(plot_cdi(changed, term = "area"), "between zero and one")
  changed <- diagnostics
  row <- which(changed$Full$composition$term == "area")[1L]
  changed$Full$composition <- rbind(changed$Full$composition, changed$Full$composition[row, ])
  expect_error(plot_cdi(changed, term = "area"), "unique within")
  changed <- diagnostics
  rows <- changed$Full$composition$term == "area"
  changed$Full$composition$term_level[which(rows)[1L]] <- "missing"
  expect_error(plot_cdi(changed, term = "area"), "must match the stored")
  changed <- diagnostics
  changed$Full$coefficients <- changed$Full$coefficients[
    changed$Full$coefficients$term != "area", ]
  expect_error(plot_cdi(changed, term = "area"), "Every model must contain CDI coefficient")
  changed <- diagnostics
  rows <- changed$Full$influence$term == "area" & changed$Full$influence$scale != "link"
  changed$Full$influence$upper[which(rows)[1L]] <- Inf
  expect_error(plot_cdi(changed, term = "area"), "finite or missing")
  changed <- diagnostics
  rows <- changed$Full$coefficients$term == "area"
  changed$Full$coefficients$centred_upper[which(rows)[1L]] <- Inf
  expect_error(plot_cdi(changed, term = "area", coefficient_scale = "link"), "finite or missing")
})

test_that("aggregate random effects require matching, identified grouping variables", {
  diagnostics <- cdi_multiple_fixture()
  for (i in seq_along(diagnostics)) {
    for (table in c("coefficients", "composition", "influence")) {
      rows <- diagnostics[[i]][[table]]$term == "area"
      diagnostics[[i]][[table]]$term[rows] <- "random_effects"
    }
  }
  expect_error(plot_cdi(diagnostics, term = "random_effects"), "verified single random-intercept")
  diagnostics$Full$metadata$lme4 <- list(random_cdi_group = "month")
  diagnostics$Recent$metadata$glmmTMB <- list(random_cdi_group = "month")
  p <- plot_cdi(diagnostics, term = "random_effects")
  expect_identical(attr(p, "cdi_comparison")$strata,
    list(kind = "random_intercept", group = "month"))
  diagnostics$Recent$metadata$glmmTMB$random_cdi_group <- "vessel"
  expect_error(plot_cdi(diagnostics, term = "random_effects"), "same identified grouping")
  for (term in c("spatial_field", "spatiotemporal_field", "iid_random_effects",
      "smooths", "time_varying_effects", "spatially_varying_effects")) {
    expect_error(.cdi_strata_signature(diagnostics$Full, term), "Aggregate latent-field")
  }
})

test_that("zero proportions are absent and preview uncertainty is retained as missing", {
  diagnostics <- cdi_multiple_fixture()
  row <- which(diagnostics$Full$composition$term == "area")[1L]
  diagnostics$Full$composition$proportion[row] <- 0
  diagnostics$Recent$coefficients$relative_lower <- NA_real_
  diagnostics$Recent$coefficients$relative_upper <- NA_real_
  d <- attr(plot_cdi(diagnostics, term = "area"), "cdi_comparison")
  expect_true(all(d$composition$proportion > 0))
  expect_true(all(is.na(subset(d$coefficients, Model == "Recent")$lower)))
  expect_true(all(is.na(subset(d$coefficients, Model == "Recent")$upper)))
})

test_that("model inputs calculate diagnostics without updating or refitting models", {
  fixture <- bentley_fixture()
  reference <- influ(fixture$model, focus = "year")
  local_mocked_bindings(update = function(...) stop("Unexpected refit"), .package = "stats")
  p <- plot_cdi(list(First = fixture$model, Second = fixture$model),
    year = "year", term = "area", uncertainty = "none")
  d <- attr(p, "cdi_comparison")
  expect_equal(subset(d$coefficients, Model == "First")$estimate,
    subset(reference$coefficients, term == "area")$relative_estimate)
  expect_true(all(is.na(d$coefficients$lower)))
  expect_true(all(is.na(d$influence$lower)))
})

test_that("compatible fixed-effect CDI overlays work across GLM and GAM backends", {
  skip_if_not_installed("mgcv")
  fixture <- bentley_fixture()
  gam <- mgcv::gam(catch ~ year + area + vessel,
    data = fixture$data, family = poisson())
  p <- plot_cdi(list(GLM = fixture$model, GAM = gam), year = "year", term = "area")
  d <- attr(p, "cdi_comparison")
  expect_identical(d$labels, c("GLM", "GAM"))
  expect_equal(subset(d$coefficients, Model == "GLM")$estimate,
    subset(d$coefficients, Model == "GAM")$estimate, tolerance = 1e-8)
  expect_equal(subset(d$influence, Model == "GLM")$estimate,
    subset(d$influence, Model == "GAM")$estimate, tolerance = 1e-8)
})

test_that("paired overlaid CDI axes remain aligned after drawing and serialisation", {
  p <- plot_cdi(cdi_multiple_fixture(), term = "area")
  path <- tempfile(fileext = ".pdf")
  on.exit(unlink(path))
  grDevices::pdf(path, width = 10, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_warning(print(p), NA)
  state <- p[[1]]$scales$get_scales("x")$guide$cdi_state
  expect_identical(state$drawn$top$labels, state$drawn$bottom$labels)
  expect_equal(state$drawn$top$positions_mm, state$drawn$bottom$positions_mm)
  restored <- unserialize(serialize(p, NULL))
  expect_warning(print(restored), NA)
  expect_identical(attr(restored, "cdi_comparison")$offsets,
    attr(p, "cdi_comparison")$offsets)
})
