test_that("a sole interaction is not mistaken for an additive focus index", {
  fixture <- bentley_fixture()
  model <- stats::glm(catch ~ year:area + vessel,
    family = stats::poisson(), data = fixture$data)
  expect_warning(diagnostic <- influ(model, focus = "year"),
    "focus term also depends on another variable")
  expect_identical(unique(diagnostic$indices$series), "nominal")
  expect_true("year:area" %in% diagnostic$influence$term)
  expect_true(all(is.finite(diagnostic$influence$estimate)))

  expect_warning(grid <- influ(model, focus = "year",
    reference_data = fixture$data), "reference_data alone is insufficient")
  expect_false(any(grid$indices$series == "standardised"))
  expect_error(.focus_link_effect(diagnostic, fixture$data, "year"),
    "does not depend on another variable")
  expect_error(influ_steps(list(Interaction = diagnostic), year = "year"),
    "standardised year-effect index")
})

test_that("focus-only transformations remain distinct from joint terms", {
  for (term in c("year", "factor(year)", "as.factor(year)",
      "s(year)", "poly(year, 2)", "I(year^2)")) {
    expect_true(.is_pure_focus_term(term, "year"), info = term)
  }
  for (term in c("year:area", "I(year * depth)", "te(year, depth)",
      "s(year, by = area)", "vessel", "1")) {
    expect_false(.is_pure_focus_term(term, "year"), info = term)
  }
  expect_true(.is_pure_focus_term("`fishing year`", "fishing year"))
})

test_that("transformed single-variable focus indices remain available", {
  fixture <- bentley_fixture()
  fixture$data$year <- as.numeric(fixture$data$year)
  model <- stats::glm(catch ~ factor(year) + area + vessel,
    family = stats::poisson(), data = fixture$data)
  expect_no_warning(diagnostic <- influ(model, focus = "year"))
  expect_true(any(diagnostic$indices$series == "standardised"))
})

test_that("combined-component index construction rejects a joint focus term", {
  fixture <- bentley_fixture()
  diagnostic <- influ(fixture$model, focus = "year")
  influence <- subset(diagnostic$influence, term == "year")
  influence$term <- "year:area"
  influence$component <- "unconditional_mean"
  expect_warning(index <- .standardised_indices(influence, "year", "year:area"),
    "focus term also depends on another variable")
  expect_equal(nrow(index), 0L)
})
