test_that("diagnostic constructors enforce the public table contract", {
  diagnostic <- influ(bentley_fixture()$model, focus = "year")
  make <- function(influence = diagnostic$influence, ...) {
    new_influ_diag("glm", diagnostic$family, "year", influence, ...)
  }
  minimal <- make()
  expect_s3_class(minimal, "influ_diag_glm")
  for (table in c("coefficients", "composition", "indices", "metrics")) {
    expect_identical(minimal[[table]], data.frame())
  }
  expect_null(minimal$draws)
  expect_null(minimal$model)

  bad <- diagnostic$influence
  bad$method <- NULL
  expect_error(make(bad), "missing required columns: method")
  expect_error(make(diagnostic$influence[FALSE, ]), "at least one row")
  for (column in c("estimate", "std_error", "lower", "upper")) {
    bad <- diagnostic$influence
    bad[[column]] <- as.character(bad[[column]])
    expect_error(make(bad), "must be numeric")
  }
  for (focus in c("month", NA_character_)) {
    bad <- diagnostic$influence
    bad$focus[1] <- focus
    expect_error(make(bad), "object's focus variable", fixed = TRUE)
  }
  bad <- diagnostic$influence
  bad$lower[] <- bad$upper[] <- NA_real_
  expect_identical(make(bad)$influence, bad)
})

test_that("extractors, printing, and summaries preserve the diagnostic", {
  diagnostic <- influ(bentley_fixture()$model, focus = "year")
  expect_identical(influ_composition(diagnostic), diagnostic$composition)
  expect_identical(influ_indices(diagnostic), diagnostic$indices)
  for (extract in list(influ_effects, influ_indices, influ_composition,
                       influ_draws, influ_metrics)) {
    expect_error(extract(list()), "must be an influ_diag object")
  }
  expect_error(influ(structure(list(), class = c("unfitted", "list"))),
               "No influ.*unfitted/list")

  # Printing should identify the backend and return the same object invisibly.
  printed <- capture.output(result <- withVisible(print(diagnostic)))
  expect_identical(result$value, diagnostic)
  expect_false(result$visible)
  expect_true(any(grepl("Backend: +glm", printed)))
  expect_true(any(grepl("Focus: +year", printed)))
  diagnostic$uncertainty <- diagnostic$retained <- list()
  printed <- capture.output(print(diagnostic))
  expect_true(any(grepl("Uncertainty: +none", printed)))
  expect_true(any(grepl("Retained: +summary", printed)))

  summary <- summary(diagnostic)
  expect_s3_class(summary, "summary.influ_diag")
  printed <- capture.output(result <- withVisible(print(summary)))
  expect_identical(result$value, summary)
  expect_false(result$visible)
  expect_true(any(grepl("Influence diagnostic summary", printed)))
  for (term in summary$term_summary$term) {
    rows <- subset(diagnostic$influence, scale == "link")
    rows <- rows[rows$term == term, ]
    best <- which.max(abs(rows$estimate))
    observed <- summary$term_summary[summary$term_summary$term == term, ]
    expect_equal(observed$maximum_absolute_link_influence, abs(rows$estimate[best]))
    expect_identical(observed$level_at_maximum, as.character(rows$level[best]))
  }
  # Saved diagnostics may contain only the natural-scale table.
  diagnostic$influence <- subset(diagnostic$influence, scale != "link")
  natural <- summary(diagnostic)$term_summary
  expect_setequal(natural$term, unique(diagnostic$influence$term))
  for (term in natural$term) {
    expect_equal(natural$maximum_absolute_link_influence[natural$term == term],
                 max(abs(diagnostic$influence$estimate[diagnostic$influence$term == term])))
  }
})
