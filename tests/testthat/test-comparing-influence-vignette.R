comparison_article_chunk <- function(label) {
  candidates <- c(
    test_path("..", "..", "vignettes", "comparing-influence.Rmd"),
    system.file("doc", "comparing-influence.Rmd", package = "influ2"),
    file.path(Sys.getenv("GITHUB_WORKSPACE", unset = ""),
      "vignettes", "comparing-influence.Rmd")
  )
  path <- candidates[file.exists(candidates)][1L]
  if (is.na(path)) skip("The comparison article source is unavailable in this installation.")
  lines <- readLines(path, warn = FALSE)
  start <- which(startsWith(lines, paste0("```{r ", label, ",")) |
    lines == paste0("```{r ", label, "}"))
  if (length(start) != 1L) stop("Expected one comparison article chunk: ", label)
  end <- which(seq_along(lines) > start & lines == "```")[1L]
  parse(text = lines[seq.int(start + 1L, end - 1L)])
}

test_that("the comparison article uses real unequal-period fits without inventing gap estimates", {
  skip_if_not_installed("glmmTMB")
  e <- new.env(parent = environment())
  utils::data("lobsters_per_pot", package = "influ2", envir = e)
  for (label in c("comparison-lobster-models", "comparison-lobster-diagnostics",
      "comparison-cpue-indices", "comparison-index-ratio", "comparison-nominal-ratio")) {
    invisible(capture.output(eval(comparison_article_chunk(label), envir = e)))
  }
  expect_equal(nrow(e$full_data), 5049L)
  expect_identical(e$full_data, e$lobsters_per_pot)
  expect_equal(range(as.numeric(as.character(e$full_data$year))), c(2000, 2017))
  expect_equal(range(as.numeric(as.character(e$recent_data$year))), c(2007, 2017))
  expect_false("2012" %in% as.character(e$recent_data$year))
  for (fit in list(e$full_model, e$recent_model)) {
    expect_identical(fit$fit$convergence, 0L)
    expect_true(fit$sdr$pdHess)
    expect_identical(family(fit)$family, "nbinom2")
  }
  expect_equal(nobs(e$full_model), nrow(e$full_data))
  expect_equal(nobs(e$recent_model), nrow(e$recent_data))
  expect_named(e$diagnostics, c("Full", "Recent"))
  expect_true(all(vapply(e$diagnostics, inherits, logical(1), "influ_diag")))
  expect_false("2012" %in% e$recent_diagnostic$influence$level)
  expect_equal(nrow(e$full_index$table), 18L)
  expect_equal(nrow(e$recent_index$table), 10L)
  expect_false("2012" %in% as.character(e$recent_index$table$Year))
  expect_identical(e$full_index$metadata$reference_variables,
    e$recent_index$metadata$reference_variables)
  expect_identical(e$full_index$metadata$units, "lobsters per pot")
  expect_identical(e$recent_index$metadata$units, "lobsters per pot")
  expect_s3_class(e$recent_to_full, "influ_index_ratio")
  expect_s3_class(e$standardised_to_nominal, "influ_index_ratio")
  ratios <- e$recent_to_full$table
  matched <- ratios$status == "ok"
  expect_equal(sum(matched), 10L)
  expect_equal(ratios$ratio[matched],
    ratios$numerator_estimate[matched] / ratios$denominator_estimate[matched])
  expect_identical(ratios$status[ratios$year == "2012"], "missing_numerator")
  expect_true(all(is.na(ratios$ratio[!matched])))
  expect_identical(e$standardised_to_nominal$metadata$baseline_years,
    as.character(2007:2011))
  nominal_ratio <- e$standardised_to_nominal$table
  expect_equal(nominal_ratio$ratio,
    nominal_ratio$numerator_display / nominal_ratio$denominator_display)
  before <- serialize(e$diagnostics, NULL)
  union <- eval(comparison_article_chunk("comparison-cdi-union"), envir = e)
  overlap <- eval(comparison_article_chunk("comparison-cdi-overlap"), envir = e)
  expect_s3_class(union, "patchwork")
  expect_s3_class(overlap, "patchwork")
  displayed <- attr(union, "cdi_comparison")
  expect_length(displayed$term_levels, 12L)
  expect_equal(as.numeric(table(displayed$coefficients$level)), rep(2, 12L))
  expect_equal(displayed$coefficients$x - displayed$coefficients$category,
    unname(displayed$offsets[as.character(displayed$coefficients$Model)]))
  expect_equal(displayed$composition$x - displayed$composition$category,
    unname(displayed$offsets[as.character(displayed$composition$Model)]))
  expect_equal(length(attr(overlap, "cdi_comparison")$term_levels), 12L)
  expect_identical(serialize(e$diagnostics, NULL), before)
  expect_s3_class(plot_index_ratio(e$recent_to_full), "ggplot")
  expect_s3_class(plot_index_ratio(e$standardised_to_nominal), "ggplot")
})

test_that("the near-zero article example demonstrates an invariant ratio rather than fitted data", {
  e <- new.env(parent = environment())
  eval(comparison_article_chunk("comparison-near-zero-values"), envir = e)
  expect_equal(nrow(e$declining), 11L)
  expect_equal(e$declining$Ratio, rep(2, 11L))
  expect_equal(e$declining$Reference[1L] / tail(e$declining$Reference, 1L), 1000)
  expect_equal(e$declining$Alternative, 2 * e$declining$Reference)
})
