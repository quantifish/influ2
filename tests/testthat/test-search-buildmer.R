search_buildmer_data <- function() {
  withr::local_seed(481)
  d <- data.frame(
    year = factor(rep(2020:2023, each = 60)),
    vessel = factor(rep(1:12, 20)), x = rnorm(240), z = rnorm(240),
    exposure = runif(240, 0.8, 1.6)
  )
  effect <- rnorm(12, 0, 0.6)
  d$y <- rpois(nrow(d), d$exposure *
    exp(0.8 + 0.9 * d$x + effect[d$vessel]))
  d
}

search_buildmer_labels <- function(model) {
  sort(attr(stats::terms(stats::formula(model)), "term.labels"))
}

test_that("bounded backward BIC selection agrees with buildmer on native ML specifications", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("buildmer")
  d <- search_buildmer_data()
  f <- y ~ year + x + z + (1 | vessel) + offset(log(exposure))
  original <- glmmTMB::glmmTMB(f, data = d, family = poisson())
  ours <- search_cpue(original, year = "year", optional = ~ x + z,
    direction = "backward", criterion = "BIC", refit = TRUE)
  native <- buildmer::buildglmmTMB(f, data = d, family = poisson(),
    buildmerControl = buildmer::buildmerControl(
      direction = "backward", crit = "BIC", elim = "BIC",
      include = ~ year + (1 | vessel) + offset(log(exposure)),
      quiet = TRUE, calc.summary = FALSE, REML = FALSE
    ))
  chosen <- selected_model(ours)
  expect_equal(search_buildmer_labels(chosen), search_buildmer_labels(native@model))
  expect_true("x" %in% search_buildmer_labels(chosen))
  expect_false("z" %in% search_buildmer_labels(chosen))

  # buildmer can return a REML final fit even when fixed-effect comparisons
  # used ML. Compare the returned specifications under the same native ML
  # objective, rather than comparing different finalisation targets.
  native_ml <- glmmTMB::glmmTMB(formula(native@model), data = d,
    family = poisson(), REML = FALSE)
  expect_equal(BIC(chosen), BIC(native_ml), tolerance = 1e-6)
  expect_equal(logLik(chosen), logLik(native_ml), tolerance = 1e-6)
  expect_equal(attr(logLik(chosen), "df"), attr(logLik(native_ml), "df"))
  expect_false(chosen$modelInfo$REML)
  expect_equal(model.offset(model.frame(chosen)), log(d$exposure))
  expect_identical(row.names(model.frame(chosen)), row.names(d))
  expect_true(all(ours$candidates$eligible))
  expect_equal(anyDuplicated(ours$candidates$key), 0L)
  expect_equal(BIC(chosen), min(ours$candidates$score))
})

test_that("bounded forward BIC selection agrees with buildmer for protected mixed models", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("buildmer")
  d <- search_buildmer_data()
  f <- y ~ year + x + z + (1 | vessel) + offset(log(exposure))
  original <- glmmTMB::glmmTMB(f, data = d, family = poisson())
  ours <- search_cpue(original, year = "year", optional = ~ x + z,
    direction = "forward", criterion = "BIC", refit = TRUE)
  native <- buildmer::buildglmmTMB(f, data = d, family = poisson(),
    buildmerControl = buildmer::buildmerControl(
      direction = "forward", crit = "BIC", elim = "BIC",
      include = ~ year + (1 | vessel) + offset(log(exposure)),
      quiet = TRUE, calc.summary = FALSE, REML = FALSE
    ))
  chosen <- selected_model(ours)
  expect_equal(search_buildmer_labels(chosen), search_buildmer_labels(native@model))
  native_ml <- glmmTMB::glmmTMB(formula(native@model), data = d,
    family = poisson(), REML = FALSE)
  expect_equal(BIC(chosen), BIC(native_ml), tolerance = 1e-6)
  expect_equal(logLik(chosen), logLik(native_ml), tolerance = 1e-6)
  expect_true(all(grepl("vessel", ours$candidates$formula)))
  expect_true(all(grepl("offset", ours$candidates$formula)))
  expect_true(all(grepl("year", ours$candidates$formula)))
  expect_true(all(ours$path$improvement[-1L] > 1e-6))
  expect_equal(anyDuplicated(ours$candidates$key), 0L)
  expect_true(all(ours$candidates$eligible))
  expect_equal(nobs(chosen), nobs(original))
})

search_vignette_chunk <- function(label) {
  candidates <- c(test_path("..", "..", "vignettes", "model-search.Rmd"),
    system.file("doc", "model-search.Rmd", package = "influ2"))
  path <- candidates[file.exists(candidates)][1L]
  if (is.na(path)) stop("The model-search vignette source is unavailable.")
  lines <- readLines(path, warn = FALSE)
  start <- grep(paste0("^```\\{r ", label, "(,|\\})"), lines)
  if (length(start) != 1L) stop("Expected one vignette chunk: ", label)
  ending <- which(lines == "```")
  end <- ending[ending > start][1L]
  parse(text = lines[seq.int(start + 1L, end - 1L)])
}

test_that("the worked lobster search vignette evaluates the real shared dataset and index", {
  skip_if_not_installed("glmmTMB")
  e <- new.env(parent = environment())
  utils::data("lobsters_per_pot", package = "influ2", envir = e)
  for (label in c("search-lobster-model", "search-lobster-preflight",
      "search-lobster-run", "search-lobster-index", "search-lobster-random")) {
    invisible(capture.output(eval(search_vignette_chunk(label), envir = e)))
  }
  expect_s3_class(e$search_plan, "influ_search")
  expect_identical(e$search_plan$metadata$stop_reason, "preflight_only")
  expect_equal(nrow(e$search_plan$candidates), 0L)
  expect_true(all(e$search_plan$terms$protected[
    e$search_plan$terms$term %in% c("year", "1 | month")]))
  expect_identical(e$lobster_search$metadata$criterion, "BIC")
  expect_true(all(e$lobster_search$candidates$eligible))
  expect_equal(nrow(e$lobster_search$candidates), 4L)
  expect_equal(anyDuplicated(e$lobster_search$candidates$key), 0L)
  expect_equal(search_buildmer_labels(e$recommended_model),
    search_buildmer_labels(e$lobster_model))
  expect_equal(BIC(e$recommended_model), min(e$lobster_search$candidates$score))
  expect_equal(nobs(e$recommended_model), nrow(e$lobsters_per_pot))
  expect_s3_class(e$recommended_index, "influ_index")
  expect_equal(nrow(index_table(e$recommended_index)),
    nlevels(e$lobsters_per_pot$year))
  expect_identical(e$recommended_index$metadata$units, "lobsters per pot")
  expect_true(all(is.finite(index_table(e$recommended_index)$Mean)))
  expect_s3_class(plot(e$lobster_search), "ggplot")
  expect_s3_class(plot_index(e$recommended_index), "ggplot")
  expect_true(all(e$month_search$candidates$eligible))
  expect_equal(nrow(e$month_search$candidates), 2L)
  expect_true(all(e$month_search$terms$protected[
    e$month_search$terms$term %in% c("year", "poly(depth, 3)", "poly(soak, 3)")]))
  expect_false(e$month_search$terms$protected[
    e$month_search$terms$term == "1 | month"])
  expect_true(all(grepl("poly\\(depth, 3\\)", e$month_search$candidates$formula)))
  expect_true(all(grepl("poly\\(soak, 3\\)", e$month_search$candidates$formula)))
})
