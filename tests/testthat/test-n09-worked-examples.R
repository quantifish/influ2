# Execute the actual article chunks, without optional model backends or refits.
n09_worked_example <- function() {
  sources <- c(test_path("..", "..", "vignettes", "residual-diagnostics.Rmd"),
    system.file("doc", "residual-diagnostics.Rmd", package = "influ2"))
  sources <- sources[nzchar(sources) & file.exists(sources)]
  if (!length(sources)) skip("The article source is unavailable in this installation")
  lines <- readLines(sources[1L], warn = FALSE)
  env <- new.env(parent = baseenv())
  env$plot <- graphics::plot
  # Preserve pkgload's source-tree system.file shim during test_local().
  env$system.file <- system.file
  ggplot_functions <- c("ggplot", "aes", "coord_cartesian", "labs",
    "geom_hline", "geom_point", "geom_smooth", "facet_grid", "labeller",
    "label_wrap_gen", "scale_fill_gradient2", "scale_x_continuous",
    "scale_y_continuous", "coord_equal", "theme", "element_text", "element_blank")
  for (name in ggplot_functions) env[[name]] <- getExportedValue("ggplot2", name)
  labels <- c("load", "align", "covariate-data", "covariate", "spatial-data", "spatial")
  for (label in paste0("residual-n09-", labels)) {
    start <- which(startsWith(lines, paste0("```{r ", label)) &
      substring(lines, nchar(paste0("```{r ", label)) + 1L,
        nchar(paste0("```{r ", label)) + 1L) %in% c(",", "}"))
    stopifnot(length(start) == 1L)
    end <- which(seq_along(lines) > start & trimws(lines) == "```")[1L]
    stopifnot(!is.na(end))
    eval(parse(text = lines[seq.int(start + 1L, end - 1L)]), envir = env)
  }
  env
}

test_that("N09 worked examples preserve saved rows, scores, and RNG state", {
  local_mocked_bindings(
    influ_residuals = function(...) stop("Do not recalculate these saved diagnostics"),
    .resid_adapter = function(...) stop("Do not simulate these saved examples"))
  set.seed(1515)
  rng <- .Random.seed
  e <- n09_worked_example()
  original <- readRDS(system.file("extdata", "n09-validation.rds", package = "influ2"))
  expect_identical(e$validation, original)
  expect_identical(e$mixed_example$replicate, 1L)
  expect_identical(e$spatial_example$replicate, 1L)
  for (spec in list(list(data = e$covariate_data, checks = e$mixed_checks),
                   list(data = e$spatial_data, checks = e$spatial_checks))) {
    expect_equal(nrow(spec$data), 960L)
    for (label in names(spec$checks)) {
      d <- spec$data[spec$data$model == label, ]
      checks <- spec$checks[[label]]
      expect_identical(d$row, checks$observations$row)
      expect_identical(d$residual, checks$observations$residual)
      expect_identical(d$pit, checks$observations$pit)
      expect_equal(d$residual, qnorm(d$pit))
      expect_identical(checks$metadata$conditioning, "fitted")
      expect_equal(checks$metadata$nsim, 499L)
      expect_equal(as.integer(table(d$year)), rep(80L, 6L))
    }
  }
  expect_identical(.Random.seed, rng)
})

test_that("article row matching handles permutations and rejects misalignment", {
  e <- n09_worked_example()
  d <- e$spatial_example$data
  checks <- e$spatial_checks[[1L]]
  columns <- c("station", "X", "Y")
  matched <- e$attach_covariates(checks, d, columns)
  expect_identical(e$attach_covariates(checks, d[rev(seq_len(nrow(d))), ], columns), matched)
  expect_equal(matched$X, d$X)
  expect_equal(matched$Y, d$Y)
  expect_error(e$attach_covariates(checks, d[-1L, ], columns))
  expect_error(e$attach_covariates(checks, d, "missing_column"))
  expect_error(e$attach_covariates(checks, d, "year"))
  bad <- checks
  bad$observations$row[1L] <- bad$observations$row[2L]
  expect_error(e$attach_covariates(bad, d, columns))
  bad$observations$row[1L] <- NA_character_
  expect_error(e$attach_covariates(bad, d, columns))
  changed <- d
  changed$response[1L] <- changed$response[1L] + 1
  expect_error(e$attach_covariates(checks, changed, columns))
  changed <- d
  changed$year[1L] <- 2020
  expect_error(e$attach_covariates(checks, changed, columns))
})

test_that("N09 covariate and spatial displays retain common scales and full support", {
  e <- n09_worked_example()
  panels <- c(e$qq_panels, e$covariate_panels)
  expect_length(panels, 4L)
  expect_s3_class(e$covariate_comparison, "patchwork")
  for (p in panels) {
    b <- ggplot2::ggplot_build(p)
    expect_equal(p$coordinates$limits$y, e$residual_limits)
    point <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1)))
    expect_length(point, 1L)
    expect_equal(nrow(b$data[[point]]), 480L)
    expect_true(all(is.finite(b$data[[point]]$y)))
  }
  p <- e$spatial_comparison
  b <- ggplot2::ggplot_build(p)
  expect_s3_class(p$coordinates, "CoordCartesian")
  expect_equal(p$coordinates$ratio, 1)
  expect_equal(p$coordinates$limits$x, c(0, 100))
  expect_equal(p$coordinates$limits$y, c(0, 100))
  expect_equal(nrow(b$layout$layout), 12L)
  expect_equal(as.integer(table(b$data[[1L]]$PANEL)), rep(80L, 12L))
  expect_equal(length(unique(b$data[[1L]]$size)), 1L)
  expect_equal(b$data[[1L]]$x, e$spatial_data$X)
  expect_equal(b$data[[1L]]$y, e$spatial_data$Y)
  scale <- b$plot$scales$get_scales("fill")
  expect_equal(scale$get_limits(), c(-e$colour_limit, e$colour_limit))
  expect_true(all(abs(e$spatial_data$residual) <= e$colour_limit))
  expect_equal(b$data[[1L]]$fill, scale$map(e$spatial_data$residual))
  expect_equal(length(unique(b$layout$layout$SCALE_X)), 1L)
  expect_equal(length(unique(b$layout$layout$SCALE_Y)), 1L)
})
