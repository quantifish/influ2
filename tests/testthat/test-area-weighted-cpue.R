area_cpue_article <- function() {
  sources <- c(test_path("..", "..", "vignettes", "cpue-indices.Rmd"),
    system.file("doc", "cpue-indices.Rmd", package = "influ2"))
  sources <- sources[nzchar(sources) & file.exists(sources)]
  if (!length(sources)) skip("The CPUE article source is unavailable")
  readLines(sources[1L], warn = FALSE)
}

area_cpue_chunk <- function(lines, label) {
  start <- which(startsWith(lines, paste0("```{r ", label, ",")))
  stopifnot(length(start) == 1L)
  end <- which(seq_along(lines) > start & trimws(lines) == "```")[1L]
  parse(text = lines[seq.int(start + 1L, end - 1L)])
}

test_that("the worked CPUE area example preserves units and joint native uncertainty", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("knitr")
  lines <- area_cpue_article()
  e <- new.env(parent = environment())
  utils::data("lobsters_per_pot", package = "influ2", envir = e)
  d <- e$lobsters_per_pot
  fit <- glmmTMB::glmmTMB(lobsters ~ year + poly(depth, 3) + poly(soak, 3) +
    (1 | month), family = glmmTMB::nbinom2(), data = d)
  expect_equal(fit$fit$convergence, 0)
  expect_true(fit$sdr$pdHess)
  e$lobster_model <- fit
  e$lobsters_per_pot <- d
  original <- fit
  withr::local_seed(271)
  rng <- .Random.seed
  for (label in c("index-cpue-area-reference", "index-cpue-area-mean",
      "index-cpue-area-native", "index-cpue-area-total")) {
    invisible(eval(area_cpue_chunk(lines, label), envir = e))
  }
  expect_identical(.Random.seed, rng)
  expect_identical(fit, original)
  domain <- e$cpue_domain
  expect_identical(domain$depth, c(15, 25, 35))
  expect_identical(domain$area_km2, c(10, 30, 60))
  expect_equal(sum(domain$area_proportion), 1)
  for (year in levels(d$year)) {
    depth_range <- range(d$depth[d$year == year])
    expect_true(all(domain$depth >= depth_range[1] & domain$depth <= depth_range[2]))
    soak_range <- range(d$soak[d$year == year])
    expect_true(all(domain$soak >= soak_range[1] & domain$soak <= soak_range[2]))
  }
  index <- e$area_cpue
  total <- e$area_cpue_total
  expect_identical(index$metadata$units, "lobsters per pot")
  expect_identical(index$metadata$method, "standardised")
  expect_identical(total$metadata$units, "lobsters * km^2 per pot")
  expect_identical(total$metadata$method, "integrated")
  expect_equal(total$metadata$total_area, 100)
  for (column in c("Mean", "SD", "Qlower", "Qupper")) {
    expect_equal(total$table[[column]], 100 * index$table[[column]])
  }
  expect_equal(total$table$CV, index$table$CV)
  proportional <- cpue_index(fit, reference_data = domain[c("depth", "soak")],
    reference_weights = domain$area_proportion, units = "lobsters per pot")
  expect_equal(proportional$table, index$table)
  expect_equal(index_vcov(proportional), index_vcov(index))
  # This additive log-link fit has one shared year effect: fixed weights can
  # change the raw level, not its relative annual pattern.
  ratio <- index$table$Mean / e$equal_habitat_cpue$table$Mean
  expect_lt(ratio[1], .98)
  expect_equal(ratio, rep(ratio[1], length(ratio)))
  expect_equal(index$table$Mean / geo_mean(index$table$Mean),
    e$equal_habitat_cpue$table$Mean / geo_mean(e$equal_habitat_cpue$table$Mean))
  # Averaging on the link scale is a different estimand, not this index.
  links <- predict(fit, e$prediction_rows, type = "link", re.form = NA)
  link_average <- exp(drop(e$W %*% links))
  expect_false(isTRUE(all.equal(link_average, index$table$Mean)))
  p <- plot_compare(list(`Area-weighted CPUE` = index,
    `Equal-habitat CPUE` = e$equal_habitat_cpue))
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  expect_error(plot_compare(list(Mean = index, Total = total)), "method")
})
