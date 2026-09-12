pit_plot_fixture <- function() {
  set.seed(932)
  d <- data.frame(year = factor(rep(2010:2013, each = 10)),
    cpue = rgamma(40, shape = 2, rate = 1))
  influ_residuals(glm(cpue ~ year, Gamma(link = "log"), data = d), nsim = 40, seed = 18)
}

test_that("PIT limits use the binomial grid identified by the frozen N09 audit", {
  skip_if_not_installed("bayesplot", "1.16.0")
  frozen <- readRDS(system.file("extdata", "n09-validation.rds", package = "influ2"))
  x <- pit_plot_fixture()
  x$observations <- data.frame(pit = (seq_len(480) - 0.5) / 480)
  original <- x
  a <- ggplot2::ggplot_build(suppressMessages(plot(x, type = "pit_ecdf")))$data
  b <- ggplot2::ggplot_build(suppressMessages(plot(x, type = "pit_ecdf_diff")))$data
  grid <- (0:100) / 100
  expect_equal(a[[1]]$x, grid)
  expect_equal(a[[1]]$y, c(0, frozen$bands$upper))
  expect_equal(a[[2]]$y, c(0, frozen$bands$lower))
  expect_equal(a[[3]]$y, ecdf(x$observations$pit)(grid))
  for (i in 1:3) expect_equal(b[[i]]$y, a[[i]]$y - a[[i]]$x)
  expect_identical(x, original)
})

test_that("PIT plots delegate stored ranks and preserve results and global state", {
  skip_if_not_installed("bayesplot", "1.16.0")
  x <- pit_plot_fixture()
  original <- x
  before_rng <- .Random.seed
  before_theme <- ggplot2::theme_get()
  before_bayes_theme <- bayesplot::bayesplot_theme_get()
  before_colours <- bayesplot::color_scheme_get()
  local_mocked_bindings(.resid_adapter = function(...) stop("must not simulate"))
  regular <- suppressMessages(plot(x, type = "pit_ecdf", pit_grid_size = 20))
  difference <- suppressMessages(plot(x, type = "pit_ecdf_diff", pit_grid_size = 20))
  a <- ggplot2::ggplot_build(regular)$data
  b <- ggplot2::ggplot_build(difference)$data
  grid <- (0:20) / 20
  expect_equal(a[[3]]$x, grid)
  expect_equal(a[[3]]$y, ecdf(x$observations$pit)(grid))
  for (i in 1:3) expect_equal(b[[i]]$y, a[[i]]$y - grid)
  native <- suppressMessages(bayesplot::ppc_pit_ecdf(pit = x$observations$pit,
    K = 20, prob = x$metadata$level, method = "independent", interpolate_adj = FALSE))
  native_data <- ggplot2::ggplot_build(native)$data
  expect_equal(a[[1]]$y, c(0, native_data[[1]]$y))
  expect_equal(a[[2]]$y, c(0, native_data[[2]]$y))
  expect_identical(attr(regular, "pit_reference")$evaluation_points, 21)
  expect_equal(a[[4]]$slope, 1)
  expect_equal(b[[4]]$yintercept, 0)
  expect_identical(attr(regular, "pit_reference")$method, "independent")
  expect_match(regular$labels$subtitle, "simultaneous iid-uniform reference", fixed = TRUE)
  expect_match(difference$labels$subtitle, "not a calibrated test", fixed = TRUE)
  expect_identical(x, original)
  expect_identical(.Random.seed, before_rng)
  expect_identical(ggplot2::theme_get(), before_theme)
  expect_identical(bayesplot::bayesplot_theme_get(), before_bayes_theme)
  expect_identical(bayesplot::color_scheme_get(), before_colours)
  expect_equal(a, ggplot2::ggplot_build(suppressMessages(
    ggplot2::autoplot(x, type = "pit_ecdf", pit_grid_size = 20, response_scale = "log1p")))$data)
})

test_that("PIT panels accept all backend summaries without treating them as LOO-PIT", {
  skip_if_not_installed("bayesplot", "1.16.0")
  x <- pit_plot_fixture()
  x$observations <- x$observations[1:3, ]
  x$observations$pit <- c(0, 0.5, 1)
  x$metadata$level <- 0.8
  for (backend in c("glm", "gam", "glmmTMB", "brms", "sdmTMB", "tinyVAST")) {
    x$metadata$backend <- backend
    p <- suppressMessages(plot(x, type = "pit_ecdf", pit_grid_size = 2))
    expect_equal(ggplot2::ggplot_build(p)$data[[3]]$y, c(1 / 3, 2 / 3, 1))
    expect_match(p$labels$subtitle, "80% simultaneous", fixed = TRUE)
  }
})

test_that("PIT endpoint masses and ties are retained on the common grid", {
  skip_if_not_installed("bayesplot", "1.16.0")
  x <- pit_plot_fixture()
  for (pit in list(c(0, 0, 0, 1), c(0, .5, .5, 1), rep(1, 4), rep(0, 4))) {
    x$observations <- data.frame(pit = pit)
    for (K in c(2L, 3L, 7L)) {
      a <- ggplot2::ggplot_build(suppressMessages(plot(x, type = "pit_ecdf", pit_grid_size = K)))$data
      b <- ggplot2::ggplot_build(suppressMessages(plot(x, type = "pit_ecdf_diff", pit_grid_size = K)))$data
      grid <- (0:K) / K
      expect_equal(a[[3]]$y, vapply(grid, function(u) mean(pit <= u), numeric(1)))
      expect_equal(a[[1]]$y[c(1, K + 1L)], c(0, 1))
      expect_equal(a[[2]]$y[c(1, K + 1L)], c(0, 1))
      for (i in 1:3) expect_equal(b[[i]]$y, a[[i]]$y - grid)
    }
  }
})

test_that("already aligned upstream plots are not corrected twice", {
  # Emulate public plots with known binomial limits, without private bayesplot APIs.
  make_plot <- function(grid, upper, lower, pit) {
    ggplot2::ggplot(data.frame(x = grid, upper, lower, empirical = ecdf(pit)(grid))) +
      ggplot2::geom_step(ggplot2::aes(x = .data$x, y = .data$upper)) +
      ggplot2::geom_step(ggplot2::aes(x = .data$x, y = .data$lower)) +
      ggplot2::geom_step(ggplot2::aes(x = .data$x, y = .data$empirical))
  }
  pit <- c(.1, .4, .6, .9)
  for (with_zero in c(FALSE, TRUE)) {
    grid <- if (with_zero) c(0, .5, 1) else c(.5, 1)
    p <- make_plot(grid, rep(1, length(grid)), c(rep(0, length(grid) - 1), 1), pit)
    if (with_zero) p$data$upper[1] <- 0
    aligned <- .resid_align_pit_plot(p, pit, 2, TRUE, version = "1.17.0")
    d <- ggplot2::ggplot_build(aligned)$data
    expect_equal(d[[1]]$x, c(0, .5, 1))
    expect_equal(d[[1]]$y, c(0, .5, 0))
    expect_equal(d[[2]]$y, c(0, -.5, 0))
    expect_equal(d[[3]]$y, c(0, 0, 0))
    expect_identical(attr(aligned, "pit_alignment")$alignment, "already_aligned")
  }
  p <- make_plot(c(0, 1), c(1, 1), c(0, 1), pit)
  expect_error(.resid_align_pit_plot(p, pit, 2, FALSE, "1.17.0"), "Cannot safely align")
  p <- make_plot(c(0, .5, 1), c(0, 1, 1), c(0, 0, 1), pit)
  for (bad in list(p + ggplot2::geom_hline(yintercept = 0),
      ggplot2::ggplot(), p + ggplot2::scale_x_reverse())) {
    expect_error(.resid_align_pit_plot(bad, pit, 2, FALSE, "1.17.0"), "Cannot safely align")
  }
  for (column in c("upper", "lower", "empirical", "x")) {
    bad <- p
    bad$data[[column]][1] <- .125
    expect_error(.resid_align_pit_plot(bad, pit, 2, FALSE, "1.17.0"), "Cannot safely align")
  }
})

test_that("any supported four panels can be ordered without changing defaults", {
  x <- pit_plot_fixture()
  auto <- plot(x)
  explicit <- plot(x, panels = c("qq", "fitted", "year", "auto"))
  expect_identical(auto$patches$annotation, explicit$patches$annotation)
  for (i in 1:4) expect_equal(ggplot2::ggplot_build(auto[[i]])$data,
    ggplot2::ggplot_build(explicit[[i]])$data)
  reordered <- plot(x, panels = c("distribution", "year", "fitted", "qq"))
  expect_match(reordered$patches$annotation$caption,
    "Panels B, C, D: simulation-based PIT residuals", fixed = TRUE)
  expect_match(reordered$patches$annotation$caption,
    "Panel A: observed versus simulated response ECDF.", fixed = TRUE)
  expect_identical(reordered[[1]]$data, plot(x, type = "distribution")$data)
  repeated <- plot(x, panels = rep("qq", 4))
  expect_false(grepl("response ECDF", repeated$patches$annotation$caption, fixed = TRUE))
  for (panels in list(character(), c("qq", "year", "fitted"), rep("qq", 5),
      1:4, c("qq", "fitted", "year", NA), c("qq", "fitted", "year", "overview"),
      c("qq", "fitted", "year", "unknown"), matrix(rep("qq", 4), 2))) {
    expect_error(plot(x, panels = panels), "exactly four names")
  }
  expect_error(plot(x, type = "qq", panels = rep("qq", 4)), "only used")
  expect_error(plot(x, panels = c("qq", "year", "fitted", "calibration")), "Calibration requires")
  old <- x
  old$metadata$response_kind <- NULL
  expect_warning(plot(old), "older diagnostic")
  expect_no_warning(plot(old, panels = rep("qq", 4)))
})

test_that("PIT overviews label actual panel positions and scales", {
  skip_if_not_installed("bayesplot", "1.16.0")
  x <- pit_plot_fixture()
  p <- suppressMessages(plot(x, panels = c("pit_ecdf", "auto", "year", "pit_ecdf_diff"),
    pit_grid_size = 20))
  caption <- p$patches$annotation$caption
  expect_match(caption, "Panel A: PIT ECDF on the uniform scale.", fixed = TRUE)
  expect_match(caption, "Panel B: observed versus simulated response ECDF.", fixed = TRUE)
  expect_match(caption, "Panel C: simulation-based PIT residuals on the normal scale", fixed = TRUE)
  expect_match(caption, "Panel D: PIT ECDF minus the uniform reference.", fixed = TRUE)
  expect_identical(p[[3]]$data$residual, x$observations$residual)
  expect_identical(.resid_select_panels(x, setNames(rep("qq", 4), letters[1:4]), "auto"), rep("qq", 4))
})

test_that("PIT guards explain missing summaries, invalid grids, and optional dependencies", {
  x <- pit_plot_fixture()
  for (size in list(1, 0, NA_real_, Inf, 1001, 3.5, c(20, 30), "20")) {
    expect_error(plot(x, type = "pit_ecdf", pit_grid_size = size), "pit_grid_size")
  }
  for (value in list(NULL, rep(NA_real_, 40), rep(Inf, 40), rep(-0.1, 40),
      rep(1.1, 40), rep("0.5", 40), matrix(0.5, 40, 1))) {
    bad <- x
    bad$observations$pit <- value
    expect_error(plot(bad, type = "pit_ecdf"), "finite stored")
  }
  for (value in list(NULL, NA_real_, Inf, 0, 1, c(0.8, 0.9), "0.95")) {
    bad <- x
    bad$metadata$level <- value
    expect_error(plot(bad, type = "pit_ecdf"), "metadata\\$level")
  }
  local_mocked_bindings(.resid_bayesplot_available = function() FALSE)
  expect_error(plot(x, type = "pit_ecdf"), "optional package 'bayesplot'", fixed = TRUE)
  expect_s3_class(plot(x), "patchwork")
})

test_that("PIT ECDF and difference displays are visually stable", {
  skip_if_not_installed("bayesplot", "1.16.0")
  skip_if_not_installed("vdiffr")
  old <- ggplot2::theme_set(ggplot2::theme_bw())
  on.exit(ggplot2::theme_set(old), add = TRUE)
  x <- pit_plot_fixture()
  vdiffr::expect_doppelganger("PIT ECDF and difference", function() {
    print(suppressMessages(plot(x, panels = c("qq", "pit_ecdf", "distribution", "pit_ecdf_diff"),
      pit_grid_size = 20)))
  })
})
