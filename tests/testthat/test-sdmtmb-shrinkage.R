test_that("sdmTMB smooth selectors distinguish empty selections from defaults", {
  Xs <- cbind(c(1, 2), c(3, 4))
  Zs <- list(matrix(c(5, 6), ncol = 1L),
    matrix(c(7, 8), ncol = 1L))
  parameters <- list(bs = matrix(c(2, 3), ncol = 1L),
    b_smooth = matrix(c(4, 5), ncol = 1L))
  contribution <- function(columns = NULL, blocks = NULL) {
    .sdmTMB_smooth_contribution(Xs, Zs, c(0L, 1L), parameters, 1L,
      columns = columns, blocks = blocks)
  }
  # NULL denotes the combined smooth; integer(0) denotes no columns/blocks.
  expect_equal(contribution(), as.numeric(Xs %*% parameters$bs +
    Zs[[1L]] %*% parameters$b_smooth[1L] +
    Zs[[2L]] %*% parameters$b_smooth[2L]))
  expect_equal(contribution(columns = integer(), blocks = 1L), c(20, 24))
  expect_equal(contribution(columns = 2L, blocks = integer()), c(9, 12))
  expect_equal(contribution(columns = integer(), blocks = integer()), c(0, 0))
})

test_that("sdmTMB shrinkage smooths remain separate across native projections", {
  skip_if_not_installed("sdmTMB")
  set.seed(44)
  n <- 400L
  d <- data.frame(year = factor(sample(2001:2005, n, TRUE)),
    depth = stats::runif(n, 10, 100), soak = stats::runif(n, 5, 50))
  d$y <- stats::rpois(n, exp(0.3 + 0.012 * d$depth -
    0.00012 * d$depth^2 + 0.02 * d$soak - 0.0003 * d$soak^2 +
    0.1 * as.integer(d$year)))
  contrast <- function(x) {
    unname(as.numeric(tapply(x, d$year, mean) - mean(x)))
  }
  link_rows <- function(x) {
    e <- subset(influ_effects(x), scale == "link")
    e[order(e$component, e$term, e$level),
      c("component", "term", "level", "estimate")]
  }

  # Both shrinkage bases have no unpenalised columns. The ordinary second
  # smooth does, making an accidental empty-to-all substitution observable.
  for (basis in c("cs", "ts")) {
    formula <- stats::as.formula(paste0(
      "y ~ year + s(depth, bs = '", basis, "') + s(soak, k = 5)"))
    model <- sdmTMB::sdmTMB(formula, data = d, family = stats::poisson(),
      spatial = "off", silent = TRUE)
    report <- model$tmb_obj$report(model$last.par.best)
    parameters <- .sdmTMB_smooth_parameters(model, model$last.par.best)
    layout <- .sdmTMB_smooth_layout(model)
    expect_length(layout[[1L]]$columns, 0L)
    expect_gt(length(layout[[2L]]$columns), 0L)
    terms <- .sdmTMB_smooth_terms(model, report, parameters, 1L)
    expect_true(terms$split)
    expect_equal(Reduce(`+`, terms$components[[1L]]$values),
      as.numeric(report$eta_smooth_i), tolerance = 1e-10)

    expect_no_warning(observed <- influ(model, focus = "year",
      uncertainty = "none"))
    expect_false("smooths" %in% observed$influence$term)
    smooth_rows <- subset(influ_effects(observed), scale == "link" &
      term %in% c("s(depth)", "s(soak)"))
    expect_setequal(unique(smooth_rows$term), c("s(depth)", "s(soak)"))
    expect_equal(unname(as.numeric(tapply(smooth_rows$estimate,
      smooth_rows$level, sum))), contrast(as.numeric(report$eta_smooth_i)),
      tolerance = 1e-10)

    expect_no_warning(gridded <- influ(model, focus = "year",
      reference_data = d, uncertainty = "none"))
    expect_equal(link_rows(gridded), link_rows(observed),
      ignore_attr = TRUE, tolerance = 1e-8)

    specs <- .sdmTMB_family_specs(model)
    for (reference in list(NULL, d)) {
      split <- .sdmTMB_joint_field_projections(model, d, "year", specs, NULL,
        ndraws = 15L, seed = 3L, reference_data = reference,
        smooth_terms = terms)[[1L]]
      combined_terms <- terms
      combined_terms$split <- FALSE
      combined <- .sdmTMB_joint_field_projections(model, d, "year", specs, NULL,
        ndraws = 15L, seed = 3L, reference_data = reference,
        smooth_terms = combined_terms)[[1L]]
      expect_equal(split$term_deltas[["s(depth)"]] +
        split$term_deltas[["s(soak)"]], combined$term_deltas$smooths,
        tolerance = 1e-10)
    }

    reference <- d[c(1L, 50L, 100L, 150L, 200L, 250L), c("depth", "soak")]
    grid <- reference
    grid$year <- factor("2003", levels = levels(d$year))
    block <- .domain_projection(model, "sdmTMB", grid, year = "year")
    expect_no_error(pieces <- block$terms(model$last.par.best))
    expect_true(all(c("s(depth)", "s(soak)") %in% names(pieces)))
    expect_equal(Reduce(`+`, pieces), block$eta(model$last.par.best),
      tolerance = 1e-10)
    draw <- model$last.par.best
    draw[names(draw) == "b_smooth"] <-
      draw[names(draw) == "b_smooth"] + 0.1
    draw[names(draw) == "bs"] <- draw[names(draw) == "bs"] - 0.05
    draw_pieces <- block$terms(draw)
    expect_equal(Reduce(`+`, draw_pieces), block$eta(draw), tolerance = 1e-10)
    expect_false(isTRUE(all.equal(draw_pieces[["s(depth)"]],
      pieces[["s(depth)"]])))
    expect_false(isTRUE(all.equal(draw_pieces[["s(soak)"]],
      pieces[["s(soak)"]])))
    .index_free_tmb(block$obj)

    sampled <- matrix(TRUE, nrow(reference), nlevels(d$year))
    weights <- seq_len(nrow(reference))
    expect_no_warning(domain <- index_domain(model, reference, year = "year",
      sampled = sampled, reference_weights = weights, uncertainty = "none"))
    index <- cpue_index(model, reference_data = reference, year = "year",
      reference_weights = weights, uncertainty = "none")
    expect_equal(domain$baseline$table$Mean, index$table$Mean,
      tolerance = 1e-10)
  }
})
