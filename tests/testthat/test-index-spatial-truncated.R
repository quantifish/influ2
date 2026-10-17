test_that("native response means use component-aligned truncated dispersion", {
  truncated <- function(family) list(family = family, linkinv = function(eta, phi) {
    mu <- exp(eta)
    if (family == "truncated_nbinom2") {
      mu / -expm1(-phi * log1p(mu / phi))
    } else mu / -expm1(-mu / phi * log1p(phi))
  })
  eta <- matrix(c(-0.5, 1), ncol = 1)
  for (family in c("truncated_nbinom1", "truncated_nbinom2")) {
    f <- truncated(family)
    one <- .index_sdmtmb_response(list(family = f), list(ln_phi = log(2)), eta)
    two <- .index_sdmtmb_response(list(family = f), list(ln_phi = log(5)), eta)
    expect_equal(one, f$linkinv(eta[, 1], phi = 2))
    expect_equal(two, f$linkinv(eta[, 1], phi = 5))
    expect_gt(max(abs(one - two)), 0.01)
    delta <- list(family = list(stats::binomial(), f, delta = TRUE))
    both <- cbind(c(-1, 0.5), eta)
    expect_equal(.index_sdmtmb_response(delta, list(ln_phi = c(0, log(5))), both),
      stats::plogis(both[, 1]) * f$linkinv(eta[, 1], phi = 5))
    expect_equal(.domain_sdmtmb_response(delta, NULL,
      list(ln_phi = c(0, log(5))), both),
      .index_sdmtmb_response(delta, list(ln_phi = c(0, log(5))), both))
    for (bad in list(NULL, numeric(), c(0, 1), NA_real_)) {
      expect_error(.index_sdmtmb_response(list(family = f),
        list(ln_phi = bad), eta), "component-aligned dispersion")
    }
    for (bad in c(1000, -1000)) {
      expect_error(.index_sdmtmb_response(list(family = f),
        list(ln_phi = bad), eta), "finite and positive")
    }
    # sdmTMB >= 1.2.0 removes the unused encounter dispersion; this must
    # yield exactly the same response as the older component-aligned layout.
    expect_equal(.index_sdmtmb_response(delta, list(ln_phi = log(5)), both),
      .index_sdmtmb_response(delta, list(ln_phi = c(0, log(5))), both))
    expect_error(.index_sdmtmb_response(delta,
      list(ln_phi = c(0, log(5), log(2))), both), "component-aligned dispersion")
  }
})

test_that("native family response safeguards preserve existing links and adjustments", {
  eta <- cbind(c(-2, 1), c(1, 2))
  delta <- list(family = list(stats::binomial(link = "log"),
    stats::Gamma(link = "log"), delta = TRUE, type = "poisson_link_delta"))
  expect_equal(.index_sdmtmb_response(delta, NULL, eta), exp(rowSums(eta)))
  mixture <- list(family = list(family = "nbinom2_mix", linkinv = exp))
  # Native proj_eta has already absorbed the mixture-mean adjustment.
  expect_equal(.index_sdmtmb_response(mixture,
    list(logit_p_mix = 1, log_ratio_mix = 2), eta[, 1, drop = FALSE]),
    exp(eta[, 1]))
  expect_error(.index_sdmtmb_response(delta, NULL, eta[, 1, drop = FALSE]),
    "align with the fitted family")
  expect_error(.index_sdmtmb_response(list(family = list(family = "custom")),
    NULL, eta[, 1, drop = FALSE]), "provide its inverse link")
})

truncated_spatial_fixture <- local({
  cache <- new.env(parent = emptyenv())
  function(type, delta) {
    key <- paste(type, delta)
    if (exists(key, cache, inherits = FALSE)) return(cache[[key]])
    set.seed(19)
    n <- 240L
    d <- data.frame(year = rep(1:3, each = 80), x = runif(n), y = runif(n))
    mu <- exp(1 + 0.3 * d$x)
    size <- if (type == "nbinom1") mu / 2 else rep(2, n)
    d$response <- stats::rnbinom(n, size = size, mu = mu)
    while (any(d$response == 0)) {
      i <- which(d$response == 0)
      d$response[i] <- stats::rnbinom(length(i), size = size[i], mu = mu[i])
    }
    if (delta) d$response <- d$response * stats::rbinom(n, 1, 0.7)
    family <- getExportedValue("sdmTMB",
      paste0(if (delta) "delta_" else "", "truncated_", type))()
    mesh <- sdmTMB::make_mesh(d, xy_cols = c("x", "y"), n_knots = 10)
    m <- sdmTMB::sdmTMB(response ~ factor(year) + x, data = d,
      mesh = mesh, family = family, time = "year", spatial = "off",
      spatiotemporal = "off", silent = TRUE)
    cache[[key]] <- m
    m
  }
})

test_that("real single and delta truncated families retain dispersion in joint predictions", {
  skip_if_not_installed("sdmTMB")
  for (type in c("nbinom1", "nbinom2")) for (delta in c(FALSE, TRUE)) {
    m <- truncated_spatial_fixture(type, delta)
    expect_identical(m$model$convergence, 0L)
    expect_true(m$sd_report$pdHess)
    d <- m$data[1:3, ]
    b <- .index_spatial_projection(m, "sdmTMB", d, "all", NULL)
    domain <- .domain_projection(m, "sdmTMB", d)
    tryCatch({
      draw <- m$last.par.best
      i <- grep("^ln_phi$", names(draw))
      expect_length(i, 1L)
      draw[i] <- draw[i] + 1
      p <- b$obj$env$parList(x = draw[setdiff(seq_along(draw), b$obj$env$random)],
        par = draw)
      expect_length(p$ln_phi, length(m$parlist$ln_phi))
      eta <- b$obj$report(draw)$proj_eta
      positive <- if (delta) 2L else 1L
      f <- if (delta) m$family[[2L]] else m$family
      expected <- f$linkinv(eta[, positive], phi = exp(tail(p$ln_phi, 1L)))
      if (delta) expected <- expected * m$family[[1L]]$linkinv(eta[, 1L])
      expect_equal(b$response(draw), expected)
      expect_equal(domain$response(draw), expected)
      expect_equal(domain$response(draw, subtract = matrix(0, nrow(d), positive)), expected)
      expect_gt(max(abs(b$response(draw) - b$response(m$last.par.best))), 0.01)
    }, finally = {
      .index_free_tmb(b$obj)
      .index_free_tmb(domain$obj)
    })
  }
})

test_that("truncated delta indices share correct paired draws independently of batching", {
  skip_if_not_installed("sdmTMB")
  m <- truncated_spatial_fixture("nbinom2", TRUE)
  ref <- data.frame(x = c(0.1, 0.6), y = c(0.2, 0.8))
  sampled <- matrix(TRUE, nrow(ref), 3)
  before <- list(last = m$tmb_obj$env$last.par,
    best = m$tmb_obj$env$last.par.best, mode = m$last.par.best)
  a <- index_domain(m, ref, year = "year", sampled = sampled,
    omit = "x", where = "all", covariate_reference = "zero",
    ndraws = 12, seed = 71, batch_size = 2, draw_batch_size = 5)
  b <- index_domain(m, ref, year = "year", sampled = sampled,
    omit = "x", where = "all", covariate_reference = "zero",
    ndraws = 12, seed = 71, batch_size = 20, draw_batch_size = 12)
  baseline <- cpue_index(m, reference_data = ref, year = "year", retain = "draws",
    ndraws = 12, seed = 71, batch_size = 3, draw_batch_size = 7)
  expect_equal(a$baseline$table, b$baseline$table)
  expect_equal(a$baseline$draws, b$baseline$draws)
  expect_equal(a$omission[[1L]]$draws, b$omission[[1L]]$draws)
  expect_equal(a$baseline$table$Mean, baseline$table$Mean)
  expect_equal(unname(a$baseline$draws), unname(baseline$draws))
  info <- .index_spatial_info(m, "sdmTMB", "year")
  d <- do.call(rbind, lapply(1:3, function(year) transform(ref, year = year)))
  block <- .domain_projection(m, "sdmTMB", d, year = "year")
  tryCatch({
    set.seed(71)
    draws <- .index_spatial_sampler(info, block$obj)(12)
    expected <- omitted <- matrix(NA_real_, 12, 3)
    for (j in seq_len(ncol(draws))) {
      draw <- draws[, j]
      p <- block$obj$env$parList(x = draw[setdiff(seq_along(draw), block$obj$env$random)],
        par = draw)
      report <- block$obj$report(draw)
      eta <- report$proj_eta
      mean_response <- function(z) m$family[[1L]]$linkinv(z[, 1]) *
        m$family[[2L]]$linkinv(z[, 2], phi = exp(tail(p$ln_phi, 1L)))
      expected[j, ] <- colMeans(matrix(mean_response(eta), nrow = nrow(ref)))
      terms <- block$terms(draw)
      omitted[j, ] <- colMeans(matrix(mean_response(eta - terms$x), nrow = nrow(ref)))
    }
    expect_equal(unname(a$baseline$draws), expected)
    expect_equal(unname(a$omission[[1L]]$draws), omitted)
    summary <- a$omission_summary[a$omission_summary$series == "x:all", ]
    expect_equal(summary$ratio_lower, apply(omitted / expected, 2, quantile, probs = 0.025),
      ignore_attr = TRUE)
    expect_equal(summary$ratio_upper, apply(omitted / expected, 2, quantile, probs = 0.975),
      ignore_attr = TRUE)
  }, finally = .index_free_tmb(block$obj))
  expect_identical(list(last = m$tmb_obj$env$last.par,
    best = m$tmb_obj$env$last.par.best, mode = m$last.par.best), before)
})
