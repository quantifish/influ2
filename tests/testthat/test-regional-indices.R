# A small independent fixture: the vignette uses a larger uneven design.
regional_index_fixture <- function() {
  set.seed(1814)
  d <- expand.grid(year = factor(c("2000", "2002", "2005")),
    area = factor(as.character(1:4)), replicate = 1:55)
  d$x <- runif(nrow(d), -1, 1)
  d$vessel <- factor(sample(1:12, nrow(d), replace = TRUE))
  a <- as.integer(d$area)
  t <- as.integer(d$year) - 1
  b <- rnorm(12, sd = 0.3)
  d$cpue <- rnbinom(nrow(d), size = 10, mu = exp(1 +
    c(-0.3, 0.3, -0.1, 0.4)[a] + c(-0.3, -0.1, 0.1, 0.4)[a] * t +
    0.4 * d$x + b[d$vessel]))
  fit <- glmmTMB::glmmTMB(cpue ~ year * area + x + (1 | vessel),
    data = d, family = glmmTMB::nbinom2())
  ref <- expand.grid(area = levels(d$area), x = c(-0.5, 0.5))
  ref$area <- factor(ref$area, levels = levels(d$area))
  list(data = d, fit = fit, refs = list(A = ref[ref$area %in% c("1", "2"), ],
    B = ref[ref$area %in% c("3", "4"), ]))
}

test_that("regional interaction indices retain native means and joint annual covariance", {
  skip_if_not_installed("glmmTMB")
  f <- regional_index_fixture()
  expect_identical(f$fit$fit$convergence, 0L)
  expect_true(f$fit$sdr$pdHess)
  original <- f$fit$fit$par
  years <- levels(f$data$year)
  indices <- list()
  for (region in names(f$refs)) {
    ref <- f$refs[[region]]
    weights <- c(1, 3, 2, 4)
    index <- cpue_index(f$fit, reference_data = ref, reference_weights = weights)
    indices[[region]] <- index
    grid <- ref[rep(seq_len(nrow(ref)), length(years)), ]
    grid$year <- factor(rep(years, each = nrow(ref)), levels = years)
    grid$vessel <- factor(levels(f$data$vessel)[1], levels = levels(f$data$vessel))
    native <- predict(f$fit, newdata = grid, re.form = NA, type = "response",
      se.fit = TRUE, cov.fit = TRUE)
    W <- kronecker(diag(length(years)), matrix(weights / sum(weights), nrow = 1))
    mu <- drop(W %*% native$fit)
    V <- W %*% native$cov.fit %*% t(W)
    expect_equal(index$table$Mean, mu, tolerance = 1e-6, ignore_attr = TRUE)
    expect_equal(index_vcov(index, "response"), V, tolerance = 1e-5, ignore_attr = TRUE)
    expect_equal(index_vcov(index), V / outer(mu, mu), tolerance = 1e-5, ignore_attr = TRUE)
    expect_equal(index$table$SD, sqrt(diag(V)), tolerance = 1e-5, ignore_attr = TRUE)
    expect_identical(index$table$Year, years)
    expect_identical(dimnames(index_vcov(index)), list(years, years))
    expect_gt(max(abs(V - diag(diag(V)))), 1e-5)
    expect_silent(index_vcov(index, require_pd = TRUE))

    # An explicit design-gradient check is independent of native predict(cov.fit).
    X <- model.matrix(~ year * area + x, grid)
    G <- W %*% (X * as.numeric(native$fit))
    analytic <- G %*% as.matrix(vcov(f$fit)$cond) %*% t(G)
    expect_equal(V, analytic, tolerance = 1e-7, ignore_attr = TRUE)

    # Reference permutations, splitting weights, and batching must not alter the target.
    perm <- c(4, 2, 1, 3)
    reordered <- cpue_index(f$fit, reference_data = ref[perm, ],
      reference_weights = weights[perm], batch_size = 5)
    expect_equal(reordered$table, index$table, tolerance = 1e-7)
    expect_equal(index_vcov(reordered), index_vcov(index), tolerance = 1e-7)
    split <- cpue_index(f$fit, reference_data = rbind(ref, ref[1, ]),
      reference_weights = c(weights * c(0.5, 1, 1, 1), weights[1] / 2))
    expect_equal(split$table, index$table, tolerance = 1e-7)
    expect_equal(index_vcov(split), index_vcov(index), tolerance = 1e-7)
    ref_dropped <- droplevels(ref)
    dropped <- cpue_index(f$fit, reference_data = ref_dropped, reference_weights = weights)
    expect_equal(dropped$table, index$table, tolerance = 1e-7)

    # Normalise the aggregated series, including the shared denominator's uncertainty.
    relative <- cpue_index(f$fit, reference_data = ref,
      reference_weights = weights, rescale = 1)
    H <- diag(length(years)) - matrix(1 / length(years), length(years), length(years))
    expect_equal(relative$table$Mean, mu / exp(mean(log(mu))), tolerance = 1e-7)
    expect_equal(index_vcov(relative), H %*% (V / outer(mu, mu)) %*% t(H),
      tolerance = 1e-5, ignore_attr = TRUE)
    expect_error(index_vcov(relative, require_pd = TRUE), "singular")
  }
  expect_identical(f$fit$fit$par, original)
  expect_gt(max(abs(indices$A$table$Mean / geo_mean(indices$A$table$Mean) -
    indices$B$table$Mean / geo_mean(indices$B$table$Mean))), 0.3)
  expect_silent(ggplot2::ggplot_build(plot_compare(indices)))
  expect_silent(ggplot2::ggplot_build(plot_index(indices$A)))
  bad_ref <- f$refs$A
  bad_ref$area <- "unseen"
  expect_error(cpue_index(f$fit, reference_data = bad_ref))
  expect_error(cpue_index(f$fit, reference_data = f$refs$A["x"]), "missing: area")
  bad_fit <- f$fit
  bad_fit$sdr$pdHess <- FALSE
  expect_error(cpue_index(bad_fit, reference_data = f$refs$A), "positive-definite Hessian")
  reml <- f$fit
  reml$modelInfo$REML <- TRUE
  expect_error(cpue_index(reml, reference_data = f$refs$A), "require ML")

  # One shared log year effect implies identical proportional regional curves.
  additive <- update(f$fit, . ~ . - year:area, data = f$data)
  shared <- lapply(f$refs, function(ref) cpue_index(additive,
    reference_data = ref, uncertainty = "none", rescale = 1))
  expect_equal(shared$A$table$Mean, shared$B$table$Mean, tolerance = 1e-8)
})
