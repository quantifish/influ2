test_that("numeric matrix observations compare values without S3 basis classes", {
  basis <- stats::poly(seq_len(12L), 2L)
  plain <- matrix(as.numeric(basis), nrow(basis), ncol(basis))
  expect_true(.same_observation_column(basis, plain))
  expect_true(.same_observation_column(plain, basis))

  changed <- plain
  changed[3L, 2L] <- changed[3L, 2L] + 0.25
  expect_false(.same_observation_column(basis, changed))
  expect_false(.same_observation_column(basis, plain[12L:1L, ]))
  expect_false(.same_observation_column(basis, t(plain)))
  expect_false(.same_observation_column(basis, as.numeric(plain)))
  changed <- plain
  # all.equal uses relative tolerance, so perturb a non-zero value relatively.
  changed[1L, 1L] <- changed[1L, 1L] * (1 + 1e-12)
  expect_false(.same_observation_column(basis, changed))
  expect_true(.same_observation_column(basis, changed, tolerance = 1e-10))
})

test_that("subsetted polynomial GLMs retain fitted values and data guards", {
  set.seed(17)
  d <- data.frame(year = factor(rep(2001:2005, each = 20L)), x = runif(100L))
  d$y <- stats::rpois(100L, exp(0.1 + 0.3 * d$x))
  fit <- stats::glm(y ~ year + stats::poly(x, 2), family = stats::poisson(),
    data = d, subset = x > 0.2)
  rows <- d[rownames(stats::model.frame(fit)), , drop = FALSE]
  expect_lt(nrow(rows), nrow(d))
  expect_no_warning(implicit <- influ(fit, focus = "year", uncertainty = "none"))
  explicit <- influ(fit, focus = "year", data = rows, uncertainty = "none")
  expect_equal(influ_effects(implicit), influ_effects(explicit))
  expect_equal(influ_composition(implicit), influ_composition(explicit))

  # Evaluate the fitted design directly: the original full-data polynomial
  # basis is retained by glm even though only a subset entered the likelihood.
  X <- stats::model.matrix(fit)
  selected_term <- attr(stats::terms(fit), "term.labels")[2L]
  columns <- which(attr(X, "assign") == 2L)
  contribution <- drop(X[, columns, drop = FALSE] %*% stats::coef(fit)[columns])
  expected <- tapply(contribution, rows$year, mean) - mean(contribution)
  actual <- subset(influ_effects(implicit), term == selected_term & scale == "link")
  expect_equal(actual$estimate, unname(as.numeric(expected)), tolerance = 1e-10)

  changed <- rows
  changed$x[1L] <- changed$x[1L] + 0.5
  expect_error(influ(fit, focus = "year", data = changed, uncertainty = "none"),
    "supplied `data` do not reproduce the fitted model frame")
  reordered <- rows[rev(seq_len(nrow(rows))), , drop = FALSE]
  expect_error(influ(fit, focus = "year", data = reordered, uncertainty = "none"),
    "supplied `data` do not reproduce the fitted model frame")
  d$x[match(rownames(rows)[1L], rownames(d))] <- rows$x[1L] + 0.5
  expect_error(influ(fit, focus = "year", uncertainty = "none"),
    "no longer reproduce the fitted model frame")
})
