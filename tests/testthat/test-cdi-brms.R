brms_cdi_fixture <- function() {
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  readRDS(system.file(
    "extdata", "brms-fixtures", "fit2.rds", package = "influ2"
  ))
}

brms_cdi_group <- function(fit, weights = NULL, reference_data = NULL,
                           reference_weights = NULL, uncertainty = "posterior",
                           probs = c(0.025, 0.975)) {
  .brms_group_diag(
    model = fit,
    data = fit$data,
    focus = "year",
    group_rows = fit$ranef,
    component = "conditional",
    family_spec = .new_family_spec("negbinomial", "log", backend = "brms"),
    weights = weights,
    uncertainty = uncertainty,
    retain = "summary",
    probs = probs,
    ndraws = NULL,
    reference_data = reference_data,
    reference_weights = reference_weights
  )
}

test_that("BRMS group CDI centres joint draws using positive observation weights", {
  fit <- brms_cdi_fixture()
  levels <- levels(fit$data$month)
  weights <- 1 + seq_len(nrow(fit$data)) %% 3
  weights[fit$data$month == levels[12]] <- 0
  probs <- c(0.05, 0.9)
  diagnostic <- brms_cdi_group(fit, weights = weights, probs = probs)
  coefficients <- diagnostic$coefficients

  parameters <- paste0("r_month[", levels, ",Intercept]")
  draws <- as.matrix(fit$influ2_draws[, parameters, drop = FALSE])
  mass <- vapply(levels, function(level) {
    sum(weights[fit$data$month == level]) / sum(weights)
  }, numeric(1))
  centre <- as.numeric(draws %*% mass)
  centred <- sweep(draws, 1L, centre, "-")
  selected <- match(coefficients$level, levels)

  expect_false(levels[12] %in% coefficients$level)
  expect_equal(coefficients$estimate, unname(colMeans(draws)[selected]))
  expect_equal(coefficients$centred_estimate, unname(colMeans(centred)[selected]))
  expect_equal(coefficients$centred_std_error,
               unname(apply(centred, 2L, stats::sd)[selected]))
  expect_equal(coefficients$centred_lower, unname(apply(
    centred, 2L, stats::quantile, probs = probs[1]
  )[selected]))
  expect_equal(coefficients$centred_upper, unname(apply(
    centred, 2L, stats::quantile, probs = probs[2]
  )[selected]))
  expect_equal(coefficients$relative_estimate,
               unname(colMeans(exp(centred))[selected]))
  expect_equal(coefficients$relative_lower, unname(apply(
    exp(centred), 2L, stats::quantile, probs = probs[1]
  )[selected]))
  expect_equal(sum(coefficients$centred_estimate * mass[selected]), 0,
               tolerance = 1e-12)
  expect_true(all(coefficients$centred_std_error > 0))
  expect_null(diagnostic$draws)
})

test_that("BRMS group CDI uses the explicit weighted influence reference", {
  fit <- brms_cdi_fixture()
  levels <- levels(fit$data$month)
  reference <- fit$data[match(levels, fit$data$month), , drop = FALSE]
  reference_weights <- seq_along(levels)
  reference_weights[12] <- 0
  diagnostic <- brms_cdi_group(
    fit, reference_data = reference, reference_weights = reference_weights
  )
  coefficients <- diagnostic$coefficients

  parameters <- paste0("r_month[", levels, ",Intercept]")
  draws <- as.matrix(fit$influ2_draws[, parameters, drop = FALSE])
  mass <- reference_weights / sum(reference_weights)
  centred <- sweep(draws, 1L, as.numeric(draws %*% mass), "-")
  selected <- match(coefficients$level, levels)
  expect_equal(coefficients$estimate, unname(colMeans(draws)[selected]))
  expect_equal(coefficients$centred_estimate, unname(colMeans(centred)[selected]))
  expect_equal(coefficients$centred_lower, unname(apply(
    centred, 2L, stats::quantile, probs = 0.025
  )[selected]))
  expect_true(levels[12] %in% coefficients$level)
  expect_equal(sum(coefficients$centred_estimate * mass[selected]), 0,
               tolerance = 1e-12)

  preview <- brms_cdi_group(
    fit, reference_data = reference, reference_weights = reference_weights,
    uncertainty = "none"
  )
  expect_equal(preview$coefficients$centred_estimate,
               coefficients$centred_estimate)
  expect_true(all(is.na(preview$coefficients$centred_lower)))
  expect_true(all(is.na(preview$coefficients$relative_lower)))
  expect_null(preview$draws)
})

test_that("BRMS smooth CDI centres the full posterior basis on its reference", {
  fit <- brms_cdi_fixture()
  reference <- fit$data[seq_len(12), , drop = FALSE]
  reference$depth <- seq(10, 40, length.out = nrow(reference))
  reference_weights <- seq_len(nrow(reference))
  diagnostics <- .brms_smooth_diags(
    model = fit,
    data = fit$data,
    focus = "year",
    component = "conditional",
    family_spec = .new_family_spec("negbinomial", "log", backend = "brms"),
    weights = NULL,
    uncertainty = "posterior",
    retain = "summary",
    probs = c(0.025, 0.975),
    ndraws = NULL,
    reference_data = reference,
    reference_weights = reference_weights
  )
  coefficients <- diagnostics[[1]]$coefficients

  standata <- brms::standata(fit)
  reference_standata <- brms::standata(fit, newdata = reference)
  design <- cbind(standata$Xs, standata$Zs_1_1)
  reference_design <- cbind(reference_standata$Xs, reference_standata$Zs_1_1)
  parameters <- c("bs_sdepth_1", "s_sdepth_1[1]")
  draws <- as.matrix(fit$influ2_draws[, parameters, drop = FALSE])
  overall <- colSums(reference_design * reference_weights) / sum(reference_weights)
  bins <- cut(
    fit$data$depth,
    breaks = unique(stats::quantile(fit$data$depth, seq(0, 1, length.out = 21))),
    include.lowest = TRUE, ordered_result = TRUE
  )
  expected <- vapply(coefficients$level, function(level) {
    contrast <- colMeans(design[bins == level, , drop = FALSE]) - overall
    values <- as.numeric(draws %*% contrast)
    c(mean(values), stats::sd(values), stats::quantile(values, 0.025))
  }, numeric(3))

  expect_equal(coefficients$centred_estimate, unname(expected[1, ]))
  expect_equal(coefficients$centred_std_error, unname(expected[2, ]))
  expect_equal(coefficients$centred_lower, unname(expected[3, ]))
  expect_null(diagnostics[[1]]$draws)
})
