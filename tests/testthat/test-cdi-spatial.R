cdi_spatial_fixture <- function() {
  data <- data.frame(
    year = factor(rep(1:3, each = 20)),
    x = seq(-1, 1, length.out = 60),
    z = cos(seq_len(60) / 9)
  )
  list(
    data = data,
    weights = rep(c(0, 1, 2, 3), 15),
    mode = c(0.4, -0.25),
    precision = Matrix::Diagonal(2, c(4, 9)),
    values = function(par, data, term, component) {
      value <- if (term == "spatial_field") {
        data$x * par[1] + data$z * par[2]
      } else {
        data$z * par[1] / 3 - data$x * par[2] / 2
      }
      value * c(1, 1.7)[component]
    },
    specs = list(
      overall = .new_family_spec("gamma", "log", response_structure = "hurdle"),
      occurrence = .new_family_spec("binomial", "logit", response_structure = "hurdle"),
      positive = .new_family_spec("gamma", "log", response_structure = "hurdle")
    )
  )
}

expect_spatial_cdi_projection <- function(projections, fixture, samples,
                                        reference_data, reference_weights,
                                        probs) {
  for (component in seq_along(projections)) {
    projection <- projections[[component]]
    for (term in names(projection$coefficient_summaries)) {
      summary <- projection$coefficient_summaries[[term]]
      groups <- .cdi_level_groups(
        fixture$data, term, projection$point_contributions[[term]], fixture$weights
      )
      expect_lte(nrow(summary), 20L)
      expect_identical(summary$level, names(groups))
      expect_true(all(is.finite(summary$centred_std_error)))
      expect_identical(unique(summary$link), c("logit", "log")[component])
      for (level in names(groups)) {
        rows <- groups[[level]]
        raw <- vapply(samples, function(par) {
          values <- fixture$values(par, fixture$data, term, component)
          stats::weighted.mean(values[rows], fixture$weights[rows])
        }, numeric(1))
        centre <- vapply(samples, function(par) {
          stats::weighted.mean(
            fixture$values(par, reference_data, term, component), reference_weights
          )
        }, numeric(1))
        centred <- raw - centre
        actual <- summary[summary$level == level, , drop = FALSE]
        expect_equal(actual$estimate, mean(raw))
        expect_equal(actual$std_error, stats::sd(raw))
        expect_equal(actual$centred_estimate, mean(centred))
        expect_equal(actual$centred_std_error, stats::sd(centred))
        expect_equal(actual$centred_lower, unname(stats::quantile(centred, probs[1])))
        expect_equal(actual$centred_upper, unname(stats::quantile(centred, probs[2])))
        if (component == 2L) {
          expect_equal(actual$relative_estimate, mean(exp(centred)))
          expect_equal(actual$relative_lower, unname(stats::quantile(exp(centred), probs[1])))
          expect_equal(actual$relative_upper, unname(stats::quantile(exp(centred), probs[2])))
        }
      }
    }
    expect_false(any(c("coefficient_draws", "centred_coefficient_draws") %in% names(projection)))
  }
}

test_that("sdmTMB field CDI retains joint centring uncertainty with compact bins", {
  skip_if_not_installed("Matrix")
  fixture <- cdi_spatial_fixture()
  seen <- list()
  report <- function(par, data, projected = FALSE) {
    field <- function(term) {
      cbind(fixture$values(par, data, term, 1L), fixture$values(par, data, term, 2L))
    }
    out <- list(
      omega_s_A = field("spatial_field"),
      epsilon_st_A_vec = field("spatiotemporal_field"),
      eta_i = field("spatial_field") + field("spatiotemporal_field")
    )
    if (projected) names(out) <- c("proj_omega_s_A", "proj_epsilon_st_A_vec", "proj_eta")
    out
  }
  model <- list(
    family = list(delta = TRUE),
    last.par.best = fixture$mode,
    sd_report = list(jointPrecision = fixture$precision),
    tmb_obj = list(report = function(par) {
      seen[[length(seen) + 1L]] <<- par
      report(par, fixture$data)
    })
  )
  reference <- fixture$data[c(5, 13, 27, 46), , drop = FALSE]
  reference_weights <- c(1, 2, 1, 5)
  testthat::local_mocked_bindings(
    predict = function(object, newdata, ...) {
      list(obj = list(report = function(par) report(par, newdata, projected = TRUE)))
    },
    .package = "stats"
  )
  probs <- c(0.1, 0.8)
  for (use_grid in c(FALSE, TRUE)) {
    seen <- list()
    projections <- .sdmTMB_joint_field_projections(
      model, fixture$data, "year", fixture$specs, fixture$weights,
      ndraws = 13L, seed = 42L, batch_size = 5L, probs = probs,
      reference_data = if (use_grid) reference else NULL,
      reference_weights = if (use_grid) reference_weights else NULL
    )
    expect_spatial_cdi_projection(
      projections, fixture, seen[-1L],
      if (use_grid) reference else fixture$data,
      if (use_grid) reference_weights else fixture$weights, probs
    )
    diagnostic <- .sdmTMB_joint_field_diags(
      model, fixture$data, "year", projections, fixture$weights, "summary", probs
    )[[1L]]
    expect_null(diagnostic$draws)
    expect_true(all(is.finite(diagnostic$coefficients$centred_std_error)))
    expect_lte(nrow(diagnostic$coefficients), 40L)
  }
})

test_that("tinyVAST field CDI centres each joint draw on its reference grid", {
  skip_if_not_installed("tinyVAST")
  skip_if_not_installed("TMB")
  fixture <- cdi_spatial_fixture()
  seen <- list()
  model <- list(
    data = fixture$data,
    obj = list(env = list(last.par.best = fixture$mode)),
    sdrep = list(jointPrecision = fixture$precision),
    internal = list(parlist = list(), control = list(profile = FALSE)),
    tmb_inputs = list(tmb_map = list(), tmb_random = character())
  )
  testthat::local_mocked_bindings(
    add_predictions = function(model, newdata, ...) newdata,
    .package = "tinyVAST"
  )
  testthat::local_mocked_bindings(
    MakeADFun = function(data, ...) {
      list(report = function(par) {
        if (nrow(data) == nrow(fixture$data)) seen[[length(seen) + 1L]] <<- par
        out <- list()
        for (component in 1:2) {
          omega <- fixture$values(par, data, "spatial_field", component)
          epsilon <- fixture$values(par, data, "spatiotemporal_field", component)
          out[[paste0("pomega", component, "_g")]] <- omega
          out[[paste0("pepsilon", component, "_g")]] <- epsilon
          out[[paste0("p", component, "_g")]] <- omega + epsilon
        }
        out
      })
    },
    .package = "TMB"
  )
  reference <- fixture$data[c(3, 11, 26, 53), , drop = FALSE]
  reference_weights <- c(2, 1, 5, 2)
  probs <- c(0.05, 0.85)
  projections <- .tinyVAST_joint_field_projections(
    model, fixture$data, "year", fixture$specs, fixture$weights,
    ndraws = 13L, seed = 42L, batch_size = 5L, probs = probs,
    reference_data = reference, reference_weights = reference_weights,
    component_prefix = "lobster"
  )
  expect_spatial_cdi_projection(
    projections, fixture, seen[-1L], reference, reference_weights, probs
  )
  expect_identical(projections[[1]]$component, "lobster:occurrence")
  diagnostic <- .tinyVAST_joint_field_diags(
    model, fixture$data, "year", projections, fixture$weights, "summary", probs
  )[[2L]]
  expect_null(diagnostic$draws)
  expect_true(all(is.finite(diagnostic$coefficients$centred_std_error)))
  expect_lte(nrow(diagnostic$coefficients), 40L)
})
