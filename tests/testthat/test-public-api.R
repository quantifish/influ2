test_that("approved legacy retirements stay outside the runtime namespace", {
  retired <- c(
    "plot_hurdle", "get_coefs", "get_coefs_raw", "get_marginal",
    "influ_app", "get_influ", "get_influ2", "plot_influ",
    "plot_bayesian_cdi", "plot_bayesian_cdi2"
  )
  namespace <- asNamespace("influ2")

  expect_false(any(retired %in% getNamespaceExports(namespace)))
  expect_false(any(vapply(
    retired, exists, logical(1), envir = namespace, inherits = FALSE
  )))
})

test_that("triage preserves the maintained diagnostic and comparison API", {
  retained <- c(
    "influ", "influ_steps", "influ_effects", "influ_indices",
    "plot_bubble", "plot_data_extent", "plot_compare", "plot_step",
    "plot_implied_residuals", "plot_predicted_residuals", "plot_qq",
    "get_bayes_R2", "table_criterion"
  )

  expect_true(all(retained %in% getNamespaceExports("influ2")))
  expect_true(all(vapply(retained, function(name) {
    is.function(getExportedValue("influ2", name))
  }, logical(1))))
})
