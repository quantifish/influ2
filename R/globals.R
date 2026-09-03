# Legacy plotting and table helpers use non-standard evaluation through
# dplyr and ggplot2. Declaring their data-mask names keeps package checks
# informative while those helpers are progressively migrated to the common
# influence object.
utils::globalVariables(c(
  ".chain", ".draw", ".iteration", "cpue", "CV", "elpd_diff",
  "Est.Error", "Estimate", "id", "iteration", "Mean", "mean_coef",
  "Median", "Model", "Q50", "Q97.5_bayes_R2", "Qlow", "Qlower",
  "Qup", "Qupper", "SD", "value", "Var1", "Var2", "variable", "y",
  "Year"
))
