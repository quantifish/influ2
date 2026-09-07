# Compare BRMS model criteria

Calculates selected public BRMS criteria without inspecting the
underlying Stan object. This keeps \`rstan\` out of the package's
mandatory dependencies.

## Usage

``` r
table_criterion(
  fits,
  criterion = c("loo", "loo_R2", "bayes_R2"),
  sort = TRUE,
  ...
)
```

## Arguments

- fits:

  A \`brmsfit\` or list of \`brmsfit\` objects.

- criterion:

  Any of \`"loo"\`, \`"loo_R2"\`, \`"bayes_R2"\`, and \`"log_lik"\`.

- sort:

  Sort models by expected log predictive density, or Bayesian R-squared
  when LOO is not requested.

- ...:

  Arguments passed to the requested BRMS criterion functions.

## Value

A data frame with one row per model.

## Details

Requires original complete \`brmsfit\` objects, not compact
influence-only fixtures shipped with influ2. The helper evaluates the
existing fits and does not run MCMC.
