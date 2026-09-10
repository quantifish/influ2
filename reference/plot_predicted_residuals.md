# Plot predictive means against generalised residuals

Uses
[`influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/influ_residuals.md),
never native Pearson or deviance residuals. The horizontal coordinate is
the predictive mean from the same simulations, preserving their response
component and random-effect conditioning. Pass a precomputed result to
avoid repeating simulation when styling plots.

## Usage

``` r
plot_predicted_residuals(fit, trend = "loess", type = "quantile", ...)
```

## Arguments

- fit:

  A supported fitted model or an `influ_residuals` object.

- trend:

  One of `"loess"`, `"lm"`, `"linear"`, or `"none"`.

- type:

  `"quantile"`, `"generalised"`, or `"generalized"` (equivalent).

- ...:

  Calculation options passed to
  [`influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/influ_residuals.md).
  Not accepted for a precomputed object. Complete brms fits are needed
  for calculation; existing posterior draws are used without fitting or
  running MCMC.

## Value

A ggplot with a `residual_metadata` attribute describing the target.
