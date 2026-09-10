# Plot generalised residual departures by year and group

The maintained successor to the historical residual-implied coefficient
display. Plot each year-by-group mean normal-score rank residual around
zero, using exactly the same calculation as
[`influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/influ_residuals.md).

## Usage

``` r
plot_implied_residuals(
  fit,
  data = NULL,
  year = NULL,
  groups = "area",
  type = "quantile",
  min_n = 10L,
  colour = "purple4",
  ...
)
```

## Arguments

- fit:

  A supported fitted model or a precomputed `influ_residuals` object.

- data:

  Original model data with original row names, for calculation only.

- year:

  Time column; `NULL` uses the standard automatic detection. With a
  stored object, an explicit value must match its recorded time column.

- groups:

  One retained categorical column used for panels.

- type:

  `"quantile"`, `"generalised"`, or `"generalized"`. All select the same
  simulation-based normal-score residuals; native types are rejected.

- min_n:

  Minimum records required in a year-by-group stratum.

- colour:

  Colour used for departures.

- ...:

  Calculation options passed to
  [`influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/influ_residuals.md),
  e.g. `nsim`, `batch_size`, `seed`, and `component`. Not accepted for a
  stored object.

## Value

A ggplot. Its `data` contains stratum means, counts, and descriptive
standard errors, not implied coefficients. The `residual_metadata`
attribute records the simulation target.

## Details

This function no longer adds residuals to year coefficients.
Normal-score residuals are dimensionless; adding them (or Pearson
residuals) to link-scale effects does not produce coefficients of an
interaction. Positive departures indicate observations tending towards
the upper part of their predictive distributions, not a percentage
correction to CPUE. Actual coefficient effects remain available through
[`influ()`](https://www.quantifish.co.nz/influ2/reference/influ.md); an
interaction-specific index requires a separately fitted model.

Bars show mean plus/minus SD/sqrt(n), a descriptive iid standard error,
not an interval accounting for dependence, model estimation, or
simulation error. They are not confidence intervals for interaction
coefficients. Singleton strata have no bar. Unsupported and missing
strata are not joined across intervening sampled years. All panels share
the same residual scale. Specify groups independently of the outcome;
response-defined selection invalidates the zero reference. Outcome
columns are rejected, but derived outcome groups cannot be detected
automatically.

To calculate once and redraw without simulation, retain the required
columns with `influ_residuals(fit, groups = c("area", "gear"))`. Then
pass that object here. Original-data alignment and component selection
are performed during calculation. A combined delta diagnostic is not a
positive-component diagnostic; use an explicit supported `component`.
Saved objects lacking the group columns must be recalculated. Data
cannot be attached later to an object without its fitted-observation
provenance.

## References

Starr, P. J., and Kendrick, T. H. (2019). FLA 1 Fishery Characterisation
and CPUE. New Zealand Fisheries Assessment Report 2019/09, Figure O.9;
Middleton, D. A. J. (2025). A Rapid Update of CPUE for the Snapper
Fishery in SNA 2 to 2024. FAR 2025/32, Appendix C. These motivate the
grouping, not the new normal-score scale. Dunn, P. K., and Smyth, G. K.
(1996). Randomized quantile residuals. Journal of Computational and
Graphical Statistics 5(3), 236-244.
