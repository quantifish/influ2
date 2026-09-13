# Calculate an assessment-ready CPUE index

Calculate expected-response indices over the same explicit reference
population in every year, or extract the existing year-effect contrasts.
No model is fitted or updated by this function.

## Usage

``` r
cpue_index(
  model,
  year = NULL,
  method = c("standardised", "standardized", "year_effect"),
  reference_data = NULL,
  reference_weights = NULL,
  uncertainty = c("auto", "none"),
  probs = c(0.025, 0.975),
  rescale = "raw",
  ndraws = 1000L,
  batch_size = 250L,
  draw_batch_size = 100L,
  retain = c("summary", "draws"),
  units = NULL,
  ...,
  spatial_fields = c("all", "spatial", "spatiotemporal", "none"),
  seed = 1L,
  prediction_offset = NULL
)

# S3 method for class 'influ_index'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'influ_index'
print(x, ...)
```

## Arguments

- model:

  A fitted GLM, `mgcv` GAM, `glmmTMB`, complete `brmsfit`, `sdmTMB`, or
  univariate `tinyVAST` model for response standardisation. For
  `method = "year_effect"`, any model supported by
  [`influ()`](https://www.quantifish.co.nz/influ2/reference/influ.md) or
  an existing `influ_diag` can be supplied.

- year:

  Name of the year variable. Defaults to the native time variable for
  spatial backends, otherwise the first formula predictor.

- method:

  `"standardised"` (default), its alias `"standardized"`, or
  `"year_effect"`. The latter is a contrast, not an expected-response
  index.

- reference_data:

  A non-empty data frame defining a common covariate profile or
  population, **without** the year column. Each row is predicted in
  every observed year. Supply all required predictors and any exposure
  variables explicitly; use one unit of exposure for a per-unit index.

- reference_weights:

  Non-negative weights, one per reference row. Defaults to equal
  weights. These are standardisation weights, not areas.

- uncertainty:

  `"auto"` for propagated uncertainty, or `"none"` for a point-estimate
  preview with missing uncertainty columns.

- probs:

  Two increasing interval probabilities.

- rescale:

  `"raw"` (default), or a positive target geometric mean across all
  returned years. A common normalisation is applied to each posterior
  draw; frequentist uncertainty includes the normalising denominator.

- ndraws:

  Maximum number of existing posterior draws for brms, or number of
  joint Gaussian parameter/field draws for spatial backends. The same
  draw identities are shared across years and prediction batches.

- batch_size:

  Maximum reference rows predicted together.

- draw_batch_size:

  Maximum posterior draws predicted together.

- retain:

  `"summary"` (default) or `"draws"`. Draw retention is available for
  standardised brms and spatial indices and stores only a draw-by-year
  matrix. Spatial draws are a joint Gaussian approximation, not MCMC.

- units:

  Optional response units, such as `"lobsters per pot"`.

- ...:

  Arguments passed to
  [`influ()`](https://www.quantifish.co.nz/influ2/reference/influ.md)
  only for `method = "year_effect"`.

- spatial_fields:

  Spatial-backend prediction target: `"all"` includes persistent,
  spatially varying, and spatiotemporal fields; `"spatial"` excludes
  spatiotemporal fields; `"spatiotemporal"` excludes persistent and
  spatially varying fields; `"none"` excludes all three. This changes
  only predictions, not the fitted model. Other smooths/time effects
  remain.

- seed:

  Non-negative integer seed for spatial joint draws. The caller's
  random-number state is restored; changing batch sizes preserves draws.

- prediction_offset:

  For sdmTMB only, the name of a numeric link-scale offset column in
  `reference_data`. `NULL` explicitly uses offset zero (one unit of
  exposure for a log-exposure offset). Other backends obtain offsets
  from their formula and the supplied reference predictors.

- x:

  An \`influ_index\` object.

- row.names, optional:

  Passed to the data-frame method.

## Value

An S3 `influ_index` object containing `table`, `metadata`, compact
`covariance`, and optional `draws`.
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the full table with `Year`, `Mean`, `Median`, `SD`, `CV`, `Qlower`,
`Qupper`, `Method`, `Distribution`, and `Link`.

## Details

For standardisation, predictions are averaged on the **response** scale,
not averaged on the link scale and then back-transformed. Supplied
reference weights are treated as known. Hurdle and zero-inflated fits
use the backend's combined expected response, including its zero
component. Binomial GLM/glmmTMB predictions are probabilities; brms
binomial expected predictions are counts at the supplied number of
trials. Supply one trial for comparable per-trial indices.
Log-transformed responses are rejected: fit an explicit lognormal model
instead of silently exponentiating a fit.

GLMs and GAMs use the delta method and their joint coefficient
covariance. GAM smooths, including random-effect smooths, are evaluated
as fitted; the smoothing-parameter correction is used when available.
glmmTMB uses the joint fitted-parameter covariance and numerical
derivatives of native response predictions, with conditional random
effects set to zero. REML and zero-component random effects are not yet
supported for glmmTMB here. brms uses
[`brms::posterior_epred()`](https://mc-stan.org/rstantools/reference/posterior_epred.html)
with group-level effects set to zero. Setting a random effect to zero is
**not** integration over its population distribution. brms smooths and
other population-level terms remain present.

`Mean` is the fitted expected-response estimate for frequentist models,
and the posterior mean of the expected-response index for brms. `Median`
is a posterior median and is `NA` for frequentist estimates, rather than
labelling an MLE as a posterior median. `SD` is the standard error or
posterior standard deviation of the index, not observation dispersion.
`CV` is `SD / Mean` when the mean is positive. Positive frequentist
indices have delta-method log-scale intervals; other indices have normal
intervals. These intervals are pointwise, not simultaneous. Preview mode
retains the brms posterior mean, but omits uncertainty summaries; it
does not replace joint predictions with predictions at posterior-mean
coefficients.

Working storage is bounded by reference and draw batches plus a compact
annual covariance or draw matrix. The result retains response-scale and,
when defined, log-index covariance matrices even with
`retain = "summary"`. Use
[`index_vcov()`](https://www.quantifish.co.nz/influ2/reference/index_vcov.md)
to extract them and
[`index_table()`](https://www.quantifish.co.nz/influ2/reference/index_table.md)
for reporting. Native prediction code can allocate additional memory.
The result does not retain the model or reference data. Spatial response
estimates evaluate the fitted model at the reference locations in each
observed year. sdmTMB IID group effects are set to zero; tinyVAST
non-spatial temporal effects and smooths remain as fitted. Joint
fixed/latent Gaussian draws propagate field and parameter uncertainty,
with empirical pointwise intervals. `Mean` remains the plug-in expected
response and `Median` remains unavailable for these frequentist models.
This is not a Laplace bias-corrected index or integration over a new
population of random effects. Grid predictions are immediately reduced
to annual values; a grid-by-draw array is never retained. Use
[`integrate_index()`](https://www.quantifish.co.nz/influ2/reference/integrate_index.md)
for area-weighted totals, which have different units. Year-effect
results preserve the original diagnostic estimand and cannot be rescaled
by this function. These indices are not biomass estimates.

## See also

[`index_table()`](https://www.quantifish.co.nz/influ2/reference/index_table.md),
[`index_vcov()`](https://www.quantifish.co.nz/influ2/reference/index_vcov.md),
[`integrate_index()`](https://www.quantifish.co.nz/influ2/reference/integrate_index.md),
[`plot_index()`](https://www.quantifish.co.nz/influ2/reference/plot_index.md),
[`plot_compare()`](https://www.quantifish.co.nz/influ2/reference/plot_compare.md),
[`geo_mean()`](https://www.quantifish.co.nz/influ2/reference/geo_mean.md),
[`influ_indices()`](https://www.quantifish.co.nz/influ2/reference/influ_extractors.md)

## Examples

``` r
if (requireNamespace("glmmTMB", quietly = TRUE)) {
  data(lobsters_per_pot)
  fit <- glmmTMB::glmmTMB(lobsters ~ year + depth + (1 | month),
    family = glmmTMB::nbinom2(), data = lobsters_per_pot)
  index <- cpue_index(fit, year = "year",
    reference_data = data.frame(depth = 40), units = "lobsters per pot")
  head(as.data.frame(index))
  plot_index(index)
}
```
