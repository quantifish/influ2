# Calculate an area-integrated expected-response index

Sum expected responses times supplied cell areas over a common
prediction domain in every observed year. This is a total, not a
standardisation mean. No model is refitted and no observation-level
predictive noise is added.

## Usage

``` r
integrate_index(
  model,
  reference_data,
  area,
  year = NULL,
  area_units,
  response_units,
  catchability = NULL,
  units = NULL,
  uncertainty = c("auto", "none"),
  probs = c(0.025, 0.975),
  rescale = "raw",
  ndraws = 1000L,
  batch_size = 250L,
  draw_batch_size = 100L,
  retain = c("summary", "draws"),
  averaging_weights = NULL,
  spatial_fields = c("all", "spatial", "spatiotemporal", "none"),
  seed = 1L,
  prediction_offset = NULL
)
```

## Arguments

- model:

  A fitted GLM, `mgcv` GAM, `glmmTMB`, complete `brmsfit`, `sdmTMB`, or
  univariate `tinyVAST` model for response standardisation. For
  `method = "year_effect"`, any model supported by
  [`influ()`](https://www.quantifish.co.nz/influ2/reference/influ.md) or
  an existing `influ_diag` can be supplied.

- reference_data:

  A non-empty data frame defining a common covariate profile or
  population, **without** the year column. Each row is predicted in
  every observed year. Supply all required predictors and any exposure
  variables explicitly; use one unit of exposure for a per-unit index.

- area:

  Cell areas, as a numeric vector of length `nrow(reference_data)`, one
  explicitly supplied common cell area, or the name of a numeric column
  in `reference_data`. Zero-area cells are excluded. At least one must
  be positive. Areas must not overlap or double-count the domain.

- year:

  Name of the year variable. Defaults to the native time variable for
  spatial backends, otherwise the first formula predictor.

- area_units:

  Non-empty area-unit label, e.g. `"km^2"`. Supply areas in the same
  area units as the denominator of the density response. No unit
  conversion or coordinate-based area inference is performed.

- response_units:

  Non-empty label for the model's response at the supplied reference
  exposure, e.g. `"kg/km^2"` or `"kg/tow"`.

- catchability:

  Optional known positive conversion from underlying density to expected
  CPUE: predictions are divided by this quantity before integration.
  Supply a scalar, one value per reference row, or a column name. `NULL`
  applies no conversion. Uncertainty in catchability is not propagated.
  Its units must convert the stated response into density per
  `area_units`; a dimensionless value cannot convert kg/tow into
  kg/km^2.

- units:

  Optional resulting-unit label. By default the response and area labels
  are combined explicitly, without claiming absolute biomass.

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

- averaging_weights:

  Optional non-negative weights for repeated seasonal or other reference
  strata within each cell, supplied like `area`. For a seasonal mean,
  these must sum to one within each spatial cell (e.g. `1 / 12` for
  twelve equally weighted months). They are not normalised internally.
  `NULL` uses one per row, appropriate for one row per cell.

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

## Value

An `influ_index` with the same assessment-table columns as
[`cpue_index()`](https://www.quantifish.co.nz/influ2/reference/cpue_index.md),
`Method = "integrated"`, and area/unit metadata. Use
[`plot_index()`](https://www.quantifish.co.nz/influ2/reference/plot_index.md)
or
[`plot_compare()`](https://www.quantifish.co.nz/influ2/reference/plot_compare.md)
without repeating calculations.

## Details

The calculation is
`sum(area * averaging_weights * expected_response / catchability)`
within each year; the divisor is one when catchability is `NULL`.
`reference_data` has no year column: the same domain and reference
covariates are used in every observed year. Temporal model effects and
included spatiotemporal fields can still change predictions by year.
This interface does not forecast, infer a domain, or construct a
year-varying environmental grid.

A density in kg/km^2 multiplied by km^2 yields kg. CPUE in kg/tow
multiplied by km^2 remains an area-weighted CPUE index, **not absolute
biomass**, unless an appropriate catchability/exposure conversion is
supplied. Area-integrated encounter probabilities describe expected
occupied area under the stated encounter definition, not abundance.
Supplying a unit label does not validate these scientific assumptions.

All six
[`cpue_index()`](https://www.quantifish.co.nz/influ2/reference/cpue_index.md)
backends are supported with the same prediction conventions and
model-structure guards. Smooth spatial predictors in GLMs and GAMs can
be integrated just like dedicated spatial fields. Joint covariance or
shared draws propagate dependence across cells and years; cell standard
errors are never added as if predictions were independent. Frequentist
estimates are plug-in expectations, not Laplace bias-corrected totals.
brms summaries use posterior expected-response draws. Areas, reference
covariates, and catchability are treated as known.

`rescale = 1` returns a relative series with geometric mean one and
propagates uncertainty in its common normalising denominator. A raw
total cannot be recovered from that relative result alone. Standardised
means and area-integrated totals cannot be mixed silently in
[`plot_compare()`](https://www.quantifish.co.nz/influ2/reference/plot_compare.md).

## See also

[`cpue_index()`](https://www.quantifish.co.nz/influ2/reference/cpue_index.md),
[`plot_index()`](https://www.quantifish.co.nz/influ2/reference/plot_index.md),
[`plot_compare()`](https://www.quantifish.co.nz/influ2/reference/plot_compare.md)

## Examples

``` r
d <- data.frame(year = factor(rep(1:3, each = 20)),
                depth = rep(seq(10, 50, length.out = 20), 3))
d$density <- exp(1 + 0.1 * as.numeric(d$year) - 0.01 * d$depth) +
             rep(c(-0.1, 0.1), 30)
fit <- glm(density ~ year + depth, family = Gamma(link = "log"), data = d)
grid <- data.frame(depth = c(15, 30, 45))
total <- integrate_index(fit, grid, area = c(2, 3, 5),
  area_units = "km^2", response_units = "kg/km^2", units = "kg")
as.data.frame(total)
#>   Year     Mean Median        SD          CV   Qlower   Qupper     Method
#> 1    1 21.45560     NA 0.2074505 0.009668825 21.05283 21.86607 integrated
#> 2    2 23.71106     NA 0.2292581 0.009668825 23.26596 24.16469 integrated
#> 3    3 26.20374     NA 0.2533593 0.009668825 25.71184 26.70505 integrated
#>   Distribution Link
#> 1        Gamma  log
#> 2        Gamma  log
#> 3        Gamma  log
```
