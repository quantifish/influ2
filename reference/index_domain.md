# Diagnose prediction-domain support and index dependence

Classify an explicit prediction domain, decompose native link
predictors, and compare full-domain indices with restricted-domain and
term-omission predictions. No model is refitted.

## Usage

``` r
index_domain(
  model,
  reference_data,
  year = NULL,
  sampled = NULL,
  sampled_definition = c("cell_time", "cell_ever", "cell_always"),
  min_obs = 1L,
  min_years = 1,
  sampled_only = TRUE,
  area = NULL,
  area_units = NULL,
  response_units = NULL,
  reference_weights = NULL,
  decompose = TRUE,
  omit = character(),
  where = c("unsampled", "all"),
  covariate_reference = "sampled_mean",
  ndraws = 500L,
  seed = 1L,
  batch_size = 250L,
  draw_batch_size = 100L,
  retain_cells = NULL,
  uncertainty = c("auto", "none"),
  probs = c(0.025, 0.975),
  trend_window = NULL,
  core_threshold = 0.05,
  unsampled_threshold = 0.5,
  share_change_threshold = 0.2,
  prediction_offset = NULL,
  units = NULL,
  omission_definition = c("cell_time", "cell_ever", "cell_always"),
  data = NULL,
  spatial_terms = NULL,
  spatiotemporal_terms = NULL
)

# S3 method for class 'influ_domain'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  what = c("coverage", "decomposition", "omission", "sampled_only", "partition", "trend")
)

# S3 method for class 'influ_domain'
summary(
  object,
  ...,
  what = c("coverage", "decomposition", "omission", "sampled_only", "partition", "trend")
)

# S3 method for class 'influ_domain'
print(x, ...)

# S3 method for class 'influ_domain'
plot(
  x,
  type = c("sampled_only", "coverage", "decomposition", "omission", "partition", "share",
    "map"),
  show_probs = TRUE,
  coords = NULL,
  years = NULL,
  terms = NULL,
  components = NULL,
  ...
)

# S3 method for class 'influ_domain'
autoplot(object, ...)
```

## Arguments

- model:

  A complete fitted sdmTMB, univariate tinyVAST, or single-predictor
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) model.

- reference_data:

  A non-empty prediction data frame without `year`.

- year:

  Year column; defaults to the model's native time variable, or the
  first formula predictor for a GAM.

- sampled:

  Sampling rule: a shared cell-ID column, a list
  `list(coords = c("X", "Y"), distance = d)`, or a logical matrix with
  reference rows and years in index order. When omitted or `NULL`, use
  half the nearest distinct grid-centre spacing, with a warning. Supply
  a rule explicitly when this default does not describe the sampling
  design. GAMs always require an explicit rule; coordinates are not
  inferred. Cell-ID and coordinate rules count observed fitted responses
  with positive likelihood weights and, where applicable, positive trial
  counts. A logical matrix instead declares support directly; its
  interpretation is the caller's responsibility.

- sampled_definition:

  One or more of `"cell_time"`, `"cell_ever"`, `"cell_always"`. All are
  computed by default for sampled-only comparisons.

- min_obs:

  Minimum observations per cell-year.

- min_years:

  Minimum sampled years for `"cell_ever"`; a positive count, or a
  proportion strictly between zero and one (rounded up).

- sampled_only:

  Return a restricted index for each definition?

- area:

  Optional explicit cell areas (numeric values or a column name).
  Supplying areas requests totals rather than weighted means.

- area_units, response_units:

  Required unit strings when supplying area.

- reference_weights:

  Non-negative reference weights. With area these are within-cell
  averaging weights, multiplied by area and not normalised internally.
  For a seasonal mean, supply weights summing to one within each spatial
  cell. Otherwise `sum(area * reference_weights)` is effective weighted
  area, not necessarily unique geographic area.

- decompose:

  Summarise link-scale terms by year and domain class?

- omit:

  Native term names to omit, including `"year"`, `"spatial"`,
  `"spatiotemporal"` and `"svc"`. `"spatial"` removes persistent and SVC
  fields together, matching `spatial_fields = "spatiotemporal"` in
  [`cpue_index()`](https://www.quantifish.co.nz/influ2/reference/cpue_index.md);
  `"svc"` removes only SVC fields. Unknown names are errors.

- where:

  One or more of `"unsampled"`, `"never_sampled"`, `"all"`.

- covariate_reference:

  `"sampled_mean"` (default), `"zero"`, or a named list of rules per
  omitted covariate. A fixed rule is
  `list(rule = "fixed", value = list(depth = 40))`.

- ndraws, seed, batch_size, draw_batch_size, uncertainty, probs,
  prediction_offset, units:

  As in
  [`cpue_index()`](https://www.quantifish.co.nz/influ2/reference/cpue_index.md).

- retain_cells:

  Optional year labels for which to keep cell-level terms.

- trend_window:

  Optional year labels defining a second trend window.

- core_threshold:

  Warn below this retained-weight share for fixed domains.

- unsampled_threshold, share_change_threshold:

  Flag years exceeding these unsampled-share and
  consecutive-share-change thresholds.

- omission_definition:

  A single definition for unsampled omission masks; defaults to
  `"cell_time"` independently of `sampled_definition`.

- data:

  For GAMs, optional original observation data retaining fitted row
  names and values. Use this to supply cell IDs or coordinates not in
  the saved model frame. Without it only the retained frame is used;
  mutable external fitting calls are not reevaluated. Native spatial
  fits use their stored data and do not accept this argument.

- spatial_terms, spatiotemporal_terms:

  For GAMs only, explicit native smooth labels to group under
  `"spatial"` or `"spatiotemporal"`. No smooth is classified from its
  name or dimensionality automatically. Ungrouped smooths retain labels
  such as `"s(X,Y)"` and can be omitted by that label.

- x, object:

  An `influ_domain` result from `index_domain()`.

- row.names, optional:

  Passed to
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- ...:

  Reserved for future use.

- what:

  Stored table to extract or summarise: `"coverage"` (default),
  `"decomposition"`, `"omission"`, `"sampled_only"`, `"partition"`, or
  `"trend"`. Index comparisons include the full-domain baseline and a
  `Series` column; their stored metadata are attached as
  `index_metadata`.

- type:

  Plot `"sampled_only"` (default), `"coverage"`, `"decomposition"`,
  `"omission"`, `"partition"`, `"share"`, or `"map"`.

- show_probs:

  Display stored pointwise intervals when available.

- coords:

  Two coordinate column names in retained cell contributions. If `NULL`,
  use `X`/`Y`, `x`/`y`, or `lon`/`lat`, when available.

- years, terms, components:

  Optional selectors for map years, terms, and components, matching the
  stored labels.

## Value

An `influ_domain` containing ordinary `influ_index` objects in
`baseline`, `sampled_only`, and `omission`; coverage, decomposition,
response-scale partition and trend tables; shared annual
`partition_draws`; and optional `retained_cells`.

Table methods return a data frame, with index metadata attached for
index comparisons. [`print()`](https://rdrr.io/r/base/print.html)
invisibly returns its input. Plot methods return a ggplot, or a
patchwork for sampled-only and omission comparisons.

## Details

The default sampling distance is a warned grid-spacing heuristic, not
inferred cell areas. Sampling interpretation depends on the supplied
cell size or distance. Reporting classes always describe actual
same-year support: `sampled`, `previously_sampled` (sampled in another
year, not necessarily an earlier year), and `never_sampled`. The chosen
definitions govern omission masks and sampled-only domains.

Contributions add on the link scale, separately for delta components;
they must not be back-transformed and added on the response scale.
Decomposition quantiles use reference-weighted, 1024-bin histograms
spanning each group's observed range; their resolution is recorded in
metadata. Means and SDs use all positive-weight cells. Only compact
annual summaries and histograms are accumulated, unless cells are
retained.

Omission is a prediction-target counterfactual, **not a refit**; use
[`plot_step()`](https://www.quantifish.co.nz/influ2/reference/plot_step.md)
to study refitted models. Fields are set to zero; the year term is
replaced by its first-year value at the same reference row. Separate
year interactions and time-varying terms remain unless selected.
`"sampled_mean"` replaces a covariate's additive contribution by its
reference-weighted mean over cells sampled that year, separately in each
joint draw and delta component, regardless of `omission_definition`. It
is not the prediction at a mean raw covariate. `"zero"` is appropriate
only for deliberately centred terms. `"fixed"` reevaluates the term at
explicit raw predictor values, retaining its other predictors; this also
handles interactions and smooths. These contrasts depend on the fitted
decomposition and its centring and identifiability constraints.
Correlated terms can share signal; omission is not a causal effect or a
unique allocation of importance or variance.

Weighted-mean restricted indices renormalise weights each year. An empty
restricted mean is undefined (`NA`); an empty restricted total is zero.
`"cell_time"` is a variable-domain diagnostic, not an alternative index.
Use this object's sampled-only plot for intentional comparisons of
different domain areas. Ordinary
[`index_ratio()`](https://www.quantifish.co.nz/influ2/reference/index_ratio.md)
and
[`plot_compare()`](https://www.quantifish.co.nz/influ2/reference/plot_compare.md)
preserve their equal-area safeguards; do not change area metadata to
bypass them.

GAMs use blocked native linear-predictor matrices and jointly sampled
coefficients. Smoothing-parameter corrected covariance is used when
available; otherwise it is conditional on fitted smoothing parameters.
Likelihood nuisance parameters are held fitted. This is Gaussian
coefficient-simulation uncertainty, not new MCMC or the delta-method
approximation used by ordinary
[`cpue_index()`](https://www.quantifish.co.nz/influ2/reference/cpue_index.md)
GAM summaries. Natural single-response Gaussian, binomial, Poisson,
Gamma, negative-binomial, and Tweedie GAMs are supported;
multi-predictor distributional families and transformed responses fail
explicitly. Formula offsets are retained. Slopes use numeric year labels
when available, otherwise ordered year positions. All comparisons use
paired joint Gaussian draws, not independent marginal intervals.
Nonpositive series have undefined log comparisons; no small positive
replacement is made.

Plotting and table extraction reuse stored results; they do not predict
again or refit a model. Sampled-only comparisons divide every displayed
series and its interval by the geometric mean over years with finite
positive estimates in every displayed series. Weighted means use the
full index's geometric mean for every series, preserving level
differences. Area totals use each series' own geometric mean over that
common period to compare shapes rather than different domain areas. This
display-only normalisation leaves stored results unchanged; the lower
panel shows coverage, not uncertainty in the sampling process.
Unavailable years remain gaps. Other index panels retain the stored
index scale. Decomposition ribbons show stored spatial contribution
quantiles (`Q10` to `Q90`), not parameter-uncertainty intervals.

Omission is a prediction counterfactual, not a refitted reduced model.
Contributions are additive on the link scale, not on the response scale.
A sampled-only domain can change between years and is not a fixed-domain
abundance index. Classification depends on the declared mapping and
sampling thresholds, not on interpolation or a mesh boundary. When no
sampling rule is supplied, the default half-grid-spacing distance rule
warns: it is a convenience convention, not an inferred survey footprint.
All three sampled-only definitions can be compared while omission uses
the single stored `omission_definition` (by default `"cell_time"`).
Year-effect omission uses the first year as its baseline. Covariate
omission defaults to the sampled-year mean link contribution as its
reference; this is not prediction at a mean response-scale covariate.

## See also

[`cpue_index()`](https://www.quantifish.co.nz/influ2/reference/cpue_index.md),
[`integrate_index()`](https://www.quantifish.co.nz/influ2/reference/integrate_index.md),
[`index_ratio()`](https://www.quantifish.co.nz/influ2/reference/index_ratio.md)
