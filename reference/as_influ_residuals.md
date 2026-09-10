# Diagnose externally simulated responses

Reduce an already generated response-simulation matrix to the same
compact diagnostic returned by
[`influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/influ_residuals.md).
No model is fitted, and no new responses are simulated.

## Usage

``` r
as_influ_residuals(
  simulations,
  data,
  response,
  year,
  response_kind,
  conditioning,
  component = NULL,
  observation_id = NULL,
  probability = NULL,
  probability_conditioning = NULL,
  trial_counts = NULL,
  batch_size = 25L,
  seed = 1L,
  grid_size = 201L,
  level = 0.95,
  groups = NULL,
  calibration_bins = 10L,
  calibration_min_n = 20L,
  calibration_groups = NULL
)
```

## Arguments

- simulations:

  Numeric matrix with observations in rows and complete joint response
  simulations in columns (at least 20). Row names are required and must
  exactly match the observation IDs in `data`, in the same order. No
  transposition, sorting, recycling, or omission is performed. Optional
  column names must be unique, non-empty simulation identifiers.

- data:

  Data frame containing the matching observations, time labels, and any
  requested grouping or calibration columns. Exactly one row per
  simulation row is required; supply only the rows actually simulated.

- response:

  Name of the numeric observed-response column in `data`. For
  Bernoulli/binomial inputs, supply integer success counts, not
  proportions.

- year:

  Name of the time column in `data`. This is required because no fitted
  formula or native time metadata is available for automatic detection.

- response_kind:

  Explicit response interpretation: `"distribution"` for general counts
  or continuous responses, `"positive_continuous"` for strictly positive
  continuous responses, `"combined"` for a non-negative combined
  hurdle/delta/zero-inflated response, `"bernoulli"` for one-trial
  successes, or `"grouped_binomial"` for known multi-trial successes.
  This is not inferred from observed zeros, ones, or the supplied
  simulations.

- conditioning:

  Required non-empty description of how the response simulations were
  generated, including parameter uncertainty and treatment of random,
  spatial, and spatiotemporal effects. This is a user declaration, not
  something that influ2 can verify from a matrix.

- component:

  `NULL` (default) means `"combined"` when `response_kind` is
  `"combined"`, and `"single"` otherwise. Explicit `"encounter"`
  requires Bernoulli inputs. Explicit `"positive"` requires strictly
  positive inputs with kind `"distribution"` or `"positive_continuous"`.
  These labels declare what was supplied; they never extract, transform,
  or filter components.

- observation_id:

  Optional name of a unique, non-missing ID column in `data`; otherwise
  use its row names. IDs are compared as character strings.

- probability:

  Name of the fitted success-probability column in `data`, required for
  Bernoulli/binomial calibration, and disallowed otherwise. Supply
  original fitted probabilities, not row means of the response
  simulations. For posterior predictions, average expected probabilities
  over the same posterior draw IDs used for the response simulations.

- probability_conditioning:

  Required description of the conditioning used for `probability`, when
  supplied. Fixed probability bins may use different conditioning from
  response simulations (e.g. fitted versus resimulated random effects);
  record that difference explicitly.

- trial_counts:

  Name of a positive-integer trial-count column in `data`. Required for
  `"grouped_binomial"`, where at least one row must have more than one
  trial. Bernoulli defaults to one trial per row. Never use arbitrary
  fitting or area weights as trial counts.

- batch_size:

  Maximum number of supplied simulation columns processed in each batch.
  No additional responses are generated.

- seed:

  Integer random seed. The caller's random-number state is restored.

- grid_size:

  Approximate number of ECDF grid points, at least 20.

- level:

  Pointwise predictive interval coverage for ECDFs, and nominal
  independent-uniform reference coverage for the Q-Q panel.

- groups:

  Optional names of original-data grouping columns to retain for
  [`plot_implied_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot_implied_residuals.md).
  Specify these independently of the response. Only these columns, not
  the complete model data, are stored.

- calibration_bins:

  Requested number of roughly equal-observation-count probability bins.
  Defaults to 10; ties are never split. Near ties within `1e-8` are kept
  together, and under-supported bins are merged.

- calibration_min_n:

  Minimum observation count per probability bin, default 20. Smaller
  datasets remain a single flagged sparse bin. Scientific groups below
  this size remain visible but have no predictive envelope.

- calibration_groups:

  Optional character vector of columns in `data` defining a joint
  scientific grouping, e.g. `c("year", "target")`. These columns must
  not be defined from the outcome.

## Value

An `influ_residuals` object, using the same plotting and grouped
residual helpers as fitted-model results. Metadata identifies external
input, explicit alignment, response component, and declared
conditioning.

## Details

Each column must be a whole response vector from the intended predictive
distribution, not fitted means, parameter draws, or PIT values. Existing
within-column dependence is preserved. The predictive mean and response
ECDF are calculated from these same supplied simulations. No separate
fitted-mean argument can introduce a different prediction target.

Validate native output order before assigning row names. For example,
brms posterior predictions normally need transposition to put
observations in rows; do not guess orientation from a square matrix. The
caller is responsible for the provenance of the IDs and declared
response structure. Matrix alignment checks cannot detect incorrectly
assigned identifiers.

A combined-response diagnostic is not a positive-component diagnostic.
For the latter, generate positive-component simulations at the matching
observed-positive rows. Do not remove zeros separately within simulation
columns or relabel a combined matrix as a component calculation. No
conversion of censored, multivariate, weighted, or other special
response encodings is provided; resolve the predictive target before
using this API.

The shared engine randomises ties and applies `qnorm(pit)` exactly as
for fitted models. The caller's random-number state is restored,
including on error. Reusing the same matrix, seed, batch size, and RNG
kind reproduces the result. A native adapter can consume random numbers
when preparing simulations (e.g. selecting posterior draws), so an
identical seed alone does not guarantee identical ranks between the two
entry points.

This initial interface accepts an in-memory matrix, not a generator or
on-disk stream. It is processed in observation-by-batch blocks; the
returned object retains neither the matrix nor a closure capturing it.
The caller's original matrix still occupies memory until they release
it. Temporary ECDF/calibration storage and the first-batch ECDF grid
follow the existing
[`influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/influ_residuals.md)
calculation. Changing batch size can change that grid, but not the ranks
or sequentially calculated predictive means for fixed supplied
simulations. No counters or full simulation-retention mode is added.

These remain exploratory predictive checks. Fitted-data ranks are not
automatically uniform or calibrated for parameter estimation, posterior
data reuse, or latent dependence. This constructor does not implement
LOO-PIT, OSA, or validate a model-specific simulation scheme.

## See also

[`influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/influ_residuals.md),
[`plot.influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot.influ_residuals.md),
[`plot_predicted_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot_predicted_residuals.md),
[`plot_implied_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot_implied_residuals.md)

## Examples

``` r
d <- data.frame(year = rep(2010:2012, each = 10),
  cpue = rep(c(0, 1, 2, 3, 4), 6))
# A small supplied-simulation example, not a fitted CPUE model.
set.seed(42)
sims <- matrix(rpois(nrow(d) * 30, lambda = 2), nrow = nrow(d),
  dimnames = list(rownames(d), NULL))
checks <- as_influ_residuals(sims, d, response = "cpue", year = "year",
  response_kind = "distribution",
  conditioning = "Independent Poisson responses at a fixed mean of 2")
checks
#> Simulation-based residual diagnostics (external)
#> 30 observations; 30 simulations
#> Time: year [ explicit external column ]
#> Independent Poisson responses at a fixed mean of 2
#> Exploratory ranks; not a calibrated goodness-of-fit test
```
