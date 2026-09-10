# Plot a four-panel CPUE residual diagnostic

Plot a precomputed
[`influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/influ_residuals.md)
result without simulation or refitting.

## Usage

``` r
# S3 method for class 'influ_residuals'
plot(
  x,
  type = c("overview", "qq", "fitted", "year", "distribution", "calibration",
    "calibration_groups"),
  response_scale = c("identity", "log1p"),
  ...,
  response_diagnostic = c("auto", "distribution", "calibration")
)

# S3 method for class 'influ_residuals'
autoplot(object, ...)
```

## Arguments

- x, object:

  An `influ_residuals` object.

- type:

  The four-panel `"overview"` (default), or one of `"qq"`, `"fitted"`,
  `"year"`, `"distribution"`, `"calibration"`, and
  `"calibration_groups"`. Grouped calibration shows
  observed-minus-predicted proportions for the scientific groups chosen
  during calculation.

- response_scale:

  Scale for the response ECDF: `"identity"` or `"log1p"`, which retains
  zero catches. The latter requires non-negative responses and is
  labelled explicitly.

- ...:

  Reserved for future methods; currently unused.

- response_diagnostic:

  Fourth overview panel: `"auto"` chooses probability calibration for
  Bernoulli/encounter responses and the existing ECDF for other families
  (including grouped binomial and combined catch). `"distribution"` and
  `"calibration"` explicitly select a panel. Explicit `type` takes
  precedence. A calibration panel always uses probability axes, never
  `response_scale`. It requires stored fitted-probability summaries.

## Value

A ggplot or a four-panel patchwork object, which can be customised.

## Details

The year panel shows a boxplot for each sampled year and its sample size
through box widths proportional to the square root of the number of
observations. Numeric years retain their spacing, including gaps; other
labels are ordered lexically. Reference lines mark the normal-score
median and quartiles. No smoother across years conceals changes in
spread or tails. The fitted panel's horizontal variable is the
simulation-based predictive mean under the conditioning recorded in the
result. A descriptive loess curve is added when there are sufficient
distinct fitted means.

The Q-Q envelope is a pointwise independent-uniform reference, not a
model-specific calibration. The ECDF envelope is a pointwise predictive
band on a compact grid. Neither envelope provides an automatic pass/fail
test. Read the calculation metadata and
[`influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/influ_residuals.md)
limitations. Calibration uses fixed, roughly equal-count bins of
original fitted probabilities. Grey ranges are pointwise predictive
envelopes for observed bin proportions, not confidence intervals for a
calibration curve. Point size represents observation count; crosses
identify sparse support. Grouped binomial calibration is available
explicitly with known trials; it pools successes/trials and
trial-weights predicted probabilities. Set bin/group options in
[`influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/influ_residuals.md),
not while plotting: discarded simulations cannot be re-binned. Older
objects without response metadata retain the distribution overview with
an informative warning. Explicit distribution plots remain unchanged.
Missing envelopes are not fabricated.

Use `plot(x, type = "qq")` to draw exactly the Q-Q panel from the
overview on its own. Here `x` must be the result of
[`influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/influ_residuals.md),
not an `influ_diag` influence summary. The returned ggplot reuses stored
results; no simulations or model fits are repeated. This is the
supported Q-Q workflow, replacing the retired `plot_qq()`
native-residual helper. It is a different diagnostic, not a reproduction
of that helper's residuals. The Q-Q ribbon here is not posterior
uncertainty around individual points.

## See also

[`influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/influ_residuals.md)
for a worked calculation and standalone Q-Q example.
