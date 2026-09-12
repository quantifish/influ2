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
    "calibration_groups", "pit_ecdf", "pit_ecdf_diff"),
  response_scale = c("identity", "log1p"),
  ...,
  response_diagnostic = c("auto", "distribution", "calibration"),
  panels = NULL,
  pit_grid_size = 100L
)

# S3 method for class 'influ_residuals'
autoplot(object, ...)
```

## Arguments

- x, object:

  An `influ_residuals` object.

- type:

  The four-panel `"overview"` (default), or one of `"qq"`, `"fitted"`,
  `"year"`, `"distribution"`, `"calibration"`, `"calibration_groups"`,
  `"pit_ecdf"`, and `"pit_ecdf_diff"`. Grouped calibration shows
  observed-minus-predicted proportions for the scientific groups chosen
  during calculation.

- response_scale:

  Scale for the response ECDF: `"identity"` or `"log1p"`, which retains
  zero responses. The latter requires non-negative responses and is
  labelled explicitly.

- ...:

  Reserved for future methods; currently unused.

- response_diagnostic:

  Fourth overview panel: `"auto"` chooses probability calibration for
  Bernoulli/encounter responses and the existing ECDF for other families
  (including grouped binomial and combined responses). `"distribution"`
  and `"calibration"` explicitly select a panel. Explicit `type` takes
  precedence. A calibration panel always uses probability axes, never
  `response_scale`. It requires stored fitted-probability summaries.

- panels:

  Optional character vector of exactly four panel types, in row-wise
  order, used only with `type = "overview"`. Any standalone type above
  is allowed, including repetitions. `"auto"` selects the response check
  using `response_diagnostic`. The default `NULL` is equivalent to
  `c("qq", "fitted", "year", "auto")`. Required summaries must already
  exist in `x`; selecting a panel never recalculates residuals.

- pit_grid_size:

  Number of equal subdivisions of `[0, 1]` for the PIT ECDF and its
  simultaneous reference limits, between 2 and 1000 (default 100). There
  are `pit_grid_size + 1` evaluation points, including zero and one.
  Used only for PIT-ECDF panels, not for the stored response ECDF grid.

## Value

A ggplot or a four-panel patchwork object, which can be customised.

## Details

In the default overview, panels A-C use simulation-based randomised PIT
(probability integral transform) ranks on the standard-normal scale,
`qnorm(pit)`. The overview caption identifies the selected panels and
distinguishes panel D: a response ECDF or probability-calibration check,
not a PIT-residual distribution. Transforming the ranks does not
establish normality or model calibration.

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

Use `plot(x, type = "distribution")` for the standalone response ECDF,
including when a Bernoulli overview defaults to calibration. It reuses
the stored observed ECDF, simulated median, and pointwise predictive
band. This is not an ECDF of residuals or a LOO-PIT diagnostic. brms
results summarise existing posterior predictive draws, not new MCMC.

`type = "pit_ecdf"` uses the optional package **bayesplot** to plot the
ECDF of the stored PIT values against a uniform reference. The
difference version, `"pit_ecdf_diff"`, plots `ECDF(u) - u` against PIT
value `u`, with zero as the reference. These reuse the same ranks as the
normal-score Q-Q plot, not new residuals, an analytic PIT, or LOO-PIT.
`response_scale` does not change their uniform horizontal scale.

The PIT plots delegate to
[`bayesplot::ppc_pit_ecdf()`](https://mc-stan.org/bayesplot/reference/PPC-distributions.html)
with `method = "independent"`, numerically adjusted simultaneous
reference limits, and the stored `level`. The limits assume independent
uniform PIT values; they are not fitted-model-calibrated bands, and do
not correct parameter estimation, posterior predictive reuse, or latent
dependence. bayesplot's alternative dependence-aware tests are not
automatically applied to these fitted-data ranks. No p-value, refit, or
further response simulation is requested. The bridge does not change
bayesplot's global theme or colours. influ2 aligns the limits and
empirical CDF on `(0:K) / K`, where `K = pit_grid_size`, correcting a
plotting-grid mismatch in bayesplot 1.16.0 without changing its interval
calculation. Differences subtract this same grid from every curve. The
zero-endpoint limits are zero; observed PIT values equal to zero or one
are retained, not jittered. Already aligned dependency output is not
shifted again; unrecognised versions/layouts fail explicitly rather than
guessing about their limits. The simultaneous reference applies at the
evaluation points, not every point between them. The returned plot's
`pit_reference` attribute records the dependency version, grid size, and
alignment action.

## See also

[`influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/influ_residuals.md)
for a worked calculation and standalone Q-Q and ECDF examples;
[`vignette("residual-diagnostics")`](https://www.quantifish.co.nz/influ2/articles/residual-diagnostics.md)
for Bayesian examples.
[`as_influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/as_influ_residuals.md)
creates the same plotting object from supplied simulations.
