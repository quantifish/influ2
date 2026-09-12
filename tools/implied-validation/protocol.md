# IV01: bounded NB2 residual-implied-effect validation

Design fixed on 12 September 2026, before production simulations. This is a
separate follow-up to N09, not a replacement for its frozen results. It tests
the existing implied-effect calculation without changing any package code,
defaults, uncertainty methods, or supported backends.

## Aims and design

Check null behaviour, recovery of deliberately omitted group trends, and the
effect of uneven sampling on estimates, conditional intervals, and plot gaps.
Use 100 independently generated datasets in each of four scenarios: balanced
or uneven sampling, crossed with no interaction or opposing seasonal trends.
Pilot IDs 1001--1003 are for timing and implementation checks only; production
IDs are 1--100. Never replace failed fits or choose examples by their appearance.

Six years (2011--2016), three seasons, and 12 vessel random intercepts are used.
The balanced design has 36 records per season-year cell (648 in total). The
uneven counts, with years in rows and Early/Mid/Late season in columns, are:

```
108 24  12
 80 24   9
 12  4   9
  0 36  60
 12  9  96
  9 24 120
```

Both designs have 648 records. The uneven design has one empty cell, five cells
below the default `min_n=10`, and 12 supported cells. Counts were reconciled by
the deterministic design checks before any pilot or production results.
The design, depth covariate, and vessel allocation are
fixed before responses are generated. Vessel effects are independently redrawn
for every dataset from N(0, 0.4^2). Responses are NB2 count CPUE per standardised
sampling unit, with log link, size 4, and variance mu + mu^2/4. There is no
exposure offset, zero-inflation, hurdle component, or spatial field.

The additive predictor is log(4) + year effect + season effect + 0.3 times
standardised depth + realised vessel effect. Year effects are
(-0.2, -0.1, 0.1, 0.25, 0.1, 0.2), and season effects are (-0.2, 0, 0.2).
The omitted pattern is 0.6 times scaled time (-1 to 1) times the season score
(-1, 0, 1); it is zero in the null scenario. This is one fixed effect size,
not a power curve. Scenario-specific seed schedules are recorded in `study.R`.

Each dataset is fitted once by ML using glmmTMB:
`response ~ year + season + depth_z + (1 | vessel)`, family `nbinom2()`.
Both main effects, the covariate, and the vessel effect are retained; only the
season-by-year interaction is omitted in the signal scenario. Eligible fits
have native convergence code zero, a positive-definite Hessian, and finite
log likelihood. Warnings and failures remain in the record; no optimiser retry,
replacement seed, or change of the generating effect is allowed after inspection.

## Two targets, not one implied interaction estimator

1. **Known-parameter control.** Apply the existing local shift/profile routines
   with the true additive predictor, realised vessel effects, and size fixed.
   The injected cell shift is then the exact parameter. Pointwise interval
   coverage here evaluates the profile construction under its ideal assumptions.
   These are developer controls, not fabricated fitted-model objects.
2. **Fitted-model diagnostic.** Call the public `implied_effects()` on the actual
   eligible fit. Preserve the whole fit, fitted values, and random-number state.
   Since estimated main effects and nuisance parameters can absorb signal,
   raw injected interactions are not necessarily the correct local target.
   Define the fitted-conditional target as the maximiser of expected NB2 log
   likelihood, using known generating means but holding that fitted predictor
   and size fixed. An independent native-density calculation verifies this
   target, with one-dimensional optimisation of the parameter-dependent kernel:
   sum(mu_true * (eta + delta) - (mu_true + phi_hat) * log(phi_hat + exp(eta + delta))).
   Constants independent of delta do not affect its maximiser. The expected
   score is sum(phi_hat * (mu_true - exp(eta + delta)) /
   (phi_hat + exp(eta + delta))). It is zero at the target.

The fitted target depends on the observed-data fit. Interval containment of it
is **not** confidence-interval coverage for a fixed population interaction.
The plot's baseline also has estimation uncertainty that these bars omit.
Neither control authorises a claim of calibrated fitted-model significance tests.
Do not add PIT/Pearson scores to coefficients or treat local shifts as regional
CPUE indices. Retain the distinction between adjustment and baseline + adjustment.

## Pre-specified outcomes and denominators

Retain one compact row for every year-season cell and route, including empty,
sparse, boundary, and calculation-failure states. Do not turn missing values
into zeros. All-zero supported cells retain the algorithm's `boundary_zero`
state and -Inf adjustment; report their number separately. No artificial
pseudocount or study-specific minimum replaces the package default.

Summarise per dataset first, then across the 100 independent datasets:

- Mean adjustment error, root mean squared error, and target containment among
  finite supported cells, with explicit usable-cell and usable-dataset counts.
- Fraction of conditional intervals excluding zero, and whether any supported
  cell does so in a dataset. The latter is not multiplicity-adjusted.
- In signal cases, direction recovery and zero exclusion for pre-defined strong
  cells: absolute injected shift >= 0.3. Show weaker cells too, but do not select
  them after looking at the results.
- Interval width by observed cell size; sparse/empty cells remain unestimated.
- Mean fitted-conditional target versus raw injected pattern, to show absorption
  into the fitted model rather than mislabelling it as a numerical error.

Monte Carlo standard errors use variation **between datasets**, not independent
binomial assumptions for the correlated cells in a fitted dataset. Report pointwise
and any-cell exclusion separately. Study summaries are descriptive, not automatic
pass/fail thresholds; simulation uncertainty must not be hidden by rounding.

For worked public plots, choose the first replicate for which all four fits
and calculations are eligible, before inspecting curves. Retain its compact
`influ_implied` objects and cell-level truth, not the fitted models or observations.
Any examples show conditional local targets on the same fitted baseline; they
are not full-interaction truth curves on an independently normalised scale.

## Reproduction and scope

Developer tests verify the generator, known-parameter recovery, independent NB
likelihood/score/profile calculations, status denominators, fit preservation,
and line breaks. Production source, runtime, package versions, seeds, and failures
are recorded. Compile compact results separately; package tests and vignette
builds audit/read the frozen artefact rather than rerunning Monte Carlo fits.

One backend, one family, one effect size, two fixed designs, and 100 datasets per
scenario do not validate other families, brms, GAM smooths, spatial models,
informative vessel selection, two-part responses, or general interval calibration.
The known-parameter and fitted-model checks are paired on the same responses,
but different scenarios are independently generated. Conclusions belong to this
bounded experiment and remain for scientific interpretation with Nicholas.
