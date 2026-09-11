# N09: bounded residual-validation protocol

Approved by the maintainer on 12 September 2026. This first increment is a
scientific audit, not approval to change diagnostics, defaults, or simulation
conditioning. Other backends, hurdle/delta models, regional indices, legacy
triage, and CRAN submission remain outside this increment.

## Design fixed before the production simulations

- 100 independent generated datasets per backend; three disjoint pilot IDs
  (1001--1003) check execution, convergence, and timing, not diagnostic power.
- 480 observations in six years; one fixed covariate/sampling design per
  backend. New latent effects and NB2 responses are generated per dataset.
- glmmTMB: 20 vessels, four observations per vessel/year, normal vessel
  intercept SD 0.7; fit `response ~ factor(year) + x + (1 | vessel)` and a
  model omitting `x`.
- sdmTMB: 80 fixed sampling stations in a 100 by 100 coordinate domain,
  a fixed 25-knot mesh, persistent Matérn spatial SD 0.45, iid yearly
  spatiotemporal SD 0.6, shared range 30; fit the full model, omit `x`, or
  omit the spatiotemporal field. Use the same mesh for generation and fitting
  to isolate residual behaviour, not mesh approximation error.
- Both backends: log link, intercept log(5), year contrasts
  `(0, 0.2, 0.35, 0.3, 0.1, -0.1)`, covariate coefficient 0.7, and NB2 size
  4, so conditional variance is `mu + mu^2 / 4`. The spatial simulator is
  native `sdmTMB::simulate_new()` with explicit parameters, not a fitted-data
  bootstrap. Response is a count-per-standardised-sampling-unit CPUE example.
- Fit every distinct model once per dataset by ML. No adaptive tuning,
  replacement datasets, best-seed selection, or hidden convergence retries.
  Record errors, warnings, convergence codes, Hessian status, gradients, and
  run time. Exclude unsuccessful/non-positive-Hessian fits from diagnostic
  summaries but retain their attempted-fit denominator and exclusion reasons.
- glmmTMB conditioning: `fitted`, `new_effects`; sdmTMB: `fitted`,
  `conditional_draw`, `new_effects`. These are different predictive targets,
  not interchangeable ways to obtain the same residuals.
- Main diagnostic: 499 response simulations, batches of 50. For IDs 1--10,
  also use a second diagnostic seed at 499 simulations and the primary seed
  at 1,999 simulations. No extra model fitting for sensitivity runs.
- Seeds are arithmetic functions of backend, replicate ID, and purpose;
  store every seed and the package/source versions. Pilot IDs never enter
  production summaries. Store compact results and checkpoints, not fitted
  TMB objects or observation-by-simulation arrays.
- Illustrative figures use the first replicate with successful comparisons
  for all planned scenarios within a backend; this rule is unrelated to the
  appearance of its residual plots. Report the chosen IDs.

## Outcomes, controls, and interpretation

The primary scalar distribution discrepancy is the two-sided empirical-CDF
distance from uniformity. Record crossing of the 95% Dvoretzky--Kiefer--Wolfowitz
(DKW) reference, `sqrt(log(2 / 0.05) / (2 * n))`. This is an iid-uniform reference
bound, **not a calibrated fitted-model hypothesis test**. Estimated parameters
and dependent predictive targets can change its operating characteristics.

Also record normal-score mean and SD, the fraction of Q-Q points outside their
pointwise reference limits, residual association with fitted means and `x`,
between-year residual means, pointwise response-ECDF departures, and a
descriptive within-year nearest-neighbour spatial score. These are summaries
of complementary diagnostics, not an automatic model-selection score or a
new family of calibrated significance tests.

For every dataset, calculate analytic randomised NB PIT using the known
conditional means, realised latent effects, and size. Independently generate
499 responses from that same known distribution and pass them through
`as_influ_residuals()`. These oracle controls distinguish finite-simulation
rank behaviour from the consequences of estimating a model.

Use Wilson 95% Monte Carlo intervals for proportions and retain paired
replicate IDs for seed/count sensitivities. With 100 datasets, rates near 5%
are estimated only to a few percentage points. Report results conditional on
fit success, alongside failures. Do not claim equivalence across backends,
universal calibration, performance for other families, or out-of-sample skill.

## Preliminary reference-band audit (before fitted-model simulations)

An initial independent-uniform control exposed a bayesplot 1.16.0 grid mismatch:
`ecdf_intervals()` uses `(0:K) / K`, while the plotting layer places its last K
limits on `seq(0, 1, length.out = K)`. With n=480, K=100, and 10,000 samples
(seed 90912), crossing rates were 12.46% as displayed, 4.77% with the reference
limits placed at their own grid, and 4.71% for DKW. This is separate from
fitted-model calibration and motivates the independent primary reference.

Preserve and reproduce this audit in the study results. Record the currently
displayed optional-band crossing as a secondary outcome, not a calibrated 5%
decision. Do not silently patch bayesplot, relabel its method, change influ2's
plots, or contact upstream on the maintainer's behalf in this increment.

## Deliverables

Reproducible developer scripts; all replicate-level outcomes and fit failures;
a compact installed results artefact; a numbered, captioned validation article
with existing four-panel and PIT displays; deterministic tests of the study's
metrics, aggregation, seeds, and artefact structure; and a short interpretation
and decision list for review with Nicholas. Full simulation runs do not become
part of CI or CRAN vignette builds.
