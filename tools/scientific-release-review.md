# Scientific interpretation review

17 October 2026. Package source reviewed: `180889ac7ebab092aa5d5c47019b8d319c337078`.

The residual, implied-effect, and prediction-domain diagnostics are defensible
as **exploratory checks with explicit predictive targets**. The saved studies
support that use, not universally calibrated tests of model adequacy. No new
calculation defect was demonstrated by this review. The remaining scientific
release decision is whether Darcy and Nicholas accept the interpretations and
limits below; Nicholas's agreement is not yet recorded.

## What the validation establishes

The [N09 protocol](n09/protocol.md) covers 100 independently generated NB2
datasets per backend, 500 eligible glmmTMB/sdmTMB fits, and 1,960 diagnostic
records. It evaluates the original finite-simulation PIT engine. The
[IV01 protocol](implied-validation/protocol.md) separately covers 400 NB2
glmmTMB fits and 800 implied-effect calculations. Both retain every attempted
fit and their pre-specified eligibility rules. Successful eligibility is not
proof of perfect optimisation or an adequate model.

The read-only [numerical review](n09/review-results.R) reconstructs the frozen
summaries, paired comparisons, and Monte Carlo intervals. The frozen-result
and worked-example tests pass **869 assertions**, with no failures, errors,
warnings, or skips. Both artefact checksums and N09's shared rank, conditioning,
and simulation source hashes agree with their recorded baselines.

Numerical agreement tests for newer backends, families, native densities, and
DHARMa calculations check the implemented arithmetic. They do not extend
these studies' false-positive or power results to those settings. In
particular, N09 does not calibrate `check_cpue()` or DHARMa p-values, and IV01
does not calibrate binomial, Gamma, Tweedie, spatial, Bayesian, or combined
two-part implied-effect intervals.

## Residual checks and model reports

Three observations from N09 determine the recommended interpretation:

- Omitting the covariate produces strong residual association with that
  covariate, while pooled fitted-effect PIT ECDFs cross the independent-uniform
  reference in only 0/100 glmmTMB and 1/100 sdmTMB datasets. A reassuring pooled
  distribution can therefore miss important structure.
- The correctly specified spatial model crosses that reference in 6/100
  datasets with fitted effects and 28/100 with new effects. Those are crossing
  rates for this design and predictive target, not calibrated model-rejection
  probabilities or evidence that new-effect simulation is implemented wrongly.
- The reference-grid correction reproduces 4.77% crossings in an independent
  uniform control. Correcting the display does not calibrate fitted-model
  residuals, remove dependence, or validate a spatial covariance model.

Current conditioning is intentional and backend-specific:

| Backend | Default | Explicit alternatives |
| --- | --- | --- |
| GLM and GAM | Responses at fitted parameters and smooths | `fitted` only |
| glmmTMB and lme4 | `new_effects` at fitted distribution parameters | `fitted` |
| sdmTMB | `fitted` | `conditional_draw`, `new_effects` |
| tinyVAST | `fitted` | `conditional_draw` |
| brms | `posterior_predictive` using the retained posterior | No fixed-parameter substitution |

Keep these defaults for this release. Choose the conditioning from the
scientific question, not the appearance of the Q-Q plot. For departures within
observed vessels or fields, `fitted` is a useful explicit starting target, but
fitting can absorb departures. `conditional_draw` uses one shared approximate
latent vector, not a fresh conditional vector per response simulation or full
posterior integration. sdmTMB's [native simulation guidance](https://sdmtmb.github.io/sdmTMB/reference/simulate.sdmTMB.html)
suggests this alternative for goodness-of-fit work; it does not make all
conditioning options interchangeable. brms posterior predictive ranks reuse
the fitting observations and are not LOO-PIT.

The first three default panels display normal scores from the same PIT values,
using `qnorm()` with documented finite display limits for exact DHARMa zero/one
endpoints. The stored uniform values are unchanged. The fourth checks the response distribution or Bernoulli probability
calibration. A Q-Q ribbon is a pointwise independent-uniform reference;
response ECDF and calibration ranges are pointwise predictive envelopes.
The optional PIT ECDF limits are simultaneous **iid-uniform** references.
None is an automatic whole-model acceptance threshold after fitting.

`check_cpue()` may flag numerical fitting problems and exploratory residual
screens, but supplies no overall model-pass certificate. Term-screen p-values
are not coefficient tests or instructions to drop terms. Holm adjustments
address the recorded multiple-screen family, not residual dependence,
posterior-data reuse, or calibration after fitting. The correlation concern
in [DHARMa issue 301](https://github.com/florianhartig/DHARMa/issues/301)
remains relevant even when DHARMa's numerical results are reproduced exactly.
Inspect scientifically relevant covariates and within-year spatial patterns
alongside the pooled overview.

## Implied effects

The default implied effect is a local conditional likelihood adjustment,
holding the original coefficients, latent effects, smooths, dispersion, and
baseline fixed. It is not a PIT score added to a coefficient, a refitted
interaction, or a regional index. Gaussian models fitted to `log(response)`,
with common residual variance, agree with the ordinary mean log-residual
construction; historical standardised
residual conventions need not agree.

Bars use a **nominal conditional profile-likelihood level**, not full fitted-model
confidence coverage. They omit original-fit uncertainty and residual
dependence. IV01 known-parameter coverage is 94.4--95.0%; in null fitted-model
cases, at least one supported cell excludes zero in 9--12% of datasets.
The fixed known-parameter target, data-dependent fitted-conditional target,
per-cell denominator, and any-cell denominator must remain distinct.

brms implied effects hold posterior-mean parameters, or one coherent selected
draw, fixed. Their profile bars are not Bayesian credible intervals. Encounter,
positive, and combined targets remain explicit. Combined curves average
adjusted expected responses over the original stratum observations, retaining
sampling composition and exposure; they are not standardised CPUE series.
Zero-inflated gates are not observed encounter probabilities, and count-process
zeros are not silently assigned to an extra-zero component. Empty, sparse,
boundary, and unidentified results remain flagged rather than repaired with
pseudocounts.

## Prediction domains and omitted terms

The new GAM, sdmTMB, and tinyVAST diagnostics describe prediction sensitivity
on a declared reference domain. Cell-ID and distance rules count eligible
fitted observations; a supplied logical matrix instead declares support.
Neither proves representativeness or measures sampling probability. An
unsampled cell may borrow information through spatial or temporal covariance;
it is not necessarily informed only by priors.

Restricted means renormalise their weights; restricted totals do not. A
changing `cell_time` domain mixes response change with domain change. For
seasonal-mean area totals, averaging weights should sum to one within each
spatial cell. The calculation does not normalise them internally; effective
weighted area need not be unique geographic area.

Link contributions add within each native component, not after separate
back-transformation. Their spatial quantiles describe heterogeneity, not
parameter uncertainty. Omitting a field or term changes predictions without
refitting; its interpretation depends on the fitted decomposition and its
centring constraints. It is not a causal effect or a unique allocation of
importance. Paired joint Gaussian draws preserve the calculated dependence
between scenarios, but their intervals do not include new response variation,
unmodelled structure, or selection uncertainty. They do not establish universal
frequentist coverage or bias-corrected abundance.

## Decisions for Darcy and Nicholas

| Decision | Recommended first-release position |
| --- | --- |
| Residuals and `check_cpue()` | Retain explicit conditioning and exploratory flags; make no universal calibration or automatic adequacy claim. Supplement pooled plots with predictor and spatial checks. |
| Implied effects | Retain conditional localisation, component distinctions, and sparse/boundary gaps. Do not interpret bars as full interaction or posterior uncertainty. |
| Domain sensitivity | Retain declared support rules, paired scenario comparisons, and non-causal, non-refitted interpretations. Require explicit reference and area-weight conventions. |

If these interpretations are accepted, no additional Monte Carlo study is
required merely to release the documented exploratory functionality. Stronger
claims about calibrated tests, latent-process validation, or prediction to new
locations would require a separately designed study. That is a scientific
scope recommendation, not CRAN acceptance or Nicholas's sign-off.

## Reproduce this review

From the repository root:

```sh
Rscript --vanilla tools/n09/review-results.R
Rscript --vanilla -e 'testthat::test_local(".", filter = "^(n09-validation|implied-validation|n09-worked-examples)$", reporter = "summary", stop_on_failure = TRUE)'
```

N09 MD5: `e47251546d8efa06d977a53b749b7071`.
IV01 MD5: `60a5ecb751c8c54e102af1f60daccf79`.
These commands read frozen studies; they do not rerun their model fits or
response simulations. The [original interpretation review](n09/interpretation-review.md)
retains the detailed study results. Private correspondence and attachments
remain ignored, and are not reproduced here.
