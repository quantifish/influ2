# Residual-conditioning increment: 11 September 2026

Source: `02573236c77ec700a42d9d99da11640f3ade3a04` (PR #26), merged as
`d230d6861e40996c2a53419bedcd7c07833f7b54`, with an identical source tree.
This records numerical and implementation validation, not proof of universal
fitted-model calibration, approval to change defaults, or CRAN submission.

## Supported targets

| Backend | Default | Explicit alternatives |
| --- | --- | --- |
| GLM / GAM | fitted parameters and smooths | none beyond explicitly naming `fitted` |
| glmmTMB | `new_effects` | `fitted` |
| sdmTMB | `fitted` | `conditional_draw`, `new_effects` |
| tinyVAST | `fitted` | `conditional_draw` |
| brms | `posterior_predictive` | none beyond explicitly naming the default |

Every native result records the requested and resolved conditioning, plus its
human-readable simulation scheme. Predictive means use the same response
simulations. Sampled-field probability bins use expected probabilities at the
same latent vector; new-effect bins deliberately retain fitted conditional
probabilities and are labelled separately from their predictive envelopes.

`conditional_draw` fixes fitted non-latent parameters and obtains one joint
native TMB Gaussian conditional latent draw. It is not a full joint parameter
posterior, MCMC, or one independent draw per observation. The vector is reused
across every response simulation and batch. Native tinyVAST `mle-mvn` instead
draws separately for each response replicate; influ2 deliberately does not
use that repeated-draw interpretation for this option.

## Implementation boundaries and safeguards

- New routes construct independent TMB objectives from fitted parameters,
  native data, and maps. They never set simulation codes on the supplied fit.
- Native parameter order and latent membership must agree before simulation.
  Shared conditional draws require a converged, positive-Hessian, unprofiled
  ML fit. REML is rejected because native latent vectors can contain fixed
  coefficients that this option promises to hold fixed.
- Native sparse conditional factorisation is prepared once. Storage includes
  a native objective, one parameter vector, response seeds, and response
  batches; sparse setup can still be substantial. No latent vector, model,
  or observation-by-simulation matrix is retained in the diagnostic result.
- New spatial schemes and glmmTMB `fitted` prepare one seed per replicate.
  Ranks, predictive means, and calibration summaries are invariant to batch
  size. The compact response-ECDF grid still depends on the first batch.
  Normal default-route RNG behaviour remains unchanged.
- glmmTMB `fitted` fixes conditional, zero-inflation, and dispersion random
  effects through native controls. If another package has altered simulation
  codes, `new_effects` uses an independent objective to honour its target
  without resetting the original fit. Native codes are verified against
  glmmTMB's implementation and replayed through its public simulation method.
- sdmTMB `new_effects` mirrors native `re_form = NA`: its process flags are
  enabled, but fitted smooths are held fixed. It does not mean every possible
  native random-effect representation is regenerated without exception.
- Combined, encounter, and positive sdmTMB delta routes share conditioning.
  Positive responses are selected by original positive-observation rows,
  never by filtering each simulated combined vector. Poisson-link delta
  encounter probabilities use `1 - exp(-intensity)`, not the intensity itself.
- Unsupported backend/target combinations fail explicitly. There is no
  term-by-term conditioning selector, unconditional tinyVAST process bridge,
  GAM coefficient-uncertainty sampler, or new-group brms prediction here.

## Evidence

The complete local suite passed **4,525 expectations**, with no failures,
warnings, or skips. Tests cover native glmmTMB replay; native joint MC and
response replay for sdmTMB and tinyVAST; spatial and spatiotemporal effects;
binomial trials; standard and Poisson-link delta components; probability
alignment; object/RNG preservation; altered simulation controls; batching;
and malformed native interfaces. brms draw-selection/routing contracts and
all frozen pre-refactor engine baselines continue to pass. No new MCMC was run.

PR #26 passed Ubuntu release and Windows release in run 34543811601,
coverage in run 34543811615, and the website build in run 34543811639.
The GitHub website build passed all 74 figure-caption checks, including its
optional-dependency examples. No additional CI platforms were added.

Local and GitHub coverage both reached **95.98%**; the new conditioning file
has **95.92%** line coverage. Local native versions were sdmTMB 1.1.0,
tinyVAST 1.6.2, glmmTMB 1.1.14, TMB 1.9.25, brms 2.23.0, and mgcv 1.9.4.

Three new spatial figures were rendered and visually inspected. All 71
plotted-image/lightbox checks on seven local articles passed. Examples reuse
the existing Pacific cod and simulated tinyVAST fits; they do not refit models
to manufacture more favourable residuals or silently select a better seed.

The final macOS R 4.6.1 archive passed `--as-cran --no-manual`: zero errors,
zero warnings, and the existing incoming-feasibility NOTE for a new submission
and tinyVAST's additional repository. All seven vignettes rebuilt. CRAN-mode
tests passed 4,511 expectations with six intentionally skipped visual-test
groups, which passed in the full suite. Optional DHARMa was absent locally;
its optional vignette branches and the PDF manual were not checked locally.

Archive: `/private/tmp/influ2-conditioning-check.4hllQr/influ2_1.1.0.tar.gz`.
SHA256: `8458bee96eecb42751b9b70e75ab1f9251bdcddbc154dc640829e4f7b06ddbba`.
The checked archive is installed in the usual Mac R library. Live publication
and the final cross-platform status are recorded in `release-review.md`.

## Scientific review still needed

N09 remains separate: assess calibration and sensitivity under estimated
parameters, approximate latent distributions, spatial dependence, and
posterior predictive reuse. The same plot layout or conditioning label does
not establish exchangeability, iid-uniform ranks, equivalent power, or a
calibrated test across models. Conditional residual plots do not replace
field-covariance checks, residual maps, or out-of-sample validation. Review
these comparisons before proposing any default change.

## Primary references

- [sdmTMB residual construction](https://sdmtmb.github.io/sdmTMB/reference/residuals.sdmTMB.html)
- [sdmTMB simulation options](https://sdmtmb.github.io/sdmTMB/reference/simulate.sdmTMB.html)
- [tinyVAST native simulation interface](https://vast-lib.github.io/tinyVAST/reference/simulate.tinyVAST.html)
- [glmmTMB native simulation controls](https://github.com/glmmTMB/glmmTMB/blob/master/glmmTMB/R/utils.R)
- [glmmTMB simulation-code definitions](https://github.com/glmmTMB/glmmTMB/blob/master/glmmTMB/src/glmmTMB.cpp)
- [Waagepetersen (2006)](https://doi.org/10.1111/j.1467-9469.2006.00504.x)
