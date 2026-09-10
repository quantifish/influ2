# Development ideas and decisions

Last reviewed: 11 September 2026.

This is the ongoing consideration list for Nicholas Ducharme-Barth's residual
proposal and ideas identified in generalised_influ / Ginflu. Inclusion records
an idea, not approval to implement it, a completed feature, or an additional
requirement for the first CRAN release. Nicholas's residual work is the current
discussion priority; Ginflu-inspired extensions are candidates for later review.

Use [release-review.md](release-review.md) for the separate release checklist,
existing issue decisions, legacy triage, and final-check evidence. This list
does not reopen completed issues or change the parked status of issue #18.

## Status key

- **Discuss next:** prioritised for discussion with Nicholas; design and scope
  still need agreement before implementation.
- **Candidate:** potentially useful, with no implementation commitment.
- **Parked:** explicitly deferred by the maintainer.
- **Addressed:** the concern is already covered; retain the note to avoid
  accidentally restoring an obsolete interface.
- **Completed:** the agreed implementation, tests, and documentation are in place.

## Nicholas's residual-diagnostic proposal

The source is his four-page *influ2 residual diagnostics: proposed work* PDF,
received on 11 September 2026. The original and the detailed combined review
remain local, ignored review material in `data-raw/review/`; do not move or
publish those attachments as part of maintaining this list. The initial review
used influ2 revision `7bb976c`.

| ID | Candidate and status | Next decision or validation needed |
| --- | --- | --- |
| N01 | Preserve a numerical and random-number baseline — **Completed, 11 September** | Frozen results from `6c3d1c9` cover six response/adapter contracts at three batch sizes (18 cases), RNG preparation order, compact size, and full summaries. These isolate the engine; native backend tests and an actual glmmTMB replay validate integration separately. |
| N02 | Retain below-observation and tied-value counters — **Discuss next** | Keep the two small vectors and simulation count to enable re-randomisation and simulated-range checks without saving the full response matrix. Agree field names and older-object handling. |
| N03 | Add a bayesplot PIT plotting bridge — **Completed, 11 September** | `plot(checks, type = "pit_ecdf")` and `"pit_ecdf_diff"` reuse stored PIT values. `panels` selects and orders any four supported panels. Defaults and response checks are unchanged. Optional bayesplot supplies explicitly labelled simultaneous iid-uniform reference limits, not calibrated fitted-model tests. |
| N04 | Extract a shared simulation-rank engine — **Completed, 11 September** | Native adapters and external matrices use the same calculation. Native preparation/RNG order, tie handling, sequential sums, first-batch ECDF grid, conditioning defaults, and compact retention remain unchanged against N01. |
| N05 | Accept externally generated response simulations — **Completed, 11 September** | `as_influ_residuals()` accepts a finite observation-by-simulation matrix with exact row IDs, aligned data, explicit response/year/kind/conditioning, and optional components/groups. Predictive means come from those same simulations; binomial calibration requires original fitted probabilities and known trial counts. No new simulations, automatic realignment, retained matrices, or streaming API. |
| N06 | Make spatial and mixed-effect conditioning explicit — **Discuss next** | Validate an sdmTMB scheme holding one sampled set of latent effects across all response-simulation batches. Inspect tinyVAST semantics separately; a common `mle-mvn` argument does not establish equivalent draws. Keep means consistent with simulations, and document glmmTMB random-effect conditioning choices. Do not change defaults without agreement. |
| N07 | Optional response-simulation retention — **Candidate** | Agree summary-only, in-memory, and potentially on-disk modes, draw/row subsetting, and memory warnings. Disk storage does not remove the cost of materialising a full matrix for another package. Keep compact retention as the default. |
| N08 | DHARMa and response-simulation bayesplot bridges — **Candidate** | Reuse explicit retained or supplied simulations, with response/component metadata. Let `DHARMa::createDHARMa()` calculate its own residuals rather than replacing them with influ2 ranks. Test supported dependency interfaces, make dependencies optional, and warn before large exports. Coordinate with G02. |
| N09 | Clarify finite-simulation calibration and diagnostic targets — **Discuss next** | Explain that randomised ranks are exactly uniform under exchangeability of the observation and simulations, not automatically for every fitted model. Distinguish analytic PIT, fitted-data simulation ranks, posterior predictive checks, and LOO-PIT. Simultaneous bands alone do not correct fitting effects or spatial dependence. |
| N10 | Competing native-residual Q-Q entry point — **Addressed** | `plot_qq()` was retired on 10 September. Use the generalised residual object and `plot(checks, type = "qq")`; do not restore the retired helper merely because the PDF refers to it. |

Next suggested discussion: N06 conditioning, applying N09 throughout; then
consider N07 retention and N08 remaining bridges. N02 counters were passed
over and remain unimplemented. This is a proposed order, not authorisation to
start further changes.

Implementation review, 11 September: the maintainer approved N03 and arbitrary
four-panel selection, retaining `c("qq", "fitted", "year", "auto")` as the
default. The footer, axes, help, and article identify normal-score PIT residuals
and distinguish the response checks. The bridge does not change residuals,
simulation schemes, RNG state, or global plotting settings. All 508 residual
expectations and 4,037 full-suite expectations passed, with zero failures,
warnings, or skips. New numerical and visual tests cover the native bayesplot
curves, reference limits, panel selection, and optional-dependency failures.
The article has executed examples. Simulation-counter retention (N02) was
passed over, not implemented or removed from the candidate list. N09 remains
a broader documentation/validation consideration; simultaneous bands alone
do not address fitting effects or spatial dependence.

Shared-engine/external-input review, 11 September: the maintainer approved
N01, N04, and N05 as the next increment. The 18 frozen results matched exactly
on the Mac after extraction; portable regression tests allow only a `1e-12`
floating-point tolerance. The external-input tests independently reconstruct
ranks, means, response ECDF intervals, and binomial calibration. They cover
alignment/support failures, combined versus positive components, RNG restoration,
batch invariance of ranks/means, and non-retention. A recorded actual glmmTMB
simulation sequence produces identical diagnostic tables via both routes.
All 4,378 full-suite expectations passed locally, with zero failures, warnings,
or skips. The residual article has an executed lobster glmmTMB example and a
separate encounter-calibration recipe. This does not change model-specific
conditioning defaults or establish universal fitted-model PIT calibration.
Archive checks, CI, and publication are recorded separately in the release review.

## Ideas to consider from generalised_influ / Ginflu

These candidates come from the 11 September source review of
[generalised_influ at c7d9b747](https://github.com/kahawai-collective/generalised_influ/tree/c7d9b747f3ad4f5be7fa8c22c296e8d6837f08c0).
They describe potentially useful ideas, not a claim that the reviewed code has
passed installation or numerical validation. Recheck the relevant upstream code
when a candidate is taken up; coordinate with Philipp and Nicholas where useful.

| ID | Candidate and status | Next decision or validation needed |
| --- | --- | --- |
| G01 | Previous-versus-current assessment comparisons — **Candidate** | Show new vessels, newly added years, coefficient changes, and influence/CDI changes. Build on existing compact comparison objects where possible. Agree matching terms, common reference periods, and separation of data updates from model changes. |
| G02 | Spatial residual aggregation, maps, and autocorrelation checks — **Candidate** | Aggregate observations and response simulations into the same cells, then recalculate residuals; do not simply average residual scores. Generalise projection, units, cell size, temporal window, and autocorrelation assumptions. Depends on an explicit simulation/retention route (N05/N07/N08). |
| G03 | Stock-assessment reporting displays — **Candidate** | Consider combining CPUE series, reference periods, removals, and relative exploitation indicators. Decide whether this belongs in influ2 or a separate reporting layer. Make scaling and interpretation explicit; relative CPUE is not automatically biomass. |
| G04 | Between-assessment index-change and RIC-style displays — **Candidate** | Review whether these answer a useful question beyond current `plot_compare()` and grouped residual displays. Define the estimand first: log-residual-plus-log-index arithmetic is not equivalent to our generalised normal-score grouped departures. Do not silently substitute one for the other. |
| G05 | Additional model coverage, including `survreg` — **Candidate** | Assess actual user need before expanding beyond the six-backend scope. A censored-model adapter would require explicit censoring, prediction, and residual contracts, not just matching a class name. |

Borrowing an idea is not a decision to transplant its implementation. Preserve
influ2's model-neutral objects and low-memory design. In particular, keep
standardised response indices distinct from year-effect contrasts and
area-integrated totals; combine joint delta responses before aggregation rather
than multiplying separately aggregated components. Verify any adopted code's
statistical assumptions, tests, dependency requirements, and licence obligations.

## Related completed and parked decisions

| Item | Status and existing record |
| --- | --- |
| Normal-score PIT transformation | **Completed / already available.** Q-Q, fitted-value, and year panels use `qnorm(pit)` by default; this is no longer outstanding work. N03 adds optional uniform-scale PIT ECDF views of those same ranks. It does not add a scale switch to the existing normal-score panels. |
| Issue #18: two regional CPUE series | **Parked.** Preserve the [two-region plan](two-region-plan.md); do not begin its example or API work yet. |
| Frequentist `Median = NA`, remaining legacy helpers, and frozen Get Started article | **Parked for later review.** Preserve the current records and article until the maintainer makes the remaining decisions. See [release-review.md](release-review.md). |

## Keeping the list current

At each relevant development/review round, update the affected stable IDs with
the decision, date, and any agreed issue or implementation link. A candidate
should become agreed work only after scope is confirmed, and completed only
after its tests, interpretation, and documentation have been checked. Keep
rejected or superseded entries with a brief reason rather than silently losing
the discussion. Reconcile this list with the release checklist before deciding
whether an item is release-blocking or a later addition. No periodic monitoring,
automatic upstream synchronisation, or new GitHub issue is established by this
document.
