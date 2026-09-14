# Residual-validation review for Darcy and Nicholas

14 September 2026. Review baseline: `52d6c6e4d56ff2971c2e4c7a0f8aba6abebf641d`.

## Follow-up: 15 September 2026

The maintainer accepted the bounded practical follow-up recommended below:
retain defaults, clarify interpretation, and demonstrate omitted covariate
and yearly spatial patterns using the existing saved N09 objects. The
[Residual diagnostics article](../../vignettes/residual-diagnostics.Rmd#what-pooled-checks-can-miss)
now compares Q-Q and covariate plots and maps all six spatial years, with
common scales and guarded row matching. The examples use the protocol-selected
first eligible replicate; they are not additional calibration experiments.
New tests and the full local regression suite pass, both frozen artefacts and
the core calculation hashes are unchanged, and no study fitting or simulation
was rerun. Nicholas's scientific review is still pending. Broader calibration,
held-out checks, and a general spatial-diagnostic API remain separate proposals.

The original review below is retained as the dated recommendation and evidence
record; its references to unaccepted recommendations describe 14 September.

## Recommendation

Keep the four-panel overview and current conditioning defaults for now. Present
them as complementary exploratory checks, not a certificate that a model is
adequate. Before release, agree a short user-facing interpretation guide and
make clear that spatial models need a separate investigation of spatial and
temporal residual patterns.

This review found no new calculation defect in the saved-result audits or the
rank/conditioning/simulation paths checked. It does not establish universal
calibration. The evidence does not justify choosing a different default merely
because it produces a more normal-looking Q-Q plot.

These are recommendations for discussion, not decisions already accepted by
Darcy or Nicholas. No defaults, runtime code, fitted models, or frozen results
were changed. No new model fits or response simulations were run.

## 1. What the saved experiments actually show

N09 contains 100 independent NB2 datasets per backend, 500 eligible glmmTMB
and sdmTMB fits, and 1,960 diagnostic/control records. IV01 separately contains
400 NB2 glmmTMB fits and 800 implied-effect calculations. Every recorded attempt
succeeded under its pre-specified eligibility rules. This is not validation of
all six supported backends, all families, or all sampling designs.

The following N09 counts use the **independent-uniform DKW reference**, not the
old misaligned bayesplot display. A crossing means the pooled PIT ECDF exceeded
that reference somewhere; it is not a calibrated fitted-model rejection.

| Backend and candidate | Conditioning | Crossings / 100 datasets | Mean correlation with omitted covariate | Mean spatial neighbour score |
| --- | --- | ---: | ---: | ---: |
| glmmTMB: full model | Fitted effects | 0 | -0.001 | Not evaluated |
| glmmTMB: omit covariate | Fitted effects | 0 | 0.613 | Not evaluated |
| sdmTMB: full model | Fitted effects | 6 | -0.002 | -0.105 |
| sdmTMB: omit covariate | Fitted effects | 1 | 0.607 | -0.095 |
| sdmTMB: omit yearly field | Fitted effects | 0 | -0.004 | 0.190 |

For full-model rows, the correlation uses the same covariate, which is included
there. The spatial score is a year-centred, symmetric four-nearest-neighbour
summary, not a calibrated autocorrelation test. Its absolute value is not
comparable across arbitrary graphs or conditioning schemes.

The practical lesson is strong: **a plausible pooled distribution can coexist
with important residual structure**. The omitted-covariate associations increase
by 0.614 (glmmTMB) and 0.609 (sdmTMB) relative to the paired full fits; their
Monte Carlo standard errors are 0.0022 and 0.0027. For fitted-effect sdmTMB,
omitting the yearly field raises the neighbour score by 0.296 (MCSE 0.0054).
These are exploratory paired contrasts calculated for this review, not new
pre-specified tests. Replicate IDs preserve pairing; the 100 datasets, not their
48,000 observations, are the independent units.

Do not interpret 0/100 as proof of a zero crossing probability: its Wilson 95%
Monte Carlo interval is 0--3.7%. For 6/100, the interval is 2.8--12.5%.
Nor is a lower crossing rate a model-ranking criterion.
These counts do not measure how often a human reviewer would detect a problem
from all four panels; that was not an outcome of this study.

### Reference calculation versus fitted-model calibration

The frozen known-truth controls have 4/100 analytic and 2/100 finite-simulation
crossings for glmmTMB, and 4/100 for both sdmTMB routes; mean normal-score SDs
are 0.999--1.002. These are reassuring checks of the rank calculation under the
known conditional distribution, not proof about residuals after estimation.

The separate, already completed independent-uniform audit recorded 4.77%
crossings after aligning the bayesplot reference grid, versus 12.46% before.
That correction is in the current plotting code. This review reads the saved
audit; it does not rerun or relabel the original `displayed_band_crossing`
metrics. Correcting that display did not make its limits calibrated for fitted
mixed or spatial models.

## 2. Choose the question before choosing the conditioning

| Option | Scientific question it addresses | Interpretation limit |
| --- | --- | --- |
| `fitted` | What response departures remain conditional on the estimated covariate, vessel, and spatial structure? | The same observations estimated that structure; departures can be absorbed into it. Random effects and other parameter uncertainty are not resimulated. |
| `conditional_draw` | How does the check behave after replacing fitted latent effects with one joint draw from their approximate conditional distribution? | One vector is shared across every response replicate. This is not repeated latent averaging or full Bayesian uncertainty; changing the seed changes that vector. |
| `new_effects` | Is the observed response compatible with replication that generates new latent effects at fitted distribution parameters? | The observed data retain their realised vessel/field pattern. The resulting PIT scores need not be independent; their spatial structure does not by itself diagnose a missing field. |
| `posterior_predictive` | What responses are predicted by the joint posterior of the fitted brms model? | This reuses fitting data and is not LOO-PIT. N09 did not validate brms operating characteristics. |

Current defaults remain `fitted` for GLM, GAM, sdmTMB, and tinyVAST;
`new_effects` for glmmTMB; and `posterior_predictive` for brms. Supported options
are backend-specific: tinyVAST does not currently expose `new_effects` here,
and glmmTMB does not expose `conditional_draw`. Spatial conditional draws
require supported converged ML fits, not REML. Selecting the same name does
not guarantee equal calibration across backends.

For full sdmTMB fits, mean residual SD is 0.906 under fitted effects and 1.004
under a conditional draw, but both have 6/100 DKW crossings. New-effect checks
have 28/100 crossings (95% Monte Carlo interval 20.1--37.5%) and a positive
neighbour score of 0.294 despite the correctly specified generating structure.
After omitting the yearly field that score is 0.296: almost unchanged. In
contrast, the conditional-draw score changes from -0.006 to 0.199. This supports
interpreting the options separately, not ranking them by apparent normality
or assuming that positive spatial association has one meaning under every option.

For an investigation of within-observed-vessel or within-observed-field
departures, I recommend explicitly selecting `fitted` as a starting question,
then using supported alternatives as sensitivity checks. This is a proposed
workflow, not a proposed universal default or a validation of latent covariance.
The native [sdmTMB documentation](https://sdmtmb.github.io/sdmTMB/reference/simulate.sdmTMB.html)
also suggests an approximate latent draw for goodness-of-fit work. That is a
reason to compare it seriously, not evidence that this one experiment settles
the choice. Its single- versus multiple-draw options must not be conflated.

## 3. How to read the four panels

| Panel | Useful question | What a reassuring plot cannot establish |
| --- | --- | --- |
| Normal-score PIT Q-Q | Are the pooled residual shape, spread, and tails compatible with the displayed reference? | No omitted covariate, independent residuals, or correct spatial structure. Its ribbon is pointwise, not a whole-plot 5% test. |
| PIT residuals versus predictive mean | Do departures change with the predictive mean under the selected conditioning? | No association with another predictor. In the omitted-covariate fitted glmmTMB case, mean fitted-value correlation is only 0.012 despite covariate correlation 0.613. |
| PIT residuals by year | Do residual centres, spreads, or tails change through time? | An unbiased CPUE index, or no within-year structure. Fitted year effects can absorb annual shifts. |
| Observed versus simulated response ECDF | Does the observed response distribution resemble replicated distributions? | Observation-level calibration or absence of covariate/spatial structure. This is not an ECDF of PIT residuals. Bernoulli overviews instead use probability calibration, outside this NB2 study. |

Switching to `pit_ecdf` or `pit_ecdf_diff` changes the view of the same ranks;
it does not add a spatial or predictor check. Keep the default overview, and
supplement it with ecologically relevant covariate/group checks and mapped or
within-year spatial checks when the model and sampling design require them.
Do not introduce automatic pass/fail labels or an aggregate diagnostic score.

The native [bayesplot documentation](https://mc-stan.org/bayesplot/reference/PPC-distributions.html)
distinguishes independent-reference limits from dependence-aware LOO-PIT
methods. Those alternatives are not a drop-in calibration fix for our current
fitted-data ranks. [Dunn and Smyth (1996)](https://gksmyth.github.io/pubs/residual.html)
provide the normal-score residual rationale; their estimated-parameter caveat
still matters after applying `qnorm()`.

## 4. What IV01 adds, and what it does not

The implied-effect plots answer a different question from grouped PIT plots:
how much local effect-scale adjustment improves the conditional likelihood
within a year/group cell, with the rest of the model held fixed.

Known-parameter pointwise coverage averages 94.4--95.0% across the four IV01
scenarios. Under null models, the fitted-model conditional intervals exclude
zero in about 0.8--0.9% of supported cells, but at least one interval excludes
zero in 9--12% of datasets. Those are different denominators, not interchangeable
false-positive rates. Fitted-target containment is not coverage for a fixed
population interaction, and the plotted baseline has uncertainty these bars omit.

For strongly injected trend cells, the fitted-model adjustment has the correct
direction in 100% of balanced and 98.3% of uneven supported cells, averaged
within datasets. Zero exclusion is 92.5% versus 32.8%, respectively. The uneven
design loses unsupported cells and allows more signal to be absorbed into
estimated main effects; these rates do not isolate a sample-size-only effect.
The fitted-conditional target differs from the raw injected signal by RMSE
0.107 in the balanced trend case and 0.317 in the uneven case.

Recommendation: retain these plots as localisation tools, retain sparse/empty
gaps, and avoid calling their pointwise intervals general interaction tests.
Do not generalise this NB2 calibration study to Gamma: the Gamma adapter has
separate numerical verification, not a coverage study. Combined delta responses
and positive-component responses also remain distinct targets.

## 5. Decisions before release, and work that can wait

My proposed decisions for Darcy and Nicholas are:

1. **Approve the exploratory interpretation and unchanged defaults.** Record
   that a common plotting interface does not imply identical predictive targets.
2. **Approve a short practical addition to the residual article.** Bring the
   conditioning guide and four-panel limitations above into the everyday workflow,
   linked to the existing validation article. Much of the caution is already in
   the help; the gap is making the practical response obvious.
3. **Choose how far the first-release spatial workflow should go.** At minimum,
   tell users to inspect residual structure outside the pooled four panels.
   A worked residual-versus-covariate and year-specific spatial example using the
   existing saved N09 objects would be a bounded next implementation, with no
   refitting. A general map/autocorrelation API is a separate proposal (G02).

There is no newly demonstrated calculation bug requiring an immediate patch.
Broader calibration, refit-based reference envelopes, held-out spatial/year
prediction checks, and additional families should be explicitly scoped if taken
up. They need not be prerequisites for an honestly labelled exploratory release;
they would be prerequisites for stronger claims of calibrated tests. This is
a scientific-scope recommendation, not CRAN approval or maintainer sign-off.

If a further simulation study is commissioned, first agree its target: fitted
response checks, latent-process adequacy, or prediction to new years/locations.
Then vary one relevant design limitation (for example, uneven spatial sampling),
retain null and omitted-structure controls, and report Monte Carlo precision.
Do not simply increase replicates or simulation counts without that decision.

N09's 10-dataset sensitivity subset is too small for a universal simulation-count
recommendation. Three of 90 spatial comparisons changed crossing status with a
second seed, and two with more simulations; these are paired comparisons, not
90 independent datasets. Extra response simulations do not average over the
one shared conditional latent draw. The largest recorded glmmTMB gradient was
0.0105; all original eligibility decisions are retained, and eligibility is not
proof of perfect optimisation. No post-hoc exclusions were made in this review.

## Evidence and reproducibility

- [Read-only review script](review-results.R): recomputes rates, intervals,
  summary means, paired contrasts, and sensitivity counts from replicate records.
  Existing `test-n09-validation.R` and `test-implied-validation.R` also reconstruct
  their summaries, including IV01 cell-to-dataset calculations.
- [N09 protocol](protocol.md), [IV01 protocol](../implied-validation/protocol.md),
  [Residual validation article](../../vignettes/residual-validation.Rmd), and
  [Residual-implied effects article](../../vignettes/implied-effects.Rmd).
- N09 artefact MD5: `e47251546d8efa06d977a53b749b7071`;
  IV01: `60a5ecb751c8c54e102af1f60daccf79`. Both are unchanged.
- N09's recorded runtime baseline is `bd634e221b0a48baaff02290c86f337ee8a14bb2`;
  IV01's is `2b174484de933fe9ceead82dd9319bc092703a38`. Study sources have their own
  recorded hashes. The current shared rank engine, conditioning implementation,
  and simulation implementation match the N09 hashes; later PIT plotting and
  documentation changes are separate. This is a review of frozen results, not
  a rerun under current dependency versions.

From the repository root:

```sh
Rscript --vanilla tools/n09/review-results.R
Rscript --vanilla -e 'testthat::test_local(".", filter = "^(n09-validation|implied-validation)$", reporter = "summary", stop_on_failure = TRUE)'
```

The numerical companion writes only to standard output. These review files are
developer material under `tools/`, excluded from the CRAN source archive. The
private PDF and correspondence remain untouched and are not redistributed.
The companion and both frozen-result test files passed on 14 September. This
review-only change does not require a new source archive or a website build;
those remain necessary after the final agreed package changes.
