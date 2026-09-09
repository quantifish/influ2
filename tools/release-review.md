# Review before the first CRAN submission

This is the remaining review plan for influ2 1.1.0. It records decisions to
make before release, rather than evidence that a check has passed. Keep the
dated results for the final source archive in `cran-comments.md`. Submission
is a separate, later step authorised by the maintainer.

## 1. Scientific and visual review

- Review the new four-panel residual display, especially automatic fishing-year
  selection, gaps and sample sizes, finite-simulation rank randomisation, and
  the distinction between nominal Q-Q reference bands and predictive ECDF bands.
  Its default spatial simulations condition on fitted fields; unconditional
  glmmTMB and posterior-predictive brms checks answer different questions.
  GitHub issue review and remaining helper triage are deferred until this
  display has been reviewed. No Shiny development is planned at this stage.
- Review the current Get Started, Bentley validation, hurdle and zero-inflated,
  and spatial and spatiotemporal articles. Check figure numbering, captions,
  lightbox zoom, labels, legends, interval visibility, and CDI panel alignment
  on both normal and narrow browser windows.
- In the Bentley comparison, confirm that the top-panel centred monthly
  point estimates agree and that the caption explains the legacy one-standard-
  error bars versus the new 95% intervals. Check the annual influence parity.
- Review the Bayesian CDI: centring happens within each joint draw, ratios are
  summarised after exponentiation, and the reference month now has an interval.
  Check the optional model-coded and centred-link displays as well.
- Review the reference distribution used in every example. Observed weights
  and prediction-grid weights define different questions; label the chosen
  question consistently for fitted effects, influence, and index comparisons.
- Confirm that logit, probit, and complementary-log-log effects are interpreted
  in their labelled link units. Check the direction of occurrence versus
  extra-zero effects and the distinction from unconditional mean influence.
- Review mapped persistent and spatiotemporal fields alongside their influence
  panels. Decide whether the small demonstration fits are adequate illustrations
  of the methods and state their limits clearly.
- Review the refitted GLM and tinyVAST step plots. These compare centred year
  effects on fixed analysis rows, not area-integrated abundance. Confirm the
  explicit process order, common effort offsets, and per-model interval labels.
- Review the revised lobster sampling shifts and known-truth table. Its
  designed confounding illustrates point-estimate recovery. The main step
  demonstration now uses negative-binomial fits with dispersion re-estimated
  at each stage; approximate model-based bands are not a coverage experiment.

## 2. Decide what to keep from the earlier interface

**First retirement decisions accepted on 7 September 2026.** The maintainer
approved the removals below, and asked to keep the undecided features for
another review round. This partial triage does not freeze the public API or
authorise CRAN submission.

Review the frozen page at `pkgdown/assets/articles/legacy-get-started.html`
against the current package. The remaining source in `tools/legacy/R/` is
preserved for this review. Do not delete the page, its figures, or remaining
source until each relevant feature has a recorded destination or an explicit removal
decision. The original Bentley `proto` implementation has a separate role as
a validation artefact and is not a candidate for the active runtime API.
The maintainer intends to remove the frozen Get Started article eventually,
but explicitly asked to retain it and its figures until that review is complete.

The following functions are already exported and maintained in the current
package. They are not merely historical helpers:

| Current function | Remaining review decision |
| --- | --- |
| `plot_bubble()` | Confirm the purple and coloured sampling displays cover the old usage. |
| `plot_data_extent()` | Confirm the missing-data coverage display and ordering are suitable. |
| `plot_compare()` | Confirm index selection, common-period rescaling, labels, and interval defaults. |
| `plot_step()`, `influ_steps()` | Review automatic ordinary-model refits, explicit spatial-process stages, and reuse of compact results or supplied fits. |
| `get_bayes_R2()` | Retain the brms summary; decide whether a worked example is needed. |
| `table_criterion()` | Retain the brms criteria; review interpretation of LOO, R-squared, and log likelihood. |
| `plot_implied_residuals()` | Review the fisheries interpretation, strata threshold, residual choice, and one-standard-error bars. |
| `plot_predicted_residuals()` | Review residual types and smooths for each intended backend. |
| `plot_qq()` | Retain as normal-quantile screening; review the new residual article's optional DHARMa examples and their limitations. |

### Accepted retirements

These ten names are no longer candidates for restoration. Their frozen
implementations and help files have been removed; they were already outside
the runtime namespace. No compatibility wrappers or new dependencies are
introduced.

| Retired functions | Decision |
| --- | --- |
| `plot_hurdle()` | Remove the old brms-specific plot. Use the supported component and index displays; their estimands are not necessarily identical to the old reference-covariate predictions. Hurdle, delta, and zero-inflated model support remains. |
| `get_coefs()`, `get_coefs_raw()`, `get_marginal()` | Remove: no retained implementation calls them. Current adapters calculate their own effects. The compact coefficient summaries and retained diagnostic draws are not general-purpose raw coefficient or response-curve extractors. |
| `get_influ()`, `get_influ2()`, `plot_influ()` | Consolidate on `influ()`, `influ_effects()`, and `plot(..., type = "influence")`. |
| `plot_bayesian_cdi()`, `plot_bayesian_cdi2()` | Consolidate on `plot(..., type = "cdi")`. |
| `influ_app()` | Remove the old brms-only Shiny launcher. A possible new model-neutral viewer is deferred, not commissioned by this decision. |

The complete pre-triage source remains recoverable at Git commit `cf12bb6`.
The frozen Get Started HTML and every accompanying figure remain unchanged.
The maintained `influ()` generic and Bentley validation implementation are
explicitly retained.

### Still awaiting review

The frozen functions below require specific choices. Keeping these files for
review does not re-export or maintain the old implementations. Related current
workflows are not assumed to reproduce every old argument or output column.

| Frozen function or feature | Proposed destination or decision |
| --- | --- |
| `get_index()`, `plot_index()` | Assessment functionality retained as `cpue_index()` and a calculated-object `plot_index()`, not a compatibility wrapper. The new table includes year, mean, posterior median where defined, SD, CV, intervals, and metadata. Explicit response references differ from the centred contrasts in `influ_indices()`. Spatial response adapters and separate `integrate_index()` totals are included in the first-release implementation. |
| `get_unstandarsied()` (original spelling) | Keep for review of geometric-mean CPUE and positive-mean times occurrence summaries versus the current weighted arithmetic nominal mean. Decide definitions and names, including treatment of zero catches. |
| `rescale_index()` | Check whether `plot_compare(rescale = ..., rescale_series = ...)` is sufficient, or whether users need a public function returning rescaled tables. |
| Earlier `table_criterion()` and `get_bayes_R2()` | Keep both maintained functions and the frozen reporting examples. Review divergence counts, chain runtime, LOO model differences, and a complete-fit brms example. |
| `glm_term_table()` | Keep the internal source for review of deviance/AIC summaries accompanying step plots. The historical one-percent improvement rule is not an accepted model-selection criterion. |
| `geo_mean()` | Explicitly retained by the maintainer on 9 September 2026 and restored as a tested public utility using stable log-scale calculation. |
| `get_first_term()`, `id_var_type()`, and other internal utilities | Keep internal only when needed by a retained feature; do not restore exports simply because they existed previously. |
| PPC bars and ECDF overlays in the frozen article | Decide which examples to restore using the original model and `bayesplot`; these are posterior predictive checks, not replacements for CDI. |

Confirmed 9 September: low-use retired helpers and the Shiny launcher remain
removed. No permanent compatibility layer is required. Active downstream
projects can be migrated deliberately later; LSD, JMA7, and morphology are
historical and are not migration targets. `plot_compare()` keeps its existing
year-effect behaviour and additionally accepts calculated CPUE indices.
`table_criterion()` and `get_bayes_R2()` remain available; no brms-specific
report columns are restored without a separate decision. The frozen legacy
Get Started page and its figures remain intact for the maintainer's review.

Suggested order for the next round: assessment tables and plots; nominal
definitions and table rescaling; Bayesian predictive examples and reporting
extras; then sequential-fit tables and residual internal utilities.

**Deferred review, 10 September 2026:** review the all-`NA` `Median` column in
frequentist assessment tables alongside the remaining legacy material. The
current definition is a posterior median for brms, not a median individual
catch. Decide whether to omit unavailable columns in displayed tables and add
a Bayesian table example. Do not change the table schema or its calculation
as part of the spatial-index and area-integration development.

**First-release scope confirmed, 10 September 2026:** standardised expected-
response CPUE tables must support all six backends, including sdmTMB and
tinyVAST. A separate `integrate_index()` interface for area-weighted totals
across those backends is also required before CRAN submission. These are no
longer proposed later-release additions. Legacy review remains parked.

### Standardised and area-integrated indices: 10 September 2026

Implemented `cpue_index()` expected-response adapters for sdmTMB and
univariate tinyVAST, plus separate `integrate_index()` totals for all six
backends. GLMs/GAMs and mixed models do not need spatial terms to integrate.
The reference domain is fixed across observed years; areas, exposure, known
catchability conversions, and within-cell seasonal averaging are explicit.
Native joint delta responses are combined before aggregation. Field choices
alter predictions, not model fits. No grid-by-draw arrays are retained.

Numerical checks cover joint covariance, native sdmTMB and tinyVAST uncorrected
totals, both ordinary and Poisson-link delta families, reference offsets,
native-time mapping, batch invariance, random-number-state restoration,
unchanged fitted-model state, and area/season/conversion scaling. A complete
existing brms fit agrees with native posterior integration to 3.6e-15; no
new MCMC was required. The final local suite passed 3,109 expectations in
183 test blocks, with no failures, warnings, or skips. The final code archive
(88e14ff) passed macOS R 4.6.1 `R CMD check --as-cran --no-manual` with zero
errors, zero warnings, and one NOTE (new submission and optional tinyVAST from
the declared additional repository). All vignettes rebuilt. CRAN-mode tests
passed 3,096 expectations; the five visual groups skipped there passed locally.
Archive SHA256: `a2ab355aad0653b3a84e657b9b4391e27a33a3081b716470421d44aa1735fec4`.

The complete site built, and the new figures and numbered lightbox captions
were reviewed locally. A subsequent prose-only correction removed an obsolete
sentence from the main article; the corrected article was rebuilt with the
current source package. Coverage for 88e14ff is 89.39% overall, 94.48% for the
spatial-index adapter, and 96.30% for area integration. Relevant GitHub runs:
R-CMD-check 34412426492, coverage 34412426533, and pkgdown 34412426567.
The 49-cell, six-year tinyVAST example with 1,000 joint draws took about
1.3 seconds for integration on this Mac and retained an 8,832-byte summary.
This is output size, not a measurement of peak process RAM.

Spatial `Mean` is the plug-in expected response, not a Laplace bias-corrected
total. Uncertainty uses shared joint Gaussian parameter/field draws, not MCMC.
Native bias-corrected estimates, forecasting/year-varying environmental grids,
multivariate response/unit targets, and nonlocal sdmTMB covariate operators
are not implemented by this adapter. Existing brms and glmmTMB prediction
guards still apply. These limitations are documented, not silently substituted.
The median-display and legacy review above remain deferred. No CRAN or
win-builder submission is part of this increment.

### Possible future viewer (recommendation only)

An optional Shiny viewer could help users select models, terms, components,
and plot types, and export the selected plots and tables. It should consume
already calculated `influ_diag` and `influ_steps` objects, reuse the public
plotting methods, and avoid silently refitting models or running MCMC.
Diagnostic objects alone cannot supply native posterior predictive checks.
Defer this until the static interface is settled; do not make it a first-CRAN
release requirement or introduce Shiny dependencies now.

Record each outcome as keep, consolidate, defer, or remove. For kept features,
add a current example and appropriate tests. For removed names, document the
replacement or removal in NEWS. Freeze the public API only after these choices.

## 3. Agree the supported scientific scope

The 7 September hardening implements these conservative release boundaries:

- Multiple focus terms and single terms involving both focus and another
  variable warn and omit the standardised index. Implied-residual baselines
  and step plots reject ambiguous focus effects. A reference grid does not
  itself implement interaction marginalisation.
- Ordinary step refits preserve offsets, including negative-binomial GLMs.
  Diagnostic calculations with offsets are restricted to single-component
  log-link ratios and identity-link contrasts, where the reference offset
  cancels. Nonlinear probability and combined hurdle/zero-inflated outputs
  with offsets fail explicitly. Nominal summaries remain observed-response
  means, not response divided by exposure.
- brms lognormal models require constant sigma and an identity location link.
  Mean-parameterised lognormal backends require a log link. glmmTMB log-mean
  ratios remain supported with varying data-scale dispersion; dispersion
  effects are not separately decomposed.

Further extensions remain optional rather than promises of the initial release:

- Review grouping of multiple random-effect terms and the distinction between
  conditional latent uncertainty and full parameter uncertainty. Verify the
  specific structures needed for fisheries examples before extending claims.
- Review joint dependence for complex component combinations. Fixed-effect,
  posterior, and sparse-precision calculations have different approximations;
  intervals should describe the calculation actually used.
- Review the residual article's normal Q-Q screening, native residual helpers,
  and optional DHARMa simulation examples. Decide separately which complete-fit
  Bayesian posterior predictive examples to restore from the frozen page.

These decisions can narrow the documented release scope; they do not all
require adding new features before submission. Unsupported cases should be
clear to users and should not silently produce a different estimand.

## 4. Close the existing provenance questions

- A bounded source/history audit on 7 September found no concrete evidence
  requiring a licence change for gamInflu or CPUETools. Local upstream
  snapshots were compared with active and frozen influ2 source, including
  manual inspection of relevant methods. This is not a claim that no
  adaptation ever occurred; retain applicable notices if copying is later
  identified.
- Bentley's complete original notice is retained in the installed validation
  source, with Bentley and Trophia represented in copyright metadata.
- The current data are simulated. Earlier real-derived data are not part of
  the source archive. No Git-history rewrite was performed; that is a separate
  maintainer decision, not routine CRAN release tidying.

## 5. Validate the final reviewed source

After the last source, documentation, or API change:

1. Regenerate documentation, build the website, and inspect the rendered
   articles and changed visual snapshots. Check that the frozen review page
   and its figures remain available.
2. Run the appropriate numerical, backend, and visual tests. Build a fresh
   source archive using `R CMD build`, then run `R CMD check --as-cran` on that
   archive. Inspect its contents and size, including optional fixtures and
   excluded local files.
3. Run win-builder on the final archive with maintainer email
   `darcy@quantifish.co.nz`. Explain unavoidable notes in `cran-comments.md`.
   Do not reuse an earlier archive's successful result after changing source.
4. Commit and push the reviewed source. Verify Ubuntu-release and
   Windows-release GitHub checks and pkgdown deployment for that commit.
5. Record the final archive path and checksum, and confirm that package
   metadata, NEWS, README badges, and the published documentation agree.
6. Recheck the current [CRAN submission checklist](https://cran.r-project.org/web/packages/submission_checklist.html)
   and [repository policy](https://cran.r-project.org/web/packages/policies.html).
   Submit through the CRAN form and confirm its email only after the maintainer
   authorises the actual submission.

GitHub issue triage can be handled separately when the maintainer is ready.
# Response-adaptive calibration increment: 10 September 2026

The default fourth residual panel now uses fixed-bin probability calibration
for Bernoulli responses. Positive and combined catch distributions retain the
CDF, as do explicit distribution requests. Grouped-binomial calibration is
an explicit option with known trial counts. Bins preserve ties and simulation
summaries retain each draw's dependence without saving observation-by-draw arrays.

Scientific year-by-target/area or vessel/season group checks are optional and
exploratory; sparse support is labelled. The residual article includes a
well-specified simulated GAM, deliberately distorted probabilities, and an
intercept-only pooled pass with substantial grouped discrepancies. No BNS
files, model fits, or CPUE-index definitions are changed by this increment.

Joint hurdle/delta encounter extraction is component-aware. Native sdmTMB
positive simulation is supported; other joint positive adapters and
zero-inflated count-component extraction remain explicit limitations. Older
saved objects without sufficient metadata retain their CDF or require
recalculation; uncertainty is never invented. Weighted one-column binomial
fits must identify their known trial-count column explicitly.

The earlier pending CPUE-index/geo_mean changes, main-vignette Figure 14,
variable-width year boxplots, and lowercase brms documentation are included
in this publication batch. Remaining legacy triage and the frozen review
article remain deferred. Publication is not a CRAN submission.
