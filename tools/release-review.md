# Review before the first CRAN submission

This is the remaining review plan for influ2 1.1.0. It records decisions to
make before release, rather than evidence that a check has passed. Keep the
dated results for the final source archive in `cran-comments.md`. Submission
is a separate, later step authorised by the maintainer.

## 1. Scientific and visual review

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
  designed confounding illustrates point-estimate recovery; the Poisson bands
  are not an assessment of interval coverage for negative-binomial catches.

## 2. Decide what to keep from the earlier interface

Review the frozen page at `pkgdown/assets/articles/legacy-get-started.html`
against the current package. The source in `tools/legacy/R/` is preserved for
this review. Do not delete the page, its figures, or the frozen source until
each relevant feature has a recorded destination or an explicit removal
decision. The original Bentley `proto` implementation has a separate role as
a validation artefact and is not a candidate for the active runtime API.

The following functions are already exported and maintained in the current
package. They are not merely historical helpers:

| Current function | Remaining review decision |
| --- | --- |
| `plot_bubble()` | Confirm the purple and coloured sampling displays cover the old usage. |
| `plot_data_extent()` | Confirm the missing-data coverage display and ordering are suitable. |
| `plot_compare()` | Confirm index selection, common-period rescaling, labels, and interval defaults. |
| `plot_step()`, `influ_steps()` | Review automatic ordinary-model refits, explicit spatial-process stages, and reuse of compact results or supplied fits. |
| `get_bayes_R2()` | Retain the BRMS summary; decide whether a worked example is needed. |
| `table_criterion()` | Retain the BRMS criteria; review interpretation of LOO, R-squared, and log likelihood. |
| `plot_implied_residuals()` | Review the fisheries interpretation, strata threshold, residual choice, and one-standard-error bars. |
| `plot_predicted_residuals()` | Review residual types and smooths for each intended backend. |
| `plot_qq()` | Retain as normal-quantile screening; decide how to illustrate simulation-based checks for non-Gaussian models. |

The frozen functions below require specific choices. The listed replacements
cover related workflows; equivalence of every old argument and output column
has not been assumed.

| Frozen function or feature | Proposed destination or decision |
| --- | --- |
| `get_influ()`, `get_influ2()`, `plot_influ()` | Use `influ()`, `influ_effects()`, and `plot(..., type = "influence")`; identify any missing behaviour before retiring the old names. |
| `plot_bayesian_cdi()`, `plot_bayesian_cdi2()` | Use `plot(..., type = "cdi")`; compare layout, ordering, labels, intervals, and available term/component combinations. |
| `get_index()`, `get_unstandarsied()` | Review the old stock-assessment output section against `influ_indices()`; decide which columns, uncertainty summaries, and rescaling operations users need. |
| `plot_index()`, `plot_hurdle()` | Compare with `plot(..., type = "index")` and `plot(..., type = "components")`; check that component and combined-mean views cover the old use cases. |
| `rescale_index()` | Check whether `plot_compare(rescale = ..., rescale_series = ...)` is sufficient, or whether users need a public function returning rescaled tables. |
| `get_coefs()`, `get_coefs_raw()`, `get_marginal()` | Decide whether users need a public fitted-effect extractor beyond the current `diagnostic$coefficients` table, including marginal effects and draw summaries. |
| `influ_app()` | Decide whether to defer the interactive application; static diagnostics already have a supported interface. |
| `get_first_term()`, `id_var_type()`, `geo_mean()`, and other internal utilities | Keep internal only when needed by a retained feature; do not restore exports simply because they existed previously. |
| PPC bars and ECDF overlays in the frozen article | Decide which examples to restore using the original model and `bayesplot`; these are posterior predictive checks, not replacements for CDI. |

Record each outcome as keep, consolidate, defer, or remove. For kept features,
add a current example and appropriate tests. For removed names, document the
replacement or removal in NEWS. Freeze the public API only after these choices.

## 3. Agree the supported scientific scope

The current interfaces expose limitations that need either a documented
release decision or further development:

- Multiple terms involving the focus variable, especially interactions, do
  not automatically define a unique standardised index. Decide the required
  marginalisation rule and how an explicit prediction grid should express it.
- Review offset and exposure handling before claiming general support for
  catch-rate models with changing effort. An offset is not an estimated
  coefficient and requires an explicit place in the reference predictor.
- Varying lognormal scale or other distributional parameters require a
  separate treatment; a constant scale adjustment cancelling from a ratio
  does not establish support for a distributional model.
- Review grouping of multiple random-effect terms and the distinction between
  conditional latent uncertainty and full parameter uncertainty. Verify the
  specific structures needed for fisheries examples before extending claims.
- Review joint dependence for complex component combinations. Fixed-effect,
  posterior, and sparse-precision calculations have different approximations;
  intervals should describe the calculation actually used.
- Decide whether normal-residual Q-Q screening and the retained residual
  helpers are sufficient for the initial release, or whether worked posterior
  predictive or simulation-residual examples should be included first.

These decisions can narrow the documented release scope; they do not all
require adding new features before submission. Unsupported cases should be
clear to users and should not silently produce a different estimand.

## 4. Close the existing provenance questions

- Confirm authorship and licence compatibility for any code actually copied
  or adapted from `gamInflu` or CPUETools, retaining applicable notices.
  Methodological acknowledgement alone does not record copied-code authorship.
- Confirm whether the earlier real-derived lobster data in Git history need
  to be removed. The current dataset is synthetic; a history rewrite is a
  separate decision and must not be done as routine release tidying.
- Keep the frozen Bentley implementation and its original notices with the
  validation material.

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
