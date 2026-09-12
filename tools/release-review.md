# Review before the first CRAN submission

This is the remaining review plan for influ2 1.1.0. It records decisions to
make before release, rather than evidence that a check has passed. Keep the
dated results for the final source archive in `cran-comments.md`. Submission
is a separate, later step authorised by the maintainer.

## Gamma(log) implied effects: 12 September 2026

The maintainer authorised Gamma(log) support and downstream validation on the
saved Bluenose positive GAMs. Source `39bb732723c9c0b08729fa4ee011153ea55da919`
adds native-scale GLM/GAM/ML-glmmTMB adapters, a log-sum-exp local adjustment,
and stable conditional Gamma profile intervals. glmmTMB's dispersion
prediction is squared to obtain phi; GAMs retain sig2 and GLMs their native
summary dispersion. No shape is re-estimated. Non-positive responses,
unsupported links, non-unit weights, and combined two-part fits fail clearly.
The plot also breaks gaps in numeric years absent from fitted factor levels.

Independent density/profile checks cover constant and varying dispersion,
smooths, vessel effects, offsets plus effort slopes, row alignment, response
units, and extreme finite predictors. The full local suite passes 6,000
assertions, with no failures, warnings, or skips. Local coverage is 95.97484%.
The isolated minimal-installation check passes all 12 saved-result contracts
and 19 optional-package guards. Frozen N09/IV01 artefacts and study scripts
are unchanged; this extension does not claim Gamma interval calibration.

The final archive is
`/private/tmp/influ2-gamma-check.f3EqwG/final/influ2_1.1.0.tar.gz`, SHA256
`a7a34833f98a1cdd7b968d184351ec9cd01bb5e455f0a8b248e23a57d238830b`.
All 168 tracked shipped files match the checkout (apart from generated
DESCRIPTION metadata, checked separately by R). Mac R 4.6.1
`--as-cran --no-manual` completed with zero errors, zero warnings, and one
existing incoming NOTE for new submission/optional tinyVAST. DHARMa is
unavailable locally, and `_R_CHECK_FORCE_SUGGESTS_=false` was used. All nine
vignettes built and rebuilt; CRAN-mode tests passed 5,984 assertions with
seven intentional visual-test groups skipped. An initial code-analysis NOTE
about duplicate local helper names was corrected in this final archive.
The archive is installed in the usual local library. Local figure/lightbox
checks pass for 84 figures across nine articles, and the new Gamma figure
has been visually reviewed.

Downstream BNS verification covers all 903 supported area/year and target/year
cells from four saved positive Gamma GAMs. Maximum independent-benchmark
differences are below 1.9e-14 for adjustments and 2.5e-11 for profile endpoints.
Thirty-three local figures were generated; 20 replace positive-GAM area/target
plots in the existing CPUE presentation, with corrected effect-scale captions.
All 172 protected files, unrelated slides, meeting date, and presentation
filename remain unchanged. No models were refitted, no response simulations
were generated, and no assessment inputs were altered. The final installed
package reproduces all eight saved results exactly. Private data and output
figures are not in the public package. The local integration audit is in
`/Users/darcy/Projects/BNS/reviews/2026-09-12-gamma-implied-integration/`.
Binomial encounter and combined delta implied effects remain separate work.

GitHub coverage run 34670537513 passed all 6,000 assertions, with no failures,
warnings, or skips, and reported 95.97% coverage. R-CMD-check 34670537492
passed on Windows release but failed on Ubuntu release: 5,999 assertions
passed and one existing NB2 optimiser-coordinate agreement assertion failed.
All Gamma assertions passed. pkgdown 34670537460 and Pages 34670940080
passed; all 87 published figure/lightbox checks passed and the live Gamma
section was verified. The NB2 test correction and its new checks are recorded
separately below; the failed Ubuntu run is not a successful package check.
No CRAN/win-builder submission, issue changes, or resumption of parked work.

### NB2 likelihood-agreement test correction: 12 September 2026

The maintainer authorised fixing the numerical test. An independent native
NB2 score root now checks the local adjustment with an explicit absolute
log-shift error bound of 1e-9, allowing for the production root's 1e-10
tolerance. The independent dnbinom maximum is still checked, on the
likelihood scale, and both profile endpoints retain their existing checks.
Year/group and full-precision values are included in failure diagnostics.
Additional analytic cases cover zero and positive/negative near-zero shifts,
with two optimiser brackets. No production calculation, plotting code,
saved results, or frozen validation studies change. The targeted implied
effects and Gamma suite passes all 500 assertions without failures, warnings,
or skips. The full local suite passes all 6,027 assertions, also without
failures, warnings, or skips. Its graphics device writes to a temporary file
to avoid the local sandbox restriction on the default Rplots.pdf path.
New GitHub check results are pending verification.

## IV01 implied-effect validation: 12 September 2026

The maintainer approved a bounded NB2 glmmTMB follow-up: 100 datasets per
balanced/uneven and null/omitted-trend combination, each with 648 records,
six years, three seasons, and 12 vessel random intercepts. All 400 ML fits
were eligible, and all 800 known-parameter/public fitted-model calculations
succeeded without warnings. Production IDs 1--100 exclude the three pilot
IDs. No failed attempt was replaced and no example was selected by appearance.

The existing runtime at `2b17448` was held fixed. New study sources are
identified independently by their recorded MD5 hashes: study.R
`45955acb86e50ad9fa26fab6ffc84a6d`, run.R
`40e7633c24fba6e57bf8eb509aedc8d6`, protocol.md
`558a76cbe94a1a1e80f2156312e604f7`. The study files were new, uncommitted
developer work when run; they are not asserted to exist in that baseline
revision. R 4.6.1, glmmTMB 1.1.14, and TMB 1.9.25 were used. Checkpoints and
session information are in
`/private/tmp/influ2-implied-validation.3B9ypC/production`; the published
415,808-byte RDS retains all attempts, metrics, 14,400 cell records, summaries,
and the four compact public results from the first eligible replicate (1).
No fitted models or observation/simulation arrays are retained in the artefact.

Known-parameter pointwise coverage is 94.4--95.0%; fitted-conditional target
containment is 94.2--96.4%, explicitly not coverage for a fixed interaction.
Under the null, fitted-model zero exclusions occur in 0.8--0.9% of supported
cell intervals, with any exclusion in 9--12% of datasets. Known-parameter
any-cell null exclusions are 49--65%; pointwise bars are not simultaneous
tests. With the omitted trend, fitted adjustments track their own conditional
targets (mean within-dataset RMSE 0.118 balanced / 0.139 uneven), but those
targets differ from the injected pattern (RMSE 0.107 / 0.317). Strong supported
cells retain the injected direction 100.0% / 98.3% of the time, whereas only
92.5% / 32.8% exclude zero. Uneven sampling changes available support and
signal absorption into fitted main effects and dispersion. These are one
effect size and two fixed designs, not universal interval calibration or
a power curve. Monte Carlo errors are calculated between datasets.

The article adds two figures, interval/fit/support tables, and these limits.
The independent developer checks pass 66 assertions; portable frozen-result
tests pass 399 assertions without fitting a study model. The original N09
artefact, hashed N09 study scripts, diagnostic code, and defaults are unchanged.
The full local suite passes 5,733 assertions with no failures, warnings, or
skips. Both new figures were rendered and visually reviewed; the isolated
conditional targets remain visible as teal crosses, and no lines bridge
unsupported cells. All 83 plotted-image/lightbox caption checks across nine
local articles pass. Browser review verified both new figures and their full
expanded captions. Local coverage remains 95.95278%; GitHub coverage run
34666609234 passed 5,733 assertions and reported 95.95%.

Source `2989db942f7aa1e9a61af45d2b1e17c3232bd835` is pushed. The fresh archive
`/private/tmp/influ2-iv01-check.KUhmnd/influ2_1.1.0.tar.gz` has SHA256
`fa4cd5c2cd650719a2b67c11e572cc82f1e46c12dd7c89c3f39b87a34ad52174`;
all 167 tracked shipped files match the checkout. The frozen IV01 RDS SHA256
is `6d4ec28807e5c576b62d48c01b539837d3c0addca9cfee6df6a0feb544ace808`.
The Mac R 4.6.1 `--as-cran --no-manual` check completed with zero errors,
zero warnings, and one existing incoming NOTE (new submission and optional
tinyVAST through its declared repository). `_R_CHECK_FORCE_SUGGESTS_=false`
was used because DHARMa is unavailable locally. All nine vignettes built and
rebuilt; CRAN-mode tests passed 5,717 assertions, with seven intentional
visual-test groups skipped. Installed size is 5.9 MB, including 3.9 MB of
documentation, reported as INFO rather than a warning. This does not claim
complete optional-dependency or PDF-manual checks. The isolated minimal
installation check passed its 12 saved-result contracts, fresh calculations,
and 19 missing-package guards. The checked archive is installed in the usual
Mac R library, with the exact frozen artefact and fresh-session plot verified.

Website run 34666609377 succeeded with 86 plotted-image/lightbox caption
checks across nine articles. Pages run 34667026970 published revision
`356feec619247658c95a74e53a7213c194a7c848`. Live browser review at
`https://www.quantifish.co.nz/influ2/articles/implied-effects.html` confirmed
the IV01 section, both new figures, and their full matching expanded captions.
GitHub R-CMD-check run 34666609202 passed on Ubuntu release and Windows
release. Each reported `Status: OK` and 5,733 passing assertions, with zero
failures, warnings, or skips. These checks apply to source
`2989db942f7aa1e9a61af45d2b1e17c3232bd835`; only excluded validation records
changed subsequently. This validation/publication follow-up is complete.
No CRAN or win-builder submission, issue change, or resumption of parked work
was made.

The additive BNS request was investigated read-only. All eight actual saved
component GAMs reproduce the supported-family error with the correct
`year_factor` and `area_factor` arguments: four Gamma(log) positive models
and four binomial(logit) encounter models. This is missing family support in
the newly restored likelihood-based implied diagnostic, not evidence that
the fits are invalid. Its preceding pinned BNS workflow plots zero-centred
PIT summaries under an older API, which cannot substitute for implied effects.
A Gamma log-mean likelihood adapter, independent optimum/profile tests, and
native GAM dispersion checks are the next appropriate extension. Positive,
encounter, and combined delta targets must remain separate. No BNS source,
model, presentation, or assessment input was modified, and no Gamma support
was silently added during this validation-only increment.

## N09 PIT plotting correction: 12 September 2026

The maintainer approved the contained reference-grid correction after reviewing
the N09 finding. Both optional PIT plots now use `(0:K) / K`, including the
zero endpoint, for bayesplot's unchanged simultaneous independent-uniform
limits and the empirical CDF. The difference display subtracts the same grid.
Stored PIT values, response simulations, conditioning, and default panels are
unchanged. The original N09 artefact and the hashed study scripts are intact;
their `displayed_band_crossing` metrics still describe the historical display.

The separate `tools/n09/check-pit-alignment.R` control reproduces 477 crossings
in 10,000 independent-uniform samples (4.77%), versus the original 1,246
(12.46%); the independent DKW reference still gives 471 (4.71%). This is a
plotting/calculation alignment check, not fitted-model residual calibration.
The latter and the broader scientific interpretation remain for review.

Validation before publication: 5,334 full-suite assertions passed, with no
failures, warnings, or skips. Coverage is 95.95278%. Numerical tests compare
the corrected plot with frozen N09 limits, retain endpoint masses and ties,
verify subtraction on the common grid, preserve diagnostic/RNG/theme state,
and reject unrecognised future dependency layouts without double-correcting
already aligned output. The one affected visual snapshot was reviewed and
updated. The two affected articles and plotting reference page were rebuilt;
the PIT figures were visually inspected, and all 81 plotted-image/lightbox
caption checks across nine local articles passed.

Source `b027b73c01e701bfcfe209b0de316e9e390dba75` is pushed. Its fresh source
archive passes the Mac R 4.6.1 `--as-cran --no-manual` check with zero errors,
zero warnings, and the existing incoming NOTE (new submission and optional
tinyVAST through its declared repository). `_R_CHECK_FORCE_SUGGESTS_=false`
was used because optional DHARMa is unavailable locally. All nine vignettes
built and rebuilt; CRAN-mode tests passed 5,318 assertions with seven
intentional visual-test groups skipped. The isolated minimal-installation
check passed its 12 saved-result contracts, fresh core calculations, and
19 missing-package guards. The exact archive hash and log are recorded in
`cran-comments.md`; all 165 tracked shipped files match the checkout.

The checked archive is installed in the usual Mac R library. A fresh-session
PIT plot verifies the corrected grid and unchanged stored diagnostic.
GitHub coverage run 34660520149 passed 5,334 assertions and reports 95.95%.
Website run 34660520151 passed all 84 figure/lightbox-caption checks across
nine articles; Pages run 34661011378 published revision
`9bbaf9c152a42f6a1e2b5e8926cd15673f85eabe`. Live browser inspection verified
the corrected residual article's Figure 16 and its full expanded caption,
and the N09 correction explanation and Figure 6 caption. R-CMD-check run
34660520086 passed on Ubuntu release and Windows release: each reported
`Status: OK` and 5,334 passing assertions with zero failures, warnings, or
skips. These checks apply to source `b027b73c01e701bfcfe209b0de316e9e390dba75`,
not to a later package change. Only excluded validation records changed
after that source revision. This validation/publication follow-up is complete.
No CRAN/win-builder submission, upstream issue, or change to the parked release
decisions was made.

## Priority update: 12 September 2026

The maintainer subsequently approved the bounded N09 study and a separate
residual-implied-effect implementation. These supersede the earlier parked
status of the first N09 increment, not the remaining release decisions.

N09 now contains 100 NB2 datasets for each of glmmTMB and sdmTMB, 500 eligible
fits, and 1,960 diagnostic records including known-truth controls and paired
seed/count sensitivity. No production fits failed eligibility. The compact
installed artefact is approximately 263 kB; it retains summaries and selected
diagnostics, not model fits or observation-by-simulation matrices. The
`Residual validation` article is built from these frozen results. Runtime and
study hashes record the original baseline independently of later package edits.
See [tools/n09](n09/README.md) for the protocol and reproduction commands.

Pooled PIT checks can miss omitted covariates or spatial structure, and different
conditioning choices answer different questions. The separate bayesplot 1.16.0
reference-grid finding was documented separately; the approved correction is
recorded above. Review
calibration and interpretation with Nicholas before changing defaults or making
broader fitted-model calibration claims.

`plot_implied_residuals()` now addresses the effect-scale question directly,
using compact `implied_effects()` results and one local likelihood shift per
year/group stratum. Gaussian identity, NB2 log, and Poisson log adapters cover
lm, GLM, GAM, and glmmTMB. Original parameters, offsets, smooths, and fitted
random effects remain fixed. Conditional profile intervals omit uncertainty in
the original fit and dependence; they are not refitted-interaction intervals.
Unsupported backends, families, two-part models, weighted fits, and year
interactions fail explicitly. They require additional validated adapters, not
an automatic fallback to PIT scores or positive-component residuals.

The `Residual-implied effects` article distinguishes two historical conventions:
ordinary log-response residuals, which agree exactly with the new shifts for
constant log variance, and the analyser GLM's centred `rstandard()` recipe,
which does not generally agree and mixes scales. Both have worked comparisons.
The old zero-centred PIT summaries remain unchanged as
`plot_grouped_residuals()`. New numerical tests independently reconstruct both
comparisons and count likelihoods; visual regression tests cover both displays.

The final local regression suite passed 5,234 assertions with zero failures,
warnings, or skips, including the two new visual baselines. The three new
implied-effect figures and six N09 figures were visually inspected. All 81
plotted-image/lightbox-caption checks across nine locally rendered articles
passed. All nine source vignettes built into a fresh archive. Source-archive
checks and publication are now verified, as recorded below.
Local coverage is 95.95%, above the agreed 95% target. The minimal-installation
check also passes its 12 saved-result, fresh-core, and 19 missing-package
contracts; those existing contracts do not yet include a separate fresh-process
implied-result case. The new implied-result tests cover numerical equality,
compact RDS round trips, plotting, ordering, RNG preservation, and explicit guards.
No CRAN or win-builder submission was made.
Issue #18, #22, the median-column decision, and legacy/frozen-article review
remain parked; no GitHub issues were closed.

### Verified checks and publication: 12 September 2026

These results apply to source
`e9ed7f279055c5cf75f6dd4e33016ee82bc1dd03`, not to a later package revision.
The checked source archive is `/private/tmp/influ2_1.1.0.tar.gz`, SHA256
`645dfd53317b5b743a8640931b9d85d765cb845436dc185fbd32e9cfb95c21b9`.
All 165 repository files shipped alongside the normal generated DESCRIPTION
metadata match the source checkout byte-for-byte; developer files and private
review material are excluded. Verify the hash before reusing this temporary
archive path. The archive is installed in the usual Mac R library, and a fresh
R session successfully calculated and plotted the new implied-effect object.

Local macOS arm64 R 4.6.1 `--as-cran --no-manual` checking finished with zero
errors, zero warnings, and one incoming-feasibility NOTE. That NOTE records
the new submission, optional tinyVAST's declared additional repository, and a
study-source URL that returned 404 before the new source was pushed. The URL
now resolves and lists all six `tools/n09` source files; the completed check
log itself has not been rewritten or described as a clean incoming check.
The first strict attempt stopped because optional DHARMa was unavailable.
The completed local check used `_R_CHECK_FORCE_SUGGESTS_=false`; it is not a
complete local optional-dependency or PDF-manual check. All nine vignettes
rebuilt successfully. CRAN-mode tests passed 5,218 assertions with seven
intentional visual-test groups skipped; the full run passed those as well.
Evidence: `/private/tmp/influ2-implied-check-optional.log` and
`/private/tmp/influ2-implied-check.WPyb0h/influ2.Rcheck/00check.log`.

[R-CMD-check run 34651733821](https://github.com/quantifish/influ2/actions/runs/34651733821)
passed on both Ubuntu release and Windows release. Each reported `Status: OK`
and 5,234 passing assertions with no failures, warnings, or skips. The existing
Ubuntu minimal-installation smoke check passed; no CI platforms were added.
[Coverage run 34651733817](https://github.com/quantifish/influ2/actions/runs/34651733817)
passed and uploaded 95.95% coverage, matching the local result.

[Website run 34651733809](https://github.com/quantifish/influ2/actions/runs/34651733809)
passed all 84 plotted-image/lightbox caption checks across nine articles.
[Pages run 34652395977](https://github.com/quantifish/influ2/actions/runs/34652395977)
published website revision `1afeba237957299dd9a735cc8470122a10921a26`.
Live browser inspection verified the three implied-effect comparisons and
their full numbered lightbox captions, with seasonal facets ordered Early
season, Mid-season, and Late season. The six N09 captions and all 18 residual
article captions also match their expanded versions. The deployed residual
article uses `plot_grouped_residuals()` for PIT departures and links the new
effect-scale calculation; the N09 article reports all 500 eligible fits.

Only the excluded validation records change after this checked revision.
No scientific defaults, issues, or parked review decisions were changed, and
no CRAN or win-builder submission was made. The completed follow-up is paused.

## Earlier priority update: 11 September 2026 (superseded above)

The subsequent N09 calibration-study and interpretation discussion is now
explicitly parked. Preserve its proposed scope for later review with Nicholas;
do not start simulation studies or change conditioning defaults. The current
work is limited to small presentation improvements, including the worked
`plot_data_extent()` example, and the website-deployment action update selected
as the second small task. The maintainer subsequently approved a contained
save/reload reliability check and a short worked example. The calibration
study is not being started.

The maintainer has explicitly parked issue #18 and its
[two-region plan](two-region-plan.md). Preserve the plan and leave the issue
open; no regional example or API work is to begin yet. Collaboration with
Nicholas Ducharme-Barth on his proposed residual-diagnostic extensions takes
priority. Remaining release review, legacy triage, and index-median decisions
will be revisited afterwards. This prioritisation does not authorise CRAN
submission or imply that the residual proposal has already been implemented.

The [development consideration list](development-backlog.md) tracks Nicholas's
individual suggestions, potential Ginflu-inspired features, and related parked
decisions. It separates discussion priorities from optional candidates; adding
an item there does not approve implementation or make it a CRAN release gate.

## Automated minimal-installation check, 12 September 2026

The maintainer approved repeating the earlier one-off minimal-library check
automatically within the existing Ubuntu-release job. The Windows-release job
is unchanged; no jobs or platforms are added. Run
`Rscript --vanilla tools/check-minimal-install.R` from the repository root.

The driver stages only the mandatory dependency closure, builds the current
source without rebuilding vignettes, and installs it with user/site libraries
excluded. R's default base/recommended packages remain available. Unexpected
extra packages in the default library fail explicitly; existing libraries are
never edited or uninstalled. A negative control confirmed that the worker
rejects the ordinary, non-isolated user/site library paths.

With optional packages available, a separate producer creates 12 compact
result baselines from the small GLM/glmmTMB fixtures and stored brms draws or
residual summaries. No MCMC is run. The isolated process then confirms that
21 optional packages, including brms, rstan, glmmTMB, sdmTMB, tinyVAST,
posterior, bayesplot, TMB, and testthat, cannot be found or loaded. It reproduces
the tables, printed summaries, plot coordinates, labels, and panel layouts
without fits or posterior arrays, preserves saved objects and the RNG state,
and draws the plots. Fresh GLM influence, preview, PIT residuals, standardised
indices, area integration, and steps also agree with the full-library results.

The expanded smoke test exposed unclear missing-backend errors in residual
and index entry points: model-frame/formula extraction could happen before
checking that native backend methods were available. An early guard now names
the required package and explains that saved compact results remain usable.
All 19 isolated missing-package checks pass, including optional PIT ECDF
panels. Ordinary regression tests additionally cover GAM requirements and
confirm that GLMs and compact diagnostics do not acquire backend dependencies.
No numerical method, prediction convention, default panel, or public API has
changed. Local smoke-test evidence is in
`/private/tmp/influ2-minimal-install-verified.log`.

The complete local regression suite passed 4,634 expectations with no failures,
warnings, or skips, including the 24 new guard expectations and existing visual
snapshots. The run uses a null PDF device to avoid creating `Rplots.pdf` in the
checkout. Evidence: `/private/tmp/influ2-minimal-regression-verified.log`.

This is an installation/core reliability check, not a full no-Suggests
vignette build, six-backend calibration study, or final CRAN archive check.
N09, N02, #18, #22, and the legacy/frozen-article review remain parked. No
CRAN or win-builder submission was made.

The preceding restart increment's R-CMD-check run 34564362595 has now completed
successfully on both Ubuntu release and Windows release.

Source `6d3b93b` passed GitHub coverage run 34644847655: 96.00% coverage and
4,634 passing expectations, with no warnings or skips. The first Ubuntu smoke
step failed before installation because the driver unnecessarily required
`cpp11`, a build-time header dependency of an already compiled binary. The
driver now stages the transitive Depends/Imports closure, plus any build-time
dependencies declared by influ2 itself. Its corrected local run passes all
checks (`/private/tmp/influ2-minimal-runtime-closure.log`). This correction
changes only excluded developer files, not the covered package code.

The initial website run 34644847533 failed before article execution on an
external CDN SSL error while downloading bootstrap-toc. Only that failed
workflow was retried; this is not a statistical or vignette regression.

The corrected developer script is published in `7bd30fa`. Its minimal check
passed in 46 seconds on Ubuntu in workflow 34645409513, and was correctly
skipped on Windows. The existing two-platform archive checks were still
running at this review, as was the website retry. The superseded run
34644847513 was cancelled to avoid duplicate Windows work. Coverage and the
website did not need another run for the developer-only correction. The Mac's
usual installed influ2 matches the tested package code; an installed CPUE-index
smoke calculation passed. All source changes are committed and pushed.

## Compact-result restart checks, 11 September 2026

The approved increment tests saving and reopening the four compact result
types: influence diagnostics, residual diagnostics, CPUE indices, and step
comparisons. Baselines are generated from the existing small GLM fixture and
a mixed-effect glmmTMB version; stored brms fixtures additionally exercise
Bayesian influence and posterior-predictive residual summaries. No MCMC is
run. These are representative backend tests, not a new six-backend calibration
study. Default numerical methods, retained data, and public APIs are unchanged.

A separate R process receives only saved-result file paths and test-code paths.
It loads the same source or installed package under test, without the original
fits, then checks full-object identity, summary/print output, plot coordinates,
axis labels/ranges, and panel layouts. Composite plots are actually drawn.
Checks reject hidden live state and verify unchanged objects and RNG state;
glmmTMB, brms, and spatial modelling namespaces are not loaded in the child.
The new `callr` and `pkgload` declarations are test-only Suggests, not runtime
imports. Tests with an installed package use that installation rather than a
possibly stale copy from another library.

The main vignette adds a save/reopen example using the existing lobster fits
and results. It distinguishes the temporary vignette file from a persistent
analysis path and documents same-version, external-draw-file, retained-fit,
and observation-data-sharing limits. All 4,610 full-suite expectations passed
locally, without failures, warnings, or skips. The 80 new expectations also
passed against an isolated installed package. Coverage instrumentation passed
and reports 95.99%. The executed main-vignette example and news rebuilt, its
rendered instructions were checked, and all 72 local plotted-image/lightbox
checks passed across seven articles. No plotting or calculation function body,
export, or existing visual snapshot changed. These are not a new CRAN archive
check; no CRAN or win-builder submission was made.

Source `4f7d78eaf27864ddb725101057359b2f955a7c75` is published and installed
in the usual Mac R library. GitHub coverage run 34564362598 passed all 4,610
expectations, with no warnings or skips, and confirmed 95.99% coverage.
Website run 34564362596 passed all 75 plotted-image/lightbox checks across
seven articles, including optional examples. Pages run 34564853504 deployed
website revision `d41f3bb543b67077f952d8b35963d6ce5775b8ad`. The completed
website and coverage jobs have zero annotations.

Direct public-page checks verified the new save/reopen section, its code and
limitations, and preservation of the preceding data-completeness example.
Ubuntu-release and Windows-release checks in run 34564362595 were still
running at this publication review. The preceding data-completeness source's
two release-platform checks (34562564813) have both passed. This record is
excluded bookkeeping, not a change to the tested package source.

## Data-completeness example and deployment maintenance, 11 September 2026

The main vignette now introduces `plot_data_extent()` immediately after loading
the simulated lobster data. A separate copy has deliberately missing depth and
soak-time measurements; all fitted examples still use the unchanged original.
The figure and help explain the non-missing-record denominator, red reference
markers, and why a zero response counts as observed. The example does not
estimate sampling intensity or joint complete-case coverage. Figure numbering
and the full lightbox caption follow the shared site implementation.

All 4,530 local test expectations passed without failures, warnings, or skips.
The main article, help example, and news rebuilt successfully. The new figure
was visually inspected, and all 72 plotted-image/lightbox checks passed on
seven locally rendered articles. No calculation or plotting function body
changed, and no new CRAN archive or win-builder submission was made.

For the maintainer's requested second small task, the website-deployment action
was upgraded from v4.4.1 to the verified v4.9.0 release, pinned to its immutable
commit `fa24774553152dd7873cd16ebd8d959b010c5445`. Its native Node 24 runtime
addresses the recurring old-runtime annotation. Upstream metadata confirms
support for the existing `clean`, `branch`, and `folder` inputs. Permissions,
deployment target, clean behaviour, workflow triggers, and the two-platform
R-check matrix are unchanged. See the [upstream release](https://github.com/JamesIves/github-pages-deploy-action/releases/tag/v4.9.0).

Publication is verified for source `6f47bfce1d1c6838d60b4b96c6550e8c5a0799ee`.
Coverage run 34562564878 passed all 4,530 expectations, with no warnings or
skips, and retained 95.98% coverage. Website run 34562564747 passed, including
all 75 plotted-image/lightbox checks across seven articles with optional
examples present. Its deployment step completed with the updated action, and
the job has zero annotations: the old-runtime warning is gone. Pages run
34563053123 published website revision
`7772f6ee984f2b3363fd9dbb0baa8c385bb12183`.

Direct public-page checks verified the new main-article section, its first
figure and full caption, and the worked help example. All 54 local
year-variable completeness proportions were independently checked against
the demonstration data; the original dataset and zero responses were
unchanged. The updated package is installed in the usual Mac R library.
Workflow YAML comparison confirmed that only the action revision changed.
Ubuntu-release and Windows-release checks in run 34562564813 were still
running at this publication review. This validation record is excluded from
the package archive and does not change the checked source.

## CDI label and website organisation, 11 September 2026

The ratio-scale CDI axis now reads "Relative Effect", independent of the term
name. The two changed visual snapshots differ only in that text and its width;
all numerical calculations, other labels, and layouts remain unchanged. The
reference index moves `plot_bubble()` to Additional diagnostics and comparisons,
while retaining class-specific `plot()`/`autoplot()` methods with their own
topics and clarifying their descriptions and help links. Residual diagnostics
is first and Bentley validation last in the menu and article index; other
articles retain their relative order. The frozen legacy article is preserved.

All 4,526 full-suite expectations passed, with no failures, warnings, or skips.
The full website rebuilt, its revised Bayesian CDI was visually inspected,
and navigation/reference grouping and all 71 local figure-caption checks
passed. The tested package is installed in the usual Mac R library. These
presentation changes do not constitute a new CRAN archive check or submission.
The N09 calibration study and interpretation review are explicitly parked.

Source `1df6de87776064d1aea93ee89cd58600260745b9` is published. Coverage run
34560235613 passed all 4,526 expectations and retained 95.98% coverage.
Website run 34560235574 passed all 74 figure-caption checks across seven
articles; Pages run 34560740242 deployed website revision
`30d2ec823228e16bac9a60c62959ba9c03c769f5`. Public verification passed on ten
page menus, the article index, all plotting reference groupings, and the new
CDI help. Local checks additionally covered all 44 generated page menus.
Platform check run 34560235596 was still running at this publication review.
The existing deployment-action Node-runtime annotation remains non-blocking.
This final publication record is excluded from the package archive.

## Explicit residual conditioning, 11 September 2026

PR #26 (source `02573236c77ec700a42d9d99da11640f3ade3a04`) adds the approved
N06 options while preserving all existing backend defaults. glmmTMB now
offers fitted or new random effects; sdmTMB offers fitted effects, one shared
conditional latent draw, or new effects; tinyVAST offers fitted effects or
one shared conditional draw. GLM/GAM and brms retain their respective fitted
and posterior-predictive targets. Unsupported combinations fail explicitly.
Shared conditional draws require converged, unprofiled ML fits, not REML.

The [conditioning audit](residual-conditioning-audit.md) records the native
interfaces, safeguards, implementation limits, and numerical evidence. All
4,525 local expectations passed without failures, warnings, or skips,
including the frozen pre-refactor baselines. Local and GitHub coverage are
95.98%. The final checked archive passed macOS `--as-cran --no-manual` with
zero errors, zero warnings, and the existing incoming-feasibility NOTE; its
checksum, optional-dependency limits, and installation are in
`../cran-comments.md`.

Spatial Figures 9–11 demonstrate the alternatives with the existing sdmTMB
and tinyVAST examples. They were rendered and visually inspected. All 71
local and 74 GitHub plotted-image/lightbox checks passed across seven pages.
PR coverage run 34543811615 and website run 34543811639 passed. Ubuntu release
and Windows release both passed run 34543811601. PR #26 was merged as
`d230d6861e40996c2a53419bedcd7c07833f7b54`, whose source tree is identical
to the checked source. Post-merge coverage run 34545318736 and website run
34545318608 passed. Pages run 34545915567 published website revision
`459ea2a96e93a1965eb544d51dabf01aead926a2`. Direct public-site checks verified
the new spatial examples, conditioning support table, and help, including the
ML/non-REML guard. The three plotted comparisons were visually reviewed
locally before publication; automated caption checks passed on the deployed
build. The existing deployment-action Node-runtime annotation remains
non-blocking; the action succeeded using GitHub's Node 24 override.

The automatic post-merge R-CMD-check rerun 34545318590 subsequently passed
both release platforms, verified during the later website tidy-up. Both had
also passed the identical source in PR run 34543811601. The excluded
validation records do not alter the package source checked in those runs.

Next discuss N09 calibration and interpretation before changing defaults.
A common layout does not make diagnostic targets interchangeable or establish
calibrated tests. N02 counters, N07 retention, N08 further bridges, issue #18,
legacy triage, and the index-median review remain untouched. No CRAN or
win-builder submission was made.

## Shared residual engine and external simulations, 11 September 2026

The approved N01/N04/N05 increment freezes the pre-refactor numerical/RNG
baseline, extracts the existing calculation into one shared engine, and adds
`as_influ_residuals()` for an already generated response matrix. Its explicit
contract covers observation IDs/order, complete joint simulation columns,
response/time columns, components, conditioning, and binomial probabilities/
trial counts. Predictive means come from the supplied simulations. No models
are fitted, responses generated, rows automatically realigned, or matrices
retained. Existing native simulation schemes and defaults are unchanged.

All 4,378 local test expectations passed, with zero failures, warnings, or
skips. Eighteen frozen engine cases matched the original source `6c3d1c9`
exactly on the Mac; portable tests allow `1e-12` floating-point roundoff.
Independent external-input calculations and a real glmmTMB simulation replay
validate the new route separately from the frozen adapter fixtures. Input
failure, RNG, component, calibration, batching, compact-retention, native
backend, and existing visual regression tests passed.

The executed lobster glmmTMB example in the residual article was rendered and
visually inspected. All 68 plotted images on seven locally rendered articles
passed the lightbox caption check. The final archive of source `09971c2`
passed macOS R 4.6.1 `--as-cran --no-manual` with zero errors, zero warnings,
and the existing incoming-feasibility NOTE. All seven vignettes rebuilt;
optional DHARMa examples were skipped locally. The checked archive is installed
on the Mac. Its checksum and check limits are recorded in `cran-comments.md`.
An additional check with an existing complete negative-binomial brms fit
verified the posterior-prediction transposition route, independent ranks/means/
ECDF intervals, input/RNG preservation, and plotting, without MCMC or a refit.

GitHub coverage passed at 95.90%, with all 4,378 expectations passing. The
website and Pages deployment passed, including all 71 caption checks across
seven articles with optional examples present. Live Figure 18 demonstrates
external glmmTMB simulations and expands with its full caption. Ubuntu release
and Windows release both passed check 34536328498. No CRAN or win-builder
submission was made. N02 counters, N06 conditioning, further
bridges, regional examples, and the remaining legacy/median reviews are untouched.

## Completed: PIT displays and caption consistency, 11 September 2026

The normal-score transformation is already implemented, not parked work:
`observations$residual = qnorm(pit)` supplies the default Q-Q, fitted-value,
and year panels. Their axes and overview footer now explicitly identify PIT.
The maintainer approved optional `pit_ecdf` and `pit_ecdf_diff` views of the
same stored PIT values, plus arbitrary four-panel selection with `panels`.
The default remains `c("qq", "fitted", "year", "auto")`; the response ECDF
and encounter-calibration choices are unchanged. These additions do not
change simulation conditioning or add refits, MCMC, or response simulations.

Optional bayesplot >= 1.16.0 provides numerically adjusted simultaneous
iid-uniform reference limits. These are explicitly exploratory references,
not calibrated fitted-model tests or LOO-PIT. Default plotting does not need
bayesplot. Normal scores do not establish calibration or imply that the
modelled response itself is normally distributed.

Validation passed 508 residual-test expectations and all 4,037 full-suite
expectations, with zero failures, warnings, or skips. Native bayesplot curves
and limits, RNG/object/global-theme preservation, optional-dependency errors,
and configurable layouts have numerical and visual regression coverage. The
residual article and plotting help were rebuilt and the new figures reviewed.

The shared lightbox now uses the full numbered visible caption rather than
the shorter image-alt description. All 67 plotted images on seven locally
rendered pages passed the caption check, including the frozen article; the
pkgdown workflow now runs that check before deployment. Long captions are
accessible below the image, and the grey calibration bars are unchanged.
No CRAN or win-builder submission is authorised by this increment. The
archive-specific checks in `cran-comments.md` must be rerun on the eventual
release candidate; the earlier archive is not this new source revision.

The new source archive passed macOS R 4.6.1 `--as-cran --no-manual` with
zero errors, zero warnings, and the existing incoming-feasibility NOTE.
All seven vignettes rebuilt, with optional DHARMa examples skipped because
that dependency was absent locally. See `cran-comments.md` for the exact
archive and checksum; dependency-complete CI and website publication are
checked separately.

Website publication and browser review are complete for source `0abbab1`.
The CI caption check and live browser audit cover all 70 plotted images on
seven articles, including optional DHARMa examples and the frozen article.
Coverage passed at 95.75%. Ubuntu release and Windows release both passed
R-CMD-check run 34529978869. The checked archive is installed on the Mac.
There is a non-blocking Actions annotation about the pinned deployment action's
old Node runtime; it ran successfully under GitHub's Node 24 override. Review
that action version during routine CI maintenance, not as a change to this
increment's numerical or plotting behaviour.

## Issue #12 completion review and issue #18 planning: 10 September 2026

The residual article now contains a standalone glmmTMB lobster ECDF (Figure 4),
an executed complete-fit brms posterior predictive ECDF (Figure 5), and twenty
individual predictive ECDF curves (Figure 6). Bayesian preparation reuses the
previously fitted Gaussian model from the six-backend comparison. Rendering
loads a 13,836-byte compact result; no model fit or observation-by-draw matrix
is shipped. Preparation and native pp_check recipes are documented, along
with the differences between response ECDFs, fitted-data ranks, LOO-PIT, and
LOOIC. A universal LOO-PIT interface and automatic refits remain outside the
agreed scope, not features claimed to be fulfilled by the Q-Q plot.

Local review passed 111 new focused expectations and all 3,934 full-suite
expectations, with zero failures, warnings, or skips. Independent native brms
replay verified all saved ECDF quantiles, predictive means, ranks, and twenty
overlay curves. The native pp_check recipe also executed on the complete fit.
The three rendered figures were visually inspected, and browser inspection
confirmed captions, sequential numbering, and larger-view controls. No
runtime calculation or public API is changed by this increment. Source
225871b passed the local archive check with zero errors/warnings and the
existing NOTE. Ubuntu release, Windows release, coverage (95.72%), pkgdown,
and Pages all passed. All fifteen live captions match the reviewed article.
Issue #12 is closed with the scope and evidence in comment 5615100852.
The exact archive and checksum are recorded in `cran-comments.md`.

Issue #18 is planning only at the maintainer's request. The proposed separate
Regional CPUE indices vignette uses one four-area model, aggregating areas
1+2 and 3+4 with fixed regional reference weights. See
[two-region-plan.md](two-region-plan.md) for interaction requirements, response-
scale averaging, regional uncertainty, tests, and scope. No regional example
or API has been implemented, and #18 remains open. Median/NA, remaining legacy
triage, BNS migration, and submission are untouched.

## Generalised residual follow-up: 10 September 2026

Follow-up: the mixed comparison example now includes all six backends in one
executed table on 150 common simulated observations. The residual helper audit
and migration are recorded in [residual-audit.md](residual-audit.md).
Both maintained helpers now use generalised simulation ranks. The old implied
coefficient arithmetic is replaced by grouped mean departures around zero;
this changed interpretation needs visual review. BNS source and saved outputs
were deliberately not changed. Its explicit native-residual calls and downstream
`$implied` assumptions require a separately reviewed migration.

Local validation of this increment: 3,823 passing expectations, no failures,
warnings, or skips; coverage 95.72%. A complete existing brms fit passed both
helpers without MCMC. The grouped visual baseline was inspected and updated.
Final archive checks passed `--as-cran --no-manual` with zero errors, zero
warnings, and the existing new-submission/optional-tinyVAST NOTE. The source
archive and checksum are recorded in `cran-comments.md`, along with the
manual-enabled attempt's TeX PATH limitation. For source 3837b0b, GitHub Ubuntu
release and Windows release both passed with Status: OK and 3,823 passing
expectations, without failures, warnings, or skips. Coverage, pkgdown, and Pages
publication also passed. The five live comparison tables and twelve residual
captions match the reviewed local pages, including the six-backend table and
the grouped normal-score interpretation. The scoped follow-up is complete and
paused. No CRAN or win-builder submission has been made.

## Model-comparison increment: 10 September 2026

The maintainer commissioned model-specific `table_criterion()` methods for
GLM, GAM, glmmTMB, brms, sdmTMB, and tinyVAST, including mixed lists. The new
Model comparison article demonstrates native likelihood summaries and a
compact, executed complete-brms-fit example. Issue #12 is explicitly parked
while this increment is reviewed; no ECDF/LOO-PIT issue closure is implied.

- Native AIC, BIC, log likelihood, response deviance, sample size, and penalty
  degrees of freedom are reported where appropriate. Bayesian LOOIC, paired
  ELPD differences/standard errors, Pareto-k, and R-squared remain distinct.
- Conditional AIC is opt-in. mgcv uses its native conditional AIC and corrected
  effective penalty where available. sdmTMB/tinyVAST use their native
  approximations. Mixed glmmTMB cAIC is unavailable; no conversion or guessed
  penalty is substituted. Profiled spatial cAIC remains unvalidated and
  unavailable, and tinyVAST does not expose its conditional penalty df.
- Comparisons check observation alignment, weights, response support, common
  covariates, and likelihood targets. An ordinary model may join either
  target, but cannot bridge conditional and marginal groups. Groups are
  constructed in input order; subsets can be compared separately. REML,
  parameter-prior, failed-fit, and unreliable-LOO rankings are withheld.
- Default input order replaces the old automatic brms sorting. Unsupported
  values have explicit notes. No MCMC, null-model fits, or cross-validation
  refits are launched by the reporting function, and only summaries persist.

Review the table definitions and article before release, especially which
prediction target is relevant to the intended CPUE comparison. Shared
vessel/temporal/spatial-fold predictive scoring remains a separate workflow,
not a claimed feature. Issue #12, issues #18/#22, remaining legacy helpers,
the index-median question, and frozen-article review remain open decisions.

Implementation source `3abad0c` is committed and pushed. All 3,788 local tests
pass, GitHub confirms 95.71% coverage, the seven-vignette local CRAN-style
check has zero errors/warnings and the existing NOTE, and the new article is
live with all five tables verified. Ubuntu release and Windows release both
passed, and the temporary validation follow-up is paused. The exact
archive checksum, workflow IDs, and completed platform outcome are in the
matching `cran-comments.md` section. No issue closure or submission is implied.

## 1. Scientific and visual review

- Review the new four-panel residual display, especially automatic fishing-year
  selection, gaps and sample sizes, finite-simulation rank randomisation, and
  the distinction between nominal Q-Q reference bands and predictive ECDF bands.
  Its default spatial simulations condition on fitted fields; unconditional
  glmmTMB and posterior-predictive brms checks answer different questions.
  GitHub issue resolution is now a required pre-release step, as recorded
  below. Remaining helper triage still needs the maintainer's review.
  No Shiny development is planned at this stage.
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
| `table_criterion()` | Extended to six backends and mixed lists; review native likelihood targets, conditional AIC limits, paired LOO differences, and the new model-comparison article. |
| `plot_implied_residuals()` | First effect-scale implementation completed on 12 September; review the worked historical/new comparisons and extension priorities. `plot_grouped_residuals()` separately retains the generalised PIT strata display. See the priority update above for supported families and conditional-interval limits. |
| `plot_predicted_residuals()` | Review residual types and smooths for each intended backend. |
| `influ_residuals()`, `plot(..., type = "qq")` | Supported simulation-based Q-Q workflow, including standalone panels. The native-residual `plot_qq()` helper was explicitly retired on 10 September 2026. |

### Accepted retirements

These eleven names are no longer candidates for restoration. Their frozen
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
| `plot_qq()` | Retire the native-residual Q-Q helper, without a compatibility wrapper. Use `influ_residuals(fit)` followed by `plot(checks, type = "qq")`. The simulation-based workflow supersedes rather than reproduces it; per-point posterior intervals are not required for the first release. Approved 10 September 2026. |

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
Ubuntu-release and Windows-release both passed for 88e14ff. The final
prose-only site build (34413390593) and Pages deployment (34413922390) passed;
the published main, CPUE-index, and spatial articles were checked, including
the corrected support statement, numbered figures, and lightbox zoom.
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
- The package's MIT licence names Darcy M. Webber as copyright holder.
  Bentley's complete original BSD 2-Clause notice is retained in the installed
  validation source. `inst/COPYRIGHTS` and the `Authors@R` comments limit the
  Bentley and Trophia copyright entries to that artefact. Referencing a paper
  does not replace the redistribution notice required for bundled source.
- The current data are simulated. Earlier real-derived data are not part of
  the source archive. No Git-history rewrite was performed; that is a separate
  maintainer decision, not routine CRAN release tidying.

## 5. Resolve every open GitHub issue before release

**Required by the maintainer, 10 September 2026:** work through all open
issues before preparing the final CRAN submission archive. Close each with
a linked implementation/test, or a documented maintainer-approved decision.
Do not bulk-close issues just to empty the tracker. Two issues remain after
resolving #6, #12, #13, and #21 on 10 September; refresh this list before the final
build to catch new issues.

| Issue | Resolution work before closure |
| --- | --- |
| [#18: Two region example](https://github.com/quantifish/influ2/issues/18) | Planning only for now: one four-area model, with areas 1+2 and 3+4 forming two regional indices. See tools/two-region-plan.md. The future separate vignette must demonstrate fixed reference weights, regional labelling, uncertainty, and distinct trends supported by the model. |
| [#22: get_index output](https://github.com/quantifish/influ2/issues/22) | Resolve the assessment-table/lognormal-parameter request together with the deliberately deferred Median/NA decision. Observation dispersion and uncertainty in an annual index are different quantities. |

Issue [#13](https://github.com/quantifish/influ2/issues/13) is closed with the
maintainer's approval. All three original checklist items are complete:
`plot_bayesian_cdi()`/`plot_bayesian_cdi2()` are superseded by
`plot(diagnostic, type = "cdi")`; `get_influ()`/`get_influ2()` are superseded
by `influ()` and diagnostic result extractors; and
`get_coefs_raw()`/`get_coefs()` are removed, with model adapters extracting
their required effects internally. This does not add a replacement public
raw-coefficient API. Commit `08a03f5` removed the approved legacy sources.
The public API tests verify absence from both exports and the runtime
namespace, and preservation of the maintained API (four expectations passed).
The separate remaining legacy review in section 2 and the frozen Get Started
article are unchanged by this closure.

Issue [#12](https://github.com/quantifish/influ2/issues/12) is closed following
the commissioned completion and review. The residual article now executes a
standalone ECDF, a complete-fit Bayesian predictive band, and individual
posterior predictive ECDF overlays using compact saved results. Existing
examples also show empty-pot proportions and the 95th catch percentile (not
the exact maximum). Fitted-data ranks, response ECDFs, LOO-PIT, and LOOIC have
distinct explanations. Universal LOO-PIT and automatic cross-validation
refits remain outside the agreed scope. The frozen article and other legacy
predictive examples remain for later triage; closing #12 does not remove them.

Issue [#6](https://github.com/quantifish/influ2/issues/6) is resolved by the
maintainer-approved retirement of `plot_qq()` and adoption of the unified
simulation-based diagnostic. The residual article now executes a standalone
glmmTMB Q-Q example. The help explains the object type, reuse of stored
results, and distinction between nominal reference bands and posterior
intervals. Tests verify that standalone and overview Q-Q coordinates and
rendered layers are identical, and that the retired function is absent from
the runtime namespace. Native-residual Q-Q plots and per-point posterior
intervals are deliberately outside the first-release API, not claimed as
implemented. The frozen Get Started article and its figures remain unchanged.
Local validation passed 207 focused expectations and all 3,604 full-suite
expectations, with no failures, warnings, or skips, including visual regression.
The residual article and affected help pages rebuilt successfully, and the
standalone figure was visually inspected. The slight expectation-count change
reflects removed native-Q-Q-only checks and added unified-panel parity checks,
not a reduced coverage target.
The exact `aa41200` archive passed macOS CRAN-style checking with zero errors,
zero warnings, and the existing new-submission/optional-tinyVAST NOTE. The
archive path and SHA256 are recorded in `cran-comments.md`. GitHub Ubuntu- and
Windows-release checks (34432837161), coverage (34432837232; 95.57%), pkgdown
(34432837168), and Pages deployment (34433206914) all succeeded. The live
standalone Figure 3, its numbering, and lightbox zoom were verified. The
checked archive is also installed in the Mac's normal user library. No CRAN
or win-builder submission was made.

Issue [#21](https://github.com/quantifish/influ2/issues/21) is closed with the
maintainer's agreement: the four-panel model-neutral overview supersedes the
original brms-only sketch. Its Q-Q, residual-versus-prediction, fishing-year,
and response-adaptive calibration/ECDF panels are documented in the residual
article. The focused four-panel and residual-calibration tests passed all
200 expectations, with no failures, warnings, or skips. This does not claim
that the original zero/max/density-bars/LOO-PIT suite was implemented verbatim;
simulation-rank Q-Q is not LOO-PIT. Targeted predictive checks remain part of
the separate diagnostics and legacy review.

Finish the deferred legacy and median-table review, resolve the two issues
above, then freeze the release API.

## 6. Validate and compile the final reviewed source

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
   Build the actual submission archive from the reviewed, committed source,
   after all issue fixes and legacy decisions. Verify that the tested archive
   is byte-for-byte the archive retained for submission; any later changes
   require a new build and corresponding checks. Earlier candidate archives
   are validation evidence, not automatically the final submission file.
6. Recheck the current [CRAN submission checklist](https://cran.r-project.org/web/packages/submission_checklist.html)
   and [repository policy](https://cran.r-project.org/web/packages/policies.html).
   Submit through the CRAN form and confirm its email only after the maintainer
   authorises the actual submission.

The final CRAN upload remains a separate action requiring explicit approval.
Neither this repository cleanup nor its validation authorises a CRAN or
win-builder submission.

### Repository and coverage review: 10 September 2026

The two historical root PDFs now live unchanged in `tools/references/`.
The old `logo.R` recipe lives in `tools/branding/`; its output remains
`man/figures/logo.png`, and it is never run by installation or checks.
README displays that existing logo, without modifying its image bytes.
The repository map in `tools/README.md` distinguishes development material
from installed runtime material. All of `tools/` is excluded from the CRAN
archive; the validation code, compact fixtures, and required notices remain
under `inst/`. The logo is now included in the archive because package help
also references it. Ignored local output and the frozen review page were
not removed.

The MIT licence holder is corrected to Darcy M. Webber. Upstream copyright
and BSD redistribution terms for `inst/legacy/influ-proto.R` remain intact,
with their scope made explicit in `inst/COPYRIGHTS` and `Authors@R`.
See the [CRAN copyright policy](https://cran.r-project.org/web/packages/policies.html)
and the [BSD 2-Clause terms](https://opensource.org/license/bsd-2-clause).

The unchanged baseline measured 89.39% line coverage. Adding 178 expectations
raised local coverage to 91.10%, without coverage exclusions or runtime API
changes. Coverage is 100% for core object handling and bubble plots, 99.20%
for family handling, and improved for residual trial/component adapters.
The tests check numerical denominators, native simulation agreement, factor
success coding, trial versus case weights, malformed objects, summaries, and
family/link transformations. All 3,287 local expectations in 194 test blocks
passed, with no failures, warnings, or skips, including visual regression.
These percentages measure executed lines, not scientific correctness or
complete backend/family coverage; the latter still needs targeted checks.

All six vignettes rebuilt. The candidate archive contains the logo, copyright
scope, frozen validation source, and current articles, but no `tools/` files,
historical PDFs, local Rplots output, or website directory. The logo and both
moved PDFs were verified byte-for-byte unchanged. The local README/homepage
layout was visually checked. The final issue/legacy/median decisions remain
release gates, and this candidate is not the final submission archive.

The local candidate for commit `8acba31` passed macOS R 4.6.1
`R CMD check --as-cran --no-manual`: zero errors, zero warnings, and one NOTE
for the new submission and optional tinyVAST repository. All six vignettes
rebuilt. The complete optional-dependency library was used after an initial
default-library attempt could not find DHARMa. Archive details and the
check boundary are recorded in `cran-comments.md`. GitHub coverage confirmed
91.10% in run 34415280040. The published GitHub README logo was visually
checked, and the outdated brms-only About description was brought into line
with the model-neutral package.

GitHub Ubuntu-release and Windows-release both passed for `8acba31` in run
34415280059. Coverage (34415280040), pkgdown (34415280055), and final Pages
deployment (34415852624) passed as well. The published homepage's logo and
maintainer details were verified. The follow-up validation-record commit
contains no packaged-source changes and does not need another matrix run.

### Logo refinement and diagnostics decision: 10 September 2026

Following the repository review, the maintainer approved simplifying the
existing logo. The updated native R recipe removes miniature axis labels,
ticks, the rectangular plot frame, and dotted grid, while preserving the
hexagon, blue/orange palette, and iris-based bubble motif. It regenerates
the canonical package-help/README/site logo and all nine website icon files.
Two consecutive runs produced identical checksums; the full-size logo and
180-pixel icon were visually inspected. The frozen legacy article is unchanged.

This branding-only increment does not change statistical code, coverage
targets, or coverage exclusions. Local pkgdown assets, the homepage, and
package-help page were rebuilt. The prior candidate archive and checks above
remain evidence for their recorded source, which contains the older logo;
the final release archive must be rebuilt after all remaining decisions.
Issue #21 was closed as the accepted four-panel replacement described above.

Publication: source commit `9422beb` contains the refined artwork. The full
pkgdown run 34417685025 was deliberately cancelled during unusually slow
Ubuntu system-dependency downloads, before building the site. For this
image-only change, website commit `a72d8c7` instead replaces exactly eleven
logo/icon assets; no HTML, scientific figures, or frozen article changed.
Pages deployment 34418589606 passed. The live logo, reference logo, favicon,
32-pixel icon, and 180-pixel touch icon match the local assets by SHA-256.
The public homepage and GitHub README were visually checked. GitHub renders
the cancelled pkgdown workflow badge as failing; this is not a failed site
build or a failed Pages deployment. The next full documentation build remains
necessary after substantive source/article changes, not solely to recolour
the badge.

### Plain logo frame and bounded coverage increment: 10 September 2026

Following the next visual review, the logo again has a plain rectangular
frame around its bubbles, with no axis labels, ticks, or grid. The hexagon,
bubble composition, and colours remain unchanged. The native recipe produces
the package/README logo and website icons reproducibly; the 180-pixel version
was visually inspected. The frozen legacy article remains unchanged.

Added 189 expectations in 21 test blocks covering comparison rescaling and
overlapping years, stored step contracts, convergence gates, weighted
references, singular joint draws, missing versus zero catches, and native
prediction contracts. Local coverage increased from 91.1037% to 92.9085%,
about 1.8 percentage points, without exclusions or threshold changes. Stop
at this bounded improvement rather than pursue 95% or 100%.

The added tests exposed a brms distributional-formula bug: `part$re` could
partially match the character `resp` field when random effects were absent.
The reference-predictor adapter now uses an exact lookup. Regression tests
use brms's real formula parser, and check the end-to-end annual reduction
with posterior predictions mocked at the native boundary. They do not claim
to run new MCMC fits. The full local suite passed all 3,476 expectations in
215 blocks, with no failures, warnings, or skips, including visual regression.
Coverage and test artefacts are in
`/private/tmp/influ2-framed-coverage.Mizg6S/`. The final release archive must
still be rebuilt and checked after the deferred release decisions.

Validation for source `0c76b40`: macOS R 4.6.1 `R CMD check --as-cran
--no-manual` passed with zero errors, zero warnings, and one NOTE for the
new submission and optional tinyVAST repository. All six vignettes rebuilt.
The source archive, checksum, and check boundary are recorded in
`cran-comments.md`. GitHub coverage run 34420248023 confirmed 92.91% and
all 3,476 expectations passing; the live badge rounds to 93%. Full pkgdown
run 34420248014 and Pages deployment 34420762929 passed. The live framed
logo and representative icons match the local assets, and the public homepage
was visually checked. The earlier cancelled pkgdown run is now superseded.
Ubuntu-release and Windows-release both passed for the same source in
R-CMD-check run 34420248025. The follow-up validation-record commit changes
only excluded development records; no additional matrix run is needed.

### Original logo restoration and 95% coverage target: 10 September 2026

The maintainer increased the coverage target to 95% after the previous tests
found a brms bug. This supersedes the earlier bounded-coverage stopping point.
Added 129 expectations in eight new blocks and extended existing spatial
and residual tests. The complete local suite passes 3,605 expectations in 223 blocks, with
zero failures, warnings, or skips, including visual regression. Instrumented
coverage is 95.5641%, up from 92.9085%, without exclusions, threshold changes,
or runtime dependencies. Stop at this target rather than pursue 100%.

The tests exposed another bug: `model.matrix.glmmTMB()` ignores `newdata`.
Explicit reference grids for fixed-effect influence could therefore fail
dimension checks, or silently reuse the observed design when row counts
matched. Both conditional and zero-component designs now use the native
prediction setup. Tests compare numerical predictions for reordered,
equal-sized, and smaller grids, polynomial bases, weighted random-effect
contrasts, and combined zero-inflated influence. The separate `cpue_index()`
expected-response prediction path was not affected and is unchanged.

Further checks cover native GAM/spatial preview contrasts, response-specific
tinyVAST weights, and disk-retained joint draws across backends. brms residual
tests mock only the native posterior boundary, then verify draw selection,
known trial counts, batching, calibration, and combined-versus-encounter
response routing end to end. They do not claim to run MCMC.

The logo again uses its original hexSticker/Aller design, ticks, dotted grid,
frame, bubbles, and palette. Only numeric axis labels are hidden; rendering
with and without numbers was compared to confirm that every other pixel is
unchanged. Current-renderer spacing is explicitly matched to the historical
layout. Two consecutive logo/icon builds were byte-identical. The frozen
legacy article is unchanged. Development renderers are not runtime package
dependencies. Validation artefacts: `/private/tmp/influ2-coverage95.YAMSA3/`.

The first GitHub coverage run (34422890618) confirmed 95.56% and uploaded
successfully; the badge rounds to 96%. It exposed a platform-sensitive
convergence warning in an older residual fixture, which fitted a zero-inflated
NB mixed model to a small Poisson sample. The follow-up simulates the intended
NB, zero-inflated, grouped structure and asserts optimiser success and a
positive-definite Hessian. It does not suppress the warning. Final local
instrumented tests pass all 3,605 expectations without warnings or skips and
retain 95.5641% coverage. The first full site build (34422890536) and Pages
deployment (34423363142) passed; the live logo, reference logo, and touch icon
match the reviewed files by SHA-256.

Final source `5b28122` is confirmed by GitHub coverage run 34423538830:
95.56%, all 3,605 expectations passing, and no warnings or skips. The final
macOS CRAN-style archive check passed with zero errors, zero warnings, and
the existing NOTE; all six vignettes rebuilt. Archive details are recorded
in `cran-comments.md`. Ubuntu-release and Windows-release both completed with
Status: OK and all 3,605 expectations passing without warnings or skips in
run 34423538829. The final pkgdown run 34423538846 and Pages deployment
34423891385 passed, publishing website commit `903c509`. The live logo matches
the reviewed asset by SHA-256, and the badge shows 96% (rounded from 95.56%).
These checks apply to packaged source `5b28122`, not unverified later code;
subsequent validation-record commits change only excluded bookkeeping files.
The completed follow-up is paused. No CRAN or win-builder submission was made.

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
