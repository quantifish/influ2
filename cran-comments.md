## Gamma(log) implied-effect extension: 12 September 2026

Source `39bb732723c9c0b08729fa4ee011153ea55da919` adds Gamma(log)
likelihood-based implied effects to the existing GLM/GAM/glmmTMB interface,
with native fitted dispersion, independent density/profile tests, a worked
vignette, and missing-year plot-gap protection. No model refits or posterior
simulation are required. The N09 and IV01 frozen studies are unchanged.

Final archive: `/private/tmp/influ2-gamma-check.f3EqwG/final/influ2_1.1.0.tar.gz`.
SHA256: `a7a34833f98a1cdd7b968d184351ec9cd01bb5e455f0a8b248e23a57d238830b`.
All 168 tracked shipped files match the checkout, apart from R-generated
DESCRIPTION metadata. The Mac arm64 R 4.6.1 `--as-cran --no-manual` check
completed with zero errors, zero warnings, and one existing incoming NOTE
for a new submission and optional tinyVAST from the declared additional
repository. DHARMa is not locally installed; `_R_CHECK_FORCE_SUGGESTS_=false`
was used. No full optional-dependency or PDF-manual check is claimed.
All nine vignettes built and rebuilt; CRAN-mode tests passed 5,984 assertions
with seven intentional visual-test groups skipped. Full local tests pass
6,000 assertions, without failures, warnings, or skips. Coverage is 95.97484%.
Minimal-installation checks pass all 12 saved-result contracts and 19
optional-backend guards. The final archive is installed and independently
reproduces the downstream Gamma results. All 84 local plotted-image/lightbox
caption checks pass. Numerical Gamma agreement is not an interval-coverage
calibration or a formal model-acceptance test.

GitHub coverage 34670537513 passed all 6,000 assertions without failures,
warnings, or skips and reported 95.97% coverage. R-CMD-check 34670537492
passed on Windows release; Ubuntu release had 5,999 passing assertions and
one failure in the existing NB2 optimiser-coordinate agreement test. All
Gamma tests passed. pkgdown 34670537460 and Pages 34670940080 passed, with
87 published figure/lightbox checks and the live Gamma section verified.
A subsequent test-only correction checks the independent NB2 score root
with an explicit absolute error bound, retains density/profile checks, and
adds analytic zero/near-zero regression cases. Its targeted suite passes
500 assertions, and the full local suite passes 6,027 assertions, both
without failures, warnings, or skips. For corrected source
`869ad140b656b9bfaff7e34ae986c29e797ed929`, R-CMD-check 34673040569
completed with `Status: OK` on both Ubuntu release and Windows release;
each passed 6,027 assertions, with no failures, warnings, or skips.
Coverage run 34673040566 also passed all 6,027 assertions and reported
95.97% coverage. pkgdown 34673040572 passed its nine-page/87-image
lightbox checks, plus caption/alt and keyboard regressions. Pages run
34673435026 successfully deployed `cb2fe9abe0dc229c2bd7b7b488d1e04fb158238a`,
whose deployment commit identifies the corrected source above.
No production calculations or frozen study results change. The archive
above predates this test-only correction and must be rebuilt before any
future submission. No CRAN or win-builder submission has been made.

## IV01 residual-implied-effect validation: 12 September 2026

Source `2989db942f7aa1e9a61af45d2b1e17c3232bd835` adds a compact frozen
400-fit NB2 glmmTMB validation, portable result audits, and worked figures
and interpretation in the existing implied-effects vignette. No diagnostic
algorithm, model-family support, or default changes. The original N09 study
and its hashed source files remain untouched.

Checked archive: `/private/tmp/influ2-iv01-check.KUhmnd/influ2_1.1.0.tar.gz`.
SHA256: `fa4cd5c2cd650719a2b67c11e572cc82f1e46c12dd7c89c3f39b87a34ad52174`.
All 167 tracked shipped files match the checkout, alongside R's generated
DESCRIPTION metadata. Developer study sources and private review material
are excluded. The IV01 RDS is 415,808 bytes and retains no fitted models,
observation arrays, or simulation arrays.

The macOS arm64 R 4.6.1 check used `--as-cran --no-manual` and
`_R_CHECK_FORCE_SUGGESTS_=false`. It completed with zero errors, zero warnings,
and one existing incoming-feasibility NOTE for a new submission and optional
tinyVAST through its declared additional repository. DHARMa was unavailable
locally; this is not a full optional-dependency or PDF-manual check. Installed
size is 5.9 MB, including 3.9 MB of documentation (INFO). All nine vignettes
built and rebuilt. CRAN-mode tests passed 5,717 assertions, with seven
intentional visual-test groups skipped. Full local tests passed 5,733
assertions, with no failures, warnings, or skips; coverage is 95.95278%.
The isolated minimal-installation check passed its 12 saved-result contracts,
fresh calculations, and 19 missing-package guards. The checked archive is
installed in the usual Mac R library; its exact frozen result and
fresh-session plotting have been verified.

The separate study developer checks pass 66 assertions. Package tests add
399 frozen-result assertions without running the Monte Carlo fits. New
figures, their gaps and target overlays, and complete lightbox captions were
visually checked; all 83 plotted-image caption checks across nine local
articles pass. Scientific conclusions remain explicitly bounded: conditional
local adjustments are not calibrated interaction tests or regional indices.
Check log: `/private/tmp/influ2-iv01-check.KUhmnd/influ2.Rcheck/00check.log`.

GitHub coverage run 34666609234 succeeded with 5,733 passing assertions and
95.95% coverage. Website run 34666609377 passed 86 lightbox/figure-caption
checks; Pages run 34667026970 published revision
`356feec619247658c95a74e53a7213c194a7c848`. The live IV01 section, Figures 4
and 5, and both full expanded captions were verified in the browser.
R-CMD-check run 34666609202 passed on Ubuntu release and Windows release.
Each reported `Status: OK` and 5,733 passing assertions, with zero failures,
warnings, or skips. This validation follow-up is complete. These results apply to the source
revision above, not a later package change. No CRAN or win-builder submission
was made. Detailed methods and the separate read-only BNS Gamma-support
diagnosis are in `tools/release-review.md`.

## N09 PIT reference-grid correction: 12 September 2026

Source `b027b73c01e701bfcfe209b0de316e9e390dba75` corrects only the optional
PIT ECDF and difference plots' horizontal reference grid. bayesplot's interval
calculation, stored ranks, residual algorithms, and default panels are unchanged.
The original N09 study artefact and hashed study sources are intact.

Checked archive: `/private/tmp/influ2-pit-check.x10eZ7/influ2_1.1.0.tar.gz`.
SHA256: `48bbf025177ab7d19b22aa5e60eb74d9b7de09f209410aec942a296b5a611eb7`.
All 165 tracked package files alongside R's generated DESCRIPTION metadata
match the checkout. Private review files and developer scripts are excluded.

The macOS arm64 R 4.6.1 source-archive check used `--as-cran --no-manual` and
`_R_CHECK_FORCE_SUGGESTS_=false`: zero errors, zero warnings, and one existing
incoming-feasibility NOTE for a new submission and optional tinyVAST through
its declared additional repository. DHARMa was unavailable locally; this is
not a complete optional-dependency or PDF-manual check. All nine vignettes
built and rebuilt. CRAN-mode tests passed 5,318 assertions, with seven
intentional visual-test groups skipped. The full local suite passed 5,334
assertions with no failures, warnings, or skips. Local coverage is 95.95278%.
The isolated minimal-installation check passed its 12 saved-result contracts,
fresh core calculations, and 19 missing-package guards.

The independent-uniform control now reproduces 477 crossings in 10,000
samples (4.77%), versus 1,246 (12.46%) with the original misaligned display.
This is not evidence of fitted-model residual calibration. The corrected
article figures and visual snapshot were reviewed; 81 local lightbox/figure
caption checks across nine articles passed. The expanded N09 Figure 6 caption
was also checked in the browser and correctly identifies the plotting fix.
Full check log: `/private/tmp/influ2-pit-check.x10eZ7/influ2.Rcheck/00check.log`.
The checked archive is installed in the usual Mac R library; a fresh-session
PIT plot verified the corrected grid and unchanged stored diagnostic.

GitHub coverage run 34660520149 passed all 5,334 assertions and reported
95.95%. Website run 34660520151 passed all 84 lightbox/figure-caption checks.
Pages run 34661011378 published website revision
`9bbaf9c152a42f6a1e2b5e8926cd15673f85eabe`. Live browser inspection verified
the corrected residual article's Figure 16 and its full lightbox caption,
and the updated N09 correction explanation and Figure 6 caption.
R-CMD-check run 34660520086 passed on both Ubuntu release and Windows release:
each reported `Status: OK` and 5,334 passing assertions with zero failures,
warnings, or skips. These checks apply to source
`b027b73c01e701bfcfe209b0de316e9e390dba75`; subsequent validation-record changes
are excluded from the package. Detailed evidence is in `tools/release-review.md`.
No CRAN or win-builder submission was made.

## Residual-implied effects and first N09 study: 12 September 2026

Source `e9ed7f279055c5cf75f6dd4e33016ee82bc1dd03` restores an effect-scale
residual-implied diagnostic, separates the unchanged grouped PIT display as
`plot_grouped_residuals()`, and adds worked historical/new comparisons.
The first N09 study is a compact frozen NB2 glmmTMB/sdmTMB validation artefact;
its model fits are not rerun during checks or vignette builds. Unsupported
implied-effect cases fail explicitly; no PIT calculation or conditioning
default changed.

Checked archive: `/private/tmp/influ2_1.1.0.tar.gz`.
SHA256: `645dfd53317b5b743a8640931b9d85d765cb845436dc185fbd32e9cfb95c21b9`.
Verify this hash before reusing the temporary path. All 165 repository files
shipped alongside R's normal DESCRIPTION metadata match the source checkout.
Private review attachments and developer study scripts are excluded. The
checked archive is installed in the usual Mac R library; a fresh-session
implied-effect calculation and plot passed.

The local macOS arm64 R 4.6.1 source check used `--as-cran --no-manual` and
`_R_CHECK_FORCE_SUGGESTS_=false`. It finished with zero errors, zero warnings,
and one incoming-feasibility NOTE: new submission, optional tinyVAST through
its declared additional repository, and a study-source URL temporarily 404
before the source was pushed. The URL now resolves, independently verified
after publication; the check log has not been altered. The first strict
attempt stopped on absent optional DHARMa. No full local optional-dependency
or PDF-manual check is claimed. All nine vignettes built and rebuilt.
CRAN-mode tests passed 5,218 assertions, with seven intentional visual-test
groups skipped; the full local suite passed 5,234 assertions with zero
failures, warnings, or skips. Local coverage is 95.95%. The existing isolated
minimal-installation/core/saved-result check passed.

Ubuntu release and Windows release both passed R-CMD-check run 34651733821:
each reported `Status: OK` and 5,234 passing assertions without failures,
warnings, or skips. Coverage run 34651733817 passed and uploaded 95.95%.
Website run 34651733809 passed all 84 plotted-image/lightbox caption checks
across nine articles. Pages run 34652395977 published website revision
`1afeba237957299dd9a735cc8470122a10921a26`. Live browser inspection verified
the three new comparison figures and their ordered seasonal facets, all six
N09 figures, and the updated residual article. All 27 expanded captions on
these three pages match their complete numbered visible captions.

Detailed evidence and limitations are in `tools/release-review.md` and
`/private/tmp/influ2-implied-check.WPyb0h/influ2.Rcheck/00check.log`.
Only excluded validation records change after this source revision. These
checks do not establish universal implied-effect interval calibration or
validate later package changes. No CRAN or win-builder submission was made.
Scientific interpretation, additional adapters, and the parked regional,
median-column, and legacy reviews remain separate decisions.

## Explicit residual conditioning: 11 September 2026

Source `02573236c77ec700a42d9d99da11640f3ade3a04` adds explicit supported
conditioning targets to `influ_residuals()`. All existing backend defaults
remain unchanged. New independent native objectives protect the original
fits. A single joint approximate conditional latent draw can be shared across
sdmTMB/tinyVAST response simulations; matching probability calculations,
delta components, reproducible batching, and ML/non-REML guards are tested.
This is not a universal fitted-model calibration claim or a change to defaults.

The full local suite passed 4,525 expectations with no failures, warnings,
or skips. The macOS arm64 R 4.6.1 source archive passed
`--as-cran --no-manual` with zero errors, zero warnings, and the existing
incoming-feasibility NOTE (new submission and optional tinyVAST available
through its declared repository). All seven vignettes built and rebuilt.
CRAN-mode tests passed 4,511 expectations with six intentional visual-test
groups skipped; those passed in the full suite. Optional DHARMa was absent
locally, so its article branches were skipped. The PDF manual was not checked.

Checked archive:
`/private/tmp/influ2-conditioning-check.4hllQr/influ2_1.1.0.tar.gz`.
SHA256: `8458bee96eecb42751b9b70e75ab1f9251bdcddbc154dc640829e4f7b06ddbba`.
The checked archive is installed in the usual Mac R library. Local and GitHub
coverage are 95.98%; the new conditioning file has 95.92% coverage. The three
new spatial examples were visually inspected, and all 71 local plotted-image
caption checks on seven pages passed. GitHub's optional-dependency-complete
website build passed all 74 plotted-image checks across those seven pages.

Ubuntu release and Windows release both passed PR run 34543811601. PR #26
was merged as `d230d6861e40996c2a53419bedcd7c07833f7b54`, with an identical
source tree. Post-merge coverage run 34545318736 and website run 34545318608
passed; Pages run 34545915567 published the new examples. Direct public-site
checks verified the spatial comparisons, support table, and updated help.
The automatic post-merge package-check rerun 34545318590 subsequently passed
both release platforms, verified during the later website tidy-up. These
archive/check results predate the subsequent CDI-label-only presentation
change; the eventual release candidate needs its own final archive check.
Detailed validation and publication records are in
`tools/release-review.md`. No CRAN or win-builder submission was made.
N09 calibration review, issue #18, the remaining legacy review, and the
index-median decision remain separate from this increment.

## Shared residual engine and external inputs: 11 September 2026

Source `09971c240d2031aa824d94364bfea8babd650ff1` extracts the existing
simulation-rank calculation into one shared engine and adds the documented
`as_influ_residuals()` matrix constructor. Existing fitted-model calculations,
native conditioning, RNG preparation order, and plotting defaults are unchanged.
Eighteen frozen pre-refactor cases matched exactly on the Mac; portable tests
allow only `1e-12` floating-point roundoff. Independent calculations and an
actual glmmTMB simulation replay test the new route separately.

The full local suite passed 4,378 expectations with no failures, warnings,
or skips. The final macOS arm64 R 4.6.1 archive check used
`--as-cran --no-manual` and passed with zero errors, zero warnings, and the
existing incoming-feasibility NOTE (new submission and optional tinyVAST
available through its declared repository). All seven vignettes built and
rebuilt. CRAN-mode tests passed 4,364 expectations, with six intentional
visual-test groups skipped; those passed in the full suite. Optional DHARMa
was absent locally, so its article examples were skipped. This is not a
PDF-manual or full optional-dependency validation.

Checked archive:
`/private/tmp/influ2-external-final-a8722fd02f42/influ2_1.1.0.tar.gz`.
SHA256: `023f373b4fd3a49987c8ab67fafdda5f1c5c2617a8ecfa9c864760882bc64cb3`.
Archive size: 2,641,331 bytes. All 148 tracked package files present alongside
DESCRIPTION match the committed checkout byte-for-byte. Original DESCRIPTION
fields match after normalising build whitespace; R added only its normal
generated fields. Private review material and developer recipes are excluded.
The exact checked archive is installed in the Mac's usual R library; a fresh
R session verified the exported constructor, RNG preservation, and overview.

An additional installed-package check reused the existing complete negative-
binomial brms fit at `/private/tmp/influ2-four-panel.6ZEXTO/brms-native.rds`
(60 observations, formula `catch ~ year + x`). Twenty-four joint posterior
predictive response vectors were transposed explicitly and passed through the
constructor. Independently reconstructed PIT values, predictive means, and
ECDF quantiles agreed; input/RNG preservation and plotting passed. No MCMC or
refit was run, and the model was not added to the package archive. This tests
the bridge, not model adequacy or posterior convergence.

GitHub coverage run 34536328637 passed all 4,378 expectations and reported
95.90% coverage, including 100% for the external constructor and shared engine.
Website run 34536328492 passed, including all 71 lightbox figure-caption checks
across seven articles. Pages run 34536914963 published website revision
`5b2568801104d45f9771806aeb7490fa89c4a148`. Live browser review confirmed the
new section, executed lobster example, Figure 18, and its complete expanded
caption. The existing deployment-action Node-runtime annotation remains
non-blocking; no workflow or platform expansion was made.

R-CMD-check run 34536328498 passed both Ubuntu release and Windows release.
Only the excluded validation records were updated after this source revision;
the checked package contents and published examples are unchanged.
No CRAN or win-builder submission was made. Simulation counters, spatial
conditioning changes, further bridges, issue #18, legacy review, and the
index-median decision remain for later consideration.

## Optional PIT plots and caption consistency: 11 September 2026

Source `0abbab1` adds `pit_ecdf` and `pit_ecdf_diff` displays of stored residual
PIT values, with optional bayesplot >= 1.16.0, and configurable four-panel
layouts. Default panels, residual calculations, simulation conditioning, and
grey calibration bars are unchanged. Help and the article distinguish
simultaneous iid-uniform reference limits from calibrated fitted-model tests.
Expanded website figures now use their complete numbered visible captions;
all 67 locally rendered plotted images on seven pages passed the shared-script
caption regression.

Local full-suite validation passed 4,037 expectations with zero failures,
warnings, or skips, including the new PIT numerical/visual tests. macOS arm64
R 4.6.1 `R CMD check --as-cran --no-manual` passed with zero errors, zero
warnings, and the existing incoming-feasibility NOTE (new submission and
optional tinyVAST available from its declared repository). All seven vignettes
built and rebuilt. Optional DHARMa was absent from this local library, so its
vignette chunks were skipped; this is not a full optional-dependency or PDF-
manual validation. GitHub's dependency-complete checks remain a separate gate.

Checked archive:
`/private/tmp/influ2-pit-check-7fd637c9b3e8/influ2_1.1.0.tar.gz`.
SHA256: `8394be2b7f5092c5a457bf7aa57cb541ce9a6246613b64684579d3a94ec829c8`.
The checked archive is installed in the Mac's usual R library, and a fresh
R session verified the new plotting interface.

GitHub coverage run 34529978852 passed all 4,037 expectations and reported
95.75% coverage. Website run 34529979017 passed, including its new caption
check for all 70 plotted images across seven pages (with the optional DHARMa
examples present). Pages run 34530659902 published website revision
`2a1ccb8ace7ac4c9c53eeaf064dbf74a9f57432d`. Browser checks of all seven live
articles found zero caption mismatches; the residual article has 17 figures,
including both new PIT examples. Expanded Figure 8 displays its full caption.
R-CMD-check run 34529978869 passed both Ubuntu release and Windows release.
The subsequent changes to this excluded validation record do not alter the
checked package source or require another website build.

No CRAN or win-builder submission was made. Issue #18, legacy review, and
the index-median decision remain parked.

## Standalone and Bayesian ECDF examples: 10 September 2026

Source `225871b65925e2ef3de6ca8188d28015b21fdab6` adds the documented
standalone glmmTMB response ECDF, a complete-fit brms posterior predictive
band, and twenty individual posterior predictive ECDF curves. These are
Figures 4-6 in the residual article. The reproducible preparation script uses
the existing converged Gaussian fit from the mixed-model comparison. The
13,836-byte result contains compact diagnostic/curve summaries, not a Stan
model or an observation-by-draw matrix. No MCMC runs during preparation or
rendering. LOO-PIT is distinguished from fitted-data ranks and LOOIC, and is
not claimed as an implemented universal diagnostic.

All 3,934 full-suite expectations passed locally and in the GitHub coverage
job, without failures, warnings, or skips; 111 cover the new examples and
standalone/overview ECDF parity. Coverage remains 95.72%. Independent native
brms replay verified the saved ECDF quantiles, ranks, predictive means, and
twenty curves; the pp_check recipe executes on the same complete fit. The
tracked generator reproduces the stored result exactly. All three new
rendered figures were visually reviewed, with numbered captions and
larger-view controls confirmed in the browser.

Checked archive:
`/private/tmp/influ2-ecdf.FJccuJ/final/influ2_1.1.0.tar.gz`.
SHA256: `e799514d7f681ba0b7e66ea93a744c4e7a4b8240bac0657cb67f4581c457c0c7`.
Archive size: 2,580,953 bytes. All 134 source code, help, test, vignette, and
installed-data files match the checkout byte-for-byte; generated inst/doc
outputs were checked separately. macOS arm64 R 4.6.1
`R CMD check --as-cran --no-manual` passed with zero errors, zero warnings,
and the existing incoming-feasibility NOTE (new submission and optional
tinyVAST from the declared repository). All seven vignettes rebuilt, including
the optional DHARMa examples. CRAN-mode tests passed 3,921 expectations with
five intentional visual groups skipped; those passed in the full suite.
The exact checked archive is installed in the Mac's usual R user library.
The separate PDF-manual validation remains deferred; it was not run here.

GitHub coverage run 34450598898 and pkgdown run 34450598822 passed.
Pages run 34451210154 published website source
`683dcf3a59af7740e00ad3bbd168f5d4060cb1dc`. All fifteen live residual-article
captions match the reviewed local figures, and the new sections are present.
R-CMD-check run 34450598790 passed both Ubuntu release and Windows release.
Issue #12 is closed as completed, with the agreed scope and validation linked
in comment 5615100852. No further validation follow-up is running.

Issue #18 is planning only: `tools/two-region-plan.md` records one model for
four statistical areas, aggregated into two regional reference populations.
No new regional vignette or API is implemented. The remaining legacy source,
frozen article, Median/NA definition, BNS files, DESCRIPTION, and namespace
are unchanged. No CRAN or win-builder submission was made.

## Generalised residual helpers: 10 September 2026

Source `3837b0ba362d26c04c997dc2160036e9081b59b3` replaces the maintained native
residual helpers with the shared simulation-based rank engine. Grouped means
are no longer added to link-scale coefficients. The residual article explains
the changed interpretation, fisheries-report context, dependence caveats,
component selection, and BNS migration boundary. The model-comparison article
now contains one executed table spanning all six supported backends, using a
complete existing brms fit without MCMC during rendering.

Final archive: `/private/tmp/influ2-general-residuals.DHp8R1/final/influ2_1.1.0.tar.gz`.
SHA256: `fc1bb03baabbad888103bc0b9eb35f09706897609a85244d07aecca9fb5533f6`.
The archive matches 124 code, help, test, vignette, namespace, and example files
byte-for-byte; original DESCRIPTION values match after build whitespace
normalisation. It is installed in the Mac's usual user R library.

macOS arm64 R 4.6.1 `R CMD check --as-cran --no-manual` completed with zero
errors, zero warnings, and one NOTE (new submission and optional tinyVAST
available from its declared additional repository). CRAN-mode tests passed
3,810 expectations with five intentional visual groups skipped. The full
local suite and GitHub coverage run passed 3,823 expectations with no failures,
warnings, or skips. Local and GitHub coverage are 95.72%. The redesigned visual
baseline and rendered article figures were inspected, and a complete existing
brms model passed both helper workflows. Native sdmTMB positive-component
tests verify that combined residuals are not substituted.

A preliminary manual-enabled local attempt failed because pdflatex was absent
from the process PATH, not because a LaTeX compilation error was established.
The final check uses the same documented --no-manual convention as earlier
increments; no PDF-manual validation is claimed. An initial namespace NOTE
for unqualified utility functions was corrected and is absent from the final
check. No test or coverage exclusion was added to obtain a passing result.

GitHub coverage run 34444319123 passed for the stated source. R-CMD-check
34444319165 passed both Ubuntu release and Windows release with Status: OK;
each platform passed 3,823 expectations without failures, warnings, or skips.
The pkgdown run 34444319146 and Pages deployment 34444849221 also passed,
publishing website commit `b57d6c10328cff7158b6d14c25898a556797e3eb`.
All five live model-comparison tables and all twelve residual-article captions
match the reviewed local pages. Table 5 includes all six backends, and the
grouped residual text explicitly describes departures, not adjusted coefficients.
The scoped follow-up is complete and paused. These excluded bookkeeping changes
do not change the checked source. The three earlier f432d95 runs were deliberately
cancelled after the namespace fix to avoid duplicate Actions usage. No issues,
BNS assessment files, or frozen legacy figures were changed. No CRAN or
win-builder submission was made.

## Model-specific comparisons: 10 September 2026

`table_criterion()` now supports GLM, GAM, glmmTMB, brms, sdmTMB, and tinyVAST,
including mixed lists. Native likelihood criteria, deviance, sample size,
penalty degrees of freedom, conditional-AIC methods, and Bayesian predictive
criteria have distinct definitions and columns. Observation/target checks
prevent inappropriate differences and rankings. The ordinary-model case is
tested against both conditional and marginal targets without bridging them.
The new Model comparison article contains five numbered tables. The executed
Bayesian example reuses a complete four-chain fit (4,000 retained draws,
maximum R-hat 1.00484) and stores only a 1,276-byte table/metadata fixture.
No MCMC is run by the reporting function or during vignette rendering.

All 3,788 local expectations pass without failures, warnings, or skips,
including visual regression; 184 specifically test the new comparison
methods. Instrumented package coverage is 95.71%, without changing exclusions
or thresholds. Tests cover native values/df across all six backends, a real
tinyVAST random-field cAIC example, cached LOO, paired ELPD differences,
old/new loo result formats, reversed/interleaved model order, data mismatches,
failed/REML/prior fits, and explicit unsupported-statistic boundaries.

Checked archive:
`/private/tmp/influ2-model-comparison.e7ymTb/final/influ2_1.1.0.tar.gz`.
SHA256: `023b93c1fe9b4787154628f0d83bb482a5c44a7c19cf8a92cf4b25203d7ff5e1`.
macOS arm64 R 4.6.1 `R CMD check --as-cran --no-manual` completed with zero
errors, zero warnings, and the existing incoming-feasibility NOTE: new
submission and optional tinyVAST available from its declared repository.
All seven vignettes rebuilt with the existing optional libraries, including
DHARMa. CRAN-mode tests passed 3,775 expectations; five visual groups skipped
in CRAN mode passed in the full local suite. The archive matches 126 source,
help, test, vignette, and example files byte-for-byte. The pkgdown site also
rebuilt successfully, and the new tables and captions were inspected.

Limitations remain explicit: no mixed-glmmTMB native cAIC, no unvalidated
profiled-spatial conditional penalties, no invented tinyVAST cAIC df, and
no universal LOO-PIT or refitted cross-validation workflow. Bare TMB objectives
are not treated as fitted model classes with known likelihood conventions.
Issue #12 is parked at the maintainer's request. Issues #18/#22, remaining
legacy helpers, the index-median question, and frozen-article review remain
pre-submission decisions. No GitHub issues were changed, and no CRAN or
win-builder submission was made by this increment.

Source commit: `3abad0c872d5e513118d624ea22f06cc366db5be`. GitHub coverage run
34439577567 passed all 3,788 expectations and confirmed 95.71% coverage.
The pkgdown run 34439577595 and Pages deployment 34440184902 passed, publishing
website commit `d56691dd47393737c6800b8b3f4299826050fe20`. All five live model-
comparison tables match the reviewed local tables. The checked archive is
installed in the Mac's normal R user library, and its public interface was
smoke-tested after installation. R-CMD-check run 34439577572 passed both
Ubuntu release and Windows release. The temporary validation follow-up has
been paused because all checks and website deployment have completed. These
excluded bookkeeping changes do not change the checked source.

## Issue #13 closure: 10 September 2026

The maintainer approved closing issue #13 after verifying all three original
legacy-consolidation checklist items. The six listed function names have
been retired; the public API tests verify absence from both exports and the
runtime namespace. The remaining legacy-function and median-table review
is unchanged, and the frozen Get Started article remains available.

Three GitHub issues remain: #12 (additional predictive diagnostics), #18
(two-region example), and #22 (annual-index output). Issue #12 is the next
selected review; its LOO-PIT scope has not yet been agreed. These are
excluded release-record changes only. The checked package source remains
`aa4120097c3f57e0444475ab7e5daef7d8809dd5`, with its validation results below.
No CRAN or win-builder submission was made.

## Unified Q-Q workflow and issue #6: 10 September 2026

Code revision: `aa4120097c3f57e0444475ab7e5daef7d8809dd5`.

The maintainer approved retirement of the native-residual `plot_qq()` helper,
without a compatibility wrapper. `influ_residuals(fit)` followed by
`plot(checks, type = "qq")` is the supported replacement diagnostic. Issue #6
is closed with the agreed scope recorded: nominal Q-Q reference bands are not
per-point posterior intervals, and those intervals are not required for the
first release. Four GitHub issues and the remaining legacy/median review
remain pre-submission gates. The frozen legacy article is unchanged.

All 3,604 local expectations passed without failures, warnings, or skips,
including visual regression. The 207 focused expectations include identical
standalone/overview Q-Q data and rendered layers, and absence of the retired
function from the namespace. GitHub coverage run 34432837232 passed the same
3,604 expectations and reports 95.57% coverage. No exclusions or thresholds
were changed.

Checked archive:
`/private/tmp/influ2-qq-retirement.ABvVhs/final/influ2_1.1.0.tar.gz`.
SHA256: `fc3b2aa9db13d96ff251c5fba90e5e895803477c98223ff1c5ee9a5880f4b9f6`.
macOS arm64 R 4.6.1 `R CMD check --as-cran --no-manual` completed with zero
errors, zero warnings, and one NOTE (new submission and optional tinyVAST
available from its declared repository). All six vignettes rebuilt, including
optional DHARMa examples. An initial check using the default library stopped
at missing DHARMa; the completed check uses the existing optional libraries.
The archive matches 116 source, help, test, and vignette files byte-for-byte,
excluding generated DESCRIPTION metadata. CRAN-mode tests passed 3,591
expectations; its five skipped visual groups passed in the full local suite.
The checked archive was installed in the Mac's normal user library.

The pkgdown build (34432837168) and Pages deployment (34433206914) succeeded.
R-CMD-check run 34432837161 also completed successfully on Ubuntu-release and
Windows-release. All results apply to source `aa41200`; subsequent changes to
this excluded validation record do not change the checked package. No active
follow-up is needed now that every workflow has completed.
The published standalone Q-Q example is Figure 3, with verified numbering and
working lightbox zoom. Retired help is absent from the reference index, and
its former URL returns 404. This is still a validation candidate, not an
approved CRAN submission. No CRAN or win-builder upload was made.

## 95% coverage target and reference-grid fix: 10 September 2026

Code revision: `5b28122d8f4a806d0d3a571788b653f716101013`.

Local and GitHub coverage are 95.56%, up from 92.91%, with no coverage
exclusions or threshold changes. GitHub run 34423538830 confirms all 3,605
expectations passing without failures, warnings, or skips, including visual
regression. The added numerical tests found and protect a glmmTMB fixed-effect
reference-grid bug: the fitted model-matrix method ignores newdata. Native
prediction setup now supplies the conditional and zero-component designs.
Expected-response `cpue_index()` calculations are unchanged.

An older residual test generated Poisson data but fitted a zero-inflated NB
mixed model, producing a platform-dependent boundary/Hessian warning. Its
replacement simulates the intended grouped, zero-inflated NB structure and
asserts optimiser convergence and a positive-definite Hessian. No warning is
suppressed. Further tests check posterior residual draw identities, trials,
component routing, response-specific weights, and disk retention.

Final validation archive:
`/private/tmp/influ2-coverage95.YAMSA3/final/influ2_1.1.0.tar.gz`.
SHA256: `9cff7549e9d4383c91d05ae02f7890071c9e38ac70f1b2be0e9925f9cb61d359`.
macOS arm64 R 4.6.1 `R CMD check --as-cran --no-manual` passed with zero
errors, zero warnings, and one NOTE for the new submission and optional
tinyVAST repository. All six vignettes rebuilt. The archive contains the new
tests and the restored original-style logo, with only its axis numbers hidden.
The frozen legacy article is unchanged.

The final Ubuntu-release and Windows-release checks (34423538829) both
finished with Status: OK and all 3,605 expectations passing without failures,
warnings, or skips. The pkgdown rerun (34423538846) and subsequent Pages
deployment (34423891385, website commit `903c509`) passed as well. The live
logo matches the reviewed file by SHA-256, and the coverage badge shows 96%
(rounded from 95.56%). These runs validate source `5b28122`; the subsequent
bookkeeping records are excluded from the archive and do not change its
packaged source. The completed follow-up is paused. This is not the final CRAN
submission: issue resolution, deferred legacy review, and the Median/NA
decision remain release gates. No CRAN or win-builder upload was made.

## Bounded coverage and logo review: 10 September 2026

Code revision: `0c76b4043120a810d3eb75cd1203ddabd60fef2c`.

Restored the logo's plain plot frame, without axes, labels, or a grid.
Added 189 test expectations across comparison rescaling, step contracts,
uncertainty, and prediction boundaries. These exposed and now protect a
brms distributional-formula reference-predictor fix: an absent `re` field
must not partially match `resp`. No API, dependency, or coverage-exclusion
changes were needed.

Local and GitHub coverage are both 92.91%, up from 91.10%; Codecov's badge
rounds this to 93%. All 3,476 expectations in 215 local test blocks passed,
including visual regression, with no failures, warnings, or skips. GitHub
coverage run 34420248023 passed the same 3,476 expectations and uploaded
successfully. Formula-parser tests use brms itself, with posterior prediction
and convergence summaries mocked only at their native interfaces.

Fresh validation archive:
`/private/tmp/influ2-framed-coverage.Mizg6S/influ2_1.1.0.tar.gz`.
SHA256: `6a82dfc1ce50c1b83ffe877a6688fe9c9dda35e5630bc9766b4bd691876743e6`.
macOS arm64 R 4.6.1 `R CMD check --as-cran --no-manual` completed with
zero errors, zero warnings, and one NOTE for the new submission and optional
tinyVAST repository. All six vignettes rebuilt. Archive tests passed 3,463
expectations; five visual groups skipped in CRAN mode passed separately above.
The archive contains the updated help logo and all four new test files.

The full pkgdown build (34420248014) and Pages deployment (34420762929)
passed. Live logo/icon checksums match the reviewed assets, and the framed
logo and 93% badge were visually checked on the published homepage. The
previous intentionally cancelled pkgdown run is superseded by this success.
Ubuntu-release and Windows-release both passed for this code revision in
R-CMD-check run 34420248025. Subsequent validation-record edits are excluded
from the package archive and do not alter the checked packaged source.
The frozen legacy article remains unchanged. Five issues, the legacy-helper
review, and the Median/NA table decision remain release gates. This is a
validation candidate, not the final submission archive; no CRAN or
win-builder upload was made.

## Repository and coverage review: 10 September 2026

Code revision: `8acba31427fee405e200a800b5b859af0e1b8e15`.

Added 178 expectations for object contracts, family/link transformations,
bubble proportions, and residual trial/component handling. The full local
suite passes 3,287 expectations in 194 blocks, with no failures, warnings,
or skips. Coverage increased from 89.39% to 91.10%, measured both locally
and by GitHub (coverage run 34415280040). No runtime API or scientific
calculations were changed, and no coverage exclusions were introduced.

Moved the historical PDFs into `tools/references/` and the logo recipe into
`tools/branding/`. Development-only `tools/` material is now excluded from
the source archive. The existing logo is displayed in README and included
in `man/figures/` for installed package help. The logo, moved PDFs, frozen
Bentley source, and frozen Get Started page/figures are unchanged.

The MIT licence holder is corrected to Darcy M. Webber. The original BSD
notice remains with `inst/legacy/influ-proto.R`; `inst/COPYRIGHTS` and the
Authors@R comments distinguish that artefact's copyright from the new
package. The maintainer address remains `darcy@quantifish.co.nz`.

All six vignettes rebuilt. The validation archive is
`/private/tmp/influ2-housekeeping.21P0Vp/influ2_1.1.0.tar.gz`.
SHA256: `0dc48c593022fec911f7bcf2b7384d7ebe176f7051dba72e7ae052d83a137da5`.
macOS arm64 R 4.6.1 `R CMD check --as-cran --no-manual` completed with
zero errors, zero warnings, and one NOTE: new submission and optional
tinyVAST from the declared additional repository. The full optional library
was used, including DHARMa; an initial default-library attempt stopped at
missing DHARMa and is not the completed check. CRAN-mode tests passed 3,274
expectations, with five visual groups skipped that passed in the local suite.

The archive and installed package contents were inspected: the help logo,
required notices, validation code, and articles are included; development
scripts, historical PDFs, website output, and Rplots output are excluded.
GitHub README logo rendering was visually verified. The GitHub About text
and homepage now describe the model-neutral package and use HTTPS.

GitHub Ubuntu-release and Windows-release checks both passed for this code
revision (run 34415280059). Coverage passed (34415280040), pkgdown passed
(34415280055), and Pages deployment passed (34415852624). The deployed home
page was checked for the loaded logo and corrected maintainer details.
Only these validation records were updated after checking the archive; they
are excluded by `.Rbuildignore` and do not alter its packaged source.

All six currently open issues, remaining legacy review, and the Median/NA
table decision must be resolved before the final submission build. This is
a validation candidate, not the final submission archive. No CRAN or
win-builder submission was made.

## Standardised and integrated indices: 10 September 2026

Code revision: `88e14ff9e49c7adf108c0dca51997d2331c0a95e`.

Added standardised expected-response CPUE indices for sdmTMB and univariate
tinyVAST, and a separate area-integration interface for all six supported
backends, including models without spatial effects. Explicit reference areas,
units, catchability conversions, seasonal averaging weights, and joint
parameter/field uncertainty are documented. Median-display and remaining
legacy-function decisions are deferred; this is not submission authorisation.

The full local suite passes 3,109 expectations in 183 blocks, with no failures,
warnings, or skips. Native point-estimate checks cover both spatial backends
and their standard/Poisson-link delta families. A complete existing brms fit
agrees with direct native posterior integration to numerical precision.

Archive SHA256:
`a2ab355aad0653b3a84e657b9b4391e27a33a3081b716470421d44aa1735fec4`.

macOS arm64 R 4.6.1 `R CMD check --as-cran --no-manual` completed with zero
errors, zero warnings, and one NOTE: new submission and optional tinyVAST,
which is available from the declared additional repository. All six vignettes
rebuilt. Archive tests passed 3,096 expectations; five visual groups skipped
in CRAN mode passed in the separate local run. Coverage on GitHub is 89.39%.
GitHub Ubuntu-release and Windows-release checks passed for the same code
revision (run 34412426492), as did coverage (34412426533). The final
documentation-only publication and Pages deployment passed (34413390593 and
34413922390), and the updated live articles were verified.

The complete pkgdown site built, and new figures, caption numbering, and
lightbox zoom were reviewed. The main article subsequently received a
prose-only correction removing an obsolete future-work sentence and was
rebuilt separately; the archive hash above identifies the checked code snapshot,
not that later prose correction. Repeat final-release checks after the deferred
review decisions. No CRAN or win-builder upload was made.

## Legend follow-up: 8 September 2026

CDI proportion legends now use a single column with at most four reference
bubbles. The shared size mapping and all calculated results are unchanged.
The mesh-margin setup and restoration still run but are omitted from the
spatial article's displayed code. Its mesh image is byte-identical to the
preceding build.

All 2,700 local expectations passed with no failures, warnings, or skips,
including ten visual snapshots. All five articles rebuilt; the displayed
mesh code contains only mesh construction and plotting, and the frequentist
and Bayesian CDI figures have unclipped, single-column legends. The source
archive results below refer to their identified earlier archives, not this
follow-up. No new win-builder upload or CRAN submission was made.

## Figure review: 8 September 2026

The four-panel residual examples now use negative-binomial and Poisson
glmmTMB models with monthly random intercepts. Both fits converged with
positive-definite Hessians. The article explicitly describes resimulated
random effects and retains the exploratory calibration cautions.

The shared CDI layout now has horizontal short term labels above the fitted
effects and below the composition, right-hand influence-panel year labels,
and an unclipped two-column size legend. Bentley Figure 1 has no redundant
plot title, and the native spatial mesh plot uses tighter margins. No
influence estimates, interval calculations, simulated data, frozen legacy
source, or frozen Get Started HTML/figures changed.

All 2,678 local expectations passed with no failures, warnings, or skips,
including ten visual snapshots. New tests check axis placement, matching
level order, the actual legend grob, and the shared Bayesian CDI layout.
All five articles rebuilt, and the changed figures and lightbox displays
were inspected in the browser, including the main article's Bayesian CDI.

Archive: `influ2-figure-review-20260908/influ2_1.1.0.tar.gz` (2,077,593 bytes).

SHA256: `9d704ddb274fd8b926a7908bff1095c73145cf3d8b98f919e2c318088a8c1f32`

The clean archive passed macOS arm64 R 4.6.1 `R CMD check --as-cran
--no-manual` with 0 errors, 0 warnings, and 1 note (new submission and
optional tinyVAST availability). All five vignettes rebuilt. Archive tests
passed 2,668 expectations; three visual-test groups were intentionally
skipped in CRAN mode and passed in the separate local run above.

No CRAN submission or win-builder upload is authorised or performed by this
visual-review increment. GitHub issue review, the remaining legacy-helper
decisions, and eventual removal of the frozen review article remain deferred.

## Four-panel residual diagnostics: 7 September 2026

Added the compact `influ_residuals()` calculator and four-panel S3 display,
with automatic fishing-year detection, explicit ambiguity handling, native
simulation adapters, and a revised residual article. This is a reviewed
development increment, not authorisation to submit the package to CRAN.
GitHub issue review and the remaining legacy-function triage are deferred.
The frozen Get Started HTML and its 19 accompanying figures are unchanged.

Archive: `influ2-four-panel-20260907/influ2_1.1.0.tar.gz` (2,059,085 bytes).

SHA256: `dc378ec5eb89d047e5b734d02a999b83affb7f35c5117a0b7fb79b2be2ac9d76`

* macOS arm64, R 4.6.1: `R CMD check --as-cran --no-manual` completed
  with 0 errors, 0 warnings, and 1 note. The note identifies a new submission
  and the optional tinyVAST dependency from the declared additional repository.
* All 2,662 local test expectations passed, including nine visual snapshots.
  Archive tests passed 2,653 expectations; the two visual-test groups were
  deliberately skipped in CRAN mode. All five vignettes rebuilt successfully.
* Executed native checks cover ordinary GLM families, negative-binomial and
  Tweedie GAMs, glmmTMB random effects and binomial/zero-inflated responses,
  sdmTMB spatial/spatiotemporal and delta models, and tinyVAST single-response,
  spatial/spatiotemporal, and delta models. A separate complete brms
  negative-binomial fit exercised posterior prediction (two chains, 1,000
  post-warmup draws in total, no divergences); no Stan object was added to
  the source package or website.
* The website built successfully with optional DHARMa examples. The two new
  lobster displays, numbered captions, and lightbox zoom were inspected.
* The saved 5,049-observation, 250-simulation lobster diagnostic is about 1 MB;
  the full response simulation matrix alone would be about 10 MB. Native
  backends have additional temporary allocations. The result retains no model
  or observation-by-simulation matrix.

The Q-Q ribbon is an independent-uniform reference, not a calibrated test for
estimated, dependent, or posterior-predictive residuals. Spatial defaults use
conditional `mle-eb` simulations and are labelled accordingly; alternative
native residual recipes remain documented. No new win-builder upload or CRAN
submission was performed. Repeat final-release checks after remaining review.

## Partial legacy triage: 7 September 2026

The first ten legacy-helper retirements have been accepted and recorded in
`tools/release-review.md`. Only approved frozen source and help files were
removed; active model code, dependencies, the Bentley validation artefact,
and the frozen review HTML and figures are unchanged. Added namespace tests
protect the maintained interface and the retirement decisions.

The full local test suite passed after these removals, including the four
new namespace expectations and all eight unchanged visual snapshots. A
parsed-source dependency audit found no references to the retired names in
the remaining helper implementations. All 20 frozen article/figure files
match the pre-triage checkout byte for byte, and the retained `plot_index()`
function body is unchanged.

For source commit `08a03f52afa21f2af6db281569885a2c4f53dd50`, GitHub
Ubuntu-release and Windows-release checks passed (run 34081472524), as did
pkgdown (34081472629) and Pages deployment (34081672960). The updated NEWS
entry was verified on the published site.

The remaining assessment-output and reporting decisions are still open.
This is not a new final submission candidate. The results below apply to
their identified earlier archives; rebuild and repeat release checks after
the next review round. No CRAN submission or new win-builder upload is
authorised by this triage.

## Pre-triage residual-diagnostics candidate: 7 September 2026

At the time of these checks, CRAN submission was unauthorised, and the
earlier-functionality triage was parked. This candidate superseded the source
checked in the historical section below; that historical win-builder result
cannot validate these changes.

Archive: `influ2-residuals-20260907/influ2_1.1.0.tar.gz` (1,882,568 bytes).

SHA256: `5c5a93db97cef8c02dd2bac47a09b0b69a52b5b8f1b7a55ed2fd3c1c0805ed4a`

* macOS Tahoe 26.6.2 (arm64), R 4.6.1: local
  `R CMD check --as-cran --no-manual`; 0 errors, 0 warnings, and 1 note.
  The note identifies a new submission and the optional tinyVAST dependency
  available from the declared additional repository.
* All five vignettes rebuilt successfully, including the optional DHARMa
  examples with DHARMa 0.5.0. Archive tests passed 2,554 expectations, with the
  visual-regression group intentionally skipped in CRAN mode.
* A separate full local suite passed 2,562 expectations, including all eight
  visual snapshots, with no failures, warnings, or skips. The new coverage
  includes observation alignment, retained model frames, polynomial roundoff,
  residual-type boundaries, compact brms fixtures, and comparison scales.
* Source URL validation passed all 21 URLs, and spelling checks passed. The
  pkgdown site rebuilt; seven new figures, numbering, lightbox operation, and
  the separate residual reference section were inspected.
* Direct native-method audits covered GLM/NB, GAM, glmmTMB, and selected spatial
  sdmTMB/tinyVAST models. External DHARMa recipes were also exercised for
  sdmTMB single and delta responses, and single-response tinyVAST fields.
  Complete-fit brms residual/R-squared/criterion execution was not rerun:
  compact fixtures intentionally cannot provide those native calculations.

The article distinguishes goodness-of-fit from influence and index
sensitivity. It documents native residual semantics, the exploratory nature
of implied coefficients, conditional simulation choices, and specialised
PIT/OSA workflows. DHARMa is optional and adds no runtime import. No universal
OSA or multivariate residual adapter is claimed. The frozen legacy page,
helper source, simulated data values, and compact fixtures are unchanged.

GitHub Ubuntu-release and Windows-release both passed for source commit
`6559b2ed8c8e05a957edeacdab25ae506220b8d2` (run 34077210580). The pkgdown job
(34077210644) and Pages deployment (34077395318) also passed. The published
residual article and its seven lightbox figures were verified. The deployment
action reports a non-blocking Node 20 deprecation warning; it ran successfully
under GitHub's Node 24 override, and can be updated in routine maintenance.

No new win-builder upload was made for this candidate. A final submission
candidate must be rechecked after the remaining API/legacy review.

## Historical pre-residual candidate: 7 September 2026

The maintainer has not authorised CRAN submission. Final review of the earlier
interface remains parked until the scientific safeguards and release checks
are complete. Repeat the affected checks after any subsequent source changes.

Source archive: `influ2_1.1.0.tar.gz` (1,502,370 bytes).

SHA256: `700724341f44ab729e55339e7a63958a77e83388c7f37ea7525cba1d3cb823f4`

### Test environments and R CMD check results

* macOS Tahoe 26.6.2 (arm64), R 4.6.1 (2026-06-24): local
  `R CMD check --as-cran --no-manual`; 0 errors, 0 warnings, and 1 note.
* Pop!_OS 24.04 LTS (x86_64), R 4.6.1 (2026-06-24): isolated PC
  `R CMD check --as-cran`, including the PDF manual; 0 errors, 0 warnings,
  and 2 notes.

The incoming-feasibility note identifies a new submission and records that
the suggested `tinyVAST` package is available from the declared additional
repository, <https://vast-lib.r-universe.dev>. It is optional; its examples
and tests are guarded when it is unavailable. The PC's additional note says
HTML validation was skipped because that machine has no HTML Tidy command.
The PDF manual built successfully, and its changed pages were visually checked.

Both source-archive checks passed 2,446 test expectations and rebuilt all four
vignettes. The visual-regression group is intentionally skipped in CRAN mode.
A separate full local run passed all 2,454 expectations across 112 test cases,
including the eight visual snapshots, with no failures, warnings, or skips.
Test coverage is 87.70% overall and 97.37% for the new scientific guard file.

An isolated mandatory-dependency-only library installed and loaded the same
archive successfully. With brms, glmmTMB, sdmTMB, tinyVAST, and their optional
stacks unavailable, GLM diagnostics, GLM/negative-binomial step calculations,
uncertainty summaries, and plots passed. R's base/recommended packages,
including MASS and mgcv, remained available. This was an installation/core
smoke test, not a complete vignette rebuild without suggested packages.

### Documentation and archive review

The pkgdown site rebuilt successfully. Figure 13 now runs negative-binomial
GLM refits, re-estimating dispersion for each changed stage and reusing the
original final model. The known-truth log-scale RMSE decreases from 0.311 to
0.084 across this designed teaching sequence. Its intervals are approximate
model-based intervals, not a coverage or model-selection experiment. Browser
checks verified figure numbering and lightbox opening/closing at normal and
narrow widths. All 18 URLs checked in the built archive passed, and spelling
checks passed with the reviewed technical word list.

The new regression coverage verifies focus-interaction boundaries,
offset/exposure restrictions, backend-specific lognormal parameterisations,
and negative-binomial refits. Step plots remain centred year-effect contrasts,
not spatially integrated abundance indices. The legacy review page, helper
source, synthetic dataset, and compact posterior fixtures were not altered.

GitHub Ubuntu-release and Windows-release checks passed for source commit
`c67f54856587054cf9b3ed15239267e8ac1dd380` (run 34070568827). The pkgdown job
(34070568852) and its Pages deployment (34070754899) passed, and the published
article was verified. The Actions matrix remains these two release platforms.

Bentley's original notice remains installed with the frozen validation source.
A bounded source/history audit found no concrete evidence requiring a licence
change for gamInflu or CPUETools; applicable notices must still be retained if
further copying is identified. The source archive contains no private source
data, compiled model objects, or website build output.

### Historical win-builder result collected

This exact archive was uploaded to the R-devel win-builder web form on
7 September 2026. The response confirmed the filename and 1,502,370-byte
upload. The archive's maintainer was verified as
`Darcy Webber <darcy@quantifish.co.nz>` before upload.

The maintainer supplied the result link on 7 September 2026:
<https://win-builder.r-project.org/YZRkXb63mjjL/>.
R-devel (2026-09-06 r90498 ucrt), Windows Server 2022:
**0 errors, 0 warnings, and 1 NOTE**. Installation, examples, all four
vignettes, and both PDF and HTML manual checks passed. Tests passed 2,446
expectations; the visual-regression group was intentionally skipped in
CRAN mode.

The single incoming-feasibility NOTE lists the new submission, technical
words in DESCRIPTION (`CPUE`, `al`, `estimands`, `et`, and
`spatiotemporal`), and optional tinyVAST availability from the declared
additional repository. It is not an installation or test failure.

The check log and binary DESCRIPTION both identify
`Darcy Webber <darcy@quantifish.co.nz>` as maintainer. All source DESCRIPTION
fields in the binary match the archived upload, including
`Packaged: 2026-09-07 00:11:34 UTC; darcy`; only the Windows `Built` field
is added. The recorded local archive checksum was reverified. Win-builder
does not supply a source checksum here, so this is a metadata/timing match,
not an independent remote source-hash verification. Direct email headers
were unavailable through the connected business Gmail search.

Logs, test/example output, and the Windows binary are preserved under
`influ2-hardening-20260907.Rcheck/winbuilder-YZRkXb63mjjL/`.
The result-collection follow-up is complete. No repeat upload or CRAN
submission was made. This result applies only to the historical pre-residual
archive, not to the later residual or legacy-triage changes. Changes confined
to this excluded release record do not alter a checked source archive.
