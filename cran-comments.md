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
