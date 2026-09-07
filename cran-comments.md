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

The remaining assessment-output and reporting decisions are still open.
This is not a new final submission candidate. The results below apply to
their identified earlier archives; rebuild and repeat release checks after
the next review round. No CRAN submission or new win-builder upload is
authorised by this triage.

## Pre-triage residual-diagnostics candidate: 7 September 2026

At the time of these checks, CRAN submission was unauthorised, and the
earlier-functionality triage was parked. This candidate superseded the source
checked in the historical section below; that pending win-builder result
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
  residual-type boundaries, compact BRMS fixtures, and comparison scales.
* Source URL validation passed all 21 URLs, and spelling checks passed. The
  pkgdown site rebuilt; seven new figures, numbering, lightbox operation, and
  the separate residual reference section were inspected.
* Direct native-method audits covered GLM/NB, GAM, glmmTMB, and selected spatial
  sdmTMB/tinyVAST models. External DHARMa recipes were also exercised for
  sdmTMB single and delta responses, and single-response tinyVAST fields.
  Complete-fit BRMS residual/R-squared/criterion execution was not rerun:
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
archive successfully. With BRMS, glmmTMB, sdmTMB, tinyVAST, and their optional
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

### Fresh win-builder validation

This exact archive was uploaded to the R-devel win-builder web form on
7 September 2026. The response confirmed the filename and 1,502,370-byte
upload. The archive's maintainer was verified as
`Darcy Webber <darcy@quantifish.co.nz>` before upload.

The new email result is pending. An earlier candidate's successful Windows
check is not being treated as validation of this archive. This upload requests
a test build only; it is not a CRAN submission. Changes confined to this
excluded release record do not alter the checked source archive.
