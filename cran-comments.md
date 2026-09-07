## Current candidate: 7 September 2026

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
