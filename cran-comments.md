## Current candidate: 7 September 2026

### Test environments

* macOS Tahoe 26.6.2 (arm64), R 4.6.1 (2026-06-24): local
  `R CMD check --as-cran --no-manual`

### R CMD check results

0 errors | 0 warnings | 1 note

The note identifies this as a new submission and records that the suggested
`tinyVAST` package is available from the declared additional repository.

`tinyVAST` is an optional backend and is currently obtained from the package
authors' R-universe repository. All `tinyVAST` examples and tests are skipped
when that suggested package is unavailable.

The source-archive check passed 2,246 test expectations and rebuilt all four
vignettes successfully. The visual-regression group is intentionally skipped
in CRAN mode; a separate full local run passed all 2,254 expectations,
including the eight visual snapshots, with no failures, warnings, or skips.
The pkgdown site also rebuilt successfully, including the new cross-model
index comparison, the shared vignette theme setup, and executed GLM and
tinyVAST refitted step-plot examples. The refitting tests include actual GLM,
GAM, glmmTMB, sdmTMB, and tinyVAST fits, alongside BRMS update and cache-safety
tests. Step plots retain year-effect contrasts, not area-integrated indices.
A separate BRMS smoke check fitted and updated a 150-observation Gaussian
model using four chains of 2,000 iterations, including 1,000 warmup iterations.
The reduced fit passed the live convergence checks, the unchanged original
fit was reused, and all plotted intervals were finite. No fits were retained
in the compact step result.

## Earlier win-builder validation: 5 September 2026

The preceding source candidate passed on Windows Server 2022 x64, R-devel
(2026-09-04 r90492 ucrt), with 0 errors, 0 warnings, and 1 note. Package
installation, examples, tests, vignette rebuilding, and the PDF and HTML
versions of the reference manual all completed successfully. The note also
listed domain-specific words in DESCRIPTION (`CPUE`, `estimands`, and
`spatiotemporal`; `et al.` is part of a bibliographic reference).

That win-builder result predates the centred CDI, refitted step plots, and
subsequent vignette changes. Repeat win-builder
on the final reviewed source before CRAN submission, using maintainer address
`darcy@quantifish.co.nz`.
