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

The source-archive check passed 2,327 test expectations and rebuilt all four
vignettes successfully. The visual-regression group is intentionally skipped
in CRAN mode; a separate full local run passed all 2,335 expectations,
including the eight visual snapshots, with no failures, warnings, or skips.
The pkgdown site also rebuilt successfully, including the new cross-model
index comparison, the shared vignette theme setup, and executed GLM and
tinyVAST refitted step-plot examples. The refitting tests include actual GLM,
GAM, glmmTMB, sdmTMB, and tinyVAST fits, alongside BRMS update and cache-safety
tests. Step plots retain year-effect contrasts, not area-integrated indices.
The revised synthetic lobster scenario contains 5,049 observations with
deliberate seasonal, depth, and soak-time shifts. Tests verify covariate
overlap, visible refitted corrections, and recovery of its known annual
effects. The matching BRMS posterior was regenerated with four chains of
5,000 iterations, including 2,000 warmup iterations per chain. It passed with
maximum R-hat 1.003966, minimum bulk ESS 1,774, minimum tail ESS 2,823, and no
divergences. Its 12,000 post-warmup draws were compacted to 200 joint draws.
A regression test confirms that this fixture uses the bundled observations.
Additional BRMS regressions verify population-term mapping after random and
smooth terms, including observed/reference-grid uncertainty and hurdle parts.

## Earlier win-builder validation: 5 September 2026

The preceding source candidate passed on Windows Server 2022 x64, R-devel
(2026-09-04 r90492 ucrt), with 0 errors, 0 warnings, and 1 note. Package
installation, examples, tests, vignette rebuilding, and the PDF and HTML
versions of the reference manual all completed successfully. The note also
listed domain-specific words in DESCRIPTION (`CPUE`, `estimands`, and
`spatiotemporal`; `et al.` is part of a bibliographic reference).

That win-builder result predates the centred CDI, refitted step plots,
revised lobster data and posterior fixture, and subsequent vignette changes.
Repeat win-builder
on the final reviewed source before CRAN submission, using maintainer address
`darcy@quantifish.co.nz`.
