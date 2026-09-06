## Current candidate: 6 September 2026

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

The complete local test suite, including seven visual regression snapshots,
passed with no failures, warnings, or skips. The source-archive check rebuilt
all vignettes successfully.

## Earlier win-builder validation: 5 September 2026

The preceding source candidate passed on Windows Server 2022 x64, R-devel
(2026-09-04 r90492 ucrt), with 0 errors, 0 warnings, and 1 note. Package
installation, examples, tests, vignette rebuilding, and the PDF and HTML
versions of the reference manual all completed successfully. The note also
listed domain-specific words in DESCRIPTION (`CPUE`, `estimands`, and
`spatiotemporal`; `et al.` is part of a bibliographic reference).

That win-builder result predates the centred CDI changes. Repeat win-builder
on the final reviewed source before CRAN submission, using maintainer address
`darcy@quantifish.co.nz`.
