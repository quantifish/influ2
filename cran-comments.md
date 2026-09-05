## Test environments

* macOS Tahoe 26.6.2 (arm64), R 4.6.1 (2026-06-24): local
  `R CMD check --as-cran --no-manual`
* Windows Server 2022 x64, R-devel (2026-09-04 r90492 ucrt): win-builder

## R CMD check results

0 errors | 0 warnings | 1 note

The note identifies this as a new submission, lists domain-specific words in
DESCRIPTION (`CPUE`, `estimands`, and `spatiotemporal`; `et al.` is part of a
bibliographic reference), and records that the suggested `tinyVAST` package
is available from the declared additional repository.

`tinyVAST` is an optional backend and is currently obtained from the package
authors' R-universe repository. All `tinyVAST` examples and tests are skipped
when that suggested package is unavailable.

On win-builder, package installation, examples, tests, vignette rebuilding,
and the PDF and HTML versions of the reference manual all completed
successfully. The win-builder report used the maintainer address
`darcy@quantifish.co.nz`.
