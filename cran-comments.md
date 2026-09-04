## Test environments

* macOS, R release: local `R CMD check --as-cran`
* Ubuntu, R devel, release, and oldrel-1: GitHub Actions
* macOS and Windows, R release: GitHub Actions

## R CMD check results

0 errors | 0 warnings | 1 note

The note identifies this as a new submission and records that the suggested
`tinyVAST` package is available from the declared additional repository.

`tinyVAST` is an optional backend and is currently obtained from the package
authors' R-universe repository. All `tinyVAST` examples and tests are skipped
when that suggested package is unavailable.
