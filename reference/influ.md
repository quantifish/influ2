# Calculate CPUE influence diagnostics

\`influ()\` is the model-neutral entry point for influence diagnostics.
It dispatches on the fitted model class and returns the same compact
\[influ_diag\] structure for every backend.

## Usage

``` r
influ(model, ...)
```

## Arguments

- model:

  A fitted model object.

- ...:

  Arguments passed to a model-specific method.

## Value

An object inheriting from \`influ_diag\`.

## Examples

``` r
data(lobsters_per_pot)
model <- glm(
  lobsters ~ year + month + poly(depth, 2) + poly(soak, 2),
  family = poisson(link = "log"),
  data = lobsters_per_pot
)
diagnostic <- influ(model, focus = "year")
diagnostic
#> <influ_diag>
#>   Backend:     glm
#>   Response:    single poisson (log)
#>   Focus:       year
#>   Terms:       4
#>   Focus levels:18
#>   Uncertainty: analytic covariance
#>   Retained:    summary
head(influ_effects(diagnostic))
#>   focus level term   component scale     estimate  std_error       lower
#> 1  year  2000 year conditional  link -0.004510875 0.05366972 -0.10970160
#> 2  year  2001 year conditional  link  0.119144290 0.04705117  0.02692570
#> 3  year  2002 year conditional  link  0.057827451 0.05056566 -0.04127942
#> 4  year  2003 year conditional  link  0.181677318 0.04104143  0.10123760
#> 5  year  2004 year conditional  link  0.252435210 0.04365982  0.16686353
#> 6  year  2005 year conditional  link  0.026666454 0.05368873 -0.07856152
#>       upper              method
#> 1 0.1006798 analytic covariance
#> 2 0.2113629 analytic covariance
#> 3 0.1569343 analytic covariance
#> 4 0.2621170 analytic covariance
#> 5 0.3380069 analytic covariance
#> 6 0.1318944 analytic covariance
```
