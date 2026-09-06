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
#>   focus level term   component scale   estimate  std_error       lower
#> 1  year  2000 year conditional  link 0.26372139 0.05631806  0.15334002
#> 2  year  2001 year conditional  link 0.32518319 0.05439295  0.21857498
#> 3  year  2002 year conditional  link 0.35979168 0.05009542  0.26160645
#> 4  year  2003 year conditional  link 0.06774274 0.04881851 -0.02793978
#> 5  year  2004 year conditional  link 0.23960938 0.04452008  0.15235163
#> 6  year  2005 year conditional  link 0.02245967 0.05465950 -0.08467098
#>       upper              method
#> 1 0.3741028 analytic covariance
#> 2 0.4317914 analytic covariance
#> 3 0.4579769 analytic covariance
#> 4 0.1634253 analytic covariance
#> 5 0.3268671 analytic covariance
#> 6 0.1295903 analytic covariance
```
