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
#>   focus level term   component scale    estimate  std_error       lower
#> 1  year  2000 year conditional  link -0.15669098 0.02954265 -0.21459351
#> 2  year  2001 year conditional  link  0.02026241 0.02949859 -0.03755375
#> 3  year  2002 year conditional  link  0.09924692 0.02818083  0.04401351
#> 4  year  2003 year conditional  link  0.09180942 0.02865144  0.03565362
#> 5  year  2004 year conditional  link -0.06410984 0.03160738 -0.12605917
#> 6  year  2005 year conditional  link  0.27668679 0.02601434  0.22569962
#>          upper              method
#> 1 -0.098788448 analytic covariance
#> 2  0.078078584 analytic covariance
#> 3  0.154480328 analytic covariance
#> 4  0.147965216 analytic covariance
#> 5 -0.002160506 analytic covariance
#> 6  0.327673959 analytic covariance
```
