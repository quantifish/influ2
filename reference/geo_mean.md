# Geometric mean

Calculate the geometric mean on the log scale to avoid overflow and
underflow from multiplying many values together.

## Usage

``` r
geo_mean(a, na.rm = FALSE)
```

## Arguments

- a:

  A numeric vector of non-negative, finite values. Missing values are
  allowed; negative values and infinities are rejected.

- na.rm:

  Remove missing values before calculation.

## Value

A number. Zero values give zero, missing values give `NA` unless
removed, and an empty vector (after removal) gives `NaN`.

## Examples

``` r
geo_mean(c(1, 4, 16))
#> [1] 4
geo_mean(c(1, NA, 4), na.rm = TRUE)
#> [1] 2
```
