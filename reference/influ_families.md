# Families supported by the influence engine

\`influ_families()\` describes the first set of response distributions
that can be represented by an \[influ_diag\] object. Model backends do
not necessarily implement every family and component immediately;
unsupported combinations fail before any calculations are attempted.

## Usage

``` r
influ_families()
```

## Value

A data frame containing canonical family names, aliases, and the default
influence scale.

## Examples

``` r
influ_families()
#>              family                       aliases default_scale
#> 1          gaussian                        normal    difference
#> 2          binomial                     bernoulli    difference
#> 3           poisson                       poisson         ratio
#> 4 negative_binomial negbinomial, nbinom1, nbinom2         ratio
#> 5         lognormal                     lognormal         ratio
#> 6             gamma                         Gamma         ratio
#> 7           tweedie                       Tweedie         ratio
```
