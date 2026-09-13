# Format a calculated index for assessment reporting

Return a reporting table without refitting or repeating predictions. The
full, stable table is still available through
[`as.data.frame.influ_index()`](https://www.quantifish.co.nz/influ2/reference/cpue_index.md).

## Usage

``` r
index_table(
  x,
  format = c("summary", "lognormal"),
  include_median = c("auto", "always", "never")
)
```

## Arguments

- x:

  An `influ_index` from
  [`cpue_index()`](https://www.quantifish.co.nz/influ2/reference/cpue_index.md)
  or
  [`integrate_index()`](https://www.quantifish.co.nz/influ2/reference/integrate_index.md).

- format:

  `"summary"` for the existing index summaries, or `"lognormal"` to
  append a moment-matched lognormal approximation to marginal index
  uncertainty. This is not the fitted response distribution.

- include_median:

  `"auto"` omits `Median` only when every value is missing; `"always"`
  retains it, and `"never"` omits it. An available brms posterior median
  is never replaced by a lognormal approximation.

## Value

A data frame in the stored year order. The input is unchanged.

## Details

`Mean`, `SD`, and `CV` refer to the expected-response index and its
uncertainty, not variability among individual observations. With
`format = "lognormal"`, `SDlog = sqrt(log(1 + CV^2))`,
`Meanlog = log(Mean) - SDlog^2 / 2`, and
`LognormalMedian = exp(Meanlog)`. These additional columns describe a
univariate moment-matched approximation, even when the CPUE model itself
is not lognormal. They require positive means and finite non-negative
index SDs. Preview and year-effect results cannot provide this output.

`Meanlog` is not `log(Mean)`, and `SDlog` is not automatically the
square root of the diagonal of
[`index_vcov()`](https://www.quantifish.co.nz/influ2/reference/index_vcov.md)
on the log scale. The latter comes from the actual joint delta method or
log-index draws, not marginal lognormal moment matching. Use the joint
matrix for a correlated-index assessment; do not combine its
correlations with these approximate SDs without explicitly choosing a
different uncertainty model.

## See also

[`index_vcov()`](https://www.quantifish.co.nz/influ2/reference/index_vcov.md),
[`plot_index()`](https://www.quantifish.co.nz/influ2/reference/plot_index.md),
[`cpue_index()`](https://www.quantifish.co.nz/influ2/reference/cpue_index.md)

## Examples

``` r
data(lobsters_per_pot)
fit <- glm(lobsters ~ year + depth, poisson(), data = lobsters_per_pot)
index <- cpue_index(fit, reference_data = data.frame(depth = 40))
head(index_table(index))
#>   Year      Mean         SD         CV    Qlower    Qupper       Method
#> 1 2000 0.9210196 0.05669518 0.06155697 0.8163408 1.0391213 standardised
#> 2 2001 0.9936961 0.05969711 0.06007582 0.8833177 1.1178672 standardised
#> 3 2002 1.0878984 0.06095491 0.05602997 0.9747551 1.2141745 standardised
#> 4 2003 0.8606127 0.04878710 0.05668880 0.7701125 0.9617481 standardised
#> 5 2004 1.2206394 0.06430581 0.05268207 1.1008910 1.3534133 standardised
#> 6 2005 1.0011957 0.06130647 0.06123325 0.8879676 1.1288619 standardised
#>   Distribution Link
#> 1      poisson  log
#> 2      poisson  log
#> 3      poisson  log
#> 4      poisson  log
#> 5      poisson  log
#> 6      poisson  log
head(index_table(index, format = "lognormal"))
#>   Year      Mean         SD         CV    Qlower    Qupper       Method
#> 1 2000 0.9210196 0.05669518 0.06155697 0.8163408 1.0391213 standardised
#> 2 2001 0.9936961 0.05969711 0.06007582 0.8833177 1.1178672 standardised
#> 3 2002 1.0878984 0.06095491 0.05602997 0.9747551 1.2141745 standardised
#> 4 2003 0.8606127 0.04878710 0.05668880 0.7701125 0.9617481 standardised
#> 5 2004 1.2206394 0.06430581 0.05268207 1.1008910 1.3534133 standardised
#> 6 2005 1.0011957 0.06130647 0.06123325 0.8879676 1.1288619 standardised
#>   Distribution Link       Meanlog      SDlog LognormalMedian
#> 1      poisson  log -0.0841650112 0.06149878       0.9192796
#> 2      poisson  log -0.0081251939 0.06002172       0.9919077
#> 3      poisson  log  0.0826805114 0.05598607       1.0861947
#> 4      poisson  log -0.1517148958 0.05664333       0.8592332
#> 5      poisson  log  0.1979890008 0.05264558       1.2189490
#> 6      poisson  log -0.0006762914 0.06117597       0.9993239
```
