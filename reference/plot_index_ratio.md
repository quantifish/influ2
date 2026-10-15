# Plot a separate index-ratio diagnostic

Plot stored point-estimate ratios with a reference line at one. Missing
or undefined ratios remain gaps; lines never bridge an internally
missing year.

## Usage

``` r
plot_index_ratio(x, log = FALSE, ...)

# S3 method for class 'influ_index_ratio'
plot(x, ...)

# S3 method for class 'influ_index_ratio'
autoplot(object, ...)
```

## Arguments

- x, object:

  An `influ_index_ratio` from
  [`index_ratio()`](https://www.quantifish.co.nz/influ2/reference/index_ratio.md).

- log:

  Use a logarithmic ratio axis? Requires strictly positive defined
  ratios; zero ratios are retained in linear displays, never replaced.

- ...:

  Reserved for future use; unused arguments are rejected.

## Value

A ggplot object, using saved values only.

## See also

[`index_ratio()`](https://www.quantifish.co.nz/influ2/reference/index_ratio.md)
