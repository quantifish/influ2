# Plot a stored CPUE model search

Plot a stored CPUE model search

## Usage

``` r
# S3 method for class 'influ_search'
plot(x, ...)

# S3 method for class 'influ_search'
autoplot(object, ...)
```

## Arguments

- x, object:

  An `influ_search` object from
  [`search_cpue()`](https://www.quantifish.co.nz/influ2/reference/search_cpue.md).

- ...:

  Reserved for additional plotting arguments.

## Value

A ggplot showing evaluated native criterion values. No fits,
simulations, or criteria are recalculated.
