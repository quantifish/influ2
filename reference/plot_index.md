# Plot calculated CPUE indices

Display assessment indices without refitting or recalculating
predictions.

## Usage

``` r
plot_index(x, show_probs = TRUE, ...)

# S3 method for class 'influ_index'
plot(x, ...)

# S3 method for class 'influ_index'
autoplot(object, ...)
```

## Arguments

- x, object:

  An
  [influ_index](https://www.quantifish.co.nz/influ2/reference/cpue_index.md)
  object.

- show_probs:

  Show the stored pointwise uncertainty interval.

- ...:

  Reserved for future use; unused.

## Value

A ggplot object.

## See also

[`cpue_index()`](https://www.quantifish.co.nz/influ2/reference/cpue_index.md),
[`integrate_index()`](https://www.quantifish.co.nz/influ2/reference/integrate_index.md),
[`plot_compare()`](https://www.quantifish.co.nz/influ2/reference/plot_compare.md)
