# Plot calculated CPUE indices

Display assessment indices without refitting or recalculating
predictions.

## Usage

``` r
plot_index(
  x,
  show_probs = TRUE,
  ...,
  type = c("index", "correlation", "covariance"),
  scale = c("log", "response")
)

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

- type:

  `"index"` (default), `"correlation"`, or `"covariance"`. The latter
  two display the stored annual uncertainty matrix as a heatmap.

- scale:

  For matrix displays, `"log"` (default) or `"response"`.

## Value

A ggplot object.

## See also

[`index_vcov()`](https://www.quantifish.co.nz/influ2/reference/index_vcov.md),
[`index_table()`](https://www.quantifish.co.nz/influ2/reference/index_table.md),
[`cpue_index()`](https://www.quantifish.co.nz/influ2/reference/cpue_index.md),
[`integrate_index()`](https://www.quantifish.co.nz/influ2/reference/integrate_index.md),
[`plot_compare()`](https://www.quantifish.co.nz/influ2/reference/plot_compare.md)
