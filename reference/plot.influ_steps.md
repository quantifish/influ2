# Plot a stored step sequence

Plot a stored step sequence

## Usage

``` r
# S3 method for class 'influ_steps'
plot(x, ...)

# S3 method for class 'influ_steps'
autoplot(object, ...)
```

## Arguments

- x, object:

  An `influ_steps` object from
  [`influ_steps()`](https://www.quantifish.co.nz/influ2/reference/influ_steps.md).

- ...:

  Plotting arguments passed to
  [`plot_step()`](https://www.quantifish.co.nz/influ2/reference/plot_step.md),
  such as `fill` or `show_probs`. Plotting a stored sequence never
  refits models.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.
