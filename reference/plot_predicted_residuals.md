# Plot predicted values against residuals

Plot predicted values against residuals

## Usage

``` r
plot_predicted_residuals(fit, trend = "loess", type = "pearson")
```

## Arguments

- fit:

  A fitted model.

- trend:

  One of \`"loess"\`, \`"lm"\`, \`"linear"\`, or \`"none"\`.

- type:

  Residual type passed to the fitted model.

## Value

A \[ggplot2::ggplot()\] object.
