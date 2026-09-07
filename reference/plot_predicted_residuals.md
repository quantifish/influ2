# Plot predicted values against residuals

Uses native response-scale fitted values and the explicitly requested
native residual type. No residual type is substituted automatically.
Complete BRMS fits are required; compact influence-only fixtures cannot
supply native predictions. sdmTMB delta models require separate
component-specific native diagnostics, and tinyVAST does not provide
Pearson residuals.

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
