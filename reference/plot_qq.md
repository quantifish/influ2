# Quantile-quantile plot of model residuals

Displays the fitted model's standardised residuals against normal
quantiles. For non-Gaussian models this is a screening diagnostic, and
should be complemented by simulation-based residual checks.

## Usage

``` r
plot_qq(fit, probs = c(0.25, 0.75), type = "pearson")
```

## Arguments

- fit:

  A fitted model.

- probs:

  Two probabilities defining the reference line.

- type:

  Residual type passed to the fitted model.

## Value

A \[ggplot2::ggplot()\] object.
