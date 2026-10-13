# Plot stored CPUE model-check results

Plots do not recompute tests, simulation, or model statistics.

## Usage

``` r
# S3 method for class 'influ_check'
plot(
  x,
  type = c("overview", "term", "status"),
  term = NULL,
  component = NULL,
  annotations = c("all", "none", "tests", "model"),
  ...
)

# S3 method for class 'influ_check'
autoplot(object, ...)
```

## Arguments

- x, object:

  An `influ_check` report from
  [`check_cpue()`](https://www.quantifish.co.nz/influ2/reference/check_cpue.md).

- type:

  `"overview"` (default) reuses its four-panel residual plot; `"term"`
  shows residuals against a retained raw predictor; `"status"` displays
  the recorded check-status table without residual data.

- term:

  Exact model term to display for `type = "term"`. Only terms with one
  available raw predictor have this univariate view; no basis,
  interaction, or latent-process plot is invented.

- component:

  Optional model component to disambiguate identical term names in a
  joint model. It does not recalculate component residuals.

- annotations:

  Saved annotations for the overview: `"all"` (default), `"none"`,
  `"tests"`, or `"model"`. Term plots show their stored exploratory
  screens. See
  [`plot.influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot.influ_residuals.md).

- ...:

  Further arguments passed to
  [`plot.influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot.influ_residuals.md)
  for an overview.

## Value

A ggplot or patchwork object.
