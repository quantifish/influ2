# Display the effect of sequential model-standardisation steps

Display the effect of sequential model-standardisation steps

## Usage

``` r
plot_step(
  fits,
  labels = NULL,
  year = NULL,
  fill = "purple4",
  probs = c(0.25, 0.75),
  show_probs = TRUE,
  ...
)
```

## Arguments

- fits:

  A fitted model, an \[influ_diag\], or a list of either.

- labels:

  Optional model labels.

- year:

  Optional focus-variable name. It is inferred when omitted.

- fill:

  Colour used for the current model's interval.

- probs:

  Interval probabilities used when diagnostics must be calculated.

- show_probs:

  Show uncertainty ribbons.

- ...:

  Arguments passed to \[influ()\] when \`fits\` contains models.

## Value

A \[ggplot2::ggplot()\] object.
