# Compare standardised CPUE indices

Compares indices from any fitted models supported by \[influ()\] or from
pre-computed \[influ_diag\] objects. Models are reduced to the same
index schema before plotting.

## Usage

``` r
plot_compare(
  fits,
  labels = NULL,
  year = NULL,
  probs = c(0.25, 0.75),
  show_probs = TRUE,
  rescale = "raw",
  rescale_series = NULL,
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

- probs:

  Interval probabilities used when diagnostics must be calculated.

- show_probs:

  Show uncertainty ribbons.

- rescale:

  \`"raw"\`, or a positive numeric geometric mean.

- rescale_series:

  Optional series number supplying the common scale over overlapping
  focus levels.

- ...:

  Arguments passed to \[influ()\] when \`fits\` contains models.

## Value

A \[ggplot2::ggplot()\] object.
