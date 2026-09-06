# Plot a model-neutral influence diagnostic

Plot a model-neutral influence diagnostic

## Usage

``` r
# S3 method for class 'influ_diag'
plot(
  x,
  type = c("influence", "index", "cdi", "components"),
  term = NULL,
  component = NULL,
  scale = NULL,
  coefficient_reference = "centred",
  coefficient_scale = "auto",
  ...
)

# S3 method for class 'influ_diag'
autoplot(object, ...)
```

## Arguments

- x:

  An \[influ_diag\] object.

- type:

  One of \`"influence"\`, \`"index"\`, \`"cdi"\`, or \`"components"\`.

- term:

  Optional term selection.

- component:

  Optional component selection.

- scale:

  Optional influence scale. By default the natural response contrast is
  plotted rather than the link-scale contrast.

- coefficient_reference:

  For CDI plots, \`"centred"\` (default) subtracts the term's mean over
  the same weighted reference distribution used for influence.
  \`"model"\` displays the original model-coded contribution on the link
  scale, including its reference factor level.

- coefficient_scale:

  For CDI plots, \`"auto"\` (default) displays centred log-response
  effects as ratios on a logarithmic axis. Other links remain in their
  labelled link units. \`"link"\` displays centred link effects for
  every model. The \`"model"\` reference always uses link units.

- ...:

  Reserved for future plotting options.

- object:

  An \[influ_diag\] object passed to \`autoplot()\`.

## Value

A \`ggplot\` or \`patchwork\` object.

## Details

CDI intervals use the probabilities supplied to \[influ()\] (95 by
default). Centring propagates the joint coefficient covariance or is
performed within each posterior/simulation draw. Ratio summaries are
calculated after transforming those draws. A CDI plot displays one
component at a time; select \`component\` when a term occurs in several
model components. Zero-probability components retain their fitted link
orientation and are explicitly labelled as such.
