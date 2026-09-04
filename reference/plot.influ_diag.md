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

- ...:

  Reserved for future plotting options.

- object:

  An \[influ_diag\] object passed to \`autoplot()\`.

## Value

A \`ggplot\` or \`patchwork\` object.
