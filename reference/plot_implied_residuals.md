# Plot residual-implied annual effects

Display the fixed baseline and local residual-implied trajectories. This
restores the effect-scale question of the historical plot, with an
explicit traditional option. Use
[`plot_grouped_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot_grouped_residuals.md)
for grouped PIT scores.

## Usage

``` r
plot_implied_residuals(fit, colour = "purple4", ncol = 3L, ...)

# S3 method for class 'influ_implied'
plot(x, ...)

# S3 method for class 'influ_implied'
autoplot(object, ...)
```

## Arguments

- fit:

  An `influ_implied` result or a supported fitted model.

- colour:

  Colour of implied-effect points, lines, and intervals.

- ncol:

  Number of facet columns, default three.

- ...:

  Calculation arguments passed to
  [`implied_effects()`](https://www.quantifish.co.nz/influ2/reference/implied_effects.md)
  for a fitted model, including `method = "traditional"`. Not accepted
  for stored results: calculate a separate result to change its method,
  interval, or baseline.

- x, object:

  An \`influ_implied\` result.

## Value

A ggplot. Its data retain every stratum and its status; metadata
describe the effect scale and conditional interpretation.

## Examples

``` r
data(lobsters_per_pot)
fit <- glm(lobsters ~ year + month + depth, family = poisson(),
  data = lobsters_per_pot)
plot_implied_residuals(fit, groups = "month")
```
