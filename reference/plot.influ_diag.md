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
orientation and are explicitly labelled as such. Ratio-scale CDI panels
use the y-axis label "Relative Effect"; the term name remains on the
horizontal axis rather than lengthening the y label. Logit-scale panels
use "Effect (log-odds)" (or "Effect (log-odds of zero)" for
zero-probability components). This shorter label does not change the
selected centring or component orientation. Short term labels (including
months) are horizontal on the upper fitted- effect axis and the lower
composition axis, for fixed and random effects. At drawing time, the
paired axes measure the available width and their label text. All labels
are shown when they fit; otherwise every second, third, or subsequent
level is labelled, starting at the first level. The last level is not
forced onto an off-stride position. The same regular spacing is used
above and below, retaining every coefficient, composition column, and
tick. Wider output devices can show more labels. Longer labels are
angled and justified for their respective top or bottom axis. The
influence panel's focus labels are on the right, with the same level
ordering as the composition. The proportion legend has one column and at
most four reference bubbles.

## See also

[`influ_residuals`](https://www.quantifish.co.nz/influ2/reference/influ_residuals.md)
and
[`plot.influ_residuals`](https://www.quantifish.co.nz/influ2/reference/plot.influ_residuals.md)
for the separate residual overview and standalone
`plot(checks, type = "qq")` diagnostic. Residual plots require an
`influ_residuals` object, not an `influ_diag`.
