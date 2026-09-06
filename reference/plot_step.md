# Display the effect of sequential model-standardisation steps

Plot supplied fits, calculated diagnostics, or a stored
[influ_steps](https://www.quantifish.co.nz/influ2/reference/influ_steps.md)
object. Set `refit = TRUE` with one original model to fit a sequence
internally. The displayed quantities are year-effect contrasts,
including when spatial processes are changed between fits; they are not
area-integrated indices.

## Usage

``` r
plot_step(
  fits,
  labels = NULL,
  year = NULL,
  fill = "purple4",
  probs = c(0.025, 0.975),
  show_probs = TRUE,
  steps = NULL,
  refit = FALSE,
  component = NULL,
  keep_fits = FALSE,
  refit_args = list(),
  ...
)
```

## Arguments

- fits:

  One supported fitted model, an `influ_diag`, or an ordered list of
  fitted models or diagnostics. An existing `influ_steps` is also
  accepted.

- labels:

  Unique step labels. Defaults to list names or generated labels.

- year:

  Focus-variable name, inferred from the first input when omitted.

- fill:

  Colour used for the current model's interval.

- probs:

  Interval probabilities for newly calculated diagnostics. Existing
  diagnostics retain their original intervals.

- show_probs:

  Show each step's uncertainty interval. These are not intervals for
  differences between models.

- steps:

  For refitting, an ordered, uniquely named list of cumulative formulas
  or lists of model-update arguments. Each specification updates the
  original model. Use `formula` in an argument list for its formula
  change. If omitted, ordinary single-component models start with the
  focus term and progressively add the remaining formula terms. Offsets
  are retained. Spatial models require explicit steps so field changes
  are deliberate.

- refit:

  Explicitly allow new model fitting. Defaults to `FALSE`.

- component:

  Index component. Must be supplied if more than one is available, for
  example `"positive"` or `"unconditional_mean"` in a hurdle model. Even
  a combined-component index remains a focus-effect contrast, not the
  full expected response integrated over space.

- keep_fits:

  Retain fitted models in the result? Defaults to `FALSE` to keep the
  returned object small. Save expensive fits separately if needed.

- refit_args:

  Named arguments passed to the model update at every step, such as BRMS
  sampling controls. Stage-specific arguments take precedence.
  Execution-only controls (`seed`, `cores`, `refresh`, `silent`, and
  `verbose`) do not by themselves force an otherwise unchanged original
  fit to rerun.

- ...:

  Arguments passed to
  [`influ()`](https://www.quantifish.co.nz/influ2/reference/influ.md)
  for fitted-model inputs, such as `uncertainty = "none"`, `weights`, or
  `reference_data`. All steps use the same calculation arguments. Not
  used for model-fitting controls.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.
