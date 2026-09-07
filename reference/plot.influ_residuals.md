# Plot a four-panel CPUE residual diagnostic

Plot a precomputed \[influ_residuals()\] result without simulation or
refitting.

## Usage

``` r
# S3 method for class 'influ_residuals'
plot(
  x,
  type = c("overview", "qq", "fitted", "year", "distribution"),
  response_scale = c("identity", "log1p"),
  ...
)

# S3 method for class 'influ_residuals'
autoplot(object, ...)
```

## Arguments

- x, object:

  An \`influ_residuals\` object.

- type:

  The four-panel \`"overview"\` (default), or one of \`"qq"\`,
  \`"fitted"\`, \`"year"\`, and \`"distribution"\`.

- response_scale:

  Scale for the response ECDF: \`"identity"\` or \`"log1p"\`, which
  retains zero catches. The latter requires non-negative responses and
  is labelled explicitly.

- ...:

  Reserved for future methods; currently unused.

## Value

A ggplot or a four-panel patchwork object, which can be customised.

## Details

The year panel shows a boxplot for each sampled year and its sample
size. Numeric years retain their spacing, including gaps; other labels
are ordered lexically. Reference lines mark the normal-score median and
quartiles. No smoother across years conceals changes in spread or tails.
The fitted panel's horizontal variable is the simulation-based
predictive mean under the conditioning recorded in the result. A
descriptive loess curve is added when there are sufficient distinct
fitted means.

The Q-Q envelope is a pointwise independent-uniform reference, not a
model-specific calibration. The ECDF envelope is a pointwise predictive
band on a compact grid. Neither envelope provides an automatic pass/fail
test. Read the calculation metadata and \[influ_residuals()\]
limitations.
