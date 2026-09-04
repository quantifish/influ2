# Influence diagnostics for tinyVAST models

Fixed GAM terms use their marginal TMB covariance. Smooth, spatial,
spatiotemporal, and spatially varying contributions are obtained through
tinyVAST's component projection interface and reduced immediately to
compact focus-level diagnostics. Spatial and spatiotemporal uncertainty
uses sparse joint-precision simulation. Delta fixed effects and latent
fields are combined draw by draw to obtain unconditional-mean influence.
Multivariate mixed-family responses are returned as response-labelled
components.

## Usage

``` r
# S3 method for class 'tinyVAST'
influ(
  model,
  focus,
  data = NULL,
  weights = NULL,
  reference_data = NULL,
  reference_weights = NULL,
  uncertainty = "auto",
  retain = "summary",
  probs = c(0.025, 0.975),
  ndraws = 1000L,
  seed = NULL,
  draws_path = NULL,
  keep_model = FALSE,
  ...
)
```

## Arguments

- model:

  A fitted \`tinyVAST\` object.

- focus:

  Name of the focus variable, usually year.

- data:

  Optional model data. The model frame is used by default.

- weights:

  Optional numeric observation weights or the name of a weight column in
  \`data\`.

- reference_data:

  Optional prediction grid defining the common standardisation
  distribution. By default the observed data are used.

- reference_weights:

  Optional numeric weights, or a column name, for \`reference_data\`.

- uncertainty:

  One of \`"auto"\`, \`"none"\`, \`"analytic"\`, or \`"simulation"\`.

- retain:

  One of \`"summary"\`, \`"derived_draws"\`, or \`"disk"\`.

- probs:

  Lower and upper interval probabilities.

- ndraws:

  Number of joint coefficient draws for simulation uncertainty.

- seed:

  Optional simulation seed.

- draws_path:

  Output path used when \`retain = "disk"\`.

- keep_model:

  Retain the fitted model inside the diagnostic object.

- ...:

  Reserved for future backend options.

## Value

An \[influ_diag\] object.
