# Influence diagnostics for brms models

Population-level posterior coefficients are projected directly into the
compact focus-by-term contrasts. The method therefore never constructs
an observation-by-draw-by-term array. Joint posterior dependence is
preserved while calculating the requested diagnostics. Lognormal models
currently require constant \`sigma\`; varying scale needs a joint
location-and-scale calculation before arithmetic-mean ratios can be
reported. The \`mu\` link must be identity because BRMS parameterises
the log-location, not the arithmetic mean. Formula offsets and response
\`rate()\` additions have the same restrictions as the GLM adapter.

## Usage

``` r
# S3 method for class 'brmsfit'
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
  ndraws = NULL,
  seed = NULL,
  draws_path = NULL,
  keep_model = FALSE,
  ...
)
```

## Arguments

- model:

  A fitted object from \[brms::brm()\].

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

  Either \`"auto"\`, \`"posterior"\`, or \`"none"\`. The \`"none"\` mode
  reduces the posterior to coefficient means before influence
  calculations.

- retain:

  One of \`"summary"\`, \`"derived_draws"\`, or \`"disk"\`.

- probs:

  Lower and upper interval probabilities.

- ndraws:

  Optional maximum number of posterior draws. \`NULL\` uses every
  available draw.

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
