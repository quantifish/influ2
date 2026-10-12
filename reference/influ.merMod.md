# Influence diagnostics for lme4 mixed models

Fixed terms and an aggregate random-effect contribution use the same
joint native conditional covariance. Variance/dispersion parameters are
held fixed; GLMM covariance is a local Gaussian approximation, not
posterior draws. Singular fits remain flagged in metadata, rather than
acquiring invented variance. Dense joint covariance calculations are
limited to 256 MiB. Non-log natural differences are evaluated at the
full fitted reference predictor, including random effects. Analytic
difference intervals condition on that reference; joint simulations also
propagate its coefficient dependence. Influence reference grids require
focus values and existing grouping levels; response-index reference
grids instead omit year and set group effects to zero. For a single
random intercept, aggregate random-effect CDI categories use the fitted
grouping labels. More complex random structures retain the aggregate
contribution categories rather than inventing separate coefficients.

## Usage

``` r
# S3 method for class 'merMod'
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
  response_transform = "identity",
  ...
)

# S3 method for class 'lmerMod'
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
  response_transform = "identity",
  ...
)

# S3 method for class 'glmerMod'
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
  response_transform = "identity",
  ...
)
```

## Arguments

- model:

  A fit from \[lme4::lmer()\], \[lme4::glmer()\], or
  \[lme4::glmer.nb()\]. Nonlinear \`nlmer()\` and arbitrary custom
  families are not supported.

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

- response_transform:

  For lme4 only, \`"identity"\` (default) or \`"log"\`. Declare
  \`"log"\` for a Gaussian lmer model of \`log(response)\`, or a named
  precomputed log-response column. Influence ratios are on the natural
  scale. Response indices use \`exp(eta + sigma^2/2)\`; sigma and
  random-effect variance parameters are treated as known. Other
  transformations are not supported.

- ...:

  Reserved for future backend options.

## Value

An \[influ_diag\] with fixed and aggregate random-effect components.
