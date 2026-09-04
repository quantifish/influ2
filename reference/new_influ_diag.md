# A model-neutral influence diagnostic

An \`influ_diag\` stores derived diagnostics rather than
observation-by-draw arrays. Its tables have a stable schema across model
backends, allowing the same summary and plotting methods to be used for
maximum-likelihood and Bayesian models.

## Usage

``` r
new_influ_diag(
  backend,
  family,
  focus,
  influence,
  coefficients = NULL,
  composition = NULL,
  indices = NULL,
  metrics = NULL,
  uncertainty = list(),
  retained = list(),
  metadata = list(),
  draws = NULL,
  model = NULL
)
```

## Arguments

- backend:

  Name of the model backend.

- family:

  An \`influ_family_spec\` object.

- focus:

  Name of the focus variable, usually time.

- influence:

  Long-form influence results.

- coefficients:

  Compact coefficient or field summaries.

- composition:

  Compact data-composition summaries.

- indices:

  Nominal and standardised index summaries.

- metrics:

  Compact overall and trend influence metrics.

- uncertainty:

  Description of the uncertainty calculation.

- retained:

  Description of retained posterior or sampling information.

- metadata:

  Additional model and calculation metadata.

- draws:

  Optional derived draws. These are never observation-level posterior
  arrays.

- model:

  Optional retained fitted model.

## Value

An object of class \`influ_diag\` and a backend-specific subclass.
