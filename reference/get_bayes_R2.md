# Summarise Bayesian R-squared for brms models

Summarise Bayesian R-squared for brms models

## Usage

``` r
get_bayes_R2(fits, probs = c(0.025, 0.975), ...)
```

## Arguments

- fits:

  A \`brmsfit\` or list of \`brmsfit\` objects.

- probs:

  Lower and upper interval probabilities.

- ...:

  Arguments passed to \[brms::bayes_R2()\].

## Value

A data frame with one row per model.

## Details

Requires an original complete \`brmsfit\`, not a compact influence-only
fixture shipped with influ2. The helper summarises the existing fit and
does not run MCMC.
