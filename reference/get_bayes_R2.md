# Summarise Bayesian R-squared for BRMS models

Summarise Bayesian R-squared for BRMS models

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
