# Package index

## Model-neutral diagnostics

Calculate, inspect, and plot influence diagnostics.

- [`influ()`](https://www.quantifish.co.nz/influ2/reference/influ.md) :
  Calculate CPUE influence diagnostics
- [`influ(`*`<brmsfit>`*`)`](https://www.quantifish.co.nz/influ2/reference/influ.brmsfit.md)
  : Influence diagnostics for brms models
- [`influ(`*`<gam>`*`)`](https://www.quantifish.co.nz/influ2/reference/influ.gam.md)
  : Influence diagnostics for generalised additive models
- [`influ(`*`<glm>`*`)`](https://www.quantifish.co.nz/influ2/reference/influ.glm.md)
  : Influence diagnostics for generalised linear models
- [`influ(`*`<glmmTMB>`*`)`](https://www.quantifish.co.nz/influ2/reference/influ.glmmTMB.md)
  : Influence diagnostics for glmmTMB models
- [`influ(`*`<sdmTMB>`*`)`](https://www.quantifish.co.nz/influ2/reference/influ.sdmTMB.md)
  : Influence diagnostics for sdmTMB models
- [`influ(`*`<tinyVAST>`*`)`](https://www.quantifish.co.nz/influ2/reference/influ.tinyVAST.md)
  : Influence diagnostics for tinyVAST models
- [`influ_effects()`](https://www.quantifish.co.nz/influ2/reference/influ_extractors.md)
  [`influ_indices()`](https://www.quantifish.co.nz/influ2/reference/influ_extractors.md)
  [`influ_composition()`](https://www.quantifish.co.nz/influ2/reference/influ_extractors.md)
  [`influ_draws()`](https://www.quantifish.co.nz/influ2/reference/influ_extractors.md)
  [`influ_metrics()`](https://www.quantifish.co.nz/influ2/reference/influ_extractors.md)
  : Extract a table from an influence diagnostic
- [`influ_families()`](https://www.quantifish.co.nz/influ2/reference/influ_families.md)
  : Families supported by the influence engine
- [`plot(`*`<influ_diag>`*`)`](https://www.quantifish.co.nz/influ2/reference/plot.influ_diag.md)
  [`autoplot(`*`<influ_diag>`*`)`](https://www.quantifish.co.nz/influ2/reference/plot.influ_diag.md)
  : Plot a model-neutral influence diagnostic
- [`plot_bubble()`](https://www.quantifish.co.nz/influ2/reference/plot_bubble.md)
  : Bubble plot of sampling composition

## Additional diagnostics and comparisons

Compare indices and inspect data coverage.

- [`plot_compare()`](https://www.quantifish.co.nz/influ2/reference/plot_compare.md)
  : Compare standardised CPUE indices
- [`influ_steps()`](https://www.quantifish.co.nz/influ2/reference/influ_steps.md)
  [`print(`*`<influ_steps>`*`)`](https://www.quantifish.co.nz/influ2/reference/influ_steps.md)
  [`summary(`*`<influ_steps>`*`)`](https://www.quantifish.co.nz/influ2/reference/influ_steps.md)
  : Calculate a sequence of refitted year-effect contrasts
- [`plot(`*`<influ_steps>`*`)`](https://www.quantifish.co.nz/influ2/reference/plot.influ_steps.md)
  [`autoplot(`*`<influ_steps>`*`)`](https://www.quantifish.co.nz/influ2/reference/plot.influ_steps.md)
  : Plot a stored step sequence
- [`plot_step()`](https://www.quantifish.co.nz/influ2/reference/plot_step.md)
  : Display the effect of sequential model-standardisation steps
- [`plot_data_extent()`](https://www.quantifish.co.nz/influ2/reference/plot_data_extent.md)
  : Plot the completeness of variables through time

## CPUE indices and assessment tables

Calculate standardised means or area-integrated totals, and plot stored
results.

- [`cpue_index()`](https://www.quantifish.co.nz/influ2/reference/cpue_index.md)
  [`as.data.frame(`*`<influ_index>`*`)`](https://www.quantifish.co.nz/influ2/reference/cpue_index.md)
  [`print(`*`<influ_index>`*`)`](https://www.quantifish.co.nz/influ2/reference/cpue_index.md)
  : Calculate an assessment-ready CPUE index
- [`integrate_index()`](https://www.quantifish.co.nz/influ2/reference/integrate_index.md)
  : Calculate an area-integrated expected-response index
- [`plot_index()`](https://www.quantifish.co.nz/influ2/reference/plot_index.md)
  [`plot(`*`<influ_index>`*`)`](https://www.quantifish.co.nz/influ2/reference/plot_index.md)
  [`autoplot(`*`<influ_index>`*`)`](https://www.quantifish.co.nz/influ2/reference/plot_index.md)
  : Plot calculated CPUE indices
- [`geo_mean()`](https://www.quantifish.co.nz/influ2/reference/geo_mean.md)
  : Geometric mean

## Residual diagnostics

Inspect generalised residual patterns, grouped departures, and quantile
comparisons.

- [`influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/influ_residuals.md)
  [`print(`*`<influ_residuals>`*`)`](https://www.quantifish.co.nz/influ2/reference/influ_residuals.md)
  : Calculate compact simulation-based residual diagnostics
- [`as_influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/as_influ_residuals.md)
  : Diagnose externally simulated responses
- [`plot(`*`<influ_residuals>`*`)`](https://www.quantifish.co.nz/influ2/reference/plot.influ_residuals.md)
  [`autoplot(`*`<influ_residuals>`*`)`](https://www.quantifish.co.nz/influ2/reference/plot.influ_residuals.md)
  : Plot a four-panel CPUE residual diagnostic
- [`plot_implied_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot_implied_residuals.md)
  : Plot generalised residual departures by year and group
- [`plot_predicted_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot_predicted_residuals.md)
  : Plot predictive means against generalised residuals

## Model comparison

Model-specific likelihood and Bayesian criteria, with explicit
comparison safeguards.

- [`get_bayes_R2()`](https://www.quantifish.co.nz/influ2/reference/get_bayes_R2.md)
  : Summarise Bayesian R-squared for brms models
- [`table_criterion()`](https://www.quantifish.co.nz/influ2/reference/table_criterion.md)
  : Summarise criteria across CPUE model types

## Data

Simulated data supplied with the package.

- [`lobsters_per_pot`](https://www.quantifish.co.nz/influ2/reference/lobsters_per_pot.md)
  : Simulated CPUE data

## Package

- [`influ2`](https://www.quantifish.co.nz/influ2/reference/influ2.md) :
  Model-neutral influence diagnostics
