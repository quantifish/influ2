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

Compare indices, inspect data coverage, and diagnose fitted models.

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
- [`plot_implied_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot_implied_residuals.md)
  : Plot implied residual coefficients
- [`plot_predicted_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot_predicted_residuals.md)
  : Plot predicted values against residuals
- [`plot_qq()`](https://www.quantifish.co.nz/influ2/reference/plot_qq.md)
  : Quantile-quantile plot of model residuals

## Bayesian model comparison

Optional summaries for fitted BRMS models.

- [`get_bayes_R2()`](https://www.quantifish.co.nz/influ2/reference/get_bayes_R2.md)
  : Summarise Bayesian R-squared for BRMS models
- [`table_criterion()`](https://www.quantifish.co.nz/influ2/reference/table_criterion.md)
  : Compare BRMS model criteria

## Data

Simulated data supplied with the package.

- [`lobsters_per_pot`](https://www.quantifish.co.nz/influ2/reference/lobsters_per_pot.md)
  : Simulated CPUE data

## Package

- [`influ2`](https://www.quantifish.co.nz/influ2/reference/influ2.md) :
  Model-neutral influence diagnostics
