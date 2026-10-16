# Package index

## Model-neutral diagnostics

Calculate and inspect influence diagnostics. The [plot() and
autoplot()](https://www.quantifish.co.nz/influ2/reference/plot.influ_diag.md)
methods here display `influ_diag` objects, including influence and CDI
plots.

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
- [`influ(`*`<merMod>`*`)`](https://www.quantifish.co.nz/influ2/reference/influ.merMod.md)
  [`influ(`*`<lmerMod>`*`)`](https://www.quantifish.co.nz/influ2/reference/influ.merMod.md)
  [`influ(`*`<glmerMod>`*`)`](https://www.quantifish.co.nz/influ2/reference/influ.merMod.md)
  : Influence diagnostics for lme4 mixed models
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
- [`plot_cdi()`](https://www.quantifish.co.nz/influ2/reference/plot_cdi.md)
  : Overlay coefficient, distribution, and influence diagnostics

## Additional diagnostics and comparisons

Compare indices and model steps, and inspect sampling composition and
data coverage. The [plot() and
autoplot()](https://www.quantifish.co.nz/influ2/reference/plot.influ_steps.md)
methods here display `influ_steps` objects.

- [`plot_bubble()`](https://www.quantifish.co.nz/influ2/reference/plot_bubble.md)
  : Bubble plot of sampling composition
- [`plot_compare()`](https://www.quantifish.co.nz/influ2/reference/plot_compare.md)
  : Compare standardised CPUE indices
- [`index_ratio()`](https://www.quantifish.co.nz/influ2/reference/index_ratio.md)
  [`as.data.frame(`*`<influ_index_ratio>`*`)`](https://www.quantifish.co.nz/influ2/reference/index_ratio.md)
  [`print(`*`<influ_index_ratio>`*`)`](https://www.quantifish.co.nz/influ2/reference/index_ratio.md)
  [`summary(`*`<influ_index_ratio>`*`)`](https://www.quantifish.co.nz/influ2/reference/index_ratio.md)
  : Calculate point-estimate ratios between stored annual indices
- [`plot_index_ratio()`](https://www.quantifish.co.nz/influ2/reference/plot_index_ratio.md)
  [`plot(`*`<influ_index_ratio>`*`)`](https://www.quantifish.co.nz/influ2/reference/plot_index_ratio.md)
  [`autoplot(`*`<influ_index_ratio>`*`)`](https://www.quantifish.co.nz/influ2/reference/plot_index_ratio.md)
  : Plot a separate index-ratio diagnostic
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

Calculate standardised means or area-integrated totals. Use
[plot_index(), plot(), or
autoplot()](https://www.quantifish.co.nz/influ2/reference/plot_index.md)
to display the resulting `influ_index` objects.

- [`cpue_index()`](https://www.quantifish.co.nz/influ2/reference/cpue_index.md)
  [`as.data.frame(`*`<influ_index>`*`)`](https://www.quantifish.co.nz/influ2/reference/cpue_index.md)
  [`print(`*`<influ_index>`*`)`](https://www.quantifish.co.nz/influ2/reference/cpue_index.md)
  : Calculate an assessment-ready CPUE index
- [`integrate_index()`](https://www.quantifish.co.nz/influ2/reference/integrate_index.md)
  : Calculate an area-integrated expected-response index
- [`index_domain()`](https://www.quantifish.co.nz/influ2/reference/index_domain.md)
  [`as.data.frame(`*`<influ_domain>`*`)`](https://www.quantifish.co.nz/influ2/reference/index_domain.md)
  [`summary(`*`<influ_domain>`*`)`](https://www.quantifish.co.nz/influ2/reference/index_domain.md)
  [`print(`*`<influ_domain>`*`)`](https://www.quantifish.co.nz/influ2/reference/index_domain.md)
  [`plot(`*`<influ_domain>`*`)`](https://www.quantifish.co.nz/influ2/reference/index_domain.md)
  [`autoplot(`*`<influ_domain>`*`)`](https://www.quantifish.co.nz/influ2/reference/index_domain.md)
  : Diagnose prediction-domain support and index dependence
- [`index_table()`](https://www.quantifish.co.nz/influ2/reference/index_table.md)
  : Format a calculated index for assessment reporting
- [`index_vcov()`](https://www.quantifish.co.nz/influ2/reference/index_vcov.md)
  [`vcov(`*`<influ_index>`*`)`](https://www.quantifish.co.nz/influ2/reference/index_vcov.md)
  : Extract covariance among calculated annual indices
- [`plot_index()`](https://www.quantifish.co.nz/influ2/reference/plot_index.md)
  [`plot(`*`<influ_index>`*`)`](https://www.quantifish.co.nz/influ2/reference/plot_index.md)
  [`autoplot(`*`<influ_index>`*`)`](https://www.quantifish.co.nz/influ2/reference/plot_index.md)
  : Plot calculated CPUE indices
- [`geo_mean()`](https://www.quantifish.co.nz/influ2/reference/geo_mean.md)
  : Geometric mean

## Residual diagnostics

Inspect generalised residual patterns, grouped departures, and quantile
comparisons. The [plot() and
autoplot()](https://www.quantifish.co.nz/influ2/reference/plot.influ_residuals.md)
methods here display `influ_residuals` objects.

- [`check_cpue()`](https://www.quantifish.co.nz/influ2/reference/check_cpue.md)
  [`print(`*`<influ_check>`*`)`](https://www.quantifish.co.nz/influ2/reference/check_cpue.md)
  [`summary(`*`<influ_check>`*`)`](https://www.quantifish.co.nz/influ2/reference/check_cpue.md)
  [`as.data.frame(`*`<influ_check>`*`)`](https://www.quantifish.co.nz/influ2/reference/check_cpue.md)
  : Check a CPUE model, overall and term by term
- [`plot(`*`<influ_check>`*`)`](https://www.quantifish.co.nz/influ2/reference/plot.influ_check.md)
  [`autoplot(`*`<influ_check>`*`)`](https://www.quantifish.co.nz/influ2/reference/plot.influ_check.md)
  : Plot stored CPUE model-check results
- [`influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/influ_residuals.md)
  [`print(`*`<influ_residuals>`*`)`](https://www.quantifish.co.nz/influ2/reference/influ_residuals.md)
  : Calculate compact simulation-based residual diagnostics
- [`as_influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/as_influ_residuals.md)
  : Diagnose externally simulated responses
- [`plot(`*`<influ_residuals>`*`)`](https://www.quantifish.co.nz/influ2/reference/plot.influ_residuals.md)
  [`autoplot(`*`<influ_residuals>`*`)`](https://www.quantifish.co.nz/influ2/reference/plot.influ_residuals.md)
  : Plot a four-panel CPUE residual diagnostic
- [`plot_implied_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot_implied_residuals.md)
  [`plot(`*`<influ_implied>`*`)`](https://www.quantifish.co.nz/influ2/reference/plot_implied_residuals.md)
  [`autoplot(`*`<influ_implied>`*`)`](https://www.quantifish.co.nz/influ2/reference/plot_implied_residuals.md)
  : Plot residual-implied annual effects
- [`implied_effects()`](https://www.quantifish.co.nz/influ2/reference/implied_effects.md)
  [`print(`*`<influ_implied>`*`)`](https://www.quantifish.co.nz/influ2/reference/implied_effects.md)
  [`as.data.frame(`*`<influ_implied>`*`)`](https://www.quantifish.co.nz/influ2/reference/implied_effects.md)
  : Calculate residual-implied effects by year and group
- [`plot_grouped_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot_grouped_residuals.md)
  : Plot generalised residual departures by year and group
- [`plot_predicted_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot_predicted_residuals.md)
  : Plot predictive means against generalised residuals

## Model comparison and search

Model-specific criteria and constrained native refitting, with explicit
comparison safeguards.

- [`get_bayes_R2()`](https://www.quantifish.co.nz/influ2/reference/get_bayes_R2.md)
  : Summarise Bayesian R-squared for brms models
- [`table_criterion()`](https://www.quantifish.co.nz/influ2/reference/table_criterion.md)
  : Summarise criteria across CPUE model types
- [`search_cpue()`](https://www.quantifish.co.nz/influ2/reference/search_cpue.md)
  [`print(`*`<influ_search>`*`)`](https://www.quantifish.co.nz/influ2/reference/search_cpue.md)
  [`summary(`*`<influ_search>`*`)`](https://www.quantifish.co.nz/influ2/reference/search_cpue.md)
  [`as.data.frame(`*`<influ_search>`*`)`](https://www.quantifish.co.nz/influ2/reference/search_cpue.md)
  : Search a constrained set of CPUE models
- [`selected_model()`](https://www.quantifish.co.nz/influ2/reference/selected_model.md)
  : Retrieve the recommended model from a CPUE search
- [`plot(`*`<influ_search>`*`)`](https://www.quantifish.co.nz/influ2/reference/plot.influ_search.md)
  [`autoplot(`*`<influ_search>`*`)`](https://www.quantifish.co.nz/influ2/reference/plot.influ_search.md)
  : Plot a stored CPUE model search

## Data

Simulated data supplied with the package.

- [`lobsters_per_pot`](https://www.quantifish.co.nz/influ2/reference/lobsters_per_pot.md)
  : Simulated CPUE data

## Package

- [`influ2`](https://www.quantifish.co.nz/influ2/reference/influ2.md) :
  Model-neutral influence diagnostics
