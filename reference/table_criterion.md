# Summarise criteria across CPUE model types

A model-specific summary table for GLMs, mgcv GAMs, glmmTMB, brms,
sdmTMB, and tinyVAST. Different criteria occupy different columns;
missing or inappropriate criteria are never replaced by another
statistic.

## Usage

``` r
table_criterion(fits, criterion = "auto", sort = FALSE, labels = NULL, ...)
```

## Arguments

- fits:

  One supported fitted model, or a non-empty list of models. A list may
  mix backends. Complete brms fits are required.

- criterion:

  Character vector, case-insensitive: `"auto"`, `"AIC"`, `"BIC"`,
  `"cAIC"`, `"logLik"`, `"deviance"`, `"loo"`, `"loo_R2"`, `"bayes_R2"`,
  or `"log_lik"`. `"auto"` requests native likelihood-based summaries
  for frequentist models and LOO/Bayesian R-squared for brms. Add
  `"cAIC"` explicitly: native spatial Hessian calculations can be
  costly. `"log_lik"` is the posterior mean total log likelihood, not ML
  `"logLik"`.

- sort:

  Preserve input order by default. `TRUE` sorts only when every row
  belongs to one verified comparison group, using LOOIC, AIC, or cAIC in
  that order. Alternatively specify one of those column names. No
  sorting combines different criteria, and R-squared is not a ranking
  rule.

- labels:

  Optional unique, non-empty model labels. Otherwise list names are
  used, or labels are generated from the list position.

- ...:

  Arguments passed only to requested brms criterion functions. These
  arguments cannot select a response or prediction subset: comparison
  metadata describe the complete fitted response. Refitting options are
  rejected; this function never launches MCMC or cross-validation
  refits.

## Value

A data frame with model labels, backend/family/likelihood metadata,
sample size, degrees of freedom, convergence flags, requested
statistics, criterion-specific comparison groups and differences, and
readable `notes`. Inapplicable or unavailable values are `NA`, with
reasons in `notes` and the `criteria_notes` attribute (model, criterion,
status, and detail). No full fits, posterior draws, or pointwise
likelihood arrays are retained.

## Details

`df` is the native log-likelihood parameter count or effective penalty
degrees of freedom, not simply the number of regression coefficients.
`df_residual` is reported separately where provided by the backend.
`cAIC_df` is the effective penalty used by the conditional criterion,
when available; `EDF_random` is sdmTMB's summed random-effect EDF.

mgcv's ordinary [`AIC()`](https://rdrr.io/r/stats/AIC.html) already uses
a conditional formulation, with its smoothing-parameter uncertainty
correction when available. Its `cAIC` column therefore equals native
AIC; it is not an independent criterion. sdmTMB and tinyVAST use their
own approximate `cAIC()` methods. glmmTMB has no supported native
conditional-AIC method here. No generic mixed-model correction, cAIC4
conversion, or refitting workaround is substituted. For an ordinary
model without random effects, cAIC reduces to native AIC. Profiled
spatial fits are not yet validated for conditional penalty counting;
their cAIC is unavailable, while native marginal criteria remain.
Restricted-likelihood and penalised/prior fits are flagged and excluded
from automatic ranking. In particular, REML objective values must not
rank different fixed-effect structures. mgcv's conditional AIC is not
its REML smoothing-selection objective.

AIC/BIC and cAIC comparison groups distinguish conditional observation
likelihoods from marginal likelihoods. Ordinary models with no random
effects can join either target, but cannot bridge incompatible targets.
Groups are formed in input order; compare a subset separately to examine
an ordinary model against another target. They require aligned fitted
row names, responses, weights, compatible response support, and matching
values of shared model-frame columns. Row names must genuinely identify
the same observations: these checks cannot establish provenance for
independently renumbered datasets. Unsupported or unverifiable cases get
separate groups and no differences. Native deviances are descriptive,
backend-specific quantities, not a universal scale for ranking families
or model classes.

brms LOO comparisons use paired pointwise ELPD differences and their
standard errors through
[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html).
Pareto-k diagnostics are reported; unreliable LOO estimates are not
ranked. Posterior R-squared standard deviations retain the earlier
`se_bayes_R2`/`se_loo_R2` names, but are posterior uncertainty, not
Monte Carlo standard errors.

Conditional criteria target new observations sharing fitted latent
effects; marginal criteria target new random effects. Neither
establishes that an abundance index is unbiased or robust to
extrapolation. LOO-PIT and refitted cross-validation are separate
workflows, not implemented here.

## References

Wood, Pya, and Saefken (2016). Smoothing parameter and model selection
for general smooth models.
[doi:10.1080/01621459.2016.1180986](https://doi.org/10.1080/01621459.2016.1180986)
.

Zheng, Cadigan, and Thorson (2024). A note on numerical evaluation of
conditional Akaike information for nonlinear mixed-effects models.
[doi:10.48550/arXiv.2411.14185](https://doi.org/10.48550/arXiv.2411.14185)
.

## See also

[`get_bayes_R2()`](https://www.quantifish.co.nz/influ2/reference/get_bayes_R2.md),
[`plot_compare()`](https://www.quantifish.co.nz/influ2/reference/plot_compare.md)

## Examples

``` r
a <- glm(mpg ~ wt, data = mtcars, family = gaussian())
b <- glm(mpg ~ wt + hp, data = mtcars, family = gaussian())
result <- table_criterion(list(Weight = a, Weight_and_power = b))
result[c("Model", "df", "AIC", "delta_AIC")]
#>              Model df      AIC delta_AIC
#> 1           Weight  3 166.0294   9.37709
#> 2 Weight_and_power  4 156.6523   0.00000
```
