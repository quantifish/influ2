# Search a constrained set of CPUE models

A bounded, buildmer-style search using each backend's native fitter and
information criteria. The default is a preflight report: fitting
requires the explicit argument `refit = TRUE`.

## Usage

``` r
search_cpue(
  model,
  year = NULL,
  scope = NULL,
  include = NULL,
  optional = NULL,
  direction = c("order", "backward"),
  criterion = "auto",
  max_models = 50,
  refit = FALSE,
  data = NULL,
  component = NULL,
  keep_fits = "best",
  refit_args = list(),
  seed = NULL,
  blocks = NULL
)

# S3 method for class 'influ_search'
print(x, ...)

# S3 method for class 'influ_search'
summary(object, ...)

# S3 method for class 'influ_search'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- model:

  One fitted GLM, mgcv GAM, glmmTMB, lme4, brms, sdmTMB, or tinyVAST
  model. Each search uses one backend and one family/link.

- year:

  Optional name of the annual variable. Otherwise the annual variable is
  detected using the same conventions as other CPUE interfaces.

- scope:

  Optional upper formula, or a list with `lower` and `upper` formulas.
  The fitted formula is the default upper scope. Required terms are
  retained even when omitted from a supplied scope.

- include:

  Additional terms to retain, as a one-sided formula or character
  vector. For annual interaction coding, explicitly include the base
  annual block, for example `~ fyear:season`.

- optional:

  Optional formula or character vector naming the only terms to search.
  Other terms remain protected. This can explicitly nominate
  non-essential random effects; annual base terms and offsets cannot be
  released. Without this argument, ordinary covariates are optional,
  while random effects and multidimensional smooths are protected.

- direction:

  `"forward"`, `"backward"`, `"both"`, or `c("order", "backward")`. The
  latter builds an admissible maximal model by adding converged blocks
  to the protected core, then prunes it. Ordering can add terms even
  when the criterion increases. Other moves must improve the criterion
  by more than a numerical tolerance of `1e-6`.

- criterion:

  `"auto"`, `"AIC"`, `"BIC"`, `"cAIC"`, or `"LOOIC"` (also `"loo"`).
  Automatic selection uses AIC for frequentist backends and LOOIC for
  brms. Native cAIC is only available for supported backends; no
  replacement mixed-model penalty is invented.

- max_models:

  Positive integer limit on distinct evaluated candidate specifications,
  including a reused original fit. Failed candidates count.

- refit:

  Logical; `FALSE` reports the constraints and fitting plan without
  optimisation, MCMC, or criterion calculation.

- data:

  Optional original raw data, required when the saved call cannot
  recover them. Fitted rows, responses, weights, and offsets are
  checked.

- component:

  For a joint sdmTMB delta model, explicitly select `"occurrence"` or
  `"positive"`; the other component stays fixed.

- keep_fits:

  `"best"` (default), `"all"`, or `"none"`. Native fit objects can be
  large; candidate tables and paths are always compact.

- refit_args:

  Named list of native execution controls. Structural changes to
  families, data, offsets, priors, meshes, or process definitions are
  rejected. For lme4 negative-binomial models, explicitly state
  `reestimate_nb = TRUE` or `FALSE`.

- seed:

  Optional non-negative integer. Evaluation seeds are recorded; a reused
  fit retains its original draws. The caller's random-number state is
  restored on success or failure.

- blocks:

  Optional named list of disjoint term groups, moved together. Members
  use term labels, not coefficient names.

- x, object:

  An \`influ_search\` object.

- ...:

  Reserved for additional method arguments.

- row.names:

  Optional row names for the data-frame method. Its generic \`optional\`
  argument is reserved and does not change the retained table.

## Value

An `influ_search` object with `terms`, `candidates`, `path`, `metadata`,
and the retained `best_model` (unless `keep_fits = "none"`).
[`selected_model()`](https://www.quantifish.co.nz/influ2/reference/selected_model.md)
retrieves the recommended native fit. Printing, summaries, and plotting
use stored results without new fits or simulations.

## Details

Year, response definition, offsets, weights, and essential
random/spatial structure are fixed specifications, not fixed numerical
estimates. Estimated coefficients, variance/dispersion parameters, and
latent effects are re-estimated in each candidate, except parameters
deliberately fixed in the original native specification. Formula terms
are atomic, and interaction dependencies present in the upper scope are
respected. New main effects are not invented for intentional
interaction-only annual coding.

Gaussian mixed models fitted by REML are refitted using ML for searching
fixed effects; this conversion is recorded. mgcv's native AIC is not its
REML smoothing objective. Priors/penalties that invalidate ordinary
maximum-likelihood ranking, missing convergence evidence, unreliable
brms LOO estimates, and incompatible observations/likelihood targets
make a candidate ineligible, rather than silently switching criteria.

Every distinct specification is evaluated at most once. The
recommendation is the best eligible evaluated model, not a guaranteed
global optimum. Search stopping, warnings, singular fits, and failed
fits remain visible. There is no production refitting, search-driven
residual simulation, model averaging, or automatic spatial-process
selection. Advanced nonlinear or distributional formulas that cannot
safely retain auxiliary specifications fail explicitly. Positional
parameter maps are not automatically remapped as terms change:
explicitly mapped TMB models require separately specified candidates
instead. Stored sdmTMB latent-process formulas and native trial counts
are retained, rather than re-evaluated from changed call variables. Use
[`check_cpue()`](https://www.quantifish.co.nz/influ2/reference/check_cpue.md)
on the shortlist separately: residual p-values are not term-removal
rules. Post-selection intervals remain conditional on the chosen model
and do not include selection uncertainty. Compact retention limits
stored results, not native fitting, LOO, or Hessian workspace. In
particular, brms criterion calculations can still require substantial
temporary memory.

## References

Voeten (2020). buildmer: Stepwise elimination and term ordering for
mixed models. <https://cran.r-project.org/package=buildmer>.

## See also

[`table_criterion()`](https://www.quantifish.co.nz/influ2/reference/table_criterion.md),
[`check_cpue()`](https://www.quantifish.co.nz/influ2/reference/check_cpue.md),
[`influ_steps()`](https://www.quantifish.co.nz/influ2/reference/influ_steps.md),
[`cpue_index()`](https://www.quantifish.co.nz/influ2/reference/cpue_index.md)

## Examples

``` r
set.seed(12)
d <- data.frame(year = factor(rep(2020:2023, each = 15)), x = rnorm(60))
d$response <- rpois(60, exp(1 + 0.5 * d$x))
fit <- glm(response ~ year + x, data = d, family = poisson())
search_cpue(fit, year = "year") # preflight only
#> <influ_search> glm / AIC
#>   Mode: preflight only
#>   Protected terms: 1
#>   Evaluated candidates: 0
#>   Stop: preflight_only
result <- search_cpue(fit, year = "year", direction = "backward", refit = TRUE)
result$candidates[c("candidate", "formula", "score", "status")]
#>   candidate                 formula    score   status
#> 1         1 response ~ (year) + (x) 219.6487 eligible
#> 2         2       response ~ (year) 262.7810 eligible
selected_model(result)
#> 
#> Call:  glm(formula = response ~ year + x, family = poisson(), data = d)
#> 
#> Coefficients:
#> (Intercept)     year2021     year2022     year2023            x  
#>     1.24374     -0.04885     -0.47464     -0.21159      0.55398  
#> 
#> Degrees of Freedom: 59 Total (i.e. Null);  55 Residual
#> Null Deviance:       100.6 
#> Residual Deviance: 47.38     AIC: 219.6
```
