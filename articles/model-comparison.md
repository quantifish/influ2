# Model comparison

## One table, model-specific statistics

[`table_criterion()`](https://www.quantifish.co.nz/influ2/reference/table_criterion.md)
accepts one fitted model or a named list, including models from
different packages. It detects each model class and reports the criteria
that apply to that fit. Models occupy rows; statistics occupy separate
columns. An `NA` is not a poor score or a zero: the statistic can be
inappropriate, unavailable, or deliberately not calculated. Read the
`notes` column and `attr(result, "criteria_notes")` for the distinction.

This is complementary to
[`plot_compare()`](https://www.quantifish.co.nz/influ2/reference/plot_compare.md),
which compares the resulting CPUE indices. Good predictive performance
against fishing observations does not establish that an index tracks
abundance, particularly when extrapolating into poorly sampled areas.
Use the
[influence](https://www.quantifish.co.nz/influ2/articles/influ2.md),
[residual](https://www.quantifish.co.nz/influ2/articles/residual-diagnostics.md),
and
[CPUE-index](https://www.quantifish.co.nz/influ2/articles/cpue-indices.md)
articles alongside this table, not as replacements for one another.

## Likelihood comparisons using the lobster data

Use the same simulated lobster observations for every candidate. The
following models differ in their treatment of depth and month; they are
examples of alternative structures, not an automatic variable-selection
procedure.

``` r

data(lobsters_per_pot)
d <- lobsters_per_pot
models <- list(
  Poisson = glm(lobsters ~ year + month + depth + soak,
    data = d, family = poisson())
)
```

``` r

models$NB <- MASS::glm.nb(lobsters ~ year + month + depth + soak, data = d)
```

``` r

models$GAM <- mgcv::gam(lobsters ~ year + month + s(depth, k = 6) + soak,
  data = d, family = mgcv::nb(), method = "REML")
```

``` r

models$Mixed <- glmmTMB::glmmTMB(
  lobsters ~ year + poly(depth, 3) + soak + (1 | month),
  data = d, family = glmmTMB::nbinom2())
```

``` r

criteria <- table_criterion(models)
knitr::kable(criteria[c("Model", "Backend", "nobs", "df", "AIC", "AIC_type",
  "AIC_group", "delta_AIC")], digits = 2,
  caption = "Table 1. Native AIC values, penalty degrees of freedom, and compatible comparison groups.")
```

| Model   | Backend | nobs |    df |      AIC | AIC_type    | AIC_group | delta_AIC |
|:--------|:--------|-----:|------:|---------:|:------------|:----------|----------:|
| Poisson | GLM     | 5049 | 31.00 | 19415.09 | ordinary    | AIC-1     |   1695.32 |
| NB      | GLM     | 5049 | 32.00 | 17756.57 | ordinary    | AIC-1     |     36.81 |
| GAM     | GAM     | 5049 | 34.85 | 17719.76 | conditional | AIC-1     |      0.00 |
| Mixed   | glmmTMB | 5049 | 24.00 | 17755.57 | marginal    | AIC-2     |        NA |

Table 1. Native AIC values, penalty degrees of freedom, and compatible
comparison groups. {.table}

The GAM’s `df` is its native effective AIC penalty, including relevant
scale and family parameters. It is not the number of spline
coefficients, and is not the residual degrees of freedom. The mixed
model’s marginal AIC counts estimated non-latent parameters after
integrating random effects out. The two penalties describe different
prediction targets, so the GAM and mixed-model AIC rows are not
automatically ranked against one another ([Wood et al.
2016](#ref-WoodPya2016)).

An ordinary GLM has no random effects: its likelihood can be compared
with either target when the response and likelihood conventions agree.
It cannot, however, make incompatible GAM and mixed-model criteria
interchangeable. Groups form in input order. In this table the ordinary
models join the GAM group; compare `models[c("NB", "Mixed")]` separately
to obtain their marginal AIC difference. The same rule allows a GLM and
a glmmTMB model in one ranking, without pooling unlike criteria in a
larger mixed list.

Differences are calculated only within a verified group containing at
least two models. A singleton’s difference is `NA`, not zero. Matching
group labels require matching fitted row names, response values,
weights, response support, and shared model-frame values, as well as a
compatible likelihood target. Use stable row names identifying the same
fishing events: independently renumbered datasets can defeat a
provenance check. Equal sample sizes alone are not sufficient.
Unverified comparisons remain visible but unranked.

Input order is retained by default. `sort = "AIC"` is available for a
list whose rows all belong to one compatible AIC group. An incompatible
request warns and retains input order; there is no overall ranking
assembled from different columns.

## Conditional AIC is a different prediction target

Conditional AIC concerns new observations sharing the fitted random
effects, such as additional samples within an existing spatial field.
Marginal AIC concerns predictions with new random effects. The presence
of random effects alone does not make one criterion universally
preferable ([Zheng et al. 2024](#ref-Zheng2024)).

Request conditional AIC explicitly because spatial implementations can
need substantial Hessian calculations:

``` r

conditional <- table_criterion(models, criterion = c("AIC", "cAIC"))
knitr::kable(conditional[c("Model", "AIC", "cAIC", "df", "cAIC_df")],
  digits = 2, caption = "Table 2. Conditional criteria, where supported. Missing mixed-model cAIC is not replaced by ordinary AIC.")
```

| Model   |      AIC |     cAIC |    df | cAIC_df |
|:--------|---------:|---------:|------:|--------:|
| Poisson | 19415.09 | 19415.09 | 31.00 |   31.00 |
| NB      | 17756.57 | 17756.57 | 32.00 |   32.00 |
| GAM     | 17719.76 | 17719.76 | 34.85 |   34.85 |
| Mixed   | 17755.57 |       NA | 24.00 |      NA |

Table 2. Conditional criteria, where supported. Missing mixed-model cAIC
is not replaced by ordinary AIC. {.table}

- For an ordinary GLM without random effects, conditional AIC reduces to
  AIC.
- mgcv’s usual [`AIC()`](https://rdrr.io/r/stats/AIC.html) already uses
  a conditional formulation. The two columns therefore match; no second,
  supposedly improved score has been calculated. Its smoothing-parameter
  uncertainty correction is used when the native fit supplies it, and
  its availability is recorded in `notes`.
- glmmTMB mixed models currently have no supported native cAIC method in
  this interface. `cAIC` stays missing, with an explanation. This does
  not prevent comparing compatible glmmTMB models using their marginal
  AIC.
- sdmTMB and tinyVAST use their native approximate cAIC methods, based
  on Zheng, Cadigan, and Thorson ([Zheng et al. 2024](#ref-Zheng2024)).
  These are approximations, not exact cross-validation scores or generic
  guarantees for every hierarchy.

## sdmTMB and tinyVAST

Here a penalised depth smooth provides a compact random-effect example
in sdmTMB. Native cAIC returns the criterion, and native EDF summarises
the random-effect contribution. `cAIC_df` includes that contribution and
the non-random parameters counted by the native approximation.

``` r

sdm_fit <- sdmTMB::sdmTMB(
  lobsters ~ year + month + s(depth, k = 6) + soak,
  data = d, family = sdmTMB::nbinom2(), spatial = "off", silent = TRUE
)
sdm_criteria <- table_criterion(sdm_fit, c("AIC", "cAIC"))
knitr::kable(sdm_criteria[c("Backend", "df", "EDF_random", "cAIC_df", "AIC", "cAIC")],
  digits = 2, caption = "Table 3. Native marginal and conditional criteria for the sdmTMB smooth model.")
```

| Backend |  df | EDF_random | cAIC_df |      AIC |     cAIC |
|:--------|----:|-----------:|--------:|---------:|---------:|
| sdmTMB  |  33 |       3.06 |   36.06 | 17729.01 | 17722.51 |

Table 3. Native marginal and conditional criteria for the sdmTMB smooth
model. {.table}

The same interface accepts tinyVAST, including spatial and
spatiotemporal fits. This small example has no random field, so its cAIC
equals ordinary AIC. For a fitted field model, its native approximate
[`tinyVAST::cAIC()`](https://vast-lib.github.io/tinyVAST/reference/cAIC.html)
is used.

``` r

tiny_fit <- tinyVAST::tinyVAST(
  lobsters ~ year + month + depth + soak, data = d,
  family = tinyVAST::nbinom2(), spatial_domain = NULL,
  control = tinyVAST::tinyVASTcontrol(calculate_deviance_explained = FALSE)
)
tiny_criteria <- table_criterion(tiny_fit, c("AIC", "cAIC", "deviance"))
knitr::kable(tiny_criteria[c("Backend", "nobs", "df", "AIC", "cAIC", "deviance")],
  digits = 2, caption = "Table 4. Native likelihood and deviance summaries for the tinyVAST example.")
```

| Backend  | nobs |  df |      AIC |     cAIC | deviance |
|:---------|-----:|----:|---------:|---------:|---------:|
| tinyVAST | 5049 |  32 | 17756.57 | 17756.57 |  5481.02 |

Table 4. Native likelihood and deviance summaries for the tinyVAST
example. {.table}

For field models, tinyVAST’s native cAIC method currently returns only
the criterion. Its conditional effective penalty is not exposed:
`cAIC_df` and `EDF_random` remain missing, rather than being replaced by
marginal `df`. The [spatiotemporal
article](https://www.quantifish.co.nz/influ2/articles/spatial-spatiotemporal.md)
provides field-fitting and interpretation examples.

Conditional penalty counting has not yet been validated for profiled
spatial fits. Their cAIC is left unavailable with an explanation; native
marginal criteria remain available. No penalty adjustment is guessed.

TMB itself supplies objectives and automatic differentiation, not a
universal model-comparison contract. The interface therefore supports
the named fitted model classes, not an arbitrary `MakeADFun()` object
with an unknown likelihood.

## Bayesian models and a mixed summary table

For several complete brms fits, the same function reports LOOIC,
expected log predictive density, effective predictive complexity
`p_loo`, and Bayesian R². It also reports paired ELPD differences and
their standard errors when the models use aligned observations and their
Pareto-k diagnostics are reliable. These paired standard errors are not
the differences between individual-model standard errors. Bayesian R² is
a descriptive fit summary, not the automatic ranking criterion.

``` r

# Original complete fitted models, not compact influence-only fixtures:
bayesian <- table_criterion(list(Base = base_brms, Smooth = smooth_brms),
  criterion = c("loo", "loo_R2", "bayes_R2"))

mixed <- table_criterion(list(GLM = glm_fit, GAM = gam_fit, brms = full_brms))
```

The next table was calculated from a complete, previously fitted
four-chain brms model and a Gaussian GLM fitted to the same 150
simulated observations. This small continuous-response example is
separate from the lobster count example. The response follows year
effects plus a covariate effect and Gaussian noise. Both fits use
`y ~ year + x`; their estimation methods differ.

The reproducible preparation script is
`data-raw/model-criteria-example.R` in the repository. It can reuse the
complete fit or explicitly fit it once. The vignette stores only the
resulting compact table and metadata: no MCMC, complete Stan fit, or
observation-by-posterior array is needed during rendering.

``` r

example <- readRDS(system.file("extdata", "brms-criteria-example.rds", package = "influ2"))
knitr::kable(example$table[c("Model", "Backend", "nobs", "df", "AIC",
  "looic", "p_loo", "bayes_R2", "loo_R2", "pareto_k_max")], digits = 3,
  caption = "Table 5. An executed mixed frequentist/Bayesian summary. Missing entries identify different criterion definitions, not worse performance.")
```

| Model | Backend | nobs |  df |     AIC |   looic | p_loo | bayes_R2 | loo_R2 | pareto_k_max |
|:------|:--------|-----:|----:|--------:|--------:|------:|---------:|-------:|-------------:|
| GLM   | GLM     |  150 |   7 | 266.885 |      NA |    NA |       NA |     NA |           NA |
| brms  | brms    |  150 |  NA |      NA | 267.449 | 6.967 |    0.698 |  0.676 |        0.329 |

Table 5. An executed mixed frequentist/Bayesian summary. Missing entries
identify different criterion definitions, not worse performance.
{.table}

The AIC and LOOIC numbers are not compared to each other. Similarly,
ordinary, pseudo-, conditional, and Bayesian R² definitions must not be
put into one interchangeable column. High Pareto-k values leave the
reported LOO values visible but disable automatic ranking. Refitting or
moment matching is not silently triggered; it is a separate
analyst-controlled action.

## Deviance, convergence, and limitations

`deviance` is reported only when an appropriate native response deviance
is available. It is not replaced by `-2 * logLik`, and it is not used to
rank models across different response distributions. Combined
zero-inflated glmmTMB models do not silently receive a
conditional-component deviance. sdmTMB delta deviances are labelled as
native sums of component deviances. The function does not fit a null
model to obtain deviance explained.

`converged` and `pdHess` report available optimiser and Hessian
information; missing flags mean unavailable, not success. These are not
substitutes for gradient, identifiability, or MCMC diagnostics. Failed
fits, parameter-prior fits, and restricted-likelihood fits are excluded
from automatic rankings. Native REML-based numbers remain inspectable,
but cannot rank different fixed-effect structures. mgcv’s conditional
AIC is distinct from its REML smoothing-selection objective.

LOO-PIT remains a predictive-calibration diagnostic, not a ranking
score, and is outside this interface. Shared held-out comparisons across
Bayesian and frequentist backends need an explicit scoring rule and
common vessel, spatial, or temporal folds. Neither that refitting
workflow nor universal LOO-PIT is performed by
[`table_criterion()`](https://www.quantifish.co.nz/influ2/reference/table_criterion.md).

## References

Wood, Simon N., Natalya Pya, and Benjamin Saefken. 2016. “Smoothing
Parameter and Model Selection for General Smooth Models.” *Journal of
the American Statistical Association* 111 (516): 1548–75.
<https://doi.org/10.1080/01621459.2016.1180986>.

Zheng, Nan, Noel Cadigan, and James T. Thorson. 2024. *A Note on
Numerical Evaluation of Conditional Akaike Information for Nonlinear
Mixed-Effects Models*. <https://doi.org/10.48550/arXiv.2411.14185>.
