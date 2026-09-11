# Residual-implied effects

## Restore the original question

Residual-implied plots ask whether individual groups suggest a different
annual trajectory from the fitted model. The original comparison is
useful: show a common fitted annual pattern, then show how residual
departures in each group would adjust that pattern. These are
**exploratory implied effects**, not separately fitted interactions or
regional abundance indices.

[`plot_implied_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot_implied_residuals.md)
now defaults to a local **likelihood adjustment**. The original model’s
parameters, fitted random effects, smooths, dispersion, and exposure
offsets stay fixed. Only one additional effect-scale shift is estimated
in each supported group-year stratum. For a log-link count model, that
shift multiplies the expected response, while respecting the fitted
count distribution. No full model is refitted and no MCMC is run.

The zero-centred PIT summaries previously displayed under this name are
now available as
[`plot_grouped_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot_grouped_residuals.md).
That calculation has not changed. It answers a different question:
whether a group’s observations tend to lie high or low within their
predictive distributions. **Normal-score PIT residuals are not added to
model coefficients.**

## Three conventions, explicitly distinguished

| Calculation | Adjustment added to the selected fitted baseline | Interpretation |
|:---|:---|:---|
| `method = "likelihood"` | Local shift maximising the fitted response likelihood | Default effect-scale diagnostic |
| `method = "traditional"` | Mean ordinary log-response residual | Classical lognormal construction; constant log variance required |
| Traditional with `traditional_scale = "standardised"` | Mean globally centred [`rstandard()`](https://rdrr.io/r/stats/influence.measures.html) residual | Actual historical analyser GLM convention; retained for comparison, not a universal effect-scale calculation |

The [historical analyser
source](https://github.com/trophia/analyser/blob/a82c5b41bfd0ebeece8e1c6526465da006435497/R/diagnoser.r)
uses [`rstandard()`](https://rdrr.io/r/stats/influence.measures.html)
for GLMs, centres those residuals globally, and adds selected fitted
term contributions. Its `implieds()` helper can include the group main
effect as well as year. Thus “traditional” was not one universal
residual definition. Fisheries examples include Starr and Kendrick
([2019](#ref-StarrKendrick2019)) and Middleton
([2025](#ref-Middleton2025)).

The new calculation agrees with **ordinary log-residual arithmetic**
when the variance is constant; it does not generally equal the
historical standardised-residual arithmetic. The examples below
demonstrate both claims.

The baseline defaults to `"year_group"`: centred fixed year and group
main effects, where the latter is present. Choose `baseline = "year"`
for only the shared year term. All selected term contributions are
centred over the same fitted observations. They are not independently
re-centred in each panel. Other covariate effects, random effects, and
smooths stay in the observation predictions used to calculate the
adjustment, but are not added to this baseline.

## A simulated lobster design with differing group trends

We retain the uneven year/month coverage, depth, and soak time from
`lobsters_per_pot`, but simulate new responses for this comparison.
These are not modifications to the package dataset. Three seasonal
groups are defined from month, and their trends intentionally differ.
Vessel effects add repeated-observation structure. Both responses
represent CPUE per standardised sampling unit: one continuous positive
response and one count response.

``` r

data(lobsters_per_pot)
example <- lobsters_per_pot
example$season <- factor(ceiling(as.integer(example$month) / 4),
  labels = c("Early season", "Mid-season", "Late season"))
set.seed(12092026)
example$vessel <- factor(sample(1:24, nrow(example), replace = TRUE))
time <- as.integer(example$year)
trend <- time - mean(time)
vessel_effect <- rnorm(24, sd = .35)
eta <- 1.5 + .15 * sin(time / 2) - .015 * (example$depth - 40) +
  .008 * (example$soak - 30) + .15 * (as.integer(example$season) - 2) +
  .07 * trend * (as.integer(example$season) - 2) +
  vessel_effect[as.integer(example$vessel)]
example$cpue_positive <- exp(eta + rnorm(nrow(example), sd = .6))
example$count_cpue <- rnbinom(nrow(example), mu = exp(eta), size = 4)
```

### Constant log variance: matching the classical construction

Fit a Gaussian model to log CPUE, omitting the generating season-by-year
interaction. This is a lognormal response model with constant log-scale
variance, not a Gaussian model fitted directly to untransformed CPUE.

``` r

log_fit <- glmmTMB::glmmTMB(
  log(cpue_positive) ~ year + season + depth + soak + (1 | vessel),
  family = gaussian(), data = example
)
new_log <- implied_effects(log_fit, groups = "season", interval = "descriptive")
traditional_log <- implied_effects(log_fit, groups = "season", method = "traditional")
max(abs(new_log$table$estimate - traditional_log$table$estimate))
#> [1] 1.110223e-16
stopifnot(isTRUE(all.equal(new_log$table, traditional_log$table, tolerance = 1e-10)))
```

The estimates agree because the conditional Gaussian likelihood shift is
the mean log-response residual when the variance is constant. Here we
deliberately use **the same descriptive one-SE bars** for both displays,
so the whole plotted table agrees. Those bars are not full fitted-model
uncertainty intervals.

``` r

patchwork::wrap_plots(
  plot(traditional_log, ncol = 1) + labs(title = "Ordinary log-residual construction",
    subtitle = "Year + season | one descriptive SE", caption = NULL),
  plot(new_log, ncol = 1) + labs(title = "Likelihood adjustment",
    subtitle = "Year + season | one descriptive SE", caption = NULL), ncol = 2
)
```

![Side-by-side identical traditional log-residual and likelihood
implied-effect plots, each with three seasonal
panels.](implied-effects_files/figure-html/implied-log-agreement-1.png)

Ordinary log-residual implied effects (left) and the new
likelihood-based effects (right) for the same Gaussian log-CPUE glmmTMB
fit. Both use the same year-plus-season baseline, fitted vessel effects,
and descriptive one-SE bars. Points, trajectories, and bars agree
numerically to 1e-10. Grey lines are fixed baseline contributions, not
independently fitted seasonal indices. The deliberately omitted seasonal
trend is visible as departures from those baselines.

The optional `glmmTMB` examples are skipped if that package is
unavailable. This equality is not a promise that every lognormal
parameterisation behaves identically. For example, glmmTMB’s directly
specified `lognormal()` family models mean and SD on the response scale:
holding that SD fixed is different from holding log-scale SD fixed. This
first implied-effect implementation therefore supports the explicit
Gaussian-log-response route, not an automatic conversion of directly
parameterised lognormal models.

### Reproduce the actual historical standardised-residual recipe

For a direct source comparison, fit a plain Gaussian GLM to log CPUE:
that is one of the classes handled by the historical `Diagnoser`. Both
methods below use this **same** GLM. This comparison does not contrast a
fixed-effect fit with a mixed model.

``` r

historical_fit <- glm(log(cpue_positive) ~ year + season + depth + soak,
  family = gaussian(), data = example)
historical <- implied_effects(historical_fit, groups = "season",
  method = "traditional", traditional_scale = "standardised")
modern <- implied_effects(historical_fit, groups = "season")

# Independent reconstruction of the historical GLM arithmetic.
term_effects <- predict(historical_fit, type = "terms")
reference <- rowSums(term_effects[, c("year", "season")])
standardised <- rstandard(historical_fit)
standardised <- standardised - mean(standardised)
manual <- do.call(rbind, lapply(seq_len(nrow(historical$table)), function(j) {
  cell <- historical$table[j, ]
  keep <- example$year == cell$level & example$season == cell$group
  data.frame(estimate = mean(reference[keep] + standardised[keep]),
    std_error = sd(standardised[keep]) / sqrt(sum(keep)))
}))
max(abs(manual$estimate - historical$table$estimate))
#> [1] 2.220446e-16
stopifnot(isTRUE(all.equal(manual$estimate, historical$table$estimate, tolerance = 1e-10)))
stopifnot(isTRUE(all.equal(manual$std_error, historical$table$std_error, tolerance = 1e-10)))
```

``` r

patchwork::wrap_plots(
  plot(historical, ncol = 1) + labs(title = "Historical standardised convention",
    subtitle = "Year + season | one descriptive SE", caption = NULL),
  plot(modern, ncol = 1) + labs(title = "Likelihood adjustment",
    subtitle = "Year + season | 95% conditional intervals", caption = NULL), ncol = 2
)
```

![Historical standardised-residual and new likelihood implied-effect
plots show different adjustment scales and interval
definitions.](implied-effects_files/figure-html/implied-historical-comparison-1.png)

Actual historical analyser GLM convention (left) versus the new
likelihood-based implied effects (right), using the same Gaussian
log-CPUE GLM and baseline. Left: globally centred rstandard residuals
added to term contributions, with descriptive one-SE bars. Right: local
log-scale likelihood shifts, with 95% conditional profile-likelihood
intervals. Unlike the preceding ordinary-log-residual comparison, these
points and bars need not agree: both the residual scale and interval
definition differ. The standardised convention is retained as a labelled
historical comparison, not a physically interpretable CPUE multiplier.

This reproduces the source’s GLM arithmetic, not every version of
earlier influ2 or every fisheries report. In particular, it does not
revive the old brms posterior-array helper or infer what every report
meant by “standardised”.

## Negative-binomial implied effects

For an NB2 log-link model, the local shift maximises the
negative-binomial likelihood using the fitted size/dispersion. It is not
the average of PIT scores or a renamed Pearson residual. Within a
stratum, adding delta to the linear predictor multiplies all its fitted
means by `exp(delta)`.

``` r

nb_fit <- glmmTMB::glmmTMB(
  count_cpue ~ year + season + depth + soak + (1 | vessel),
  family = glmmTMB::nbinom2(), data = example
)
nb_implied <- implied_effects(nb_fit, groups = "season")
head(as.data.frame(nb_implied))
#>   level        group   n    baseline  adjustment    estimate  std_error
#> 1  2000 Early season 153  0.23187481  0.19752139  0.42939619 0.05137036
#> 2  2001 Early season 143  0.21491937  0.12104052  0.33595989 0.05376133
#> 3  2002 Early season 154  0.17412683  0.18936487  0.36349170 0.05169645
#> 4  2003 Early season 160  0.09752318  0.13935836  0.23688154 0.05120520
#> 5  2004 Early season  90 -0.06401734  0.13669553  0.07267819 0.06914226
#> 6  2005 Early season  67 -0.13942674 -0.02277379 -0.16220053 0.08505896
#>         lower      upper status
#> 1  0.32965956 0.53107330     ok
#> 2  0.23156150 0.44235353     ok
#> 3  0.26307818 0.46577048     ok
#> 4  0.13736603 0.33813090     ok
#> 5 -0.06142452 0.20971571     ok
#> 6 -0.32754176 0.00607398     ok
```

``` r

plot_implied_residuals(nb_implied)
```

![Three seasonal trajectories compare NB2 residual-implied effects with
their fitted annual
baselines.](implied-effects_files/figure-html/implied-nb-1.png)

New likelihood-based residual-implied effects for the NB2 glmmTMB
count-CPUE model, holding fitted vessel effects, dispersion, and all
original coefficients fixed. Grey lines show centred year-plus-season
contributions; purple trajectories add one local log-mean adjustment per
season-year cell. Bars are 95% conditional profile-likelihood intervals
for those adjustments, not uncertainty intervals for fully refitted
interactions. Point area represents sample size. No response simulations
or MCMC are used.

For routine use, the shortcut computes the same default result:

``` r

plot_implied_residuals(nb_fit, groups = "season")
plot_implied_residuals(log_fit, groups = "season", method = "traditional")
```

Keeping `nb_implied` separates calculation from styling and supports
save/reload without the model. Its table retains sparse, empty, and
boundary cells with explicit statuses. Lines never bridge omitted cells.
For an all-zero count cell, the likelihood optimum is a zero mean (log
shift `-Inf`); the table flags that boundary and the plot omits the
non-finite point, rather than inventing a finite value by adding a
pseudocount.

## Interpretation, uncertainty, and scope

The current adapters support `lm`, GLM, GAM, and ML glmmTMB for Gaussian
identity-link, Poisson log-link, and NB2 log-link models. Fitted GAM
smooths and mixed-model effects stay fixed. Traditional comparison is
restricted to constant-variance Gaussian models of `log(response)`; the
historical standardised option additionally requires a plain GLM.

Conditional profile intervals ignore uncertainty in the original model,
its baseline, and its estimated latent effects. Descriptive one-SE bars
also ignore dependence. Neither should be read as full uncertainty in a
regional index or an interaction. A suitable refit/bootstrap or
explicitly defined posterior propagation would be a further development,
not something these bars already provide.

Other families, direct lognormal parameterisations, brms, sdmTMB,
tinyVAST, non-unit weights, and year interactions currently fail
explicitly for this **new implied-effect calculation**. They remain
supported where documented by the existing PIT diagnostics and other
influ2 functions. Joint hurdle/delta models require separate decisions
about encounter, positive-response, and combined-response shifts; a
positive-component calculation is never silently substituted for a
combined-response diagnostic.

The tests independently reconstruct the historical recipe, check
log-response agreement, compare NB2 shifts and profile endpoints with
native density calculations, and cover offsets, fitted random effects,
GAMs, row alignment, unsupported cases, and compact save/reload. This
validates the implemented arithmetic, not universal scientific
calibration of implied-effect intervals.

## References

Middleton, D. A. J. 2025. *A Rapid Update of CPUE for the Snapper
Fishery in SNA 2 to 2024*. New Zealand Fisheries Assessment Report
2025/32. Fisheries New Zealand.
<https://www.mpi.govt.nz/dmsdocument/70215/direct>.

Starr, Paul J., and Terese H. Kendrick. 2019. *FLA 1 Fishery
Characterisation and CPUE*. New Zealand Fisheries Assessment Report
2019/09. Fisheries New Zealand.
