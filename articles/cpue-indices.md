# CPUE indices

## An assessment-ready table

[`cpue_index()`](https://www.quantifish.co.nz/influ2/reference/cpue_index.md)
calculates the expected response for a fixed reference population in
every year. It produces the familiar assessment columns: `Year`, `Mean`,
`Median`, `SD`, `CV`, `Qlower`, and `Qupper`, alongside method,
distribution, and link metadata. The result is an `influ_index` object;
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) gives the
table for reporting or CSV export.

This is a different quantity from the centred **year-effect contrasts**
in the influence and step plots. Both are useful, but neither is
automatically an area-integrated abundance or biomass index.

We use the same simulated lobster catches as the [main
article](https://www.quantifish.co.nz/influ2/articles/influ2.md). The
model below includes a monthly random intercept and a negative-binomial
observation distribution. No simulation of new catches is needed for its
index.

``` r

data(lobsters_per_pot)
lobster_model <- glmmTMB::glmmTMB(
  lobsters ~ year + poly(depth, 3) + poly(soak, 3) + (1 | month),
  family = glmmTMB::nbinom2(), data = lobsters_per_pot
)
```

We explicitly choose median observed depth and a 24-hour soak. The year
column is omitted:
[`cpue_index()`](https://www.quantifish.co.nz/influ2/reference/cpue_index.md)
repeats this same profile in every observed year. The monthly random
effect is set to zero. That represents a zero-effect month on the link
scale, **not** the average response across the population of months.
These choices belong to the scientific definition of the index.

``` r

reference <- data.frame(depth = median(lobsters_per_pot$depth), soak = 24)
lobster_index <- cpue_index(
  lobster_model, year = "year", method = "standardised",
  reference_data = reference, units = "lobsters per pot"
)
knitr::kable(as.data.frame(lobster_index), digits = 3)
```

| Year |  Mean | Median |    SD |    CV | Qlower | Qupper | Method       | Distribution | Link |
|:-----|------:|-------:|------:|------:|-------:|-------:|:-------------|:-------------|:-----|
| 2000 | 1.674 |     NA | 0.238 | 0.142 |  1.267 |  2.212 | standardised | nbinom2      | log  |
| 2001 | 1.744 |     NA | 0.247 | 0.142 |  1.321 |  2.303 | standardised | nbinom2      | log  |
| 2002 | 1.815 |     NA | 0.254 | 0.140 |  1.380 |  2.387 | standardised | nbinom2      | log  |
| 2003 | 1.387 |     NA | 0.189 | 0.136 |  1.062 |  1.812 | standardised | nbinom2      | log  |
| 2004 | 1.572 |     NA | 0.214 | 0.136 |  1.204 |  2.053 | standardised | nbinom2      | log  |
| 2005 | 1.310 |     NA | 0.185 | 0.141 |  0.993 |  1.729 | standardised | nbinom2      | log  |
| 2006 | 1.167 |     NA | 0.158 | 0.136 |  0.895 |  1.522 | standardised | nbinom2      | log  |
| 2007 | 1.184 |     NA | 0.161 | 0.136 |  0.907 |  1.545 | standardised | nbinom2      | log  |
| 2008 | 1.172 |     NA | 0.164 | 0.140 |  0.891 |  1.541 | standardised | nbinom2      | log  |
| 2009 | 1.123 |     NA | 0.157 | 0.140 |  0.853 |  1.477 | standardised | nbinom2      | log  |
| 2010 | 1.219 |     NA | 0.167 | 0.137 |  0.932 |  1.595 | standardised | nbinom2      | log  |
| 2011 | 1.379 |     NA | 0.189 | 0.137 |  1.055 |  1.803 | standardised | nbinom2      | log  |
| 2012 | 1.214 |     NA | 0.170 | 0.140 |  0.922 |  1.598 | standardised | nbinom2      | log  |
| 2013 | 1.372 |     NA | 0.187 | 0.136 |  1.051 |  1.792 | standardised | nbinom2      | log  |
| 2014 | 1.259 |     NA | 0.171 | 0.136 |  0.965 |  1.642 | standardised | nbinom2      | log  |
| 2015 | 1.067 |     NA | 0.146 | 0.137 |  0.816 |  1.395 | standardised | nbinom2      | log  |
| 2016 | 1.094 |     NA | 0.148 | 0.135 |  0.840 |  1.426 | standardised | nbinom2      | log  |
| 2017 | 1.118 |     NA | 0.153 | 0.137 |  0.855 |  1.462 | standardised | nbinom2      | log  |

`method = "standardized"` works identically. There is no need to refit a
model when printing, exporting, or plotting this result.

``` r

plot_index(lobster_index)
```

![Standardised expected lobsters per pot at the stated depth and
soak-time reference profile, with the monthly random effect set to zero.
The ribbon is a pointwise 95% delta-method confidence interval for the
index, not the spread of individual
catches.](cpue-indices_files/figure-html/index-plot-1.png)

Standardised expected lobsters per pot at the stated depth and soak-time
reference profile, with the monthly random effect set to zero. The
ribbon is a pointwise 95% delta-method confidence interval for the
index, not the spread of individual catches.

## What do the uncertainty columns mean?

For GLMs, GAMs, and glmmTMB, `Mean` is the fitted expected-response
estimate; `SD` is its propagated standard error. `CV` is `SD / Mean`
when the estimate is positive. `Median` is deliberately missing: an MLE
is not a posterior median. Positive estimates use log-scale delta-method
intervals.

For a complete brms fit, `Mean`, `Median`, `SD`, and the interval
columns summarise posterior draws of the **expected-response index**.
They do not describe the mean and median of individual catches. The
native
[`posterior_epred()`](https://paulbuerkner.com/brms/reference/posterior_epred.brmsfit.html)
calculation combines the distributional parameters, including zero and
positive components where appropriate, within each draw.

``` r

# Use an existing complete fit, not the compact influence-vignette fixture.
bayesian_index <- cpue_index(
  complete_brms_fit, year = "year", reference_data = reference,
  ndraws = 1000, batch_size = 250, draw_batch_size = 100,
  retain = "summary"
)
as.data.frame(bayesian_index)
```

Prediction batches preserve the same draw identities across years and
reference rows. Only the annual summaries are retained by default.
Setting `retain = "draws"` retains a draw-by-year matrix, not the much
larger reference-row-by-draw prediction matrix. The fitted model and
reference data are not embedded in the output object. Native prediction
routines can still allocate their own working memory.

`uncertainty = "none"` omits uncertainty summaries. For frequentist
models this also avoids covariance propagation. For brms it still
averages existing posterior expected-response draws, so it is not a
shortcut that substitutes posterior-mean coefficients and changes the
quantity being estimated.

## A common reference population and relative scaling

Supply multiple reference rows and explicit weights to standardise over
a population. Predictions are averaged on the response scale, after
applying the inverse link, using the same weights in each year. The
function never chooses missing factor values or an exposure variable on
the user’s behalf.

``` r

population <- data.frame(depth = c(25, 45, 65), soak = 24)
relative_index <- cpue_index(
  lobster_model, year = "year", reference_data = population,
  reference_weights = c(0.2, 0.5, 0.3), rescale = 1
)
geo_mean(relative_index$table$Mean)
#> [1] 1
```

The weights here are illustrative population proportions, **not cell
areas**. For models of catch with an exposure offset, put the offset in
the formula and explicitly supply the desired exposure in the reference
data. For a per-pot index from a catch model, this would usually be one
pot. This example already models catches from individual pots.

`rescale = 1` normalises across all returned years. Frequentist
uncertainty includes covariance with that denominator. For brms, each
complete annual draw is normalised before summarising; the geometric
mean of the resulting posterior-mean column need not be exactly one. Do
not separately normalise the mean, median, and interval columns: that
changes their relative scales.

[`geo_mean()`](https://www.quantifish.co.nz/influ2/reference/geo_mean.md)
is also available independently. It calculates on the log scale to avoid
numerical overflow, includes zeros, and supports `na.rm = TRUE`.
Negative values and infinities are rejected.

## Comparing models

[`plot_compare()`](https://www.quantifish.co.nz/influ2/reference/plot_compare.md)
accepts calculated `influ_index` objects. Use the same response
definition, reference population, weights, exposure, and scale.
Automatic metadata checks cannot establish scientific comparability.

``` r

linear_model <- glmmTMB::glmmTMB(
  lobsters ~ year + depth + soak + (1 | month),
  family = glmmTMB::nbinom2(), data = lobsters_per_pot
)
linear_index <- cpue_index(linear_model, reference_data = reference,
  units = "lobsters per pot")
plot_compare(list(Curved = lobster_index, Linear = linear_index))
```

![Expected-response indices from two negative-binomial glmmTMB
candidates at the same reference profile. Both use the same monthly
random-effect convention and response units. Intervals describe each
index separately, not uncertainty in the difference between the
models.](cpue-indices_files/figure-html/index-model-comparison-1.png)

Expected-response indices from two negative-binomial glmmTMB candidates
at the same reference profile. Both use the same monthly random-effect
convention and response units. Intervals describe each index separately,
not uncertainty in the difference between the models.

When supplied fitted models or `influ_diag` objects instead,
[`plot_compare()`](https://www.quantifish.co.nz/influ2/reference/plot_compare.md)
keeps its existing year-effect-contrast behaviour. It does not silently
switch to response predictions.
[`plot_step()`](https://www.quantifish.co.nz/influ2/reference/plot_step.md)
also remains a year-effect comparison.

## Current boundaries

Response standardisation currently supports GLM, GAM, glmmTMB, and
complete, univariate brms fits. glmmTMB indices require a converged ML
fit and do not yet support random effects in the zero component. brms
autocorrelation and Gaussian-process predictions need a separate
joint-prediction adapter. GAM smooths are evaluated as fitted, including
random-effect smooths; their target can differ from the
zero-group-effect convention used for glmmTMB and brms.

For binomial GLMs and glmmTMB, native response predictions are
probabilities. For brms binomial models they are expected counts at the
reference number of trials: supply one trial for a per-trial comparison.
Quasi families and transformed responses are not supported here. Use an
explicit lognormal family instead of asking the package to guess a
back-transformation.

For sdmTMB and tinyVAST, `cpue_index(method = "year_effect")` can
already produce a table from the existing diagnostic contrasts. Full
spatial response standardisation, integration over latent-effect
distributions, and `integrate_index()` remain separate development work.
They must preserve spatial and spatiotemporal dependence and make areas,
units, and catchability assumptions explicit; they are not silently
substituted by this interface.
