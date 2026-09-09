# Residual diagnostics

## Three complementary questions

A CPUE model needs more than a plausible-looking index. We need to ask
whether it describes the observations adequately, what drives the
standardised index, and how robust that index is to modelling choices.
Residual checks address the first question; influence, CDI, and step
plots primarily address the second. Comparing credible candidate models
helps address the third. None is a substitute for the others.

This article uses the same **simulated lobster CPUE** example as
[Influence
diagnostics](https://www.quantifish.co.nz/influ2/articles/influ2.md). It
includes deliberately incomplete and poorly fitting candidates, not just
the preferred example model. The aim is to connect a visible diagnostic
pattern to a possible modelling response, without turning individual
plots or p-values into automatic selection rules.

| influ2 helper | Question it helps answer | Important limitation |
|:---|:---|:---|
| [`influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/influ_residuals.md) then [`plot()`](https://rdrr.io/r/graphics/plot.default.html) | How do the overall response distribution, fitted-value patterns, and fishing-year residual distributions compare with model simulations? | Simulation conditioning differs between backends; the panels are exploratory, not a calibrated pass/fail test. |
| [`plot_predicted_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot_predicted_residuals.md) | Does residual behaviour change with the fitted mean? | Uses the fitted model’s native residual definition; it is not a predictive interval plot. |
| [`plot_implied_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot_implied_residuals.md) | Do groups suggest departures from the common year effect? | An exploratory CPUE display, not a fitted interaction or a distributional goodness-of-fit test. |
| [`plot_qq()`](https://www.quantifish.co.nz/influ2/reference/plot_qq.md) | How does the residual distribution compare with normal quantiles? | Count-model Pearson and deviance residuals need not be normal, even under a suitable model. |

These functions take a fitted model, not an `influ_diag` summary: the
latter deliberately does not retain all observation-level predictions
and residuals.

## Fit competing lobster models

The data include uneven sampling across years and months, changing depth
and soak time, and gaps in coverage. Catch was simulated from a
negative-binomial distribution. We fit a reduced negative-binomial model
that omits depth and soak time, a fuller negative-binomial model, and a
Poisson model with the same full mean formula. All use exactly the same
observations.

``` r

data(lobsters_per_pot)

lobster_reduced <- MASS::glm.nb(
  lobsters ~ year + month,
  data = lobsters_per_pot
)
lobster_nb <- MASS::glm.nb(
  lobsters ~ year + month + poly(depth, 3) + poly(soak, 3),
  data = lobsters_per_pot
)
lobster_poisson <- glm(
  formula(lobster_nb), family = poisson(link = "log"),
  data = lobsters_per_pot
)
```

`MASS` is optional. The executed model examples are skipped if it is not
installed. Nothing in this article fits an MCMC model.

## A four-panel overview

The year panel uses box widths proportional to the square root of the
number of observations in each year. This shows uneven sampling without
crowding the year labels with counts; wider boxes represent larger
samples.

Calculate the residual diagnostic once and reuse it. This does not
update or refit the model. The four panels cover complementary
questions, without making the user choose an arbitrary grouping
variable:

1.  **Distributional Q-Q:** normal scores of randomised simulation
    ranks. Unlike a normal Q-Q plot of Pearson residuals, these account
    for the fitted response distribution, including discrete counts and
    zeros.
2.  **Residuals against fitted values:** check changes in centre,
    spread, and tails. The fitted quantity is the predictive mean
    estimated from the same simulations, so its conditioning matches the
    residual calculation.
3.  **Residuals by fishing year:** compare within-year distributions,
    not just mean residuals. A year factor can make those means look
    reassuring by construction. Boxes show medians, quartiles, whiskers,
    and outlying points; widths show relative sample sizes, and numeric
    year gaps remain visible.
4.  **Response-adaptive check:** Bernoulli encounter models use
    probability calibration; other models retain the
    observed-versus-simulated ECDF, including zero catches, with a
    pointwise predictive band. The examples immediately below are count
    models, so their fourth panel remains the CDF.

For this overview, use **glmmTMB mixed models** with a monthly random
intercept, as in the [main
article](https://www.quantifish.co.nz/influ2/articles/influ2.html#glmmtmb).
Year, depth, and soak time enter the mean formula as fixed effects; the
monthly effect is partially pooled. The negative-binomial and Poisson
candidates use the same mean structure and the original, unchanged
simulated lobster dataset.

``` r

data(lobsters_per_pot)
lobster_nb_mixed <- glmmTMB::glmmTMB(
  lobsters ~ year + poly(depth, 3) + poly(soak, 3) + (1 | month),
  family = glmmTMB::nbinom2(link = "log"), data = lobsters_per_pot
)
lobster_poisson_mixed <- glmmTMB::glmmTMB(
  formula(lobster_nb_mixed),
  family = poisson(link = "log"), data = lobsters_per_pot
)
```

These examples require the optional `glmmTMB` package. Its native
simulations resimulate monthly random effects from their estimated
distribution, rather than holding the twelve fitted monthly effects
fixed. Thus the ECDF band also reflects between-month variation in
replicated data. These are model checks, not a test of whether
particular observed months must follow a realised curve. The existing
native-residual and DHARMa examples below remain separate fixed-effect
comparisons.

``` r

nb_checks <- influ_residuals(lobster_nb_mixed, nsim = 250, batch_size = 25,
  seed = 20260907)
nb_checks
#> Simulation-based residual diagnostics (glmmTMB)
#> 5049 observations; 250 simulations
#> Time: year [ year-name detection ]
#> Native simulations at fitted parameters; random effects resimulated
#> Exploratory ranks; not a calibrated goodness-of-fit test
```

``` r

plot(nb_checks, response_scale = "log1p")
```

![Four panels for a negative-binomial glmmTMB mixed model show
simulation-rank Q-Q, residuals against the predictive mean, fishing-year
boxplots, and observed and simulated lobster-catch
ECDFs.](residual-diagnostics_files/figure-html/residual-overview-1.png)

Four-panel simulation-based residual diagnostic for the
negative-binomial glmmTMB lobster model with a monthly random intercept.
The year panel reports the uneven sample sizes. The Q-Q ribbon is a
nominal 95% pointwise independent-uniform reference, not a calibrated
test for this mixed model. The ECDF ribbon is a 95% pointwise predictive
band, including resimulated monthly effects. Its log1p response axis
retains zero catches.

For comparison, apply exactly the same display to the Poisson glmmTMB
candidate. Its mean formula includes the same terms, but it cannot
reproduce the extra variation used to simulate the lobster data. Look
for departures across the panels rather than declaring a model adequate
from one favourable plot.

``` r

poisson_checks <- influ_residuals(lobster_poisson_mixed, nsim = 250,
  batch_size = 25, seed = 20260907)
plot(poisson_checks, response_scale = "log1p")
```

![Four-panel residual diagnostic for the Poisson glmmTMB mixed model,
including resimulated monthly
effects.](residual-diagnostics_files/figure-html/residual-overview-poisson-1.png)

The same four-panel diagnostic for the Poisson glmmTMB lobster
candidate, again with a monthly random intercept. Its observation
distribution lacks the negative-binomial variation in the simulated
data. Compare all four panels with the preceding model; resimulated
monthly effects contribute to both displays, and the reference ribbons
are not automatic model-selection thresholds.

### Automatic time selection

`year`, `yr`, `fy`, `fishing_year`, `Fishing.Year`, and similar
recognised year names in the model formula are detected without regard
to case or punctuation. If there is no year-named term, the calculator
looks for native time metadata, then `time` or `season`, then the first
single-variable model term. That final fallback warns rather than
silently calling a covariate “year”. Multiple candidates require an
explicit choice. The printed result records the selected column and how
it was found.

``` r

checks <- influ_residuals(fit, data = original_data, year = "fishing_year")
plot(checks, type = "year")
```

Supply the original data if a transformed term, such as `factor(year)`,
hides the raw column and it cannot be recovered from the model call.
Original row names are checked, including after subsetting or
missing-value omissions. For a region-specific or covariate-specific
question, use a separate targeted plot; the standard four-panel layout
remains consistent across models.

### Interpretation and memory

With `B` simulations, `L` simulated responses below an observation, and
`E` ties, the rank is `(L + U * (E + 1)) / (B + 1)`, where `U` is
uniform. This finite-simulation construction randomises ties, including
zero atoms, without jittering catches in response units. It approaches a
PIT residual as the number of simulations increases. The normal score is
a display transformation, not an assumption that the catches themselves
are normally distributed. Try additional seeds and more simulations
before drawing strong conclusions about tails. The seed and batch size
are recorded; repeat both for identical results because native
simulation methods can consume random numbers differently.

The Q-Q ribbon is an **independent-uniform reference**. Fitting
parameters, spatial dependence, and Bayesian data reuse mean it is not
automatically a correctly calibrated goodness-of-fit envelope. There are
deliberately no automatic p-values or red/green judgements. The ECDF
band describes replicated data under the selected simulation scheme, not
uncertainty in the abundance index, and is not a simultaneous band.

For delta, hurdle, and zero-inflated models, the default overview checks
the **combined response**, including zeros. It is not a diagnostic of
the positive component alone. A positive-component check requires that
component’s fitted model and matching positive observations, or an
explicitly component-aware native diagnostic. Subsetting or relabelling
a combined-response overview does not turn it into a positive-component
check.

| Backend | Default simulation target in [`influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/influ_residuals.md) |
|:---|:---|
| GLM / GAM | Observation error at fitted parameters, including fitted smooths; parameter uncertainty is not added. |
| glmmTMB | Native simulation with random effects resimulated from their estimated distribution. |
| sdmTMB / tinyVAST | `mle-eb`: observation error conditional on fitted spatial, spatiotemporal, and other latent effects. |
| brms | Existing joint posterior draws followed by response simulation, including existing group effects. This requires a complete fit, not a compact influence fixture. |

The native documentation describes the respective
[glmmTMB](https://glmmtmb.github.io/glmmTMB/reference/simulate.glmmTMB.html),
[sdmTMB](https://sdmtmb.github.io/sdmTMB/reference/simulate.sdmTMB.html),
[tinyVAST](https://vast-lib.github.io/tinyVAST/reference/simulate.tinyVAST.html),
and
[brms](https://paulbuerkner.com/brms/reference/posterior_predict.brmsfit.html)
simulation interfaces. The table above states the choices made by this
wrapper, not a claim that every backend has the same native defaults.

These targets differ. In particular, conditional checks can conceal
latent structure absorbed by an overly flexible model, and posterior
predictive ranks need not be uniform. For the alternative sdmTMB
approximate-latent-draw recipe, spatial correlation checks, or
specialised OSA/LOO-PIT diagnostics, see the later sections. Delta and
zero-inflated models default to the **combined response**; explicit
supported component checks are described below. Multivariate, censored,
quasi, non-binomial weighted, and multi-trial sdmTMB inputs are not
supported by this initial display. A native simulation failure is
reported rather than replaced with a different response distribution.
Binomial GLM/glmmTMB panels use success counts, with integer trials
preserved. Weighted one-column binomial responses now require
`trial_counts = "trials_column"` and the original `data`, or a
two-column success/failure response. Arbitrary fitting weights are not
assumed to be trial counts.

Calculation retains one residual and predictive mean per observation,
plus compact ECDF summaries. It does **not** retain the model or the
full observation-by-simulation matrix. Working memory is approximately
observation count times batch size, plus ECDF grid size times simulation
count, with additional allocations inside native backends. Each
replicate remains a whole joint response vector; batching does not
discard its spatial or posterior dependence. The ECDF uses a grid
spanning the observations and first simulation batch, rather than
storing every simulated step. Replotting `nb_checks` is immediate and
performs no new simulations.

The existing helpers below remain available.
[`plot_qq()`](https://www.quantifish.co.nz/influ2/reference/plot_qq.md)
deliberately retains its older native-residual interpretation; it has
not silently changed meaning.

## Encounter calibration

For a Bernoulli encounter model, the pooled binary CDF mainly checks the
overall proportion of positive catches. A fitted intercept can enforce
that agreement, and year effects can similarly reproduce annual
proportions. Neither result establishes that probabilities are correct
for particular fishing operations. The default fourth panel therefore
changes to calibration: among observations assigned about 80%
probability of positive catch, did about 80% actually contain positive
catch?

The first three panels and their residuals are unchanged. Automatic
selection uses the response family, component, and known trial counts,
not whether the sample happens to contain zeros and ones. A Poisson
sample containing only zeros and ones remains a count diagnostic. An
all-zero Bernoulli sample remains an encounter diagnostic, although its
fitted model may have convergence issues. Gamma/lognormal positive
responses and combined delta catch retain their CDFs.

The following completely simulated teaching example has variable
encounter probabilities driven by depth and target fishery. Nothing here
uses BNS data or refits an existing fisheries model. A small GAM
demonstrates the workflow.

``` r

set.seed(813)
encounter_data <- data.frame(
  year = factor(rep(2010:2013, each = 300)),
  target = factor(rep(c("A", "B"), 600)),
  depth = runif(1200, 20, 100)
)
encounter_data$true_probability <- plogis(
  -1.6 + 2.4 * (encounter_data$target == "B") +
    1.2 * sin((encounter_data$depth - 20) / 80 * pi)
)
encounter_data$present <- rbinom(
  nrow(encounter_data), 1, encounter_data$true_probability
)
encounter_gam <- mgcv::gam(
  present ~ year + target + s(depth, k = 5),
  family = binomial(), data = encounter_data, method = "REML"
)
encounter_checks <- influ_residuals(
  encounter_gam, nsim = 250, seed = 813,
  calibration_bins = 10, calibration_min_n = 30
)
```

``` r

plot(encounter_checks)
```

![Four-panel encounter diagnostic with probability calibration replacing
the catch-distribution panel at bottom
right.](residual-diagnostics_files/figure-html/encounter-calibration-overview-1.png)

Response-adaptive four-panel overview for the simulated encounter GAM.
The fourth panel compares observed encounter proportions (purple points)
with original fitted probabilities in fixed bins; grey ranges are
pointwise 95% predictive envelopes from observation simulations at
fitted parameters, including fitted smooths. They are not confidence
intervals for a calibration curve or a calibrated goodness-of-fit test.

### Fixed bins and predictive envelopes

Bins contain roughly equal numbers of observations, with a requested
default of ten bins and a default minimum of 20 observations. Identical
and effectively identical probabilities (absolute tolerance `1e-8`) are
never divided between bins. The number of bins is reduced and small bins
are merged when necessary. Points sit at the mean fitted probability in
each bin, point area represents observation count, and the dashed line
is the 1:1 reference. An effectively constant probability gives one
point, labelled as an overall-frequency check only. Sparse support is
disclosed; black crosses flag sparse points.

Bin membership comes from **original fitted probabilities**, not the
noisy average of simulated binary outcomes. It stays fixed across all
joint response simulations. Within each batch, influ2 reduces each
complete simulation to bin proportions before discarding it. Only
summary tables remain in the result; no observation-by-simulation matrix
is saved for calibration. Configure bins when calculating, rather than
when plotting, because discarded simulations cannot subsequently be
re-binned.

Grey ranges predict variation in the **observed proportions** under the
recorded simulation scheme. They are not uncertainty intervals for an
unknown calibration curve. They are pointwise, not simultaneous, and do
not turn this exploratory fitted-data display into a test. GLM/GAM
simulations hold fitted parameters and smooths fixed. glmmTMB bins use
probabilities conditional on fitted random effects, whereas its native
simulations redraw those effects; the simulated envelope can
consequently be displaced from the identity line. sdmTMB/tinyVAST use
their existing `mle-eb` conditioning. brms bins use posterior mean
expected probabilities over the same draw identities as the posterior
predictive simulations, including existing group effects.

Whole simulations preserve the dependence represented by those methods.
They do not add unmodelled vessel or temporal dependence. If such
dependence matters, interpret the envelope cautiously and use
scientifically appropriate grouped checks; held-out or blocked
validation would provide stronger evidence. Nothing in these plotting
calls automatically refits a model or cross-validates it.

### Deliberately distorted probabilities

For contrast, force an exaggerated probability pattern using an
offset-only binomial model. This intentionally wrong model specifies its
probabilities without estimating an intercept. It is a demonstration of
visible miscalibration, not a recommended candidate fitting procedure.

``` r

encounter_data$distorted_logit <-
  2 * predict(encounter_gam, type = "link") + 0.8
distorted_model <- glm(
  present ~ 0 + offset(distorted_logit),
  family = binomial(), data = encounter_data
)
distorted_checks <- influ_residuals(
  distorted_model, data = encounter_data, year = "year",
  nsim = 250, seed = 813, calibration_min_n = 30
)
```

``` r

patchwork::wrap_plots(
  plot(encounter_checks, type = "calibration") + labs(title = "Encounter GAM"),
  plot(distorted_checks, type = "calibration") + labs(title = "Distorted probabilities"),
  ncol = 2
)
```

![Two calibration panels compare a reasonable GAM with deliberately
distorted
probabilities.](residual-diagnostics_files/figure-html/encounter-distorted-comparison-1.png)

Encounter calibration for the simulated-data GAM (left) and deliberately
exaggerated probabilities (right). Purple points are observed bin
proportions, the dashed line is identity, and grey ranges are pointwise
95% predictive envelopes under each model’s fixed probabilities. The
distorted probabilities show systematic departures, rather than merely a
different overall encounter rate.

### A pooled pass can conceal missing structure

An intercept-only model estimates one overall encounter probability. Its
single pooled point lies on the identity line even when target fishery
is an important omitted predictor. The example below shows that
limitation and checks the mean raw encounter residual,
`observed presence - fitted probability`, within each year-by-target
combination. Groups come from the scientific question, not from the
observed outcome.

``` r

pooled_model <- glm(present ~ 1, family = binomial(), data = encounter_data)
pooled_checks <- influ_residuals(
  pooled_model, data = encounter_data, year = "year",
  nsim = 250, seed = 813,
  calibration_groups = c("year", "target")
)
```

``` r

patchwork::wrap_plots(
  plot(pooled_checks, type = "calibration"),
  plot(pooled_checks, type = "calibration_groups"),
  ncol = 2
)
```

![One pooled calibration point matches identity, while year-by-target
residual differences show large positive and negative
departures.](residual-diagnostics_files/figure-html/encounter-pooled-versus-grouped-1.png)

The intercept-only model matches the overall encounter frequency exactly
(left), yet year-by-target mean raw residuals expose omitted target
structure (right). Purple points on the right are observed minus fitted
encounter proportions; grey ranges are pointwise 95% predictive
envelopes for those differences, conditional on the fixed fitted
probabilities. A pooled calibration pass is not an all-clear for model
structure.

Use `calibration_groups = c("year", "area")`, `"vessel"`, or a supported
season grouping to answer other scientific questions. Supply original
`data` when grouping columns were not in the model formula. Groups below
`calibration_min_n` remain visible and flagged, without a predictive
envelope; they should not be treated as precise estimates. The compact
tables are available as `checks$calibration$bins` and
`checks$calibration$groups$table`. Do not define groups using catch
presence, catch magnitude, or a derived outcome.

### Components, grouped trials, and older objects

`component = "auto"` is deliberately conservative for joint models: it
keeps the combined catch response and the CDF. For a supported joint
hurdle/delta model, `component = "encounter"` compares presence with its
fitted encounter probability. It transforms complete native catch
simulations to presence, so their within-simulation dependence is
retained. This is not the same as using a zero-inflation probability as
an encounter probability in a count mixture. Automatic encounter
extraction from zero-inflated count mixtures is rejected.

sdmTMB also provides native positive-component simulation. The following
calls reuse an existing fitted model; they do not fit one here:

``` r

combined_checks <- influ_residuals(delta_fit, component = "combined")
encounter_checks <- influ_residuals(delta_fit, component = "encounter")
positive_checks <- influ_residuals(delta_fit, component = "positive")
```

The positive check uses native component-2 simulations at rows with
**observed** positive catch. It never filters each simulated combined
catch vector to its positive outcomes. Native component
prediction/simulation is documented by
[sdmTMB](https://sdmtmb.github.io/sdmTMB/reference/simulate.sdmTMB.html).
Joint positive extraction for other backends is not implemented: use the
separately fitted positive component with its matching data.
Standard-link tinyVAST delta encounter extraction is supported; other
tinyVAST delta parameterisations require a separate encounter fit.

Known multi-trial binomial responses retain the CDF in the default
overview. Explicit `type = "calibration"` is supported: observed
proportions are total successes divided by total trials, and fitted
probabilities are averaged with trial-count weights. Point size still
represents the number of observation rows, not a claim that all trials
are independent. Native beta-binomial dependence, where supported, is
retained in the simulation envelopes. The original multi-trial sdmTMB
limitation remains explicit.

``` r

plot(checks, type = "overview", response_diagnostic = "auto")
plot(checks, type = "calibration")
plot(checks, type = "distribution")
autoplot(checks, response_diagnostic = "auto")
```

An explicit distribution request retains the previous CDF, including for
binary data. Existing callers that manually assemble panels and request
`type = "distribution"` therefore remain unchanged; opt into the
automatic fourth panel by using the overview. `response_scale = "log1p"`
applies only to a CDF, never to calibration axes. Old saved objects
without response-type metadata retain the distribution overview with a
warning. Without fitted probabilities, recalculation is required for
calibration; missing simulation envelopes are not replaced by fabricated
or unrequested binomial intervals.

## Residuals against fitted values and predictors

Pearson residuals divide response errors by the model’s observation
standard deviation. Look for a changing centre, unusual spread, or
influential tails, but remember that low fitted counts produce asymmetry
and discrete bands. The residual axis must retain both negative and
positive values.

``` r

patchwork::wrap_plots(
  plot_predicted_residuals(lobster_reduced) + labs(title = "Reduced NB"),
  plot_predicted_residuals(lobster_nb) + labs(title = "Full NB"),
  ncol = 2
)
```

![Two residual-versus-fitted panels comparing reduced and full
negative-binomial
models.](residual-diagnostics_files/figure-html/residual-fitted-1.png)

Pearson residuals against fitted lobster catch for reduced and full
negative-binomial models. Blue smooths help reveal changes in the
residual centre; the dotted line marks zero.

A fitted-value plot can hide structure in an omitted covariate. Plotting
the same residuals against depth and soak time is more direct. Here a
systematic pattern in the reduced model motivates including those
relationships, rather than treating extra dispersion alone as a
solution.

``` r

residual_data <- do.call(rbind, lapply(
  c("Reduced NB", "Full NB"), function(label) {
    fit <- if (label == "Reduced NB") lobster_reduced else lobster_nb
    r <- residuals(fit, type = "pearson")
    rbind(
      data.frame(model = label, predictor = "Depth (m)",
                 value = lobsters_per_pot$depth, residual = r),
      data.frame(model = label, predictor = "Soak time (hours)",
                 value = lobsters_per_pot$soak, residual = r)
    )
  }
))
residual_data$model <- factor(
  residual_data$model, levels = c("Reduced NB", "Full NB")
)
ggplot(residual_data, aes(value, residual)) +
  geom_hline(yintercept = 0, linetype = 3, colour = "grey45") +
  geom_point(alpha = 0.08, size = 0.5) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE) +
  facet_grid(model ~ predictor, scales = "free_x") +
  labs(x = NULL, y = "Pearson residual")
```

![Four panels compare residual patterns against depth and soak time for
reduced and full
models.](residual-diagnostics_files/figure-html/residual-predictors-1.png)

Pearson residuals against depth and soak time. The reduced model omits
both predictors; the full model includes their polynomial effects.
Smooth trends reveal structure that a fitted-value plot can conceal.

For real fisheries, repeat this examination against vessel, gear, year,
season, and location. Sparse regions and changing fleet composition
deserve particular attention. A smooth is descriptive: these are
in-sample residuals, not independent validation observations.

## Implied residual coefficients

New Zealand inshore CPUE reports use implied coefficients to explore
departures from a shared year effect ([Starr and Kendrick
2019](#ref-StarrKendrick2019); [Middleton 2025](#ref-Middleton2025)).
For each year-by-group stratum,
[`plot_implied_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot_implied_residuals.md)
adds its mean residual to the centred link-scale year effect. The common
year effect is grey; purple points show the implied departures. Here the
groups are months, but an area, vessel group, or other categorical
variable could be used.

``` r

plot_implied_residuals(
  lobster_nb,
  data = lobsters_per_pot,
  year = "year", groups = "month", min_n = 10
)
```

![Twelve monthly panels compare implied coefficients with the same grey
annual
baseline.](residual-diagnostics_files/figure-html/residual-implied-1.png)

Implied year coefficients by month for the full negative-binomial
lobster model. Grey lines show the common centred year effect; purple
values add each stratum’s mean Pearson residual. Bars are plus or minus
one standard error of that residual mean, and point size represents
record count. Strata with fewer than 10 records are omitted.

The bars exclude uncertainty in the year coefficient and its covariance
with the residual mean; they are **not** confidence intervals for a
fitted year-by-month interaction. Adding a Pearson residual to a
link-scale effect is an interpretive convention, not an exact
interaction estimator. Native residual scaling also differs between
model packages. Persistent departures suggest checking sampling support
and an explicit interaction or other structure, then refitting and
reassessing the model.

Keep the original observation row names when supplying `data`. influ2
aligns omitted, subsetted, or reordered rows with the fitted model, and
rejects data whose identity cannot be verified. This prevents residuals
from being assigned silently to the wrong year or group.

## Normal Q-Q plots: useful, but not universal

``` r

plot_qq(lobster_nb, type = "pearson")
```

![A normal quantile comparison of the negative-binomial model's Pearson
residuals, with a quartile reference
line.](residual-diagnostics_files/figure-html/residual-normal-qq-1.png)

Normal Q-Q plot of Pearson residuals from the full negative-binomial
model. Curvature is not, by itself, evidence against the count model:
these residuals are not expected to be normally distributed.

The reference line in
[`plot_qq()`](https://www.quantifish.co.nz/influ2/reference/plot_qq.md)
passes through the chosen quantiles (the first and third quartiles by
default). Its `probs` argument does not define a confidence envelope.
Normal residual Q-Q plots are most natural for Gaussian errors; for
counts, zeros, skewed positive responses, and mixtures, use a
distribution-aware check as well.

## Simulation-based checks with DHARMa

DHARMa supplies simulation-based quantile residuals and associated
diagnostics ([Hartig 2026](#ref-DHARMa2026)). It is a suggested,
optional dependency, not part of the compact influence calculation. The
following example contrasts Poisson and negative-binomial variation
while holding the mean formula fixed.

``` r

dharma_poisson <- DHARMa::simulateResiduals(
  fittedModel = lobster_poisson, n = 250, refit = FALSE, seed = 20260907
)
dharma_nb <- DHARMa::simulateResiduals(
  fittedModel = lobster_nb, n = 250, refit = FALSE, seed = 20260907
)
```

Each object is calculated once and reused. With `refit = FALSE`,
simulated responses are generated from the fitted parameters; the model
is not refitted 250 times. This quick example uses 250 simulations. For
substantive work, increase that number and check stability. Simulation
storage still scales with observations times simulations, independently
of influ2’s compact uncertainty-retention settings.

``` r

par(mfrow = c(1, 2))
DHARMa::plotQQunif(
  dharma_poisson, testUniformity = FALSE, testOutliers = FALSE,
  testDispersion = FALSE, main = "Poisson"
)
DHARMa::plotQQunif(
  dharma_nb, testUniformity = FALSE, testOutliers = FALSE,
  testDispersion = FALSE, main = "Negative binomial"
)
```

![Two uniform quantile plots contrast Poisson misfit with the
negative-binomial
candidate.](residual-diagnostics_files/figure-html/residual-dharma-qq-1.png)

DHARMa uniform Q-Q checks for Poisson and negative-binomial lobster
models with identical mean formulas. The Poisson model does not
accommodate the simulated overdispersion. Formal tests are suppressed
here so interpretation starts with the diagnostic pattern.

``` r

par(mfrow = c(1, 1))
```

DHARMa residuals are approximately **uniform on zero to one** under an
adequate simulation model, not normally distributed. Do not feed them
into a normal Q-Q plot without a deliberate transformation.
Distributional agreement does not rule out remaining covariate patterns:

``` r

par(mfrow = c(1, 2))
DHARMa::plotResiduals(
  dharma_poisson, form = lobsters_per_pot$depth, quantreg = FALSE,
  main = "Poisson: depth"
)
DHARMa::plotResiduals(
  dharma_nb, form = lobsters_per_pot$depth, quantreg = FALSE,
  main = "Negative binomial: depth"
)
```

![Two plots show uniform-scale simulation residuals against ranked
depth.](residual-diagnostics_files/figure-html/residual-dharma-depth-1.png)

Simulation-based quantile residuals against depth for the two full-mean
lobster models. The thin black dashed line marks the uniform median of
0.5; the thicker dashed curve is a descriptive smooth. Depth is
rank-scaled. These are not independent validation tests.

``` r

par(mfrow = c(1, 1))
```

The simulated responses can also check quantities of direct fisheries
interest. Compare the observed proportion of empty pots and the upper
catch quantile with the same quantities calculated from each simulated
dataset. These are predictive checks at fitted parameters, not posterior
predictive checks or confidence intervals for an index.

``` r

statistics <- list(
  "Empty-pot proportion" = function(x) mean(x == 0),
  "95th catch percentile" = function(x) unname(quantile(x, 0.95))
)
simulations <- list(Poisson = dharma_poisson, NB = dharma_nb)
predictive_checks <- do.call(rbind, lapply(names(simulations), function(model) {
  do.call(rbind, lapply(names(statistics), function(statistic) {
    data.frame(
      model = model, statistic = statistic,
      value = apply(simulations[[model]]$simulatedResponse, 2, statistics[[statistic]])
    )
  }))
}))
observed_checks <- data.frame(
  statistic = names(statistics),
  observed = vapply(statistics, function(f) f(lobsters_per_pot$lobsters), numeric(1))
)
ggplot(predictive_checks, aes(value)) +
  geom_histogram(bins = 20, fill = "grey75", colour = "white") +
  geom_vline(data = observed_checks, aes(xintercept = observed),
             colour = "purple4", linewidth = 0.8) +
  facet_grid(model ~ statistic, scales = "free_x") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Statistic across one simulated dataset", y = "Simulated datasets")
```

![Four histograms compare simulated empty-pot proportions and 95th catch
percentiles with their observed
values.](residual-diagnostics_files/figure-html/residual-predictive-summaries-1.png)

Observed empty-pot proportions and upper catch quantiles (purple lines)
against their distributions across 250 simulated datasets. The
histograms expose consequences of the candidate observation
distributions that a standardised index alone does not show.

More zeros than simulated do not prove that a zero-inflated model is
needed: the mean structure or dispersion can also be wrong. For hurdle
or delta models, check encounter probability, the positive-response
distribution, and the combined response. The combined check alone can
conceal compensating errors in the components.

## Which residuals answer which question?

| Residual or approach | Useful for | What to watch |
|:---|:---|:---|
| Response, `y - fitted` | Errors in catch or CPUE units; patterns against predictors | Variance often changes with the mean; tails and zeros depend on the family. |
| Pearson | Mean/variance patterns after scaling by the model’s observation SD | Not generally normal for non-Gaussian data; native weighting and Bayesian definitions differ. |
| Deviance | Signed contributions based on likelihood discrepancy | Family-specific, not automatically normal, and not supplied by every backend. |
| PIT / randomised quantile | Distribution-aware checks using a predictive CDF | Discrete observations require randomisation; conditioning and estimated parameters affect calibration. |
| DHARMa | Simulation-based quantile checks, predictive summaries, and residual-pattern tools | It implements a PIT-like approach, rather than a distinct residual principle; simulations must match the response and hierarchy being checked. |
| One-step-ahead (OSA) | Sequential predictive checks in dependent latent-variable models | Requires suitable implementation, an observation order, and accurate conditional prediction; often more expensive. |

At a probability mass, a randomised PIT value is drawn between the CDF
just below the observation, `F(y-)`, and `F(y)`. For integer counts this
becomes `F(y - 1)` to `F(y)`; hurdle and Tweedie responses require
randomisation at their zero atom. Applying
[`qnorm()`](https://rdrr.io/r/stats/Normal.html) puts the result on the
standard-normal scale ([Dunn and Smyth 1996](#ref-DunnSmyth1996)). At
continuous responses, use `F(y)` directly. Exact reference distributions
assume the correct predictive distribution with known parameters;
fitted-model checks generally have approximate calibration. Keep a
reproducible seed and check sensitivity to randomisation, rather than
choosing the most attractive plot.

### Random effects, space, and time

Conditioning is a separate choice from residual scale. Conditional
simulations hold fitted random effects fixed and focus on
observation-level behaviour; unconditional simulations regenerate random
effects and inspect more of the hierarchy. Neither by itself validates
the whole model. For latent spatial fields, plugging in fitted modes can
distort goodness-of-fit diagnostics.

DHARMa changed its default to conditional simulations in version 0.5.0.
Record the package version and the conditioning used, and consult the
native simulation method rather than assuming that identical arguments
mean the same thing in every backend. The DHARMa development NEWS also
records a fix after 0.5.0 for mutation of glmmTMB simulation settings;
check that fix before using that combination for repeated production
diagnostics. See the [DHARMa
documentation](https://CRAN.R-project.org/package=DHARMa) and
[NEWS](https://florianhartig.r-universe.dev/DHARMa/NEWS).

Map residuals and inspect them through time as well as against
predictors. Spatial or temporal dependence can invalidate routine
independent-residual test p-values. Repeated locations and timestamps
need appropriate grouping, and that grouping changes the question being
tested. A well-calibrated in-sample plot does not establish
out-of-sample predictive skill.

### sdmTMB

sdmTMB recommends `type = "mle-mvn"` PIT residuals: fixed effects stay
at their maximum-likelihood estimates, while one approximate conditional
random-effect draw is used. These are **not OSA residuals**. Its
`"mle-eb"` plug-in mode is discouraged for goodness-of-fit checks; the
`"mle-mcmc"` alternative requires a suitable converged MCMC draw. See
the [native residual
reference](https://sdmtmb.github.io/sdmTMB/reference/residuals.sdmTMB.html)
and [worked residual
examples](https://sdmtmb.github.io/sdmTMB/articles/residual-checking.html).

For a single-response model such as `pcod_model` in [Spatial and
spatiotemporal
diagnostics](https://www.quantifish.co.nz/influ2/articles/spatial-spatiotemporal.md),
the native normal-scale PIT residuals can be displayed with influ2:

``` r

set.seed(41)
plot_qq(pcod_model, type = "mle-mvn") +
  geom_abline(intercept = 0, slope = 1, colour = "purple4") +
  labs(subtitle = "Purple: standard-normal identity; grey: fitted quartile line")
```

Unlike the fitted quartile line, the identity reference also reveals
location and scale departures from the expected standard-normal PIT
distribution.

sdmTMB also has a dedicated DHARMa bridge. The following recipe reuses
an already fitted model; it is not evaluated again in this article.

``` r

set.seed(41)
pcod_simulations <- simulate(pcod_model, nsim = 250, type = "mle-mvn")
pcod_dharma <- sdmTMB::dharma_residuals(
  pcod_simulations, pcod_model, plot = FALSE, return_DHARMa = TRUE
)
DHARMa::plotQQunif(
  pcod_dharma, testUniformity = FALSE,
  testOutliers = FALSE, testDispersion = FALSE
)
```

The [sdmTMB
bridge](https://sdmtmb.github.io/sdmTMB/reference/dharma_residuals.html)
can check the combined delta/hurdle response. Its analytical residuals
instead select a component. influ2’s three generic residual helpers
currently reject sdmTMB delta fits: pairing occurrence residuals with
unconditional fitted catch would be misleading. Use the native
component-specific workflow and matching predictions instead. Native
Pearson support is also family-specific; influ2 does not silently
replace it with another residual type.

### tinyVAST

The tinyVAST 1.6.2 interface provides deviance and response residual
types, not Pearson residuals or an OSA convenience interface. In checks
against that version, single-response deviance residuals worked with all
three influ2 helpers. Its native response method returned an empty
vector, so influ2 now reports that failure clearly rather than drawing
an empty diagnostic.

``` r

plot_predicted_residuals(tiny_model, type = "deviance")
plot_qq(tiny_model, type = "deviance")
```

The [tinyVAST simulation
interface](https://vast-lib.github.io/tinyVAST/reference/simulate.tinyVAST.html)
can support an external
[`DHARMa::createDHARMa()`](https://rdrr.io/pkg/DHARMa/man/createDHARMa.html)
workflow, but it needs careful response selection and observation
alignment for multivariate models. Its `"mle-mvn"` simulation draws a
new approximate random-effect vector for each replicate, unlike sdmTMB’s
single-draw residual recipe. This article does not claim those
procedures have identical calibration, or provide an automatic
multivariate DHARMa adapter.

### GLM, GAM, glmmTMB, and brms

The ordinary GLM, GAM, and glmmTMB residual plots use their native
methods. Current glmmTMB also provides a `"dunn-smyth"` option, but
family and version limitations matter, including fixes for multi-trial
binomial responses. Do not assume it handles every zero-inflated mixture
correctly. The [glmmTMB residual
source](https://github.com/glmmTMB/glmmTMB/blob/master/glmmTMB/R/methods.R)
and [release notes](https://glmmtmb.r-universe.dev/glmmTMB/NEWS)
describe the available implementation. Being built on TMB does not
itself provide an OSA interface.

For brms, use the **original complete `brmsfit`**. The compact example
fixtures in influ2 retain joint draws for influence calculations but
omit the Stan state needed by native prediction and residual methods.
They cannot support these residual helpers,
[`get_bayes_R2()`](https://www.quantifish.co.nz/influ2/reference/get_bayes_R2.md),
or
[`table_criterion()`](https://www.quantifish.co.nz/influ2/reference/table_criterion.md).
Those two comparison helpers remain brms-specific; they are not generic
frequentist model-selection tables.

brms’s native residual summaries depend on its prediction method;
Pearson residuals are based on predictive dispersion, not simply the GLM
formula. Posterior predictive checks
([`brms::pp_check()`](https://mc-stan.org/bayesplot/reference/pp_check.html))
can retain features obscured by a residual mean. Reusing a full fitted
model does not rerun MCMC, but prediction across posterior draws can
still require substantial memory. See the [brms residual
reference](https://paulbuerkner.com/brms/reference/residuals.brmsfit.html).

### OSA is a specialised additional workflow

OSA residuals use the predictive distribution of each observation
conditional on the preceding observations, which can account for latent
dependence ([Thygesen et al. 2017](#ref-Thygesen2017)). The order and
conditioning set must therefore be explicit. TMB offers
[`oneStepPredict()`](https://search.r-project.org/CRAN/refmans/TMB/html/oneStepPredict.html),
but its methods require suitable template support and approximation
checks. They are not universally available through
[`residuals()`](https://rdrr.io/r/stats/residuals.html) in glmmTMB,
sdmTMB, or tinyVAST. influ2 does not yet implement an OSA adapter.
Likewise, a refitting simulation study is a separate, potentially costly
validation exercise, not a default action performed by a residual plot.

## Connect model checks back to the index

An observation model that fails to reproduce dispersion or zeros may
still produce a plausible year curve. Conversely, changing a
well-supported mean relationship can materially alter the index. After
inspecting residuals, use
[`plot_compare()`](https://www.quantifish.co.nz/influ2/reference/plot_compare.md)
and refitted step plots to explain those consequences; the [main
article](https://www.quantifish.co.nz/influ2/articles/influ2.md)
demonstrates both.

Keep the same observations, years, response component, and index scale
when comparing models. influ2’s current comparisons are centred
year-effect contrasts, **not area-weighted abundance indices**.
Prediction-grid coverage, area expansion, and extrapolation are
additional downstream checks.

The practical sequence is to identify a residual pattern, consider
plausible data or model explanations, fit defensible alternatives, and
then inspect both model adequacy and index sensitivity. Residual tests
alone should not decide which CPUE series enters an assessment.

## References

Dunn, Peter K., and Gordon K. Smyth. 1996. “Randomized Quantile
Residuals.” *Journal of Computational and Graphical Statistics* 5 (3):
236–44. <https://doi.org/10.1080/10618600.1996.10474708>.

Hartig, Florian. 2026. *DHARMa: Residual Diagnostics for Hierarchical
(Multi-Level / Mixed) Regression Models*.
<https://doi.org/10.32614/CRAN.package.DHARMa>.

Middleton, D. A. J. 2025. *A Rapid Update of CPUE for the Snapper
Fishery in SNA 2 to 2024*. New Zealand Fisheries Assessment Report
2025/32. Fisheries New Zealand.
<https://www.mpi.govt.nz/dmsdocument/70215/direct>.

Starr, Paul J., and Terese H. Kendrick. 2019. *FLA 1 Fishery
Characterisation and CPUE*. New Zealand Fisheries Assessment Report
2019/09. Fisheries New Zealand.

Thygesen, Uffe Høgsbro, Christoffer Moesgaard Albertsen, Casper
Willestofte Berg, Kasper Kristensen, and Anders Nielsen. 2017.
“Validation of Ecological State Space Models Using the Laplace
Approximation.” *Environmental and Ecological Statistics* 24 (2):
317–39. <https://doi.org/10.1007/s10651-017-0372-4>.
