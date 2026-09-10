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
| [`plot_predicted_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot_predicted_residuals.md) | Does residual behaviour change with the predictive mean? | Reuses generalised residuals and matching predictive means; it is not a predictive interval plot. |
| [`plot_implied_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot_implied_residuals.md) | Do groups show generalised residual departures through time? | Mean normal-score departures, not implied coefficients or a fitted interaction. |
| `plot(checks, type = "qq")` | How do simulation-based quantile residuals compare with their normal reference? | Uses a precomputed `influ_residuals` object; the ribbon is a nominal reference, not a calibrated model-specific test. |

[`influ_residuals()`](https://www.quantifish.co.nz/influ2/reference/influ_residuals.md)
takes a fitted model, not an `influ_diag` summary: the latter
deliberately does not retain all observation-level predictions and
residuals. The unified
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) method then
operates on the calculated `influ_residuals` object.

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
particular observed months must follow a realised curve. The
generalised-residual helper and DHARMa examples below remain separate
fixed-effect comparisons.

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

### A standalone Q-Q plot

Each panel can also be drawn separately. Reuse the `influ_residuals`
object calculated above to show exactly the Q-Q panel from the
four-panel overview, without refitting the model or repeating its
simulations:

``` r

plot(nb_checks, type = "qq")
```

![Standalone normal-score simulation-rank Q-Q plot for the
negative-binomial glmmTMB lobster model, with a dashed identity line and
a grey reference
ribbon.](residual-diagnostics_files/figure-html/residual-standalone-qq-1.png)

Standalone simulation-based Q-Q diagnostic for the negative-binomial
glmmTMB lobster model. The points and ribbon are identical to the Q-Q
panel in its four-panel overview. The grey ribbon is a nominal 95%
pointwise independent-uniform reference, not posterior uncertainty
around individual points or a calibrated goodness-of-fit threshold.

The result is a ggplot object, so it can be customised and saved in the
usual way. This method is documented in
[`?plot.influ_residuals`](https://www.quantifish.co.nz/influ2/reference/plot.influ_residuals.md);
[`?influ_residuals`](https://www.quantifish.co.nz/influ2/reference/influ_residuals.md)
explains the simulation calculation and includes the standalone plotting
call. It takes an `influ_residuals` result, not an `influ_diag`
influence summary.

The earlier influ2 `plot_qq()` function has been retired. It compared
native model residuals, usually Pearson residuals, with normal
quantiles. The unified simulation-based diagnostic supersedes that
workflow; it does not reproduce those native residuals. In particular,
count-model Pearson residuals need not be normal even under a suitable
model. The grey reference ribbon above is not a posterior credible
interval for each point; those intervals are not part of this display.
No compatibility wrapper for `plot_qq()` is retained.

### A standalone ECDF plot

An empirical cumulative distribution function (ECDF) reports the
proportion of observations at or below each response value. At zero, it
shows the empty-pot proportion; further along the curve, it describes
the body and upper tail of the catch distribution. This is a
response-distribution check, not an ECDF of residuals or a standardised
CPUE index.

Reuse the same calculated lobster diagnostic to display its fourth
panel:

``` r

plot(nb_checks, type = "distribution", response_scale = "log1p")
```

![Standalone lobster-catch ECDF comparison with observed and
simulated-median step curves and a grey predictive
band.](residual-diagnostics_files/figure-html/residual-standalone-ecdf-1.png)

Standalone observed and simulated catch ECDFs for the negative-binomial
glmmTMB lobster model. The purple line is the observed ECDF, the blue
line is the pointwise median of 250 simulated ECDFs, and the grey ribbon
is their 95% pointwise predictive band. These are exactly the
distribution-panel results from the first overview, with monthly random
effects resimulated. The log1p axis retains zero catches.

This call performs no new simulations. A curve above the predictive band
at a catch threshold means that more observed pots fall at or below that
threshold than the model commonly generates. Agreement in the pooled
ECDF does not establish that depth, year, or spatial patterns are
correct. The band is pointwise, not simultaneous, and crossing it is not
an automatic rejection test. The plotted simulated curves use a compact
grid rather than every simulated jump. For a Bernoulli model, an
explicit `type = "distribution"` still draws an ECDF, although the
default overview uses the more informative probability-calibration
panel.

### Posterior predictive ECDFs with brms

For a complete brms fit, the same interface draws from the posterior
predictive distribution: it includes response variation and posterior
parameter uncertainty. It does not refit the model or run MCMC. For
example, after fitting your model once:

``` r

bayesian_checks <- influ_residuals(full_brms, nsim = 500,
  batch_size = 25, seed = 20260910)
plot(bayesian_checks, type = "distribution")
```

The executed example below reuses the **same complete, previously fitted
brms model** as the [six-backend
comparison](https://www.quantifish.co.nz/influ2/articles/model-comparison.html#bayesian-models-and-a-mixed-summary-table):
150 simulated continuous responses over five years, with `y ~ year + x`
and a Gaussian observation distribution. This small example demonstrates
the Bayesian workflow; it is not a Gaussian model for lobster counts.
Negative responses are possible here, so the response axis is
untransformed.

The preparation script
[`data-raw/brms-residual-example.R`](https://github.com/quantifish/influ2/blob/master/data-raw/brms-residual-example.R)
was run on that complete fit. It calculates 500 posterior predictive
replicates in batches of 25 and saves only the compact diagnostic and
ECDF curves. The article renders those saved results, not a fabricated
compact `brmsfit`, and does not require Stan or MCMC to rebuild the
figures.

``` r

bayesian_example <- readRDS(system.file("extdata",
  "brms-residual-example.rds", package = "influ2"))
bayesian_checks <- bayesian_example$checks
```

The original four-chain fit retained 4000 post-warmup draws, had maximum
R-hat 1.0048, and no divergent transitions. These sampler checks do not
establish observation-model adequacy.

``` r

plot(bayesian_checks, type = "distribution")
```

![Bayesian observed and posterior predictive response ECDFs with a 95%
pointwise band on an untransformed continuous-response
axis.](residual-diagnostics_files/figure-html/brms-predictive-ecdf-1.png)

Posterior predictive response ECDF for the previously fitted Gaussian
brms example. The grey ribbon and blue median summarise 500 joint
posterior predictive replicates, including parameter uncertainty and
observation noise. The purple line is the observed ECDF. This is a
fitted-data predictive check, not LOO-PIT or a confidence interval for a
CPUE index.

An overlay shows individual replicated ECDFs instead of a predictive
ribbon. For a complete fit, the native brms alternative is:

``` r

set.seed(20260911)
brms::pp_check(full_brms, type = "ecdf_overlay", ndraws = 20)
```

The next figure uses 20 separately generated whole-response replicates
from
[`brms::posterior_predict()`](https://mc-stan.org/rstantools/reference/posterior_predict.html)
on the same fit. Each curve is evaluated on the stored grid. These are
actual posterior predictive replicates, not draws from a posterior-mean
coefficient vector. Saving their ECDF curves lets us render an overlay
without retaining an observation-by-draw matrix.

``` r

ggplot(bayesian_example$overlay, aes(response, probability)) +
  geom_step(aes(group = replicate), colour = "grey65", alpha = 0.6) +
  geom_step(data = bayesian_checks$observed_ecdf,
    colour = "purple4", linewidth = 0.9) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = 0)) +
  labs(x = "y", y = "Cumulative probability")
```

![Twenty grey posterior predictive ECDF step curves overlaid with the
purple observed-response
ECDF.](residual-diagnostics_files/figure-html/brms-predictive-overlay-1.png)

Twenty individual posterior predictive ECDFs (grey) and the observed
ECDF (purple) for the same Gaussian brms model. Each grey step curve
comes from one whole posterior predictive replicate, evaluated on the
compact grid. This smaller, separately generated set illustrates
replicate-to-replicate variation; it is not the 500-replicate ribbon
shown above.

See the native [posterior
prediction](https://paulbuerkner.com/brms/reference/posterior_predict.brmsfit.html)
and [ECDF
overlay](https://mc-stan.org/bayesplot/reference/PPC-distributions.html)
documentation. Use a limited number of overlay draws for readability and
memory; the influ2 ribbon can summarise more replicates in batches. An
`influ_residuals` summary cannot regenerate discarded replicates or run
a new native `pp_check()`; keep the original fit separately for that
purpose.

### These checks are not LOO-PIT

The response ECDF above compares whole observed and replicated datasets.
The normal-score rank residuals in the Q-Q panel instead locate each
observation within its own fitted predictive distribution. Both use the
data that fitted the model. Posterior predictive ranks need not be
uniformly distributed even when the model is appropriate.

LOO-PIT uses a predictive distribution that leaves the observation out.
It is a distinct predictive-calibration diagnostic, **not a
model-comparison score** and not supplied merely by calculating LOOIC in
[`table_criterion()`](https://www.quantifish.co.nz/influ2/reference/table_criterion.md).
Native LOO-PIT workflows need aligned predictive draws and leave-one-out
calculations, including checks on any importance-sampling approximation.
For dependent fisheries observations, the held-out unit must also match
the intended prediction question. See the [loo model-checking
example](https://mc-stan.org/loo/articles/loo2-example.html#marginal-posterior-predictive-checks).
influ2 does not implement universal LOO-PIT or automatic
cross-validation refits. Those remain outside this release’s residual
interface.

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

The generalised-residual helpers
[`plot_predicted_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot_predicted_residuals.md)
and
[`plot_implied_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot_implied_residuals.md)
remain available for their separate questions. Q-Q plots use the unified
simulation-based workflow shown above.

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

## Generalised residuals against fitted values and predictors

All maintained influ2 residual plots use the same simulation-based
normal-score rank residuals. These incorporate the fitted response
distribution, including its discrete outcomes and supported hurdle/delta
structure. They are not native Pearson or deviance residuals. Positive
scores identify observations high in their predictive distributions;
negative scores identify low observations.

Calculate once, then reuse the stored object for different views. The
fitted axis is the predictive mean from the **same simulations**, with
the same response component and random-effect conditioning.

``` r

reduced_checks <- influ_residuals(lobster_reduced, data = lobsters_per_pot,
  groups = "month", nsim = 250, seed = 41)
full_checks <- influ_residuals(lobster_nb, data = lobsters_per_pot,
  groups = "month", nsim = 250, seed = 41)
patchwork::wrap_plots(
  plot_predicted_residuals(reduced_checks) + labs(title = "Reduced NB"),
  plot_predicted_residuals(full_checks) + labs(title = "Full NB"),
  ncol = 2
)
```

![Two generalised residual-versus-predictive-mean panels for reduced and
full
models.](residual-diagnostics_files/figure-html/residual-fitted-1.png)

Generalised residuals against predictive mean lobster catch for reduced
and full negative-binomial models. Blue smooths describe changes in the
residual centre; the dashed line marks zero.

A fitted-value plot can conceal an omitted covariate. The same residuals
can be matched back to depth and soak time using their original row
identifiers. Neither this matching nor redrawing the figures repeats
simulation.

``` r

residual_data <- do.call(rbind, lapply(
  c("Reduced NB", "Full NB"), function(label) {
    checks <- if (label == "Reduced NB") reduced_checks else full_checks
    rows <- match(checks$observations$row, rownames(lobsters_per_pot))
    r <- checks$observations$residual
    rbind(
      data.frame(model = label, predictor = "Depth (m)",
        value = lobsters_per_pot$depth[rows], residual = r),
      data.frame(model = label, predictor = "Soak time (hours)",
        value = lobsters_per_pot$soak[rows], residual = r)
    )
  }
))
residual_data$model <- factor(residual_data$model,
  levels = c("Reduced NB", "Full NB"))
ggplot(residual_data, aes(value, residual)) +
  geom_hline(yintercept = 0, linetype = 3, colour = "grey45") +
  geom_point(alpha = 0.08, size = 0.5) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE) +
  facet_grid(model ~ predictor, scales = "free_x") +
  labs(x = NULL, y = "Normal-score rank residual")
```

![Four panels compare generalised residual patterns against depth and
soak
time.](residual-diagnostics_files/figure-html/residual-predictors-1.png)

The same generalised residuals against depth and soak time. The reduced
model omits both predictors; the full model includes their polynomial
effects. Smooth trends are descriptive, not tests.

For real fisheries, inspect vessel, gear, year, season, and location.
Sparse regions and changing fleet composition deserve attention. These
are in-sample diagnostics, not independent validation observations.

## Grouped departures: revisiting implied coefficients

New Zealand inshore CPUE reports use residual-implied coefficients to
explore departures from a shared year effect. Figure O.9 of Starr and
Kendrick ([2019](#ref-StarrKendrick2019)) adds a stratum’s mean
standardised residual to a normalised year coefficient. Figures
C.19-C.20 of Middleton ([2025](#ref-Middleton2025)) show related
target-by-year and area-by-year displays for lognormal positive catches,
omitting strata with fewer than 10 records. These examples motivate the
grouping and support checks; their captions do not establish a universal
Pearson-residual definition.

The earlier influ2 helper defaulted to native Pearson residuals and
added their mean to the link-scale year effect. This was an exploratory
convention, not a general interaction estimator. Pearson residuals and
normal-score residuals are dimensionless, whereas the coefficient is on
the model’s link scale. **Replacing Pearson residuals with quantile
residuals in that sum would still mix scales.** Ordinary GLM partial
residuals instead involve working residuals, as described in [R’s GLM
documentation](https://search.r-project.org/R/refmans/stats/html/glm.summaries.html);
that does not supply a universal quantile-residual-to-coefficient
conversion.

The maintained
[`plot_implied_residuals()`](https://www.quantifish.co.nz/influ2/reference/plot_implied_residuals.md)
therefore now displays **mean generalised residual departures around
zero**, not adjusted coefficients. The familiar helper name is retained,
but the axis and returned data explicitly describe the new quantity. A
positive point means that a group’s catches tend to be high within their
own fitted predictive distributions. It is not a log-CPUE adjustment,
biomass multiplier, or fitted interaction coefficient.

``` r

plot_implied_residuals(full_checks, groups = "month", min_n = 10)
```

![Twelve monthly panels show mean normal-score residual departures
around
zero.](residual-diagnostics_files/figure-html/residual-implied-1.png)

Generalised year-by-month residual departures for the full
negative-binomial lobster model. The grey zero line is the common
normal-score reference, not the annual coefficient. Bars show mean plus
or minus a descriptive iid standard error (SD/sqrt(n)), and point area
represents record count. Strata with fewer than 10 records are omitted;
all panels share the same scale.

The bars do not account for within-vessel or spatial dependence,
parameter estimation, or Monte Carlo variation. They are **not
confidence intervals for an interaction**. Repeat with more simulations
or different seeds when a feature matters, and inspect the full residual
distribution as well as its mean. A zero mean alone cannot rule out
wrong dispersion or tails. Posterior predictive ranks reuse the fitted
data and are not guaranteed uniform; conditioning for hierarchical
models remains important.

Actual year effects are still available from
[`influ()`](https://www.quantifish.co.nz/influ2/reference/influ.md) and
the index functions. Estimating a group-specific change in the index
requires a separate model with the relevant interaction or process,
followed by model checking. Do not add a normal score back onto a
coefficient.

For a different grouping, retain it during calculation:

``` r

checks <- influ_residuals(fit, data = original_data,
  groups = c("area", "gear"), nsim = 1000, seed = 41)
plot_implied_residuals(checks, groups = "area")
plot_implied_residuals(checks, groups = "gear")
plot_predicted_residuals(checks)
```

Keep original row names. influ2 verifies fitted data before retaining
the group columns, including after omissions, subsets, or reordering. It
does not attach arbitrary new data to an already calculated object.
Choose groups independently of the outcome: selecting high catches as a
group invalidates the zero reference.

For delta models, `component = "combined"` checks the combined response.
It does **not** diagnose positive catches separately. Supported explicit
`component = "positive"` calculations use the native positive component
and its positive observation rows. The plotting helpers preserve that
selection. Unsupported component extraction fails instead of
substituting a different residual type. Existing code requesting
`type = "pearson"` now receives a migration error; remove that argument
and recalculate. Old saved figures do not change when the package is
updated.

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
influ2’s unified Q-Q workflow is:

``` r

pcod_checks <- influ_residuals(pcod_model, nsim = 250, seed = 41)
plot(pcod_checks, type = "qq")
```

This simulates responses conditional on the fitted fields (`"mle-eb"`),
as recorded in `pcod_checks$metadata$scheme`. It is an exploratory
conditional check, **not** sdmTMB’s native `"mle-mvn"` PIT residual
calculation or a claim that the two procedures have the same
calibration. The identity reference preserves location and scale
departures instead of fitting a quartile line through the points. The
native alternative remains available through the sdmTMB/DHARMa workflow
below.

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
instead select a component. influ2’s helpers now reuse the generalised
engine and preserve its selected component and simulation target.

### tinyVAST

For supported single-response tinyVAST fits, the plotting helpers use
simulated normal-score ranks conditional on the fitted fields, not
native deviance or response residuals.

``` r

tiny_checks <- influ_residuals(tiny_model, nsim = 250, seed = 41)
plot_predicted_residuals(tiny_checks)
plot(tiny_checks, type = "qq")
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

All influ2 helpers use the generalised simulation engine for GLMs, GAMs,
and glmmTMB. Native packages additionally offer their own residual
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
or the Bayesian calculations in
[`table_criterion()`](https://www.quantifish.co.nz/influ2/reference/table_criterion.md).
These require complete brms fits.
[`table_criterion()`](https://www.quantifish.co.nz/influ2/reference/table_criterion.md)
also supports model-specific frequentist likelihood criteria, without
treating those criteria as interchangeable with Bayesian scores; see
[Model
comparison](https://www.quantifish.co.nz/influ2/articles/model-comparison.md).

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
when comparing models. The main article’s step plots compare centred
year-effect contrasts.
[`plot_compare()`](https://www.quantifish.co.nz/influ2/reference/plot_compare.md)
also accepts calculated expected-response indices from
[`cpue_index()`](https://www.quantifish.co.nz/influ2/reference/cpue_index.md)
and area-integrated results from
[`integrate_index()`](https://www.quantifish.co.nz/influ2/reference/integrate_index.md);
these are different quantities and must be labelled accordingly.
Prediction-grid coverage, area expansion, and extrapolation need their
own checks; see [CPUE
indices](https://www.quantifish.co.nz/influ2/articles/cpue-indices.md).

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
