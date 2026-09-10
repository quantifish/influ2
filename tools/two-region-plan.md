# Two regional CPUE series from one fitted model

Planning only for issue #18, commissioned 10 September 2026. No regional
model, new vignette, or regional API is implemented by this plan. Keep #18
open until an executed example is approved and validated.

## Scientific question

Fit one model to four statistical areas. Report Region A from areas 1 and 2,
and Region B from areas 3 and 4. Each index is a standardised expected catch
per unit effort for a fixed regional reference population, not a sum of year
coefficients or an area-expanded biomass estimate.

For region r in year t, calculate the response-scale mean

    I[r,t] = sum_a w[r,a] * sum_j v[r,a,j] * E(catch | t, a, reference[j])

Area weights sum to one within a region; reference-profile weights sum to one
within each area. Keep both constant across years. Use unit exposure when
the model contains an effort offset. Do not average on the link scale,
average already normalised area indices, or let changing annual sampling
proportions silently redefine the regional population.

## Proposed vignette: Regional CPUE indices

1. Simulate four labelled areas with distinct temporal patterns, changing
   annual sampling allocations, depth and soak effects, and known regional
   reference-index truth. Keep support in every area/year needed by the
   primary model, while allowing uneven sample sizes and gaps in other
   covariate combinations. Do not modify the existing lobster dataset.
2. Fit one negative-binomial glmmTMB model to all areas, with shared covariate
   effects and a supported random intercept. A `year * area` interaction is
   a transparent first demonstration: one model permits four temporal curves,
   which are subsequently aggregated into the two specified regions.
3. Explain the simpler alternative: region-specific year effects with
   time-constant differences between areas within each region. This needs
   explicit, full-rank nested contrasts; naively adding region and area main
   effects duplicates information. Select one main formulation before coding.
4. Define the area-to-region lookup explicitly. Use equal area weights in the
   primary demonstration, then a fixed unequal-weight sensitivity. These
   are declared standardisation weights, not claimed physical habitat areas.
   Use the same defensible reference depth/soak profile across regions where
   supported, isolating regional composition and temporal differences.
5. Predict every reference row in every year from that same fitted model.
   `cpue_index(method = "standardised", reference_data = ..., reference_weights
   = ...)` already supplies the main calculation. A separate reference table
   for each region should suffice for the two point estimates and marginal
   uncertainty series; verify the interaction and factor-level handling.
   Do not subset and refit a model for each region.
6. Show raw expected-response indices and their assessment-ready tables,
   followed by region-wise relative indices normalised over the same years.
   Normalise only after regional aggregation. Use `plot_index()` and
   `plot_compare()` on calculated results with explicit regional labels.
7. Compare with known simulated regional truth and regional nominal CPUE.
   Explain sampling shifts, uncertainty, and extrapolation. Do not claim
   that pooled residual adequacy validates each regional curve.

## One crucial model limitation

A log-link additive model with one shared year effect and time-constant area
and covariate effects produces regionally different levels but identical
proportional trends under fixed reference weights. Grouping areas alone does
not create two distinct temporal indices. A year-by-area/region interaction,
or a genuinely time-varying regional process, must supply those differences.
This statement is not universal for every nonlinear link or mixture model.

## Uncertainty and validation before calling the example complete

- Compare regional means with explicit native response predictions and the
  chosen fixed weights; check invariance to reference-row order and splitting
  one row into two identical rows with weights summing to the original.
- Propagate the fitted joint covariance through the weighted regional mean,
  including covariance between areas. Never add area standard errors or
  combine their CVs as if independent. Propagate the normalising denominator
  when producing relative series.
- Two separate cpue_index calls give marginal uncertainties; the same fitted
  model makes the regional estimates correlated. Do not calculate a ratio,
  difference, or combined-index interval from independent regional draws.
  Joint regional contrasts would require verified cross-region gradients or
  aligned joint parameter draws; they are not necessary for the initial two-
  curve vignette. A common seed alone is not proof of draw alignment.
- Test full factor-level preservation, common years, interaction support,
  positive-definite Hessian, and explicit failures for unsupported predictions.
- Include the additive shared-year model as a short counterexample: the
  normalised curves coincide, which is correct rather than an extraction bug.
- Clearly identify conditional/population-level random-effect treatment.
  The current glmmTMB response-index path sets random effects to zero; this
  is not marginal integration over the random-effect distribution.
- Standardised regional means and area-integrated totals answer different
  questions. Link to integrate_index, but do not add biomass expansion to the
  main regional CPUE demonstration.

## Boundaries

Use the existing index and plotting interfaces first. Add a new public
regional helper only if the executed example exposes a genuine gap. Broader
sdmTMB/tinyVAST regional examples, region-ratio inference, and alternative
spatial-process specifications can be discussed after this single-model
demonstration. No changes to BNS, legacy triage, Median/NA output, or CRAN
submission are authorised by this planning item.
