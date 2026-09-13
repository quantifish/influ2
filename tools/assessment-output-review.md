# Assessment-output contract review: issue #22

Reviewed 13 September 2026. This audit informed the annual table/covariance
implementation; it does not adopt or rerun any stock assessment. Issue #18
and selective regional random-effect prediction remain separate work.

## Downstream evidence

The maintainer referred to PACWEST. No repository with that name was found;
the packhorse (PHC) assessment contains the described matrix workflow, so it
was used as the likely example, with that identification flagged explicitly.
The local sources are under `CRA/PHC/exploration/ensemble/`:
`PHC_cpue_full_ensemble.R` and `PHC_spm_full_ensemble_rtmb.R`.

The CPUE producer aggregates each joint draw first, retains covariance of the
**unscaled log annual indices**, and separately constructs relative display
indices. The assessment matches covariance by source and year, uses log
plug-in estimates, and adds a separately estimated source-specific error
variance before evaluating a multivariate-normal likelihood. It does not use
model-coefficient covariance as annual covariance. Its separation of sources
is an independence assumption, not a generic property of CPUE indices.

A read-only audit of the saved `full_cpue_ensemble/cpue_indices.rds` and
`cpue_covariances.rds` checked all 5,055 matrix/table pairs. Year labels match
both dimensions, and every covariance diagonal reproduces the saved unscaled
log-index SE exactly. No fit or assessment input was changed. These private
files, their stock results, and the associated report are not shipped.

The CRA5 sources are under
`CRA/CRA5/2026/models/cpue_joint_sensitivity/`, particularly
`run_joint_cpue_sensitivity.R` and `cpue_joint_sensitivity.qmd`.
The `CRA 5` task's covariance discussion was also read. It distinguishes
effects on fitted CPUE uncertainty from effects on stock status and leaves
contrast-only and reweighting decisions for assessment review.

CRA5's workflow is **not** the same as the PHC export:

- Native predictions retain selected year/area/season random effects, while
  vessel and month levels are set to missing for prediction.
- Native log-prediction covariance is converted to correlations, numerically
  adjusted where needed, and combined with the assessment's existing marginal
  log SDs. Iterated likelihood weights are applied separately.
- Covariance is retained within selected series, not across every regional
  and reporting-system combination.
- The saved FSU model is REML. The current influ2 glmmTMB response-index
  adapter deliberately rejects it and only supplies zero-group-effect ML
  predictions. Native predictions from the saved fit are finite, but that
  does not make it an eligible influ2 index or establish equivalent targets.

Therefore the new extractor is not advertised as a drop-in replacement for
the current CRA5 regional pipeline. Changing REML, selective random-effect
inclusion, bias correction, regional aggregation, or assessment weights needs
an explicit migration decision. No silent ML conversion, index rescaling,
eigenvalue repair, or assessment refit was performed. The historical
`Year/Mean/Median/SD/CV` callers can keep the full `as.data.frame()` schema;
the table alone never proves numerical equivalence to an older extractor.

## Public contract

`cpue_index()` and `integrate_index()` retain response and log annual covariance
when uncertainty is calculated. The same-year response diagonal equals
`SD^2`; log covariance is calculated with log-index gradients or log annual
draws, never recovered from marginal SDs alone. The compact matrices remain
available with summary-only retention. All six existing backend adapters
are covered, within their documented model-structure boundaries.

`index_vcov()`/`vcov()` select both axes by exact year labels and reject
misalignment, non-finite entries, asymmetry, and materially indefinite input.
An optional positive-definiteness requirement rejects singular results;
positive-semidefinite matrices remain available for inspection. Preview,
year-effect, and old objects without stored joint covariance fail explicitly.
Log covariance is unavailable for non-positive indices/draws; no clipping.

Response covariance scales with the square of a known positive unit/area
multiplier; log covariance is invariant. Draw-wise geometric-mean
normalisation, or its delta analogue, removes a common log-level and gives
singular log covariance. Use unscaled indices with a suitable catchability
parameter, or design a contrast likelihood explicitly. Do not regularise
the matrix solely to make a likelihood run.

`index_table(format = "lognormal")` provides marginal moment-matched
`Meanlog`, `SDlog`, and `LognormalMedian`. Those are not the fitted response
distribution, a genuine posterior median, or automatically the joint matrix's
log moments. Reporting omits an all-missing `Median` by default; the full
data-frame schema and actual posterior medians remain unchanged.

## Verification

Independent tests compare GLM and GAM analytic gradients, glmmTMB native
response/link covariance, brms shared posterior draws, and sdmTMB/tinyVAST
joint parameter/field draws. They cover batching, normalisation, explicit
weights, area scaling, missing uncertainty, and matrix/plot alignment.
Reporting tests reconstruct arithmetic lognormal moments and protect
genuine posterior medians. Existing clean-session and minimal-install tests
now compare covariance and matrix plots without optional modelling packages.

An additional complete saved Gaussian brms fit (four chains, 4,000 posterior
draws) was checked using 500 existing draws and independently aggregated native
`posterior_epred()` values. Annual means, medians, both covariance matrices,
batching, relative-index singularity, and area scaling agree. No MCMC ran.
Its summary result is 12,368 bytes and contains no fitted model or draws.
Saved brms, CRA5, and PHC inputs were hash-checked unchanged.

Full-suite, archive, coverage, website, and publication evidence is recorded
in [release-review.md](release-review.md). The worked public examples use
simulated lobster data, not private assessment results. Hoyle et al. (2024),
Sections 5.5 and 5.8, support uncertainty-source distinctions and covariance
propagation in index construction; they are not cited as proof that every
covariance-aware stock assessment is superior.
