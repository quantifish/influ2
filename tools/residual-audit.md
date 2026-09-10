# Generalised residual audit — 10 September 2026

## Finding and correction

Before this increment, the maintained `plot_implied_residuals()` and
`plot_predicted_residuals()` defaulted to native Pearson residuals. The unified
`influ_residuals()` overview, Q-Q, year, and ECDF/calibration displays already
used simulation-based quantile ranks. The older bridges had not been migrated.
This was not a scientific reason to prefer Pearson residuals universally.

The frozen original brms helper in `tools/legacy/R/plot-resids.R` called native
residuals without selecting a type for its implied plot, and defaulted to
`ordinary` for its predicted plot. Do not attribute the later Pearson default
to Bentley or assume it reproduces every historical implementation.

The maintained helpers now use one engine: finite-simulation randomised ranks,
transformed with the standard-normal quantile function. The fitted axis uses
the same simulations' predictive mean, preserving their conditioning. No
maintained plotting code requests native Pearson or deviance residuals.
Native response deviance in model-comparison tables is a fit criterion, not
a replacement residual diagnostic. Native fitting/simulation software may
internally estimate dispersion using its own statistical methods; influ2
does not replace those estimators.

## Why the implied display changes its scale

The inspected public sources were:

- Starr and Kendrick (2019), FAR 2019/09, Figure O.9, printed page 132:
  <https://fs.fish.govt.nz/Doc/24676/FAR-2019-09-FLA1-Characterisation-and-CPUE.pdf.ashx>.
  The figure adds mean standardised residuals to a normalised year coefficient.
- Middleton (2025), FAR 2025/32, Figures C.19–C.20, printed page 42:
  <https://fs.fish.govt.nz/Doc/26007/FAR-2025-32-A-rapid-update-of-CPUE-for-snapper-in-SNA2-to-2024.pdf.ashx>.
  The lognormal positive-catch displays show target-year and area-year implied
  coefficients, common annual effects, and support thresholds of ten records.
- Dunn and Smyth (1996), randomised quantile residuals:
  <https://gksmyth.github.io/pubs/residual.html>.
- R's GLM residual documentation distinguishes working/partial residuals:
  <https://search.r-project.org/R/refmans/stats/html/glm.summaries.html>.
- sdmTMB's residual article distinguishes analytical quantiles, simulations,
  and latent-effect conditioning:
  <https://sdmtmb.github.io/sdmTMB/articles/residual-checking.html>.

The fisheries captions do not establish a universal Pearson definition. Nor
does substituting a normal-score quantile residual into coefficient-plus-
residual arithmetic make it a valid interaction coefficient. Scores and
link-scale coefficients have different meanings and units. The revised
helper retains year-by-group structure, record-count information, and shared
axes, but plots mean normal-score departures around zero. It intentionally
does not label these as coefficients, CPUE adjustments, or biomass multipliers.
Actual interaction effects require a fitted interaction/process model.

Bars are descriptive iid SD/sqrt(n), not dependence-aware or fitted-interaction
confidence intervals. Means alone can conceal dispersion/tail problems;
retain the full diagnostic overview. Simulation ranks have Monte Carlo
variation; posterior predictive ranks reuse data, and hierarchical targets
depend on how latent effects are conditioned or resimulated.

## BNS boundary and migration

The BNS task was located; its recent-turn retrieval returned no message items.
The current Mac scripts and presentation were therefore checked directly,
read-only. `BNS-CPUE-2026/R/109_chosen_gam_diagnostics.R` explicitly requests
Pearson residuals for implied, effort, and gear plots. Its four-panel plots
already use separately cached generalised residuals. Script 111 and saved
presentation captions also encode the old coefficient-plus-residual quantity.

Those assessment scripts, cached outputs, and scientific results were not
changed here. A package update cannot relabel old saved plots. A later BNS
migration must remove explicit native-type requests, retain group columns
during residual calculation, replace downstream `$implied` expectations with
the new `$residual` summaries, and review the changed figure interpretation.
Effort/gear plots that call native residuals directly also need explicit BNS
changes; they are outside the influ2 package.

Combined delta-response diagnostics must remain distinct from positive-only
diagnostics. The helpers retain the selected component and original row IDs.
Positive sdmTMB component diagnostics use native component-2 simulations on
observed-positive rows; no encounter or combined residual is substituted.

Historical files under `tools/legacy` and the frozen review article remain
unchanged, as requested. Issue #12 and the legacy functionality review remain
parked. This work does not authorise a CRAN or win-builder submission.
