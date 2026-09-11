# influ2 1.1.0

* Shortened the ratio-scale CDI y-axis label to "Relative Effect" for all
  terms. Reorganised the website reference and article lists, with residual
  diagnostics first and Bentley validation last. Calculations are unchanged.

* Added explicit residual `conditioning` options. Existing defaults remain:
  fitted effects for GLM/GAM/sdmTMB/tinyVAST, new random effects for glmmTMB,
  and posterior predictive draws for brms. New options hold glmmTMB effects
  fixed, reuse one joint conditional latent-effect draw for sdmTMB/tinyVAST,
  or simulate new sdmTMB processes. Unsupported combinations fail explicitly.
  Independent native objectives protect the supplied fit. Shared draws and
  per-replicate seeds preserve the new schemes across response batches;
  sampled-field encounter probabilities and delta components stay aligned.
  The spatial article compares schemes on the same fitted models. These are
  exploratory diagnostic targets, not interchangeable calibrated tests.

* Added `as_influ_residuals()` for externally generated response simulations.
  It requires explicit observation alignment, response kind, time column,
  and simulation conditioning, and returns the same compact diagnostic used
  by the standard plots and residual helpers. Bernoulli/binomial calibration
  requires original fitted probabilities and known trial counts. Combined
  and component inputs are never silently converted or filtered.
* Extracted one shared residual-summary engine, protected by a frozen
  pre-refactor numerical/RNG baseline and native replay tests. Existing fitted
  model defaults and calculations are unchanged. The supplied-matrix route
  does not simulate, refit, retain the full matrix, or add a streaming interface.
  The residual article demonstrates it with the fitted lobster glmmTMB model.

* Added optional standalone PIT ECDF and ECDF-difference panels through
  bayesplot, reusing stored PIT values without additional response simulation.
  Their simultaneous iid-uniform reference limits are labelled as exploratory,
  not calibrated tests for fitted models. `panels` now selects and orders any
  four supported residual panels; the default layout is unchanged.

* Expanded website figures now display the complete numbered caption from
  beneath each figure, rather than its shorter accessibility description.
  Long captions remain accessible, and the website build checks every plotted
  image for a matching caption. Calibration plots retain their separate grey
  predictive bars; the residual article clarifies their interpretation.

* Labelled the four-panel overview's normal-score PIT residuals explicitly in
  its footer and residual axes. The footer distinguishes panels A-C from the
  response ECDF or probability-calibration check in panel D. Help and the
  residual article explain the existing `qnorm(pit)` transformation; residual
  calculations, simulation schemes, and default panel choices are unchanged.

* Added standalone lobster ECDF and complete-fit brms posterior predictive
  examples to the residual article, with compact saved Bayesian results and
  an individual-replicate overlay. Help and tests cover standalone/overview
  ECDF agreement and calculation reuse. Fitted-data ranks, response ECDFs,
  LOO-PIT, and LOOIC are explicitly distinguished; no universal LOO-PIT or
  MCMC-on-render workflow is introduced.

* Unified the remaining residual plotting helpers with `influ_residuals()`.
  `plot_predicted_residuals()` now uses normal-score rank residuals and the
  predictive means from the same simulations. `plot_implied_residuals()` now
  shows year-by-group mean generalised departures around zero, not residuals
  added to coefficients on a different scale. Explicit native residual types
  are rejected with a migration message. Both helpers accept calculated
  residual objects, preserve response-component metadata, and avoid repeated
  simulation when restyling. Retain grouping columns with `groups` during
  calculation. Historical source and frozen review figures are unchanged.
  The Model comparison article now includes an executed six-backend table
  including a complete brms fit, without running MCMC during rendering.

* Extended `table_criterion()` to GLM, GAM, glmmTMB, sdmTMB, tinyVAST, and
  complete brms fits, including mixed-backend lists. Native AIC, BIC,
  deviance, degrees of freedom, and Bayesian summaries use separate columns.
  Explicit conditional AIC uses mgcv, sdmTMB, and tinyVAST native methods;
  unsupported glmmTMB mixed-model cAIC stays missing with a reason.
  Input order is now preserved by default. Optional differences and sorting
  require compatible fitted observations and likelihood targets; REML,
  penalised, failed, and unreliable-LOO cases are not automatically ranked.
  Paired LOO differences support both older matrix and newer data-frame
  outputs from loo. The new Model comparison article includes an executed
  compact Bayesian/frequentist table; no MCMC is run during rendering.

* Retired `plot_qq()` without a compatibility wrapper. Calculate
  `checks <- influ_residuals(fit)`, then use `plot(checks, type = "qq")`
  for the same simulation-based Q-Q diagnostic as the four-panel overview.
  The residual article includes a standalone glmmTMB example, and the plotting
  help cross-references this workflow. Its pointwise reference ribbon is not
  posterior uncertainty around individual points. Native-residual Q-Q plots
  and per-point posterior intervals are not part of the first-release API.
  The frozen legacy article remains available for review.

* Fixed glmmTMB influence diagnostics with explicit reference grids: the
  fitted model-matrix method ignores `newdata`, so fixed-effect designs now
  come from the native prediction setup. Regression tests cover different
  and equal-sized reference grids, polynomial bases, zero-inflation terms,
  and random-effect alignment. `cpue_index()` uses a separate prediction
  path and is unchanged.

* Fixed reference-predictor checks for brms distributional terms without
  random effects, where partial field matching could interrupt CPUE-index
  calculations. Added numerical and contract tests for index comparisons,
  joint uncertainty, prediction inputs, and refitted step sequences.

* Standardised expected-response CPUE tables now include sdmTMB and univariate
  tinyVAST, alongside GLM, GAM, glmmTMB, and complete brms fits. Spatial indices
  use native combined response predictions and compact joint Gaussian
  parameter/field uncertainty, with explicit field and exposure choices.
  Shared draw identities are invariant to prediction and draw batch sizes.
* Added `integrate_index()` for all six model types, with or without spatial
  terms. It sums expected responses times supplied cell areas and records
  response/area units, known catchability conversions, and optional within-cell
  seasonal averaging weights. Standardised means and integrated totals remain
  distinct calculated objects for plotting and comparison. The CPUE and spatial
  articles demonstrate these workflows; frequentist medians and the remaining
  legacy-function review are unchanged and explicitly deferred.

* Residual overviews now select encounter probability calibration for Bernoulli
  responses, while retaining ECDFs for positive, combined delta/hurdle, count,
  and grouped-binomial responses. Fixed fitted-probability bins preserve ties;
  compact native simulation summaries supply pointwise predictive envelopes.
  Added explicit calibration and scientific grouped checks, known-trial guards,
  component-aware hurdle/SDM simulation routing, and informative legacy-object
  fallbacks. Existing residual values and explicit distribution panels are
  unchanged. The residual article demonstrates good, distorted, and deceptively
  pooled calibration. No model fitting or cross-validation is triggered by plotting.

* Restored `geo_mean()` as a stable, documented public utility. Added
  `cpue_index()` assessment tables and `plot_index()`, with both standardised
  and standardized spellings accepted. Expected-response indices remain
  distinct from existing year-effect contrasts. Response standardisation
  initially supported GLM, GAM, glmmTMB, and complete brms fits, with joint delta-method
  or batched posterior uncertainty and explicit reference-population choices.
  `plot_compare()` now also accepts calculated index objects without changing
  its existing fitted-model behaviour. The CPUE indices article documents
  the table schema, uncertainty meaning, and model-specific prediction choices.
* Year-panel residual box widths now encode the square root of sample size;
  year labels no longer contain sample counts. Corrected the package name to
  lowercase brms throughout maintained documentation and messages.

* CDI proportion legends use one column with at most four reference bubbles,
  preserving the composition panel's bubble-size mapping. The spatial article
  hides the mesh-margin setup and restoration while still executing both.
* Refined CDI layouts: short term labels are horizontal and repeated above
  the fitted-effect panel; focus/year labels sit on the influence panel's
  right-hand axis. The two-column size legend is extracted without unused
  guide slots, preventing its title from being clipped. Four-panel residual
  examples now use glmmTMB models with a monthly random intercept. Bentley
  comparison labelling and the native mesh figure's margins are tidier.
* Added `influ_residuals()` and a reusable four-panel residual display:
  simulation-based Q-Q, residuals against the predictive mean, residuals by
  automatically detected fishing year, and observed versus simulated ECDFs.
  Complete response simulations are processed in small batches; fitted models
  and observation-by-simulation matrices are not retained. Backend conditioning,
  finite-rank randomisation, and reference-band limitations are explicit.
  An explicit `year` override handles ambiguous time terms, and numeric years
  retain gaps in sampling. The residual article demonstrates full and
  misspecified lobster models. The frozen legacy review article is preserved.
* Completed the first legacy-helper triage: retired `plot_hurdle()`,
  `get_coefs()`, `get_coefs_raw()`, `get_marginal()`, and the old Shiny
  `influ_app()`. No retained implementation depends on these functions.
  Consolidated `get_influ()`, `get_influ2()`, and `plot_influ()` on the
  model-neutral influence interface, and `plot_bayesian_cdi()` and
  `plot_bayesian_cdi2()` on `plot(..., type = "cdi")`. Their frozen source
  and help files were removed without adding compatibility wrappers.
  The current `influ()` API, hurdle/zero-inflated support, frozen review page
  and figures, and undecided assessment-output helpers remain intact.
* Added a residual-diagnostics article and reference section, with executable
  lobster examples, optional DHARMa checks, and guidance on PIT, spatial,
  Bayesian, and one-step-ahead residuals.
* Aligned residual plots with fitted observation rows after omissions, subsets,
  and reordered data. Ambiguous multivariate, delta, and compact brms inputs
  now fail informatively instead of producing mismatched diagnostics.
* Preserved negative additive contrasts in model comparisons, rejected mixed
  index scales, and prevented duplicate labels from merging model curves.
* Added the model-neutral `influ()` S3 API and compact `influ_diag` result.
* Added adapters for GLMs, `mgcv` GAMs, `brms`, `glmmTMB`, `sdmTMB`, and
  `tinyVAST`.
* Separated uncertainty calculation from draw retention, including a
  posterior-mean preview mode and derived-draw or disk retention.
* Added common print, summary, plot, and autoplot methods.
* Added `influ_steps()` for ordered comparisons of genuinely refitted models,
  with automatic main-formula sequences for simple GLMs, GAMs, and `glmmTMB`
  models, explicit step specifications, and reuse of already fitted models.
  `plot_step()` accepts the same refitting route or a reusable `influ_steps`
  result. Step plots compare centred year-effect contrasts, with 95% intervals
  by default; they do not calculate area-weighted abundance indices.
* Added negative-binomial GLM refitting through `MASS::glm.nb()`, preserving
  formula offsets, weights, and analysis rows while re-estimating dispersion
  at each changed stage. The main lobster step demonstration now uses this
  model and its approximate negative-binomial confidence intervals.
* Guarded ambiguous interaction-only focus indices and implied-residual
  baselines. Reference grids centre effects; they do not automatically
  marginalise interactions.
* Restricted offset/exposure diagnostics to supported single-component
  log-link ratios and identity-link contrasts. Other links and combined
  hurdle/zero-inflated calculations with offsets fail explicitly. Nominal
  summaries remain observed-response means, not exposure-adjusted CPUE.
* Added backend-specific lognormal guards: brms requires constant log-scale
  `sigma` and an identity location link; mean-parameterised backends require
  log links. `glmmTMB` log-mean ratios remain available with varying dispersion.
* Centred CDI fitted-effect panels on the same weighted reference as influence.
  Log-link and lognormal components now display relative effects about one on
  a logarithmic axis; other links retain clearly labelled additive units.
  Confidence and credible intervals propagate uncertainty in the estimated
  reference, and posterior ratios are summarised after transforming each
  centred joint draw. The previous model-coded link display remains available
  with `coefficient_reference = "model"`.
* Added the initial Gaussian, binomial, Poisson, negative-binomial, lognormal,
  Gamma, and Tweedie family registry. Quasi and specialist extended families
  are deliberately excluded.
* Moved the original `proto` implementation out of the runtime namespace and
  added a frozen Bentley parity fixture and tests.
* Replaced the introductory and hurdle vignettes with model-neutral design and
  reference documentation.
* Added sparse joint-precision uncertainty for spatial and spatiotemporal
  fields, including draw-by-draw delta-field combinations.
* Added explicit prediction-grid and reference-weight standardisation,
  multivariate mixed-family `tinyVAST` responses, CDI coefficient intervals,
  and model-neutral Bentley overall and trend metrics.
* Propagated the joint conditional latent covariance for `glmmTMB` random
  effects instead of treating conditional modes as uncertainty-free.
* Retained selected comparison, model-criterion, data-extent, residual, Q-Q,
  and step-plot helpers, while moving their brms and `rstan` tooling out of
  mandatory dependencies. The remaining historical helper source is frozen
  under `tools/legacy/` for explicit function-by-function triage.
* Replaced embedded fitted-model test data with compact posterior-draw
  fixtures, substantially reducing the source-package size.
* Replaced the lobster example with a reproducible, simulated dataset
  and added stricter validation for intervals, weights, focus ordering,
  component draws, data-extent plots, bubble plots, and implied residuals.
* Strengthened the simulated lobster teaching scenario with changes in sampled
  season, depth, and soak time. Retained its known annual effects as dataset
  metadata and added a dynamically calculated truth check for the refitted
  negative-binomial GLM sequence. The brms example includes soak time alongside
  monthly and depth effects. Documentation distinguishes point-estimate
  recovery from interval coverage and explains the model-based intervals.
* Corrected brms population-term mapping when ordinary predictors follow
  group-level or smooth terms, so their labels and CDI grouping match the
  fixed-effect design matrix.
