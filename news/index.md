# Changelog

## influ2 1.1.0

- Added the model-neutral
  [`influ()`](https://www.quantifish.co.nz/influ2/reference/influ.md) S3
  API and compact `influ_diag` result.
- Added adapters for GLMs, `mgcv` GAMs, `brms`, `glmmTMB`, `sdmTMB`, and
  `tinyVAST`.
- Separated uncertainty calculation from draw retention, including a
  posterior-mean preview mode and derived-draw or disk retention.
- Added common print, summary, plot, and autoplot methods.
- Added
  [`influ_steps()`](https://www.quantifish.co.nz/influ2/reference/influ_steps.md)
  for ordered comparisons of genuinely refitted models, with automatic
  main-formula sequences for simple GLMs, GAMs, and `glmmTMB` models,
  explicit step specifications, and reuse of already fitted models.
  [`plot_step()`](https://www.quantifish.co.nz/influ2/reference/plot_step.md)
  accepts the same refitting route or a reusable `influ_steps` result.
  Step plots compare centred year-effect contrasts, with 95% intervals
  by default; they do not calculate area-weighted abundance indices.
- Added negative-binomial GLM refitting through
  [`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html),
  preserving formula offsets, weights, and analysis rows while
  re-estimating dispersion at each changed stage. The main lobster step
  demonstration now uses this model and its approximate
  negative-binomial confidence intervals.
- Guarded ambiguous interaction-only focus indices and implied-residual
  baselines. Reference grids centre effects; they do not automatically
  marginalise interactions.
- Restricted offset/exposure diagnostics to supported single-component
  log-link ratios and identity-link contrasts. Other links and combined
  hurdle/zero-inflated calculations with offsets fail explicitly.
  Nominal summaries remain observed-response means, not
  exposure-adjusted CPUE.
- Added backend-specific lognormal guards: BRMS requires constant
  log-scale `sigma` and an identity location link; mean-parameterised
  backends require log links. `glmmTMB` log-mean ratios remain available
  with varying dispersion.
- Centred CDI fitted-effect panels on the same weighted reference as
  influence. Log-link and lognormal components now display relative
  effects about one on a logarithmic axis; other links retain clearly
  labelled additive units. Confidence and credible intervals propagate
  uncertainty in the estimated reference, and posterior ratios are
  summarised after transforming each centred joint draw. The previous
  model-coded link display remains available with
  `coefficient_reference = "model"`.
- Added the initial Gaussian, binomial, Poisson, negative-binomial,
  lognormal, Gamma, and Tweedie family registry. Quasi and specialist
  extended families are deliberately excluded.
- Moved the original `proto` implementation out of the runtime namespace
  and added a frozen Bentley parity fixture and tests.
- Replaced the introductory and hurdle vignettes with model-neutral
  design and reference documentation.
- Added sparse joint-precision uncertainty for spatial and
  spatiotemporal fields, including draw-by-draw delta-field
  combinations.
- Added explicit prediction-grid and reference-weight standardisation,
  multivariate mixed-family `tinyVAST` responses, CDI coefficient
  intervals, and model-neutral Bentley overall and trend metrics.
- Propagated the joint conditional latent covariance for `glmmTMB`
  random effects instead of treating conditional modes as
  uncertainty-free.
- Retained selected comparison, model-criterion, data-extent, residual,
  Q-Q, and step-plot helpers, while moving their BRMS and `rstan`
  tooling out of mandatory dependencies. The remaining historical helper
  source is frozen under `tools/legacy/` for explicit
  function-by-function triage.
- Replaced embedded fitted-model test data with compact posterior-draw
  fixtures, substantially reducing the source-package size.
- Replaced the lobster example with a reproducible, fully synthetic
  dataset and added stricter validation for intervals, weights, focus
  ordering, component draws, data-extent plots, bubble plots, and
  implied residuals.
- Strengthened the synthetic lobster teaching scenario with changes in
  sampled season, depth, and soak time. Retained its known annual
  effects as dataset metadata and added a dynamically calculated truth
  check for the refitted negative-binomial GLM sequence. The BRMS
  example includes soak time alongside monthly and depth effects.
  Documentation distinguishes point-estimate recovery from interval
  coverage and explains the model-based intervals.
- Corrected BRMS population-term mapping when ordinary predictors follow
  group-level or smooth terms, so their labels and CDI grouping match
  the fixed-effect design matrix.
