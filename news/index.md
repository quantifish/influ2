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
