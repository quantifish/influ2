# influ2 1.1.0.9000

* Added the model-neutral `influ()` S3 API and compact `influ_diag` result.
* Added initial adapters for GLMs, `mgcv` GAMs, `brms`, `glmmTMB`, `sdmTMB`,
  and TinyVAST.
* Separated uncertainty calculation from draw retention, including a
  posterior-mean preview mode and derived-draw or disk retention.
* Added common print, summary, plot, and autoplot methods.
* Added the initial Gaussian, binomial, Poisson, negative-binomial, lognormal,
  Gamma, and Tweedie family registry. Quasi and specialist extended families
  are deliberately excluded.
* Moved the original `proto` implementation out of the runtime namespace and
  added a frozen Bentley parity fixture and tests.
* Replaced the introductory and hurdle vignettes with model-neutral design and
  reference documentation.
