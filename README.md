# influ2

[![R-CMD-check](https://github.com/quantifish/influ2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/quantifish/influ2/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/quantifish/influ2/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/quantifish/influ2/actions/workflows/pkgdown.yaml)

`influ2` calculates and visualises how explanatory variables, random effects,
and spatial fields influence standardised fisheries CPUE indices.

The package uses one model-neutral S3 interface:

```r
diagnostic <- influ(model, focus = "year")
diagnostic
summary(diagnostic)
plot(diagnostic, type = "influence")
plot(diagnostic, type = "cdi", term = "vessel")
```

Initial adapters are available for:

- `stats::glm()`;
- `mgcv::gam()`;
- `brms::brm()`;
- `glmmTMB::glmmTMB()`;
- `sdmTMB::sdmTMB()`; and
- `tinyVAST::tinyVAST()`.

The result is a compact `influ_diag` object with consistent tables and plots
across backends. BRMS posterior coefficients are projected directly into
focus-by-term diagnostics, avoiding observation-by-draw-by-term arrays.

```r
# Full posterior calculation, compact stored summaries.
diagnostic <- influ(brms_fit, focus = "year", retain = "summary")

# Retain only draws of the derived diagnostics.
diagnostic <- influ(
  brms_fit,
  focus = "year",
  retain = "derived_draws"
)

# Fast posterior-mean preview.
preview <- influ(brms_fit, focus = "year", uncertainty = "none")
```

Supported base families in the first implementation are Gaussian, binomial,
Poisson, negative binomial, lognormal, Gamma, and Tweedie. Hurdle/delta and
zero-inflated components are kept explicit.

## Installation

```r
pak::pak("quantifish/influ2")
```

See `vignette("influ2")` for the framework, memory strategy, and model
examples. The separate `vignette("bentley-validation")` documents and plots
the frozen agreement check against the original Bentley implementation.
