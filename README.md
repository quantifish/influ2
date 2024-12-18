# influ2 <img src="man/figures/logo.png" align="right" height=140/>

Nokome Bentley's R package `influ` was developed for use with frequentist models 
fitted in R using the `glm` function. The `infu2` package is the Bayesian 
couterpart and has been developed for use with `brms`. It works with 
population-level or group-level effects, the Bayesian equivalents of 
fixed-effects and random-effects. It contains functions for extracting 
coefficients, calculating the influence of terms, generating CDI plots, step 
plots, and other diagnostic plots.

## Installation

The `influ2` package can be installed from within R using:

``` r
library(devtools)
install_github(repo = "quantifish/influ2", build_vignettes = TRUE)
```

## References

Bentley, N., Kendrick, T. H., Starr, P. J., and Breen, P. A. Inﬂuence plots and metrics: tools for better understanding ﬁsheries catch-per-unit-effort standardizations. – ICES Journal of Marine Science, doi:10.1093/icesjms/fsr174.
