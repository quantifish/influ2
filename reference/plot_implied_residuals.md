# Plot implied residual coefficients

Implied residual coefficients mimic a focus-by-group interaction that
was not fitted. For each stratum, the mean standardised residual is
added to the normalised focus coefficient. This follows the definition
used in New Zealand inshore CPUE reports. Error bars show one standard
error of the standardised residuals.

## Usage

``` r
plot_implied_residuals(
  fit,
  data = NULL,
  year = "year",
  groups = "area",
  type = "pearson",
  min_n = 10L,
  colour = "purple4"
)
```

## Arguments

- fit:

  A fitted model supported by \[influ()\].

- data:

  Optional original model data.

- year:

  Name of the focus variable.

- groups:

  Name of the categorical variable used for panels.

- type:

  Residual type passed to the fitted model's \`residuals()\` method.

- min_n:

  Minimum records required in a focus-by-group stratum.

- colour:

  Colour used for implied coefficients.

## Value

A \[ggplot2::ggplot()\] object.

## References

Starr, P. J., and Kendrick, T. H. (2019). \*FLA 1 Fishery
Characterisation and CPUE\*. New Zealand Fisheries Assessment Report
2019/09, Figure O.9.
