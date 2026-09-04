# Bentley validation

## Purpose

The original `influ` code developed by Nokome Bentley used the `proto`
object system and GLMs ([Bentley et al. 2012](#ref-Bentley2012)). That
implementation is retained in `influ2` as a frozen validation artefact,
not as part of the package API. This vignette runs the old code
explicitly and compares it with the model-neutral S3 engine.

The comparison uses the same realistic `lobsters_per_pot` example as the
[main vignette](https://www.quantifish.co.nz/influ2/articles/influ2.md).
A smaller frozen fixture remains in the test suite for fast regression
testing.

## Fit the common model

``` r

library(influ2)
data(lobsters_per_pot)

bentley_glm <- glm(
  lobsters ~ year + month + poly(depth, 3) + poly(soak, 3),
  family = poisson(link = "log"),
  data = lobsters_per_pot
)
new_diagnostic <- influ(bentley_glm, focus = "year")
```

The legacy source remains outside the namespace under `inst/legacy`. It
is loaded into an isolated environment only for this comparison.

``` r

legacy_available <- requireNamespace("proto", quietly = TRUE)
if (legacy_available) {
  legacy_environment <- new.env(parent = globalenv())
  legacy_environment$proto <- proto::proto
  sys.source(
    system.file("legacy", "influ-proto.R", package = "influ2"),
    envir = legacy_environment
  )
  legacy_diagnostic <- legacy_environment$Influence$new(
    model = bentley_glm,
    data = lobsters_per_pot,
    response = "lobsters",
    focus = "year"
  )
  legacy_diagnostic$calc()
  # The original code predates R 4.0 and assumes that data.frame() converts
  # text to factors. Restore that historical plotting assumption here without
  # modifying the frozen implementation.
  legacy_diagnostic$influences$level <- factor(
    legacy_diagnostic$influences$level,
    levels = legacy_diagnostic$influences$level
  )
}
legacy_available
#> [1] TRUE
```

## Influence plots

The left-hand figure is produced by the original `proto` method. The
right-hand figure is produced from the new `influ_diag`. Both show
monthly, depth, and soak-time influence as ratios centred on one across
18 years.

``` r

legacy_diagnostic$influPlot()

plot(
  new_diagnostic,
  type = "influence",
  term = c("month", "poly(depth, 3)", "poly(soak, 3)"),
  scale = "ratio"
) +
  facet_grid(term ~ scale) +
  labs(title = "New influ2")
```

![Legacy Bentley influence plot for month, depth, and soak time by
year.](bentley-validation_files/figure-html/bentley-influence-plots-1.png)![New
influ2 influence plot for month, depth, and soak time by
year.](bentley-validation_files/figure-html/bentley-influence-plots-2.png)

Influence plots for the lobster example from the original Bentley proto
implementation (left) and the new influ2 engine (right).

## CDI plots

The complete coefficient-distribution-influence display combines the
fitted effect, the distribution of observations, and the resulting
influence. Both figures below use month as the model term and year as
the focus.

``` r

legacy_diagnostic$cdiPlot(term = "month")
plot(new_diagnostic, type = "cdi", term = "month")
```

![Legacy Bentley coefficient-distribution-influence plot for
month.](bentley-validation_files/figure-html/bentley-cdi-plots-1.png)![New
influ2 coefficient-distribution-influence plot for
month.](bentley-validation_files/figure-html/bentley-cdi-plots-2.png)

CDI plots for month in the lobster example from the original Bentley
proto implementation (left) and the new influ2 engine (right).

## Numerical agreement

The old and new engines are also compared directly on all 54
year-by-term contrasts in the lobster example. Routine `testthat` tests
retain both this realistic comparison and a smaller frozen numerical
fixture.

``` r

legacy <- reshape(
  as.data.frame(legacy_diagnostic$influences),
  direction = "long",
  varying = names(legacy_diagnostic$influences)[-1],
  v.names = "link_influence_legacy",
  timevar = "term",
  times = names(legacy_diagnostic$influences)[-1]
)
rownames(legacy) <- NULL

current <- subset(
  influ_effects(new_diagnostic),
  term != "year" & scale == "link",
  select = c(level, term, estimate)
)
names(current)[names(current) == "estimate"] <- "link_influence_new"

comparison <- merge(
  legacy[c("level", "term", "link_influence_legacy")],
  current,
  by = c("level", "term")
)
comparison$natural_influence_legacy <- exp(comparison$link_influence_legacy)
comparison$absolute_difference <- abs(
  comparison$link_influence_legacy - comparison$link_influence_new
)
head(comparison)
#>   level           term link_influence_legacy link_influence_new
#> 1  2000          month         -5.459421e-02      -5.459421e-02
#> 2  2000 poly(depth, 3)         -4.025057e-03      -4.025057e-03
#> 3  2000  poly(soak, 3)          1.734128e-02       1.734128e-02
#> 4  2001          month          1.075912e-02       1.075912e-02
#> 5  2001 poly(depth, 3)          7.654306e-05       7.654306e-05
#> 6  2001  poly(soak, 3)          2.001786e-02       2.001786e-02
#>   natural_influence_legacy absolute_difference
#> 1                0.9468693        0.000000e+00
#> 2                0.9959830        8.673617e-19
#> 3                1.0174925        0.000000e+00
#> 4                1.0108172        6.938894e-18
#> 5                1.0000765        5.014435e-19
#> 6                1.0202196        0.000000e+00
max(comparison$absolute_difference)
#> [1] 2.775558e-17
stopifnot(max(comparison$absolute_difference) < 1e-8)
```

The same validation covers the two scalar metrics from the original
implementation. `overall` summarises mean absolute influence, while
`trend` summarises the ordered change through the focus levels.

``` r

legacy_metrics <- subset(
  legacy_diagnostic$summary,
  term != "intercept" & term != "year",
  select = c(term, overall, trend)
)
new_metrics <- reshape(
  influ_metrics(new_diagnostic)[c("term", "metric", "estimate")],
  direction = "wide",
  idvar = "term",
  timevar = "metric"
)
names(new_metrics) <- sub("^estimate\\.", "", names(new_metrics))
metric_comparison <- merge(
  legacy_metrics,
  new_metrics,
  by = "term",
  suffixes = c("_legacy", "_new")
)
metric_comparison
#>             term overall_legacy  trend_legacy overall_new     trend_new
#> 1          month    0.022447566  4.102224e-05 0.022447566  4.102224e-05
#> 2 poly(depth, 3)    0.005227671  5.112907e-04 0.005227671  5.112907e-04
#> 3  poly(soak, 3)    0.024195702 -3.937291e-04 0.024195702 -3.937291e-04
stopifnot(
  max(abs(metric_comparison$overall_legacy - metric_comparison$overall_new)) < 1e-8,
  max(abs(metric_comparison$trend_legacy - metric_comparison$trend_new)) < 1e-8
)
```

The final overlay makes exact agreement visually apparent: legacy
results are points, and new results are lines.

``` r

ggplot(comparison, aes(x = level, group = term)) +
  geom_hline(yintercept = 1, linetype = 3, colour = "grey55") +
  geom_point(
    aes(y = natural_influence_legacy, shape = "Legacy proto"),
    size = 3
  ) +
  geom_line(
    aes(y = exp(link_influence_new), colour = "New influ2"),
    linewidth = 0.9
  ) +
  facet_wrap(~term, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = c("New influ2" = "#0072B2")) +
  scale_shape_manual(values = c("Legacy proto" = 21)) +
  labs(
    x = "Year",
    y = "Influence ratio",
    colour = NULL,
    shape = NULL
  ) +
  theme_bw() +
  theme(legend.position = "bottom")
```

![Legacy influence points overlaid by new influ2 lines for month, depth,
and soak
time.](bentley-validation_files/figure-html/bentley-parity-plot-1.png)

Direct overlay of legacy Bentley influence values and new influ2 results
for the lobster example.

## References

Bentley, Nokome, Terese H. Kendrick, Paul J. Starr, and Paul A. Breen.
2012. “Influence Plots and Metrics: Tools for Better Understanding
Fisheries Catch-Per-Unit-Effort Standardizations.” *ICES Journal of
Marine Science* 69 (1): 84–88. <https://doi.org/10.1093/icesjms/fsr174>.
