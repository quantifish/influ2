# Spatial and spatiotemporal diagnostics

## Why separate field components?

A spatiotemporal model can contain at least three distinct
contributions:

- fixed effects, including measured covariates and temporal effects;
- a persistent spatial field, shared through time; and
- a spatiotemporal field, representing time-specific spatial departures.

Their sum describes the fitted linear predictor, but their influence
interpretations differ. A persistent spatial-field influence can reveal
a change in where observations were collected. A spatiotemporal-field
influence can also contain a genuine fitted annual spatial departure.
`influ2` therefore keeps the fields separate instead of silently
combining them.

The examples below follow the packages’ own worked examples: the
[`sdmTMB`
introduction](https://sdmtmb.github.io/sdmTMB/articles/basic-intro.html)
and the [`tinyVAST` vector-autoregressive
vignette](https://vast-lib.github.io/tinyVAST/articles/web_only/VAST.html).
They are deliberately small enough to rebuild as package documentation.

## sdmTMB

### Data and mesh

The `sdmTMB` example uses its Pacific cod survey data from Queen
Charlotte Sound ([Anderson et al. 2025](#ref-Anderson2025)). The
response is whether Pacific cod were encountered in a tow. Depth is
included as a quadratic effect, and year is the diagnostic focus.

``` r

library(influ2)
data("pcod", package = "sdmTMB")
data("qcs_grid", package = "sdmTMB")

ggplot(pcod, aes(X, Y, colour = density)) +
  geom_point(size = 1.2) +
  coord_fixed() +
  scale_colour_viridis_c(trans = "sqrt") +
  labs(x = "Easting (km)", y = "Northing (km)", colour = "Density") +
  theme_bw()
```

![Observed Pacific cod survey tows, coloured by catch
density.](spatial-spatiotemporal_files/figure-html/sdmtmb-data-1.png)

Observed Pacific cod survey tows, coloured by catch density.

The triangulated mesh approximates a continuous Matérn spatial field.
The coarse cutoff is appropriate for a documentation example, rather
than a recommendation for production analysis.

``` r

pcod_mesh <- sdmTMB::make_mesh(pcod, c("X", "Y"), cutoff = 10)
plot(pcod_mesh)
```

![Triangulated mesh used for the Pacific cod spatiotemporal
model.](spatial-spatiotemporal_files/figure-html/sdmtmb-mesh-1.png)

Triangulated mesh used for the Pacific cod spatiotemporal model.

### Fit and diagnose

This is the package website’s binomial spatiotemporal model, with a
persistent spatial field and independent year-specific spatiotemporal
fields.

``` r

pcod_model <- sdmTMB::sdmTMB(
  present ~ depth_scaled + depth_scaled2,
  data = pcod,
  mesh = pcod_mesh,
  family = binomial(link = "logit"),
  spatial = "on",
  time = "year",
  spatiotemporal = "IID",
  silent = TRUE
)

pcod_diagnostic <- influ(
  pcod_model,
  focus = "year",
  ndraws = 100,
  seed = 11
)
summary(pcod_diagnostic)
#> Influence diagnostic summary
#>   Backend: sdmTMB
#>   Family:  single binomial (logit)
#>   Focus:   year
#> 
#>                  term                 component maximum_absolute_link_influence
#>          depth_scaled               conditional                       0.5119533
#>  spatiotemporal_field conditional:latent_fields                       0.4760166
#>         depth_scaled2               conditional                       0.3191610
#>         spatial_field conditional:latent_fields                       0.2820632
#>  level_at_maximum
#>              2003
#>              2011
#>              2004
#>              2003
```

The common components plot shows the influence associated with changing
depth coverage, persistent spatial structure, and year-specific spatial
departures.

``` r

plot(
  pcod_diagnostic,
  type = "components",
  term = c(
    "depth_scaled", "depth_scaled2", "spatial_field",
    "spatiotemporal_field"
  )
)
```

![Fixed, spatial, and spatiotemporal influence components from the
sdmTMB Pacific cod
model.](spatial-spatiotemporal_files/figure-html/sdmtmb-components-1.png)

Fixed, spatial, and spatiotemporal influence components from the sdmTMB
Pacific cod model.

### Fitted field surfaces

`sdmTMB::predict()` exposes the persistent field as `omega_s`, the
spatiotemporal field as `epsilon_st`, and their combined contribution as
`est_rf`. Mapping the same components that enter the influence
diagnostic is an important interpretation check.

``` r

mapping_grid <- do.call(
  rbind,
  lapply(c(2003L, 2017L), function(selected_year) {
    transform(qcs_grid, year = selected_year)
  })
)
field_predictions <- predict(pcod_model, newdata = mapping_grid)
field_surfaces <- tidyr::pivot_longer(
  field_predictions,
  cols = c(omega_s, epsilon_st, est_rf),
  names_to = "component",
  values_to = "link_contribution"
)
field_surfaces$component <- factor(
  field_surfaces$component,
  levels = c("omega_s", "epsilon_st", "est_rf"),
  labels = c("Persistent spatial", "Spatiotemporal", "Combined fields")
)

ggplot(field_surfaces, aes(X, Y, fill = link_contribution)) +
  geom_raster() +
  coord_fixed() +
  facet_grid(component ~ year) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0
  ) +
  labs(
    x = "Easting (km)", y = "Northing (km)",
    fill = "Link-scale\ncontribution"
  ) +
  theme_bw()
```

![Persistent spatial field, year-specific spatiotemporal field, and
their sum for two Pacific cod survey
years.](spatial-spatiotemporal_files/figure-html/sdmtmb-fields-1.png)

Persistent spatial field, year-specific spatiotemporal field, and their
sum for two Pacific cod survey years.

## tinyVAST

### Simulated spatiotemporal process

The `tinyVAST` example adapts the package’s official univariate
vector-autoregressive simulation ([Thorson et al.
2025](#ref-Thorson2025)). It is smaller, uses a Poisson response, and
adds changing spatial coverage so that an influence diagnostic has a
sampling pattern to detect. The known process contains a persistent
spatial field and an AR(1) spatiotemporal field.

``` r

set.seed(101)
n_x <- n_y <- 7
n_time <- 6
rho <- 0.45
spatial_sd <- 0.70
spatiotemporal_sd <- 0.50

spatial_correlation <- exp(
  -0.5 * abs(outer(seq_len(n_x), seq_len(n_y), FUN = "-"))
)
spatial_covariance <- kronecker(spatial_correlation, spatial_correlation)
spatial_chol <- t(chol(
  spatial_sd^2 * spatial_covariance + diag(1e-8, n_x * n_y)
))
spatiotemporal_chol <- t(chol(
  spatiotemporal_sd^2 * spatial_covariance + diag(1e-8, n_x * n_y)
))

spatiotemporal_field <- t(replicate(
  n_time,
  as.numeric(spatiotemporal_chol %*% rnorm(n_x * n_y))
))
for (time_index in 2:n_time) {
  spatiotemporal_field[time_index, ] <-
    rho * spatiotemporal_field[time_index - 1, ] +
    sqrt(1 - rho^2) * spatiotemporal_field[time_index, ]
}
spatial_field <- as.numeric(spatial_chol %*% rnorm(n_x * n_y))

linear_predictor <-
  1 +
  outer(seq(-0.15, 0.15, length.out = n_time), rep(1, n_x * n_y)) +
  outer(rep(1, n_time), spatial_field) +
  spatiotemporal_field

tiny_grid <- data.frame(
  expand.grid(
    time = seq_len(n_time),
    x = seq_len(n_x),
    ycoord = seq_len(n_y)
  ),
  true_linear_predictor = as.vector(linear_predictor)
)
tiny_grid$mean <- exp(tiny_grid$true_linear_predictor)
tiny_grid$count <- rpois(nrow(tiny_grid), tiny_grid$mean)
tiny_grid$var <- "density"
tiny_grid$dist <- "poisson"

sampling_centre <- 1 +
  (tiny_grid$time - 1) * (n_x - 1) / (n_time - 1)
sampling_probability <- 0.45 + 0.50 * exp(
  -(tiny_grid$x - sampling_centre)^2 / (2 * 2^2)
)
tiny_data <- tiny_grid[runif(nrow(tiny_grid)) < sampling_probability, ]
rownames(tiny_data) <- NULL
```

The observation window shifts from left to right through time. That
imbalance allows the persistent field to affect temporal summaries even
though the field itself does not change through time.

``` r

ggplot(tiny_data, aes(x, ycoord, colour = count, size = count + 1)) +
  geom_point(alpha = 0.8) +
  coord_equal() +
  facet_wrap(~time, nrow = 2) +
  scale_colour_viridis_c() +
  scale_size_area(max_size = 5) +
  labs(x = "X", y = "Y", colour = "Count", size = "Count + 1") +
  theme_bw()
```

![Changing spatial coverage in the simulated tinyVAST
example.](spatial-spatiotemporal_files/figure-html/tinyvast-sampling-1.png)

Changing spatial coverage in the simulated tinyVAST example.

### Fit and diagnose

The structural-equation strings match the official `tinyVAST` notation:
one persistent spatial variance, plus an AR(1) spatiotemporal process.

``` r

tiny_mesh <- fmesher::fm_mesh_2d(
  tiny_data[c("x", "ycoord")],
  cutoff = 1
)
space_term <- "
  density <-> density, spatial_sd
"
spacetime_term <- "
  density -> density, 1, rho
  density <-> density, 0, spatiotemporal_sd
"

tiny_model <- tinyVAST::tinyVAST(
  count ~ factor(time),
  data = tiny_data,
  family = list(poisson = poisson()),
  spatial_domain = tiny_mesh,
  space_term = space_term,
  spacetime_term = spacetime_term,
  space_columns = c("x", "ycoord")
)

tiny_diagnostic <- influ(
  tiny_model,
  focus = "time",
  ndraws = 100,
  seed = 12
)
summary(tiny_diagnostic)
#> Influence diagnostic summary
#>   Backend: tinyVAST
#>   Family:  single poisson (log)
#>   Focus:   time
#> 
#>                  term                 component maximum_absolute_link_influence
#>          factor(time)               conditional                      0.24617106
#>  spatiotemporal_field conditional:latent_fields                      0.04604271
#>         spatial_field conditional:latent_fields                      0.02606292
#>  level_at_maximum
#>                 5
#>                 6
#>                 6
```

``` r

plot(tiny_diagnostic, type = "components")
```

![Fixed, spatial, and spatiotemporal influence components from the
tinyVAST
model.](spatial-spatiotemporal_files/figure-html/tinyvast-components-1.png)

Fixed, spatial, and spatiotemporal influence components from the
tinyVAST model.

### Fitted field surfaces

[`tinyVAST::project()`](https://vast-lib.github.io/tinyVAST/reference/project.html)
exposes the same latent components used by the adapter. The combined map
below is calculated on the link scale before applying the inverse link.

``` r

tiny_mapping_grid <- subset(tiny_grid, time %in% c(1, 3, 6))
tiny_mapping_grid$spatial <- tinyVAST::project(
  tiny_model,
  extra_times = numeric(0),
  newdata = tiny_mapping_grid,
  what = "pomega1_g",
  future_var = FALSE,
  past_var = FALSE,
  parm_var = FALSE
)
tiny_mapping_grid$spatiotemporal <- tinyVAST::project(
  tiny_model,
  extra_times = numeric(0),
  newdata = tiny_mapping_grid,
  what = "pepsilon1_g",
  future_var = FALSE,
  past_var = FALSE,
  parm_var = FALSE
)
tiny_mapping_grid$combined <-
  tiny_mapping_grid$spatial + tiny_mapping_grid$spatiotemporal

tiny_surfaces <- tidyr::pivot_longer(
  tiny_mapping_grid,
  cols = c(spatial, spatiotemporal, combined),
  names_to = "component",
  values_to = "link_contribution"
)
tiny_surfaces$component <- factor(
  tiny_surfaces$component,
  levels = c("spatial", "spatiotemporal", "combined"),
  labels = c("Persistent spatial", "Spatiotemporal", "Combined fields")
)

ggplot(tiny_surfaces, aes(x, ycoord, fill = link_contribution)) +
  geom_raster() +
  coord_equal() +
  facet_grid(component ~ time) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0
  ) +
  labs(x = "X", y = "Y", fill = "Link-scale\ncontribution") +
  theme_bw()
```

![Fitted persistent spatial field, spatiotemporal field, and their sum
from the tinyVAST
example.](spatial-spatiotemporal_files/figure-html/tinyvast-fields-1.png)

Fitted persistent spatial field, spatiotemporal field, and their sum
from the tinyVAST example.

## Multivariate tinyVAST responses

`tinyVAST` can fit multiple responses, including responses with
different families. `influ2` keeps the common schema and prefixes each
component with its response name. This small non-spatial example
isolates that interface; the same response labels are used when spatial
and spatiotemporal terms are added.

``` r

multivariate_data <- rbind(
  transform(
    tiny_data,
    response = count,
    var = "count",
    dist = "poisson"
  ),
  transform(
    tiny_data,
    response = as.numeric(count > 0),
    var = "encounter",
    dist = "binomial"
  )
)

multivariate_model <- tinyVAST::tinyVAST(
  response ~ factor(time),
  data = multivariate_data,
  family = list(
    poisson = poisson(),
    binomial = binomial()
  ),
  spatial_domain = NULL
)
multivariate_diagnostic <- influ(
  multivariate_model,
  focus = "time"
)
unique(multivariate_diagnostic$influence[c("term", "component")])
#>            term             component
#> 1  factor(time)     count:conditional
#> 13 factor(time) encounter:conditional
```

## Joint field uncertainty

The fixed-effect bands use each model’s joint maximum-likelihood
covariance. Spatial and spatiotemporal intervals use sparse
joint-precision simulation. The implementation processes parameter draws
in small batches and retains only focus-by-term diagnostic draws, never
an observations-by-draws latent field array. For delta models,
occurrence and positive fields are taken from the same joint draw before
unconditional-mean influence is calculated. This preserves
cross-component dependence while keeping memory use bounded.

`uncertainty = "none"` remains available as a fast fitted-mode preview.
An explicit `reference_data` grid, optionally with `reference_weights`,
can be used to hold the spatial and covariate standardisation
distribution fixed across model comparisons.

The same reference is used to centre the fitted-effect panel in a CDI
plot. For a binomial logit component, as in the Pacific cod example,
centred effects remain in log-odds, with zero as the reference. A
log-link component, as in the simulated count example, uses relative
effects about one on a logarithmic axis. Select the relevant response
and component when comparing different fields or mixed-family responses.
The fitted field maps above remain on their original link scale; they
describe the underlying field, whereas the influence panels describe its
contribution relative to the chosen reference distribution.

## References

Anderson, Sean C., Eric J. Ward, Phil A. English, Lewis A. K. Barnett,
and James T. Thorson. 2025. “sdmTMB: An r Package for Fast, Flexible,
and User-Friendly Generalized Linear Mixed Effects Models with Spatial
and Spatiotemporal Random Fields.” *Journal of Statistical Software* 115
(2): 1–46. <https://doi.org/10.18637/jss.v115.i02>.

Thorson, James T., Sean C. Anderson, Pamela Goddard, and Christopher N.
Rooper. 2025. “tinyVAST: R Package with an Expressive Interface to
Specify Lagged and Simultaneous Effects in Multivariate Spatio-Temporal
Models.” *Global Ecology and Biogeography* 34 (4): e70035.
<https://doi.org/10.1111/geb.70035>.
