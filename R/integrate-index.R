#' Calculate an area-integrated expected-response index
#'
#' Sum expected responses times supplied cell areas over a common prediction
#' domain in every observed year. This is a total, not a standardisation mean.
#' No model is refitted and no observation-level predictive noise is added.
#' @md
#'
#' @inheritParams cpue_index
#' @param area Cell areas, as a numeric vector of length `nrow(reference_data)`,
#'   one explicitly supplied common cell area, or the name of a numeric column
#'   in `reference_data`. Zero-area cells are excluded. At least one must be
#'   positive. Areas must not overlap or double-count the domain.
#' @param area_units Non-empty area-unit label, e.g. `"km^2"`. Supply areas in
#'   the same area units as the denominator of the density response. No unit
#'   conversion or coordinate-based area inference is performed.
#' @param response_units Non-empty label for the model's response at the
#'   supplied reference exposure, e.g. `"kg/km^2"` or `"kg/tow"`.
#' @param catchability Optional known positive conversion from underlying
#'   density to expected CPUE: predictions are divided by this quantity before
#'   integration. Supply a scalar, one value per reference row, or a column
#'   name. `NULL` applies no conversion. Uncertainty in catchability is not
#'   propagated. Its units must convert the stated response into density per
#'   `area_units`; a dimensionless value cannot convert kg/tow into kg/km^2.
#' @param units Optional resulting-unit label. By default the response and area
#'   labels are combined explicitly, without claiming absolute biomass.
#' @param averaging_weights Optional non-negative weights for repeated
#'   seasonal or other reference strata within each cell, supplied like `area`.
#'   For a seasonal mean, these must sum to one within each spatial cell (e.g.
#'   `1 / 12` for twelve equally weighted months). They are not normalised
#'   internally. `NULL` uses one per row, appropriate for one row per cell.
#'
#' @details The calculation is
#'   `sum(area * averaging_weights * expected_response / catchability)`
#'   within each year; the divisor is one when catchability is `NULL`.
#'   `reference_data` has no year column: the same domain and reference
#'   covariates are used in every observed year. Temporal model effects and
#'   included spatiotemporal fields can still change predictions by year.
#'   This interface does not forecast, infer a domain, or construct a
#'   year-varying environmental grid.
#'
#'   A density in kg/km^2 multiplied by km^2 yields kg. CPUE in kg/tow multiplied
#'   by km^2 remains an area-weighted CPUE index, **not absolute biomass**,
#'   unless an appropriate catchability/exposure conversion is supplied.
#'   Area-integrated encounter probabilities describe expected occupied area
#'   under the stated encounter definition, not abundance. Supplying a unit
#'   label does not validate these scientific assumptions.
#'
#'   All six [cpue_index()] backends are supported with the same prediction
#'   conventions and model-structure guards. Smooth spatial predictors in GLMs
#'   and GAMs can be integrated just like dedicated spatial fields. Joint
#'   covariance or shared draws propagate dependence across cells and years;
#'   cell standard errors are never added as if predictions were independent.
#'   Frequentist estimates are plug-in expectations, not Laplace bias-corrected
#'   totals. brms summaries use posterior expected-response draws.
#'   Areas, reference covariates, and catchability are treated as known.
#'
#'   `rescale = 1` returns a relative series with geometric mean one and
#'   propagates uncertainty in its common normalising denominator. A raw total
#'   cannot be recovered from that relative result alone. Standardised means
#'   and area-integrated totals cannot be mixed silently in [plot_compare()].
#'
#' @return An `influ_index` with the same assessment-table columns as
#'   [cpue_index()], `Method = "integrated"`, and area/unit metadata. Use
#'   [plot_index()] or [plot_compare()] without repeating calculations.
#' @seealso [cpue_index()], [plot_index()], [plot_compare()]
#' @examples
#' d <- data.frame(year = factor(rep(1:3, each = 20)),
#'                 depth = rep(seq(10, 50, length.out = 20), 3))
#' d$density <- exp(1 + 0.1 * as.numeric(d$year) - 0.01 * d$depth) +
#'              rep(c(-0.1, 0.1), 30)
#' fit <- glm(density ~ year + depth, family = Gamma(link = "log"), data = d)
#' grid <- data.frame(depth = c(15, 30, 45))
#' total <- integrate_index(fit, grid, area = c(2, 3, 5),
#'   area_units = "km^2", response_units = "kg/km^2", units = "kg")
#' as.data.frame(total)
#' @export
integrate_index <- function(model, reference_data, area, year = NULL,
    area_units, response_units, catchability = NULL, units = NULL,
    uncertainty = c("auto", "none"), probs = c(0.025, 0.975), rescale = "raw",
    ndraws = 1000L, batch_size = 250L, draw_batch_size = 100L,
    retain = c("summary", "draws"), averaging_weights = NULL,
    spatial_fields = c("all", "spatial", "spatiotemporal", "none"),
    seed = 1L, prediction_offset = NULL) {
  if (!is.data.frame(reference_data) || !nrow(reference_data)) {
    stop("Supply a non-empty reference_data prediction domain.", call. = FALSE)
  }
  if (missing(area)) stop("Supply explicit cell areas; they are not inferred from coordinates.", call. = FALSE)
  if (missing(area_units) || missing(response_units)) {
    stop("Supply both `area_units` and `response_units` explicitly.", call. = FALSE)
  }
  for (value in list(area_units, response_units)) {
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
      stop("Area and response units must be non-empty strings.", call. = FALSE)
    }
  }
  resolve <- function(x, name, positive = FALSE) {
    if (is.character(x) && length(x) == 1L && !is.na(x) && x %in% names(reference_data)) {
      x <- reference_data[[x]]
    }
    if (!is.numeric(x) || !length(x) || !is.null(dim(x)) ||
        !length(x) %in% c(1L, nrow(reference_data)) ||
        any(!is.finite(x)) || any(if (positive) x <= 0 else x < 0)) {
      stop("`", name, "` must supply ", if (positive) "positive" else "non-negative",
        " finite values, one per reference row or one common value.", call. = FALSE)
    }
    rep(x, length.out = nrow(reference_data))
  }
  area <- resolve(area, "area")
  if (!any(area > 0) || !is.finite(sum(area))) stop("Total area must be positive and finite.", call. = FALSE)
  q <- if (is.null(catchability)) rep(1, nrow(reference_data)) else resolve(catchability, "catchability", TRUE)
  averaging <- if (is.null(averaging_weights)) rep(1, nrow(reference_data)) else {
    resolve(averaging_weights, "averaging_weights")
  }
  weight <- area * averaging / q
  multiplier <- sum(weight)
  if (!is.finite(multiplier) || multiplier <= 0 || any(!is.finite(weight))) {
    stop("Area/catchability weights must have a positive finite sum.", call. = FALSE)
  }
  if (is.null(units)) units <- paste0(response_units, " * ", area_units,
    if (!is.null(catchability)) " / catchability")
  result <- cpue_index(model, year = year, method = "standardised",
    reference_data = reference_data, reference_weights = weight,
    uncertainty = match.arg(uncertainty), probs = probs, rescale = rescale,
    ndraws = ndraws, batch_size = batch_size, draw_batch_size = draw_batch_size,
    retain = match.arg(retain), units = units, spatial_fields = match.arg(spatial_fields),
    seed = seed, prediction_offset = prediction_offset)
  if (identical(rescale, "raw")) {
    for (column in c("Mean", "Median", "SD", "Qlower", "Qupper")) {
      result$table[[column]] <- result$table[[column]] * multiplier
    }
    if (!is.null(result$draws)) result$draws <- result$draws * multiplier
    if (!is.null(result$covariance$response)) {
      result$covariance$response <- result$covariance$response * multiplier^2
    }
    # Multiplication by a known positive constant only translates log indices.
  }
  if (any(!is.finite(result$table$Mean))) stop("Integrated expectations overflowed; check areas and units.", call. = FALSE)
  result$table$Method <- "integrated"
  result$metadata$method <- "integrated"
  result$metadata$reference <- "fixed common domain; area-weighted response total"
  result$metadata$area_units <- area_units
  result$metadata$response_units <- response_units
  result$metadata$total_area <- sum(area * averaging)
  result$metadata$positive_area_rows <- sum(area * averaging > 0)
  result$metadata$averaging <- if (is.null(averaging_weights)) "one per row" else "explicit within-cell averaging weights"
  result$metadata$catchability <- if (is.null(catchability)) "not adjusted" else "known conversion; uncertainty excluded"
  result$metadata$integration <- "sum(area * averaging weight * expected response / catchability)"
  result
}
