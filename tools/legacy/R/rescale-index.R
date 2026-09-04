#' Rescale two indices to have the same geometric mean
#' 
#' @param fit1,fit2 Data frames containing an index in a `Mean` column and a
#'   common time variable in a `Year` column. Interval columns named
#'   `Qlower`, `Qupper`, and `Median`, when present, are rescaled with the mean.
#' @param rescale_series Which input, `1` or `2`, supplies the reference
#'   geometric mean.
#' @return A list containing the two index data frames on a common scale.
#' 
#' @import dplyr
#' @export
#' 
rescale_index <- function(fit1, fit2, rescale_series = 1) {
  if (!rescale_series %in% c(1, 2)) {
    stop("`rescale_series` must be 1 or 2.", call. = FALSE)
  }
  indices <- list(fit1, fit2)
  valid <- vapply(
    indices,
    function(x) is.data.frame(x) && all(c("Year", "Mean") %in% names(x)),
    logical(1)
  )
  if (!all(valid)) {
    stop("`fit1` and `fit2` must be data frames with `Year` and `Mean` columns.",
         call. = FALSE)
  }

  target <- 3L - rescale_series
  common_years <- intersect(indices[[rescale_series]]$Year, indices[[target]]$Year)
  if (!length(common_years)) {
    stop("The two indices do not share any `Year` values.", call. = FALSE)
  }
  reference_mean <- geo_mean(
    indices[[rescale_series]]$Mean[indices[[rescale_series]]$Year %in% common_years]
  )
  target_mean <- geo_mean(indices[[target]]$Mean[indices[[target]]$Year %in% common_years])
  multiplier <- reference_mean / target_mean
  columns <- intersect(c("Mean", "Qlower", "Qupper", "Median"), names(indices[[target]]))
  indices[[target]][columns] <- lapply(indices[[target]][columns], `*`, multiplier)
  indices
}
