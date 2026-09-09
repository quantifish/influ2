#' Geometric mean
#'
#' Calculate the geometric mean on the log scale to avoid overflow and
#' underflow from multiplying many values together.
#' @md
#'
#' @param a A numeric vector of non-negative, finite values. Missing values
#'   are allowed; negative values and infinities are rejected.
#' @param na.rm Remove missing values before calculation.
#' @return A number. Zero values give zero, missing values give `NA` unless
#'   removed, and an empty vector (after removal) gives `NaN`.
#' @examples
#' geo_mean(c(1, 4, 16))
#' geo_mean(c(1, NA, 4), na.rm = TRUE)
#' @export
geo_mean <- function(a, na.rm = FALSE) {
  if (!is.numeric(a) || is.complex(a) || !is.null(dim(a))) {
    stop("`a` must be a numeric vector.", call. = FALSE)
  }
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm)) {
    stop("`na.rm` must be TRUE or FALSE.", call. = FALSE)
  }
  if (any(is.infinite(a)) || any(a < 0, na.rm = TRUE)) {
    stop("`a` must contain non-negative, finite values or missing values.", call. = FALSE)
  }
  if (na.rm) a <- a[!is.na(a)]
  if (anyNA(a)) return(NA_real_)
  if (!length(a)) return(NaN)
  if (any(a == 0)) return(0)
  exp(mean(log(a)))
}
