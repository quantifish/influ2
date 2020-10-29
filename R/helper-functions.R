#' Geometric mean
#' 
#' @param a a vector.
#' @return The geometric mean of the vector.
#' 
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' 
#' @export
#' 
geo_mean <- function(a) {
  prod(a)^(1.0 / length(a))
}


#' Inverse logit
#' 
#' @param a a vector.
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' @export
#' 
inv_logit <- function(a) {
  exp(a) / (exp(a) + 1)
}


#' logit
#' 
#' @param p a vector.
#' @author Darcy Webber \email{darcy@quantifish.co.nz}
#' @export
#' 
logit <- function(p) {
  log(p / (1 - p))
}
