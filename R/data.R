#' Simulated CPUE data
#'
#' Simulated catch of lobsters per pot from 2000 to 2017, with changing
#' seasonal, depth, and soak-time coverage.
#'
#' @format a \code{tibble} containing 5 fields including:
#' \describe{
#'   \item{lobsters}{Number of lobsters caught in one pot.}
#'   \item{year}{Factor identifying fishing year, from 2000 to 2017.}
#'   \item{month}{Two-digit factor identifying calendar month.}
#'   \item{depth}{Fishing depth in metres.}
#'   \item{soak}{Pot soak time in hours.}
#'  }
#'
"lobsters_per_pot"
