#' Model-neutral influence diagnostics
#'
#' @description
#' \if{html}{\figure{logo.png}{options: align='right' alt='logo' width='120'}}
#'
#' Standardising catch per unit effort attempts to distinguish changes in
#' abundance from changes in fishing practice and sampling composition.
#' `influ2` calculates how fitted explanatory terms, random effects, and
#' spatial fields move a standardised index through time. All supported model
#' backends return the same compact [influ_diag] object and use the same plots.
#'
#' Start with [influ()], then inspect the result with [summary.influ_diag()] or
#' [plot.influ_diag()].
#'
#' @references
#' Bentley, N., Kendrick, T. H., Starr, P. J., and Breen, P. A. (2012).
#' Influence plots and metrics: tools for better understanding fisheries
#' catch-per-unit-effort standardizations. \emph{ICES Journal of Marine
#' Science}, 69(1), 84--88. \doi{10.1093/icesjms/fsr174}.
#'
#' @name influ2
NULL
