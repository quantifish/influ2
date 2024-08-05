#' Influence plots
#'
#' @description 
#' \if{html}{\figure{logo.png}{options: align='right' alt='logo' width='120'}}
#' 
#' Standardisation of catch per unit effort using generalised linear models (GLMs) is a common procedure that attempts to remove the
#' confounding effects of variables other than abundance. The influ2 package can be used to generate plots and metrics to assist 
#' understanding the standardisation effects of explanatory variables included in GLMs.
#' 
#' The influ2 package provides two categories of important functions:
#' \code{get} and \code{plot}.
#' 
#' @section Get functions:
#' The get functions obtain information from \code{brmsfit} objects and do some processing of that data before returning it as 
#' a \code{data.frame}.
#' 
#' @section Plot functions:
#' The plot functions include a get function and an appropriate \code{ggplot2} geom to plot the output.
#'
#' @references 
#' Bentley, N., Kendrick, T. H., Starr, P. J., and Breen, P. A. (2011). Influence plots and metrics: tools for better 
#' understanding fisheries catch-per-unit-effort standardizations. \emph{ICES Journal of Marine Science}. 
#' \code{doi:10.1093/icesjms/fsr174}
#' 
#' @name influ2
#' 
NULL
