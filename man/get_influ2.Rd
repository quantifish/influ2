% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-influ.R
\name{get_influ2}
\alias{get_influ2}
\title{Get the influence metric}
\usage{
get_influ2(fit, group = c("fishing_year", "area"), hurdle = FALSE)
}
\arguments{
\item{fit}{An object of class \code{brmsfit}.}

\item{group}{the variable to obtain}

\item{hurdle}{if a hurdle model then use the hurdle}
}
\value{
a \code{data.frame}.
}
\description{
Get the influence metric
}
