% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-index.R
\name{plot_hurdle}
\alias{plot_hurdle}
\title{Plot the hurdle and positive components}
\usage{
plot_hurdle(fit, year = "Year", fill = "purple", probs = c(0.025, 0.975))
}
\arguments{
\item{fit}{An object of class \code{brmsfit}.}

\item{year}{the year or time label.}

\item{fill}{the fill colour for the percentiles.}

\item{probs}{The percentiles to be computed by the \code{quantile} function.}
}
\value{
a \code{ggplot} object.
}
\description{
Plot the hurdle and positive components
}
\author{
Darcy Webber \email{darcy@quantifish.co.nz}
}
