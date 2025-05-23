% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-cdi.R
\name{plot_bayesian_cdi2}
\alias{plot_bayesian_cdi2}
\title{Bayesian version of the CDI plot (depreciated)}
\usage{
plot_bayesian_cdi2(
  fit,
  xfocus = "area",
  yfocus = "fishing_year",
  hurdle = FALSE,
  xlab = "Month",
  ylab = "Fishing year",
  colour = "purple"
)
}
\arguments{
\item{fit}{a model fit}

\item{xfocus}{The column name of the variable to be plotted on the x axis. This column name must match one of the
column names in the \code{data.frame} that was passed to \code{brm} as the \code{data} argument.}

\item{yfocus}{The column name of the variable to be plotted on the y axis. This column name must match one of the
column names in the \code{data.frame} that was passed to \code{brm} as the \code{data} argument. This is generally the
temporal variable in a generalised linear model (e.g. year).}

\item{hurdle}{if a hurdle model then use the hurdle}

\item{xlab}{the x axis label}

\item{ylab}{the y axis label}

\item{colour}{the colour to use in the plot}
}
\value{
a ggplot object
}
\description{
Bayesian version of the CDI plot (depreciated)
}
