% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cbf_stats.R
\name{cbf.cumsumskipna}
\alias{cbf.cumsumskipna}
\title{Cummulative Skip NA}
\usage{
cbf.cumsumskipna(x, FUNC)
}
\arguments{
\item{x}{a numeric vector}

\item{FUNC}{FUNC can be any one of sum, prod, min, or max, and x is a numeric vector}
}
\value{

}
\description{
cbf.cumsumskipna from https://stackoverflow.com/questions/25576358/calculate-cumsum-while-ignoring-na-values
It calculates the cumulative FUNC of x while ignoring NA.
}
