% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cbfc.R
\name{cbf.coltype}
\alias{cbf.coltype}
\title{CBF Mutate Type function family}
\arguments{
\item{.x}{A dataframe (tibble) with column to be mutated}

\item{.cbfcols}{A [cbf.c()] or string of column name(s) to mutate}

\item{...}{Arguments passed to across}
}
\description{
This family of functions mutates a named set of columns (as a string input for now) to the desired type.
Note that the input is a [cbf.c]
}
\seealso{
Other coltype: 
\code{\link{cbf.coltype_char}()},
\code{\link{cbf.coltype_factor}()},
\code{\link{cbf.coltype_int}()},
\code{\link{cbf.coltype_numeric}()}
}
\concept{coltype}
