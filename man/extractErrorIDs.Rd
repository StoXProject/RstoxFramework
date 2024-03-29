% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Utilities.R
\name{extractErrorIDs}
\alias{extractErrorIDs}
\title{Get IDs of bad Hauls or Samples}
\usage{
extractErrorIDs(errorString, bullet = "* ", sep = ": ", collapse = "\\n")
}
\arguments{
\item{errorString}{A error string (as returned from \code{\link{runFunction}}).}

\item{bullet}{A character string giving the prefix used when listing bad Hauls or Samples.}

\item{sep}{A character string markingg the start of the reported ID.}

\item{collapse}{A character string separator between ID lines.}
}
\value{
An list of IDs.
}
\description{
Get IDs of bad Hauls or Samples
}
