% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Framework.R
\name{readProcessOutputFile}
\alias{readProcessOutputFile}
\title{Function to read a single process output file, possibly by pages and in flattened and pretty view:}
\usage{
readProcessOutputFile(
  filePath,
  flatten = FALSE,
  pretty = FALSE,
  pretty.json = TRUE,
  pageindex = integer(0),
  linesPerPage = 1000L,
  columnSeparator = " ",
  lineSeparator = NULL,
  na = "-",
  enable.auto_unbox = FALSE,
  splitGeoJson = TRUE
)
}
\arguments{
\item{filePath}{The file path of the process output file to read.}

\item{flatten}{Logical: Should the output tables that contain cells of length > 1 be expanded to that the other columns are repeated, resulting in a regular table.}

\item{pretty}{Logical: If TRUE pad with space in each cell to the maximum number of characters of the column including header.}

\item{pretty.json}{Logical: If TRUE prettify the geojson.}

\item{pageindex}{A vector of the pages to return with \code{linesPerPage} number of lines (rows). Default is to not split into pages.}

\item{linesPerPage}{The number of lines per page if \code{pageindex} is given.}

\item{columnSeparator}{The string to separate columns by, defaulted to a single space.}

\item{lineSeparator}{The string to separate lines by, defaulted to a NULL, which keeps the output as a vector of strings.}

\item{na}{The string to replace NAs by, defaulted to "-".}

\item{enable.auto_unbox}{Logical: If TRUE wrap the output in a list if  \code{pretty} is TRUE and the output is of length 1. This keeps the array when converting to JSON also for length 1.}

\item{splitGeoJson}{Logical: If TRUE split the geojson into a vector of separate lines.}
}
\description{
Function to read a single process output file, possibly by pages and in flattened and pretty view:
}
