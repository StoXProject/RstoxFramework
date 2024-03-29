% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Versions.R
\name{RstoxPackageStatus}
\alias{RstoxPackageStatus}
\title{Get package status of an Rstox package}
\usage{
RstoxPackageStatus(
  RstoxPackageName,
  StoXVersion,
  officialRstoxPackageVersionsFile
)
}
\arguments{
\item{RstoxPackageName}{The name of the Rstox package.}

\item{StoXVersion}{The version of the StoX GUI defining the combination of official Rstox package versions.}

\item{officialRstoxPackageVersionsFile}{The path to the file holding the link between StoX GUI version and Rstox package versions. If missing, the file on the RstoxFramework master on GitHub is used ("https://raw.githubusercontent.com/StoXProject/RstoxFramework/master/inst/versions/OfficialRstoxFrameworkVersions.txt").}
}
\description{
Get package status of an Rstox package
}
