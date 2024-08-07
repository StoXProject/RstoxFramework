% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/API.R
\name{readModelData}
\alias{readModelData}
\title{Read the output files of a project}
\usage{
readModelData(
  projectPath,
  modelName = NULL,
  processName = NULL,
  verifyFiles = FALSE,
  unlist = FALSE,
  emptyStringAsNA = FALSE,
  ...
)
}
\arguments{
\item{projectPath}{The path to the StoX project, i.e., the folder of the project with the sub folders "input", "output" and "process". Can possibly be the path to a file inside the project folder.}

\item{modelName}{The name of the model (possible values are "baseline", "analysis" and "report").}

\item{processName}{The name of the process.}

\item{verifyFiles}{Logical: If TRUE verify that the files are from processes that exist in the project.}

\item{unlist}{Either 1 to unlist the models, 2 to unlist the models and the process. TRUE is interpreted as 2.}

\item{emptyStringAsNA}{Logical: If TRUE, read empty strings as NA from the stored original tables, as RstoxFramework has started writing NAs as NAs and not as empty strings.}

\item{...}{Arguments passed to \code{\link{readBootstrapData}}, e.g. \code{selection}, which must be set to NA to read the entire file.}
}
\value{
A list of model output.
}
\description{
This function reads all or some of the output files of a project, indicated by model and process names.
}
