% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Framework.R
\name{getProcessIndexFromProcessID}
\alias{getProcessIndexFromProcessID}
\title{Function to get process index from process ID}
\usage{
getProcessIndexFromProcessID(projectPath, modelName = NULL, processID)
}
\arguments{
\item{projectPath}{The path to the StoX project, i.e., the folder of the project with the sub folders "input", "output" and "process". Can possibly be the path to a file inside the project folder.}

\item{modelName}{The name of the model (possible values are "baseline", "analysis" and "report").}

\item{processID}{The ID of the process.}
}
\description{
Function to get process index from process ID
}
