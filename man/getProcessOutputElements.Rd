% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Framework.R
\name{getProcessOutputElements}
\alias{getProcessOutputElements}
\title{Get the names of the output tables of a process}
\usage{
getProcessOutputElements(projectPath, modelName, processID)
}
\arguments{
\item{projectPath}{The path to the StoX project, i.e., the folder of the project with the sub folders "input", "output" and "process". Can possibly be the path to a file inside the project folder.}

\item{modelName}{The name of the model (possible values are "baseline", "analysis" and "report").}

\item{processID}{The ID of the process.}
}
\description{
Get the names of the output tables of a process
}
