% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Framework.R
\name{findProcess}
\alias{findProcess}
\title{Find a processes from processName or functionName}
\usage{
findProcess(projectPath, modelName, processName = NULL, functionName = NULL)
}
\arguments{
\item{projectPath}{The path to the StoX project, i.e., the folder of the project with the sub folders "input", "output" and "process". Can possibly be the path to a file inside the project folder.}

\item{modelName}{The name of the model (possible values are "baseline", "analysis" and "report").}

\item{processName}{The name of the process.}

\item{functionName}{The name of the function used by the process.}
}
\description{
Find a processes from processName or functionName
}
