% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Framework.R
\name{getProcessPlotOutput}
\alias{getProcessPlotOutput}
\title{Get output of a StoX process.}
\usage{
getProcessPlotOutput(projectPath, modelName, processID, plotName = NULL)
}
\arguments{
\item{projectPath}{The path to the StoX project, i.e., the folder of the project with the sub folders "input", "output" and "process". Can possibly be the path to a file inside the project folder.}

\item{modelName}{The name of the model (possible values are "baseline", "analysis" and "report").}

\item{processID}{The ID of the process.}

\item{plotName}{The name of the plot.}
}
\description{
Gets the output of a process that has been run.
}
