% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Framework.R
\name{getProcessTable}
\alias{getProcessTable}
\alias{scanForModelError}
\alias{getProcessesSansProcessData}
\alias{getProcessAndFunctionNames}
\title{Functions to get the process table of a model.}
\usage{
getProcessTable(
  projectPath,
  modelName = NULL,
  argumentFilePaths = NULL,
  only.valid = TRUE,
  return.processIndex = FALSE,
  return.processFlow = TRUE
)

scanForModelError(
  projectPath,
  modelName = NULL,
  startProcess = 1,
  endProcess = Inf,
  afterProcessID = NULL,
  beforeProcessID = NULL,
  argumentFilePaths = NULL,
  only.valid = TRUE,
  return.processIndex = FALSE,
  check.only.enabled = TRUE
)

getProcessesSansProcessData(
  projectPath,
  modelName = NULL,
  startProcess = 1,
  endProcess = Inf,
  afterProcessID = NULL,
  beforeProcessID = NULL,
  argumentFilePaths = NULL,
  only.valid = FALSE,
  return.processIndex = FALSE,
  warn = TRUE
)

getProcessAndFunctionNames(
  projectPath,
  modelName = NULL,
  startProcess = 1,
  endProcess = Inf,
  afterProcessID = NULL,
  beforeProcessID = NULL,
  argumentFilePaths = NULL,
  return.processIndex = FALSE,
  warn = TRUE
)
}
\arguments{
\item{projectPath}{The path to the StoX project, i.e., the folder of the project with the sub folders "input", "output" and "process". Can possibly be the path to a file inside the project folder.}

\item{modelName}{The name of the model (possible values are "baseline", "analysis" and "report").}

\item{argumentFilePaths}{A nested list of paths to argument files of a model, as returned from \code{getArgumentFilePaths}. This is used to speed up some functions.}

\item{only.valid}{Logical: If TRUE subset function arguments (inputs and parameters) to only those to be shown as a consequence of argument hierarchy (e.g., one argument being irrelevant for a specific setting of another).}

\item{return.processIndex}{Logical: If TRUE include the process indices as a sequence starting from 1 in each model.}

\item{return.processFlow}{Logical: If TRUE include columns giving the processIDs and processNames of processes used as input to each process (processNames of these are already in) and of the processes that use the output from each process. Also, terminal processes are indicated.}

\item{startProcess}{The process index, name or ID at which to start the model run.}

\item{endProcess}{The process index, name or ID at which to stop the model run.}

\item{afterProcessID}{The ID of the process after which to get the process table or to place a process.}

\item{beforeProcessID}{The ID of the process before which to get the process table.}

\item{check.only.enabled}{Logical: If TRUE check input errors only for enabled procecsses.}

\item{warn}{Logical: If TRUE show warnings that are not highly important.}
}
\description{
Functions to get the process table of a model.
}
