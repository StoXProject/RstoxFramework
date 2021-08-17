% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/API.R
\name{runProject}
\alias{runProject}
\title{Run all models of a StoX project}
\usage{
runProject(
  projectPath,
  modelNames = getRstoxFrameworkDefinitions("stoxModelNames"),
  processes = NULL,
  startProcess = 1,
  endProcess = Inf,
  drop.datatype = TRUE,
  unlistDepth2 = FALSE,
  run = TRUE,
  save = TRUE,
  force.restart = FALSE,
  replaceDataList = list(),
  replaceArgsList = list(),
  fileOutput = NULL,
  setUseProcessDataToTRUE = TRUE,
  purge.processData = FALSE,
  try = TRUE,
  close = FALSE,
  unlist.models = TRUE,
  ...
)
}
\arguments{
\item{projectPath}{The path to the StoX project, i.e., the folder of the project with the sub folders "input", "output" and "process". Can possibly be the path to a file inside the project folder.}

\item{modelNames}{The name of the model(s) (possible values are "baseline", "analysis" and "report").}

\item{processes}{The name of the process(es).}

\item{startProcess}{The process index, name or ID at which to start and stop the model run.}

\item{endProcess}{The process index, name or ID at which to start and stop the model run.}

\item{drop.datatype}{Logical: If TRUE, drop the top level of the output list if it has length 1 and that single element is named by the datatype name.}

\item{unlistDepth2}{Logical: Related to \code{drop.datatype}, but setting this to TRUE unlists output data that are nested in 2 levels, such as output from \code{\link[RstoxData]{ReadBiotic}}, which outputs a set of tables for each input file. Using unlistDepth2 = TRUE puts all these tables in one list, and uses the concatenation of the file names and the table name separated by underscore. This is how it is displayed in the StoX GUI when selecting "View output".}

\item{run}{Logical: If TRUE run the model.}

\item{save}{Logical: If TRUE save the project before closing. Default (NULL) is to ask the user whether to save the project before closing.}

\item{force.restart}{Logical: If TRUE, start the processes even if the status file indicating that the model is being run exists. This is useuful when something crached in a preivous run, in which case the model is still appearing as running.}

\item{replaceDataList}{A list named by the processes to replace output data for. See \code{\link{runProcess}}.}

\item{replaceArgsList}{A list of \code{replaceArgs} holding parameters to replace in the function call, named by the processes to modify.}

\item{fileOutput}{Logical: If TRUE save the output as a text file (or other format specified by the class or attributes of the output). If NULL (defafult) use the corresponding parameter of the process.}

\item{setUseProcessDataToTRUE}{Logical: If TRUE set the UseProcessData function parameter to TRUE in the process memory after execution, if the process is a ProcessData process.}

\item{purge.processData}{Logical: If TRUE replace process data entirely.}

\item{try}{Logical: If FALSE do not run the process in a \code{tryCatch}. Set this to FALSE when debugging, as the \code{tryCatch} masks the errors in the \code{traceback}.}

\item{close}{Logical: If TRUE close the project after running and getting the output.}

\item{unlist.models}{Logical: If TRUE unlist the top level so that all processes are in one list.}

\item{...}{\code{replaceArgsList} can also be given directly.}
}
\value{
A list of model output.
}
\description{
This function runs and returns output from all models of a StoX project.
}