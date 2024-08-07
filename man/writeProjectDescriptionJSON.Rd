% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Framework.R
\name{writeProjectDescriptionJSON}
\alias{writeProjectDescriptionJSON}
\title{Write project description to a file in json format}
\usage{
writeProjectDescriptionJSON(
  projectDescription,
  projectDescriptionFile,
  Application
)
}
\arguments{
\item{projectDescription}{a list of lists with project description.}

\item{projectDescriptionFile}{The path to the file holding the projectDescription (project.json).}

\item{Application}{A single string naming the application used when saving the project. Defaulted to R.version.string.}
}
\description{
This function writes project description to json file.
}
