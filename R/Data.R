##################################################
##################################################
#' General parameters of RstoxFramework.
#' 
#' All functions referring to a project, a model, a process or an output table use the same parameters, listed here.
#' 
#' @param projectPath The path to the StoX project, i.e., the folder of the project with the sub folders "input", "output" and "process". Can possibly be the path to a file inside the project folder.
#' @param projectPaths A vector of paths to the StoX projects.
#' @param projectDescriptionFile The path to the file holding the projectDescription (project.json).
#' @param modelName The name of the model (possible values are "baseline", "analysis" and "report").
#' @param modelNames The name of the models (possible values are "baseline", "analysis" and "report").
#' @param processID The ID of the process.
#' @param processName The name of the process.
#' @param processNames A vector of names of the processes.
#' @param processes The name of the processes.
#' @param functionName The name of the function used by the process. 
#' @param tableName The name of the output table to get from the process.
#' @param startProcess The process index, name or ID at which to start the model run.
#' @param endProcess The process index, name or ID at which to stop the model run.
#' @param beforeProcessID The ID of the process before which to get the process table.
#' @param afterProcessID The ID of the process after which to get the process table or to place a process.
#' @param warn Logical: If TRUE show warnings that are not highly important.
#' @param verbose Logical: If TRUE extra messages are printed to console.
#' @param msg Logical: If FALSE no messages are printed to console (except possibly for extremely important ones).
#' @param msg.GUI Logical: If TRUE, exlcude certain messages (used by the StoX GUI).
#' @param archive Logical: Should the project memory state be archived using \code{archiveProject}? This stores one step in the history.
#' @param template A string naming the template to use when generating the project. See \code{getAvaiableTemplates} for a list of available templates.
#' @param ow Logical: If TRUE overwrite the project.
#' @param showWarnings Logical: If TRUE display warninigs when creting the project folders.
#' @param Application A single string naming the application used when saving the project. Defaulted to R.version.string.
#' @param argumentFilePaths A nested list of paths to argument files of a model, as returned from \code{getArgumentFilePaths}. This is used to speed up some functions.
#' @param only.valid Logical: If TRUE subset function arguments (inputs and parameters) to only those to be shown as a consequence of argument hierarchy (e.g., one argument being irrelevant for a specific setting of another).
#' @param returnProcessTable Logical: If TRUE return the process table (much used in functions used by the GUI).
#' @param applyBackwardCompatibility Logical: If TRUE apply backward compatibility actions when running \code{readProjectDescription}.
#' @param Seed The seed, given as a single integer.
#' 
#' @name general_arguments
#' 
NULL


##################################################
##################################################
#' StoX data types of the RstoxFramework package
#' 
#' StoX data types are the data types used to transfer data and information between processes in a StoX estimation model. The data types are divided into two types, the \code{\link{ModelData}} and \code{\link{ProcessData}}.
#' 
#' The StoX output data can be written to file using the "Write output to file" process parameter in the StoX GUI which corresponds to the logical parameter fileOutput in the project description, which is set to TRUE by default. The output files are written differently based on the class of the output data from the process, with the following file types/file extensions: 
#' 
#' \tabular{llll}{
#' \bold{Output data class} \tab \bold{StoX data types} \tab \bold{Output file type} \tab \bold{Output file extension} \cr
#' NetCDF4 \tab Bootstrap \tab NetCDF4 \tab nc \cr
#' Spatial (multi)polygon. R class: sf \tab StratumPolygon \tab GeoJSON \tab geojson \cr
#' Table (R class: data.table) \tab ReadBiotic, StoxBiotic, NASC, etc \tab Tab separated, NA as "NA" \tab txt \cr
#' Plot. R class: ggplot \tab PlotReportBootstrap, etc \tab Default: PNG (user choice) \tab Default: png (user choice) \cr
#' Character matrix \tab WriteICESAcoustic, WriteICESBiotic, etc \tab Comma separated, NA as "" \tab csv \cr
#' Vector \tab WriteICESDatras, WriteICESDatsusc, etc \tab Comma separated \tab csv \cr
#' }
#' 
#' Output data cannot be composed of multiple output data classes.
#' 
#' @name DataTypes
#' 
NULL

##################################################
##################################################
#' Model data generated by RstoxFramework
#' 
#' The model data of a StoX model are the data generated during the model run based on input data and user settings and resources given in the project description (project.json file). Model data are transient and only exists from a process has been run until the project is closed. 
#' 
#' @param BootstrapData The \code{\link{BootstrapData}} data.
#' @param ReportBootstrapData The \code{\link{ReportBootstrapData}} data.
#' @param PlotReportBootstrapData The \code{\link{PlotReportBootstrapData}} data.
#' @param StratumAreaData The \code{\link[RstoxBase]{StratumAreaData}} data (defined in RstoxBase).
#' @param NASCData The \code{\link[RstoxBase]{NASCData}} data (defined in RstoxBase).
#' @param SumNASCData The \code{\link[RstoxBase]{SumNASCData}} data (defined in RstoxBase).
#' @param MeanNASCData The \code{\link[RstoxBase]{MeanNASCData}} data (defined in RstoxBase).
#' @param LengthDistributionData The \code{\link[RstoxBase]{LengthDistributionData}} data (defined in RstoxBase).
#' @param SumLengthDistributionData The \code{\link[RstoxBase]{SumLengthDistributionData}} data (defined in RstoxBase).
#' @param MeanLengthDistributionData The \code{\link[RstoxBase]{MeanLengthDistributionData}} data (defined in RstoxBase).
#' @param AssignmentLengthDistributionData The \code{\link[RstoxBase]{AssignmentLengthDistributionData}} data (defined in RstoxBase).
#' @param DensityData The \code{\link[RstoxBase]{DensityData}} data (defined in RstoxBase).
#' @param MeanDensityData The \code{\link[RstoxBase]{MeanDensityData}} data (defined in RstoxBase).
#' @param SpeciesCategoryCatchData The \code{\link[RstoxBase]{SpeciesCategoryCatchData}} data (defined in RstoxBase).
#' @param SumSpeciesCategoryCatchData The \code{\link[RstoxBase]{SumSpeciesCategoryCatchData}} data (defined in RstoxBase).
#' @param MeanSpeciesCategoryCatchData The \code{\link[RstoxBase]{MeanSpeciesCategoryCatchData}} data (defined in RstoxBase).
#' @param QuantityData The \code{\link[RstoxBase]{QuantityData}} data (defined in RstoxBase).
#' @param IndividualsData The \code{\link[RstoxBase]{IndividualsData}} data (defined in RstoxBase).
#' @param SuperIndividualsData The \code{\link[RstoxBase]{SuperIndividualsData}} data (defined in RstoxBase).
#' @param ReportSuperIndividualsData The \code{\link[RstoxBase]{ReportSuperIndividualsData}} data (defined in RstoxBase).
#' @param PlotAcousticTrawlSurveyData The \code{\link[RstoxBase]{PlotAcousticTrawlSurveyData}} data (defined in RstoxBase).
#' @param BioticData \code{\link[RstoxData]{BioticData}} (defined in RstoxData).
#' @param AcousticData \code{\link[RstoxData]{AcousticData}} (defined in RstoxData).
#' @param LandingData \code{\link[RstoxData]{LandingData}} (defined in RstoxData).
#' @param StoxBioticData \code{\link[RstoxData]{StoxBioticData}} (defined in RstoxData).
#' @param StoxAcousticData \code{\link[RstoxData]{StoxAcousticData}} (defined in RstoxData).
#' @param StoxLandingData \code{\link[RstoxData]{StoxLandingData}} (defined in RstoxData).
#' @param MergeStoxBioticData \code{\link[RstoxData]{MergeStoxBioticData}} (defined in RstoxData).
#' @param MergeStoxAcousticData \code{\link[RstoxData]{MergeStoxAcousticData}} (defined in RstoxData).
#' @param ICESBioticData \code{\link[RstoxData]{ICESBioticData}} (defined in RstoxData).
#' @param ICESAcousticData \code{\link[RstoxData]{ICESAcousticData}} (defined in RstoxData).
#' @param ICESDatrasData \code{\link[RstoxData]{ICESDatrasData}} (defined in RstoxData).
#' @param ICESDatsuscData \code{\link[RstoxData]{ICESDatsuscData}} (defined in RstoxData).
#' @param WriteICESBioticData \code{\link[RstoxData]{WriteICESBioticData}} (defined in RstoxData).
#' @param WriteICESAcousticData \code{\link[RstoxData]{WriteICESAcousticData}} (defined in RstoxData).
#' @param WriteICESDatrasData \code{\link[RstoxData]{WriteICESDatrasData}} (defined in RstoxData).
#' @param WriteICESDatsuscData \code{\link[RstoxData]{WriteICESDatsuscData}} (defined in RstoxData).
#' 
#' @name ModelData
#' 
#' @seealso \code{\link{ProcessData}} for process data types and \code{\link{DataTypes}} for all data types produced by \code{\link{RstoxFramework}}. See also \code{\link[RstoxData]{ModelData}} in RstoxData and \code{\link[RstoxBase]{ModelData}} in RstoxBase.
#' 
NULL


##################################################
##################################################
#' Process data used by RstoxFramework
#' 
#' There are currently no process data in RstoxFramework, but in the dependent packages RstoxData and RstoxBase.
#' 
#' @param StratumPolygon The \code{\link[RstoxBase]{StratumPolygon}} process data (defined in RstoxBase).
#' @param Survey The \code{\link[RstoxBase]{Survey}} process data (defined in RstoxBase).
#' @param AcousticLayer The \code{\link[RstoxBase]{AcousticLayer}} process data (defined in RstoxBase).
#' @param AcousticPSU The \code{\link[RstoxBase]{AcousticPSU}} process data (defined in RstoxBase).
#' @param BioticLayer The \code{\link[RstoxBase]{BioticLayer}} process data (defined in RstoxBase).
#' @param BioticPSU The \code{\link[RstoxBase]{BioticPSU}} process data (defined in RstoxBase).
#' @param BioticAssignment The \code{\link[RstoxBase]{BioticAssignment}} process data (defined in RstoxBase).
#' @param AcousticTargetStrength The \code{\link[RstoxBase]{AcousticTargetStrength}} process data (defined in RstoxBase).
#' @param Regression The \code{\link[RstoxBase]{Regression}} process data (defined in RstoxBase).
#' @param Translation \code{\link[RstoxData]{Translation}} (defined in RstoxData).
#'
#' @name ProcessData
#' 
#' @seealso \code{\link{ModelData}} for model data types and \code{\link{DataTypes}} for all data types produced by \code{\link[RstoxFramework]{RstoxFramework}}.
#' 
NULL


##################################################
##################################################
#' Bootstrap data
#' 
#' The BootstrapData model data is the path to a NetCDF4 memory file holding the outputs from the baseline generated by re-running baseline with random sampling of the PSUs and assignments (in the case of acoustic-trawl estimates).
#' 
#' @seealso This data type is produced by the function \code{\link{Bootstrap}}. See \code{\link{DataTypes}} for a list of all data types produced by \code{\link[RstoxData]{RstoxData}}, \code{\link[RstoxBase]{RstoxBase}} and \code{\link[RstoxFramework]{RstoxFramework}}.
#' 
#' @name BootstrapData
#' 
NULL


##################################################
##################################################
#' Reported Bootstrap data
#' 
#' The ReportBootstrapData model data is a report of the \code{\link{BootstrapData}} such as cv of Biomass per Stratum and SpeciesCategory, given as a table or list of tables.
#' 
#' @seealso This data type is produced by \code{\link{ReportBootstrap}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link[RstoxData]{RstoxData}}, \code{\link[RstoxBase]{RstoxBase}} and \code{\link[RstoxFramework]{RstoxFramework}}.
#' 
#' @name ReportBootstrapData
#' 
NULL

##################################################
##################################################
#' Plot of reported Bootstrap data
#' 
#' The PlotReportBootstrapData model data is a ggplot object or a list of ggplot objects generated from \code{\link{BootstrapData}}. The plots show e.g. error bars with lower, mid and upper value of Abundance per IndividualAge.
#' 
#' @seealso This data type is produced by \code{\link{PlotReportBootstrap}}. See \code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link[RstoxData]{RstoxData}}, \code{\link[RstoxBase]{RstoxBase}} and \code{\link[RstoxFramework]{RstoxFramework}}.
#' 
#' @name PlotReportBootstrapData
#' 
NULL

