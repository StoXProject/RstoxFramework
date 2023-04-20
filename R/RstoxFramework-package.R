#' The Engine of StoX
#'
#' The framwork of StoX >= 3.0.
#'
#' The RstoxFramework package is the engine of the stock assesment utility StoX, which is an open source application fo acoustic-trawl and swept-area survey estimation.
#'
#' The package creates an evironment containing the StoX project(s) in separate environments. Each StoX project consists of a list of data tables holding e.g. biotic and acoustic data, filtered versions of the data, strata system, definitios of primary sampling units, accompanied by a list of specifications of the StoX processes comprising the StoX project. A StoX process is an R function taking as input the project name, the input data and parameters used by the function.
#'
#' The package replaces the old Java library in StoX versions prior to StoX 4.0.
#' @docType package
#' @name RstoxFramework
#'
"_PACKAGE"

# Global variables
utils::globalVariables(c(
	 "..BootstrapID", "..EDSUInfoToKeep", "..PSU", "..activeProcessID", "..clickPointNames",
	 "..coordinateNames", "..functionInputs", "..functionName", "..functionParameters",
	 "..haulInfoToKeep", "..ind", "..newProcessName", "..presentVariables", "..processDirty",
	 "..propertyDirty", "..skipNAAt", "..stationInfoToKeep", "..subsetByNAOn_New",
	 "..subsetByNAOn_Old", "..toKeep", "..validInd", "AcoCat", "BeamKey", "BootstrapData",
	 "BootstrapID", "BootstrapSampleFactor", "CruiseKey", "Haul", "Layer", "LogKey", "LogOrigin",
	 "LogOrigin2", "PSU", "Package", "PolygonKey", "ProcessName", "ResampleFunction", "SampleUnit",
	 "Station", "StationWeight", "StoX", "Stratum", "Version", "WeightingFactor",
	 "assignmentPasted", "binaryPath", "canShowInMap", "capture.output", "ch", "col2rgb",
	 "colorRampPalette", "download.file", "filePath", "freq", "functionInputError", "functionName",
	 "functionOutputDataType", "hasProcessData", "modelName", "name", "newVersion", "numStations",
	 "offset", "possibleValues", "processID", "processIndex", "processName", "projectPath",
	 "read.table", "remove.packages", "resampledCountWithUniqueName", "start_time", "transceiver",
	 "value", "variable", "verbose", "weightsPasted"))

.onLoad <- function(libname, pkgname) {
	# Initiate the RstoxFramework environment:
	initiateRstoxFramework()
} 

# Packages to import to NAMESPACE (typically packages which are used extensively or packcages with special syntax that requires import, e.g, data.table)
#' @import data.table
NULL

