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
	 "..EDSUInfoToKeep", "..PSU", "..activeProcessID", "..areNumeric", "..digits",
	 "..functionInputs", "..functionInputs_UseProcessData", "..functionName",
	 "..functionParameters", "..haulInfoToKeep", "..idCol", "..newProcessName",
	 "..presentVariables", "..processDirty", "..propertyDirty", "..signifDigits", "..skipNAAt",
	 "..stationInfoToKeep", "..subsetByNAOn_New", "..subsetByNAOn_Old",
	 "..temporaryScaleFromResampling", "..validInd", "..x1x2y1y2", "AcoCat", "BeamKey",
	 "BootstrapID", "BootstrapSampleFactor", "CruiseKey", "DateTime", "Haul", "Layer", "LogKey",
	 "LogOrigin", "LogOrigin2", "N", "PSU", "PolygonKey", "ProcessName", "ResampleFunction",
	 "SampleUnit", "Station", "StationWeight", "StoX", "Stratum", "WeightingFactor",
	 "assignmentPasted", "canShowInMap", "ch", "currentVersion", "enabled", "freq",
	 "functionInputError", "functionInputProcessIDs", "functionInputsRecursive",
	 "functionInputs_UseProcessData", "functionName", "functionOutputDataType",
	 "functionParameters", "hasBeenRun", "hasProcessData", "listOf", "modelName", "name",
	 "numStations", "offset", "oldCurrentVersion", "packageName", "possibleValues", "processDirty",
	 "processID", "processIndex", "processName", "resamplingFactor", "start_time", "tableName",
	 "terminalProcess", "transceiver", "usedInProcessIDs", "usedInProcessIndices",
	 "usedInRecursiveProcessIndices", "usedInRecursiveProcessNames", "value", "variableName",
	 "verbose", "weightsPasted"))

.onLoad <- function(libname, pkgname) {
	# Initiate the RstoxFramework environment:
	initiateRstoxFramework()
} 

# Packages to import to NAMESPACE (typically packages which are used extensively or packcages with special syntax that requires import, e.g, data.table)
#' @import data.table
NULL

