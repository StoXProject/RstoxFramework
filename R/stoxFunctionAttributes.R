getPlottingVariable_PlotReportBootstrap <- function(ReportBootstrapData) {
    # Get the original GroupingVariables and InformationVariables used in the report:
    originalGroupingVariables <- attr(ReportBootstrapData, "GroupingVariables")
    originalInformationVariables <- attr(ReportBootstrapData, "InformationVariables")
    setdiff(names(ReportBootstrapData), c(originalGroupingVariables, originalInformationVariables))
}

getGroupingVariables_PlotReportBootstrap <- function(ReportBootstrapData) {
    # Get the original GroupingVariables and InformationVariables used in the report:
    originalGroupingVariables <- attr(ReportBootstrapData, "GroupingVariables")
    originalInformationVariables <- attr(ReportBootstrapData, "InformationVariables")
    c(originalGroupingVariables, originalInformationVariables)
}

getPossibleVariables <- function(BootstrapData, BaselineProcess) {
    
    #  Open the file:
    if(length(unlist(BootstrapData))) {
        nc <- ncdf4::nc_open(unlist(BootstrapData))
        on.exit(ncdf4::nc_close(nc))
    }
    else {
        warning("StoX: Bootstrap output NetCDF4 file missing.")
        return(NULL)
    }
    
    # Read the baseline process names:
    processNameAndTableName <- getProcessNameAndTableName(BaselineProcess, nc, na.rm = FALSE)
    #processNameSlashTableName <- paste(processNameAndTableName, collapse = "/")
    
    # Get all variables:
    processNameTableNameVariableName <- getProcessNameTableNameVariableName(nc)
    processNameTableNameVariableName <- subset(processNameTableNameVariableName, processName == processNameAndTableName[1] & if(is.na(processNameAndTableName[2])) is.na(tableName) else tableName == processNameAndTableName[2])
    
    output <- sort(unique(processNameTableNameVariableName$variableName))
    
    return(output)
}   
    
    
    
    
    
getPossibleVariables.nc <- function(BootstrapData, BaselineProcess, exceptions, nc.classes = c("double", "int", "character")) {
    
    #  Open the file:
    if(length(unlist(BootstrapData))) {
        nc <- ncdf4::nc_open(unlist(BootstrapData))
        on.exit(ncdf4::nc_close(nc))
    }
    else {
        warning("StoX: Bootstrap output NetCDF4 file missing.")
        return(NULL)
    }
    
    # Get the process, table and variable names, with classes:
    processNameTableNameVariableName <- getProcessNameTableNameVariableName(nc, add.class = TRUE)
    possibleVariables <- subset(processNameTableNameVariableName, processName == BaselineProcess & class %in% nc.classes)$variableName
    
    # The NetCDF4 file has an nrow defined that is not part of the actual data:
    possibleVariables <- setdiff(possibleVariables, "nrow")
    
    # Get the columns not used as TargetVariable, GroupingVariables or  InformationVariables:
    possibleVariables <- 
        sort(setdiff(possibleVariables, exceptions))
    
    return(possibleVariables)
}



#' A list of the attributes of the exported StoX functions:
#' 
#' The format describes the actual content, such as catchabilityTable, filePath, filter, etc. These are used by StoX to choose action on these parameters.
#' The primitive type (one of integer, double, logical, character) will be interpreted in the process property functions from the type of the function input or parameter.
#' 
#' @export
#' 
stoxFunctionAttributes <- list(
    # Add this in StoX 4.0.0:
    # Bootstrap baseline:
    Bootstrap = list(
        functionType = "bootstrap", 
        functionCategory = "analysis", 
        functionOutputDataType = "BootstrapData", 
        functionParameterFormat = list(
            BootstrapMethodTable = "bootstrapMethodTable", 
            OutputProcesses = "outputProcesses", 
            OutputVariables = "outputVariables_Bootstrap", 
            BaselineSeedTable = "baselineSeedTable"
        )
    ), 
    
    # Add this in StoX 4.0.0:
    ReportBootstrap = list(
        functionType = "modelData", 
        functionCategory = "report", 
        functionOutputDataType = "ReportBootstrapData", 
        # This is an example of using an expression to determine when to show a parameter:
        functionParameterFormat = list(
            BaselineProcess = "baselineProcess_ReportBootstrap", 
            TargetVariable = "targetVariable_ReportBootstrap", 
            TargetVariableUnit = "targetVariableUnit_ReportBootstrap", 
            GroupingVariables = "groupingVariables_ReportBootstrap", 
            InformationVariables = "informationVariables_ReportBootstrap", 
            Percentages = "percentages_ReportBootstrap",
            WeightingVariable = "weightingVariable_ReportBootstrap", 
            ConditionOperator = "conditionOperator", 
            FractionOverVariable = "fractionOverVariable"
        ), 
        functionArgumentHierarchy = c(
            list(
                AggregationFunction = list(
                    ReportFunction = "CompletelyUnlikelyFunctionNameDesignedJustToNotShowTheAggregationFunctionInTheGUI"
                )
            ), 
            # The specification parameters for Baseline:
            RstoxBase::getFunctionArgumentHierarchyForSpcificationParameters(use = "Baseline", functionName = "ReportFunction"), 
            # The specification parameters for Boostrap:
            RstoxBase::getFunctionArgumentHierarchyForSpcificationParameters(use = "Bootstrap", functionName = "BootstrapReportFunction"), 
            list(
                TargetVariableUnit = list(
                    ReportFunction = function(functionArguments) {
                        if(length(functionArguments$ReportFunction)) {
                            !startsWith(functionArguments$ReportFunction, "fractionOf")
                        }
                        else {
                            FALSE
                        }
                        
                    }
                )
            )
        ), 
        functionParameterDefaults = list(
            Percentages = c(5, 50, 95), 
            GroupingVariables = c("Survey", "SpeciesCategory")
        )
    ), 
    
    
    PlotReportBootstrap = list(
        functionType = "modelData", 
        functionCategory = "report", 
        functionOutputDataType = "PlotReportBootstrapData", 
        functionParameterFormat = list(
            GroupingVariables = "groupingVariables_PlotReportBootstrap", 
            SubPlots = "subPlots_PlotReportBootstrap", 
            PlottingVariable = "plottingVariable_PlotReportBootstrap", 
            PlottingVariableLower = "plottingVariableLower_PlotReportBootstrap", 
            PlottingVariableUpper = "plottingVariableUpper_PlotReportBootstrap", 
            CVVariable = "cvVariable_PlotReportBootstrap"
        ),
        functionArgumentHierarchy = list(
            CVVariable = list(
                AddCVToPlot = TRUE
            ), 
            # Options for the labels and other text:
            Title = list(
                UseDefaultTextSettings = FALSE
            ), 
            AxisTitleSize = list(
                UseDefaultTextSettings = FALSE
            ), 
            AxisTickSize = list(
                UseDefaultTextSettings = FALSE
            ), 
            LegendTitleSize = list(
                UseDefaultTextSettings = FALSE
            ), 
            LegendTextSize = list(
                UseDefaultTextSettings = FALSE
            ), 
            # Options for the output file:
            Format = list(
                UseDefaultFileSettings = FALSE
            ), 
            Width = list(
                UseDefaultFileSettings = FALSE
            ), 
            Height = list(
                UseDefaultFileSettings = FALSE
            ), 
            DotsPerInch = list(
                UseDefaultFileSettings = FALSE
            )
        ), 
        functionParameterDefaults = c(
            # Default general options:
            RstoxBase::getRstoxBaseDefinitions("defaultPlotOptions")$defaultPlotGeneralOptions, 
            # Default file options:
            RstoxBase::getRstoxBaseDefinitions("defaultPlotOptions")$defaultPlotFileOptions
        )
    )
)

#' Utility function for processPropertyFormats. This is exported in order for processPropertyFormats to be albe to use it:
#' 
#' @inheritParams general_arguments
#' 
#' @export
#' 
getResamplableProcesses <- function(projectPath) {
   
    # Get the data types that can be resampled:
    resamplableDataTypes <- getRstoxFrameworkDefinitions("resamplableDataTypes")
    # Find the functions that can be resampled:
    stoxLibrary <- getRstoxFrameworkDefinitions("stoxLibrary")
    atValidFunction <- lapply(resamplableDataTypes, getValidFunctionsOneResamplableDataType, stoxLibrary = stoxLibrary)
    atValidFunction <- do.call(cbind, atValidFunction)
    atValidFunction <- rowSums(atValidFunction) > 0
    validFunctions <- names(stoxLibrary)[atValidFunction]
    
    # Get the baseline processes with valid functions:
    baselineProcesses <- getProcessAndFunctionNames(
        projectPath = projectPath, 
        modelName = "baseline"
    )
    
    processNames <- baselineProcesses[getFunctionNameFromPackageFunctionName(functionName) %in% validFunctions, processName]
    
    return(processNames)
}

# Function to get a logical vector finding resamplable functions for one data type:
getValidFunctionsOneResamplableDataType <- function(resamplableDataType, stoxLibrary) {
    hasResamplableDataType <- sapply(stoxLibrary, "[[", "functionOutputDataType")  %in% resamplableDataType
    functionTypes <- sapply(stoxLibrary, "[[", "functionType")
    unequalFunctionTypes <- !RstoxBase::allEqual(functionTypes[hasResamplableDataType])
    if(sum(hasResamplableDataType) > 1 && unequalFunctionTypes) {
        # Accept only process data when both process data and model data have the same data type (speicfiaclly DefineBioticAssignment and BioticAssignmentWeighting)
        hasResamplableDataType <- hasResamplableDataType & functionTypes == "processData"
    }
    return(hasResamplableDataType)
}



#' Utility function for processPropertyFormats. This is exported in order for processPropertyFormats to be able to use it:
#' 
#' @inheritParams general_arguments
#' 
#' @export
#' 
getResampleFunctions <- function(projectPath) {
    
    # Get the baseline processes with valid functions:
    baselineProcesses <- getProcessAndFunctionNames(
        projectPath = projectPath, 
        modelName = "baseline"
    )
    baselineDataTypes <- baselineProcesses[, sapply(functionName, getStoxFunctionMetaData, metaDataName = "functionOutputDataType")]
    
    
    #paste0("Resample", getRstoxFrameworkDefinitions("resamplableDataTypes"))
    resamplableDataTypes <- getRstoxFrameworkDefinitions("resamplableDataTypes")
    resamplableDataTypes <- intersect(resamplableDataTypes, baselineDataTypes)
    
    unname(unlist(getRstoxFrameworkDefinitions("resampleFunctions")[resamplableDataTypes]))
}


#' Process property formats for RstoxFramework
#' 
#' @export
#' 
processPropertyFormats <- list(
    bootstrapMethodTable = list(
        class = "table", 
        title = "Define the bootstrap method",
        columnNames = c(
            "ResampleFunction", 
            "ProcessName", 
            #"ResampleBy", 
            "Seed"
        ), 
        variableTypes = c(
            "character",
            "character", 
            #"character",
            "integer"
        ), 
        possibleValues = function(projectPath) {
            # Must be an unnamed list:
            possibleValues = list(
                getResampleFunctions(projectPath),
                getResamplableProcesses(projectPath), 
                NULL
            )
        }
        #possibleValues = list(
        #    NULL, 
        #    getResampleFunctions(),
        #    NULL
        #)
    ), 
    
    baselineSeedTable = list(
        class = "table", 
        title = "Define the seeds for Baseline processes with a Seed parameter",
        columnNames = c(
            "ProcessName", 
            "Seed"
        ), 
        variableTypes = c(
            "character",
            "integer"
        ), 
        possibleValues = function(projectPath, BootstrapMethodTable, OutputProcesses) {
            # Get the processes to run:
            processesSansProcessData <- getProcessesSansProcessData(projectPath, modelName = "baseline", startProcess = BootstrapMethodTable$ProcessName, endProcess = OutputProcesses, return.processIndex = TRUE, only.valid = TRUE)
            # Scan through the baseline processes to be run and look for processes with the parameter Seed:
            hasSeed <- sapply(processesSansProcessData$functionParameters, function(x) "Seed" %in% names(x))
            
            # Must be an unnamed list:
            possibleValues = list(
                processesSansProcessData$processName[hasSeed], 
                NULL
            )
        }
    ), 
    
    
    outputProcesses = list(
        class = "vector", 
        title = "One or more processes to store in BootstrapData", 
        possibleValues = function(projectPath, BootstrapMethodTable) {
            if(!NROW(BootstrapMethodTable)) {
                stop("StoX: The BootstrapMethodTable must be specified.")
            }
            # Get the process table:
            processIndexTable <- readProcessIndexTable(projectPath, modelName = "baseline", startProcess = BootstrapMethodTable$ProcessName, endProcess = Inf)
            
            # Must be an unnamed list:
            possibleValues = as.list(processIndexTable$processName)
        }, 
        variableTypes <- "character"
    ), 
    
    outputVariables_Bootstrap = list(
        class = "vector", 
        title = "One or more variables to store in BootstrapData across processes", 
        possibleValues = function(projectPath, OutputProcesses) {
            # Get the outputs of the OutputProcesses:
            outputProcessesIDs <- 
            processOutput <- getModelData(
                projectPath = projectPath, 
                modelName = "baseline", 
                processes = OutputProcesses, 
                drop.datatype = FALSE
            )
            
            # Get the unique variable names:
            possibleValues = unique(unlist(lapply(processOutput, function(x) lapply(x, names))))
            # Must be an unnamed list:
            possibleValues = as.list(possibleValues)
            
            return(possibleValues)
        }, 
        variableTypes <- "character"
    ), 
    

    targetVariableUnit_ReportBootstrap = list(
        class = "single", 
        title = "Select Unit for the TargetVariable", 
        possibleValues = function(BootstrapData, BaselineProcess, TargetVariable) {
            #  Open the file:
            if(length(unlist(BootstrapData))) {
                nc <- ncdf4::nc_open(unlist(BootstrapData))
                on.exit(ncdf4::nc_close(nc))
            }
            else {
                warning("StoX: Bootstrap output NetCDF4 file missing.")
                return(NULL)
            }
            
            dataType <- getDataTypeFromBootstrap(nc, BaselineProcess)
            quantity <- RstoxBase::getBaseUnit(dataType = dataType, variableName = TargetVariable, element = "quantity")
            if(is.na(quantity)) {
                list()
            }
            else {
                RstoxData::getUnitOptions(quantity)
            }
        }
    ), 
    
    groupingVariables_PlotReportBootstrap = list(
        class = "vector", 
        title = "Select 1 or 2 GroupingVariables for the plot (defining the x-axis).", 
        possibleValues = getGroupingVariables_PlotReportBootstrap
    ), 
    
    subPlots_PlotReportBootstrap = list(
        class = "vector", 
        title = "Select sub plots.", 
        possibleValues = getSubPlotNames_PlotReportBootstrap
    ), 
    
    plottingVariable_PlotReportBootstrap = list(
        class = "single", 
        title = "Select plotting variable.", 
        possibleValues = getPlottingVariable_PlotReportBootstrap
    ), 
    plottingVariableLower_PlotReportBootstrap = list(
        class = "single", 
        title = "Select variable for upper end of error bars.", 
        possibleValues = getPlottingVariable_PlotReportBootstrap
    ), 
    plottingVariableUpper_PlotReportBootstrap = list(
        class = "single", 
        title = "Select variable for lower end of error bars.", 
        possibleValues = getPlottingVariable_PlotReportBootstrap
    ), 
    cvVariable_PlotReportBootstrap = list(
        class = "single", 
        title = "Select CV variable.", 
        possibleValues = getPlottingVariable_PlotReportBootstrap
    ),
    
    percentages_ReportBootstrap = list(
        class = "vector", 
        title = "Percentages defining the percentiles in the summaryStox function.", 
        possibleValues = function(...) {
            list()
        },
        variableTypes = "double"
    ), 
    
    
    baselineProcess_ReportBootstrap = list(
        class = "single", 
        title = "Select one baseline process to report from.", 
        possibleValues = function(BootstrapData) {
            #  Open the file:
            if(length(unlist(BootstrapData))) {
                nc <- ncdf4::nc_open(unlist(BootstrapData))
                on.exit(ncdf4::nc_close(nc))
            }
            else {
                warning("StoX: Bootstrap output NetCDF4 file missing.")
                return(NULL)
            }
            
            # Read the baseline process names:
            processNamesAndTableNames <- getProcessNamesAndTableNames(nc)
            processNames <- unique(processNamesAndTableNames$processName)
            
            return(processNames)
        }, 
        variableTypes <- "character"
    ),
    
    
    
    
    targetVariable_ReportBootstrap = list(
        class = "single", 
        title = "Select one variable to report from.", 
        possibleValues = function(BootstrapData, BaselineProcess) {
            #possibleVariables <- getPossibleVariables(BootstrapData, BaselineProcess)
            
            #output <- setdiff(possibleVariables, "nrow")
            
            getPossibleVariables.nc(
                BootstrapData, 
                BaselineProcess, 
                exceptions = NULL, 
                nc.classes = c("double", "int", "char")
            )
            
            #return(output)
        }, 
        variableTypes <- "character"
    ), 
    
    groupingVariables_ReportBootstrap = list(
        class = "vector", 
        title = "One or more variables to group super-individuals by when reporting BootstrapData", 
        possibleValues = function(BootstrapData, BaselineProcess, TargetVariable) {
            possibleVariables <- getPossibleVariables(BootstrapData, BaselineProcess)
            
            output <- setdiff(possibleVariables, c("nrow", TargetVariable))
            
            return(output)
        }, 
        variableTypes <- "character"
    ), 
    
    informationVariables_ReportBootstrap = list(
        class = "vector", 
        title = "One or more variables to group super-individuals by when reporting BootstrapData", 
        possibleValues = function(BootstrapData, BaselineProcess, TargetVariable, GroupingVariables) {
            possibleVariables <- getPossibleVariables(BootstrapData, BaselineProcess)
            
            output <- setdiff(possibleVariables, c("nrow", TargetVariable, GroupingVariables))
            
            return(output)
        }, 
        variableTypes <- "character"
    ), 
    
    weightingVariable_ReportBootstrap = list(
        class = "single", 
        title = "Select weighting variable", 
        possibleValues = function(BootstrapData, BaselineProcess, TargetVariable, GroupingVariables, InformationVariables) {
            #nc <- ncdf4::nc_open(unlist(BootstrapData))
            #on.exit(ncdf4::nc_close(nc))
            ## Get the process, table and variable names, with classes:
            #processNameTableNameVariableName <- getProcessNameTableNameVariableName(nc, add.class = TRUE)
            #possibleVariables <- subset(processNameTableNameVariableName, processName == BaselineProcess & class %in% c("double", "int"))$variableName
            ## The NetCDF4 file has an nrow defined that is not part of the actual data:
            #possibleVariables <- setdiff(possibleVariables, "nrow")
            #
            ## Get the columns not used as TargetVariable, GroupingVariables or  InformationVariables:
            #possibleVariables <- 
            #    sort(setdiff(possibleVariables, c(TargetVariable, GroupingVariables, InformationVariables)))
            #
            #return(possibleVariables)
            
            getPossibleVariables.nc(
                BootstrapData, 
                BaselineProcess, 
                exceptions = c(TargetVariable, GroupingVariables, InformationVariables), 
                nc.classes = c("double", "int")
            )
        }, 
        variableTypes = "character"
    ), 
    
    directoryPath = list(
        class = "single", 
        title = "The path to a folder", 
        variableTypes = "character"
    ), 
    
    filePaths = list(
        class = "vector", 
        title = "The path to one or more files", 
        variableTypes = "character"
    ), 
    
    conditionOperator = list(
        class = "single", 
        title = "Select ConditionOperator", 
        possibleValues = c("%in%", "%notin%", "==", "!=", "%notequal%", "<", "<=", ">=", ">")
    ),
    
    fractionOverVariable = list(
        class = "single", 
        title = "Select variable to sum over in the denominator of the fraction", 
        possibleValues = function(GroupingVariables) {
            GroupingVariables
        }, 
        variableTypes = "character"
    )
    #,
    #
    #fileExtension = list(
    #    class = "single", 
    #    title = "File extension", 
    #    variableTypes = "character", 
    #    possibleValues = function(FolderName) {
    #        if(missing(FolderName) || !length(FolderName)) {
    #            ext <- NULL
    #        }
    #        else {
    #            files <- list.files(FolderName)
    #            ext <- unique(tools::file_ext(files))
    #        }
    #        
    #        return(ext)
    #    }
    #)
    
)



