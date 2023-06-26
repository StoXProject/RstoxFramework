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

getPossibleVariables <- function(BootstrapNetCDF4Data, BaselineProcess) {
    #  Open the file:
    nc <- ncdf4::nc_open(unlist(BootstrapNetCDF4Data))
    on.exit(ncdf4::nc_close(nc))
    # Read the baseline process names:
    processNameAndTableName <- getProcessNameAndTableName(BaselineProcess, nc)
    #processNameSlashTableName <- paste(processNameAndTableName, collapse = "/")
    
    # Get all variables:
    processNameTableNameVariableName <- getProcessNameTableNameVariableName(nc)
    processNameTableNameVariableName <- subset(processNameTableNameVariableName, ProcessName == processNameAndTableName[1] & TableName == processNameAndTableName[2])
    
    return(processNameTableNameVariableName$VariableName)
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
    #BootstrapNetCDF4 = list(
    #    functionType = "bootstrapNetCDF4", 
    #    functionCategory = "analysis", 
    #    functionOutputDataType = "BootstrapNetCDF4Data", 
    #    functionParameterFormat = list(
    #        BootstrapMethodTable = "bootstrapMethodTable", 
    #        OutputProcesses = "outputProcesses", 
    #        OutputVariables = "outputVariables_BootstrapNetCDF4", 
    #        BaselineSeedTable = "baselineSeedTable"
    #    )
    #), 
    
    # Original bootstrap function using RData:
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
    
    ReportBootstrap = list(
        functionType = "modelData", 
        functionCategory = "report", 
        functionOutputDataType = "ReportBootstrapData", 
        # This is an example of using an expression to determine when to show a parameter:
        functionParameterFormat = list(
            GroupingVariables = "groupingVariables_ReportBootstrap", 
            InformationVariables = "informationVariables_ReportBootstrap", 
            TargetVariableUnit = "targetVariableUnit_ReportBootstrap", 
            Percentages = "percentages_ReportBootstrap"
        ), 
        functionArgumentHierarchy = list(
            AggregationWeightingVariable = list(
                AggregationFunction = expression(RstoxBase::getWeightingFunctions())
            ), 
            BootstrapReportWeightingVariable = list(
                BootstrapReportFunction = expression(RstoxBase::getWeightingFunctions())
            ), 
            Percentages = list(
                BootstrapReportFunction = expression(RstoxBase::getSpecificationFunctions())
            )
        ), 
        functionParameterDefaults = list(
            Percentages = c(5, 50, 95)
        )
    ), 
    
    # Add this in StoX 4.0.0:
    #ReportBootstrapNetCDF4 = list(
    #    functionType = "modelData", 
    #    functionCategory = "report", 
    #    functionOutputDataType = "ReportBootstrapNetCDF4Data", 
    #    # This is an example of using an expression to determine when to show a parameter:
    #    functionParameterFormat = list(
    #        BaselineProcess = "baselineProcess_ReportBootstrapNetCDF4", 
    #        TargetVariable = "targetVariable_ReportBootstrapNetCDF4", 
    #        TargetVariableUnit = "targetVariableUnit_ReportBootstrapNetCDF4", 
    #        GroupingVariables = "groupingVariables_ReportBootstrapNetCDF4", 
    #        InformationVariables = "informationVariables_ReportBootstrapNetCDF4", 
    #        Percentages = "percentages_ReportBootstrap"
    #    ), 
    #    functionArgumentHierarchy = list(
    #        AggregationWeightingVariable = list(
    #            AggregationFunction = expression(RstoxBase::getWeightingFunctions())
    #        ), 
    #        BootstrapReportWeightingVariable = list(
    #            BootstrapReportFunction = expression(RstoxBase::getWeightingFunctions())
    #        ), 
    #        Percentages = list(
    #            BootstrapReportFunction = expression(RstoxBase::getSpecificationFunctions())
    #        )
    #    ), 
    #    functionParameterDefaults = list(
    #        Percentages = c(5, 50, 95), 
    #        NetCDF4ChunkSize = Inf
    #    )
    #), 
    
    ### ReportBootstrapNetCDF4_lowMemory = list(
    ###     functionType = "modelData", 
    ###     functionCategory = "report", 
    ###     functionOutputDataType = "ReportBootstrapNetCDF4Data", 
    ###     # This is an example of using an expression to determine when to show a parameter:
    ###     functionParameterFormat = list(
    ###         BaselineProcess = "baselineProcess_ReportBootstrapNetCDF4", 
    ###         TargetVariable = "targetVariable_ReportBootstrapNetCDF4", 
    ###         TargetVariableUnit = "targetVariableUnit_ReportBootstrapNetCDF4", 
    ###         GroupingVariables = "groupingVariables_ReportBootstrapNetCDF4", 
    ###         InformationVariables = "informationVariables_ReportBootstrapNetCDF4", 
    ###         Percentages = "percentages_ReportBootstrap"
    ###     ), 
    ###     functionArgumentHierarchy = list(
    ###         AggregationWeightingVariable = list(
    ###             AggregationFunction = expression(RstoxBase::getWeightingFunctions())
    ###         ), 
    ###         BootstrapReportWeightingVariable = list(
    ###             BootstrapReportFunction = expression(RstoxBase::getWeightingFunctions())
    ###         ), 
    ###         Percentages = list(
    ###             BootstrapReportFunction = expression(RstoxBase::getSpecificationFunctions())
    ###         )
    ###     ), 
    ###     functionParameterDefaults = list(
    ###         Percentages = c(5, 50, 95)
    ###     )
    ### ), 
    
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
    resamplableDataTypes<- intersect(resamplableDataTypes, baselineDataTypes)
    
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
    
    targetVariable_ReportBootstrap = list(
        class = "single", 
        title = "Select TargetVariable for ReportBootstrap",
        possibleValues = function(BootstrapData, BaselineProcess) {
            sort(setdiff(names(BootstrapData[[BaselineProcess]]), "BootstrapID"))
        }
    ), 
    
    groupingVariables_ReportBootstrap = list(
        class = "vector", 
        title = "One or more variables to group super-individuals by when reporting BootstrapData", 
        #possibleValues = function(BootstrapData, BaselineProcess) {
        #    sort(setdiff(names(BootstrapData[[BaselineProcess]]), "BootstrapID"))
        #}, 
        possibleValues = list(), 
        variableTypes <- "character"
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
        variableTypes <- "character"
    ), 
    
    
    outputVariables_BootstrapNetCDF4 = list(
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
    
    
    
    
    informationVariables_ReportBootstrap = list(
        class = "vector", 
        title = "One or more columns to inlcude in ReportBootstrapData", 
        possibleValues = function(BootstrapData, BaselineProcess, GroupingVariables) {
            sort(setdiff(names(BootstrapData[[BaselineProcess]]), GroupingVariables))
        }, 
        variableTypes <- "character"
    ), 
    
    targetVariableUnit_ReportBootstrap = list(
        class = "vector", 
        title = "Select Unit for the TargetVariable", 
        possibleValues = function(BootstrapData, BaselineProcess, TargetVariable) {
            # If the specified process name does not exist in the BootstrapData:
            if(!BaselineProcess %in% names(BootstrapData)) {
                return(list())
            }
            
            dataType <- attr(BootstrapData[[BaselineProcess]], "dataType")
            quantity <- RstoxBase::getBaseUnit(dataType = dataType, variableName = TargetVariable, element = "quantity")
            if(is.na(quantity)) {
                list()
            }
            else {
                RstoxData::getUnitOptions(quantity)
            }
        }
    ), 
    targetVariableUnit_ReportBootstrapNetCDF4 = list(
        class = "vector", 
        title = "Select Unit for the TargetVariable", 
        possibleValues = function(BootstrapNetCDF4Data, BaselineProcess, TargetVariable) {
            nc <- ncdf4::nc_open(unlist(BootstrapNetCDF4Data))
            on.exit(ncdf4::nc_close(nc))
            dataType <- getDataTypeFromBootstrapNetCDF4(nc, BaselineProcess)
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
        class = "vector", 
        title = "Select plotting variable.", 
        possibleValues = getPlottingVariable_PlotReportBootstrap
    ), 
    plottingVariableLower_PlotReportBootstrap = list(
        class = "vector", 
        title = "Select variable for upper end of error bars.", 
        possibleValues = getPlottingVariable_PlotReportBootstrap
    ), 
    plottingVariableUpper_PlotReportBootstrap = list(
        class = "vector", 
        title = "Select variable for lower end of error bars.", 
        possibleValues = getPlottingVariable_PlotReportBootstrap
    ), 
    cvVariable_PlotReportBootstrap = list(
        class = "vector", 
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
    
    
    baselineProcess_ReportBootstrapNetCDF4 = list(
        class = "vector", 
        title = "Select one baseline process to report from.", 
        possibleValues = function(BootstrapNetCDF4Data) {
            #  Open the file:
            nc <- ncdf4::nc_open(unlist(BootstrapNetCDF4Data))
            on.exit(ncdf4::nc_close(nc))
            # Read the baseline process names:
            processNamesAndTableNames <- getProcessNamesAndTableNames(nc)
            processNames <- unique(processNamesAndTableNames$processName)
            
            return(processNames)
        }, 
        variableTypes <- "character"
    ),
    
    
    
    
    targetVariable_ReportBootstrapNetCDF4 = list(
        class = "vector", 
        title = "Select one variable to report from.", 
        possibleValues = function(BootstrapNetCDF4Data, BaselineProcess) {
            possibleVariables <- getPossibleVariables(BootstrapNetCDF4Data, BaselineProcess)
            
            output <- setdiff(possibleVariables, "nrow")
            
            return(output)
        }, 
        variableTypes <- "character"
    ), 
    
    groupingVariables_ReportBootstrapNetCDF4 = list(
        class = "vector", 
        title = "One or more variables to group super-individuals by when reporting BootstrapNetCDF4Data", 
        possibleValues = function(BootstrapNetCDF4Data, BaselineProcess, TargetVariable) {
            possibleVariables <- getPossibleVariables(BootstrapNetCDF4Data, BaselineProcess)
            
            output <- setdiff(possibleVariables, c("nrow", TargetVariable))
            
            return(output)
        }, 
        variableTypes <- "character"
    ), 
    
    informationVariables_ReportBootstrapNetCDF4 = list(
        class = "vector", 
        title = "One or more variables to group super-individuals by when reporting BootstrapNetCDF4Data", 
        possibleValues = function(BootstrapNetCDF4Data, BaselineProcess, TargetVariable, GroupingVariables) {
            possibleVariables <- getPossibleVariables(BootstrapNetCDF4Data, BaselineProcess)
            
            output <- setdiff(possibleVariables, c("nrow", TargetVariable, GroupingVariables))
            
            return(output)
        }, 
        variableTypes <- "character"
    )
    
)



