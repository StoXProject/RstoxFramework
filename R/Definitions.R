# 
# In DefineAcousticPSU and other functions requiring specific inputs we were for a while # thinking that these inputs should define which layers to plot in the map. We divert from # this and reserve the resposibility of plotting the appropriate layers to the user!


# We use CamelCase for StoX functions and the parameters shown in StoX, for data types, models, 
# We use camelCase for everything else.

# Should we use "~" for the workspace folder? Tilde gives the Documents folder on Windows, whereas StoX 2.7 uses the user folder.
#


##################################################
##################################################
#' Definitions stored in the RstoxFramework environment
#' 
#' This function declares the RstoxFramework environment and writes vital definitions to it.
#' 
#' @return
#' A list of definitions.
#' 
#' @seealso Use \code{\link{getRstoxFrameworkDefinitions}} to get the definitions.
#' 
initiateRstoxFramework <- function(){
    
    ##### Packages: #####
    officialStoxLibraryPackages <- c(
        "RstoxBase", 
        "RstoxData", 
        "RstoxFDA"
    )
    # Remove non-installed packages (typically packcages that are suggests):
    officialStoxLibraryPackages <- intersect(
        utils::installed.packages()[, "Package"], 
        officialStoxLibraryPackages
    )
    # Add RstoxFramework:
    officialStoxLibraryPackagesAll <- c("RstoxFramework", officialStoxLibraryPackages)
    # Get installed versions:
    InstalledRstoxPackageVersion <- getPackageVersion(officialStoxLibraryPackagesAll, only.version = FALSE)
    
    # Get the versions of the dependencies:
    ### dependentPackagesOnlyRstoxFramework <- getPackageVersion(
    ###     getDependencies(
    ###         "RstoxFramework", 
    ###         packageTable = NULL, 
    ###         repos = NA, 
    ###         recursive = FALSE, 
    ###         append = FALSE, 
    ###         sort = FALSE
    ###     )
    ### )
    ### dependentPackageVersionSansRstoxFramework <- getDependentPackageVersion(
    ###     packageName = officialStoxLibraryPackages, 
    ###     dependencyTypes = NA, 
    ###     Rstox.repos = NULL, 
    ###     # Get dependencies from the locally installed packates (setting nonRstox.repos to NULL). 
    ###     nonRstox.repos = NULL, 
    ###     sort = FALSE
    ### )
    ### dependentPackageVersion <- unique(c(dependentPackagesOnlyRstoxFramework, dependentPackageVersionSansRstoxFramework))
    
    # Get the versions of the dependencies:
    dependentPackageVersion <- getDependentPackageVersion(
        packageName = officialStoxLibraryPackagesAll, 
        dependencyTypes = c("Depends", "Imports", "LinkingTo") # Use the types explicitely, since the keyword "strong" was introduced in R 4.1, and will cause an error in R <= 4.0.
    )

    ### # Define formats for files saved by Rstox:
    ### memoryFileFormat_Empty <- "rds"
    ### # 2020-06-08: The fst::write_fst() does not retain the encoding, and has been discarded until these problems are fixed:
    ### #memoryFileFormat_Table <- "fst"
    ### memoryFileFormat_Table <- "rds"
    ### memoryFileFormat_Matrix <- "rds"
    ### memoryFileFormat_Spatial <- "rds"
    ### memoryFileFormat_List <- "rds"
    ### memoryFileFormat_Character <- "rds"
    ### memoryFileFormat_Numeric <- "rds"
    ### memoryFileFormat_Integer <- "rds"
    ### memoryFileFormat_Logical <- "rds"
    ### memoryFileFormat_Other <- "rds"
    ### allMemoryFileFormats <- unique(
    ###     c(
    ###         memoryFileFormat_Empty, 
    ###         memoryFileFormat_Table, 
    ###         memoryFileFormat_Matrix, 
    ###         memoryFileFormat_Spatial, 
    ###         memoryFileFormat_List, 
    ###         memoryFileFormat_Character, 
    ###         memoryFileFormat_Numeric, 
    ###         memoryFileFormat_Integer, 
    ###         memoryFileFormat_Logical, 
    ###         memoryFileFormat_Other
    ###     )
    ### )

    allMemoryFileFormats <- c(
        "rds", 
        "nc"
    )
    
    default.output.file.type <- list(
        baseline = "text", 
        analysis = "RData", 
        report = "text"
    )
    
    processProperties <- c(
        "processName", 
        "functionName", 
        "functionInputs", 
        "functionParameters", 
        "processParameters", 
        "processData"
    )
    
    orderProcessArguments <- function(process) {
        process[processProperties]
    }

    # Define the requested (all) function attributes:
    requestedFunctionAttributeNames <- c(
        "packageName", 
        "functionName", 
        "functionType", 
        "functionCategory", 
        "functionOutputDataType", 
        "functionParameterType", 
        "functionParameterFormat", 
        "functionArgumentHierarchy", 
        "functionParameterDefaults"
    )
    
    # Define parameters that are needed to run processData functions or bootstrap functions (or other kinds of special functions):
    systemParameters <- c(
        "processData", 
        "projectPath", 
        # Changed on 2020-10-22 to use the actual data and not the file:
        #"outputDataPath"
        "outputData", 
        # Added the outputMemoryFile for Bootstrap with netCDF4:
        "outputMemoryFile"
    )
    
    # Load the required packages to enable searching for formals and documentation, e.g. for getStoxFunctionParameterPossibleValues():
    lapply(officialStoxLibraryPackages, library, character.only = TRUE)
    
    # Get the stoxLibrary as the list of function attributes from all official packages:
    stoxLibrary <- getStoxLibrary(officialStoxLibraryPackagesAll, requestedFunctionAttributeNames = requestedFunctionAttributeNames)
    availableFunctions <- names(stoxLibrary)
    availablePackageFunctionNames <- unname(sapply(stoxLibrary, "[[", "functionName"))
    
    # Get all process data functions:
    processDataFunctions <- availableFunctions[sapply(stoxLibrary, "[[", "functionType") == "processData"]
    
    
    
    # Define the supported backward compatibility actions. The order of the actions is defined here!!!:
    backwardCompatibilityActionNames <- c(
        "renameAttribute", # 1
        "addAttribute", # 2
        "renameFunction", # 3
        "removeParameter", # 4
        "renameParameter", # 5
        "addParameter", # 6
        "translateParameter", # 7
        "reshapeParameter", # 8
        "renameProcessData", # 9
        "renameColumInProcessDataTable", # 10
        "reshapeProcessData" # 11
    )
    
    renameResampleFunctionInBootstrapMethodTableOne <- function(projectDescriptionOne, oldName, newName) {
        # Find any use of the ResampleBioticAssignment resampling function:
        hasOldName <- sapply(projectDescriptionOne$functionParameters$BootstrapMethodTable, function(x) x$ResampleFunction == oldName)
        if(any(hasOldName)) {
            atOldName <- which(hasOldName)
            for(ind in atOldName) {
                projectDescriptionOne$functionParameters$BootstrapMethodTable[[ind]]$ResampleFunction <- newName
            }
        }
        
        return(projectDescriptionOne$functionParameters$BootstrapMethodTable)
    }
    
    renameResampleFunctionInBootstrapMethodTable <- function(projectDescriptionOne, oldName, newName) {
        if(length(oldName) != length(newName)) {
            stop("oldName and newName must have equal length!")
        }
        for(ind in seq_along(oldName)) {
            projectDescriptionOne$functionParameters$BootstrapMethodTable <- renameResampleFunctionInBootstrapMethodTableOne(
                projectDescriptionOne = projectDescriptionOne, 
                oldName = oldName[ind], 
                newName = newName[ind]
            )
        }
        
        return(projectDescriptionOne$functionParameters$BootstrapMethodTable)
    }
    
    #getOutputVariablesUsedInReportFromProjectDescription <- function(projectDescription, modelName, processIndex) {
    #    # The report parameters to get variable names from:
    #    parameters <- c("GroupingVariables", "InformationVariables", "WeightingVariable", "TargetVariable")
    #    variableNames <- unique(unlist(lapply(lapply(projectDescription$report, "[[", "functionParameters"), "[", parameters)))
    #    
    #    processName <- projectDescription[[modelName]][[processIndex]]$processName
    #    if(length(variableNames)) {
    #        warning("StoX: The process ", processName, " gained the parameter OutputVariables with value", if(length(variableNames) > 1) "s", " ", paste0("\"", variableNames, "\"", collapse = ", "), " in order to speed up the process. If other variables than these are needed in reports, they must be added in the OutputVariables.")
    #    }
    #    
    #    return(variableNames)
    #}
    
    getEmptyOutputVariables <- function(projectDescription, modelName, processIndex) {
        
        parameters <- c("GroupingVariables", "InformationVariables", "WeightingVariable", "TargetVariable")
        variableNames <- unique(unlist(lapply(lapply(projectDescription$report, "[[", "functionParameters"), "[", parameters)))
        processName <- projectDescription[[modelName]][[processIndex]]$processName
        
        
        warning("StoX: The process ", processName, " has gained the parameter OutputVariables. It is recommended to use this parameter to save time and disk space! Existing StoX projects with bootstrapping may be slower in StoX >= 4.0.0 compared to StoX <= 3.6.2 if OutputVaribles is not used.", if(length(variableNames)) paste0(" The following variables are used in the Report model, and should at least be included in OutputVariables: ",  paste0("\"", variableNames, "\"", collapse = ", ") ) )
        
        return(NULL)
    }
    
    
    
    #### Backward compabitibility actions. These need not to be exported as is the case for any other Rstox-packages, since RstoxFramework is the package that collects the backwardCompatibility objects:
    backwardCompatibility_RstoxFramework <- list(
        renameAttribute = list(
            list(
                changeVersion = "1.2.39", 
                attributeName = "OfficalRstoxPackageVersion", 
                newAttributeName = "CertifiedRstoxPackageVersion"
            ), 
            list(
                changeVersion = "1.2.39", 
                attributeName = "AllOfficialRstoxPackageVersion", 
                newAttributeName = "AllCertifiedRstoxPackageVersion"
            )
        ), 
        
        addAttribute = list(
            list(
                changeVersion = "1.2.39", 
                attributeName = "OfficialRstoxFrameworkVersion", 
                attributeValue = FALSE
            )
        ), 
        
        removeParameter = list(
            list(
                changeVersion = "3.6.3-9007", 
                functionName = "ReportBootstrap", 
                modelName = "report", 
                parameterName = "BootstrapReportWeightingVariable"
            )
        ),  
        
        addParameter  = list(
            list(
                changeVersion = "3.0.19", 
                functionName = "Bootstrap", 
                modelName = "analysis", 
                parameterName = "BaselineSeedTable", 
                parameterValue = data.table::data.table(
                    ProcessName = "ImputeSuperIndividuals", 
                    Seed = 1
                )
            ), 
            list(
                changeVersion = "3.5.2", 
                functionName = "ReportBootstrap", 
                modelName = "report", 
                parameterName = "TargetVariableUnit"
            ), 
            list(
                changeVersion = "3.6.0-9003", 
                functionName = "ReportBootstrap", 
                modelName = "report", 
                parameterName = "Percentages", 
                parameterValue = c(5, 50, 95)
            ), 
            list(
                changeVersion = "3.6.3-9004", 
                functionName = "Bootstrap", 
                modelName = "analysis", 
                parameterName = "OutputVariables",
                parameterValue = getEmptyOutputVariables
            ), 
            list(
                changeVersion = "3.6.3-9007", 
                functionName = "ReportBootstrap", 
                modelName = "report", 
                parameterName = "ConditionOperator"
            ), 
            list(
                changeVersion = "3.6.3-9007", 
                functionName = "ReportBootstrap", 
                modelName = "report", 
                parameterName = "ConditionValue"
            ), 
            list(
                changeVersion = "3.6.3-9007", 
                functionName = "ReportBootstrap", 
                modelName = "report", 
                parameterName = "FractionOverVariable"
            )
        ), 
        
        renameParameter = list(
            list(
                changeVersion = "3.6.3-9007", 
                functionName = "ReportBootstrap", 
                modelName = "report", 
                parameterName = "AggregationWeightingVariable",
                newParameterName = "WeightingVariable"
            ),
            list(
                changeVersion = "3.6.3-9007", 
                functionName = "ReportBootstrap", 
                modelName = "report", 
                parameterName = "AggregationFunction",
                newParameterName = "ReportFunction"
            )
        ), 
        
        translateParameter = list(
            list(
                changeVersion = "3.6.3-9001", 
                functionName = "Bootstrap", 
                modelName = "analysis", 
                parameterName = "BootstrapMethodTable",
                # Multiple values must be given in a list!!! Also if only :
                value = function(value) {
                    TRUE # Translate regardless of the value.
                }, 
                newValue = function(projectDescription, modelName, processIndex) {
                    
                    renameResampleFunctionInBootstrapMethodTable(
                        projectDescription[[modelName]][[processIndex]], 
                        oldName = "ResampleBioticAssignment", 
                        newName = "ResampleBioticAssignmentByStratum"
                    )
                    ## Find any use of the ResampleBioticAssignment resampling function:
                    #hasResampleBioticAssignment <- sapply(projectDescriptionOne$functionParameters$BootstrapMethodTable, function(x) x$ResampleFunction == "ResampleBioticAssignment")
                    #if(any(hasResampleBioticAssignment)) {
                    #    atResampleBioticAssignment <- which(hasResampleBioticAssignment)
                    #    for(ind in atResampleBioticAssignment) {
                    #        projectDescriptionOne$functionParameters$BootstrapMethodTable[[ind]]$ResampleFunction <- "ResampleBioticAssignmentByStratum"
                    #    }
                    #}
                    
                    #return(projectDescriptionOne$functionParameters$BootstrapMethodTable)
                }
            )
        )
    )
    ####
    
    # Get the backward compatibility:
    backwardCompatibility <- lapply(officialStoxLibraryPackages, getBackwardCompatibility)
    #rm(
    #    renameResampleFunctionInBootstrapMethodTableOne, 
    #    renameResampleFunctionInBootstrapMethodTable, 
    #    getEmptyOutputVariables
    #)
    names(backwardCompatibility) <- officialStoxLibraryPackages
    # Add the backwardCompatibility_RstoxFramework:
    backwardCompatibility$RstoxFramework <- backwardCompatibility_RstoxFramework
    
    
    # Add packageName to all elements:
    for(packageName in names(backwardCompatibility)) {
        for(actionType in names(backwardCompatibility[[packageName]])) {
            if(length(backwardCompatibility[[packageName]][[actionType]])) {
                backwardCompatibility[[packageName]][[actionType]] <- lapply(backwardCompatibility[[packageName]][[actionType]], append, list(packageName = packageName))
            }
        }
    }
    
    # Order by changeVersion:
    backwardCompatibility <- lapply(backwardCompatibility, orderBackwardCompatibility)
    
    
    # Get the possible values of the functions. Here we use the full name of the functions in case the parameter defaults are defined using functions in the specific packages, such as ReportBootstrap(). In extractStoxFunctionParameterPossibleValues() the packageName RstoxFramework is discarded, as that package has not been loaded yet (this function is run onload):
    availableFunctionPossibleValues <- lapply(availablePackageFunctionNames, extractStoxFunctionParameterPossibleValues, systemParameters = systemParameters)
    names(availableFunctionPossibleValues) <- availableFunctions
    
    # Get the json schema for RstoxFramework:
    schema <- jsonlite::read_json(system.file("formats", "projectSchema.json", package = "RstoxFramework"))
    
    # Get the schemas of the Rstox packages:
    processDataSchemas <- lapply(officialStoxLibraryPackages, readProcessDataSchema)
    # Remove duplicates (by name) and unlist:
    toKeep <- split(!duplicated(unlist(lapply(processDataSchemas, names))), rep(seq_along(processDataSchemas), lengths(processDataSchemas)))
    processDataSchemas <- mapply("[", processDataSchemas, toKeep)
    processDataSchemas <- unlist(processDataSchemas, recursive = FALSE)

    
    # Get the names of the processData schemas:
    processDataSchemaNames <- names(processDataSchemas)
    processDataSchema <- list(
        processData = list(
            #  This supports both an empty array, which currently is what is written to the project.json for processes with no process data, and a list of process data. Note here that only process data with only one element (table or spatialPolygonsDataFrame) are enclosed in a list named by the data type. Process data with two or more elements are given as a list with the individual elements, and it is these elements that are validated:
            type = c("array", "object"),
            minItems = 0,
            patternProperties = list(
                "^.*$" = list(
                    anyOf = lapply(
                        processDataSchemaNames, 
                        function(x) list(
                            # Removed the quotation as it failed with jsonvalidate v1.3.1, which contained update to ajv version 8.5.0: 
                            # "$ref" = paste0("\"#/", x, "\"")
                            "$ref" = paste0("#/", x, "")
                        )
                    )
                )
            ),
            additionalProperties = FALSE
            #oneOf = lapply(
            #    processDataSchemaNames, 
            #    function(x) list(
            #        # Removed the quotation as it failed with jsonvalidate v1.3.1, which contained update to ajv version #8.5.0: 
            #        # "$ref" = paste0("\"#/", x, "\"")
            #        "$ref" = paste0("#/", x, "")
            #    )
            #) 
        )
    )
    
    # Paste the subSchemas to the RstoxFramework schema:
    schema <- jsonlite::toJSON(
        c(
            schema, 
            processDataSchema, 
            processDataSchemas
        ), 
        digits = NA, 
        auto_unbox = TRUE, 
        na = "null", 
        pretty = TRUE
    )
    # Create a project.json validator:
    projectValidator <- jsonvalidate::json_validator(schema)
    projectValidatorIMJV <- jsonvalidate::json_validator(schema, engine = "imjv")
    projectValidatorAJV <- jsonvalidate::json_validator(schema, engine = "ajv")
    
    getProcessDataColumnTypes <- function(processDataSchemas, onlyFirst = TRUE) {
        # Get the process data which are lists of tables, which are those that have properties:
        atMultiTableProcessData <- which(sapply(processDataSchemas, function(x) "properties" %in% names(x)))
        atSingleTableProcessData <- setdiff(seq_along(processDataSchemas), atMultiTableProcessData)
        # Find column types of the process data tables:
        columnTypes <- lapply(processDataSchemas[atSingleTableProcessData], function(x) sapply(x$items[[1]]$properties, function(x) utils::head(x$type, if(onlyFirst) 1 else Inf)))
        columnTypes <- rapply(columnTypes, function(x) replace(x, x == "string", "character"), how = "replace")
        columnTypes <- rapply(columnTypes, function(x) replace(x, x == "number", "double"), how = "replace")
        return(columnTypes)
    }
    processDataColumnTypes <- getProcessDataColumnTypes(processDataSchemas)
    processDataTypes <- names(processDataColumnTypes)
    
    
    
    
    getProcessDataItemTypes <- function(items) {
        sapply(processDataSchemas$Stratum_PSU$items[[1]]$properties, "[[", "type")
    }
    
    
    
    # Get the functions that can be resampled in bootstrapping:
    resamplableDataTypes <- c(
        "MeanNASCData",
        "MeanLengthDistributionData", 
        "MeanSpeciesCategoryCatchData", 
        "BioticAssignment"
    )
    # ... and the reample functions, 
    resampleFunctions <- list(
        MeanNASCData = "ResampleMeanNASCData",
        MeanLengthDistributionData = "ResampleMeanLengthDistributionData", 
        MeanSpeciesCategoryCatchData = "ResampleMeanSpeciesCategoryCatchData", 
        #BioticAssignment = "ResampleBioticAssignment" 
        BioticAssignment = c("ResampleBioticAssignmentByStratum", "ResampleBioticAssignmentByAcousticPSU")
    )
    
    
    #### Data types: ####
    oldStoxModelDataTypes <- c(
        "AcousticData",
        "BioticData",
        "LandingData",
        "NASC",
        "LengthDist",
        "Density",
        "Abundance",
        "IndividualDataStations",
        "IndividualData",
        "SuperIndividuals",
        "PolygonArea",
        "StationSpecCatDensity",
        "BioticCovData",
        "LandingCovData",
        "LandingWeightCovData",
        "ProcessData"
    )
    
    stoxDataTypes <- data.table::data.table(
        functionOutputDataType = sapply(stoxLibrary, "[[", "functionOutputDataType"), 
        functionType = sapply(stoxLibrary, "[[", "functionType"), 
        functionName = availableFunctions, 
        packageName = sapply(stoxLibrary, "[[", "packageName")
    )
    
    # Check that there are no functions with the same name as a datatype:
    commonFunctionAndDataTypeName <- intersect(stoxDataTypes$functionOutputDataType, stoxDataTypes$functionName)
    if(length(commonFunctionAndDataTypeName)) {
        warning("StoX: The function name ", paste0("\"", commonFunctionAndDataTypeName, "\"", collapse = ", "), " of the package ", paste0("\"", stoxDataTypes$packageName[stoxDataTypes$functionName == commonFunctionAndDataTypeName], "\"", collapse = ", "),  " is identical to the name of a data type. This may lead to unexpected errors when overriding a model using 'replaceArgs' and '...' in RstoxBase::runProcesses() and runModel(). Please notify the packcage maintainer.")
    }
    
    
    ##### Data: #####
    speciesVariables <- list(
        NMDBiotic1.4 = c(
            "commonname", 
            "catchcategory", 
            "aphia", 
            "scientificname"
        ), 
        NMDBiotic3.0 = c(
            "species", 
            "noname", 
            "aphia"
        ), 
        ICESBiotic1 = c(
            "SpeciesCode"
        )
    )
    
    #### Fundamental settings of StoX: ####
    # Define the number of digits (12) and the number of significant digits (6, used if values are very low) used by the Rstox packages:
    digits <- 12
    signifDigits <- 12
    
    # Value of numeric NA in processData stored in the project.json:
    #jsonNA <- -999999
    
    typeToNetCDF4Prec <- data.table::data.table(
        type = c("numeric", "double", "integer", "character", "POSIXc"), 
        prec = c("double", "double", "integer", "char", "char")
    )
    
    # Define the permitted classes for individual outputs from StoX functions:
    validOutputDataClasses <- c(
        "data.table", 
        "matrix", 
        "character", 
        "numeric", 
        "integer", 
        "logical", 
        "sf", 
        #"StoX_multipolygon_WKT", 
        #"StoX_shapefile"
        "ggplot", 
        #"BootstrapData", 
        "StoXNetCDF4File"
    )
    
    outputTypes <- list(
        data.table = "table", 
        matrix = "table", 
        character = "table", 
        numeric = "table", 
        integer = "table", 
        logical = "table", 
        sf = "geojson", 
        ggplot = "plot"
    )
    
    vectorClasses <- c(
        "character", 
        "numeric", 
        "integer", 
        "logical"
    )
    
    
    # Define code words for the start and end of files to write geojson data to, which are read into the project.json after being written for a project:
    spatialFileReferenceCodeStart <- "<stratumpolygontempfile:"
    spatialFileReferenceCodeEnd <- ":stratumpolygontempfile>"
    
    # Define the regular expression listing lower and upper characters, integers, underscore and dot:
    validProcessNameSet <- "[[:alnum:]_.]"
    # The prefix for new unnamed processes:
    process_Prefix <- "Process_"
    # The number of digits in the integer part of the project IDs:
    numDigitsOfProcessIntegerID <- 3
    
    
    # Define the process property types:
    processPropertyTypes <- list(
        default = "character", 
        optional = list(
            "logical", 
            "integer", 
            "double", 
            "numeric"
        )
    )
    
    # Get the processPropertyFormats of all packages, and merge the lists and add the default ("none"):
    processPropertyFormats <- unlist(
        c(
            lapply(officialStoxLibraryPackagesAll, getProcessPropertyFormats), 
            # Add the default format "none"
            list(defaultProcessPropertyFormat)
        ), 
        recursive = FALSE
    )
    
    #
    #allFormatClasses <- unique(unlist(lapply(processPropertyFormats, names)))
    #processPropertyFormats <- lapply(allFormatClasses, function(x) unlist(lapply(processPropertyFormats, "[[", x)))
    #names(processPropertyFormats) <- allFormatClasses
    
    # Get the parameterTableInfo from all packages, and combine into a list:
    parameterTableInfo  <- processPropertyFormats[sapply(processPropertyFormats, "[[", "class") == "table"]
    parameterVectorInfo <- processPropertyFormats[sapply(processPropertyFormats, "[[", "class") == "vector"]
    
    
    # Define filter operators for the different data types:
    filterOperators <- list(
        character = c("%in%", "%notin%", "==", "!=", "%notequal%"), 
        logical   = c("==", "!=", "%notequal%"), # This may never be used
        integer   = c("%in%", "%notin%", "==", "!=", "%notequal%", "<", "<=", ">=", ">"),
        double    = c("%in%", "%notin%", "==", "!=", "%notequal%", "<", "<=", ">=", ">"),
        numeric   = c("%in%", "%notin%", "==", "!=", "%notequal%", "<", "<=", ">=", ">"),
        POSIXct   = c("%in%", "%notin%", "==", "!=", "%notequal%", "<", "<=", ">=", ">")
    )
    
    # Define the StoX folders, data sources, model names, model display names, model descriptions, and the latter three grouped as model info:
    stoxFolders <- c(
        input = "input", 
        output = "output", 
        process = "process"
    )
    stoxFoldersList <- as.list(stoxFolders)
    names(stoxFoldersList) <- names(stoxFolders)
    stoxDataSourceFolders <- c(
        acoustic = "acoustic", 
        biotic = "biotic", 
        landing = "landing"
    )
    stoxModelFolders <- c(
        baseline = "baseline", 
        analysis = "analysis", 
        report = "report"
    )
    stoxModelNames <- c(
        baseline = "baseline", 
        analysis = "analysis", 
        report = "report"
    )
    stoxModelDisplayNames <- c(
        baseline = "Baseline", 
        analysis = "Analysis", 
        report = "Report"
    )
    stoxModelHierarchy <- c(
        baseline = "baseline", 
        analysis = "analysis", 
        report = "report"
    )
    stoxModelDescriptions <- c(
        baseline = "Baseline: The estimation model", 
        analysis = "Analysis: Processes that run Baseline for analysis, such as estimation of variation", 
        report = "Report: Processes that run Baseline or Analysis processes to generate reports"
    )
    stoxModelInfo <- data.table::data.table(
        modelName = stoxModelNames, 
        displayName = stoxModelDisplayNames, 
        description = stoxModelDescriptions
    )

    # Backwards compatibility:
    # Get the mapping between models iin 
    modelNameMapping2.7To3 <- structure(c("baseline", "analysis", "report", "report"), names = c("baseline", "r", "baseline-report", "r-report"))
    
    
    # Define the folder structure of StoX:
    stoxFolderStructure <- list(
        stoxDataSourceFolders, 
        stoxModelFolders, 
        c(process = "")
    )
    stoxFolderStructureNames <- unlist(lapply(stoxFolderStructure, names))
    stoxFolderStructure <- unname(unlist(mapply(file.path, stoxFolders, stoxFolderStructure)))
    stoxFolderStructure <- gsub('\\/$', '', stoxFolderStructure)
    names(stoxFolderStructure) <- stoxFolderStructureNames
    stoxFolderStructureList <- as.list(stoxFolderStructure)
    
    stoxFolderStructureList$outputFolders <- stoxFolderStructure[stoxModelFolders]
    
    
    # Define data types which can be plotted in the map (includes also changing colour etc, such as assigned stations of an acoustic PSU):
    dataTypesToShowInMap <- c(
        StoxBioticData = "StoxBioticData", 
        StoxAcousticData = "StoxAcousticData", 
        StratumPolygon = "StratumPolygon"
    )
    
    # Define the data types for the interactive modes:
    stratumDataType <- "StratumPolygon"
    acousticPSUDataType <- "AcousticPSU"
    bioticPSUDataType <- "BioticPSU"
    acousticLayerDataType <- "AcousticLayer"
    bioticLayerDataType <- "BioticLayer"
    bioticAssignmentDataType <- "BioticAssignment"
    stationDataType <- "StoxBioticData"
    EDSUDataType <- "StoxAcousticData"
    
    # Define the process parameters with default values, display names and descriptions:
    processParameters <- list(
        enabled = TRUE, 
        showInMap = FALSE, 
        fileOutput = TRUE
    )
    processParametersDisplayNames <- list(
        enabled = "Enabled", 
        showInMap = "Show in map", 
        fileOutput = "Write output to file"
    )
    processParametersDescriptions <- list(
        enabled = "Whether to execute the process or not", 
        showInMap = "Whether to show specific data from the process in the map, such as stations, EDSUs, strata, or assignment shown as colors on stations and EDSUs", 
        fileOutput = "Whether to write tab separated text files of the output data from the process"
    )
    
    # Define process property names:
    processPropertyNames <- data.table::data.table(
        name = c("process", "functionInputs", "functionParameters"), 
        displayName = c("Process", "Function inputs", "Function parameters")
    )
    
    # Define the process arguments, which define a process:
    processDefaultFull <- list(
        processName = NULL, 
        functionName = "", 
        processParameters = processParameters,
        processData = list(), 
        functionParameters = list(), 
        functionInputs = list()
    )
    processDefaultSansProcessData <- processDefaultFull[names(processDefaultFull) != "processData"]
    processDefault <- list(
        baseline = processDefaultFull, 
        analysis = processDefaultSansProcessData, 
        report = processDefaultSansProcessData
    )
    
    
    #### Define the folders and paths used when a project is open: ####
    projectSessionFolder <- file.path(stoxFolders["process"], "projectSession")
    
    # Sub folders 1:
    dataFolder <- file.path(projectSessionFolder, "data")
    memoryFolder <- file.path(projectSessionFolder, "memory")
    statusFolder <- file.path(projectSessionFolder, "status")
    
    #bootstrapProgressFile <- file.path(statusFolder, "bootstrapProgress.txt")
    #NumberOfBootstrapsFile <- file.path(statusFolder, "NumberOfBootstraps.txt")
    #stopBootstrapFile <- file.path(statusFolder, "stopBootstrap.txt")
    progressFile <- structure(mapply(file.path, statusFolder, paste0(stoxModelNames, "Progress.txt"), SIMPLIFY = FALSE), names = stoxModelNames)
    NFile <- structure(mapply(file.path, statusFolder, paste0(stoxModelNames, "N.txt"), SIMPLIFY = FALSE), names = stoxModelNames)
    stopFile <- structure(mapply(file.path, statusFolder, paste0(stoxModelNames, "Stop.txt"), SIMPLIFY = FALSE), names = stoxModelNames)
    
    # Sub folders of the data folder:
    dataModelsFolder <- file.path(dataFolder, "models")
    dataModelsFolders <- file.path(dataModelsFolder, stoxModelFolders)
    names(dataModelsFolders) <- stoxModelFolders
    
    # Sub folders of the memory folder:
    memoryCurrentFolder <- file.path(memoryFolder, "current")
    memoryHistoryFolder <- file.path(memoryFolder, "history")
    memoryModelsFolder <- file.path(memoryFolder, "models")
    memoryModelsFolders <- file.path(memoryModelsFolder, stoxModelFolders)
    names(memoryModelsFolders) <- stoxModelFolders
    
    memoryCurrentModelsFolder <- file.path(memoryCurrentFolder, "models")
    memoryCurrentModelsFolders <- file.path(memoryCurrentModelsFolder, stoxModelFolders)
    
    # Return also a vector of all session folders, to generate the folder structure recursively:
    projectSessionFolderStructure <- c(
        dataFolder, 
        memoryFolder, 
        statusFolder, 
        dataModelsFolder, 
        dataModelsFolders, 
        memoryCurrentFolder, 
        memoryHistoryFolder, 
        memoryModelsFolder, 
        memoryModelsFolders, 
        memoryCurrentModelsFolder, 
        memoryCurrentModelsFolders
    )
    
    
    #### Project description: ####
    projectXMLFile <- file.path(stoxFolders["process"], "project.xml")
    projectJSONFile <- file.path(stoxFolders["process"], "project.json")
    projectSavedStatusFile <- file.path(statusFolder, "projectSavedStatus.txt")
    #projectIsRunningFile <- file.path(statusFolder, "projectIsRunning.txt")
    #modelIsRunningFile <- list(
    #    baseline = file.path(statusFolder, "baselineIsRunning.txt"), 
    #    analysis = file.path(statusFolder, "analysisIsRunning.txt"), 
    #    report = file.path(statusFolder, "reportIsRunning.txt")
    #)
    modelIsRunningFile <- structure(mapply(file.path, statusFolder, paste0(stoxModelNames, "IsRunning.txt"), SIMPLIFY = FALSE), names = stoxModelNames)
    
    # Memory files:
    projectMemoryIndexFile <- file.path(memoryHistoryFolder, "projectMemoryIndex.txt")
    # The file containing a table of modelName, processID and processName, where the rows are ordered by the processIndex:
    processIndexTableFile <- file.path(memoryCurrentFolder, "processIndexTable.txt")
    # The file containing a table of one row holding the index of the active process for each model (columns named by the model names):
    activeProcessIDFile <- file.path(memoryCurrentFolder, "activeProcessID.txt")
    # The file containing a table of one row holding the maximum process ID (sequential integer starting from 1 at the firstly generated process) for each model (columns named by the model names):
    maxProcessIntegerIDFile <- file.path(memoryCurrentFolder, "maxProcessIntegerID.txt")
    
    
    # The file containing the project description attributes:
    projectDescriptionAttributesFile <- file.path(memoryModelsFolder, "projectDescriptionAttributes.rds")
    

    #### Define an object with all path objects for convenience in getProjectPaths(): ####
    paths <- c(
        stoxFolderStructureList, 
        stoxFoldersList, 
        list(
            # Folders:
            stoxFolders = stoxFolders, 
            stoxFolderStructure = stoxFolderStructure, 
            
            # Project session:
            projectSessionFolder = projectSessionFolder, 
            
            dataFolder = dataFolder, 
            memoryFolder = memoryFolder, 
            statusFolder = statusFolder, 
            #bootstrapProgressFile = bootstrapProgressFile,
            #NumberOfBootstrapsFile = NumberOfBootstrapsFile,
            #stopBootstrapFile = stopBootstrapFile, 
            progressFile = progressFile, 
            NFile = NFile, 
            stopFile = stopFile, 
            dataModelsFolder = dataModelsFolder, 
            dataModelsFolders = dataModelsFolders, 
            memoryCurrentFolder = memoryCurrentFolder, 
            memoryHistoryFolder = memoryHistoryFolder, 
            memoryModelsFolder = memoryModelsFolder, 
            memoryModelsFolders = memoryModelsFolders, 
            memoryCurrentModelsFolder = memoryCurrentModelsFolder, 
            memoryCurrentModelsFolders = memoryCurrentModelsFolders, 
            
            projectSessionFolderStructure = projectSessionFolderStructure, 
            
            # Project description:
            projectXMLFile = projectXMLFile, 
            projectJSONFile = projectJSONFile, 
            projectSavedStatusFile = projectSavedStatusFile, 
            #projectIsRunningFile = projectIsRunningFile, 
            modelIsRunningFile = modelIsRunningFile, 
            
            projectMemoryIndexFile = projectMemoryIndexFile, 
            processIndexTableFile = processIndexTableFile, 
            activeProcessIDFile = activeProcessIDFile, 
            maxProcessIntegerIDFile = maxProcessIntegerIDFile, 
            
            projectDescriptionAttributesFile = projectDescriptionAttributesFile
        )
    )
    
    
    #### Assign to RstoxEnv and return the definitions: ####
    definitionsNames <- ls()
    definitionsNames <- setdiff(definitionsNames, names(paths))
    definitions <- lapply(definitionsNames, get, pos = environment())
    names(definitions) <- definitionsNames
    
    # Add the stoxTemplates: 
    definitions$stoxTemplates <- stoxTemplates
    
    # The globalVariables were moved to pkgnameFile written by RstoxBuild:
    #### Create the RstoxFrameworkEnv environment, holding definitions on folder structure and all the projects. This environment cna be accesses using RstoxFramework:::RstoxFrameworkEnv: ####
    #utils::globalVariables("RstoxFrameworkEnv")
    #utils::globalVariables(c(
    #    "RstoxFrameworkEnv", 
    #    ":=", ".", 
    #    "..PSU", 
    #    "..activeProcessID", 
    #    "..clickPointNames", 
    #    "..coordinateNames", 
    #    "..functionInputs", 
    #    "..functionName", 
    #    "..functionParameters", 
    #    "..infoToKeep", 
    #    "..processDirty", 
    #    "..newProcessName", 
    #    "CruiseKey", 
    #    "Latitude", 
    #    "Latitude2", 
    #    "LogOrigin", 
    #    "LogOrigin2", 
    #    "Longitude", 
    #    "Longitude2", 
    #    "PSU", 
    #    "atRemove", 
    #    "canShowInMap", 
    #    "filePahts", 
    #    "functionName", 
    #    "functionOutputDataType", 
    #    "hasBeenRun", 
    #    "hasProcessData", 
    #    "modelName", 
    #    "processDirty", 
    #    "name", 
    #    "possibleValues", 
    #    "processID", 
    #    "projectPath", 
    #    "value"
    #))
    
    
    assign("RstoxFrameworkEnv", new.env(), parent.env(environment()))
    
    assign("definitions", definitions, envir=get("RstoxFrameworkEnv"))
    assign("projects", list(), envir=get("RstoxFrameworkEnv"))
    
    #### Return the definitions: ####
    definitions
}


##################################################
##################################################
#' Re-define definitions stored in the RstoxFramework environment
#' 
#' This function is useful e.g. to test a new package as an official package:
#' 
#' @return
#' A list of definitions.
#' 
#' @noRd
#' @seealso Use \code{\link{getRstoxFrameworkDefinitions}} to get the definitions.
#' 
reinitiateRstoxFramework <- function(){
    
}


orderBackwardCompatibility <- function(x) {
    lapply(x, orderBackwardCompatibilityOne)
}

orderBackwardCompatibilityOne <- function(x) {
    changeVersion <- sapply(x, "[[", "changeVersion")
    changeVersionOrder <- order(semver::parse_version(changeVersion))
    x[changeVersionOrder]
}


# This function gets the stoxFunctionAttributes of the specified packages.
getStoxLibrary <- function(packageNames, requestedFunctionAttributeNames) {
    
    # Validate the pakcages:
    packageNames <- packageNames[sapply(packageNames, validateStoxLibraryPackage)]
    # Get a list of the 'stoxFunctionAttributes' from each package:
    stoxFunctionAttributeLists <- lapply(packageNames, getStoxFunctionAttributes, requestedFunctionAttributeNames = requestedFunctionAttributeNames)
    
    # Collapse to one list:
    stoxFunctionAttributes <- unlist(stoxFunctionAttributeLists, recursive = FALSE)
    # Check for duplicaetd function names:
    
    functionNames <- names(stoxFunctionAttributes)
    packageNames <- sapply(stoxFunctionAttributes, "[[", "packageName")
    areDuplicatedFunctionNames <- duplicated(functionNames)
    
    # If there are any duplicated function names, report a warning stating which function names and from which packages:
    if(any(areDuplicatedFunctionNames)) {
        # Get the package strings as concatenations of the packages with common function names:
        packageNamesString <- as.character(
            by(
                functionNames[areDuplicatedFunctionNames], 
                packageNames[areDuplicatedFunctionNames], 
                paste, 
                collapse = ", "
            )
        )
        # Get the unique duplicated function names, and paste the packageNamesString to these:
        uniqueDuplicatedFunctionNames <- unique(functionNames[areDuplicatedFunctionNames])
        functionNamePackageNamesString <- paste0(
            uniqueDuplicatedFunctionNames, 
            "(", 
            packageNamesString, 
            ")"
        )
        
        warning("StoX: The following functions are present in several packages (package names in parenthesis): ", paste(functionNamePackageNamesString, collapse = ", "))
    }
    
    # Keep only the non-duplicated functions: 
    stoxFunctionAttributes <- stoxFunctionAttributes[!areDuplicatedFunctionNames]
    return(stoxFunctionAttributes)
}


# Define the default process property format:
defaultProcessPropertyFormat <- list(
    none = list(
        title = "Default format", 
        type = "single"
    )
)


# Function getting formats of a package:
getProcessPropertyFormats <- function(packageName) {
    # Get the exported object 'stoxFunctionAttributes' from the package:
    if(!identical(packageName, "RstoxFramework")) {
        processPropertyFormats <- tryCatch(
            getExportedValue(packageName, "processPropertyFormats"), 
            error = function(err) NULL
        )
    }
    
    # Add the package name:
    for( ind in seq_along(processPropertyFormats)) {
        processPropertyFormats[[ind]]$packageName <- packageName
    }
    
    return(processPropertyFormats)
}



readProcessDataSchema <- function(packageName) {
    # Get the file to the schema:
    schemaFile <- system.file("formats", "processDataSchema.json", package = packageName)
    if(nchar(schemaFile) > 0) {
        schema <- jsonlite::read_json(schemaFile)
    }
    else {
        schema <- NULL
    }
    
    return(schema)
}


extractStoxFunctionParameterPossibleValues <- function(functionName, systemParameters, dropSystemParameters = TRUE) {
    
    # Split the function name into function name and package name, and get the formals in the package environment:
    packageFunctionName <- strsplit(functionName, "::")[[1]]
    
    # Discard RstoxFramework as a packageName, as we have not yet loaded this package:
    if(packageFunctionName[1] == "RstoxFramework") {
        functionName <- packageFunctionName[2]
        useOnlyFunctionName = TRUE
    }
    else if(length(packageFunctionName) == 1) {
        useOnlyFunctionName = TRUE
    }
    else {
        useOnlyFunctionName = FALSE
    }
    
    # Use the package enironment if needed:
    if(useOnlyFunctionName) {
        f <- formals(functionName)
    }
    else {
        packageName <- packageFunctionName[1]
        functionName <- packageFunctionName[2]
        f <- formals(functionName, envir = as.environment(paste("package", packageName, sep = ":")))
    }
    
    # Convert missing inputs to NULL, to preserve the name-value-pair convention, and to allow evaluating the calls returned by formals():
    areMissing <- sapply(f, class) == "name" & sapply(f, function(x) length(x) > 0 & sum(nchar(x)) == 0)
    f[areMissing] <- vector("list", sum(areMissing))
    
    if(dropSystemParameters) {
        parameterNamesToKeep <- setdiff(names(f), systemParameters)
        f <- f[parameterNamesToKeep]
    }
    
    # Evaluate and return:
    output <- f
    
    # Use the package enironment if needed, and evaluate the formals:
    if(useOnlyFunctionName) {
        for(i in seq_along(f)) {
            assign(names(f[i]), if(!is.null(f[[i]])) output[[i]] <- eval(f[[i]]) else eval(f[[i]]))
        }
    }
    else {
        packageName <- packageFunctionName[1]
        for(i in seq_along(f)) {
            assign(names(f[i]), 
                if(!is.null(f[[i]])) 
                    output[[i]] <- eval(f[[i]], envir = 
                        list(
                            environment(), 
                            as.environment(paste("package", packageName, sep = ":"))
                        )
                    ) 
                else 
                    eval(f[[i]], envir = list(
                        environment(), 
                        as.environment(paste("package", packageName, sep = ":"))
                    )
                )
            )
        }
    }
    
    return(output)
}



##################################################
##################################################
#' Get RstoxFramework definitions
#' 
#' This function gets vital definitions from the RstoxFramework environment.
#' 
#' @param name  An optional string vector denoting which definitions to extract.
#' @param ...   values overriding the values of definitions.
#' 
#' @return
#' A list of definitions.
#' 
#' @examples
#' getRstoxFrameworkDefinitions("officialStoxLibraryPackages")
#' 
#' @export
#' 
getRstoxFrameworkDefinitions <- function(name = NULL, ...) {
    
    # Save the optional inputs for overriding the output:
    l <- list(...)
    
    # Get all or a subset of the definitions:
    definitions <- get("RstoxFrameworkEnv")$definitions
    if(length(name)){
        definitions <- definitions[[name]]
    }
    
    l <- l[names(l) %in% names(definitions)]
    if(length(l)){
        definitions <- utils::modifyList(definitions, l)
    }
    
    definitions
}


# Function for reading the backwardCompatibility object of a package.
getBackwardCompatibility <- function(packageName) {
    output <- tryCatch(
        {
            getExportedValue(packageName, "backwardCompatibility")
        }, 
        error = function(err) NULL
    )
    if(!length(output)) {
        output <- tryCatch(
            {
                getExportedValue(packageName, paste("backwardCompatibility", packageName, sep = "_"))
            }, 
            error = function(err) NULL
        )
    }
    
    return(output)
}



getDefaultOutputFileType <- function(processOutput) {
    if(length(processOutput)) {
        # Support for class specified in the output of function:
        classes <- unique(c(class(processOutput), class(processOutput[[1]])))
        
        if("StoXNetCDF4File" %in% classes) {
            ext <- "nc"
        }
        #else if("BootstrapData" %in% classes) {
        #    ext <- "RData"
        #}
        
        # List of outputs:
        else if("sf" %in% classes) {
            # Set file extension:
            ext <- "geojson"
        }
        else if("data.table" %in% classes) {
            # Set file extension:
            ext <- "txt"
        }
        else if("matrix" %in% classes || any(getRstoxFrameworkDefinitions("vectorClasses") %in% classes)) {
            # Set file extension:
            ext <- "csv"
        }
        else if("ggplot" %in% classes) {
            # Set file extension:
            ext <- RstoxBase::getRstoxBaseDefinitions("defaultPlotOptions")$defaultPlotFileOptions$Format # "png" 
            # This is the default, and is changed to the value specified by the user in the process later in reportFunctionOutputOne().
        }
        # List of lists of outputs:
        else if("sf" %in% class(processOutput[[1]][[1]])) {
            # Set file extension:
            ext <- "geojson"
        }
        else if("data.table" %in% class(processOutput[[1]][[1]])) {
            # Set file extension:
            ext <- "txt"
        }
        else if("matrix" %in% class(processOutput[[1]][[1]]) || any(getRstoxFrameworkDefinitions("vectorClasses") %in% class(processOutput[[1]][[1]]))) {
            # Set file extension:
            ext <- "csv"
        }
        else if("ggplot" %in% class(processOutput[[1]][[1]])) {
            # Set file extension:
            ext <- RstoxBase::getRstoxBaseDefinitions("defaultPlotOptions")$defaultPlotFileOptions$Format # "png" 
            # This is the default, and is changed to the value specified by the user in the process later in reportFunctionOutputOne().
        }
        else {
            stop("Unknown process output: [[1]]: ", classes, ", [[1]][[1]]: ", class(processOutput[[1]][[1]]))
        }
    }
    
    return(ext) 
}


classRecursive <- function(x) {
    
}

