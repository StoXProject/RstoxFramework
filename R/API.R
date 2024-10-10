##################################################
##################################################
#' Run a model of a StoX project
#' 
#' This function runs and returns output from a model of a StoX project.
#' 
#' @inheritParams general_arguments
#' @inheritParams getModelData
#' @inheritParams runProcesses
#' @inheritParams Projects
#' @inheritParams readBootstrapData
#' @inheritParams readMemoryFile
#' @param run Logical: If TRUE run the model.
#' @param save Logical: If TRUE save the project after running.
#' @param force.restart Logical: If TRUE restart the model before running.
#' @param close Logical: If TRUE close the project after running and getting the output.
#' @param returnModelData Logical: If TRUE return the output of the model runs. Can also be given as a string vector holding the names of the models to return data from. If TRUE, the specific models to return data from (across models) can be given by \code{processes}.
#' @param returnBootstrapData Logical: If TRUE read the content of any NetCDF4 files (i.e. output from \code{\link{Bootstrap}}).
#' @param ... Arguments passed on to \code{\link{runProcesses}}.
#' 
#' @return
#' A list of model output.
#' 
#' @examples
#' \dontrun{
#' # Open and run the sandeel test project:
#' tmp <- tempdir()
#' sandeelZip <- system.file("testresources", "sandeel_20.zip", package = "RstoxFramework")
#' unzip(sandeelZip, exdir = tmp)
#' projectPath <- file.path(tmp, "sandeel_20")
#' d1 <- runModel(projectPath, modelName = "baseline")
#' 
#' # Define depth dependent acoustic target strength, and rerun:
#' process <- getProcess(
#'   projectPath = projectPath, 
#'   modelName = "baseline", 
#'   processName = "DefineAcousticTargetStrength"
#' )
#' newAcousticTargetStrengthTable <- process$functionParameters$AcousticTargetStrengthTable
#' # Add the depth dependence:
#' newAcousticTargetStrengthTable$DepthExponent = -2.3
#' 
#' replaceArgsList <- list(
#'   DefineAcousticTargetStrength = list(
#'     UseProcessData = FALSE, # Needed to override the existing process data
#'     AcousticTargetStrengthModel = "LengthAndDepthDependent", 
#'     AcousticTargetStrengthTable = newAcousticTargetStrengthTable
#'   )
#' )
#' d2 <- RstoxFramework::runModel(
#'   projectPath, 
#'   modelName = "baseline", 
#'   replaceArgsList = replaceArgsList
#' )
#' 
#' # Show the change in the output:
#' all.equal(d1, d2)
#' # The difference is larger at larger depths:
#' plot(
#'   d1$Abundance$Data$MinLayerDepth, 
#'   d1$Abundance$Data$Abundance  /  d2$Abundance$Data$Abundance
#' )
#' }
#' 
#' @export
#' 
runModel <- function(
    projectPath, modelName, 
    processes = NULL, startProcess = 1, endProcess = Inf, 
    drop.datatype = TRUE, unlistDepth2 = FALSE, 
    run = TRUE, save = TRUE, force.save = FALSE, force.restart = FALSE, 
    replaceDataList = list(), replaceArgsList = list(), prependProcessList = list(), 
    fileOutput = NULL, 
    setUseProcessDataToTRUE = TRUE, purge.processData = FALSE, 
    returnModelData = TRUE, returnBootstrapData = FALSE, selection = list(), BootstrapID = NA, unlistSingleTable = FALSE, 
    try = TRUE, 
    close = FALSE, 
    msg = TRUE, 
    ...
) {
    
    
    # Close after running if requested:
    if(close) {
        on.exit(closeProject(projectPath, save = save, msg = msg))
    }
    
    # Run the model if required:
    modelData <- NULL
    if(run) {
        if(isProject(projectPath)) {
            # Open the project if not open:
            temp <- openIfNotAlreadyOpenProject(projectPath)
            
            # Run the model:
            if(msg) {
                message(
                    "StoX: Running model ", modelName, " of project ", 
                    projectPath, 
                    "...", 
                    appendLF = TRUE
                )
            }
            runProcesses(
                projectPath = projectPath, 
                modelName = modelName, 
                startProcess = startProcess, 
                endProcess = endProcess, 
                save = save, force.save = force.save, 
                force.restart = force.restart, 
                replaceDataList = replaceDataList, 
                replaceArgsList = replaceArgsList, 
                prependProcessList = prependProcessList, 
                fileOutput = fileOutput, 
                setUseProcessDataToTRUE = setUseProcessDataToTRUE, 
                purge.processData = purge.processData, 
                try = try, 
                msg = msg, 
                ...
            )
            # Get the model data:
            if(returnModelData) {
                modelData <- getModelData(
                    projectPath = projectPath, 
                    modelName = modelName, 
                    processes = processes, 
                    startProcess = startProcess, 
                    endProcess = endProcess, 
                    drop.datatype = drop.datatype, 
                    warn = FALSE, 
                    unlistDepth2 = unlistDepth2, 
                    returnBootstrapData = returnBootstrapData, selection = selection, BootstrapID = BootstrapID, unlistSingleTable = unlistSingleTable
                )
            }
        }
        else{
            warning("The path ", projectPath, " does not point to a valid StoX project.")
        }
    }
    
    return(modelData)
}



##################################################
##################################################
#' Run all models of a StoX project
#' 
#' This function runs and returns output from all models of a StoX project.
#' 
#' @inheritParams general_arguments
#' @inheritParams getModelData
#' @inheritParams runProcesses
#' @inheritParams Projects
#' @inheritParams runModel
#' @inheritParams readBootstrapData
#' @inheritParams readMemoryFile
#' @param startProcess The process index, name or ID at which to start the model run. A list can be given named by the models if one needs to specify start process for each model. Models given inn \code{modelNames} but not in the list will be run from the start of the model.
#' @param endProcess The process index, name or ID at which to stop the model run. A list can be given named by the models if one needs to specify end process for each model. Models given inn \code{modelNames} but not in the list will be run to the end of the model.
#' @param unlist.models Logical: If TRUE unlist the top level so that all processes are in one list.
#' 
#' @return
#' A list of model output.
#' 
#' @export
#' 
runProject <- function(
    projectPath, 
    modelNames = getRstoxFrameworkDefinitions("stoxModelNames"), 
    processes = NULL, startProcess = 1, endProcess = Inf, 
    drop.datatype  = TRUE, unlistDepth2 = FALSE, 
    run = TRUE, save = TRUE, force.save = FALSE, force.restart = FALSE, 
    replaceDataList = list(), replaceArgsList = list(), prependProcessList = list(), 
    fileOutput = NULL, 
    setUseProcessDataToTRUE = TRUE, purge.processData = FALSE, 
    returnModelData = TRUE, returnBootstrapData = FALSE, selection = list(), BootstrapID = NA, unlistSingleTable = FALSE, 
    try = TRUE, 
    close = FALSE, 
    unlist.models = TRUE, 
    msg = TRUE, 
    ...
) {
    
    if(isTRUE(save)) {
        warning("The default save = TRUE will be changed to save = FALSE in RstoxFramework 4.1.0.")
    }
    
    if(msg) {
        startTime <- proc.time()[3]
    }
    
    # Close after running if requested:
    if(close) {
        on.exit(closeProject(projectPath, save = save, msg = msg))
    }
    if(isProject(projectPath)) {
        # Open the project if not open:
        temp <- openIfNotAlreadyOpenProject(projectPath)
    }
    
    if(msg) {
        message(
            "StoX: Running project ", 
            projectPath, 
            "...", 
            appendLF = TRUE
        )
    }
    
    # Return model data from all models by default (is this risky?):
    if(isTRUE(returnModelData)) {
        returnModelData <- rep(TRUE, length(modelNames))
    }
    else if(is.character(returnModelData)) {
        returnModelData <- modelNames %in% returnModelData
    }
    
    # Recognize the start and end process and create a list linking to the model:
    if(!is.list(startProcess)) {
        processTable <- getProcessesSansProcessData(projectPath)
        atProcess <- matchProcesses(startProcess, processTable)
        startProcess <- structure(list(processTable[atProcess, processID]), names = processTable[atProcess, modelName])
    }
    if(!is.list(endProcess)) {
        processTable <- getProcessesSansProcessData(projectPath)
        atProcess <- matchProcesses(endProcess, processTable)
        endProcess <- structure(list(processTable[atProcess, processID]), names = processTable[atProcess, modelName])
    }
    
    # Support for model specific startProcess and endProcess:
    if(is.list(startProcess)) {
        startProcess <- startProcess[modelNames]
        # Add the default for the models that are not specified in startProcess:
        if(any(lengths(startProcess) == 0)) {
            startProcess[lengths(startProcess) == 0] <- formals()$startProcess
        }
    }
    if(is.list(endProcess)) {
        endProcess <- endProcess[modelNames]
        # Add the default for the models that are not specified in endProcess:
        if(any(lengths(endProcess) == 0)) {
            endProcess[lengths(endProcess) == 0] <- formals()$endProcess
        }
    }
    
    # Run the models and get the output:
    if(msg) {
        message(
            "StoX: Running project ", 
            projectPath, 
            "...", 
            appendLF = TRUE
        )
    }
    projectData <- mapply(
        runModel, 
        modelName = modelNames, 
        returnModelData = returnModelData, 
        startProcess = startProcess, 
        endProcess = endProcess, 
        MoreArgs = list(
            projectPath, 
            drop.datatype = drop.datatype, unlistDepth2 = unlistDepth2, 
            run = run, 
            save = save, force.save = force.save, 
            force.restart = force.restart, 
            replaceDataList = replaceDataList, 
            replaceArgsList = replaceArgsList, 
            prependProcessList = prependProcessList, 
            fileOutput = fileOutput, 
            processes = processes, 
            setUseProcessDataToTRUE = setUseProcessDataToTRUE, 
            purge.processData = purge.processData, 
            returnBootstrapData = returnBootstrapData, selection = selection, BootstrapID = BootstrapID, unlistSingleTable = unlistSingleTable,
            try = try, 
            close = FALSE, 
            msg = msg, 
            ...
        ), 
        SIMPLIFY = FALSE
    )
    ## Close after running if requested:
    #if(close) {
    #    closeProject(projectPath)
    #}
    
    # Drop the list over models:
    if(unlist.models) {
        projectData <- unlist(unname(projectData), recursive = FALSE)
    }
    
    if(msg) {
        timeSpent <- proc.time()[3] - startTime
        message(
            "(time used(project) : ", round(timeSpent, digits = 3), " s)"
        )
    }
    
    
    return(projectData)
}


##################################################
##################################################
#' Run all models of a StoX project
#' 
#' This function runs and returns output from all models of a StoX project.
#' 
#' @inheritParams general_arguments
#' @inheritParams getModelData
#' @inheritParams runProcesses
#' @inheritParams Projects
#' @inheritParams runModel
#' @inheritParams runProject
#' @inheritParams readBootstrapData
#' @inheritParams readMemoryFile
#' @param startProcess The process index, name or ID at which to start the model run. A list can be given named by the models if one needs to specify start process for each model. Models given inn \code{modelNames} but not in the list will be run from the start of the model.
#' @param endProcess The process index, name or ID at which to stop the model run. A list can be given named by the models if one needs to specify end process for each model. Models given inn \code{modelNames} but not in the list will be run to the end of the model.
#' 
#' @return
#' A list of model output.
#' 
#' @export
#' 
runProjects <- function(
    projectPaths, 
    modelNames = getRstoxFrameworkDefinitions("stoxModelNames"), 
    processes = NULL, startProcess = 1, endProcess = Inf, 
    drop.datatype = TRUE, unlistDepth2 = FALSE, 
    run = TRUE, save = TRUE, force.save = FALSE, force.restart = FALSE, 
    replaceDataList = list(), replaceArgsList = list(), prependProcessList = list(), 
    fileOutput = NULL, 
    setUseProcessDataToTRUE = TRUE, purge.processData = FALSE, 
    returnModelData = TRUE, returnBootstrapData = FALSE, selection = list(), BootstrapID = NA, unlistSingleTable = FALSE, 
    try = TRUE, 
    unlist.models = FALSE, 
    close = FALSE, 
    ...
) {
    
    # Run all projects:
    output <- sapply(
        projectPaths, 
        runProject, 
        modelNames = modelNames, 
        processes = processes, startProcess = startProcess, endProcess = endProcess, 
        drop.datatype = drop.datatype, unlistDepth2 = unlistDepth2, 
        run = run, save = save, force.save = force.save, force.restart = force.restart, 
        replaceDataList = replaceDataList, replaceArgsList = replaceArgsList, prependProcessList = prependProcessList, 
        fileOutput = fileOutput, 
        setUseProcessDataToTRUE = setUseProcessDataToTRUE, purge.processData = purge.processData, 
        returnModelData = returnModelData, 
        returnBootstrapData = returnBootstrapData, selection = selection, BootstrapID = BootstrapID, unlistSingleTable = unlistSingleTable, 
        try = try, 
        unlist.models = unlist.models, 
        close = close, 
        ..., 
        simplify = FALSE
    )
    
    output <- rbindListRecursive(output, names = basename(projectPaths))
    
    
    return(output)
}

rbindListRecursive <- function(x, names) {
    
    x <- unlist(unname(x), recursive = FALSE)
    
    # Scan through the list and identify data.table(s):
    namesList <- namesRecursive(x)
    
    # Loop through the tables and rbind:
    output <- list()
    for(namesVector in namesList) {
        # Declare the list element:
        output <- createNestedListElement(output, namesVector)
        
        #  Extract the output:
        extractedOutput <- extractFromAllProcessOutputs(namesVector, x)
        
        
        if(length(extractedOutput)) {
            names(extractedOutput) <- names
            # Rbind tables and matrices:
            if(data.table::is.data.table(extractedOutput[[1]])) {
                extractedOutput <- data.table::rbindlist(extractedOutput, fill = TRUE, idcol = "projectName")
            }
            else if(is.matrix(extractedOutput[[1]])) {
                extractedOutput <- mapply(cbind, projectName =  names(extractedOutput), extractedOutput)
                extractedOutput <- do.call(rbind, extractedOutput)
            }
            
            # Insert into the output:
            output[[namesVector]] <- extractedOutput
        }
    }
        
    return(output)
}

extractFromAllProcessOutputs <- function(nameVector, x) {
    
    if(length(nameVector) == 1) {
        output <- x[names(x) == nameVector]
    }
    else if(length(nameVector) == 2) {
        output <- lapply(x[names(x) == nameVector[1]], "[[", nameVector[2])
    }
    else if(length(nameVector) == 3) {
        output <- lapply(x[names(x) == nameVector[1]], "[[", nameVector[2])
        output <- lapply(output, "[[", nameVector[3])
    }
    else {
        warning("Process outputs with three levels (BioticData and AcousticData) cannot be combined, as these are named by the input data files, which generally differ bewteen StoX projects. A list of the un-combined output is returned.")
        output <- x
    }
    
    # Remove the empty ones:
    output <- output[lengths(output) > 0]
    
    return(output)
}

pasteNamesRecursive <- function (L, sep = "/") {
    # Return the names if all elements are not lists, and recurse futher if any are lists:
    #areNotList <- sapply(L, inherits, c("data.table", "data.frame", "ggplot", "SpatialPolygonsDataFrame"))
    areNotList <- sapply(L, inherits, c("data.table", "data.frame", "ggplot", "sf"))
    areList <- !areNotList
    
    if(any(areList)) {
        return(
            c(
                names(L)[areNotList], 
                mapply(paste,  names(L)[areList], sapply(L[areList], pasteNamesRecursive), sep = sep)
            )
        )
    }
    
    else {
        return(names(L)[areNotList])
    }
}
# Function to get the names of a list recursively:
namesRecursive <- function (L, uniquify = TRUE) {
    # Get the names pasted by "/"
    namesRecursivePasted <- unlist(pasteNamesRecursive(L, sep = "/"))
    
    # Split by "/":
    namesRecursiveSplit <- strsplit(namesRecursivePasted, "/")
    
    # Uniquify:
    if(uniquify) {
        namesRecursiveSplit <- unique(namesRecursiveSplit)
    }
    
    return(namesRecursiveSplit)
}

createNestedListElement <- function(x, namesVector) {
    if(!length(namesVector)) {
        return(x)
    }
    else if(length(namesVector) == 1) {
        if(!length(x[[namesVector]]) && !is.list(x[[namesVector]])) {
            x[[namesVector]] <- list()
        }
    }
    else {
        for(ind in seq_along(namesVector)) {
            thisNamesVector <- namesVector[seq_len(ind)]
            if(!length(x[[thisNamesVector]]) && !is.list(x[[thisNamesVector]])) {
                x[[thisNamesVector]] <- list()
            }
        }
    }
    return(x)
}




##################################################
##################################################
#' Read the output files of a project
#' 
#' This function reads all or some of the output files of a project, indicated by model and process names.
#' 
#' @inheritParams general_arguments
#' @inheritParams runProject
#' @param verifyFiles Logical: If TRUE verify that the files are from processes that exist in the project.
#' @param unlist Either 1 to unlist the models, 2 to unlist the models and the process. TRUE is interpreted as 2.
#' @param emptyStringAsNA Logical: If TRUE, read empty strings as NA from the stored original tables, as RstoxFramework has started writing NAs as NAs and not as empty strings.
#' @param ... Arguments passed to \code{\link{readBootstrapData}}, e.g. \code{selection}, which must be set to NA to read the entire file.
#' 
#' @return
#' A list of model output.
#' 
#' @export
#'  
readModelData <- function(projectPath, modelName = NULL, processName = NULL, verifyFiles = FALSE, unlist = FALSE, emptyStringAsNA = FALSE, ...) {
    
    # List the files of the project:
    if(isProject(projectPath)) {
        outputFolders <- getProjectPaths(projectPath)$outputFolders
        names(outputFolders) <- basename(outputFolders)
        outputFiles <- lapply(outputFolders, listOutputfiles)
        
        
        matchName <- function(x, name = NULL) {
            if(length(name)) {
                presentName <- intersect(names(x), name)
                x[presentName]
            }
        }
        
        # Subset by the modelName:
        if(length(modelName)) {
            outputFiles <- matchName(outputFiles, modelName)
        }
        # Subset by the processName:
        if(length(processName)) {
            outputFiles <- lapply(outputFiles, matchName, processName)
        }
        
        
        
        
        if(verifyFiles) {
            # Do not validate since we are only reading the project description file to check the process names:
            projectDescription <- readProjectDescription(projectPath, applyBackwardCompatibility = FALSE, formatProcesses = FALSE, validateJSON = FALSE)$projectDescription
            
            processNames <- lapply(projectDescription, function(x) unname(sapply(x, "[[", "processName")))
            outputFolderNames <- lapply(outputFiles, names)
            
            invalidProcesses <- setdiff(
                unlistSep(outputFolderNames), 
                unlistSep(processNames)
            )
            
            if(length(invalidProcesses)) {
                warning("The following folders of the output are not present as processes in the project description file, and were discarded from the output:", paste(invalidProcesses, collapse = ", "))
                outputFiles <- lapply(outputFiles, function(x) x[!names(x) %in% basename(invalidProcesses)])
            }
        }
        
        # Read the files using the appropriate function:
        #output <- lapply(outputFiles, function(x) lapply(x, readStoxOutputFile))
        output <- rapply(
            outputFiles, readStoxOutputFiles, emptyStringAsNA = emptyStringAsNA, how = "replace", 
            ...  # Used in readBootstrapData()
        )

        # Drop the list over models:
        if(isTRUE(unlist)) {
            unlist <- 2
        }
        if(unlist > 1) {
            #output <- lapply(output, function(x) if(hasOnlyOneTabble(x)) unlist(unname(x), recursive = FALSE) else x)
            # Changed to equal the output from runProject():
            output <- lapply(output, function(x) lapply(x, function(y) if(hasOnlyOneTabble(y)) y[[1]] else y))
            # Still the processes with 2 levels are output in a flat list with names separated by underscore, as there is no secure way to determine from those file names that there actually are 2 levels:
        }
        if(unlist > 0) {
            output <- unlist(unname(output), recursive = FALSE)
        }
        # Did not work:
        #if(unlist) {
        #    output <- unlistToDataType(output)
        #}
        
        return(output)
    }
    else {
        stop("The projectPath ", projectPath, " is not a StoX project.")
    }
    
}


hasOnlyOneTabble <- function(x) {
    is.list(x) && !data.table::is.data.table(x) && length(x) == 1
}

readStoxOutputFiles <- function(paths, emptyStringAsNA = FALSE, ...) {
    output <- structure(
        lapply(
            paths, readStoxOutputFile, emptyStringAsNA = emptyStringAsNA, 
            ...  # Used in readBootstrapData()
        ), 
        names = basename(tools::file_path_sans_ext(paths))
    )
    
    # Unlist the top level of an RData file, as an RData file is a joint file of several outputs, and we do not want the extra BootstrapData level on top of this list:
    areRDataFiles <- tolower(tools::file_ext(paths)) == "rdata"
    # isTRUE is TRUE only for one TRUE:
    if(isTRUE(areRDataFiles)) {
        output <- output[[1]]
    }
    return(output)
}


readStoxOutputFile <- function(path, emptyStringAsNA = FALSE, ...) {
    # This function only reads one file:
    if(length(path) != 1) {
        stop("Exactly one file required")
    }
    
    # Get file extension:
    ext <- tools::file_ext(path)
    
    if(tolower(ext) == "rdata") {
        output <- readOutputRDataFile(path)
        
        # Apply subset given in ...:
        output <- subsetModelData(output, subsetList = list(...))
    }
    else if(tolower(ext) %in% c("json", "geojson")) {
        # Use DefineStratumPolygon from RstoxBase:
        output <- tryCatch(
            RstoxBase::DefineStratumPolygon(
                DefinitionMethod = "ResourceFile", 
                FileName = path, 
                StratumNameLabel = "StratumName"
            ), 
            error = function(e) {
                NULL
            }
        )
        if(!length(output)) {
            output <- tryCatch(
                RstoxBase::DefineStratumPolygon(
                    DefinitionMethod = "ResourceFile", 
                    FileName = path, 
                    StratumNameLabel = "polygonName"
                ), 
                error = function(e) {
                    NULL
                }
            )
        }
        if(!length(output)) {
            stop("The existing output file from the DefineStratumPolygon process is no longer compatible (tried both StratumNameLabel = \"StratumName\" and \"polygonName\")")
        }
    }
    else if(tolower(ext) %in% "txt") {
        # Use "" as NA string, but do not inclcude "NA" as NA string, as "" is used when writing the data:
        #output <- data.table::fread(path, na.strings = c("NA", ""), tz = "UTC", encoding = "UTF-8")
        #output <- data.table::fread(path, na.strings = "", tz = "UTC", encoding = "UTF-8")
        output <- freadKeepQuotedCharacterAsCharacter(path, na.strings = "NA", tz = "UTC", encoding = "UTF-8")
        
        # If here are any keys that are time (such as LogKey of the StoxAcoustic format), convert these to character with 3 digits:
        #areKeys <- endsWith(names(output), "Key")
        #areDateTime <- sapply(output, getRelevantClass) %in% "POSIXct"
        #toConvertToCharacter <- areKeys & areDateTime
        #if(any(toConvertToCharacter)) {
        #    for(col in names(output)[toConvertToCharacter]) {
        #        output[, (col) := format(get(col), format = "%Y-%m-%dT%H:%M:%OS3Z")]
        #    }
        #}
        
        
        # If here are any keys that are time (such as LogKey of the StoxAcoustic format), convert these to character with 3 digits. Note that we use the hack to add 1e-4 to avoid the POSIXc formatting bug in R:
        keys <- names(output)[endsWith(names(output), "Key")]
        formatPOSIXAsISO8601(output, cols = keys, add = 1e-4, format = "%Y-%m-%dT%H:%M:%OS3Z")
    }
    else if(tolower(ext) %in% "csv") {
        # Use "" as NA string, but do not inclcude "NA" as NA string, as "" is used when writing the data:
        output <- unname(as.matrix(utils::read.csv(path, encoding = "UTF-8", header = FALSE, na.strings = c(""))))
    }
    else if(tolower(ext) == "nc") {
        # Do not unlist here, as it is rather done in readModelData() (using unlist = 0 here):
        output <- readBootstrapData(
            path, 
            ...  # Used in readBootstrapData()
        )
    }
    #else if(tolower(ext) %in% "png") {
    #    output <- path
    #}
    else {
        warning("Unknown file extension for StoX output file: ", ext, ". Returning the file path: ", path, ".")
        output <- path
    }
    
    if(emptyStringAsNA  && data.table::is.data.table(output)) {
        characterColumns <- names(output)[sapply(output, getRelevantClass) == "character"]
        if(length(characterColumns)) {
            output[, (characterColumns) := lapply(.SD, function(x) replace(x, nchar(x) == 0, NA_character_)), .SDcols = characterColumns]
        }
    }
    
    return(output)
}


subsetModelData <- function(modelData, subsetList = list()) {
    # Support for using NA to get all data:
    areNA <- sapply(subsetList, function(x) length(x) == 1 && is.na(x))
    subsetList <- subsetList[!areNA] 
    
    if(data.table::is.data.table(modelData)) {
        intersectingNames <- intersect(names(subsetList), names(modelData))
        if(length(intersectingNames)) {
            for(name in intersectingNames) {
                modelData <- subset(modelData, modelData[[name]] %in% subsetList[[name]])
            }
        }
        return(modelData)
    }
    else if(is.list(modelData)){
        modelData <- lapply(modelData, subsetModelData, subsetList = subsetList)
        
        return(modelData)
    }
    else {
        stop("The modelData must be a (nested) list of valid StoX output claasses.")
    }
}


#POSIXctToCharacter <- function(x, digits = 3) {
#    browser()
#    formatString <- paste0("%Y-%m-%dT%H:%M:%OS", digits, "Z")
#    print(formatString)
#    format(x, format = formatString)
#}

readOutputRDataFile <- function(outputDataPath) {
    if(file.exists(outputDataPath)) {
        outputData <- tryCatch(
            get(load(outputDataPath)), 
            error = function(err) NULL
        )
    }
    else {
        outputData <- NULL
    }
    
    return(outputData)
}



listOutputfiles <- function(modelPath) {
    dirs <- list.dirs(modelPath, recursive = FALSE)
    structure(lapply(dirs, list.files, full.names = TRUE), names = basename(dirs))
}


unlistSep <- function(x, sep = "/") {
    paste(rep(names(x), lengths(x)), unlist(x), sep = sep)
}



#' General functions to run a function of an Rstox package, modeled by do.call().
#' 
#' \code{runFunction} runs a function using the \code{\link[base]{do.call}} syntax, whereas \code{runFunction.JSON} accepts a JSON string conntaining the parameters to pass on to \code{runFunction}.
#' 
#' @inheritParams base::do.call
#' @param package The name of the package holding the function given by \code{what}.
#' @param removeCall Logical: If FALSE, keep the call in the error message.
#' @param onlyStoxMessages Logical: If TRUE show only the StoX messages, which are those starting with "StoX: ".
#' @param onlyStoxWarnings Logical: If TRUE show only the StoX warnings, which are those starting with "StoX: ".
#' @param onlyStoxErrors Logical: If TRUE show only the StoX errors, which are those starting with "StoX: ".
#' @param maxLength.Message,maxLength.Warning,maxLength.Error The maximum number of characters to return for messages, warnings and errors, respectively (with allowed values 100...8170, default 1000).
#' @param cmd A JSON string containing parameters listed above.
#' @param nwarnings The number of warnings to save.
#' @param collapseMessages,collapseWarnings,collapseErrors Logical: If TRUE report only unique messages, warnings and errors, respectively, adding the number of occurrences in parenthesis.
#' @param messageLogFile,warningLogFile,errorLogFile If given, messages, warnings and errors are printed to these files.
#' @param messageAppend,warningAppend,errorAppend Logical: If TRUE, append to the log files (\code{messageLogFile}, \code{warningLogFile} or \code{errorLogFile}, respectively)
#' @param uniquify Logical: If TRUE (the default) return unique messages, warnings and errors, with the nnumber of occurrences in parenthesis.
#' @param header The string to start each line with.
#' 
#' @export
#' 
runFunction <- function(
    what, args, 
    package = "RstoxFramework", removeCall = TRUE, 
    onlyStoxMessages = TRUE, onlyStoxWarnings = TRUE, onlyStoxErrors = FALSE, 
    maxLength.Message = 2000, maxLength.Warning = 2000, maxLength.Error = 2000, 
    nwarnings = 10000, 
    collapseMessages = TRUE, collapseWarnings = TRUE, collapseErrors = TRUE, 
    messageLogFile = NULL, warningLogFile = NULL, errorLogFile = NULL, 
    messageAppend = FALSE, warningAppend = FALSE, errorAppend = FALSE, 
    uniquify = TRUE, header = "> ") {
    
    # Parse the args if given as a JSON string:
    args <- parseParameter(args)
    
    # Reset the warnings:
    #assign("last.warning", NULL, envir = baseenv())
    
    # Temporarily set the number of warnings to a large number to capture not only 50 warnings:
    old_nwarnings <- getOption("nwarnings")
    options(nwarnings = nwarnings)
    
    # Run the function 'what' and store the warnings and error along with the result:
    ### warn <- character(0)
    ### err <- NULL
    ### msg <- utils::capture.output({
    ###     value <- withCallingHandlers(
    ###         tryCatch(
    ###             #do.call(what, args), 
    ###             do.call(getExportedValue(package, what), args), 
    ###             error = function(e) {
    ###                 err <<- if(removeCall) conditionMessage(e) else e
    ###                 NULL
    ###             }
    ###         ), 
    ###         warning = function(w) {
    ###                 warn <<- append(warn, if(removeCall) conditionMessage(w) else w)
    ###             invokeRestart("muffleWarning")
    ###         }
    ###     )
    ### }, type = "message")
    
    # The followig is inspired from https://www.r-bloggers.com/2020/10/capture-message-warnings-and-errors-from-a-r-function/ written by Bangyou Zheng:
    logs <- list()
    add_log <- function(type, message) {
        new_l <- logs
        new_log <- list(timestamp = format(Sys.time(), tz = 'UTC', format = '%Y-%m-%d %H:%M:%S'),
                        type = type,
                        message =  message)
        new_l[[length(new_l) + 1]]  <- new_log
        logs <<- new_l
    }
    value <- withCallingHandlers(
        tryCatch(
            do.call(getExportedValue(package, what), args), 
            error = function(e) {
                add_log("error", conditionMessage(e))
                NULL
            }), 
        warning = function(w) {
            add_log("warning", conditionMessage(w))
            invokeRestart("muffleWarning")
        }, 
        message = function(m) {
            # Messages are with a line space at the end by default:
            add_log("message", gsub("\n$", "", conditionMessage(m)))
            invokeRestart("muffleMessage")
        }
    )
    
    type <- sapply(logs, "[[", "type")
    # Use unlist and lapply here as either of message, warning og error can be missing inn logs, resulting in list() from sapply, whereas we need NULL:
    msg <- unlist(lapply(logs[type == "message"], "[[", "message"))
    warn <- unlist(lapply(logs[type == "warning"], "[[", "message"))
    err <- unlist(lapply(logs[type == "error"], "[[", "message"))
    
    
    # Get only the StoX-messages, and add "> "
    if(length(msg)) {
        if(onlyStoxMessages) {
            msg <- msg[startsWith(msg, "StoX:")]
        }
        if(length(header)){
            msg <- trimws(sub("StoX: ", "", msg, fixed = TRUE))
            if(length(msg)) {
                msg <- paste0(header, msg)
            }
        }
    }
    
    # Get only the StoX-warnings, and add "> "
    if(length(warn)) {
        if(onlyStoxWarnings) {
            warn <- warn[startsWith(warn, "StoX:")]
        }
        if(length(header)){
            warn <- trimws(sub("StoX: ", "", warn, fixed = TRUE))
            if(length(warn)) {
                warn <- paste0(header, warn)
            }
        }
    }
    
    # Get only the StoX-errors, and add "> "
    if(length(err)) {
        if(onlyStoxErrors) {
            err <- err[startsWith(err, "StoX:")]
        }
        if(length(header)){
            err <- trimws(sub("StoX: ", "", err, fixed = TRUE))
            if(length(err)) {
                err <- paste0(header, err)
            }
        }
    }
    
    if(length(msg)) {
        msg <- sapply(msg, truncateString, maxLength.Message)
    }
    
    if(length(warn)) {
        warn <- sapply(warn, truncateString, maxLength.Warning)
    }
    
    if(length(err)) {
        err <- sapply(err, truncateString, maxLength.Error)
    }
    
    # Clean the warnings:
    #warn <- unname(unlist(warn[names(warn) == "message"]))
    
    # Reset to 50 warnings:
    options(nwarnings = old_nwarnings)
    
    # Write messages warnings and errors:
    if(length(messageLogFile)) {
        write(msg, messageLogFile, append = messageAppend)
    }
    if(length(warningLogFile)) {
        write(warn, warningLogFile, append = warningAppend)
    }
    if(length(errorLogFile) && file.exists(errorLogFile)) {
        write(err, errorLogFile, append = errorAppend)
    }
    
    if(uniquify) {
        msg <- uniqueText(msg, tag = "messages")
        warn <- uniqueText(warn, tag = "warnings")
        err <- uniqueText(err, tag = "errors")
    }
    
    # Return a list of warnings and error along with the result:
    list(
        #value = if(!is.list(value)) as.list(value) else value, 
        value = value, 
        message = msg,
        warning = warn,
        error = err
    )
}


uniqueText <- function(x, tag = c("messages", "warnings", "errors")) {
    tag <- match.arg(tag)
    if(length(x)) {
        u <- unique(x)
        t <- table(x)
        t <- t[match(u, names(t))]
        out <- ifelse(
            t > 1, 
            paste0(names(t), "\n(", t, " identical ", tag, ")"),  
            names(t)
        )
        
        return(out)
    }
    else {
        return(x)
    }
}





truncateString <- function(string, length) {
    if(nchar(string) > length) {
        string <- paste0(substr(string, 1, length), " [... truncated]")
    }
    return(string)
}


#' @export
#' @rdname runFunction
#' 
runFunction.JSON <- function(cmd){
    # Service command/response handler
    tryCatch({
        cmdj <- jsonlite::fromJSON(cmd)
        res <- runFunction(cmdj$what, cmdj$args, cmdj$package)
        res$message <- unname(as.list(res$message))
        res$warning <- unname(as.list(res$warning))
        res$error <- unname(as.list(res$error))
        r <- jsonlite::toJSON(res, pretty = TRUE, auto_unbox = TRUE, na = 'string')
        r
    }, warning = function(warning_condition) {
        'warning'
    }, error = function(error_condition) {
        'error'
    })
}


##################################################
##################################################
#' Export StoX JSON schema
#' 
#' @param con A connection to which to write the schema. Returned as JSON if missing.
#' 
#' @export
#' 
writeStoxJsonSchema <- function(con) {
    schema <- getRstoxFrameworkDefinitions("schema")
    if(missing(con)) {
        return(schema)
    }
    else {
        writeLines(as.character(schema), con)
    }
}





# Function to read a file with quoted strings:
freadKeepQuotedCharacterAsCharacter <- function(...) {
    # Get the inputs.
    lll <- list(...)
    
    # Read using quote = "":
    lll$quote <- ""
    x <- do.call(data.table::fread, lll)
    
    # Remove escaped double quote characters in header:
    names(x) <- stripLeadingAndTrailingQuote(names(x))
    
    # Remove escaped double quote characters in character columns:
    areCharacter <- sapply(x, class) == "character"
    if(any(areCharacter)) {
        characterCols <- names(areCharacter)[areCharacter]
        x[, (characterCols) := lapply(.SD, stripLeadingAndTrailingQuote), .SDcols = characterCols]
    }
    
    return(x)
}

stripLeadingAndTrailingQuote <- function(x) {
    x <- gsub('\\\\\"', '\"', x)
    x <- gsub('^\\"|\\"$', '', x)
    #x <-stringi::stri_unescape_unicode(x)
    return(x)
}





##################################################
##################################################
#' Add a process DefineAcousticPSU with DefinitionMethod "PreDefined"
#' 
#' This function adds a process named DefineAcousticPSU using the function DefineAcousticPSU with DefinitionMethod = "PreDefined". It is a requirement that there exists a procecss named DefineAcousticPSU in the project, which will be renamed and used as input to the new DefineAcousticPSU process.
#' 
#' @inheritParams general_arguments
#' @inheritParams getModelData
#' @inheritParams runProcesses
#' @param run Logical: If TRUE run the model.
#' @param save Logical: If TRUE save the project after running.
#' @param projectPath0 Optional: Path to the project to copy to the project given by \code{projectPath}.
#' @param AcousticPSUProcessName The name of the existing process using DefineAcousticPSU.
#' @param AcousticPSUProcessNameNew The new name of the existing process using DefineAcousticPSU.
#' @param ReadAcoustic.FileNames Optional: character vector with file paths to input acoustic files to replaec the existing acoustic files by in the procecss using the function \code{ReadAcoustic}.
#' 
#' @return
#' A list of model output.
#' 
#' @export
#' 
runProject_ReplaceAcousticFiles <- function(projectPath, ReadAcoustic.FileNames, modelNames = getRstoxFrameworkDefinitions("stoxModelNames"), projectPath0 = NULL, run = TRUE, AcousticPSUProcessName = "DefineAcousticPSU", AcousticPSUProcessNameNew = "DefineAcousticPSU0", save = NULL, ow = FALSE) {
    
    # If using a model project, copy this to the given project path:
    if(length(projectPath0) && isProject(projectPath0)) {
        copyProject(projectPath0, projectPath, ow = ow)
    }
    
    # Open the project, if not already open:
    if(!isProject(projectPath)) {
        warning("Non-existing project ", projectPath, ".")
        return(NULL)
    }
    # Open the project if not open:
    temp <- openIfNotAlreadyOpenProject(projectPath)
    
    # Get the process ID of the existing DefineAcousticPSU process:
    processID_existing <- findProcess(
        projectPath, 
        modelName = "baseline", 
        functionName = AcousticPSUProcessName
    )$processID
    if(length(processID_existing) != 1) {
        stop("Can only add new acoustic input files if exactly one ReadAcoustic process exists in the model.")
    }
    
    # Rename the process to the name given by AcousticPSUProcessNameNew:
    modifyProcessName(
        projectPath, 
        modelName = "baseline", 
        processID = processID_existing, 
        newProcessName = AcousticPSUProcessNameNew, 
        update.functionInputs = FALSE
    )
    
    # Duplicate the existing (now renamed) process, and use the original name:
    suppressWarnings(duplicateProcess(
        projectPath, 
        modelName = "baseline", 
        processID = processID_existing, 
        newProcessName = AcousticPSUProcessName
    ))
    
    # Get the procecss ID of the new process, which replaces the existing in terms of inputs to other processes:
    processID <- getProcessIDFromProcessName(
        projectPath, 
        modelName <- "baseline", 
        processName = AcousticPSUProcessName
    )$processID
    
    # Move the new process to immediately after the existing:
    rearrangeProcesses(
        projectPath, 
        modelName = "baseline", 
        processID = processID, 
        afterProcessID = processID_existing
    )
    
    # Set DefinitionMethod and AcousticPSU for the new process. Set also UseProcessData = FALSE since the process needs to be run to be effective:
    modifyProcess(
        projectPath, 
        modelName = "baseline", 
        processName = AcousticPSUProcessName, 
        newValues = list(
            functionParameters = list(
                DefinitionMethod = "PreDefined", 
                UseProcessData = FALSE
            ), 
            functionInputs = list(
                AcousticPSU = AcousticPSUProcessNameNew
            )
        )
    )
    
    # Replace the FileNames parameter of the ReadAcoustic process (requiring only one such process):
    if(length(ReadAcoustic.FileNames)) {
        ReadAcousticProcess <- findProcess(
            projectPath, 
            modelName = "baseline", 
            functionName = "ReadAcoustic"
        )
        if(NROW(ReadAcousticProcess) != 1) {
            stop("Can only add new acoustic input files if exactly one ReadAcoustic process exists in the model.")
        }
        modifyProcess(
            projectPath, 
            modelName = "baseline", 
            processName = ReadAcousticProcess$processName, 
            newValues = list(
                functionParameters = list(
                    FileNames = ReadAcoustic.FileNames
                )
            )
        )
    }
    
    # Run the baseline, save and close:
    output <- projectPath
    if(run) {
        output <- runProject(projectPath, modelNames = modelNames)
    }
    
    closeProject(projectPath, save = save)
    
    return(output)
}


