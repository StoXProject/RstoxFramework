# Backward compabitibility actions. These need not to be exported as is the case for any other Rstox-packages, since RstoxFramework is the package that collects the backwardCompatibility objects:
backwardCompatibility <- list(
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
        )
    )
)




applyBackwardCompatibility <- function(projectDescription, verbose = FALSE) {
    
    ## Save the original projectDescription:
    #originalProjectDescription <- projectDescription
    #temp <- tempfile()
    ##message(paste("Original file saved to", temp))
    #save(projectDescription, file = temp)
    
    # Get the backward compatibility specifications:
    backwardCompatibility <- getRstoxFrameworkDefinitions("backwardCompatibility")
    
    # Get the supported backward compatibility actions:
    backwardCompatibilityActionNames <- getRstoxFrameworkDefinitions("backwardCompatibilityActionNames")
    
    # Run the backward compatibility actions:
    projectDescription <- applyBackwardCompatibilityActions(
        backwardCompatibilityActionNames = backwardCompatibilityActionNames, 
        backwardCompatibility = backwardCompatibility, 
        projectDescription = projectDescription, 
        verbose = verbose
    )
    
    return(projectDescription)
}



applyBackwardCompatibilityActions <- function(
    backwardCompatibilityActionNames, 
    backwardCompatibility, 
    projectDescription, 
    verbose = FALSE
) {
    # Run through the supported backward compatibility action names:
    for(backwardCompatibilityActionName in backwardCompatibilityActionNames) {
        
        # Run through the packcages with backward compatibility actions:
        for(packageName in names(backwardCompatibility)) {
            
            # Run through the backward compatibility actions:
            for(backwardCompatibilityAction in backwardCompatibility [[packageName]] [[backwardCompatibilityActionName]]) {
                run <- checkBackwardCompatibilityVersion(
                    backwardCompatibilityAction = backwardCompatibilityAction, 
                    projectDescription = projectDescription, 
                    packageName = packageName
                )
                
                if(run) {
                    # Apply the backwardCompatibilityAction:
                    projectDescription <- applyBackwardCompatibilityAction(
                        backwardCompatibilityActionName = backwardCompatibilityActionName, 
                        backwardCompatibilityAction = backwardCompatibilityAction, 
                        projectDescription = projectDescription, 
                        packageName = packageName, 
                        verbose = verbose
                    )
                }
            }
            
        }
        
    }
    
    return(projectDescription)
    
}

applyBackwardCompatibilityAction <- function(
    backwardCompatibilityActionName, 
    backwardCompatibilityAction, 
    projectDescription, 
    packageName, 
    verbose = FALSE
) {
    # Construct the function name:
    applyFunctionName <- paste0("apply", capitalizeFirstLetter(backwardCompatibilityActionName))
    args <- list(
        backwardCompatibilityAction, 
        projectDescription = projectDescription, 
        packageName = packageName, 
        verbose = verbose
    )
    # Call the function:
    projectDescription <- do.call(applyFunctionName, args)
    
    return(projectDescription)
}




interpretVersionString <- function(versionString) {
    # Keep everything after first underscore:
    versionString <- sub("^[^_| ]*_", "", versionString)
    # Keep everything after first space:
    versionString <- sub("^\\S+\\s+", '', versionString)
    
    return(versionString)
}





#### These functions are run by applyBackwardCompatibilityAction(): 

applyRenameAttribute <- function(action, projectDescription, packageName, verbose = FALSE) {
    # Get the indices at functions to apply the action to:
    att <- attributes(projectDescription)
    
    if(action$attributeName %in% names(att)) {
        if(verbose) {
            message("Backward compatibility: Renaming attribute ", action$attributeName, " to ", action$newAttributeName)
        }
        # Add the new attribute name:
        att[[action$newAttributeName]] <- att[[action$attributeName]]
        # Delete the old:
        att[[action$attributeName]] <- NULL
        # Add the modified attributes:
        attributes(projectDescription) <- att
    }
    
    return(projectDescription)
}


applyAddAttribute <- function(action, projectDescription, packageName, verbose = FALSE) {
    # Get the indices at functions to apply the action to:
    att <- attributes(projectDescription)
    
    if(!action$attributeName %in% names(att)) {
        if(verbose) {
            message("Backward compatibility: Adding attribute ", action$attributeName, " = ", action$attributeValue)
        }
        # Add the new attribute name:
        attr(projectDescription, action$attributeName) <- action$attributeValue
        
    }
    
    return(projectDescription)
}


applyRenameFunction <- function(action, projectDescription, packageName, verbose = FALSE) {
    
    # Get the indices at functions to apply the action to:
    atFunctionName <- getIndicesAtFunctionName(
        projectDescription = projectDescription, 
        action = action, 
        packageName = packageName
    )
    
    if(verbose && length(atFunctionName)) {
        message("Backward compatibility: Renaming function ", action$functionName, " to ", action$newFunctionName)
    }
    
    for(ind in atFunctionName) {
        # Rename function: 
        projectDescription[[action$modelName]][[ind]]$functionName <- action$newFunctionName
    }
    
    return(projectDescription)
}


applyRemoveParameter <- function(action, projectDescription, packageName, verbose = FALSE) {
    
    # Get the indices at functions to apply the action to:
    atFunctionName <- getIndicesAtFunctionName(
        projectDescription = projectDescription, 
        action = action, 
        packageName = packageName
    )
    
    for(ind in atFunctionName) {
        
        # Remove any relevant function input: 
        projectDescription[[action$modelName]][[ind]]$functionInputs <- removeParameterInOneProcess(
            projectDescription[[action$modelName]][[ind]]$functionInputs, 
            action, 
            verbose = verbose
        )
        
        # Remove any relevant function parameter: 
        projectDescription[[action$modelName]][[ind]]$functionParameters <- removeParameterInOneProcess(
            projectDescription[[action$modelName]][[ind]]$functionParameters, 
            action, 
            verbose = verbose
        )
    }
    
    return(projectDescription)
}


applyRenameParameter <- function(action, projectDescription, packageName, verbose = FALSE) {
    
    # Get the indices at functions to apply the action to:
    atFunctionName <- getIndicesAtFunctionName(
        projectDescription = projectDescription, 
        action = action, 
        packageName = packageName
    )
    
    for(ind in atFunctionName) {
        
        # Rename any relevant function input: 
        projectDescription[[action$modelName]][[ind]]$functionInputs <- renameParameterInOneProcess(
            projectDescription[[action$modelName]][[ind]]$functionInputs, 
            action, 
            verbose = verbose
        )
        
        # Rename any relevant function parameter: 
        projectDescription[[action$modelName]][[ind]]$functionParameters <- renameParameterInOneProcess(
            projectDescription[[action$modelName]][[ind]]$functionParameters, 
            action, 
            verbose = verbose
        )
    }
    
    return(projectDescription)
}


applyTranslateParameter <- function(action, projectDescription, packageName, verbose = FALSE) {
    
    # Get the indices at functions to apply the action to:
    atFunctionName <- getIndicesAtFunctionName(
        projectDescription = projectDescription, 
        action = action, 
        packageName = packageName
    )
    
    for(ind in atFunctionName) {
        # Only relevant for function parameters, as function inputs are without possible values:
        # Remove any relevant function parameter: 
        projectDescription[[action$modelName]][[ind]]$functionParameters <- translateParameterInOneProcess(
            projectDescription[[action$modelName]][[ind]]$functionParameters, 
            action, 
            verbose = verbose
        )
    }
    
    return(projectDescription)
}


applyRenameProcessData <- function(action, projectDescription, packageName, verbose = FALSE) {
    
    # Get the indices at functions to apply the action to:
    atFunctionName <- getIndicesAtFunctionName(
        projectDescription = projectDescription, 
        action = action, 
        packageName = packageName
    )
    
    for(ind in atFunctionName) {
        
        # Rename any relevant function parameter: 
        projectDescription[[action$modelName]][[ind]]$processData <- renameProcessDataInOneProcess(
            projectDescription[[action$modelName]][[ind]]$processData, 
            action, 
            verbose = verbose
        )
    }
    
    return(projectDescription)
}

applyAddParameter <- function(action, projectDescription, packageName, verbose = FALSE) {
    # Get the indices at functions to apply the action to:
    atFunctionName <- getIndicesAtFunctionName(
        projectDescription = projectDescription, 
        action = action, 
        packageName = packageName
    )
    
    for(ind in atFunctionName) {
        # Add the function parameter: 
        projectDescription[[action$modelName]][[ind]]$functionParameters[[action$parameterName]] <- action$parameterValue
    }
    
    return(projectDescription)
}




#applySplitFunction <- function(action, projectDescription, packageName, verbose = FALSE) {
#    
#    # Get the indices at functions to apply the action to:
#    atFunctionName <- getIndicesAtFunctionName(
#        projectDescription = projectDescription, 
#        action = action, 
#        packageName = packageName
#    )
#    
#    for(ind in atFunctionName) {
#        
#        # Rename any relevant function parameter: 
#        projectDescription[[action$modelName]][[ind]]$processData <- splitFunctionInOneProcess(
#            projectDescription[[action$modelName]][[ind]]$processData, 
#            action, 
#            verbose = verbose
#        )
#    }
#    
#    return(projectDescription)
#}



getIndicesAtFunctionName <- function(projectDescription, action, packageName) {
    # Get the function names (packageName::functionName) of the processes of the model on which the action works:
    functionNames <- sapply(projectDescription[[action$modelName]], "[[", "functionName")
    
    # Match with the function in the action:
    action_functionName <- paste(packageName, action$functionName, sep = "::")
    atFunctionName <- which(action_functionName == functionNames)
    
    return(atFunctionName)
}






#### The actual backward compatibility actions are performed using the following functions:
removeParameterInOneProcess <- function(list, action, verbose = FALSE) {
    # Find the objects to remove:
    toRemove <- names(list) == action$parameterName
    # Remove if any to remove:
    if(any(toRemove)) {
        if(verbose) {
            message("Backward compatibility: Removing parameter ", action$parameterName, " in function ", action$functionName)
        }
        
        # Remove the parameter:
        list <- list[!toRemove]
    }
    return(list)
}

renameParameterInOneProcess <- function(list, action, verbose = FALSE) {
    # Find the objects to remove:
    toRename <- names(list) == action$parameterName
    # Remname if any to remove:
    if(any(toRename)) {
        if(verbose) {
            message("Backward compatibility: Remnaming parameter ", action$parameterName, " to ", action$newParameterName, " in function ", action$functionName)
        }
        
        # Remname the parameter:
        names(list)[names(list) == action$parameterName] <- action$newParameterName
    }
    return(list)
}

translateParameterInOneProcess <- function(list, action, verbose = FALSE) {
    
    # Find the objects to remove:
    toTranslate <- which(names(list) == action$parameterName)
    # Remove if any to remove:
    if(length(toTranslate) == 1) {
        if(identical(list[[toTranslate]], action$value)) {
            if(verbose) {
                message("Backward compatibility: Translating parameter ", action$parameterName, " from ", action$value, " to ", action$newValue, " in function ", action$functionName)
            }
            
            # Translate the parameter:
            list[[toTranslate]] <- action$newValue
        }
    }
    return(list)
}

renameProcessDataInOneProcess <- function(list, action, verbose = FALSE) {
    # Rename if the processData has the old name:
    if(names(list) == action$processDataName) {
        if(verbose) {
            message("Backward compatibility: Renaming process data ", action$processDataName, " to ", action$newProcessDataName, " in function ", action$functionName)
        }
        
        # Rename the process data:
        names(list) <- action$newProcessDataName
    }
    
    return(list)
}

#splitFunctionInOneProcess <- function(list, action, verbose = FALSE) {
#    # Find the objects to remove:
#    toRemove <- names(list) == action$parameterName
#    # Remove if any to remove:
#    if(any(toRemove)) {
#        if(verbose) {
#            message("Backward compatibility: Removing parameter ", action$parameterName, " in function ", #action$functionName)
#        }
#        
#        # Remove the parameter:
#        list <- list[!toRemove]
#    }
#    return(list)
#}


checkActionKeys <- function(action) {
    required_function <- c(
        "changeVersion",
        "functionName",
        "modelName"
    )
    required_attribute <- c(
        "changeVersion",
        "attributeName"
    )
    
    # Check whether all required elements are present:
    allPresent <- all(required_function %in% names(action)) || all(required_attribute %in% names(action))
    
    if(!allPresent) {
        warning("The following action of package ", action$packageName, "does not contain all required elements (", paste(required_function, collapse = ", "), " or ", paste(required_attribute, collapse = ", "), "): \n", paste("\t", names(action), action, sep = ": ", collapse = "\n"))
    }
    
    return(allPresent)
}


checkBackwardCompatibilityVersion <-  function(backwardCompatibilityAction, projectDescription, packageName) {
    
    # Skip if not a valid backwardCompatibilityAction:
    if(!checkActionKeys(backwardCompatibilityAction)) {
        return(FALSE)
    }
    # Skip if the projectDescription does not have the required attribute:
    if(!length(attr(projectDescription, "RstoxPackageVersion"))) {
        return(FALSE)
    }
    
    # Do only if the old version is lower than or equal to the fromVersion, and that the current version is higher than or equal to the toVersion:
    # Get last saved version:
    lastSavedVersion <- attr(projectDescription, "RstoxPackageVersion")
    
    # If the projectDescription does not have attrtibutes, apply the conversion with a warning:
    if(length(lastSavedVersion)) {
        # ..of the relevant package:
        lastSavedVersion <- lastSavedVersion[startsWith(lastSavedVersion, packageName)]
        lastSavedVersion <- interpretVersionString(lastSavedVersion)
        #convert <- lastSavedVersion < backwardCompatibilityAction$changeVersion
        convert <- utils::compareVersion(backwardCompatibilityAction$changeVersion, lastSavedVersion) == 1
    }
    # NA is introduced 
    else if(is.na(lastSavedVersion)) {
        convert <- TRUE
    }
    else {
        warning("StoX: The project does not have attributes, and is assumed to be of an old form prior to StoX 2.9.16. All backward compatibility actions are attempted.")
        convert <- TRUE
    }
    
    return(convert)
}




#' Backward compabitibility actions:
#' 
#' @inheritParams general_arguments
#' @param projectPath2.7 The path to the StoX 2.7 project to convert to StoX 3.x.x.
#' @param projectPath3 The path to the StoX 3.x.x project used as model for the conversion.
#' @param newProjectPath3 The path to the StoX 3.x.x project to create.
#' @param run Logical: If TRUE, run the entire project after converting. If FALSE, changes will not be stored in the project.json before one opens and runs the baseline.
#' 
#' @export
#' 
convertStoX2.7To3 <- function(projectPath2.7, projectPath3, newProjectPath3, ow = FALSE, run = TRUE) {
    
    # Save to a different project:
    createProject(
        projectPath = newProjectPath3, 
        template = "EmptyTemplate", 
        ow = ow, 
        showWarnings = FALSE, 
        open = FALSE, 
        Application = R.version.string
    )
    # Replace the project.json file from the projectPath3:
    projectDescriptionFile3 <- getProjectPaths(projectPath3, "projectJSONFile")
    newProjectDescriptionFile3 <- getProjectPaths(newProjectPath3, "projectJSONFile")
    unlink(newProjectDescriptionFile3, force = TRUE)
    file.copy(projectDescriptionFile3, newProjectDescriptionFile3)
    
    # Open the project, and use the built-in functions to manipulate an open project, instead of developing functions to manipulate a project description file directly:
    openProject(newProjectPath3)
    
    # Copy input data.
    copyInputDataFrom2.7(
        projectPath2.7 = projectPath2.7, 
        projectPath3 = newProjectPath3, 
        clearExisting = TRUE
    )
    # And update the file paths:
    updateInputDataFiles(newProjectPath3)
    
    # Replace the project.xml file path:
    projectXMLFilePath <- RstoxBase::getProjectXMLFilePath(projectPath = projectPath2.7)
    functionsToApplyProjectXML2.7To <- c(
        "DefineSurvey", 
        "DefineStratumPolygon", 
        "DefineBioticPSU", 
        "DefineAcousticPSU", 
        "DefineBioticAssignment"
    )
    lapply(functionsToApplyProjectXML2.7To, 
        applyProjectXML2.7, 
        projectPath3 = newProjectPath3, 
        projectXMLFilePath = projectXMLFilePath
    )
    
    output <- newProjectPath3
    if(run) {
        output <- tryCatch(
            # Save afterwards in case the run fails:
            runModel(newProjectPath3, modelName = "baseline", returnModelData = FALSE, save = FALSE), 
            error = function(err) err$message
        )
    }
    saveProject(newProjectPath3)
    closeProject(newProjectPath3)
    
    return(output)
}





# Function to change the FileName in processes using Define* with DefinitionMethod = "ResourceFile", and FileName ending with "xml":
applyProjectXML2.7 <- function(functionName, projectPath3, projectPath2.7, projectXMLFilePath = NULL) {
    
    # Find the processes:
    processName <- findProcess(
        projectPath3, 
        modelName = "baseline", 
        functionName = functionName
    )$processName
    
    if(!length(processName)) {
        warning("No processes using the function ", functionName, " found in the baseline model. Returning NULL.")
        return(NULL)
    }
    else if(length(processName) > 1) {
        stop("Applying a project.xml file from StoX 2.7 requires only one process using the function ", functionName, ".")
    }
    
    # Set DefinitionMethod to ResourceFile and FileName to the path to the project.xml:
    if(!length(projectXMLFilePath)) {
        projectXMLFilePath <- RstoxBase::getProjectXMLFilePath(projectPath = projectPath2.7)
    }
    
    # We need the processID for some of the below actions:
    processID <- getProcessIDFromProcessName(
        projectPath = projectPath3, 
        modelName = "baseline", 
        processName = processName
    )$processID
    
    
    # Set DefinitionMethod = "ResourceFile":
    modifyProcess(
        projectPath = projectPath3, 
        modelName = "baseline", 
        processName = processName, 
        newValues = list(
            functionParameters = list(
                DefinitionMethod = "ResourceFile"
            )
        )
    )
    
    # Set FileName = projectXMLFilePath:
    modifyProcess(
        projectPath = projectPath3, 
        modelName = "baseline", 
        processName = processName, 
        newValues = list(
            functionParameters = list(
                FileName = projectXMLFilePath
            )
        )
    )
    
    # In the case of DefineStratumPolygon, set StratumNameLabel = "StratumName":
    if(processName == "DefineStratumPolygon") {
        modifyProcess(
            projectPath = projectPath3, 
            modelName = "baseline", 
            processName = processName, 
            newValues = list(
                functionParameters = list(
                    StratumNameLabel = "StratumName"
                )
            )
        )
    }
    
    # Set UseProcessData = FALSE:
    setUseProcessData(
        projectPath = projectPath3, 
        modelName = "baseline", 
        processID = processID, 
        UseProcessData = FALSE
    )
    
    # Empty non-visible parameters: 
    nonVisibleArguments <- emptyNonVisibleArguments(
        projectPath = projectPath3, 
        modelName = "baseline", 
        processID = processID
    )
    message("Emptied the following function parameters of procecss ", processName, ", as no longer relevant when DefinitionMethod = \"ResourceFile\":\n", paste0("\t", nonVisibleArguments, collapse = "\n "))
    
    
    return(processName)
}

# Function to empty all non-visible arguments of a process, used to avoid confusion when changing process data with info from a project.xml file from StoX 2.7:
emptyNonVisibleArguments <- function(projectPath, modelName, processID) {
    # Get the names of the non-visible arguments:
    nonVisibleArguments <- getArgumentsToShow(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        return.only.names = FALSE
    )
    nonVisibleArguments <- names(nonVisibleArguments)[nonVisibleArguments %in% FALSE]
    
    areInputs <- isFunctionInput(nonVisibleArguments)
        
    # Empty the arguments:
    emptyFunctionInputs( 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        functionInputsNames = nonVisibleArguments[areInputs], 
        archive = TRUE
    )
    emptyFunctionParameters( 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        functionParameterNames = nonVisibleArguments[!areInputs], 
        archive = TRUE
    )
    
    return(nonVisibleArguments)
}


copyInputDataFrom2.7 <- function(projectPath2.7, projectPath3, clearExisting = TRUE,  types = c("Acoustic", "Biotic", "Landing")) {
    # Copy for the given possible input file types:
    temp <- lapply(
        types, 
        copyInputFilesOneType, 
        projectPath2.7 = projectPath2.7, 
        projectPath3 = projectPath3, 
        clearExisting = clearExisting
    )
    names(temp) <- types[lengths(temp) > 0]
    temp <- temp[lengths(temp) > 0]
    
    return(temp)
}

# Find processes using ReadBiotic, ReadAcoustic or ReadLanding:
copyInputFilesOneType <- function(
    type = c("Acoustic", "Biotic", "Landing"), 
    projectPath2.7, 
    projectPath3, 
    clearExisting = TRUE
) {
    # Get the data type:
    type <- match.arg(type)
    
    # Read the project.xml file to a list:
    projectList2.7 <- RstoxBase::readProjectXMLToList(projectPath = projectPath2.7)
    
    # Find the reading processes of baseline model:
    atBaseline <- which(lapply(getElementsByName(projectList2.7, "model"), attr, "name") == "baseline")
    functions2.7 <- unname(unlist(lapply(projectList2.7[[atBaseline]], "[[", "function")))
    readFunctionName <- paste0("Read", type, "XML")
    atRead2.7 <- which(functions2.7 == readFunctionName)
    
    # There can only be one reading process of the type in the StoX 2.7 project, so we know for sure where to get the files:
    if(!length(atRead2.7)) {
        return(NULL)
    }
    else if(length(atRead2.7) > 1) {
        stop("More than one process using ", readFunctionName, " detected. Only projects with exactly one such process can be used.")
    }
    
    # Get the file names:
    projectList2.7[[atBaseline]][[atRead2.7]]
    atFileName <- which(startsWith(sapply(getElementsByName(projectList2.7[[atBaseline]][[atRead2.7]], "parameter"), attr, "name"), "FileName"))
    fileRelativePaths <- unlist(getElementsByName(projectList2.7[[atBaseline]][[atRead2.7]], "parameter")[atFileName])
    fileAbsolutePaths <- file.path(projectPath2.7, fileRelativePaths)
    
    # Get the path to the input directory:
    inputDir <- getProjectPaths(projectPath3, tolower(type))
    
    # Delete the existing files:
    if(clearExisting) {
        toClear <- list.files(inputDir, full.names = TRUE)
        unlink(toClear, force = TRUE, recursive = TRUE)
    }
    
    # Copy the files:
    file.copy(fileAbsolutePaths, inputDir)
    
    # Return the paths of the copied files:
    copiedFiles <- file.path(inputDir, basename(fileAbsolutePaths))
    
    return(copiedFiles)
}

getElementsByName <- function(x, name) {
    x[names(x) == name]
}





updateInputDataFiles <- function(projectPath, inputDataTypes = c("acoustic", "biotic", "landing")) {
    
    if(!isOpenProject(projectPath)) {
        stop("The project ", projectPath, " needs to be open to modify the project.")
        return(NULL)
    }
    
    # Get the table of baseline processes:
    baselineTable <- getProcessAndFunctionNames(projectPath, "baseline")
    
    
    lapply(
        inputDataTypes, 
        updateInputDataFilesOne, 
        projectPath = projectPath, 
        baselineTable = baselineTable
    )
    
    saveProject(projectPath)
}

updateInputDataFilesOne <- function(inputDataType, projectPath, baselineTable) {
    
    # Get the input files:
    inputFiles <- list.files(getProjectPaths(projectPath, inputDataType), full.names = TRUE)
    
    inputFunctionName <- paste0("Read", tools::toTitleCase(tolower(inputDataType)))
    inputProcessNames <- baselineTable[endsWith(functionName, inputFunctionName), processName]
    
    for(inputProcessName in inputProcessNames) {
        modifyProcess(
            projectPath = projectPath, 
            modelName = "baseline", 
            processName = inputProcessName, 
            newValues = list(
                functionParameters = list(
                    FileNames = inputFiles
                )
            )
        )
    }
    
    return(inputFiles)
}
















# Functions to subset an NMDBiotic or NMDEchosounder file, useful for creating small test-projects:
subsetNMDBiotic <- function(NMDBioticFile, newNMDBioticFile, stationsIndex = 1) {
    subsetXMLFile(
        XMLFile = NMDBioticFile, 
        newXMLFile = newNMDBioticFile, 
        tag = "fishstation", 
        index = stationsIndex
    )
}

subsetNMDEchosounder <- function(NMDEchosounderFile, newNMDEchosounderFile, distanceIndex = 1) {
    subsetXMLFile(
        XMLFile = NMDEchosounderFile, 
        newXMLFile = newNMDEchosounderFile, 
        tag = "distance", 
        index = distanceIndex
    )
}

subsetXMLFile <- function(XMLFile, newXMLFile, tag = "fishstation", index = 1) {
    # Read the lines of the file:
    l <- readLines(XMLFile)
    
    # Get the start end end tag.
    tagStart <- paste0("<", tag)
    tagEnd <- paste0("</", tag)
    
    atStart <- which(grepl(tagStart, l))
    atEnd <- which(grepl(tagEnd, l))
    numberOfTags <- length(atStart)
    
    # Cannot extend the number of tags:
    index <- subset(index, index <= numberOfTags)
    
    
    
    before <- l[seq_len(atStart[1] - 1)]
    bodyIndex <- unlist(mapply(seq, atStart[index], atEnd[index]))
    body <- l[bodyIndex]
    after <- l[seq(atEnd[numberOfTags] + 1, length(l))]
    
    out <- c(
        before,  
        body, 
        after
    )
    
    if(missing(newXMLFile)) {
        newXMLFile <- paste(tools::file_path_sans_ext(XMLFile), "_subse.", tools::file_ext(XMLFile))
    }
    writeLines(out, newXMLFile)
}





