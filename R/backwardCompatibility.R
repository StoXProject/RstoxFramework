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




# Backwards compatibility:

# 1. Re-organize models
# 	Baseline report and Report into report, R renamed to analysis

# 2. Distribute process data into processes
# 2.0. Convert process data to appropriate tables and sp objects
# 2.1. Copy stratumpolygon to all functions DefineStrata
# 2.2. Copy edsupsu and psustratum to all functions DefineAcousticPSU
# 2.3. Copy bioticassignment, suassignment and assignmentresolution to all functions BioStationAssignment and BioStationWeighting
# 2.4. Copy temporal to DefineTemporal, gearfactor to DefineGearFactor, spatial to DefineSpatial, platformfactor to DefinePlatform, ageerror to DefineAgeErrorMatrix and stratumneighbour to DefineStratumNeighbour, and treat covparam somehow, since this is a table that is appended by each of DefineTemporal, DefineGearFactor, DefineSpatial and DefinePlatform.


#
#Generate process data processes. DefineAccoustic
#
#
#- Removed function
#- Added function
#- Renamed function
#- Split function
#
#- Renamed parameter
#- Removed parameter
#- Added parameter
#
#
#


# 1. Remove processes:
# 1.1. Remove ReadProcessData
# 2.2. Remove WriteProcessData
# 
# 2. Copy process data into processes:
# 2.0. Convert process data to appropriate tables and sp objects
# 2.1. Copy stratumpolygon to all functions DefineStrata
# 2.2. Copy edsupsu and psustratum to a data.table for all functions DefineAcousticPSU
# 2.3. Copy bioticassignment to all functions BioStationAssignment and BioStationWeighting
# 2.4. Copy stratumpolygon to all functions DefineStrata
# 2.5. Copy stratumpolygon to all functions DefineStrata
# 2.6. Copy stratumpolygon to all functions DefineStrata
# 
# 
# - Split ReadBioticXML into RstoxData::ReadBiotic and RstoxData::StoxBiotic
# - Split ReadAcousticXML into RstoxData::ReadAcoustic and RstoxData::StoxAcoustic
# - Split ReadLandingXML into RstoxData::ReadLanding and RstoxData::StoxLanding
# 
# - Convert FilterBiotic to RstoxData::AddToStoxBiotic with all variables detected in the FilterBiotic and RstoxData::FilterStoxBiotic # with a filter expression built based on the Java JEXL expression of FilterBiotic
# - Convert FilterAcoustic to RstoxData::AddToAcoustic with all variables detected in the FilterAcoustic and RstoxData::FilterStoxAcou# stic with a filter expression built based on the Java JEXL expression of FilterAcoustic
# - Convert FilterLanding to RstoxData::FilterStoxLanding with a filter expression built based on the Java JEXL expression of FilterLan# ding
# 
# - Read biostationassignment global process data and map to BioticAssignment process data
# 
# - Read edsupsu and psustratum global process data and map to AcousticPSU process data
# 
# - Split AcousticDensity into RstoxBase::DefineAcousticTargetStrength and RstoxBase::AcousticDensity
# 
# - Replace StationLengthDist with RstoxBase::LengthDistribution
# 
# - Replace RegroupLengthDist with RstoxBase::RegroupLengthDistribution
# 
# - Replace DefineStrata with RstoxBase::DefineStratumPolygon
# 
# - Replace BioStationAssignment with RstoxBase::DefineBioticAssignment
# 
# - Replace BioStationWeighting with RstoxBase::BioticAssignmentWeighting
# 
# - Replace TotalLengthDist with RstoxBase::MeanLengthDistribution in swept-area models and RstoxBase::AssignmentLengthDistribution
# 
# - Delete SumDensity
# 
# - Replace IndividualDataStations and IndividualData with RstoxBase::Individuals
# 
# - Replace SuperIndAbundance with RstoxBase::SuperIndividuals
# 
# - Move imputeByAge to RstoxBase::ImputeSuperIndividuals
# 
# - Replace runBootstrap with RstoxFramework::Bootstrap
# 
# 
# 
# Backwards compatibility actions:
#     
# - Delete process
# - Replace process by new process
# - Replace process by several process


# Rename DefineStratumPolygon to DefineStratum:
#rename_DefineStratumPolygon_to_DefineStratum <- function(projectDescription) {
#    if(StoxVersion == 2.7) {
#        # Get the function names:
#        functionNames <- sapply(projectDescription$baseline, "[[", "functionName")
#        # Get the position of the process using DefineStratumPolygon():
#        atDefineStratumPolygon <- which("DefineStratumPolygon" %in% functionNames)
#        if(length(atDefineStratumPolygon)) {
#            # Split the process:
#            projectDescription$baseline[[atDefineStratumPolygon]]$functionName <- "DefineStratum"
#        }
#    }
#    
#    return(projectDescription)
#}







### # A list of functions performing conversions of the projectDescription to ensure backward compatibilit### y:
### #' 
### #' @export
### #' 
### backwardCompatibility2.7 <- list(
###     
###     # 1. Move process between models:
###     moveProcess = list(
###         list(
###             functionName = "ImpuetByAge", 
###             model = "R", 
###             newModel = "Baseline"
###         )
###     ), 
###     
###     # 2. Remove process: 
###     removeProcess = list(
###         list(
###             functionName = "ReadProcessData", 
###             model = "Baseline"
###         ), 
###         list(
###             functionName = "WriteProcessData", 
###             model = "Baseline"
###         ), 
###         list(
###             functionName = "SumDensity", 
###             model = "Baseline"
###         ), 
###         list(
###             functionName = "saveProjectData", 
###             model = "Analysis"
###         )
###     ), 
###     
###     # 3. Rename process:
###     
###     # 4. Split process:
###     
###     # 5. Join processes:
###     joinProcess = list(
###         list(
###             functionNames = c(
###                 "IndividualDataStations", 
###                 "IndividualData"
###             ), 
###             newFunctionName = "Individuals",
###             model = "Baseline"
###         ), 
###         list(
###             functionName = "WriteProcessData", 
###             model = "Baseline"
###         )
###     )
###     
###     
### )






#' Backward compabitibility actions:
#' 
#' @inheritParams general_arguments
#' @param projectPath2.7 The path to the StoX 2.7 project to convert to StoX 3.x.x.
#' @param projectPath3 The path to the StoX 3.x.x project used as model for the conversion.
#' @param newProjectPath3 The path to the StoX 3.x.x project to create.
#' 
#' @export
#' 
convertStoX2.7To3 <- function(projectPath2.7, projectPath3, newProjectPath3 = NULL, ow = FALSE) {
    
    # Save to a different project:
    if(!length(newProjectPath3)) {
        newProjectPath3 <- projectPath3
    }
    else {
        copyProject(
            projectPath = projectPath3, 
            newProjectPath = newProjectPath3, 
            ow = ow
        )
    }
    
    # Replace input data.
    if(ow) {
        inputDir2.7 <- file.path(projectPath2.7, "input")
        inputDir3 <- file.path(newProjectPath3, "input")
        unlink(inputDir3, force = TRUE, recursive = TRUE)
        file.copy(inputDir2.7, newProjectPath3, recursive = TRUE)
    }
    
    
    #redefineAcousticPSUFrom2.7(
    #    projectPath2.7 = projectPath2.7, 
    #    projectPath3 = projectPath3, 
    #    newProjectPath3 = newProjectPath3, 
    #    ow = ow
    #)
    
    #redefineBioticAssignmentFrom2.7(projectPath2.7, newProjectPath3)
    
    updateInputDataFiles(newProjectPath3)
    
    closeProject(newProjectPath3)
}

redefineAcousticPSUFrom2.7 <- function(projectPath2.7, projectPath3, newProjectPath3 = NULL, ow = FALSE) {
    
    # Read the AcousticPSU from the project.xml file:
    AcousticPSU <- RstoxBase::readAcousticPSUFrom2.7(projectPath2.7)
    
    projectDescription <- readProjectDescription(projectPath3)
    atDefineAcousticPSU <- which(sapply(projectDescription$projectDescription$baseline, "[[", "functionName") == "RstoxBase::DefineAcousticPSU")
    
    if(!length(atDefineAcousticPSU)) {
        warning("No process using RstoxBase::DefineAcousticPSU found in the project", newProjectPath3)
        return(FALSE)
    }
    else if(length(atDefineAcousticPSU) > 1) {
        warning("Multiple processes using RstoxBase::DefineAcousticPSU found in the project", newProjectPath3, ". All were modified.")
    }
    for(ind in atDefineAcousticPSU) {
        projectDescription$projectDescription$baseline[[ind]]$processData <- AcousticPSU
    }
    
    openProject(newProjectPath3)
    writeProjectDescription(newProjectPath3, projectDescription = projectDescription$projectDescription)
    
    closeProject(projectPath3)
    closeProject(newProjectPath3)
    
    return(unname(newProjectPath3))
}



updateInputDataFiles <- function(projectPath, inputDataTypes = c("acoustic", "biotic", "landing")) {
    openProject(projectPath)
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
    
    inputFunctionName <- paste0("Read", inputDataType)
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





