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
            parameterName = "OutputVariables"
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
            newValue = function(projectDescriptionOne) {
                
                renameResampleFunctionInBootstrapMethodTable(
                    projectDescriptionOne, 
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







# The main function to apply the backward compatibility (BWC) actions:
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
    #projectDescription <- applyBackwardCompatibilityActions(
    #    backwardCompatibilityActionNames = backwardCompatibilityActionNames, 
    #    backwardCompatibility = backwardCompatibility, 
    #    projectDescription = projectDescription, 
    #    verbose = verbose
    #)
    projectDescription <- applyBackwardCompatibilityActionsReordered(
        backwardCompatibility = backwardCompatibility, 
        projectDescription = projectDescription, 
        verbose = verbose
    )
    
    return(projectDescription)
}





# Apply one BWC action category:
applyBackwardCompatibilityActionsReordered <- function(
    backwardCompatibility, 
    projectDescription, 
    verbose = FALSE
) {
    
    # First add the package name and action type:
    for(packageName in names(backwardCompatibility)) {
        for(actionName in names(backwardCompatibility[[packageName]])) {
            for(actionIndex in seq_along(backwardCompatibility [[packageName]] [[actionName]])) {
                backwardCompatibility [[packageName]] [[actionName]] [[actionIndex]] [["packageName"]] <- packageName
                backwardCompatibility [[packageName]] [[actionName]] [[actionIndex]] [["actionName"]] <- actionName
            }
        }
    }
    # The unlist down to the actions:
    for(ind in seq_len(2)) {
        backwardCompatibility <- unlist(backwardCompatibility, recursive = FALSE)
    }
    
    # Now, order by version first, then by action name, and finally by package:
    packageNames <- sapply(backwardCompatibility, "[[", "packageName")
    actionNames <- sapply(backwardCompatibility, "[[", "actionName")
    changeVersions <- sapply(backwardCompatibility, "[[", "changeVersion")
    newOrder <- order(
        packageNames, 
        RstoxData::createOrderKey(changeVersions, split = "."), 
        match(actionNames, getRstoxFrameworkDefinitions("backwardCompatibilityActionNames"))
    )
    backwardCompatibility <- backwardCompatibility[newOrder]
    
    
    # Run through the supported backward compatibility action names:
    for(backwardCompatibilityAction in backwardCompatibility) {
        
        packageName <- backwardCompatibilityAction$packageName
        actionName <- backwardCompatibilityAction$actionName
        
        run <- checkBackwardCompatibilityVersion(
            backwardCompatibilityAction = backwardCompatibilityAction, 
            projectDescription = projectDescription, 
            packageName = packageName
        )
        
        if(run) {
            # Apply the backwardCompatibilityAction:
            projectDescription <- applyBackwardCompatibilityAction(
                backwardCompatibilityActionName = actionName, 
                backwardCompatibilityAction = backwardCompatibilityAction, 
                projectDescription = projectDescription, 
                packageName = packageName, 
                verbose = verbose
            )
        }
    }
    
    return(projectDescription)
}




# Apply one specific BWC action:
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





########## These functions are run by applyBackwardCompatibilityAction(): ##########

#### 1. renameAttribute: ####
applyRenameAttribute <- function(action, projectDescription, packageName, verbose = FALSE) {
    # Get the indices at functions to apply the action to:
    att <- attributes(projectDescription)
    
    if(action$attributeName %in% names(att)) {
        if(verbose) {
            message("StoX: Backward compatibility: Renaming attribute '", action$attributeName, "' to '", action$newAttributeName, "'")
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


#### 2. addAttribute: ####
applyAddAttribute <- function(action, projectDescription, packageName, verbose = FALSE) {
    # Get the indices at functions to apply the action to:
    att <- attributes(projectDescription)
    
    if(!action$attributeName %in% names(att)) {
        if(verbose) {
            message("StoX: Backward compatibility: Adding attribute ", action$attributeName, " = ", action$attributeValue)
        }
        # Add the new attribute name:
        attr(projectDescription, action$attributeName) <- action$attributeValue
        
    }
    
    return(projectDescription)
}


#### 3. renameFunction: ####
applyRenameFunction <- function(action, projectDescription, packageName, verbose = FALSE) {
    
    # Get the indices at functions to apply the action to:
    atFunctionName <- getIndicesAtFunctionName(
        projectDescription = projectDescription, 
        action = action, 
        packageName = packageName
    )
    
    if(verbose && length(atFunctionName)) {
        message("StoX: Backward compatibility: Renaming function '", action$functionName, "' to '", action$newFunctionName, "'")
    }
    
    for(ind in atFunctionName) {
        # Rename function: 
        projectDescription[[action$modelName]][[ind]]$functionName <- action$newFunctionName
    }
    
    return(projectDescription)
}


#### 4. removeParameter: ####
applyRemoveParameter <- function(action, projectDescription, packageName, verbose = FALSE) {
    # Get the indices at functions to apply the action to:
    atFunctionName <- getIndicesAtFunctionName(
        projectDescription = projectDescription, 
        action = action, 
        packageName = packageName
    )
    
    for(ind in atFunctionName) {
        
        if(verbose && length(atFunctionName)) {
            message("StoX: Backward compatibility: Removing parameter '", action$parameterName, "' in process '", projectDescription[[action$modelName]][[ind]]$processName, "'")
        }
        
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

removeParameterInOneProcess <- function(functionParameters, action, verbose = FALSE) {
    # Find the objects to remove:
    toRemove <- names(functionParameters) %in% action$parameterName
    # Remove if any to remove:
    if(any(toRemove)) {
        #if(verbose) {
        #    message("StoX: Backward compatibility: Removing parameter ", action$parameterName, " in function ", action$f#unctionName)
        #}
        
        # Remove the parameter:
        functionParameters <- functionParameters[!toRemove]
    }
    return(functionParameters)
}


#### 5. renameParameter: ####
applyRenameParameter <- function(action, projectDescription, packageName, verbose = FALSE) {
    
    # Get the indices at functions to apply the action to:
    atFunctionName <- getIndicesAtFunctionName(
        projectDescription = projectDescription, 
        action = action, 
        packageName = packageName
    )
    
    for(ind in atFunctionName) {
        
        if(verbose && length(atFunctionName)) {
            message("StoX: Backward compatibility: Renaming parameter '", action$parameterName, "' to '", action$newParameterName, "' in process '", projectDescription[[action$modelName]][[ind]]$processName, "'")
        }
        
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

renameParameterInOneProcess <- function(functionParameters, action, verbose = FALSE) {
    # Find the objects to remove:
    toRename <- names(functionParameters) %in% action$parameterName
    # Remname if any to remove:
    if(any(toRename)) {
        #if(verbose) {
        #    message("StoX: Backward compatibility: Remnaming parameter ", action$parameterName, " to ", action$newParameterName, " in function ", action$functionName)
        #}
        
        # Remname the parameter:
        names(functionParameters)[names(functionParameters) %in% action$parameterName] <- action$newParameterName
    }
    return(functionParameters)
}


#### 6. addParameter: ####
applyAddParameter <- function(action, projectDescription, packageName, verbose = FALSE) {
    # Get the indices at functions to apply the action to:
    atFunctionName <- getIndicesAtFunctionName(
        projectDescription = projectDescription, 
        action = action, 
        packageName = packageName
    )
    for(ind in atFunctionName) {
        
        # Add the function parameter as function...:
        if(is.function(action$parameterValue)) {
            valueToAdd <- action$parameterValue(projectDescription[[action$modelName]][[ind]])
        }
        # Or as a single value:
        else {
            valueToAdd <- action$parameterValue
        }
        
        if(verbose && length(atFunctionName)) {
            message("StoX: Backward compatibility: Adding parameter '", action$parameterName, "' with value '", deparse(valueToAdd), "' in process '", projectDescription[[action$modelName]][[ind]]$processName, "'")
        }
        
        projectDescription[[action$modelName]][[ind]]$functionParameters[[action$parameterName]] <- valueToAdd
    }
    
    return(projectDescription)
}


#### 7. translateParameter: ####
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
        projectDescription[[action$modelName]][[ind]] <- translateParameterInOneProcess(
            projectDescriptionOne = projectDescription[[action$modelName]][[ind]], 
            action = action, 
            verbose = verbose
        )
    }
    
    return(projectDescription)
}

translateParameterInOneProcess <- function(projectDescriptionOne, action, verbose = FALSE) {
    
    # Possibly add the function parameter using a function:
    if(is.function(action$newValue)) {
        action$newValue <- action$newValue(projectDescriptionOne = projectDescriptionOne)
    }
    
    # Print message:
    if(verbose) {
        message("StoX: Backward compatibility: Translating parameter '", action$parameterName, "' from '", deparse(action$value), "' to '", action$newValue, "' in process ", projectDescriptionOne$processName, "'", "'")
    }
    
    # Find the parameter to translate:
    toTranslate <- which(names(projectDescriptionOne$functionParameters) %in% action$parameterName)
    
    # Translate if any to translate:
    if(length(toTranslate) == 1) {
        if(matchParameter(projectDescriptionOne$functionParameters[[toTranslate]], action$value)) {
            # Translate the parameter:
            projectDescriptionOne$functionParameters[[toTranslate]] <- action$newValue
        }
    }
    return(projectDescriptionOne)
}


# Support for function or multiple values in action$value, and also an empty list for non-specified parameters (list()):
matchParameter <- function(x, value) {
    #if(is.function(value)) {
    #    value(x)
    #}
    #else {
    #    isTRUE(x %in% value) || identical(x, value)
    #}
    if(length(value) == 1 && is.function(value)) {
        value(x)
    }
    else if(is.list(value)) {
        any(sapply(value, identical, x))
    }
    else{
        identical(x, value)
    }
}


#### 8. reshapeParameter: ####
applyReshapeParameter <- function(action, projectDescription, packageName, verbose = FALSE) {
    
    # Get the indices at functions to apply the action to:
    atFunctionName <- getIndicesAtFunctionName(
        projectDescription = projectDescription, 
        action = action, 
        packageName = packageName
    )
    
    for(ind in atFunctionName) {
        
        if(verbose && length(atFunctionName)) {
            message("StoX: Backward compatibility: Reshaping function parameter '", action$parameterName, "' in             process '", projectDescription[[action$modelName]][[ind]]$processName, "'")
        }
        
        # Rename any relevant process data column: 
        projectDescription[[action$modelName]][[ind]] <- reshapeParameter(
            projectDescription[[action$modelName]][[ind]], 
            action, 
            verbose = verbose
        )
    }
    
    return(projectDescription)
}

reshapeParameter <- function(projectDescriptionOne, action, verbose = FALSE) {
    # Reshape if the parameter is present
    if(action$parameterName %in% names(projectDescriptionOne$functionParameters)) {
        # Apply the reshape function:
        if(is.function(action$newValue)) {
            projectDescriptionOne <- action$newValue(projectDescriptionOne)
        }
        # Or insert a value directl
        else {
            projectDescriptionOne$functionParameters[[action$parameterName]] <- action$newValue
        }
    }
    
    return(projectDescriptionOne)
}


#### 9. renameProcessData: ####
applyRenameProcessData <- function(action, projectDescription, packageName, verbose = FALSE) {
    
    # Get the indices at functions to apply the action to:
    atFunctionName <- getIndicesAtFunctionName(
        projectDescription = projectDescription, 
        action = action, 
        packageName = packageName
    )
    
    for(ind in atFunctionName) {
        
        if(verbose && length(atFunctionName)) {
            message("StoX: Backward compatibility: Renaming process data '", action$processDataName, "' to '", action$newProcessDataName, "' in process '", projectDescription[[action$modelName]][[ind]]$processName, "'")
        }
        
        # Rename any relevant function parameter: 
        projectDescription[[action$modelName]][[ind]]$processData <- renameProcessDataInOneProcess(
            projectDescription[[action$modelName]][[ind]]$processData, 
            action, 
            verbose = verbose
        )
    }
    
    return(projectDescription)
}

renameProcessDataInOneProcess <- function(processData, action, verbose = FALSE) {
    # Rename if the processData has the old name:
    for(name in names(processData)) {
        if(name %in% action$processDataName) {
            #if(verbose) {
            #    message("StoX: Backward compatibility: Renaming process data ", action$processDataName, " to ", action$newProcessDataName, " in function ", action$functionName)
            #}
            
            # Rename the process data:
            names(processData)[names(processData) == name] <- action$newProcessDataName
        }
    }
    
    return(processData)
}


#### 10. renameColumInProcessDataTable: ####
applyRenameColumInProcessDataTable <- function(action, projectDescription, packageName, verbose = FALSE) {
    
    # Get the indices at functions to apply the action to:
    atFunctionName <- getIndicesAtFunctionName(
        projectDescription = projectDescription, 
        action = action, 
        packageName = packageName
    )
    
    for(ind in atFunctionName) {
        
        if(verbose && length(atFunctionName)) {
            message("StoX: Backward compatibility: Renaming column '", action$processDataColumnName,"' in process data '", action$processDataName, "' to '", action$newProcessDataColumnName, "' in process '", projectDescription[[action$modelName]][[ind]]$processName, "'")
        }
        
        # Reshape any relevant process data column: 
        projectDescription[[action$modelName]][[ind]]$processData <- renameProcessDataTableColumnInOneProcess(
            projectDescription[[action$modelName]][[ind]]$processData, 
            action, 
            verbose = verbose
        )
    }
    
    return(projectDescription)
}

# Unsuccessful attempt to format the proecss data before backwardscompatibility, which failed since the formatting requires the correct names of functiions etc.:
#renameProcessDataTableColumnInOneProcess <- function(list, action, verbose = FALSE) {
#    # Rename if the processData table column has the old name:
#    for(name in names(list)) {
#        if(
#            name == action$processDataName && 
#            data.table::is.data.table(list[[name]]) && 
#            action$processDataColumnName %in% names(list[[name]])
#        ) {
#            if(verbose) {
#                message("StoX: Backward compatibility: Renaming process data table", action$processDataColumnName, " to ", action$newProcessDataColumnName, " in function ", action$functionName)
#            }
#            # Rename using data.table::setnames():
#            data.table::setnames(
#                list[[name]], 
#                old = action$processDataColumnName, 
#                new = action$newProcessDataColumnName
#            )
#        }
#    }
#    
#    return(list)
#}
renameProcessDataTableColumnInOneProcess <- function(processData, action, verbose = FALSE) {
    # Rename if the processData table column has the old name:
    for(name in names(processData)) {
        if(
            name %in% action$processDataName && 
            action$processDataColumnName %in% names(processData[[name]][[1]])
        ) {
            # Rename each row:
            processData[[name]] <- lapply(processData[[name]], renameByName, old = action$processDataColumnName, new = action$newProcessDataColumnName)
        }
    }
    
    return(processData)
}

renameByName <- function(x, old, new) {
    names(x)[names(x) == old] <- new
    return(x)
}


#### 11. renameColumInProcessDataTable: ####
applyReshapeProcessData <- function(action, projectDescription, packageName, verbose = FALSE) {
    
    # Get the indices at functions to apply the action to:
    atFunctionName <- getIndicesAtFunctionName(
        projectDescription = projectDescription, 
        action = action, 
        packageName = packageName
    )
    
    for(ind in atFunctionName) {
        
        if(verbose && length(atFunctionName)) {
            message("StoX: Backward compatibility: Reshaping process data '", action$processDataName, "' in process '", projectDescription[[action$modelName]][[ind]]$processName, "'")
        }
        
        # Reshape any relevant process data column: 
        projectDescription[[action$modelName]][[ind]] <- reshapeProcessData(
            projectDescription[[action$modelName]][[ind]], 
            action, 
            verbose = verbose
        )
    }
    
    return(projectDescription)
}

reshapeProcessData <- function(projectDescriptionOne, action, verbose = FALSE) {
    # Reshape if the processData table column has the old name:
    if(action$processDataName %in% names(projectDescriptionOne$processData)) {
        # Apply the reshape function:
        projectDescriptionOne <- action$newProcessData(projectDescriptionOne)
    }
    
    return(projectDescriptionOne)
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












#splitFunctionInOneProcess <- function(list, action, verbose = FALSE) {
#    # Find the objects to remove:
#    toRemove <- names(list) == action$parameterName
#    # Remove if any to remove:
#    if(any(toRemove)) {
#        if(verbose) {
#            message("StoX: Backward compatibility: Removing parameter ", action$parameterName, " in function ", #action$functionName)
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
        
        #convert <- utils::compareVersion(backwardCompatibilityAction$changeVersion, lastSavedVersion) == 1
        convert <- semver::parse_version(backwardCompatibilityAction$changeVersion) > semver::parse_version(lastSavedVersion)
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

interpretVersionString <- function(versionString) {
    # Keep everything after first underscore:
    versionString <- sub("^[^_| ]*_", "", versionString)
    # Keep everything after first space:
    versionString <- sub("^\\S+\\s+", '', versionString)
    
    return(versionString)
}


#' Backward compabitibility actions:
#' 
#' @param projectPath2.7 The path to the StoX 2.7 project to convert to StoX 3.x.x.
#' @param projectPath3 The path to the StoX 3.x.x project used as model for the conversion.
#' @param newProjectPath3 The path to the StoX 3.x.x project to create.
#' @param functionsToApplyProjectXML2.7To A character vector of names of process data functions to read from the project.xml of the \code{projectPath2.7}.
#' @param ow Logical: If TRUE, overwrite the \code{newProjectPath3}.
#' @param run Logical: If TRUE, run the entire project after converting. If FALSE, changes will not be stored in the project.json before one opens and runs the baseline.
#' @param msg Logical: If TRUE, print extra messages.
#' 
#' @export
#' 
convertStoX2.7To3 <- function(
    projectPath2.7, 
    projectPath3, 
    newProjectPath3, 
    functionsToApplyProjectXML2.7To = c(
        "DefineSurvey", 
        "DefineStratumPolygon", 
        "DefineBioticPSU", 
        "DefineAcousticPSU", 
        "DefineBioticAssignment"
    ), 
    ow = FALSE, 
    run = TRUE, 
    msg = FALSE
) {
    
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
    if(!file.exists(projectPath3)) {
        stop("The StoX >= 3.0.0. template project must exist.")
    }
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
    
    lapply(functionsToApplyProjectXML2.7To, 
        applyProjectXML2.7, 
        projectPath3 = newProjectPath3, 
        projectXMLFilePath = projectXMLFilePath, 
        msg = msg
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
applyProjectXML2.7 <- function(functionName, projectPath3, projectPath2.7, projectXMLFilePath = NULL, msg = FALSE) {
    
    # Find the processes:
    processName <- findProcess(
        projectPath3, 
        modelName = "baseline", 
        functionName = functionName
    )$processName
    
    if(!length(processName)) {
        if(msg) {
            message("No processes using the function ", functionName, " found in the baseline model. Returning NULL.")
        }
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
    type <- RstoxData::match_arg_informative(type)
    
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






# Utility functions:
# Add EDSU as defined in the new StoX:
old2NewESDU <- function(x) {
    x <- lapply(strsplit(x, "/"), "[", -2)
    time <- lapply(x, function(y) paste0(paste(y[2], y[3], sep = "T"), ".000Z"))
    paste(sapply(x, utils::head, 1), time, sep = "/")
}
# Add LogKey as defined in the new StoX:
old2NewLogKey <- function(x) {
    format(as.POSIXct(x), format = "%Y-%m-%dT%H:%M:%OS3Z")
}
# Add BeamKey as defined in the new StoX:
old2NewBeamKey <- function(frequency, transceiver) {
    paste(frequency, transceiver, sep = "/")
}
# Add Station as defined in the old StoX:
new2OldStation <- function(x) {
    paste(
        sub("/.*", "", x), 
        sub(".*-", "", x), 
        sep = "/"
    )
}
# Convert all data.frames to data.table:
setDTs <- function(x) {
    if(is.list(x) && !is.data.frame(x)) {
        for(ind in seq_along(x)) {
            setDTs(x[[ind]])
        }
    }
    else if(is.data.frame(x)){
        data.table::setDT(x)
    }
    else {
        x
    }
}
# Order all data.tables:
setOrders <- function(x) {
    
    if(is.list(x) && !is.data.frame(x)) {
        for(ind in seq_along(x)) {
            setOrders(x[[ind]])
        }
    }
    else if(is.data.frame(x)){
        data.table::setorder(x)
    }
    else {
        x
    }
}

#' Read output file from StoX 2.7
#' 
#' @param x The path to the StoX 2.7 project file to read.
#' 
#' @export
#' 
readStoX2.7OutputFile <- function(x) {
    # Read the file:
    out <- data.table::fread(x, na.strings = "NA")
    
    # Do type conversions:
    # StratumArea:
    if("PolygonKey" %in% names(out)) {
        out[, PolygonKey := as.character(PolygonKey)]
    }

    # General:
    if("SampleUnit" %in% names(out)) {
        out[, SampleUnit := as.character(SampleUnit)]
    }
    if("Layer" %in% names(out)) {
        out[, Layer := as.character(Layer)]
    }
    if("Stratum" %in% names(out)) {
        out[, Stratum := as.character(Stratum)]
    }
    
    # Acoustic and NASC:
    if("AcoCat" %in% names(out)) {
        out[, AcoCat := as.character(AcoCat)]
    }
    if("ch" %in% names(out)) {
        out[, ch := as.character(ch)]
    }
    
    
    return(out)
}

#' Convert keys of a 2.7 table to mimic the keys of StoX >= 3
#' 
#' @param x The StoX 2.7 table.
#' 
#' @export
#' 
convertKeys2.7To3 <- function(x) {
    # NASC data type: 
    if(all(c("NASC", "SampleUnit") %in% names(x))) {
        x[, SampleUnit := old2NewESDU(SampleUnit)]
    }
    # NASC table of AcousticXML:
    if(all(c("sa", "start_time", "freq", "transceiver") %in% names(x))) {
        x[, LogKey := old2NewLogKey(start_time)]
        x[, BeamKey := old2NewBeamKey(freq, transceiver)]
    }
    
    if("Station" %in% names(x)) {
        message("Old Station cannot be converted to new Station. Use new2OldStation() on the new table instead.")
    }
     return(x)
}


mergeAssignment2.7With3 <- function(bioticassignment_2.7, BioticAssignment_3) {
    
    if(!data.table::is.data.table(bioticassignment_2.7)) {
        warning("bioticassignment_2.7 must be converted to a data.table.")
    }
    
    # Add Station as in old StoX:
    BioticAssignment_3[, Station := new2OldStation(Haul)]
    
    # Convert bioticassignment_2.7 to the form of the new BioticAssignment after pasting:
    bioticassignment_2.7 <- bioticassignment_2.7[, list(
        assignmentPasted = paste(Station, collapse = "__"), 
        weightsPasted = paste(StationWeight, collapse = "__")
    ), by = "AssignmentID"]
    
    # Convert BioticAssignmentWeighting to the form of the old bioticassignment with AssignmentID:
    data.table::setorderv(BioticAssignment_3, c("Stratum", "PSU", "Layer", "Station"))
    BioticAssignment_3[, assignmentPasted := paste(Station, collapse = "__"), by = "PSU"]
    BioticAssignment_3[, weightsPasted := paste(WeightingFactor, collapse = "__"), by = "PSU"]
    BioticAssignment_3[, numStations := .N, by = "PSU"]
    
    newBioticassignment <- unique(BioticAssignment_3[, c("assignmentPasted", "weightsPasted", "numStations")], by = "assignmentPasted")
    
    # Now we are ready for merging:
    m <- merge(bioticassignment_2.7, newBioticassignment, by = "assignmentPasted", all = TRUE)
    
    BioticAsignment <- data.table::data.table(
        AssignmentID = rep(m$AssignmentID,  m$numStations), 
        assignmentPasted = rep(m$assignmentPasted, m$numStations), 
        Station = unlist(strsplit(m$assignmentPasted, "__")), 
        StationWeight.x = as.numeric(unlist(strsplit(m$weightsPasted.x, "__"))), 
        StationWeight.y = as.numeric(unlist(strsplit(m$weightsPasted.y, "__")))
    )
    data.table::setorderv(BioticAsignment, c("AssignmentID", "Station"))
    
    return(BioticAsignment)
}


convertSampleUnitToChatacter <- function(DT) {
    if("SampleUnit" %in% names(DT)) {
        DT[, SampleUnit := as.character(SampleUnit)]
    }
}


# Function to compare one output from the old and new StoX project using merging:
compareByMerging <- function(output_Old, output_New, processName_Old, processName_New, keys_Old, keys_New, addKeys_Old, addKeys_New, dataVariable_Old, dataVariable_New, tolerance, data.out = FALSE) {
    
    old <- data.table::as.data.table(output_Old$outputData[[processName_Old]])
    convertSampleUnitToChatacter(old)
    
    new <- output_New[[processName_New]]
    if(!data.table::is.data.table(new) && is.list(new) && "Data" %in% names(new)) {
        new <- data.table::as.data.table(new$Data)
    }
    else {
        new <- data.table::as.data.table(new)
    }
    
    # Add keys if requested:
    if(length(addKeys_New) && is.list(addKeys_New) && !(length(addKeys_New) == 1 && is.na(addKeys_New))) {
        for(ind in seq_along(addKeys_New)) {
            # Get the name of the argument of the function:
            var <- names(formals(addKeys_New$Station_Old))[1]
            # Did not work out how to use expression for 'value' in set(). The following worked but not when assigning by reference:
            # new[,  addKeys_New[[ind]](.SD[[eval(var)]])]
            # This did not work (returning NAs): data.table::set(new, j = names(addKeys_New)[ind], value = addKeys_New[[ind]](.SD[[eval(var)]])) 
            data.table::set(new, j = names(addKeys_New)[ind], value = addKeys_New[[ind]](new$Haul))
        }
    }
    
    
    merged <- merge(old, new, by.x = keys_Old, by.y = keys_New, all = TRUE)
    
    # If the data variable has the same name in the old and new StoX data.table::merge appends .x and .y to the name:
    if(dataVariable_Old == dataVariable_New) {
        dataVariable_Old <- paste0(dataVariable_Old, ".x")
        dataVariable_New <- paste0(dataVariable_New, ".y")
    }
    
    merged[ , offset := get(dataVariable_New) / get(dataVariable_Old)]
    
    # :
    hasOffset <- subset(merged, abs((offset - 1)) > tolerance)
    positiveInOld_NAInNew <- subset(merged, !is.na(get(dataVariable_Old)) & is.na(get(dataVariable_New)))
    NAInOld_PositiveInNew <- subset(merged, is.na(get(dataVariable_Old)) & !is.na(get(dataVariable_New)))
    
    
    sumPositiveInOld_NAInNew <- positiveInOld_NAInNew[,  sum(get(dataVariable_Old), na.rm = TRUE)]
    sumNAInOld_PositiveInNew <- NAInOld_PositiveInNew[,  sum(get(dataVariable_New), na.rm = TRUE)]
    sum_Old <- merged[,  sum(get(dataVariable_Old), na.rm = TRUE)]
    sum_New <- merged[,  sum(get(dataVariable_Old), na.rm = TRUE)]
    
    
    output <- list(
        sumPositiveInOld_NAInNew = sumPositiveInOld_NAInNew,
        sumNAInOld_PositiveInNew = sumNAInOld_PositiveInNew,
        sum_Old = sum_Old,
        sum_New = sum_New,
        fractionSumPositiveInOld_NAInNew = sumPositiveInOld_NAInNew / sum_Old, 
        fractionSumNAInOld_PositiveInNew = sumNAInOld_PositiveInNew / sum_New, 
        hasOffset = hasOffset, 
        positiveInOld_NAInNew = positiveInOld_NAInNew,
        NAInOld_PositiveInNew = NAInOld_PositiveInNew, 
        tolerance = tolerance
    )
    if(data.out) {
        output <- c(
            output, 
            list(
                merged = merged
            )
        )
    }
    
    return(output)
}

# Function to compare one output from the old and new StoX project using cbinding (used for SuperIndAbundance/SuperIndividuals, where keys are not sufficient in the SuperIndAbundance):
compareByCbinding <- function(output_Old, output_New, processName_Old, processName_New, subsetByNAOn_Old, subsetByNAOn_New, orderBy_Old, orderBy_New, dataVariable_Old, dataVariable_New, tolerance, data.out = FALSE) {
    
    old <- data.table::as.data.table(output_Old$outputData[[processName_Old]])
    new <- output_New[[processName_New]]
    if(!data.table::is.data.table(new) && is.list(new) && "Data" %in% names(new)) {
        new <- data.table::as.data.table(new$Data)
    }
    else {
        new <- data.table::as.data.table(new)
    }
    
    # Discard rows of Old with NA for subsetByNAOn_Old:
    atNA_Old <- rowSums(is.na(old[, ..subsetByNAOn_Old])) == length(subsetByNAOn_Old)
    old <- subset(old, atNA_Old)
    atNA_New <- rowSums(is.na(new[, ..subsetByNAOn_New])) == length(subsetByNAOn_New)
    new <- subset(new, atNA_New)
    
    # Order:
    data.table::setorderv(old, orderBy_Old)
    data.table::setorderv(new, orderBy_New)
    
    
    output <- list(
        test = all(abs(new[[dataVariable_New]] - old[[dataVariable_Old]]) < tolerance), 
        tolerance = tolerance
    )
    if(data.out) {
        output <- c(
            output, 
            list(
                old = old, 
                new = new
            )
        )
    }
    
    return(output)
}



#' Compare processes in a StoX 2.7 project and a copy in StoX >= 3.0.0:
#' 
#' @param projectPathOld The path to the StoX 2.7 project.
#' @param projectPathNew The path to the copy project in StoX >= 3.0.0.
#' @param comparisonModel A list with specifications of what and how to compare the outputs from the old and new project (see Details).
#' @param output_Old The StoX 2.7 table.
#' @param output_New The StoX 2.7 table.
#' @param data.out The StoX 2.7 table.
#' 
#' The \code{comparisonModel} is a list with the following required elements:
#' \itemize{
#'  \item{dataType}{The StoX >= 3.0.0 datatypes to compare. Used only for reference.}
#'  \item{dataVariable_Old}{The specific variable to compare in the old datatypes.}
#'  \item{dataVariable_New}{The specific variable to compare in the new datatypes.}
#'  \item{processName_Old,processName_New}{The process named in the old and new project, respectively. Must correspond to the \code{dataType}.}
#'  \item{compareAction}{A string vector specifyfing whether to merge ("merge") or cbind ("cbind") when comparing. Cbind is only for SuperIndividuals in StoX 2.7, which does not have unique keys in.}
#'  \item{keys_Old,keys_New"}{A list of the keys of the datatypes in the old and new project, respectively. Specify only for datatypes to be merged (use NA for those that use compareAction = "cbind").}
#'  \item{subsetByNAOn_Old,subsetByNAOn_New"}{A list of variables to subset the old and new data by, respectively. When any of these are NA in a row, the row is skipped. Specify only for datatypes to be cbinded (use NA for those that use compareAction = "merge").}
#'  \item{orderBy_Old,orderBy_New}{SA list of variables to order the old and new data by, respectively (after subsetting using \code{subsetByNAOn_Old} and \code{subsetByNAOn_New}). Specify only for datatypes to be cbinded (use NA for those that use compareAction = "merge").}
#'  \item{tolerance}{A numeric vector setting the tolerance for the comparison, below which the outputs are regarded as equal.}
#' }
#' 
#' @export
#' 
compareSweptAreaBaseline <- function(projectPathOld, projectPathNew, comparisonModel, output_Old = NULL, output_New = NULL, data.out = FALSE) {
    
    numberOfComparisons <- length(comparisonModel$dataType)
    
    comparison <- vector("list", numberOfComparisons)
    
    # Run in old StoX:
    if(!length(output_Old)) {
        stop("The output_Old must be given. Please run the following in Rstox 1.11.1: \n\tlibrary(Rstox) \n\toutput_Old <- Rstox::getBaseline(\"", projectPathOld, "\")")
        #library(Rstox)
        #output_Old <- Rstox::getBaseline(projectPathOld)
    }
    
    # Run in new StoX:
    if(!length(output_New)) {
        output_New <- RstoxFramework::runModel(projectPathNew, modelName = "baseline", verbose = TRUE, close = TRUE)
    }
    
    
    
    for(ind in seq_len(numberOfComparisons)) {
        if(comparisonModel$compareAction[[ind]] == "merge") {
            comparison[[ind]] <- compareByMerging(
                output_Old = output_Old, 
                output_New = output_New, 
                processName_Old = comparisonModel$processName_Old[[ind]], 
                processName_New = comparisonModel$processName_New[[ind]], 
                addKeys_Old = comparisonModel$addKeys_Old[[ind]], 
                addKeys_New = comparisonModel$addKeys_New[[ind]], 
                keys_Old = comparisonModel$keys_Old[[ind]], 
                keys_New = comparisonModel$keys_New[[ind]], 
                dataVariable_Old = comparisonModel$dataVariable_Old[[ind]], 
                dataVariable_New = comparisonModel$dataVariable_New[[ind]], 
                tolerance = comparisonModel$tolerance[[ind]], 
                data.out = data.out
            )
        }
        else if(comparisonModel$compareAction[[ind]] == "cbind") {
            comparison[[ind]] <- compareByCbinding(
                output_Old = output_Old, 
                output_New = output_New, 
                processName_Old = comparisonModel$processName_Old[[ind]], 
                processName_New = comparisonModel$processName_New[[ind]], 
                subsetByNAOn_Old = comparisonModel$subsetByNAOn_Old[[ind]], 
                subsetByNAOn_New = comparisonModel$subsetByNAOn_New[[ind]], 
                orderBy_Old = comparisonModel$orderBy_Old[[ind]], 
                orderBy_New = comparisonModel$orderBy_New[[ind]], 
                dataVariable_Old = comparisonModel$dataVariable_Old[[ind]], 
                dataVariable_New = comparisonModel$dataVariable_New[[ind]], 
                tolerance = comparisonModel$tolerance[[ind]], 
                data.out = data.out
            )
        }
        else {
            stop("Unknown compareAction")
        }
    }
    
    names(comparison) <- comparisonModel$dataType
    
    if(data.out) {
        output <- list(
            comparison = comparison, 
            output_Old = output_Old, 
            output_New = output_New
        )
    }
    else {
        output <- comparison
    }
    
    return(output)
}

