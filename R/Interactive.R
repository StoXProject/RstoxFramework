
# Decided after discussion with Aasmund on 2019-11-12:

# 1. We will strive for minimum redundancy in the process data. Process data will be saved as data tables with the possibility of variable length vectors in each cell, such as StratumID, StratumName and a vector of PSUs, and saved to the project.xml as JSON



############################################################
############################################################
#' Add or remove biotic hauls to/from assignment of hauls to acoustic PSU/Layer
#' 
#' The functions \code{addHaulsToAssignment} and \code{removeHaulsFromAssignment} adds or removes biotic hauls from the \code{\link{BioticAssignment}} process data of the specified process.
#' 
#' @param Stratum The stratum name related to the PSU.
#' @param PSU The acoustic primary sampling unit (PSU) for which to add/remove the haul.
#' @param Haul The biotic haul to add/remove (can be a vector of hauls).
#' @inheritParams general_arguments
#' 
#' @details 
#' The assignment IDs are refreshed for every change, after sorting the assignemnts by the PSU column.
#' 
#' @name BioticAssignment
#' 
NULL
#' 
#' @export
#' @rdname BioticAssignment
#' 
addHaulToAssignment <- function(Stratum, PSU, Haul, projectPath, modelName, processID) {
    modifyAssignment(
        Stratum = Stratum, 
        PSU = PSU, 
        Haul = Haul, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        action = "add"
    )
}
#' 
#' @export
#' @rdname BioticAssignment
#' 
removeHaulFromAssignment <- function(Stratum, PSU, Haul, projectPath, modelName, processID) {
    modifyAssignment(
        Stratum = Stratum, 
        PSU = PSU, 
        Haul = Haul, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        action = "remove"
    )
}

# Generic function to add or remove a haul:
modifyAssignment <- function(Stratum, PSU, Haul, projectPath, modelName, processID, action = c("add", "remove")) {
    
    # Check that the process returns BioticAssigment process data:
    checkDataType("BioticAssignment", projectPath = projectPath, modelName = modelName, processID = processID)
    
    ## Get the process data of the process, a table of PSU, Layer, Haul and HaulWeight:
    BioticAssignment <- getProcessData(projectPath = projectPath, modelName = modelName, processID = processID, check.activeProcess = TRUE)$BioticAssignment
    
    # PSUs are added in assignment_addHaul, so we do not need this warnning. Since most projects at IMR use Stratum, we have not discovered that this warning is false.
    ## Check for existing PSU:
    #if(!PSU %in% BioticAssignment$PSU) {
    #    warning("StoX: The acoustic PSU with name ", PSU, " does not exist. Please choose a different PSU name or add the PSU using DefineAcousticPSU.")
    #}
    
    # Add or remove the hauls (either using assignment_addHaul() or assignment_removeHaul()):
    utilityFunctionName <- paste0("assignment_", action[1], "Haul")
    BioticAssignment <- do.call(
        utilityFunctionName, 
        list(
            Stratum = Stratum, 
            PSU = PSU, 
            Haul = Haul, 
            BioticAssignment = BioticAssignment
        )
    )
    
    # Set the BioticAssignment back to the process data of the process:
    setProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        argumentValue = list(list(BioticAssignment = BioticAssignment)) # We need to list this to make it correspond to the single value of the argumentName parameter.
    )
    
    # Revert the active process ID to the previous process:
    resetModel(projectPath = projectPath, modelName = modelName, processID = processID, processDirty = TRUE, deleteCurrent = TRUE)
    
    # Return the active process:
    activeProcess <- getActiveProcess(projectPath = projectPath, modelName = modelName)
    
    return(
        list(
            activeProcess = activeProcess, 
            saved = isSaved(projectPath)
        )
    )
}

# Function that adds a haul to the assignment data:
assignment_addHaul <- function(Stratum, PSU, Layer, Haul, BioticAssignment) {
    
    # Add the hauls:
    toAdd <- data.table::data.table(
        Stratum = Stratum, 
        PSU = PSU, 
        # Changed to add NA as Layer. The Layers are added when RstoxBase::DefineBioticAssignment() is run:
        #Layer = Layer, 
        Layer = NA, 
        Haul = Haul, 
        WeightingFactor = 1
    )
    # If the PSU does not have any assignments it is still present with Haul = NA. If this is the case, remove that row before rbinnding, as such a row implies no assigned hauls:
    #atThisPSU <- BioticAssignment$Stratum == Stratum & BioticAssignment$PSU == PSU
    atThisPSU <- BioticAssignment$PSU == PSU
    numberOfAssignments <- sum(atThisPSU)
    if(numberOfAssignments == 1 && is.na(BioticAssignment[atThisPSU, Haul])) {
        BioticAssignment <- BioticAssignment[!atThisPSU, ]
    }
    # Add the new assignment:
    BioticAssignment <- rbind(
        BioticAssignment, 
        toAdd
    )
    
    # Order the BioticAssignment:
    #setorderv(BioticAssignment, cols = c("PSU", "Layer", "Haul"), na.last = TRUE)
    # Format the output, also ordering by PSUs:
    RstoxBase::formatOutput(BioticAssignment, dataType = "BioticAssignment", keep.all = FALSE)
    
    return(BioticAssignment)
}

# Function that removes a haul from the assignment data:
assignment_removeHaul <- function(Stratum, PSU, Haul, BioticAssignment) {
    
    # Get the row indixces of the PSU and Layer:
    # Changed to only consider PSUs, and not Layers. Assigning to different specific Layers will be implemented with separate functions in the future:
    #atPSU <- BioticAssignment$PSU %in% PSU
    #atLayer <- BioticAssignment$Layer  %in% Layer
    #at <- which(atPSU & atLayer)
    at <- which(BioticAssignment$PSU %in% PSU)
    
    # Get the indices in 'at' to remove:
    atHauls <- BioticAssignment[at, ]$Haul %in% Haul
    
    # Remove the hauls:
    BioticAssignment <- BioticAssignment[-at[atHauls], ]
    
    # If all Hauls were removed add a row with Haul = NA to keep the PSU in the data:
    if(!sum(BioticAssignment$PSU == PSU)) {
        dummyRowWithNAHaul <- data.table::data.table(
            Stratum = Stratum, 
            PSU = PSU, 
            Layer = NA, 
            Haul = NA, 
            WeightingFactor = 1
        )
        # Add the row with Haul = NA:
        BioticAssignment <- rbind(
            BioticAssignment, 
            dummyRowWithNAHaul
        )
    }

    # Format the output, also ordering by PSUs:
    RstoxBase::formatOutput(BioticAssignment, dataType = "BioticAssignment", keep.all = FALSE)
    
    return(BioticAssignment)
}




############################################################
############################################################
#' Add or remove acoustic PSUs and EDSUs
#' 
#' The functions \code{addStations} and \code{removeStations} adds or removes biotic stations from the BioticAssignment process data of the specified process.
#' 
#' @param Stratum The name of a stratum.
#' @param PSU The name of a PSU.
#' @param newPSUName The new name of a PSU.
#' @param EDSU The name of an EDSU.
#' 
#' @details 
#' The assignment IDs are refreshed for every change, after sorting the assignemnts by the PSU column.
#' 
#' @inheritParams getProcessOutput
#' @name AcousticPSU
#' 
NULL
#' 
#' @export
#' @rdname AcousticPSU
#' 
addAcousticPSU <- function(Stratum, PSU = NULL, projectPath, modelName, processID) {
    
    # Check that the process returns AcousticPSU process data:
    checkDataType("AcousticPSU", projectPath = projectPath, modelName = modelName, processID = processID)
    
    # Get the process data of the process, a table of PSU, Layer, AssignmentID, Haul and HaulWeight:
    AcousticPSU <- getProcessData(projectPath = projectPath, modelName = modelName, processID = processID, check.activeProcess = TRUE)
    
    # If the PSU is not given, use the default PSU name:
    if(length(PSU) == 0) {
        PSU <- getNewDefaultName(AcousticPSU$Stratum_PSU$PSU, prefix = RstoxBase::getRstoxBaseDefinitions("getPSUPrefix")("Acoustic"))
    }
    # Check whether the acoustic PSU already exists:
    if(any(AcousticPSU$Stratum_PSU$PSU == PSU)) {
        stop("StoX: The name of the Acoustic PSU (", PSU, ") already exists.")
    }
    
    # Add the acsoutic PSU:
    toAdd <- data.table::data.table(
        Stratum = Stratum, 
        PSU = PSU
    )
    AcousticPSU$Stratum_PSU <- rbind(
        AcousticPSU$Stratum_PSU, 
        toAdd
    )
    
    # Format the output:
    RstoxBase::formatOutput(AcousticPSU, dataType = "AcousticPSU", keep.all = FALSE)
    
    # Set the AcousticPSU back to the process data of the process:
    setProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        argumentValue = list(AcousticPSU) # We need to list this to make it correspond to the single value of the argumentName parameter.
    )
    
    # Revert the active process ID to the current process (currently it is a requirement that the process is the active process for the GUI to allow modification, so resetting the active process is not effective here) and set it as dirty:
    resetModel(projectPath = projectPath, modelName = modelName, processID = processID, processDirty = TRUE, deleteCurrent = TRUE)
    
    # Return the active process:
    activeProcess <- getActiveProcess(projectPath = projectPath, modelName = modelName)
    return(
        list(
            PSU = PSU, 
            activeProcess = activeProcess, 
            saved = isSaved(projectPath)
        )
    )
}
#' 
#' @export
#' @rdname AcousticPSU
#' 
removeAcousticPSU <- function(PSU, projectPath, modelName, processID) {
    
    # Check that the process returns AcousticPSU process data:
    checkDataType("AcousticPSU", projectPath = projectPath, modelName = modelName, processID = processID)
    
    # Get the process data of the process:
    AcousticPSU <- getProcessData(projectPath = projectPath, modelName = modelName, processID = processID, check.activeProcess = TRUE)
    
    # Remove the acoustic PSUs:
    PSUsToKeepInStratum_PSU <- !AcousticPSU$Stratum_PSU$PSU %in% PSU
    PSUsToSetToNAInEDSU_PSU <- AcousticPSU$EDSU_PSU$PSU %in% PSU
    
    AcousticPSU$Stratum_PSU <- AcousticPSU$Stratum_PSU[PSUsToKeepInStratum_PSU, ]
    #AcousticPSU$EDSU_PSU <- AcousticPSU$EDSU_PSU[EDSUsToKeep, ]
    AcousticPSU$EDSU_PSU[PSUsToSetToNAInEDSU_PSU, PSU := NA]
    
    # Format the output:
    RstoxBase::formatOutput(AcousticPSU, dataType = "AcousticPSU", keep.all = FALSE)
    
    # Set the AcousticPSU back to the process data of the process:
    setProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        argumentValue = list(AcousticPSU) # We need to list this to make it correspond to the single value of the argumentName parameter.
    )
    
    # Revert the active process ID to the previous process:
    resetModel(projectPath = projectPath, modelName = modelName, processID = processID, processDirty = TRUE, deleteCurrent = TRUE)
    
    # Return the active process:
    activeProcess <- getActiveProcess(projectPath = projectPath, modelName = modelName)
    return(
        list(
            activeProcess = activeProcess, 
            saved = isSaved(projectPath)
        )
    )
}
#' 
#' @export
#' @rdname AcousticPSU
#' 
renameAcousticPSU <- function(PSU, newPSUName, projectPath, modelName, processID) {
    
    # Check that the process returns AcousticPSU process data:
    checkDataType("AcousticPSU", projectPath = projectPath, modelName = modelName, processID = processID)
    
    ## Get the process data of the process:
    AcousticPSU <- getProcessData(projectPath = projectPath, modelName = modelName, processID = processID, check.activeProcess = TRUE)
    
    # Add the acsoutic PSU:
    atPSUsToRename <- AcousticPSU$Stratum_PSU$PSU %in% PSU
    atEDSUsToRename <- AcousticPSU$EDSU_PSU$PSU %in% PSU
    
    AcousticPSU$Stratum_PSU$PSU[atPSUsToRename] <- newPSUName
    AcousticPSU$EDSU_PSU$PSU[atEDSUsToRename] <- newPSUName
    
    # Format the output:
    RstoxBase::formatOutput(AcousticPSU, dataType = "AcousticPSU", keep.all = FALSE)
    
    # Set the AcousticPSU back to the process data of the process:
    setProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        argumentValue = list(AcousticPSU) # We need to list this to make it correspond to the single value of the argumentName parameter.
    )
    
    # Revert the active process ID to the previous process:
    resetModel(projectPath = projectPath, modelName = modelName, processID = processID, processDirty = TRUE, deleteCurrent = TRUE)
    
    # Return the active process:
    activeProcess <- getActiveProcess(projectPath = projectPath, modelName = modelName)
    return(
        list(
            PSU = PSU, 
            activeProcess = activeProcess, 
            saved = isSaved(projectPath)
        )
    )
}
#' 
#' @export
#' @rdname AcousticPSU
#' 
addEDSU <- function(PSU, EDSU, projectPath, modelName, processID) {
    
    # Check that the process returns AcousticPSU process data:
    checkDataType("AcousticPSU", projectPath = projectPath, modelName = modelName, processID = processID)
    
    # Get the process data of the process, a table of PSU, Layer, AssignmentID, Haul and HaulWeight:
    AcousticPSU <- getProcessData(projectPath = projectPath, modelName = modelName, processID = processID, check.activeProcess = TRUE)
    
    # Set the PSU column for the given EDSUs:
    atEDSUs <- AcousticPSU$EDSU_PSU$EDSU %in% EDSU
    AcousticPSU$EDSU_PSU[atEDSUs, PSU := ..PSU]
    
    # Format the output:
    RstoxBase::formatOutput(AcousticPSU, dataType = "AcousticPSU", keep.all = FALSE)
    
    # Set the AcousticPSU back to the process data of the process:
    setProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        argumentValue = list(AcousticPSU) # We need to list this to make it correspond to the single value of the argumentName parameter.
    )
    
    # Revert the active process ID to the previous process:
    resetModel(projectPath = projectPath, modelName = modelName, processID = processID, processDirty = TRUE, deleteCurrent = TRUE)
    
    # Return the active process:
    activeProcess <- getActiveProcess(projectPath = projectPath, modelName = modelName)
    return(
        list(
            activeProcess = activeProcess, 
            saved = isSaved(projectPath)
        )
    )
}
#' 
#' @export
#' @rdname AcousticPSU
#' 
removeEDSU <- function(EDSU, projectPath, modelName, processID) {
    
    # Check that the process returns AcousticPSU process data:
    checkDataType("AcousticPSU", projectPath = projectPath, modelName = modelName, processID = processID)
    
    # Get the process data of the process, a table of PSU, Layer, AssignmentID, Haul and HaulWeight:
    AcousticPSU <- getProcessData(projectPath = projectPath, modelName = modelName, processID = processID, check.activeProcess = TRUE)
    
    # Set the PSU column to empty string for the given EDSUs:
    atEDSUs <- AcousticPSU$EDSU_PSU$EDSU %in% EDSU
    AcousticPSU$EDSU_PSU[atEDSUs, PSU := NA]
    
    # Format the output:
    RstoxBase::formatOutput(AcousticPSU, dataType = "AcousticPSU", keep.all = FALSE)
    
    ## Add the acsoutic PSU:
    #EDSUsToKeep <- !AcousticPSU$PSU_EDSU$EDSU %in% EDSU
    #
    #AcousticPSU$PSU_EDSU <- AcousticPSU$PSU_EDSU[EDSUsToKeep, ]
    
    # Set the AcousticPSU back to the process data of the process:
    setProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        argumentValue = list(AcousticPSU) # We need to list this to make it correspond to the single value of the argumentName parameter.
    )
    
    # Revert the active process ID to the previous process:
    resetModel(projectPath = projectPath, modelName = modelName, processID = processID, processDirty = TRUE, deleteCurrent = TRUE)
    
    # Return the active process:
    activeProcess <- getActiveProcess(projectPath = projectPath, modelName = modelName)
    return(
        list(
            activeProcess = activeProcess, 
            saved = isSaved(projectPath)
        )
    )
}
#' 
#' @export
#' @rdname AcousticPSU
#' 
removeAllAcousticPSUsOfStratum <- function(Stratum, projectPath, modelName, processID) {
    
    # Check that the process returns AcousticPSU process data:
    checkDataType("AcousticPSU", projectPath = projectPath, modelName = modelName, processID = processID)
    
    # Get the process data of the process:
    AcousticPSU <- getProcessData(projectPath = projectPath, modelName = modelName, processID = processID, check.activeProcess = TRUE)
    
    # Remove the acoustic PSUs:
    atPSUsToRemoveInStratum_PSU <- AcousticPSU$Stratum_PSU$Stratum %in% Stratum
    PSUsToRemove <- subset(AcousticPSU$Stratum_PSU, atPSUsToRemoveInStratum_PSU)$PSU
    atPSUsToSetToNAInEDSU_PSU <- AcousticPSU$EDSU_PSU$PSU %in% PSUsToRemove
    
    
    AcousticPSU$Stratum_PSU <- AcousticPSU$Stratum_PSU[!atPSUsToRemoveInStratum_PSU, ]
    AcousticPSU$EDSU_PSU[atPSUsToSetToNAInEDSU_PSU, PSU := NA]
    
    # Format the output:
    RstoxBase::formatOutput(AcousticPSU, dataType = "AcousticPSU", keep.all = FALSE)
    
    # Set the AcousticPSU back to the process data of the process:
    setProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        argumentValue = list(AcousticPSU) # We need to list this to make it correspond to the single value of the argumentName parameter.
    )
    
    # Revert the active process ID to the previous process:
    resetModel(projectPath = projectPath, modelName = modelName, processID = processID, processDirty = TRUE, deleteCurrent = TRUE)
    
    # Return the active process:
    activeProcess <- getActiveProcess(projectPath = projectPath, modelName = modelName)
    return(
        list(
            activeProcess = activeProcess, 
            saved = isSaved(projectPath)
        )
    )
}





############################################################
############################################################
#' Stratum manipulation
#' 
#' The functions \code{addStations} and \code{removeStations} adds or removes biotic stations from the BioticAssignment process data of the specified process.
#' 
#' @details 
#' The assignment IDs are refreshed for every change, after sorting the assignemnts by the PSU column.
#' 
#' @inheritParams getProcessOutput
#' @param stratum The SpatialPolygonsDataFrame object defining the stratum to add.
#' @param stratumName The name of the stratum.
#' @param newStratumName The new name of the stratum.
#' 
#' @name Stratum
#' 
NULL
#' 
#' @export
#' @rdname Stratum
#' 
addStratum <- function(stratum, projectPath, modelName, processID) {
    
    # Check that the process returns StratumPolygon process data:
    checkDataType("StratumPolygon", projectPath = projectPath, modelName = modelName, processID = processID)
    
    # Get the process data of the process, a table of PSU, Layer, AssignmentID, Haul and HaulWeight:
    StratumPolygon <- getProcessData(projectPath = projectPath, modelName = modelName, processID = processID, check.activeProcess = TRUE)
    
    # If given as a GeoJSON string, parse to a SpatialPolygonsDataFrame object:
    if(is.character(stratum)) {
        #stratum <- geojsonio::geojson_sp(geojsonio::as.json(stratum))
        #stratum <- rgdal::readOGR(stratum, stringsAsFactors = FALSE)
        stratum <- sf::as_Spatial(geojsonsf::geojson_sf(stratum))
        
        stratum <- copyStratumNameToID(stratum)
        # Add "x", "y" as column names of the coords, since readOGR() does not do this:
        stratum <- addCoordsNames(stratum)
        # added by aasmund: Set projection to empty by default, rbind will then work.
        #stratum@proj4string <- sp::CRS()
        suppressWarnings(sp::proj4string(stratum) <- RstoxBase::getRstoxBaseDefinitions("proj4string_longlat"))
        #suppressWarnings(sp::proj4string(stratum) <- RstoxBase::getRstoxBaseDefinitions("proj4string"))
    }
    
    # Add the new strata, but check that the stratum names are not in use:
    usedStratumNames <- intersect(
        RstoxBase::getStratumNames(stratum), 
        RstoxBase::getStratumNames(StratumPolygon$StratumPolygon)
    )
    if(length(usedStratumNames)) {
        stop("StoX: The stratum name ", usedStratumNames, " already exist. Choose a different name")
    }
    if(!length(RstoxBase::getStratumNames(stratum)) || !nchar(RstoxBase::getStratumNames(stratum))) {
        stop("StoX: The new stratum must have a name")
    }
    
    #toAdd <- list(Polygons(list(Polygon(coordinates)), ID = stratumName))
    #StratumPolygon$StratumPolygon@polygons <- c(
    #    StratumPolygon$StratumPolygon@polygons, 
    #    stratum@polygons
    #)
    
    # Avoid errors due to difference in projection, as an empty SpatialPolygonsDataFrame cannot have projection:
    if(length(StratumPolygon$StratumPolygon)) {
        StratumPolygon$StratumPolygon <- rbind(
            StratumPolygon$StratumPolygon, 
            stratum
        )
    }
    else {
        StratumPolygon$StratumPolygon <- stratum
    }
    
    
    # Set the StratumPolygon back to the process data of the process:
    setProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        argumentValue = list(StratumPolygon) # We need to list this to make it correspond to the single value of the argumentName parameter.
    )
    
    # Revert the active process ID to the previous process:
    resetModel(projectPath = projectPath, modelName = modelName, processID = processID, processDirty = TRUE, deleteCurrent = TRUE)
    
    # Return the active process:
    activeProcess <- getActiveProcess(projectPath = projectPath, modelName = modelName)
    return(
        list(
            activeProcess = activeProcess, 
            saved = isSaved(projectPath)
        )
    )
}
#' 
#' @export
#' @rdname Stratum
#' 
removeStratum <- function(stratumName, projectPath, modelName, processID) {
    
    # Check that the process returns StratumPolygon process data:
    checkDataType("StratumPolygon", projectPath = projectPath, modelName = modelName, processID = processID)
    
    # Get the process data of the process, a table of PSU, Layer, AssignmentID, Haul and HaulWeight:
    StratumPolygon <- getProcessData(projectPath = projectPath, modelName = modelName, processID = processID, check.activeProcess = TRUE)
    
    # Add the coordinates:
    # Modify the coordinates:
    atRemove <- match( 
        stratumName, 
        RstoxBase::getStratumNames(StratumPolygon$StratumPolygon)
    )
    if(!any(is.na(atRemove))) {
        #StratumPolygon$StratumPolygon@polygons <- StratumPolygon$StratumPolygon@polygons[-atRemove]
        StratumPolygon$StratumPolygon <- StratumPolygon$StratumPolygon[-atRemove, ]
    }
    else {
        processName <- getProcessNameFromProcessID(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID
        )
        warning("StoX: The stratum with name ", stratumName, " does not exist for the process ", processName, ". No stratum deleted.")
    }
    
    # Set the StratumPolygon back to the process data of the process:
    setProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        argumentValue = list(StratumPolygon) # We need to list this to make it correspond to the single value of the argumentName parameter.
    )
    
    # Remove PSUs in all subsequent PSU processes:
    #removePSUsByStratum(
    #    stratumName = stratumName, 
    #    projectPath = projectPath, 
    #    modelName = modelName, 
    #    processID = processID
    #)
    
    # Revert the active process ID to the previous process:
    resetModel(projectPath = projectPath, modelName = modelName, processID = processID, processDirty = TRUE, deleteCurrent = TRUE)
    
    # Return the active process:
    activeProcess <- getActiveProcess(projectPath = projectPath, modelName = modelName)
    return(
        list(
            activeProcess = activeProcess, 
            saved = isSaved(projectPath)
        )
    )
}
#' 
#' @export
#' @rdname Stratum
#' 
modifyStratum <- function(stratum, projectPath, modelName, processID) {
    
    # Check that the process returns StratumPolygon process data:
    checkDataType("StratumPolygon", projectPath = projectPath, modelName = modelName, processID = processID)
    
    # Get the process data of the process, a table of PSU, Layer, AssignmentID, Haul and HaulWeight:
    StratumPolygon <- getProcessData(projectPath = projectPath, modelName = modelName, processID = processID, check.activeProcess = TRUE)
    
    # If given as a GeoJSON string, parse to a SpatialPolygonsDataFrame object:
    if(is.character(stratum)) {
        #stratum <- geojsonio::geojson_sp(geojsonio::as.json(stratum))
        #stratum <- rgdal::readOGR(stratum)
        stratum <- sf::as_Spatial(geojsonsf::geojson_sf(stratum))
        
        # Add "x", "y" as column names of the coords, since readOGR() does not do this:
        stratum <- addCoordsNames(stratum)
    }
    
    # Modify the coordinates:
    if(length(RstoxBase::getStratumNames(stratum)) > 1) {
        stop("Only one stratum can be modified.")
    }
    
    atModify <- match( 
        RstoxBase::getStratumNames(stratum), 
        RstoxBase::getStratumNames(StratumPolygon$StratumPolygon)
    )
    if(!any(is.na(atModify))) {
        # Set ID of the modified strata:
        for(ind in seq_along(stratum@polygons)) {
            stratum@polygons[[ind]]@ID <- StratumPolygon$StratumPolygon@polygons[[atModify[ind]]]@ID
        }
        
        # Insert the modified strata
        StratumPolygon$StratumPolygon@polygons[atModify] <- stratum@polygons
    }
    else {
        processName <- getProcessNameFromProcessID(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID
        )
        warning("StoX: The stratum with name ", RstoxBase::getStratumNames(stratum), " does not exist for the process ", processName, ". No stratum modified.")
    }
    
    
    # Set the StratumPolygon back to the process data of the process:
    setProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        argumentValue = list(StratumPolygon) # We need to list this to make it correspond to the single value of the argumentName parameter.
    )
    
    # Revert the active process ID to the previous process:
    resetModel(projectPath = projectPath, modelName = modelName, processID = processID, processDirty = TRUE, deleteCurrent = TRUE)
    
    # Return the active process:
    activeProcess <- getActiveProcess(projectPath = projectPath, modelName = modelName)
    return(
        list(
            activeProcess = activeProcess, 
            saved = isSaved(projectPath)
        )
    )
}
#' 
#' @export
#' @rdname Stratum
#' 
renameStratum <- function(stratumName, newStratumName, projectPath, modelName, processID) {
    
    # Check that the process returns StratumPolygon process data:
    checkDataType("StratumPolygon", projectPath = projectPath, modelName = modelName, processID = processID)
    
    # Get the process data of the process, a table of PSU, Layer, AssignmentID, Haul and HaulWeight:
    StratumPolygon <- getProcessData(projectPath = projectPath, modelName = modelName, processID = processID, check.activeProcess = TRUE)
    
    # Add the coordinates:
    # Modify the coordinates:
    atRename <- match( 
        stratumName, 
        RstoxBase::getStratumNames(StratumPolygon$StratumPolygon)
    )
    if(!any(is.na(atRename))) {
        StratumPolygon$StratumPolygon$StratumName[atRename] <- newStratumName
    }
    else {
        processName <- getProcessNameFromProcessID(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID
        )
        warning("StoX: The stratum with name ", stratumName, " does not exist for the process ", processName, ". No stratum renamed.")
    }
    
    # Set the StratumPolygon back to the process data of the process:
    setProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        argumentValue = list(StratumPolygon) # We need to list this to make it correspond to the single value of the argumentName parameter.
    )
    
    # Remove PSUs in all subsequent PSU processes:
    #removePSUsByStratum(
    #    stratumName = stratumName, 
    #    projectPath = projectPath, 
    #    modelName = modelName, 
    #    processID = processID
    #)
    
    # Revert the active process ID to the previous process:
    resetModel(projectPath = projectPath, modelName = modelName, processID = processID, processDirty = TRUE, deleteCurrent = TRUE)
    
    # Return the active process:
    activeProcess <- getActiveProcess(projectPath = projectPath, modelName = modelName)
    return(
        list(
            activeProcess = activeProcess, 
            saved = isSaved(projectPath)
        )
    )
}


# Function to add colnames to the coords slot of a SpatialPolygonsDataFrame:
addCoordsNames <- function(stratum, names = c("x", "y")) {
    # Hack to change the names of the coords to "x" and "y":
    for(i in seq_along(stratum@polygons)) {
        for(j in seq_along(stratum@polygons[[i]]@Polygons)) {
            colnames(stratum@polygons[[i]]@Polygons[[j]]@coords) <- names
        }
    }
    
    return(stratum)
}

# Function to rename the IDs to the column StratumName of a SpatialPolygonsDataFrame:
copyStratumNameToID <- function(stratum) {
    
    # Get the polygon names and IDs:
    StratumName <- stratum$StratumName
    
    # Rename all IDs to the polygon names:
    for (ind in seq_along(stratum@polygons)) {
        stratum@polygons[[ind]]@ID <- StratumName[ind]
    }
    
    # Update rownames of the data slot:
    rownames(methods::slot(stratum, "data")) <- StratumName
    
    return(stratum)
}

## Function to rename the IDs to the column polygonNames of a SpatialPolygonsDataFrame:
#setEmptyID <- function(stratum) {
#    
#    # Get the polygon names and IDs:
#    polygonName <- stratum$polygonName
#    
#    # Rename all IDs to the polygon names:
#    for (ind in seq_along(stratum@polygons)) {
#        stratum@polygons[[ind]]@ID <- ""
#    }
#    
#    return(stratum)
#}



