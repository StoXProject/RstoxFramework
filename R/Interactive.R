
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

















# General function to modify PSUProcessData:
modifyPSUProcessData <- function(fun, ..., projectPath, modelName, processID, PSUType = c("Acoustic", "Biotic")) {
    
    # Get the PSUType:
    PSUType <- RstoxData::match_arg_informative(PSUType)
    dataType <- paste0(PSUType, "PSU")
    
    # Check that the process returns PSU process data:
    checkDataType(dataType, projectPath = projectPath, modelName = modelName, processID = processID)
    
    # Get the process data of the process:
    PSUProcessData <- getProcessData(projectPath = projectPath, modelName = modelName, processID = processID, check.activeProcess = TRUE)
    # Rename to SSU:
    PSUProcessData <- RstoxBase::renameSSULabelInPSUProcessData(PSUProcessData, PSUType = PSUType, reverse = TRUE)
    
    # Apply the modification function, which needs to have PSUProcessData as input and output:
    if("PSUType" %in% names(formals(fun))) {
        PSUProcessData <- fun(PSUProcessData, ..., PSUType = PSUType)
    }
    else {
        PSUProcessData <- fun(PSUProcessData, ...)
    }
    
    # Rename back from SSU:
    PSUProcessData <- RstoxBase::renameSSULabelInPSUProcessData(PSUProcessData, PSUType = PSUType, reverse = FALSE)
    
    # Format the output:
    RstoxBase::formatOutput(PSUProcessData, dataType = dataType, keep.all = FALSE)
    
    # Set the AcousticPSU back to the process data of the process:
    setProcessMemory(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        argumentName = "processData", 
        argumentValue = list(PSUProcessData) # We need to list this to make it correspond to the single value of the argumentName parameter.
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





# General functions to add, remove and rename PSUs, add and remove SSUs and remove all PSUs of a Stratum:
addPSU <- function(Stratum, PSU = NULL, projectPath, modelName, processID, PSUType = c("Acoustic", "Biotic")) {
    
    modifyPSUProcessData(
        addPSUToPSUProcessData, 
        Stratum = Stratum, PSU = PSU, # The arguments of the addPSUToPSUProcessData()
        projectPath = projectPath, modelName = modelName, processID = processID, PSUType = PSUType
    )
    
}

addPSUToPSUProcessData <- function(PSUProcessData, Stratum, PSU = NULL, PSUType) {
    # If the PSU is not given, use the default PSU name:
    if(length(PSU) == 0) {
        PSU <- getNewDefaultName(PSUProcessData$Stratum_PSU$PSU, prefix = RstoxBase::getRstoxBaseDefinitions("getPSUPrefix")(PSUType))
    }
    # Check whether the PSU already exists:
    if(any(PSUProcessData$Stratum_PSU$PSU == PSU)) {
        stop("StoX: The name of the ", PSUType, "PSU (", PSU, ") already exists.")
    }
    
    # Add the PSU:
    toAdd <- data.table::data.table(
        Stratum = Stratum, 
        PSU = PSU
    )
    PSUProcessData$Stratum_PSU <- rbind(
        PSUProcessData$Stratum_PSU, 
        toAdd
    )
    
    return(PSUProcessData)
}


removePSU <- function(PSU = NULL, projectPath, modelName, processID, PSUType = c("Acoustic", "Biotic")) {
    
    modifyPSUProcessData(
        removePSUFromPSUProcessData, 
        PSU = PSU, # The arguments of the removePSUFromPSUProcessData()
        projectPath = projectPath, modelName = modelName, processID = processID, PSUType = PSUType
    )
    
}

removePSUFromPSUProcessData <- function(PSUProcessData, PSU = NULL) {
    
    # Remove the PSUs:
    PSUsToKeepInStratum_PSU <- !PSUProcessData$Stratum_PSU$PSU %in% PSU
    PSUsToSetToNAInSSU_PSU <- PSUProcessData$SSU_PSU$PSU %in% PSU
    
    PSUProcessData$Stratum_PSU <- PSUProcessData$Stratum_PSU[PSUsToKeepInStratum_PSU, ]
    PSUProcessData$SSU_PSU[PSUsToSetToNAInSSU_PSU, PSU := NA]
    
    return(PSUProcessData)
}


renamePSU <- function(PSU, newPSUName, projectPath, modelName, processID, PSUType = c("Acoustic", "Biotic")) {
    
    modifyPSUProcessData(
        renamePSUInPSUProcessData, 
        PSU = PSU, newPSUName = newPSUName, # The arguments of the renamePSUInPSUProcessData()
        projectPath = projectPath, modelName = modelName, processID = processID, PSUType = PSUType
    )
    
}

renamePSUInPSUProcessData <- function(PSUProcessData, PSU, newPSUName) {
    
    # Rename the PSU:
    atPSUsToRename <- PSUProcessData$Stratum_PSU$PSU %in% PSU
    atSSUsToRename <- PSUProcessData$SSU_PSU$PSU %in% PSU
    
    PSUProcessData$Stratum_PSU$PSU[atPSUsToRename] <- newPSUName
    PSUProcessData$SSU_PSU$PSU[atSSUsToRename] <- newPSUName
    
    return(PSUProcessData)
}


addSSU <- function(PSU, SSU, projectPath, modelName, processID, PSUType = c("Acoustic", "Biotic")) {
    
    modifyPSUProcessData(
        addSSUToPSUProcessData, 
        PSU = PSU, SSU = SSU, # The arguments of the addSSUToPSUProcessData()
        projectPath = projectPath, modelName = modelName, processID = processID, PSUType = PSUType
    )
    
}

addSSUToPSUProcessData <- function(PSUProcessData, PSU, SSU) {
    
    # Set the PSU column for the given SSUs:
    atSSUs <- PSUProcessData$SSU_PSU$SSU %in% SSU
    PSUProcessData$SSU_PSU[atSSUs, PSU := ..PSU]
    
    return(PSUProcessData)
}


removeSSU <- function(PSU, SSU, projectPath, modelName, processID, PSUType = c("Acoustic", "Biotic")) {
    
    modifyPSUProcessData(
        removeSSUFromPSUProcessData, 
        PSU = PSU, SSU = SSU, # The arguments of the removeSSUFromPSUProcessData()
        projectPath = projectPath, modelName = modelName, processID = processID, PSUType = PSUType
    )
    
}

removeSSUFromPSUProcessData <- function(PSUProcessData, PSU, SSU) {
    
    # Set the PSU column to empty string for the given SSUs:
    atSSUs <- PSUProcessData$SSU_PSU$SSU %in% SSU
    PSUProcessData$SSU_PSU[atSSUs, PSU := NA]
    
    return(PSUProcessData)
}


removeAllPSUsOfStratum <- function(Stratum, projectPath, modelName, processID, PSUType = c("Acoustic", "Biotic")) {
    
    modifyPSUProcessData(
        removeAllPSUsOfStratumFromPSUProcessData, 
        Stratum, # The arguments of the removeAllPSUsOfStratumFromPSUProcessData()
        projectPath = projectPath, modelName = modelName, processID = processID, PSUType = PSUType
    )
    
}

removeAllPSUsOfStratumFromPSUProcessData <- function(PSUProcessData, Stratum) {
    
    # Remove the acoustic PSUs:
    atPSUsToRemoveInStratum_PSU <- PSUProcessData$Stratum_PSU$Stratum %in% Stratum
    PSUsToRemove <- subset(PSUProcessData$Stratum_PSU, atPSUsToRemoveInStratum_PSU)$PSU
    atPSUsToSetToNAInSSU_PSU <- PSUProcessData$SSU_PSU$PSU %in% PSUsToRemove
    
    
    PSUProcessData$Stratum_PSU <- PSUProcessData$Stratum_PSU[!atPSUsToRemoveInStratum_PSU, ]
    PSUProcessData$SSU_PSU[atPSUsToSetToNAInSSU_PSU, PSU := NA]
    
    return(PSUProcessData)
}
















############################################################
############################################################
#' Add, remove and renamme acoustic PSUs and EDSUs
#' 
#' The functions \code{addAcousticPSU}, \code{removeAcousticPSU} and \code{renameAcousticPSU} adds, removes or renames AcousticPSU from the AcousticPSU process data of the specified process. 
#' The functions \code{addEDSU} and \code{removeEDSU} adds or removes EDSUSs from the AcousticPSU process data of the specified process. 
#' The function \code{removeAllAcousticPSUsOfStratum} removes all PSUs from the AcousticPSU process data of the specified process. 
#' 
#' @param Stratum The name of a stratum.
#' @param PSU The name of a PSU.
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
    
    addPSU(
        Stratum = Stratum, PSU = PSU, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        PSUType = "Acoustic"
    )
    
}
#' 
#' @export
#' @rdname AcousticPSU
#' 
removeAcousticPSU <- function(PSU, projectPath, modelName, processID) {
    
    removePSU(
        PSU = PSU, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        PSUType = "Acoustic"
    )
    
}
#' 
#' @export
#' @rdname AcousticPSU
#' 
addEDSU <- function(PSU, EDSU, projectPath, modelName, processID) {
    
    addSSU(
        PSU = PSU, SSU = EDSU, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        PSUType = "Acoustic"
    )
    
}
#' 
#' @export
#' @rdname AcousticPSU
#' 
removeEDSU <- function(EDSU, projectPath, modelName, processID) {
    
    removeSSU(
        PSU = PSU, SSU = EDSU, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        PSUType = "Acoustic"
    )
    
}

# Not currenly used by the GUI. Today it is not possible to rename a PSU:
renameAcousticPSU <- function(PSU, newPSUName, projectPath, modelName, processID) {
    
    renamePSU(
        PSU = PSU, newPSUName = newPSUName, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        PSUType = "Acoustic"
    )
    
}
# Not currently used by the GUI, but intended for new delete stratum functionality:
removeAllAcousticPSUsOfStratum <- function(Stratum, projectPath, modelName, processID) {
    
    removeAllPSUsOfStratum(
        Stratum = Stratum, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        PSUType = "Acoustic"
    )
    
}











############################################################
############################################################
#' Add, remove and renamme biotic PSUs and Stations
#' 
#' The functions \code{addBioticPSU}, \code{removeBioticPSU} and \code{renameBioticPSU} adds, removes or renames BioticPSUs from the BioticPSU process data of the specified process. 
#' The functions \code{addStation} and \code{removeStation} adds or removes Stations from the BioticPSU process data of the specified process. 
#' The function \code{removeAllBioticPSUsOfStratum} removes all PSUs from the BioticPSU process data of the specified process. 
#' 
#' @param Stratum The name of a stratum.
#' @param PSU The name of a PSU.
#' @param Station The name of a biotic station.
#' 
#' @details 
#' The assignment IDs are refreshed for every change, after sorting the assignemnts by the PSU column.
#' 
#' @inheritParams getProcessOutput
#' @name BioticPSU
#' 
NULL
#' 
#' @export
#' @rdname BioticPSU
#' 
addBioticPSU <- function(Stratum, PSU = NULL, projectPath, modelName, processID) {
    
    addPSU(
        Stratum = Stratum, PSU = PSU, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        PSUType = "Biotic"
    )
    
}
#' 
#' @export
#' @rdname BioticPSU
#' 
removeBioticPSU <- function(PSU, projectPath, modelName, processID) {
    
    removePSU(
        PSU = PSU, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        PSUType = "Biotic"
    )
    
}
#' 
#' @export
#' @rdname BioticPSU
#' 
addStation <- function(PSU, Station, projectPath, modelName, processID) {
    
    addSSU(
        PSU = PSU, SSU = Station, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        PSUType = "Biotic"
    )
    
}
#' 
#' @export
#' @rdname BioticPSU
#' 
removeStation <- function(Station, projectPath, modelName, processID) {
    
    removeSSU(
        PSU = PSU, SSU = Station, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        PSUType = "Biotic"
    )
    
}


# Not currenly used by the GUI. Today it is not possible to rename a PSU:
renameBioticPSU <- function(PSU, newPSUName, projectPath, modelName, processID) {
    
    renamePSU(
        PSU = PSU, newPSUName = newPSUName, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        PSUType = "Biotic"
    )
    
}
# Not currently used by the GUI, but intended for new delete stratum functionality:
removeAllBioticPSUsOfStratum <- function(Stratum, projectPath, modelName, processID) {
    
    removeAllPSUsOfStratum(
        Stratum = Stratum, 
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        PSUType = "Biotic"
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
        #stratum <- sf::as_Spatial(geojsonsf::geojson_sf(stratum))
        stratum <- geojsonsf::geojson_sf(stratum)
        
        #stratum <- copyStratumNameToID(stratum)
        # Add "x", "y" as column names of the coords, since readOGR() does not do this:
        #stratum <- addCoordsNames(stratum)
        # added by aasmund: Set projection to empty by default, rbind will then work.
        #stratum@proj4string <- sp::CRS()
        #suppressWarnings(sp::proj4string(stratum) <- RstoxBase::getRstoxBaseDefinitions("proj4string_longlat"))
        #suppressWarnings(sp::proj4string(stratum) <- RstoxBase::getRstoxBaseDefinitions("proj4string"))
        sf::st_crs(stratum) <- RstoxBase::getRstoxBaseDefinitions("proj4string_longlat")
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
        #stratum <- sf::as_Spatial(geojsonsf::geojson_sf(stratum))
        stratum <- geojsonsf::geojson_sf(stratum)
        
        # Add "x", "y" as column names of the coords, since readOGR() does not do this:
        #stratum <- addCoordsNames(stratum)
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
        ## Set ID of the modified strata:
        #for(ind in seq_along(stratum@polygons)) {
        #    stratum@polygons[[ind]]@ID <- StratumPolygon$StratumPolygon@polygons[[atModify[ind]]]@ID
        #}
        #
        ## Insert the modified strata
        #StratumPolygon$StratumPolygon@polygons[atModify] <- stratum@polygons
        
        StratumPolygon$StratumPolygon$geometry[atModify] <- stratum$geometry
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


# # Function to add colnames to the coords slot of a SpatialPolygonsDataFrame:
# addCoordsNames <- function(stratum, names = c("x", "y")) {
#     # Hack to change the names of the coords to "x" and "y":
#     for(i in seq_along(stratum@polygons)) {
#         for(j in seq_along(stratum@polygons[[i]]@Polygons)) {
#             colnames(stratum@polygons[[i]]@Polygons[[j]]@coords) <- names
#         }
#     }
#     
#     return(stratum)
# }

## Function to rename the IDs to the column StratumName of a SpatialPolygonsDataFrame:
#copyStratumNameToID <- function(stratum) {
#    
#    # Get the polygon names and IDs:
#    StratumName <- stratum$StratumName
#    
#    # Rename all IDs to the polygon names:
#    for (ind in seq_along(stratum@polygons)) {
#        stratum@polygons[[ind]]@ID <- StratumName[ind]
#    }
#    
#    # Update rownames of the data slot:
#    rownames(methods::slot(stratum, "data")) <- StratumName
#    
#    return(stratum)
#}

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



