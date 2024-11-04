# To install Rstox-packages like in the GUI, run something like the following:
#source ("https://raw.githubusercontent.com/StoXProject/RstoxFramework/master/R/Versions.R")
#installOfficialRstoxPackagesWithDependencies("3.4.0")

# 




#' Get certified Rstox package versions 
#' 
#' This function reads the OfficialRstoxFrameworkVersions.txt file for the given StoX GUI version.
#'
#' @inheritParams readOfficialRstoxPackageVersionsFile
#' @param StoXVersion The version of the StoX GUI defining the combination of official Rstox package versions.
#' @param toJSON Logical: If TRUE output a JSON string.
#' @param list.out Logical: If TRUE wrap the output of \code{getOfficialRstoxPackageVersion} in a list with packageName and version.
#' 
#' @export
#'
getOfficialRstoxPackageVersion <- function(
    StoXVersion = NULL, 
    officialRstoxPackageVersionsFile = system.file("versions", "OfficialRstoxFrameworkVersions.txt", package = "RstoxFramework"), 
    optionalDependencies = FALSE, 
    toJSON = FALSE, 
    list.out = FALSE
) {
    
    # Read the officialRstoxPackageVersionsFile:
    official <- readOfficialRstoxPackageVersionsFile(officialRstoxPackageVersionsFile)
    
    if(length(official)) {
        # Get rows of RstoxFrameworkVersions within the minimum and maximum StoXGUI version:
        if(length(StoXVersion)) {
            if(!StoXVersion %in% official$StoX) {
                stop("StoX GUI version ", StoXVersion, " not present in the file ", officialRstoxPackageVersionsFile, ".")
            }
            official <- subset(official, StoX == StoXVersion)
        }
        # If StoXVersion is not given, we use the versions of the last row of the officialRstoxPackageVersionsFile, which gives the latest official 
        else {
            official <- utils::tail(official, 1)
        }
        
        # Split the Dependencies:
        officialDependencies <- strsplit(official$Dependencies, "[,]")[[1]]
        officialDependencies <- extractPackageNameAsNames(officialDependencies)
        packageVersionList <- c(list(RstoxFramework = official$RstoxFramework), officialDependencies)
        
        # Split the optional dependencies:
        if(length(official$OptionalDependencies) && !isFALSE(optionalDependencies)) {
            officialOptionalDependencies <- strsplit(official$OptionalDependencies, "[,]")[[1]]
            officialOptionalDependencies <- extractPackageNameAsNames(officialOptionalDependencies)
            #if(!isFALSE(optionalDependencies)) {
                toKeep <- names(officialOptionalDependencies) %in% utils::installed.packages()[, "Package"]
                officialOptionalDependencies <- subset(officialOptionalDependencies, toKeep)
            #}
            packageVersionList <- c(packageVersionList, officialOptionalDependencies)
        }
        
        if(list.out) {
            packageVersions <- list(
                packageName = names(packageVersionList), 
                version = unlist(packageVersionList)
            )
        }
        else {
            packageVersions <- getPackageNameAndVersionString(packageVersionList)
        }
        # Add the StoX version as an attribute:
        attr(packageVersions, "StoX") <- official$StoX
        attr(packageVersions, "Official") <- official$Official
        
        if(toJSON) {
            packageVersions <- vector2json(packageVersions)
        }
        
        return(packageVersions)
    }
    else {
        return(NULL)
    }
}




#' Get certified Rstox package versions 
#' 
#' This function reads the OfficialRstoxFrameworkVersions.txt file for the given StoX GUI version.
#'
#' @param officialRstoxPackageVersionsFile The path to the file holding the link between StoX GUI version and Rstox package versions. If missing, the file on the RstoxFramework master on GitHub is used ("https://raw.githubusercontent.com/StoXProject/RstoxFramework/master/inst/versions/OfficialRstoxFrameworkVersions.txt").
#' @param optionalDependencies Logical: If TRUE include also the column OptionalDependencies from the OfficialRstoxFrameworkVersions.txt file, which holds Rstox packages used in the Suggests field of the DESCRIPTION file.
#' @param toTable Logical: If TRUE output a table.
#' 
#' @export
#'
readOfficialRstoxPackageVersionsFile <- function(officialRstoxPackageVersionsFile, optionalDependencies = FALSE, toTable = FALSE) {
    # Get the file name:
    if(missing(officialRstoxPackageVersionsFile) || !length(officialRstoxPackageVersionsFile)) {
        officialRstoxPackageVersionsFile = system.file("versions", "OfficialRstoxFrameworkVersions.txt", package = "RstoxFramework")
    }
       
    # Read the officialRstoxPackageVersionsFile:
    official <- tryCatch(
        utils::read.table(
            officialRstoxPackageVersionsFile, 
            header = TRUE, 
            stringsAsFactors = FALSE, 
            sep = "\t"
        ), 
        error = function(e) {
            NULL
        }
    )
    
    if(toTable) {
        officialDependencies <- strsplit(official$Dependencies, "[,]")
        officialDependencies <- lapply(officialDependencies, extractPackageNameAsNames)
        officialDependencies <- data.table::rbindlist(officialDependencies)
        
        if(length(official$OptionalDependencies) && !isFALSE(optionalDependencies)) {
            officialOptionalDependencies <- lapply(official$OptionalDependencies, strsplit, "[,]")
            officialOptionalDependencies <- lapply(official$OptionalDependencies, "[[", 1)
            officialOptionalDependencies <- lapply(officialOptionalDependencies, extractPackageNameAsNames)
            areEmptyString <- !sapply(officialOptionalDependencies, nzchar)
            if(any(areEmptyString)) {
                officialOptionalDependencies[areEmptyString] <- rep(list(list(NA)), sum(areEmptyString))
            }
            
            officialOptionalDependencies <- data.table::rbindlist(officialOptionalDependencies, fill = TRUE)
            
            
            #if(!isFALSE(optionalDependencies)) {
                keep <- names(officialOptionalDependencies) %in% utils::installed.packages()[, "Package"]
                officialOptionalDependencies <- subset(officialOptionalDependencies, select = keep)
            #}
            officialDependencies <- cbind(officialDependencies, officialOptionalDependencies)
        }
        
        official <- data.table::data.table(
            StoX = official$StoX, 
            RstoxFramework = official$RstoxFramework, 
            officialDependencies,
            Official = official$Official
        )
    }
    
    return(official)
}

## Small function to extract package name from strings:
#extractPackageName <- function(x) {
#    x <- strsplit(x, "[_]")
#    x <- sapply(x, "[", 1)
#    return(x)
#}

#' Small function to parse the string defining officical Rstox-package versions (for each RstoxFramwork):
#' 
#' @param packageVersionString Character: A vector of strings of the form PACKAGENAME_PACKAGEVERSION (e.g. "RstoxFramework_3.6.1") to be converted into a list of versions named by the package names.
#' 
#' @export
#' 
extractPackageNameAsNames <- function(packageVersionString) {
    if(!length(packageVersionString) || !sum(nchar(packageVersionString))) {
        return(packageVersionString)
    }
    packageVersionList <- strsplit(packageVersionString, "[_]")
    packageVersionList <- structure(lapply(packageVersionList, "[", 2), names = sapply(packageVersionList, "[", 1))
    
    return(packageVersionList)
}

getPackageNameAndVersionString <- function(packageName, version, sep = "_") {
    if(is.list(packageName)) {
        version <- packageName
        packageName <- names(packageName)
    }
    paste(packageName, version, sep = sep)
}








isOfficialRstoxFrameworkVersion <- function() {
    officialRstoxFrameworkVersion <- attr(getOfficialRstoxPackageVersion(), "Official")
    return(officialRstoxFrameworkVersion)
}





#' Get package status of an Rstox package
#' 
#' @param RstoxPackageName The name of the Rstox package.
#' @inheritParams getOfficialRstoxPackageVersion
#' 
#' @export
#' 
RstoxPackageStatus <- function(RstoxPackageName, StoXVersion, officialRstoxPackageVersionsFile) {
    
    # This function returns a status of an Rstox package that is 
    # 3: Package not installed
    # 2: Package is not an official RstoxPackage
    # 1: Package version is not certified
    # 0: Package version IS certified 
    
    # Read the officialRstoxPackageVersionsFile and select the row given by the StoXVersion:
    certifiedTable <- readOfficialRstoxPackageVersionsFile(officialRstoxPackageVersionsFile, optionalDependencies = TRUE, toTable = TRUE)
    RstoxPackageColumnNames <- startsWith(names(certifiedTable), "Rstox")
    certifiedRow <- subset(certifiedTable, certifiedTable$StoX == StoXVersion, select = RstoxPackageColumnNames)
    
    # Check whether the packages are installed.:
    isInstalled <- nzchar(system.file(package = RstoxPackageName))
    if(!isInstalled) {
        return(3)
    }
    
    # Get the installed version:
    currentVersion <- getVersionStringOfPackage(RstoxPackageName)
    
    # Compare to the certified versions:
    if(! RstoxPackageName %in% names(certifiedRow)) {
        return(2)
    }
    
    certifiedVersion <- certifiedRow[[RstoxPackageName]]
    
    status <- as.numeric(certifiedVersion != currentVersion)
    
    return(status)
}




getVersionStringOfPackage <- function(packageName) {
    pkgs <- utils::installed.packages()
    vers <- pkgs[, "Version"]
    vers[packageName]
}


vector2json <- function(x) {
    paste0("[", paste(sapply(x, deparse), collapse = ","), "]")
}

