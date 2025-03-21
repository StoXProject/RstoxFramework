#' Function for comparing existing output files with the memory read using runProject()
#' 
#' @inheritParams readModelData
#' @inheritParams runProcesses
#' @inheritParams compareProjectToStoredOutputFiles
#' @param projectPaths The projects to be run and tested against the existing output files of the project gievn by \code{projectPath_original}.
#' @param projectPaths_original The projects holding the existing output files, defaulted to \code{projectPath}.
#' 
#' @export
#'
compareProjectToStoredOutputFilesAll <- function(projectPaths, projectPaths_original = projectPaths, emptyStringAsNA = FALSE, intersect.names = TRUE, ignore.variable = NULL, skipNAFraction = FALSE, skipNAAt = FALSE, NAReplacement = NULL, classOf = c("first", "second"), try = TRUE, data.out = FALSE, ...) {
    
    out <- mapply(
        compareProjectToStoredOutputFiles, 
        projectPath = projectPaths, 
        projectPath_original = projectPaths_original, 
        MoreArgs = list(
            emptyStringAsNA = emptyStringAsNA, 
            intersect.names = intersect.names, 
            ignore.variable = ignore.variable, 
            skipNAFraction = skipNAFraction, 
            skipNAAt = skipNAAt, 
            NAReplacement = NAReplacement, 
            classOf = classOf, 
            try = try, 
            data.out = data.out, 
            ...
        ), 
        SIMPLIFY = FALSE
    )
    
    if(data.out) {
        outNames <- names(out[[1]])
        out <- lapply(outNames, function(name) lapply(out, "[[", name))
        names(out) <- outNames
    }
    
    
    return(out)
}


#' Function for comparing existing output files with the memory read using runProject()
#' 
#' @inheritParams readModelData
#' @inheritParams runProcesses
#' @inheritParams general_arguments
#' @inheritParams readBootstrapData
#' @inheritParams readMemoryFile
#' @param projectPath_original The project holding the existing output files, defaulted to \code{projectPath}.
#' @param intersect.names Logical: If TRUE, compare only same named columns.
#' @param ignore.variable Either a vector of names of variables/columns to ignore in the comparison, or a list of such variables per process (named by the process names).
#' @param ignore.process A vector of names of processes to ignore in the comparison.
#' @param ignore.process.variable A list of variables to ignore for different processes, named by the processes, e.g. list(MeanNASC = c("MinLayerDepth", "MaxLayerDepth"))
#' @param skipNAFraction Logical: If TRUE, skip rows with more than 50 percent NAs. Can also be set to a numeric value between 0 and 1.
#' @param skipNAAt A vector of strings naming the columns in which NA values identifies rows to skip. If more than one variable is given and more than one of these are present in a table, all rows where at least one of the variables are NA are skipped. Note that this may reduce the number of rows and may results in diffs for that reason. Using this option is best used in combination with \code{mergeWhenDifferentNumberOfRows}.
#' @param NAReplacement List of replacement values for different classes of NA, applied after any merging as to incorporate NAs generated during merging.
#' @param ignoreEqual Logical: If TRUE, ignore columns where all values are equal.
#' @param classOf Character string specifying whether to compare after converting to the class of the first or second table. Set this to "first" (default) to convert class to the original data.
#' @param data.out Logical, if TRUE output the original and new data along with the tests. \code{data.out} = NULL implies \code{data.out} = FALSE if no difference was found and  \code{data.out} = TRUE otherwise.
#' @param sort Logical, if TRUE sort the tables before all.equal. When  mergeWhenDifferentNumberOfRows = TRUE the tables are always sorted.
#' @param compareReports Logical, if TRUE compare the report specifically (old method kept for robustness).
#' @param checkOutputFiles Logical, if TRUE compare the file names of the output files.
#' @param tolerance The tolerance to use in all.equal.
#' @param debug Logical: If TRUE, interrupt the execution just before priting the test results.
#' @param check.columnNames_identical Logical: If TRUE, test that the column names are identical between corresponding tables of the original and new data.
#' @param testAllTRUE Logical: If FALSE, only test differences between common rows between tables that are compared by merging. If TRUE the function reports test failures when the extra rows from merging wih all = TRUE contains differences.
#' @param ... Used in runModel().
#' 
#' @export
#'
compareProjectToStoredOutputFiles <- function(
    projectPath, projectPath_original = projectPath, 
    emptyStringAsNA = FALSE, intersect.names = TRUE, 
    ignore.variable = NULL, ignore.process = NULL, ignore.process.variable = NULL, 
    skipNAFraction = FALSE, skipNAAt = NULL, NAReplacement = NULL, 
    ignoreEqual = FALSE, classOf = c("first", "second"), 
    try = TRUE, data.out = FALSE, 
    #mergeWhenDifferentNumberOfRows = FALSE, 
    sort = TRUE, 
    compareReports = FALSE, checkOutputFiles = TRUE, 
    returnBootstrapData = FALSE, selection = list(), BootstrapID = NA, unlistSingleTable = TRUE, 
    tolerance = sqrt(.Machine$double.eps), debug = FALSE, save = FALSE, check.columnNames_identical = FALSE, testAllTRUE = FALSE, 
    ...
) {
    
    # Unzip if zipped:
    if(tolower(tools::file_ext(projectPath)) == "zip") {
        projectPath <- unzipProject(projectPath, exdir = tempdir())
    }
    if(projectPath_original != projectPath && tolower(tools::file_ext(projectPath_original)) == "zip") {
        projectPath_original <- unzipProject(projectPath_original, exdir = tempdir())
    }
    
    # Run the test project:
    projectPath_copy <- file.path(tempdir(), paste0(basename(projectPath), "_copy"))
    temp <- copyProject(projectPath, projectPath_copy, ow = TRUE, close = TRUE, msg = FALSE)
    
    if(debug) {
        browser()
    }
    # Open the project:
    openProject(projectPath_copy)
    # Changed to using unlistDepth2 = FALSE‚  as this is in line with the bug fix from StoX 3.6.0 where outputs with multiple tables were no longer unlisted in Bootstrap data:
    #dat <- runProject(projectPath_copy, unlist.models = TRUE, drop.datatype = FALSE, unlistDepth2 = TRUE, close = TRUE, save = save, try = try, msg = FALSE, ...)
    dat <- runProject(projectPath_copy, unlist.models = TRUE, drop.datatype = FALSE, unlistDepth2 = FALSE, close = TRUE, save = save, try = try, returnBootstrapData = returnBootstrapData, selection = selection, BootstrapID = BootstrapID, unlistSingleTable = unlistSingleTable, ...)
    
    
    # Read the original data:
    #dat_orig <- readModelData(projectPath_original, unlist.models = TRUE)
    dat_orig <- readModelData(
        projectPath_original, unlist = 1, emptyStringAsNA = emptyStringAsNA, verifyFiles = TRUE, 
        returnBootstrapData = returnBootstrapData, selection = selection, BootstrapID = BootstrapID, unlistSingleTable = unlistSingleTable
    )
    
    # Reorder the original data to the order of the new data:
    newOrder <- c(intersect(names(dat), names(dat_orig)), setdiff(names(dat_orig), names(dat)))
    dat_orig <- dat_orig[newOrder]
    
    # Compare only those elemens common to the two datasets:
    processNames_present <- all(names(dat_orig) %in% names(dat))
    
    # Expect all column names:
    tableNames_identical <- list()
    columnNames_identical <- list()
    processesToCheck <- setdiff(names(dat_orig), ignore.process)
    
    # Store the file paths of the output files to compare to the new output file paths:
    if(checkOutputFiles) {
        # List the output files:
        outputDir <- file.path(projectPath_copy, "output")
        outputDirs <- list.dirs(outputDir)
        # Keep only the output folders of the processes to check:
        outputDirs <- outputDirs[basename(outputDirs) %in% processesToCheck]
        # List the files:
        outputFiles <- unlist(lapply(outputDirs, list.files, full.names = TRUE))
    }
    
    
    for(name in processesToCheck) {
        # Check identical table names: 
        tableNames_identical[[name]] <- identical(sort(names(dat_orig[[name]])), sort(names(dat[[name]]))) || all(names(dat_orig[[name]]) %in% names(dat[[name]]))
        # Check identical column names: 
        if(check.columnNames_identical)  {
            columnNames_identical[[name]] <- list()
            for(subname in names(dat_orig[[name]])) {
                columnNames_identical[[name]][[subname]] <- identical(names(dat_orig[[name]][[subname]]), names(dat[[name]][[subname]]))
            }
        }
    }
    
    # Check the actual data:
    data_equal <- list()
    diffData <- list()
    
    #if(debug) {
    #    browser()
    #}
    
    
    # Tests will fail for (1) strings "NA" that are written unquoted (as RstoxFramework do from objects of class data.table) and which are read as NA by data.table::fread, and (2) numbers stored as strings (e.g. software version numbers), which are strirpped of leading and trailing zeros by data.table::fread. Thus it is adivced to not compare CESAcocustic().
    for(name in processesToCheck) {
        data_equal[[name]] <- list()
        for(subname in names(dat_orig[[name]])) {
            if(data.table::is.data.table(dat_orig[[name]][[subname]])) {
                if(intersect.names) {
                    intersectingNames <- intersect(names(dat_orig[[name]][[subname]]), names(dat[[name]][[subname]]))
                    result <- compareDataTablesUsingClassOf(
                        dat_orig[[name]][[subname]][, intersectingNames, with = FALSE], 
                        dat[[name]][[subname]][, intersectingNames, with = FALSE], 
                        # Support ignoring specific variables of specific processes:
                        ignore.variable = c(ignore.variable, ignore.process.variable[[name]]), 
                        skipNAFraction = skipNAFraction, 
                        skipNAAt = if(is.list(skipNAAt)) skipNAAt[[name]] else skipNAAt, 
                        NAReplacement = NAReplacement, 
                        ignoreEqual = ignoreEqual,
                        classOf = classOf, 
                        #mergeWhenDifferentNumberOfRows = mergeWhenDifferentNumberOfRows, 
                        sort = sort, 
                        tolerance = tolerance, 
                        testAllTRUE = testAllTRUE
                    )
                    
                    data_equal[[name]][[subname]] <- result$warn
                    diffData[[name]][[subname]] <- result$diffData
                }
                else {
                    result <- compareDataTablesUsingClassOf(
                        dat_orig[[name]][[subname]], 
                        dat[[name]][[subname]], 
                        # Support ignoring specific variables of specific processes:
                        ignore.variable = c(ignore.variable, ignore.process.variable[[name]]), 
                        skipNAFraction = skipNAFraction, 
                        skipNAAt = if(is.list(skipNAAt)) skipNAAt[[name]] else skipNAAt, 
                        NAReplacement = NAReplacement, 
                        ignoreEqual = ignoreEqual, 
                        classOf = classOf, 
                        #mergeWhenDifferentNumberOfRows = mergeWhenDifferentNumberOfRows, 
                        sort = sort, 
                        tolerance = tolerance, 
                        testAllTRUE = testAllTRUE
                    )
                    
                    data_equal[[name]][[subname]] <- result$warn
                    diffData[[name]][[subname]] <- result$diffData
                }
                
                # This caused trouble when converting character to POSIXct:
                #data_equal[[name]][[subname]] <- compareDataTablesUsingClassOfSecond(dat_orig[[name]][[subname]], dat[[name]][[subname]])
            }
            else if("sf" %in% class(dat_orig[[name]][[subname]])){
                data_equal[[name]][[subname]] <- all.equal(
                    sf::st_coordinates(dat_orig[[name]][[subname]]), 
                    sf::st_coordinates(dat[[name]][[subname]])
                )
            }
            else {
                data_equal[[name]][[subname]] <- paste(all.equal(dat_orig[[name]][[subname]], dat[[name]][[subname]], check.attributes = FALSE, tolerance = tolerance), collapse = "\n")
                #all.equal(dat_orig[[name]][[subname]], dat[[name]][[subname]])
            }
        }
    }
    
    unlistDiff <- function(x) {
        unlist(x)[!unlist(x) == "TRUE"]
    }
    
    diffWarning <- function(x) {
        x_info <- unlistDiff(x)
        if(length(x_info)) {
            x_info <- paste0("\n", names(x_info), ":\n", x_info, collapse = "\n")
            msg <- paste0(
                "projectPath:\n", 
                projectPath, 
                "\n", 
                deparse(substitute(x)), 
                ":\n===========================================>>>>>\n", 
                "Project: ", projectPath, 
                "\n", 
                "Old project: ", projectPath_original, 
                x_info, 
                "\n<<<<<==========================================="
            )
            warning(msg)
            message(msg)
        }
    }
    diffWarning(data_equal)
    diffWarning(columnNames_identical)
    diffWarning(tableNames_identical)
    
    # Compare reports, but only numeric values:
    if(compareReports) {
        reports <- startsWith(names(dat_orig), "Report")
        reports_equal <- list()
        
        for(name in names(dat_orig)[reports]) {
            reports_equal[[name]] <- list()
            for(subname in names(dat_orig[[name]])) {
                reports_equal[[name]][[subname]] <- compareReport(
                    dat_orig[[name]][[subname]], 
                    dat[[name]][[subname]]
                )
            }
        }
        message("reports_equal")
        message(reports_equal)
    }
    
    
    
    allTests <- list(
        processNames_present = processNames_present,
        tableNames_identical = tableNames_identical,
        columnNames_identical = columnNames_identical,
        data_equal = data_equal#, 
        #reports_equal = reports_equal
    )
    if(compareReports) {
        allTests$reports_equal <- reports_equal
    }
    # Store the file paths of the output files to compare to the new output file paths:
    if(checkOutputFiles) {
        # List the output files:
        newOutputFiles <- unlist(lapply(outputDirs, list.files, full.names = TRUE))
        allTests$outputFileNames_equal <- outputFiles == newOutputFiles
    }
    
    
    ok <- all(unlist(allTests) %in% TRUE)
    if(!length(data.out)) {
        data.out <- !ok
    }
    
    out <- lapply(allTests, formatDiffs)
    
    
    if(data.out) {
        out <- list(
            dat = dat, 
            dat_orig = dat_orig, 
            test = out, 
            diffData = diffData
        )
        
        return(out)
    }
    
    
    if(!ok) {
        return(out)
    }
    else {
        return(TRUE)
    }
}




compareReport <- function(x, y, tolerance = sqrt(.Machine$double.eps)) {
    # Set all columns of character class to NA:
    setCharacterColumnsToNA(x)
    setCharacterColumnsToNA(y)
    setLogicalColumnsToNA(x)
    setLogicalColumnsToNA(y)
    #all.equal(x, y, check.attributes = FALSE)
    paste(all.equal(as.data.frame(x), as.data.frame(y), check.attributes = FALSE, tolerance = tolerance), collapse = "\n")
}

setCharacterColumnsToNA <- function(x) {
    areCharacter <- sapply(x, class) == "character"
    if(any(areCharacter)) {
        characterCols <- names(areCharacter)[areCharacter]
        x[, (characterCols) := lapply(.SD, function(y) rep(NA_real_, length(y))), .SDcols = characterCols]
    }
    return(x)
}
setLogicalColumnsToNA <- function(x) {
    areLogical <- sapply(x, class) == "logical"
    if(any(areLogical)) {
        logicalCols <- names(areLogical)[areLogical]
        x[, (logicalCols) := lapply(.SD, function(y) rep(NA_real_, length(y))), .SDcols = logicalCols]
    }
    return(x)
}




# Compare two data.tables while ignoring attributes and coercing classes of the first to classes of the second:
compareDataTablesUsingClassOf <- function(
        x, y, 
        classOf = c("first", "second"), 
        ignore.variable = NULL, 
        skipNAFraction = FALSE, skipNAAt = NULL, 
        NAReplacement = NULL, 
        ignoreEqual = FALSE, 
        #mergeWhenDifferentNumberOfRows = FALSE, 
        sort = TRUE, tolerance = sqrt(.Machine$double.eps), testAllTRUE = TRUE
) {
    
    if(isFALSE(skipNAAt)) {
        skipNAAt <- NULL
    }
    
    
    classOf <- RstoxData::match_arg_informative(classOf)
    
    if(length(ignore.variable)) {
        ignore.variable <- intersect(ignore.variable, names(x))
        if(length(ignore.variable)) {
            x <- x[, (ignore.variable):=NULL]
            y <- y[, (ignore.variable):=NULL]
        }
    }
    
    # Get the classes of the first and second table:
    classes_in_x <- sapply(x, getRelevantClass)
    classes_in_y <- sapply(y, getRelevantClass)
    
    tryFormats <- c("%Y-%m-%dT%H:%M:%OSZ", "%Y/%m/%dT%H:%M:%OSZ", "%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M", "%Y-%m-%d", "%Y/%m/%d")
    
    if(!identical(classes_in_x, classes_in_y)) {
        
        if(classOf == "first") {
            # Coerce to the class in the memory:
            differ <- names(x)[classes_in_x != classes_in_y]
            for(col in differ){
                if(classes_in_y[col] == "POSIXct") {
                    data.table::set(x, j = col, value = as.POSIXct(x[[col]], tryFormats = tryFormats, tz = "UTC"))
                }
                else {
                    data.table::set(x, j = col, value = methods::as(x[[col]], classes_in_y[col]))
                }
            }
        }
        else if(classOf == "second") {
            # Coerce to the class in the memory:
            differ <- names(y)[classes_in_x != classes_in_y]
            for(col in differ){
                if(classes_in_y[col] == "POSIXct") {
                    data.table::set(y, j = col, value = as.POSIXct(y[[col]], tryFormats = tryFormats, tz = "UTC"
                    ))
                }
                else {
                    data.table::set(y, j = col, value = methods::as(y[[col]], classes_in_x[col]))
                }
            }
        }
    }
    
    if(!isFALSE(skipNAFraction)) {
        # TRUE translates to 0.5:
        if(isTRUE(skipNAFraction)) {
            skipNAFraction <- 0.5
        }
        # Subset both in x and y, possibly making these of different size. It may be more robust t o use mergeWhenDifferentNumberOfRows = TRUE:
        if(NROW(x) && NROW(y)) {
            x <- subset(x, rowMeans(is.na(x)) < skipNAFraction)
            y <- subset(y, rowMeans(is.na(y)) < skipNAFraction)
        }
    }
    # Skip also all rows with at least one NAs in the rows given by skipNAAt:
    if(length(skipNAAt)) {
        x <- skipRowsAtNA(x, skipNAAt)
        y <- skipRowsAtNA(y, skipNAAt)
    }
    
    # Detect keys and use these in all_equal_mergeIfDifferentNumberOfRows:
    keys_x <- locateUniqueKeys(x, requireNextPositive = TRUE)
    keys_y <- locateUniqueKeys(y, requireNextPositive = TRUE)
    
    # Apply the NA replacement:
    if(length(NAReplacement) && (!isFALSE(skipNAFraction) || length(skipNAAt))) {
        message("Using NAReplacement in combination with skipNAFraction or skipNAAt will happen after skipping the rows specified by skipNAFraction and skipNAAt.")
    }
    RstoxBase::replaceNAByReference(x, cols = setdiff(names(x), keys_x), replacement = NAReplacement)
    RstoxBase::replaceNAByReference(y, cols = setdiff(names(y), keys_y), replacement = NAReplacement)
    
    
    # Check equality:
    if(NROW(x) != NROW(y)) {
        out <- all_equal_mergeIfDifferentNumberOfRows(
            x, y, 
            keys_x = keys_x, keys_y = keys_y, 
            check.attributes = FALSE, 
            sort = sort, 
            #NAReplacement = NAReplacement, 
            ignoreEqual = ignoreEqual, 
            tolerance = tolerance, 
            testAllTRUE = testAllTRUE
        )
    }
    else {
        out <- all_equal_reorder(
            x, y, 
            check.attributes = FALSE, 
            #NAReplacement = NAReplacement, 
            tolerance = tolerance
        )
    }
    
    return(out)
}


all_equal_reorder <- function(
        x, y, 
        check.attributes = TRUE, sort = TRUE, 
        #NAReplacement = NULL, 
        tolerance = sqrt(.Machine$double.eps)
) {
    
    # Try first all.equal on the tables, and if not TRUE try again after reordering:
    testAllEqual <- all.equal(as.data.frame(x), as.data.frame(y), check.attributes = check.attributes, all = TRUE, tolerance = tolerance)
    if(sort && !isTRUE(testAllEqual) && data.table::is.data.table(x) && data.table::is.data.table(y)) {
        data.table::setorder(x)
        data.table::setorder(y)
        testAllEqual <- all.equal(as.data.frame(x), as.data.frame(y), check.attributes = check.attributes, all = TRUE, tolerance = tolerance)
    }
    
    warn <- paste(testAllEqual, collapse = "\n")
    
    return(list(
        warn = warn, 
        diffData = NULL
    ))
}


getListOfXYByMerging <- function(
        x, y, 
        keys_x, keys_y, 
        sort = TRUE, 
        ignoreEqual = FALSE, 
        #NAReplacement = NULL, 
        ...
){
    
    # Copy the tables, as we are replacing by reference:
    x <- data.table::copy(x)
    y <- data.table::copy(y)
    
    # This only applies to data.table:
    if(data.table::is.data.table(x) && data.table::is.data.table(y) && NROW(x) != NROW(y)) {
        
        # We can only do this if the same keys are located in both tables:
        if(length(keys_x) && identical(sort(keys_x), sort(keys_y))) {
            keys <- keys_x
            
            # Merge both with all = FALSE and TRUE:
            merged <- merge(x, y, by = keys, all = FALSE)
            mergedAll <- merge(x, y, by = keys, all = TRUE)
            # Get the additional rows we get when changing from all = FALSE to all = TRUE: 
            mergedPlus <- mergedAll[!merged, on = keys]
            
            # Split into x and y again:
            xy <- splitMergedTable(merged, keys = keys, names = c("x", "y"))
            xyPlus <- splitMergedTable(mergedPlus, keys = keys, names = c("x", "y"))
            
            if(ignoreEqual) {
                xy <- subsetByAllEqual(xy, keys)
                xyPlus <- subsetByAllEqual(xyPlus, keys)
            }
            
            #lapply(xy, RstoxBase::replaceNAByReference, replacement = NAReplacement)
            #lapply(xyPlus, RstoxBase::replaceNAByReference, replacement = NAReplacement)
        }
        else {
            warning("Locating common keys failed. The keys found for x (", if(length(keys_x)) paste(keys_x, collapse = ", ") else "no keys", ") and y (", if(length(keys_y)) paste(keys_y, collapse = ", ") else "no keys", ") differ.")
            return(list(xy = list(x = x, y = y), xyPlus = NULL))
        }
    }
    
    # Order the tables:
    if(sort && data.table::is.data.table(x) && data.table::is.data.table(y)) {
        lapply(xy, data.table::setorder)
        lapply(xyPlus, data.table::setorder)
    }
    
    return(list(xy = xy, xyPlus = xyPlus))
}



#' Remove columns where all values are equal both in the x and y element of the input
#' 
#' @param xy A list of tables \code{x} and \code{y}.
#' @param keys The keys of the tables in \code{xy}.
#' 
subsetByAllEqual <- function(xy, keys) {
    # The x and y are assumed to have the same column names:
    nonKeys <- setdiff(names(xy$x), keys)
    
    allEqualX <- sapply(xy$x[, -keys, with = FALSE], RstoxBase::allEqual, na.rm = TRUE)
    allEqualY <- sapply(xy$y[, -keys, with = FALSE], RstoxBase::allEqual, na.rm = TRUE)
    allEqualXY <- allEqualX & allEqualY
    
    if(any(allEqualXY)) {
        variablesToIgnore <- nonKeys[allEqualX & allEqualY]
        xy$x <- xy$x[, !variablesToIgnore, with = FALSE]
        xy$y <- xy$y[, !variablesToIgnore, with = FALSE]
    }
    
    return(xy)
}


locateUniqueKeys <- function(x, requireNextPositive = FALSE) {
    if(any(duplicated(x))) {
        warning("Cannot locate keys if there are duplicated rows of the entire table.")
        return(NULL)
    }
    
    keys <- NULL
    for(ind in seq_along(x)) {
        if(!any(duplicated(x[, seq_len(ind), with = FALSE]))) {
            keys <- names(x)[seq_len(ind)]
            return(keys)
        }
        else if(requireNextPositive) {
            atPositive <- x[[ind + 1]] > 0
            afterRemovingNonPositive <- subset(x, atPositive)
            if(!any(duplicated(afterRemovingNonPositive[, seq_len(ind), with = FALSE]))) {
                keys <- names(x)[seq_len(ind)]
                return(keys)
            }
        }
    }
    
    if(!length(keys)) {
        warning("Could not locate keys (unknown error).")
    }
    
    return(NULL)
}



splitMergedTable <- function(DT, keys, names = c("x", "y")) {
    # If all columns are keys, both x and y are equal:
    if(length(keys) == ncol(DT)) {
        out <- list(
            x = DT,  
            y = DT
        )
    }
    else {
        keyTable <- DT[, keys, with = FALSE]
        data1 <- getColumnsEndingWith(DT, paste0(".", names[1]), fill = TRUE, stripSuffixInNames = TRUE)
        data2 <- getColumnsEndingWith(DT, paste0(".", names[2]), fill = TRUE, stripSuffixInNames = TRUE)
        x <- cbind(keyTable, data1)
        y <- cbind(keyTable, data2)
        out <- list(
            x = x,  
            y = y
        )
    }
    
    return(out)
}


getColumnsEndingWith <- function(x, suffix, fill = FALSE, stripSuffixInNames = FALSE) {
    atSuffix <- which(endsWith(names(x), suffix))
    if(stripSuffixInNames) {
        oldNames <- names(x)[atSuffix]
        newNames <- substr(oldNames, 1, nchar(oldNames) - nchar(suffix))
    }
    if(fill) {
        atSuffix <- seq(min(atSuffix), max(atSuffix))
    }
    out <- x[, atSuffix, with = FALSE]
    if(stripSuffixInNames) {
        data.table::setnames(out, oldNames, newNames)
    }
    return(out)
}




# Function to skip row where at least one of the variables given by skipNAAt are NA:
skipRowsAtNA <- function(x, skipNAAt) {
    skipNAAt <- intersect(skipNAAt, names(x))
    if(length(skipNAAt)) {
        toKeep <- rowSums(is.na(x[, ..skipNAAt])) == 0
        x <- subset(x, toKeep)
    }
    
    return(x)
}




# Function to keep only diffs.
formatDiffs <- function(x) {
    
    x <- unlist(x)
    
    atNotTRUE <- !x %in% TRUE
    
    out <- x[atNotTRUE]
    
    out <- lapply(out, function(x) if(is.character(x)) strsplit(x, "\n")[[1]] else x)
    
    return(out)
}


all_equal_mergeIfDifferentNumberOfRows <- function(
        x, y, 
        keys_x, keys_y, 
        check.attributes = FALSE, 
        sort = TRUE, 
        #NAReplacement = NULL, 
        ignoreEqual = FALSE, 
        tolerance = sqrt(.Machine$double.eps), 
        testAllTRUE = TRUE, 
        ...
) {
    
    if(length(x) == 0 && length(y) == 0 ) {
        return(TRUE)
    }
    
    xyList <- getListOfXYByMerging(
        x, y, 
        keys_x = keys_x, keys_y = keys_y, 
        sort = sort, 
        ignoreEqual = ignoreEqual, 
        #NAReplacement = NAReplacement, 
        ...
    )
    
    
    #all.equal(xy$x, xy$y, check.attributes = check.attributes)
    test <- all.equal(as.data.frame(xyList$xy$x), as.data.frame(xyList$xy$y), check.attributes = FALSE, tolerance = tolerance)
    testPlus <- all.equal(as.data.frame(xyList$xyPlus$x), as.data.frame(xyList$xyPlus$y), check.attributes = FALSE, tolerance = tolerance)
    
    if(testAllTRUE) {
        if(isTRUE(test) && isTRUE(testPlus)) {
            warn <- NULL
            xyList <- NULL
        }
        else if(isTRUE(testPlus)) {
            warn <- paste(c("From merging with all = FALSE:", test), collapse = "\n")
            xyList$xyPlus <- NULL
        }
        else if(isTRUE(test)) {
            warn <- paste(c("From merging with all = TRUE antijoined with merging with all = FALSE:", testPlus), collapse = "\n")
            xyList$xy <- NULL
        }
        else {
            warn <- c(
                paste(c("From merging with all = FALSE:", test), collapse = "\n"), 
                paste(c("From merging with all = TRUE antijoined with merging with all = FALSE:", testPlus), collapse = "\n")
            )
        }
    }
    else {
        if(isTRUE(test)) {
            warn <- NULL
            xyList <- NULL
        }
        else {
            warn <- paste(c("From merging with all = FALSE:", test), collapse = "\n")
        }
    }
    
    
    return(list(
        warn = warn, 
        diffData = xyList
    ))
}








# Compare two data.tables while ignoring attributes and coercing classes of the first to classes of the second:
#compareDataTablesUsingClassOfSecond <- function(x, y) {
#    # Get the classes of the first and second table:
#    classes_in_x <- sapply(x, getRelevantClass)
#    classes_in_y <- sapply(y, getRelevantClass)
#    if(!identical(classes_in_x, classes_in_y)) {
#        # Coerce to the class in the memory:
#        differ <- names(x)[classes_in_x != classes_in_y]
#        for(col in differ){
#            data.table::set(y, j = col, value = methods::as(y[[col]], classes_in_x[col]))
#        }
#    }
#    # Check equality:
#    all.equal(x, y, check.attributes = FALSE)
#}



## Get all coordinates of a SpatialPolygonsDataFrame in one data.table:
#getAllCoords <- function(x) {
#    out <- RstoxBase::getStratumPolygonList(x)
#    out <- data.table::rbindlist(lapply(out, unname))
#    return(out)
#}
#
## Compare two SpatialPolygonsDataFrames using only the polygons:
#compareSPDF <- function(x, y) {
#    xc <- getAllCoords(x)
#    yc <- getAllCoords(y)
#    all.equal(xc, yc)
#}




