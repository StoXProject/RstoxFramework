##############################################
##############################################
#' Merge two data tables with all=TRUE and the specified columns and keys.
#' 
#' Merges two data tables (see \code{\link[data.table]{data.table}}) with all=TRUE, while keeping only columns of the data tables with names intersecting \code{var}, and using the intersect of \code{keys} and the names of the data tables as the 'by' argument.
#' 
#' @param x,y		Two data tables to be merged.
#' @param var		A character vector of names of the columns to keep while merging.
#' @param keys		A character vector of names of the columns to merge by (see the \code{by} argument in \code{\link[data.table]{merge}}).
#' @param keys.out	Logical: If TRUE return a list with the keys used and the merged data.
#'
#' @return A merged data table.
#' 
#' @noRd
#'
merge2 <- function(x, y, var=c("distance", "weight", "lengthsampleweight", "length", "lengthresolution"), keys=c("cruise", "serialnumber", "samplenumber", "SpecCat"), keys.out=FALSE) {
    # Get the keys common for the two data tables:
    commonVar <- intersect(names(x), names(y))
    thisKeys <- intersect(keys, commonVar)
    # Get the variables requested for x and y:
    xvar <- intersect(names(x), c(var, keys))
    yvar <- intersect(names(y), c(var, keys))
    # Remove variables named identically ('weight' in biotic v1.4):
    yvar <- setdiff(yvar, xvar)
    # Add the keys:
    xvar <- unique(c(thisKeys, xvar))
    yvar <- unique(c(thisKeys, yvar))
    
    # Merge the data.tables:
    out <- merge(x[,xvar, with=FALSE], y[,yvar, with=FALSE], all=TRUE, by=thisKeys)
    
    if(keys.out) {
        list(data=out, keys=thisKeys)
    }
    else {
        out
    }
}

# Function to select valid elements by name
selectValidElements <- function(x, names) {
    validNames <- intersect(names, names(x))
    x[validNames]
}

# Function to check whether a data table is rugged:
isDataTableRugged <- function(x) {
    # Return immediately if x has length 0:
    if(nrow(x) == 0) {
        return(FALSE)
    }
    lens <- sapply(x, lengths)
    all(lens == lens[1])
}

# Function to remove all empty elements of a data.table:
replaceEmptyInDataTable = function(DT, replace = NA) {
    for (i in names(DT)) {
        DT[lengths(get(i)) == 0, (i):= replace]
    }
    DT     
}



# Function to create a rectangular data table from a data table which may contain empty cells and vectors in cells (all vectors must have equal length for one row):
flattenDataTable <- function(x, replace = NA) {
    
    # Return immediately if x has length 0:
    if(length(x) == 0) {
        return(x)
    }
    
    # Replace all empty with NA
    x <- replaceEmptyInDataTable(x, replace = replace)

    x <- expandDT(x)
    
    return(x)
}

#' Function to convert data.table to fixed width:
#' 
#' @param x The table to modify.
#' @param columnSeparator The string to separate columns by, defaulted to a single space. 
#' @param lineSeparator The string to separate lines by, defaulted to a NULL, which keeps the output as a vector of strings.
#' @param na The string to replace NAs by, defaulted to "-".
#' @param enable.auto_unbox Logical: If TRUE wrap the output in a list if  \code{pretty} is TRUE and the output is of length 1. This keeps the array when converting to JSON also for length 1.
#' 
fixedWidthTable <- function(x, columnSeparator = " ", lineSeparator = NULL, na = "-", enable.auto_unbox = TRUE) {
    # Return immediately if x has length 0:
    if(length(x) == 0) {
        return(x)
    }
    
    # Hack to make it possible to print matrices:
    if(is.matrix(x)) {
        # Replace all NA with the user specified na:
        x[is.na(x)] <- na
        
        # Add the column names:
        if(length(colnames(x))) {
            x <- rbind(colnames(x), x)
        }
        
        # Right pad with spaecs:
        x <- apply(x, 2, function(y) stringi::stri_pad_left(y, max(nchar(y)), pad = " "))
        
        # Collapse to lines:
        x <- apply(x, 1, paste, collapse = columnSeparator)
    }
    else if(data.table::is.data.table(x)) {
        # First convert all columns to character:
        ### # Take special care of datetime:
        ### isDateTime <- startsWith(sapply(x, getRelevantClass), "POSIX")
        ### POSIXCols  <- colnames(x)[isDateTime]
        ### if(any(isDateTime)) {
        ###     x[, (POSIXCols) := lapply(.SD, function(datetime) format(datetime, format = "%Y-%m-%dT%H:%M:%OS3Z")), .SDcols = POSIXCols]
        ### }
        x[, (colnames(x)) := lapply(.SD, as.character), .SDcols = names(x)]
        
        # Replace all NA with the user specified na:
        x[is.na(x)] <- na
        
        # Add the column names:
        x <- rbindlist(list(structure(as.list(names(x)), names = names(x)), x))
        # Right pad with spaecs:
        x <- x[, lapply(.SD, function(y) stringi::stri_pad_left(y, max(nchar(y)), pad = " "))]
        
        #for(name in names(x)) {
        #    x[, eval(name) := lapply(get(name), function(y) paste0(y, paste(rep(" ", max(nchar(get(name))) - nchar(y)), collapse = "")))]
        #}
        
        
        # Collapse to lines:
        x <- x[, do.call(paste, c(.SD, sep = columnSeparator)), .SDcols = names(x)]
    }
    
    # Collapse the lines if requested:
    if(length(lineSeparator)) {
        x <- paste(x, collapse = lineSeparator)
    }
    else if(enable.auto_unbox && length(x) == 1) {
        x <- list(x)
    }
    
    return(x)
}

# Function to extract the trailing integer of a string (vector):
getTrailingInteger <- function(x, integer = TRUE) {
    # Get the trailing numerics:
    #trailing <- stringr::str_extract(x, "[^[a-z]]*$")
    #trailing <- stringr::str_extract(x, "\\-*\\d+\\.*\\d*")
    #trailing <- gsub("^\\d.*|[A-Za-z]", "", x) # Kept the underscore
    trailing <- gsub( "^\\d.*|[A-Za-z[:punct:][:space:]]", "", x )
    
    # Convert to numeric if specified:
    if(integer) {
        as.integer(trailing)
    }
    else {
        trailing
    }
}

# Function to generate a new name in the sequence of names starting with the prefix:
getNewDefaultName <- function(names, prefix) {
    
    if(length(names)) {
        # Find the names starting with the prefix:
        startsWithProcess_Prefix <- which(startsWith(names, prefix))
        # Get the trailing integers of the names starting with the prefix:
        trailingIntegerString <- getTrailingInteger(names[startsWithProcess_Prefix], integer = FALSE)
        trailingInteger <- getTrailingInteger(names[startsWithProcess_Prefix])
        # Verify that the number of characters equals the sum of the prefix and the number of characters of the numeric string:
        hasCorrectNumberOfCharacters <- nchar(names[startsWithProcess_Prefix]) == nchar(prefix) + nchar(trailingIntegerString)
    }
    else {
        hasCorrectNumberOfCharacters <- NULL
    }
    
    if(length(hasCorrectNumberOfCharacters) == 0) {
        newInteger <- 1
    }
    else {
        # Get new integer as one more than the maximum integer:
        newInteger <- max(trailingInteger[hasCorrectNumberOfCharacters]) + 1
    }
    
    # Add 1 to the latest integer:
    newName <- paste0(prefix, newInteger)
    
    return(newName)
}

#' Function to convert from json to R expression:
#' 
#' @param json A JSON string
#' 
#' @export
#' 
json2expression <- function(json) {
    #l <- parseParameter(json, simplifyVector = FALSE)
    #l <- parseParameter(json)
    l <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    #l <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    list2expression(l)
}


#' Function to convert from R list to R expression:
#' 
#' @param l An R list.
#' @param parentHasSiblings Logical: If TRUE there are more than one element in the top level of the list.
#' 
#' @export
#' 
list2expression <- function(l, parentHasSiblings = FALSE) {
    # Declare the resulting expression
    result <- NULL
    # If the current rules or expression should be negated, we need to enclose the expression in paretheses:
    negate <- isTRUE(l$negate)
    needParentheses <- negate
    
    # Identify rules by the condition:
    if(any(names(l) == "condition")) { 
        # Rules need parentheses, and the link is padded by spaces for readability:
        needParentheses <- needParentheses || parentHasSiblings
        link <- paste('', l$condition, '')
        
        # Recurse into the children:
        result <- paste(lapply(l$rules, list2expression, parentHasSiblings = length(l$rules) > 1), collapse = link)
    } 
    # Otherwise build the expression:
    else {
        # Extract the value for some processing:
        value <- l$value
        
        # If the value is a character, pad with quotation marks:
        if(length(value) && is.character(value[[1]])) {
            # Replace the string "NA" with NA:
            areNAString <- sapply(value, "%in%", "NA")
            if(any(areNAString)) {
                value[areNAString] <- rep(list(NA), sum(areNAString))
            }
            
            # Add quotes:
            value[!areNAString] <- lapply(value[!areNAString], function(x) paste0("\"", x, "\""))
        }
        
        # If there is one single value, and this is NA, change operator to %in%, with a warning:
        if(length(value) == 1 && is.na(value[[1]])) {
            if(l$operator %in% "==") {
                warning("StoX: Operator cannot be == when extracting NAs. Changed from == to %in% for ", l$field, l$operator, value)
                l$operator <- "%in%"
            }
            else if(l$operator %in% "!=") {
                warning("StoX: Operator cannot be != when excluding NAs. Changed from != to %notin% for ", l$field, l$operator, value)
                l$operator <- "%notin%"
            }
        }
        if(length(value) > 1 && l$operator %in% "==") {
            warning("StoX: The operator == cannot be used with multiple reference values, and was replaced by %in%")
            l$operator <- "%in%"
        }
        if(length(value) > 1 && l$operator %in% "!=") {
            warning("StoX: The operator != cannot be used with multiple reference values, and was replaced by %notin%")
            l$operator <- "%notin%"
        }
        
        # Then collapse to a vector:
        #value <- unlist(value)
        
        #value <- lapply(value, function(x) if (is.character(x)) if(x == "NA") NA else paste0("\"", x, "\"") else x)
        # If more than one value, obtain the c(...) notation: 
        if(!length(value)) {
            value = 'c()'
        }
        if(l$operator %in% c('%in%', '%notin%') && length(value) > 1) {
            value = paste0('c(', paste(value, collapse=', '), ')')
        }
        
        # Paste field, operator and value:
        result <- paste(l$field, l$operator, value)
    }
    
    # Enclose in parentheses if there was a link operator or a negation:
    if(needParentheses) {
        result <- paste0('(', result, ')')
    }
    
    # Add the exclamation mark to apply negation:
    if(negate) {
        result <- paste0('!', result)
    }
    
    return(result)
}



splitStrByOpAtLevel0 = function(expr, splitOperator){
    resArr <- list()
    currentArr <- list()
    res <- list()
    exprArr <- unlist(strsplit(expr, ''))
    exprSeq <- seq_along(exprArr)
    
    level <- 0
    for(i in exprSeq) {
        c <- exprArr[i]
        if (c == '(') {
            level <- level + 1
        } else if (c == ')') {
            level <- level - 1
        } 
        if(c != splitOperator || level > 0) {
            currentArr[[length(currentArr) + 1]] <- c
        }
        if (level == 0) {
            if(length(currentArr) > 0 && (c == splitOperator || i == nchar(expr))) {
                # flush the currentArr to a string result vector
                res[[length(res) + 1]] <- paste(currentArr, collapse='')
                currentArr <- list()
            }
        }
    }
    unlist(res)
}


#' Parse an R expression to a nested list:
#' 
#' @param expr An R expression string such as "a == 3 && !(b > 0)".
#' @param generateRuleset Logical: If TRUE output the expressions as rules, which are lists of condition and rules.
#' 
#' @export
#' 
expression2list <- function(expr, generateRuleset = TRUE) {
    res <- NULL
    expr <- trimws(expr)
    negate <- startsWith(expr, '!') 

    orFoundAtLevel0 <- FALSE
    andFoundAtLevel0 <- FALSE
    
    level <- 0
    for(c in unlist(strsplit(expr, ''))) {
        andFoundAtLevel0 <- andFoundAtLevel0 | level == 0 & c == '&'
        orFoundAtLevel0 <- orFoundAtLevel0 | level == 0 & c == '|'
        if (c == '(') {
            level <- level + 1
        } else if (c == ')') {
            level <- level -1
        }
    }
    splitOperator <- NULL
    rulesList <- NULL
    if(orFoundAtLevel0) {
        splitOperator <- '|'
    } else if(andFoundAtLevel0) {
        splitOperator <- '&'
    }
    
    # If a splitOperator (| or &) is found, split into a list of rules:
    if(!is.null(splitOperator)) {
        res = list()
        res$condition = splitOperator
        rulesList <- splitStrByOpAtLevel0(expr, splitOperator)
        #unlist(strsplit(expr, splitOperator, fixed = TRUE))
        
        res$rules <- list()
        for(grp in rulesList) {
          res$rules[[length(res$rules) + 1]] <- expression2list(grp, FALSE) 
        }
    } else if(isTRUE(negate)) { 
        # Handle negate both outside and inside parenthesis by recursing
        res <- expression2list(substr(expr, 2, nchar(expr)), generateRuleset)
        if(!is.null(res)) {
              if('rules' %in% names(res) && length(res$rules) == 1) {
                if('negate' %in% res$rules[[1]]) {
                    res$rules[[1]]$negate = !res$rules[[1]]$negate
                } else {
                  res$rules[[1]] <- c(list(negate = TRUE), res$rules[[1]])
                }
            } else {
                # keep the negate outside at the ruleset
                if('negate' %in% names(res)) {
                    res$negate = !res$negate
                } else {
                  res <- c(list(negate = TRUE), res)
                }
            }

        }
    }
    else if(startsWith(expr, '(') & endsWith(expr, ')')) {
        # rid off surrounding parenthesis (..)
        res <- expression2list(substr(expr, 2, nchar(expr) - 1), generateRuleset)
    } 
    # rule expression field=value:
    else {
        
        # expression field op value
        allPossibleOperators <- unique(unlist(getRstoxFrameworkDefinitions("filterOperators")))
        space <- "\\s*"
        regularExpression <- paste0(
            "([[:alnum:]^\\\\s]+)", 
            space, 
            paste0("(", paste(allPossibleOperators, collapse = "|"), ")"), 
            space, 
            "(.+)"
        )
        safeSeparator <- ";"
        groupingKey <- paste0("\\", 1:3, collapse = safeSeparator)
        code <- gsub(
            regularExpression, 
            groupingKey, 
            expr
        )
        splittedCode <- strsplit(code, safeSeparator)[[1]]
        if(length(splittedCode) != 3) {
            stop("Syntax error in expression: ", expr)
        }
        s <- c(
            splittedCode[1], 
            splittedCode[2], 
            paste(splittedCode[-c(1,2)], collapse = safeSeparator)
        )

        if(length(s) == 3) {
            #valid syntax: field op value
            val <- eval(parse(text = s[[3]]))
            rule <- list(
                field = s[[1]], 
                operator = s[[2]], 
                value = val)
            if(rule$operator %in% c('%in%', '%notin%') && length(rule$value) == 1) {
                rule$value <- list(rule$value)
            }
            if(generateRuleset) {
                res <- list(
                    condition = '&', 
                    rules = list(rule))
            } else {
                res <- rule
            }
        }	 
    }
    res
}








#convert from expression to list 
#expr2j = function(expr) {
#    
#    json_l <- expr2l(expr, NULL, 0, log)
#    jsonlite::toJSON(json_l, auto_unbox = T, pretty=T)
#}
#j2expr(expr2j(expr))
    
verifyPaths <- function(x) {
    valid <- file.exists(x)
    if(any(!valid)) {
        warning("StoX: The following files do not exist: ", paste(x[!valid], collapse = ", "), ".")
    }
    return(x[valid])
}


# getMemoryFileFormat <- function(x) {
#     if(length(x) == 0) {
#         memoryFileFormat <- getRstoxFrameworkDefinitions("memoryFileFormat_Empty")
#     }
#     else if(data.table::is.data.table(x)) {
#         memoryFileFormat <- getRstoxFrameworkDefinitions("memoryFileFormat_Table")
#     }
#     else if(is.matrix(x)) {
#         memoryFileFormat <- getRstoxFrameworkDefinitions("memoryFileFormat_Matrix")
#     }
#     else if("SpatialPolygonsDataFrame" %in% class(x)) {
#         memoryFileFormat <- getRstoxFrameworkDefinitions("memoryFileFormat_Spatial")
#     }
#     else if("SpatialPointsDataFrame" %in% class(x)) {
#         memoryFileFormat <- getRstoxFrameworkDefinitions("memoryFileFormat_Spatial")
#     }
#     else if(is.character(x)) {
#         memoryFileFormat <- getRstoxFrameworkDefinitions("memoryFileFormat_Character")
#     }
#     else if(is.numeric(x)) {
#         memoryFileFormat <- getRstoxFrameworkDefinitions("memoryFileFormat_Numeric")
#     }
#     else if(is.integer(x)) {
#         memoryFileFormat <- getRstoxFrameworkDefinitions("memoryFileFormat_Integer")
#     }
#     else if(is.logical(x)) {
#         memoryFileFormat <- getRstoxFrameworkDefinitions("memoryFileFormat_Logical")
#     }
#     else if(is.list(x)) {
#         memoryFileFormat <- getRstoxFrameworkDefinitions("memoryFileFormat_List")
#     }
#     else {
#         stop("StoX: Wrong memory file class ", class(x)[1])
#     }
#     return(memoryFileFormat)
# }


writeMemoryFile <- function(x, filePathSansExt, ext = NULL) {
    
    if(!length(ext)) {
        stop("ext must be given")
    }
    
    filePath <- paste(filePathSansExt, ext, sep = ".")
    
    # Create the directory if missing:
    dir <- dirname(filePath)
    if(!file.exists(dir)) {
        dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    }
    
    # Write the file:
    #if(ext == "fst") {
    #    fst::write_fst(as.data.frame(x), path = filePath)
    #}
    #else 
    if(ext == "rds") {
        saveRDS(x, file = filePath)
    }
    else {
        stop("StoX: Wrong ext")
    }
    
    return(filePath)
}



writeMemoryFiles <- function(objects, filePathsSansExt, writeOrderFile = TRUE, ext = NULL) {
    
    # Write the files, in an mapply loop if not a valid class at the top level (for outputDepth 2):
    if(isValidOutputDataClass(objects)) {
        filePaths <- writeMemoryFile(objects, filePathSansExt = filePathsSansExt, ext = ext)
    }
    else {
        filePaths <- mapply(writeMemoryFile, objects, filePathSansExt = filePathsSansExt, ext = ext)
    }
    
    
    # Write the orderfile:
    if(writeOrderFile) {
        orderFileName <- file.path(dirname(filePathsSansExt[1]), "tableOrder.txt")
        write(filePaths, orderFileName)
    }
}


readMemoryFile <- function(filePath) {
    
    # Get the file extension:
    ext <- tools::file_ext(filePath)
    
    # Read the file:
    #if(grepl("fst", ext, ignore.case = TRUE)) {
    #    output <- data.table::as.data.table(fst::read_fst(path = filePath))
    #}
    #else 
    if(grepl("rds", ext, ignore.case = TRUE)) {
        output <- readRDS(file = filePath)
    }
    else if(grepl("nc", ext, ignore.case = TRUE)) {
        output <- createStoXNetCDF4FileDataType(filePath)
    }
    else {
        stop("StoX: Unsupported file format")
    }
    
    return(output)
}

# Small function to expand a logical to possible values starting with the the givevn value:
expandLogical <- function(x) {
    c(x, !x)
}


createStoXNetCDF4FileDataType <- function(x) {
    class(x) <- "StoXNetCDF4File"
    return(x)
}




isProcessOutputDataType <- function(processOutput) {
    is.list(processOutput) && 
    length(processOutput) == 1 && 
    names(processOutput) %in% getRstoxFrameworkDefinitions("stoxDataTypes")$functionOutputDataType
}


hasUseOutputData <- function(projectPath, modelName, processID) {
    functionParameters <- getFunctionParameters(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID
    )
    "UseOutputData" %in% names(functionParameters)
}



emptyNamedList <- function() {
    list(a = 1)[0]
}


#' Function to expand a data table so that the cells that are vectors are transposed and the rest repeated to fill the gaps
#' 
#' @param DT A data.table.
#' @param toExpand A vector of names of the tables to expand.
#' 
#' @export
#' 
expandDT <- function(DT, toExpand = NULL) {
    # Set the columns to expand:
    if(length(toExpand) == 0) {
        lens <- lapply(DT, lengths)
        lensLargerThan1 <- sapply(lens, function(l) any(l > 1))
        toExpand <- names(DT)[lensLargerThan1]
    }
    
    if(length(toExpand)) {
        expanded <- lapply(toExpand, function(x) DT[, unlist(get(x))])
        names(expanded) <- toExpand
        DT <- do.call(
            cbind, 
            c(
                list(
                    DT[rep(1:.N, lengths(get(toExpand[1]))), !toExpand, with = FALSE]
                ), 
                #lapply(toExpand, function(x) DT[, unlist(get(x))])
                expanded
            )
        )
    }
    
    DT
}

as.list1 <- function(x) {
    if(!length(x) && !is.list(x) && is.vector(x)) {
        x <- list()
    }
    if(length(x) == 1 && !is.list(x) && is.vector(x)) {
        x <- list(x)
    }
    
    return(x)
}


capitalizeFirstLetter <- function(x) {
    gsub("(^[[:alpha:]])", "\\U\\1", x, perl=TRUE)
}





#' Function for unzipping a zipped StoX project
#' 
#' @param projectPath The project to be run and tested against the existing output files of the project gievn by \code{projectPath_original}.
#' @param exdir The direcory to unzip to, defaulted to the current directory.
#' 
#' @export
#'
unzipProject <- function(projectPath, exdir = ".") {
    if(!isProject(projectPath)) {
        stop("The zip ", projectPath, " does not contain a valid StoX project at the root folder.")
    }
    
    utils::unzip(projectPath, exdir = exdir)
    projectName <- basename(tools::file_path_sans_ext(projectPath))
    unzippedProjectPath <- file.path(exdir, projectName)
    return(unzippedProjectPath)
}




#' Function for comparing existing output files with the memory read using runProject()
#' 
#' @inheritParams readModelData
#' @inheritParams runProcesses
#' @param projectPath The project to be run and tested against the existing output files of the project gievn by \code{projectPath_original}.
#' @param projectPath_original The project holding the existing output files, defaulted to \code{projectPath}.
#' @param intersect.names Logical: If TRUE, compare only same named columns.
#' @param ignore A vector of names of columns to ignore in all.equal().
#' @param skipNAFraction Logical: If TRUE, skip rows with more than 50 percent NAs. Can be set to a value between 0 and 1.
#' @param skipNAAt A vector of strings naming the columns in which NA values identifies rows to skip.
#' @param NAReplacement List of replacement values for different classes of NA, applied after any merging as to incorporate NAs generated during merging.
#' @param ignoreEqual Logical: If TRUE, ignore columns where all values are equal.
#' @param classOf Character string specifying whether to compare after converting to the class of the first or second table. Set this to "first" (default) to convert class to the original data.
#' @param data.out Logical, if TRUE output the original and new data along with the tests. \code{data.out} = NULL implies \code{data.out} = FALSE if no difference was found and  \code{data.out} = TRUE otherwise.
#' @param mergeWhenDifferentNumberOfRows Logical, if TRUE use all.equal_mergeIfDifferentNumberOfRows instead of all.equal.
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
compareProjectToStoredOutputFiles <- function(projectPath, projectPath_original = projectPath, emptyStringAsNA = FALSE, intersect.names = TRUE, ignore = NULL, skipNAFraction = FALSE, skipNAAt = FALSE, NAReplacement = NULL, ignoreEqual = FALSE, classOf = c("first", "second"), try = TRUE, data.out = FALSE, mergeWhenDifferentNumberOfRows = FALSE, sort = TRUE, compareReports = FALSE, checkOutputFiles = TRUE, tolerance = sqrt(.Machine$double.eps), debug = FALSE, save = FALSE, check.columnNames_identical = FALSE, testAllTRUE = FALSE, ...) {
    
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
    
    # Store the file paths of the output files to compare to the new output file paths:
    if(checkOutputFiles) {
        # List the output files:
        outputDir <- file.path(projectPath_copy, "output")
        outputFiles <- list.files(outputDir, recursive = TRUE, full.names = TRUE)
    }
    
    
    openProject(projectPath_copy)
    # Changed to using unlistDepth2 = FALSE‚  as this is in line with the bug fix from StoX 3.6.0 where outputs with multiple tables were no longer unlisted in Bootstrap data:
    #dat <- runProject(projectPath_copy, unlist.models = TRUE, drop.datatype = FALSE, unlistDepth2 = TRUE, close = TRUE, save = save, try = try, msg = FALSE, ...)
    dat <- runProject(projectPath_copy, unlist.models = TRUE, drop.datatype = FALSE, unlistDepth2 = FALSE, close = TRUE, save = save, try = try, msg = FALSE, ...)
    
    
    # Read the original data:
    #dat_orig <- readModelData(projectPath_original, unlist.models = TRUE)
    dat_orig <- readModelData(projectPath_original, unlist = 1, emptyStringAsNA = emptyStringAsNA, verifyFiles = TRUE)
    
    # Reorder the original data to the order of the new data:
    newOrder <- c(intersect(names(dat), names(dat_orig)), setdiff(names(dat_orig), names(dat)))
    dat_orig <- dat_orig[newOrder]
    
    # Compare only those elemens common to the two datasets:
    processNames_present <- all(names(dat_orig) %in% names(dat))
    
    # Expect all column names:
    tableNames_identical <- list()
    columnNames_identical <- list()
    for(name in names(dat_orig)) {
        # Check identical table names: 
        tableNames_identical[[name]] <- identical(sort(names(dat_orig[[name]])), sort(names(dat[[name]])))
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
    
    # Tests will fail for (1) strings "NA" that are written unquoted (as RstoxFramework do from objects of class data.table) and which are read as NA by data.table::fread, and (2) numbers stored as strings (e.g. software version numbers), which are strirpped of leading and trailing zeros by data.table::fread. Thus it is adivced to not compare CESAcocustic().
    for(name in names(dat_orig)) {
        data_equal[[name]] <- list()
        for(subname in names(dat_orig[[name]])) {
            if(data.table::is.data.table(dat_orig[[name]][[subname]])) {
                if(intersect.names) {
                    intersectingNames <- intersect(names(dat_orig[[name]][[subname]]), names(dat[[name]][[subname]]))
                    result <- compareDataTablesUsingClassOf(
                        dat_orig[[name]][[subname]][, intersectingNames, with = FALSE], 
                        dat[[name]][[subname]][, intersectingNames, with = FALSE], 
                        ignore = ignore, 
                        skipNAFraction = skipNAFraction, 
                        skipNAAt = skipNAAt, 
                        NAReplacement = NAReplacement, 
                        ignoreEqual = ignoreEqual,
                        classOf = classOf, 
                        mergeWhenDifferentNumberOfRows = mergeWhenDifferentNumberOfRows, 
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
                        ignore = ignore, 
                        skipNAFraction = skipNAFraction, 
                        skipNAAt = skipNAAt, 
                        NAReplacement = NAReplacement, 
                        ignoreEqual = ignoreEqual, 
                        classOf = classOf, 
                        mergeWhenDifferentNumberOfRows = mergeWhenDifferentNumberOfRows, 
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
    
    if(debug) {
        browser()
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
        newOutputFiles <- list.files(outputDir, recursive = TRUE, full.names = TRUE)
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


# Function to keep only diffs.
formatDiffs <- function(x) {
    
    x <- unlist(x)
    
    atNotTRUE <- !x %in% TRUE
    
    out <- x[atNotTRUE]
    
    out <- lapply(out, function(x) if(is.character(x)) strsplit(x, "\n")[[1]] else x)
    
    return(out)
}

all.equal_mergeIfDifferentNumberOfRows <- function(x, y, check.attributes = FALSE, sort = TRUE, NAReplacement = NULL, ignoreEqual = FALSE, tolerance = sqrt(.Machine$double.eps), testAllTRUE = TRUE, ...) {
    
    if(length(x) == 0 && length(y) == 0 ) {
        return(TRUE)
    }
    
    xyList <- getListOfXYByMerging(x, y, sort = sort, ignoreEqual = ignoreEqual, NAReplacement = NAReplacement, ...)
    
    
    #all.equal(xy$x, xy$y, check.attributes = check.attributes)
    test <- all.equal(as.data.frame(xyList$xy$x), as.data.frame(xyList$xy$y), check.attributes = FALSE, tolerance = tolerance)
    testPlus <-all.equal(as.data.frame(xyList$xyPlus$x), as.data.frame(xyList$xyPlus$y), check.attributes = FALSE, tolerance = tolerance)
    
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


getListOfXYByMerging <- function(x, y, sort = TRUE, ignoreEqual = FALSE, NAReplacement = NULL, ...) {
    x <- data.table::copy(x)
    y <- data.table::copy(y)
    if(data.table::is.data.table(x) && data.table::is.data.table(y) && NROW(x) != NROW(y)) {
        # Locate keys of each table:
        keys_x <- locateUniqueKeys(x, requireNextPositive = TRUE)
        keys_y <- locateUniqueKeys(y, requireNextPositive = TRUE)
        
        if(identical(sort(keys_x), sort(keys_y))) {
            keys <- keys_x
            
            merged <- merge(x, y, by = keys, all = FALSE)
            mergedAll <- merge(x, y, by = keys, all = TRUE)
            mergedPlus <- mergedAll[!merged, on = keys]
            
            
            # Split into x and y again:
            xy <- splitMergedTable(merged, keys = keys, names = c("x", "y"))
            xyPlus <- splitMergedTable(mergedPlus, keys = keys, names = c("x", "y"))
            
            if(ignoreEqual) {
                xy <- subsetByAllEqual(xy, keys)
                xyPlus <- subsetByAllEqual(xyPlus, keys)
            }
            
            lapply(xy, RstoxBase::replaceNAByReference, replacement = NAReplacement)
            lapply(xyPlus, RstoxBase::replaceNAByReference, replacement = NAReplacement)
        }
    }
    
    # Order the tables:
    if(sort && data.table::is.data.table(x) && data.table::is.data.table(y)) {
        lapply(xy, data.table::setorder)
        lapply(xyPlus, data.table::setorder)
    }
    
    return(list(xy = xy, xyPlus = xyPlus))
}

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

splitMergedTable <- function(DT, keys, names = c("x", "y")) {
    keyTable <- DT[, keys, with = FALSE]
    data1 <- getColumnsEndingWith(DT, paste0(".", names[1]), fill = TRUE, stripSuffixInNames = TRUE)
    data2 <- getColumnsEndingWith(DT, paste0(".", names[2]), fill = TRUE, stripSuffixInNames = TRUE)
    x <- cbind(keyTable, data1)
    y <- cbind(keyTable, data2)
    out <- list(x = x,  y = y)
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


locateUniqueKeys <- function(x, requireNextPositive = FALSE) {
    if(any(duplicated(x))) {
        warning("Cannot locate keys if there are duplicated rows of the entire table.")
        return(NULL)
    }
    for(ind in seq_along(x)) {
        if(!any(duplicated(x[, seq_len(ind), with = FALSE]))) {
            return(names(x)[seq_len(ind)])
        }
        else if(requireNextPositive) {
            atPositive <- x[[ind + 1]] > 0
            afterRemovingNonPositive <- subset(x, atPositive)
            if(!any(duplicated(afterRemovingNonPositive[, seq_len(ind), with = FALSE]))) {
                return(names(x)[seq_len(ind)])
            }
        }
    }
    warning("Could not locate keys (unknown error).")
    return(NULL)
}



# Compare two data.tables while ignoring attributes and coercing classes of the first to classes of the second:
compareDataTablesUsingClassOf <- function(x, y, classOf = c("first", "second"), ignore = NULL, skipNAFraction = FALSE, skipNAAt = NULL, NAReplacement = NULL, ignoreEqual = FALSE, mergeWhenDifferentNumberOfRows = FALSE, sort = TRUE, tolerance = sqrt(.Machine$double.eps), testAllTRUE = TRUE) {
    
    
    
    
    classOf <- RstoxData::match_arg_informative(classOf)
    
    if(length(ignore)) {
        ignore <- intersect(ignore, names(x))
        if(length(ignore)) {
            x <- x[, (ignore):=NULL]
            y <- y[, (ignore):=NULL]
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
        if(isTRUE(skipNAFraction)) {
            skipNAFraction <- 0.5
        }
        x <- subset(x, rowMeans(is.na(x)) < skipNAFraction)
        y <- subset(y, rowMeans(is.na(y)) < skipNAFraction)
    }
    
    if(length(skipNAAt)) {
        x <- skipRowsAtNA(x, skipNAAt)
        y <- skipRowsAtNA(y, skipNAAt)
    }
    
    
    
    # Check equality:
    if(mergeWhenDifferentNumberOfRows && NROW(x) != NROW(y)) {
        out <- all.equal_mergeIfDifferentNumberOfRows(x, y, check.attributes = FALSE, all = TRUE, sort = sort, NAReplacement = NAReplacement, ignoreEqual = ignoreEqual, tolerance = tolerance, testAllTRUE = testAllTRUE)
        
        return(out)
    }
    else {
        RstoxBase::replaceNAByReference(x, replacement = NAReplacement)
        RstoxBase::replaceNAByReference(y, replacement = NAReplacement)
        
        # Try first all.equal on the tables, and if not TRUE try again after reordering:
        testAllEqual <- all.equal(as.data.frame(x), as.data.frame(y), check.attributes = FALSE, all = TRUE, tolerance = tolerance)
        if(sort && !isTRUE(testAllEqual) && data.table::is.data.table(x) && data.table::is.data.table(y)) {
            data.table::setorder(x)
            data.table::setorder(y)
            testAllEqual <- all.equal(as.data.frame(x), as.data.frame(y), check.attributes = FALSE, all = TRUE, tolerance = tolerance)
        }
        
        warn <- paste(testAllEqual, collapse = "\n")
        
        return(list(
            warn = warn, 
            diffData = NULL
        ))
    }
}

skipRowsAtNA <- function(x, skipNAAt) {
    skipNAAt <- intersect(skipNAAt, names(x))
    if(length(skipNAAt)) {
        toKeep <- rowSums(is.na(x[, ..skipNAAt])) == 0
        x <- subset(x, toKeep)
    }
    
    return(x)
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
compareProjectToStoredOutputFilesAll <- function(projectPaths, projectPaths_original = projectPaths, emptyStringAsNA = FALSE, intersect.names = TRUE, ignore = NULL, skipNAFraction = FALSE, skipNAAt = FALSE, NAReplacement = NULL, classOf = c("first", "second"), try = TRUE, data.out = FALSE, ...) {
    
    out <- mapply(
        compareProjectToStoredOutputFiles, 
        projectPath = projectPaths, 
        projectPath_original = projectPaths_original, 
        MoreArgs = list(
            emptyStringAsNA = emptyStringAsNA, 
            intersect.names = intersect.names, 
            ignore = ignore, 
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







##################################################
##################################################
#' Get IDs of bad Hauls or Samples
#' 
#' @param errorString A error string (as returned from \code{\link{runFunction}}).
#' @param bullet A character string giving the prefix used when listing bad Hauls or Samples.
#' @param sep A character string markingg the start of the reported ID.
#' @param collapse A character string separator between ID lines.
#'
#' 
#' @return
#' An list of IDs.
#' 
#' @export
#' 
extractErrorIDs <- function(errorString, bullet = "* ", sep = ": ", collapse = "\n") {
    key1 <- escapeForRegex(bullet)
    key2 <- escapeForRegex(sep)
    regex <- paste0("(?<=", key1, ").*(?=", key2, ")")
    errorNames <- unique(regmatches(errorString, gregexpr(regex, errorString, perl = TRUE))[[1]])
    out <- lapply(errorNames, extractErrorIDsOne, errorString = errorString, bullet = bullet, sep = sep, collapse = collapse)
    names(out) <- errorNames
    return(out)
}

extractErrorIDsOne <- function(errorName, errorString, bullet = "* ", sep = ": ", collapse = "\n") {
    key1 <- paste0(escapeForRegex(bullet), errorName, escapeForRegex(sep))
    key2 <- escapeForRegex(collapse)
    regex <- paste0("(?<=", key1, ").*(?=", key2, ")")
    errorIDs <- regmatches(errorString, gregexpr(regex, errorString, perl = TRUE))[[1]]
    return(errorIDs)
}

escapeForRegex <- function(x) {
    gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", x)
}



#' Does the process store output files?:
#' 
#' @inheritParams general_arguments
#' @param requireExists Logical: Should the existence of the output folder be checked.
#' @export
#' 
hasFileOutput <- function(projectPath, modelName, processID, requireExists = TRUE) {
    
    #browser()
    #
    #functionArguments <- getFunctionArguments(
    #    projectPath = projectPath, 
    #    modelName = modelName, 
    #    processID = processID
    #)
    #
    ## Extract the process and the function arguments:
    #process <- functionArguments$process
    
    process <- getProcessArguments(
        projectPath = projectPath, 
        modelName = modelName, 
        processID = processID, 
        #only.valid = TRUE
        only.valid = FALSE
    )
    
    fileOutput <- process$processParameters$fileOutput
    
    if(!length(fileOutput)) {
        fileOutput <- FALSE
    }
    # If the process is supposed to have a file output, check that it exists as a folder:
    if(fileOutput && requireExists) {
        folderPath <- getProcessOutputFolder(
            projectPath = projectPath, 
            modelName = modelName, 
            processID = processID, 
            type = "output"
        )
        
        
        if(!file.exists(folderPath) || !isTRUE(file.info(folderPath)$isdir)) {
            fileOutput <- FALSE
        }
    }
    
    
    return(fileOutput)
}



getFunctionOutputDataType <- function(functionName) {
    # Get the simple function name and find the function in the stoxLibrary:
    functionNameSansPackageName <- sub(".*::", "", functionName)
    functionProperties <- getRstoxFrameworkDefinitions("stoxLibrary")[[functionNameSansPackageName]]
    
    functionProperties$functionOutputDataType
}


readsItsOwnOutputDataType <- function(functionName) {
    
    # Get the function output datatype:
    functionOutputDataType <- getFunctionOutputDataType(functionName)
    
    functionNameSansPackageName <- sub(".*::", "", functionName)
    functionOutputDataType %in% names(formals(get(functionNameSansPackageName)))
}



#' Write/append a table/list of tables to NetCDF4
#' 
#' @inheritParams ncdf4::ncvar_def
#' @param list A list of tables to write to NetCDF4.
#' @param filePath The path to the file.
#' @param nc A netCDF4 object, overiding the \code{filePath}.
#' @param index The index of the current data to write, with number of rows indicated in the \code{dims} argument.
#' @param dims A list of lists of dimensions of the tables to write.
#' @param nchars A list of lists of lists holding the maximum number of characters for each variable of each table of each process.
#' @param append Logical: If TRUE, append to the existing file.
#' @param ow Logical: If TRUE, overwrite the existing file.
#' @param missval The value to use for representing missing values (NA) for numeric variables.
#' 
#' @export
#' 
write_list_as_tables_NetCDFF4 <- function(list, filePath, nc, index, dims, nchars, append = FALSE, ow = FALSE, missval = -9, compression = NA, verbose = FALSE) {
    
    # Thoughout this function for loops are used instead of *apply, as the ncdf4 functions did not seem to work as expected in *apply functions:
    
    # Unlist to the StoX data type with sep = "/" to produce groups.
    list <- unlistToDataType(list, sep = "/", keepNonStandardAttributes = TRUE)
    
    dimsTable <- data.table::rbindlist(lapply(lapply(dims, unlistToDataType, sep = "/"), function(x) lapply(x, utils::head, 1)))
    ncharsTable  <- data.table::rbindlist(lapply(nchars, unlistToDataType, sep = "/"))
    
    nsteps <- nrow(dimsTable)
    stepDim <- ncdf4::ncdim_def("step", "cardinality", seq_len(nsteps), unlim = FALSE)
    
    
    
    #  Run through the tables and format the DateTime:
    for(name in names(list)) {
        # Convert any POSIX to character ISO 8601:
        formatPOSIXAsISO8601(list[[name]])
    }
    
    
    
    # Define variables and dimensions if not appending:
    if(!append) {
        
        variables <- list()
        
        nrowVariables <- list()
        
        #  Run through the tables:
        for(name in names(list)) {
            
            # Add first the nrow variable:
            nrowsVariableName <- paste(name, "nrow", sep = "/")
            nrowVariables[[nrowsVariableName]] <- ncdf4::ncvar_def(
                name = nrowsVariableName, 
                units = "cardinality", 
                prec = "integer", 
                dim = stepDim, 
                compression = compression, 
                verbose = verbose
            )
            
            
            # Select the current table:
            table <- list[[name]]
            
            # Get the number rof rows:
            numberOfRows <- nrow(table)
            
            # Convert any POSIX to character ISO 8601:
            #formatPOSIXAsISO8601(table)
            
            # Define dimension as the number of rows: 
            totalNumberOfRows <- sum(dimsTable[[name]])
            # We need to redefine the rowDim every time, even though it is already defined at the first iteration:
            rowDim <- ncdf4::ncdim_def(paste0(name, "_row"), "cardinality", seq_len(totalNumberOfRows), unlim = FALSE)
            
            # Get StoX units, if present:
            units <- sapply(table, getStoxUnits)
            
            # Get precision:
            prec <- getPrec(table)
            
            # Define the variables:
            for(ind in seq_along(names(table))) {
                thisVariableName <- names(table)[ind]
                thisFullVariableName <- paste(name, thisVariableName, sep = "/")
                
                if(is.character(table[[ind]])) {
                    # Create the string dimension:
                    thisStringDimName <- paste(
                        name, 
                        paste0("nchar_", thisVariableName), 
                        sep = "/"
                    )
                    
                    # Get the maximum number of characters:
                    max_length <- max(1, max(ncharsTable[[thisFullVariableName]]))
                    thisStringDim <- ncdf4::ncdim_def(thisStringDimName, "", seq_len(max_length), create_dimvar = FALSE)
                    
                    # Define the variable:
                    variables[[thisFullVariableName]] <- ncdf4::ncvar_def(
                        name = thisFullVariableName, 
                        units = units[[ind]], 
                        prec = prec[[ind]], 
                        dim = list(thisStringDim, rowDim), 
                        compression = compression, 
                        verbose = verbose
                    )
                }
                else {
                    # Only the row and step dimension for non-character:
                    variables[[thisFullVariableName]] <- ncdf4::ncvar_def(
                        name = thisFullVariableName, 
                        units = units[[ind]], 
                        prec = prec[[ind]], 
                        missval = missval, 
                        dim = list(rowDim), 
                        compression = compression, 
                        verbose = verbose
                    )
                }
            }
            
        }
        
        
        # Create/open the file:
        if(file.exists(filePath)) {
            if(ow){
                ncout <- ncdf4::nc_create(filePath, c(nrowVariables, variables), force_v4 = TRUE, verbose = verbose)
            }
            else{
                stop("File ", filePath, " exists. Chose a different file path or set ow to TRUE.")
            }
        }
        else {
            # Create the file:
            ncout <- ncdf4::nc_create(filePath, c(nrowVariables, variables), force_v4 = TRUE, verbose = verbose)
        }
        
        variableNames <- names(variables)
    }
    else {
        # Use the open file if given:
        if(inherits(nc, "ncdf4")) {
            ncout <- nc
        }
        else if(file.exists(filePath)) {
            ncout <- ncdf4::nc_open(filePath, write = TRUE, verbose = verbose)
        }
        else {
            stop("Cannot append to the non-existing file ", filePath)
        }
        
        
        variableNames <- names(ncout$var)
        # Do not write the nrow variables:
        variableNames <- variableNames[!endsWith(variableNames, "/nrow")]
    }
    
    
    
    if(!append) {
        for(tableVariableName in names(nrowVariables)) {
            
            # Get the name of the table and the name of the variable of the table:
            tableVariableNameSplitLastSlash <- strsplit(tableVariableName, "/(?=[^/]+$)", perl = TRUE)[[1]]
            tableName <- tableVariableNameSplitLastSlash[1]
            variableName <- tableVariableNameSplitLastSlash[2]
            
            ncdf4::ncvar_put(
                ncout, 
                varid = tableVariableName, 
                vals = dimsTable[[tableName]], 
                verbose = verbose
            )
            
            # Add global attributes:
            attributesToBeWritten <- getNonStandardAttributes(list)
            # allAttritues <- attributes(list)
            # standardAttritues <- c("dim", "names", "dimnames")
            # attributesToBeWritten <- setdiff(names(allAttritues), standardAttritues)
            
            for(attributeName in names(attributesToBeWritten)) {
                ncdf4::ncatt_put( ncout, varid = 0, attname = attributeName, attval = attributesToBeWritten[[attributeName]], verbose= verbose)
            }
            
            # Sync, so that not all is written at nc_close:
            ncdf4::nc_sync( ncout )
        }
    }
    
    
    # Put the variables of the given step (e.g. bootstrap ID):
    for(tableVariableName in variableNames) {
        
        # Get the name of the table and the name of the variable of the table:
        tableVariableNameSplitLastSlash <- strsplit(tableVariableName, "/(?=[^/]+$)", perl = TRUE)[[1]]
        tableName <- tableVariableNameSplitLastSlash[1]
        variableName <- tableVariableNameSplitLastSlash[2]
        # Get the length of the variable to write:
        variableLength <- length(list[[tableName]][[variableName]])
        
        
        if(index == 1) {
            startInd <- 1
        }
        else {
            startInd <- dimsTable[seq_len(index - 1), sum(get(tableName))] + 1
        }
        thisNrow <- dimsTable[index, get(tableName)]
        
        if(is.character(list[[tableName]][[variableName]])) {
            
            # Get maximum number of characters of the variable to write:
            suppressWarnings(max_length <- max(1, max(nchar(list[[tableName]][[variableName]]), na.rm = TRUE)))
            ncdf4::ncvar_put(
                ncout, 
                varid = tableVariableName, 
                vals = list[[tableName]][[variableName]], 
                start = c(1, startInd), 
                count = c(max_length, thisNrow), 
                verbose = verbose
            )
        }
        else {
            ncdf4::ncvar_put(
                ncout, 
                varid = tableVariableName, 
                vals = list[[tableName]][[variableName]], 
                start = startInd, 
                count = thisNrow, 
                verbose = verbose
            )
        }
        #ncdf4::nc_sync( ncout )
    }
    
    # Close the file if an open file was not given:
    if(inherits(nc, "ncdf4")) {
        return(nc)
    }
    else {
        ncdf4::nc_close(ncout)
        return(filePath)
    }
}



getNonStandardAttributes <- function(x) {
    allAttributes <- attributes(x)
    standardAttritueNames <- c("dim", "names", "dimnames")
    nonStandardAttritueNames <- setdiff(names(allAttributes), standardAttritueNames)
    nonStandardAttritues <- allAttributes[nonStandardAttritueNames]
    return(nonStandardAttritues)
}

setNonStandardAttributes <- function(x, att) {
    standardAttritueNames <- c("dim", "names", "dimnames")
    nonStandardAttritueNames <- setdiff(names(att), standardAttritueNames)
    for(attributeName in nonStandardAttritueNames) {
        attr(x, attributeName) <- att[[attributeName]]
    }
    return(x)
}





# Utility functions for write_table_NetCDFF4():
formatPOSIXAsISO8601 <- function(table, cols = NULL) {
    if(!length(cols)) {
        cols <- names(table)
    }
    
    areDateTime <- names(table)[sapply(table, getRelevantClass) %in% "POSIXct"]
    
    toConvertToCharacter <- intersect(areDateTime, cols)
    
    if(length(toConvertToCharacter)) {
        for(col in toConvertToCharacter) {
            table[, (col) := format(get(col), format = "%Y-%m-%dT%H:%M:%OS3Z")]
        }
    }
}

getStoxUnits <- function(x) {
    if(is.list(x)) {
        sapply(x, getStoxUnitOne)
    }
    else {
        getStoxUnitOne(x)
    }
}

getStoxUnitOne <- function(x) {
    stoxUnit <- attr(x, "stoxUnit")
    if(!length(stoxUnit)) {
        stoxUnit <- ""
    }
    return(stoxUnit)
}

getPrec <- function(x) {
    if(is.list(x)) {
        lapply(x, getPrecOne)
    }
    else {
        getPrecOne(x)
    }
}

getPrecOne <- function(x) {
    typeToNetCDF4Prec <- getRstoxFrameworkDefinitions("typeToNetCDF4Prec")
    thisType <- getRelevantClass(x)
    atType <- which(thisType == typeToNetCDF4Prec$type)
    
    if(length(atType)) {
        typeToNetCDF4Prec$prec[atType]
    }
    else {
        NA
    }
}







setRstoxPrecision <- function(x) {
    # Get the defines number of digits:
    digits <- getRstoxFrameworkDefinitions("digits")
    signifDigits <- getRstoxFrameworkDefinitions("signifDigits")
    
    currentClass <- class(x)
    currentAttributes <- attributes(x)
    
    if(is.list(x) && ! any(c("sf", "data.frame") %in% class(x))){
        output <- lapply(x, setRstoxPrecision)
        class(output) <- currentClass
        output <- setNonStandardAttributes(output, currentAttributes)
                
        return(output)
    }
    else {
        if(data.table::is.data.table(x)) {
            x <- roundSignifDT(x, digits = digits, signifDigits = signifDigits)
        }
        else if(is.numeric(x) && !is.integer(x) && !all(is.na(x))) {
            x  <- roundSignif(x, digits = digits, signifDigits = signifDigits)
        }
        else if(getRelevantClass(x) == "sf") {
            # The precision of geographical coordinates will only be set to the specified digits, with no consideration of significant digits. Using digits = 12 implies a precision of approximately 100 nanometers, which should be enough:
            precision <- 10^digits
            
            # Write the data to a file to utilize the function sf::st_set_precision:
            outdata <- sf::st_set_precision(x, precision = precision)
            tmp  <- paste0(tempfile(), "nc.shp")
            # Keep the stratum names and crs: 
            stratumNames <- outdata$StratumName
            crs <- sf::st_crs(outdata)
            
            # Write the multipolygons to a temporary file (suppress the name abbreviation warning "Field names abbreviated for ESRI Shapefile driver"):
            suppressWarnings(sf::st_write(outdata, tmp, quiet = TRUE))
            x <- sf::st_read(tmp, quiet = TRUE)
            
            # Add the stratum names and crs again: 
            x <- x[, NULL]
            x$StratumName <- stratumNames
            # Suppress "replacing crs does not reproject data; use st_transform for that":
            suppressWarnings(sf::st_crs(x) <- crs)
            # sf::st_read may read as POLYGON. We want MULTIPOLYGON always:
            x <- sf::st_cast(x, "MULTIPOLYGON")
            
            # Place geometry last, as this seems to be the default in sf, e.g. in st_cast:
            newOrder <- c(setdiff(names(x), "geometry"), "geometry")
            x <- x[, newOrder]
            
            
            file.remove(tmp)
        }
        # None of the other validOutputDataClasses need setting precision to.
        
        return(x)
    }
}

# Function setting the precision of one data table:
roundSignifDT <- function(x, digits, signifDigits) {
    if(NROW(x)) {
        # Detect numeric columns and round off to the specified number of digits:
        atNumericButNotInteger <- sapply(x, is.numeric) & ! sapply(x, is.integer) & sapply(x, function(y) ! all(is.na(y)))
        if(any(atNumericButNotInteger)) {
            numericButNotIntegerCols <- names(x)[atNumericButNotInteger]
            for(numericButNotIntegerCol in numericButNotIntegerCols) {
                x[, eval(numericButNotIntegerCol) := roundSignif(get(numericButNotIntegerCol), digits = ..digits, signifDigits = ..signifDigits)]
            }
        }
    }
    
    return(x)
}

roundSignif <- function(x, digits = 12, signifDigits = NULL) {
    # Get the digits to round off to:
    digits <- getPrecisionDigits(x, digits = digits, signifDigits = signifDigits)
    # Round off:
    out <- round(x, digits)
    
    return(out)
}


getPrecisionDigits <- function(x, digits = 12, signifDigits = NULL) {
    if(length(signifDigits)) {
        # The output is the maximum of the user specified 'digits' and the number of digits needed to obtain 'signifDigits' significant digits:
        digits <- pmax(signifDigits - floor(log10(abs(x))) - 1, digits)
    }
    
    return(digits)
}



dataTable2sf_POINT <- function(x, coords = c("Longitude", "Latitude"), idCol = NULL, crs = NULL) {
    
    # Keep only the idCol and the coords:
    pos <- subset(x, select = c(idCol, coords))
    
    # Convert to POINT sf object:
    points <- sf::st_as_sf(pos, coords = coords)
    # Set projection:
    sf::st_crs(points) <- if(length(crs)) crs else RstoxBase::getRstoxBaseDefinitions("proj4string_longlat")
    
    return(points)
}

dataTable2sf_LINESTRING <- function(x, x1x2y1y2 = c("startLongitude", "startLatitude", "endLongitude", "endLatitude"), idCol = NULL, crs = NULL) {
    
    # Create a geometry column:
    linestrings  <- data.frame(geometry = sf::st_sfc(sapply(seq_len(nrow(x)), function(i) create_segment(x[i, ..x1x2y1y2]), simplify = FALSE)))
    
    # Add info:
    if(length(idCol) && idCol %in% names(x)) {
        linestrings[[idCol]] <- x[, ..idCol]
    } 
    
    # Convert to proper sf data frame:
    linestrings <-  sf::st_sf(linestrings)
    
    # Set projection:
    sf::st_crs(linestrings) <- if(length(crs)) crs else RstoxBase::getRstoxBaseDefinitions("proj4string_longlat")

    return(linestrings)                   
}

# Simple function to create a segment:
create_segment <- function(r) {
    sf::st_linestring(t(matrix(unlist(r), 2, 2)))
}


