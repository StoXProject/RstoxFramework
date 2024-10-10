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
#' @param add.line.index Logical: If TRUE (the default) print row indices as in data.table.
#' @param line.index.start The start of the line indices.
#' 
fixedWidthTable <- function(x, columnSeparator = " ", lineSeparator = NULL, na = "-", enable.auto_unbox = TRUE, add.line.index = FALSE, line.index.start = 1) {
    
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
        
        # Add data.table style line indices which is a sequence from 1:
        if(add.line.index) {
            x <- cbind(c(line.index_column_ = "", paste0(line.index.start - 1 + seq_len(nrow(x)), ":")), x)
        }
        
        # Right pad with spaecs:
        x <- apply(x, 2, function(y) stringi::stri_pad_left(y, max(nchar(y)), pad = " "))
        
        # Collapse to lines:
        x <- apply(x, 1, paste, collapse = columnSeparator)
    }
    else if(is.character(x) && length(dim(x)) == 0) {
        # Replace all NA with the user specified na:
        x[is.na(x)] <- na
        
        # Add the name:
        if(length(names(x))) {
            x <- unname(c(names(x), x))
        }
        
        # Add data.table style line indices which is a sequence from 1:
        if(add.line.index) {
            x <- cbind(line.index_column_ = c("", paste0(line.index.start - 1 + seq_len(length(x) - 1), ":")), x)
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
        
        # If the data has no rows, do not add line index:
        if(!NROW(x)) {
            add.line.index <- FALSE
        }
        
        # Add the column names:
        x <- rbindlist(list(structure(as.list(names(x)), names = names(x)), x))
        
        # Add data.table style line indices which is a sequence from 1:
        if(add.line.index) {
           x <- cbind(line.index_column_ = c("", paste0(line.index.start - 1 + seq_len(nrow(x) - 1), ":")), x)
        }
        
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

#' Read a StoX memory file
#' 
#' @inheritParams readBootstrapData
#' @param filePath The path to the file.
#' @param returnBootstrapData Logical: If TRUE read the content of bootstrap NetCDF4 file.
#' 
readMemoryFile <- function(
    filePath, 
    returnBootstrapData = FALSE, selection = list(), BootstrapID = NA, unlistSingleTable = FALSE
) {
    
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
        # Read the content of the NetCDF4 files:
        if(returnBootstrapData) {
            # The 'unlist' argument is not relevant here, as we specify unlisting arguments in e.g. runModel():
            output <- readBootstrapData(filePath, selection = selection, BootstrapID = BootstrapID, unlistSingleTable = unlistSingleTable)
            # Add class BootstrapData to the output, as the datatype is currently not written to a textfile "outputClass.txt" like the Baseline and Report processes:
            class(output) <- "BootstrapData"
        }
        
        else {
            output <- createStoXNetCDF4FileDataType(filePath)
        }
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
    names(x) <- "FilePath"
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
#' @inheritParams general_arguments
#' @param exdir The directory to unzip to, defaulted to the current directory.
#' 
#' @export
#'
unzipProject <- function(projectPath, exdir = ".") {
    if(!isProject(projectPath)) {
        stop("The zip ", projectPath, " does not contain a valid StoX project at the root folder.")
    }
    
    utils::unzip(projectPath, exdir = exdir, setTimes = TRUE)
    projectName <- basename(tools::file_path_sans_ext(projectPath))
    unzippedProjectPath <- file.path(exdir, projectName)
    return(unzippedProjectPath)
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
#' @inheritParams unlistToDataType
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
write_list_as_tables_NetCDFF4 <- function(list, filePath, nc, index, dims, nchars, append = FALSE, ow = FALSE, missval = -9, compression = NA, verbose = FALSE, validOutputDataClasses = getRstoxFrameworkDefinitions("validOutputDataClasses")) {
    
    # Throughout this function for loops are used instead of *apply, as the ncdf4 functions did not seem to work as expected in *apply functions:
    # Unlist to the StoX data type with sep = "/" to produce groups.
    list <- unlistToDataType(list, sep = "/", validOutputDataClasses = validOutputDataClasses, keepNonStandardAttributes = TRUE)
    
    
    dimsTable <- data.table::rbindlist(lapply(lapply(dims, unlistToDataType, sep = "/"), function(x) lapply(x, utils::head, 1)))
    
    nsteps <- nrow(dimsTable)
    stepDim <- ncdf4::ncdim_def("step", "cardinality", seq_len(nsteps), unlim = FALSE)
    
    #  Run through the tables and format the DateTime:
    for(name in names(list)) {
        # Convert any POSIX to character ISO 8601 (while making sure that POSIXct are not shifted one millisecond down):
        formatPOSIXAsISO8601(list[[name]], cols = NA, add = 1e-4, format = "%Y-%m-%dT%H:%M:%OS3Z")
    }
    
    # Define variables and dimensions if not appending:
    if(!append) {
        
        variables <- list()
        
        nrowVariables <- list()
        
        # For convenience convert the nchars list to a table (but only when !append):
        # Using unlistToDataType() here is maybe confusing, but the  advantage is that we get to set the sep:
        ncharsTable  <- data.table::rbindlist(lapply(nchars, unlistToDataType, sep = "/"))
        
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
                #ncdf4::ncatt_put( ncout, varid = 0, attname = attributeName, attval = attributesToBeWritten[[attributeName]], verbose= verbose)
                writeStringVectorAsAttribute(
                    nc = ncout, 
                    varid = 0, 
                    attname = attributeName, 
                    attval = attributesToBeWritten[[attributeName]], 
                    verbose = verbose
                )
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
            max_length <- getMaxNchar(list[[tableName]][[variableName]])
            
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
    #if(inherits(nc, "ncdf4")) {
    #    return(ncout)
    #}
    #else {
    #    ncdf4::nc_close(ncout)
    #    return(filePath)
    #}
    return(ncout)
}


writeStringVectorAsAttribute <- function(nc, varid, attname, attval, verbose = FALSE) {
    # Paste as a CSV:
    attval <- paste(attval, collapse = ",")
    ncdf4::ncatt_put(
        nc, 
        varid = varid, 
        attname = attname, 
        attval = attval, 
        verbose = verbose
    )
}

readStringVectorAttribute <- function(nc, varid, attname, attval, verbose = FALSE) {
    # Read the attribute:
    out <- ncdf4::ncatt_get(nc, varid = varid, attname = attname, verbose = verbose)$value
    # ... and split by comma:
    out  <- strsplit(out, ",")[[1]]
    
    return(out)
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
formatPOSIXAsISO8601 <- function(table, cols = NULL, add = 0, format = "%Y-%m-%dT%H:%M:%OS3Z") {
    if(!length(cols)) {
        return(NULL)
    }
    else if(length(cols) == 1 && is.na(cols)) {
        cols <- names(table)
    }
    
    areDateTime <- names(table)[sapply(table, getRelevantClass) %in% "POSIXct"]
    
    toConvertToCharacter <- intersect(areDateTime, cols)
    
    if(length(toConvertToCharacter)) {
        for(col in toConvertToCharacter) {
            if(add != 0 && inherits(table[[col]], "POSIXct")) {
                # For some reason we need to set the tz as "UTC" in the as.POSIXct() before the format():
                table[, (col) := format(addToDateTime(get(col), add = add, origin = "1970-01-01", tz = "UTC"), format = format, tz = "UTC")]
            }
            else {
                table[, (col) := format(get(col), format = format, tz = "UTC")]
            }
            #table[, (col) := format(shiftPosix(get(col), add =  add), format = format)]
            #table[, (col) := format(get(col), format = format)]
        }
    }
    
    return(NULL)
}


addToDateTime <- function(dateTime, add, tz = "UTC", origin = "1970-01-01") {
    if(add != 0) {
        dateTime <- unclass(dateTime)
        attr(dateTime, "tzone") <- NULL
        dateTime <- dateTime + add
        dateTime <- as.POSIXct(dateTime + add, origin = origin, tz = tz)
    }
    return(dateTime)
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
    
    # Do not set precision to ggplot objects:
    if("ggplot" %in% class(x)){
        return(x)
    }
    else if(is.list(x) && ! any(c("sf", "data.frame") %in% class(x))){
        output <- lapply(x, setRstoxPrecision)
        class(output) <- currentClass
        output <- setNonStandardAttributes(output, currentAttributes)
                
        return(output)
    }
    # Added this on 2024-06-21 to aviod setting precision to ggplot objects, which caused errors:
    else{
        # These make StoX robust to rounding errors. Be extremely careful to remove or modify any of these!!!:
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




onlyOneToResample_Warning <- function(x, toResample, within, functionName = NULL, toResamplePrefix = "", withinPrefix = "") {
    
    if(NROW(x)) {
        # Count the number toResample per within:
        ux <- unique(x, by = c(toResample, within))
        number <- ux[, .N, by = within]
        number1 <- subset(number, N == 1)[[within]]
        # Ignore missing values (e.g. missing Stratum):
        number1 <- stats::na.omit(number1)
        # Issue the waring:
        if(length(number1)) {
            text <- paste0(
                "StoX: ", if(length(functionName)) paste0("(", functionName, ") "), "The following ", 
                paste0(withinPrefix, within), 
                " have only one ",
                paste0(toResamplePrefix, toResample), 
                ", which is in conflict with the principle of bootstrapping as there will be no contribution to the variance from these ", 
                paste0(withinPrefix, within), 
                ". Please consider merging strata to avoid this problem:", 
                RstoxData::printErrorIDs(number1)
            )
            
            warning(text)
        }
    }
}




