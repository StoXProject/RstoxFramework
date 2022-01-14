# Test all help pages:
packageFunctionNames <- RstoxFramework::getRstoxFrameworkDefinitions("availablePackageFunctionNames")
  
testGetObjectHelpAsHtmlOne <- function(packageFunctionNames) {
    packageFunctionNames <- strsplit(packageFunctionNames, "::", fixed = TRUE)[[1]]
    html <- tryCatch(
        RstoxFramework::getObjectHelpAsHtml(
            packageName = packageFunctionNames[1], 
            objectName = packageFunctionNames[2]
        ), 
        error = NULL
    )
    
    fail <- length(html) == 0
    
    return(fail)
}

fails <- sapply(packageFunctionNames, testGetObjectHelpAsHtmlOne)

expect_equal(unname(fails), logical(length(packageFunctionNames)))

