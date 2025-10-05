# Temporarily disable this test Intel macs, as these do not seem to be able to connect to the ICES vocabularies any more (as of October 2025). We should immediately change warnings to errors in RstoxData:::testICESURL! Possibly with an option to turn errors to warnings:

if(Sys.info()["sysname"] == "Darwin" && Sys.info()["machine"] != "arm64") {
    expect_true(TRUE)
}
else {
    # Run the test project:
    projectPaths <- system.file("test",  "export_ICESbiotic.zip", package = "RstoxFramework")
    
    
    test <- compareProjectToStoredOutputFiles(projectPaths, data.out = TRUE)
    
    if(NROW(test$dat$ICESBiotic$Catch) > NROW(test$dat_orig$ICESBiotic$Catch)) {
        warning("There might have been additions to http://vocab.ices.dk/?ref=365 causing more species to be inclcuded in the Catch table. This is accepted for this test of export_ICESbiotic.zip.")
        
        # Defiene here the new species added and remove those rows:
        sp <- c(
            "106790" # Added in 2023
        )
        remove <- test$dat$WriteICESBiotic$WriteICESBioticData[, 5] %in% sp
        
        expect_equal(test$dat_orig$WriteICESBiotic$WriteICESBioticData, test$dat$WriteICESBiotic$WriteICESBioticData[!remove, ])
    } else {
        expect_true(all(unlist(test$test)))
    } 
}

