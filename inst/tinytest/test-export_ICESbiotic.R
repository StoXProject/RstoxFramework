# Run the test project:
projectPaths <- system.file("test",  "export_ICESbiotic.zip", package = "RstoxFramework")

# TEMPORARILY DISABLED DUE TO ICES BEING HACKED. UN-COMMENT THIS WHEN THE ICES PROBLEM IS FIXED:

test <- compareProjectToStoredOutputFiles(projectPaths, data.out = TRUE)


if(NROW(test$dat$ICESBiotic$Catch) > NROW(test$dat_orig$ICESBiotic$Catch)) {
    warning("There might have been additions to http://vocab.ices.dk/?ref=365 causing more species to be inclcuded in the Catch table. This is accepted for this accepted for this test of export_ICESbiotic.zip.")
    
    # Defiene here the new species added and remove those rows:
    sp <- c(
        "106790" # Added in 2023
    )
    remove <- test$dat$WriteICESBiotic$WriteICESBioticData[, 5] %in% sp
    
    expect_equal(test$dat_orig$WriteICESBiotic$WriteICESBioticData, test$dat$WriteICESBiotic$WriteICESBioticData[!remove, ])
} else {
    expect_true(all(unlist(test$test)))
}
