# Run the test project:
projectPaths <- system.file("test",  "export_ICESbiotic.zip", package = "RstoxFramework")
test <- compareProjectToStoredOutputFiles(projectPaths, mergeWhenDifferentNumberOfRows = TRUE, data.out = TRUE)

if(NROW(test$dat$ICESBiotic$Catch) > NROW(test$dat_orig$ICESBiotic$Catch)) {
    warning("There might have been additions to http://vocab.ices.dk/?ref=365 causing more species to be inclcuded in the Catch table. This is accepted for this accepted for this test of export_ICESbiotic.zip.")
}

expect_true(all(unlist(test$test)))
