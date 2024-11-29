# Run the test project:
projectPath <- system.file("test",  "export_ICESAcoustic.zip", package = "RstoxFramework")

#expect_true(
#    compareProjectToStoredOutputFiles(
#        projectPath, 
#        tolerance = 1e-12
#    )
#)


test <- compareProjectToStoredOutputFiles(projectPath, data.out = TRUE)

# Find the column containin EchoType:
hasEchoType <- apply(test$dat$WriteICESAcoustic$WriteICESAcousticData, 2, function(x) any(grepl("EchoType", x)))
expect_equal(test$dat_orig$WriteICESAcoustic$WriteICESAcousticData, test$dat$WriteICESAcoustic$WriteICESAcousticData[, !hasEchoType])

