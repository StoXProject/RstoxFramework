# In RstoxFramework 3.6.0 the BIAS_19_ICES test project should be updated and the test reset to the following:
projectPath <- system.file("test",  "BIAS_19_ICES.zip", package = "RstoxFramework")
#expect_true(compareProjectToStoredOutputFiles(projectPath, skipNAFraction = TRUE, tolerance = 1e-12, ignore.process = "Bootstrap"))

expect_true(
    compareProjectToStoredOutputFiles(
        projectPath, 
        tolerance = 1e-12, 
        returnBootstrapData = TRUE, selection = NA, unlistSingleTable = TRUE
    )
)