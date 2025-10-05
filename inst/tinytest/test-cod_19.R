# In RstoxFramework 3.6.0 the cod_19 test project should be updated and the test reset to the following:
projectPath <- system.file("test",  "cod_19.zip", package = "RstoxFramework")
#expect_true(compareProjectToStoredOutputFiles(projectPath, tolerance = 1e-12, ignore.process = "Bootstrap", skipNAAt = "IndividualTotalLength"))

expect_true(
    compareProjectToStoredOutputFiles(
        projectPath, 
        tolerance = 1e-12, 
        returnBootstrapData = TRUE, selection = NA, unlistSingleTable = TRUE, unlistSingleBootstrapData = FALSE
    )
)

