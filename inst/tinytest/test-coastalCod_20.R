# In RstoxFramework 3.6.0 the coastalCod_20 test project should be updated and the test reset to the following:
projectPath <- system.file("test",  "coastalCod_20.zip", package = "RstoxFramework")
#expect_true(compareProjectToStoredOutputFiles(projectPath, tolerance = 1e-12, ignore.process = "Bootstrap"))

expect_true(
    compareProjectToStoredOutputFiles(
        projectPath, 
        tolerance = 1e-12, 
        returnBootstrapData = TRUE, selection = NA, unlistSingleTable = TRUE
    )
)