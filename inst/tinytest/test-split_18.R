# In RstoxFramework 3.6.0 the splitNASC_18 test project should be updated and the test reset to the following:
projectPath <- system.file("test",  "splitNASC_18.zip", package = "RstoxFramework")
expect_true(compareProjectToStoredOutputFiles(projectPath, tolerance = 1e-12))
