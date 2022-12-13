# In RstoxFramework 3.6.0 the catch_99 test project should be updated and the test reset to the following:
projectPath <- system.file("test",  "catch_99.zip", package = "RstoxFramework")
expect_true(compareProjectToStoredOutputFiles(projectPath, tolerance = 1e-12))