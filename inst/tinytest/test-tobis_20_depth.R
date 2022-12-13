# In RstoxFramework 3.6.0 the tobis_20 test project should be updated and the test reset to the following:
projectPath <- system.file("test",  "tobis_20_depth.zip", package = "RstoxFramework")
expect_true(compareProjectToStoredOutputFiles(projectPath, tolerance = 1e-12))
