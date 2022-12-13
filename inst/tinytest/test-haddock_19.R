# In RstoxFramework 3.6.0 the haddock_19 test project should be updated and the test reset to the following:
projectPath <- system.file("test",  "haddock_19.zip", package = "RstoxFramework")
expect_true(compareProjectToStoredOutputFiles(projectPath, tolerance = 1e-12))



