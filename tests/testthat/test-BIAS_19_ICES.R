# Run the test project:
projectPaths <- system.file("test",  "BIAS_19_ICES_.zip", package = "RstoxFramework")
expect_true(compareProjectToStoredOutputFiles(projectPaths))
