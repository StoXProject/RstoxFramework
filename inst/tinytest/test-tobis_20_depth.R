# Run the test project:
projectPaths <- system.file("test",  "tobis_20_depth.zip", package = "RstoxFramework")
expect_true(compareProjectToStoredOutputFiles(projectPaths))
