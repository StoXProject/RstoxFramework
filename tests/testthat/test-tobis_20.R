# Run the test project:
projectPaths <- system.file("test",  "tobis_20_.zip", package = "RstoxFramework")
expect_true(compareProjectToStoredOutputFiles(projectPaths))
