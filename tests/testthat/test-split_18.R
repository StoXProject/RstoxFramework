# Run the test project:
projectPaths <- system.file("test",  "split_18_.zip", package = "RstoxFramework")
expect_true(compareProjectToStoredOutputFiles(projectPaths))
