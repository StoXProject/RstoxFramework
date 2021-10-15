# Run the test project:
projectPaths <- system.file("test",  "haddock_19_.zip", package = "RstoxFramework")
expect_true(compareProjectToStoredOutputFiles(projectPaths))
