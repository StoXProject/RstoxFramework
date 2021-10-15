# Run the test project:
projectPaths <- system.file("test",  "catch_99_.zip", package = "RstoxFramework")
expect_true(compareProjectToStoredOutputFiles(projectPaths))
