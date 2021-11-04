# Run the test project:
projectPaths <- system.file("test",  "splitNASC_18.zip", package = "RstoxFramework")
expect_true(compareProjectToStoredOutputFiles(projectPaths))
