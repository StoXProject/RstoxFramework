# Run the test project:
projectPaths <- system.file("test",  "coastalCod_20.zip", package = "RstoxFramework")
expect_true(compareProjectToStoredOutputFiles(projectPaths, try = FALSE))
