# Run the test project:
projectPaths <- system.file("test",  "catch_99.zip", package = "RstoxFramework")
expect_true(compareProjectToStoredOutputFiles(projectPaths, NAReplacement = list(integer = 0, numeric = 0)))
