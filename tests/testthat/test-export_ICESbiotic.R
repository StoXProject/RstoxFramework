# Run the test project:
projectPaths <- system.file("test",  "export_ICESbiotic01.zip", package = "RstoxFramework")
expect_true(compareProjectToStoredOutputFiles(projectPaths))
