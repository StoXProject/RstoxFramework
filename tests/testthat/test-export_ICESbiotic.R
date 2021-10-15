# Run the test project:
projectPaths <- system.file("test",  "export_ICESbiotic_.zip", package = "RstoxFramework")
expect_true(compareProjectToStoredOutputFiles(projectPaths))
