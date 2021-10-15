# Run the test project:
projectPaths <- system.file("test",  "export_ICESAcoustic_.zip", package = "RstoxFramework")
expect_true(compareProjectToStoredOutputFiles(projectPaths))
