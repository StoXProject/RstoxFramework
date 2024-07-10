# Run the test project:
projectPath <- system.file("test",  "export_ICESAcoustic.zip", package = "RstoxFramework")

expect_true(
    compareProjectToStoredOutputFiles(
        projectPath, 
        tolerance = 1e-12, 
        returnBootstrapData = TRUE, selection = NA, unlistSingleTable = TRUE
    )
)

