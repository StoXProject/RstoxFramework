# Test a project with limited data (Ciaran's project):
projectPath <- system.file("test",  "WP_CSHER_2019.zip", package = "RstoxFramework")
#expect_true(compareProjectToStoredOutputFiles(projectPath, tolerance = 1e-12, ignore.process = "Bootstrap"))

expect_true(
    compareProjectToStoredOutputFiles(
        projectPath, 
        tolerance = 1e-12, 
        returnBootstrapData = TRUE, selection = NA, unlistSingleTable = TRUE
    )
)