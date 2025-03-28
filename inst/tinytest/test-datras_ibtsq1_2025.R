projectPath <- system.file("test",  "datras_ibtsq1_2025.zip", package = "RstoxFramework")

expect_true(
    compareProjectToStoredOutputFiles(
        projectPath, 
        tolerance = 1e-12, 
        readCsvAsLines = TRUE, 
        ignore.variable = "file"
    )
)

