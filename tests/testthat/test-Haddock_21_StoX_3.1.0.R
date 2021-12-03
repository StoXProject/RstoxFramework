# Run the test project:
projectPaths <- system.file("test",  "Haddock_21_StoX_3.1.0.zip", package = "RstoxFramework")
# Ignore columns which contain more NAs in StoX >= 3.2.0, and also read empty strings as NA as per StoX 3.1.0 wiriting NAs as "":
result <- c(
    columnNames_identical.AcousticDensity.Data = FALSE, 
    columnNames_identical.AssignmentLengthDistribution.AssignmentLengthDistributionData = FALSE, 
    columnNames_identical.MeanDensity.Data = FALSE 
)
expect_equal(
    compareProjectToStoredOutputFiles(
        projectPaths, 
        emptyStringAsNA = TRUE, 
        ignore = c("Layer", "PSU", "MinLayerDepth", "MaxLayerDepth", "DensityWeight", "MeanDensityWeight"), 
        setNATo0 = TRUE
    ), 
    result
)
