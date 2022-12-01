# Run the test project:
projectPaths <- system.file("test",  "splitNASC_18.zip", package = "RstoxFramework")
expect_true(compareProjectToStoredOutputFiles(projectPaths, ignore = c("MinLayerDepth", "MaxLayerDepth")))

# Stratum area is identical for the simple shapes of this project (based on a grid):
d <- compareProjectToStoredOutputFiles(projectPaths, ignore = c("MinLayerDepth", "MaxLayerDepth"), data.out = TRUE, tolerance = 1e-12)
expect_true(all(lengths(d$test) == 0))

# In RstoxFramework 3.6.0 the splitNASC_18 test project should be updated and the test reset to the following:
#projectPaths <- system.file("test",  "splitNASC_18.zip", package = "RstoxFramework")
#expect_true(compareProjectToStoredOutputFiles(projectPaths, tolerance = 1e-12))
