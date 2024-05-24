# In RstoxFramework 3.6.0 the tobis_20 test project should be updated and the test reset to the following:
projectPath <- system.file("test",  "tobis_20_depth.zip", package = "RstoxFramework")
# The Biomass_sum_cv seems to diff by up to 2.605614e-12,so we increases the tolerance a bit:
expect_true(compareProjectToStoredOutputFiles(projectPath, tolerance = 3e-12, ignore.process = "Bootstrap", skipNAFraction = 0.5))
