# Run the test project:
projectPaths <- system.file("test",  "BIAS_19_ICES.zip", package = "RstoxFramework")

# The change in ReportBootstrap, where rows with combinations of the grouping variables that are not present in the data are removed, results in fewer rows:
d <- compareProjectToStoredOutputFiles(projectPaths, ignore = c("MinLayerDepth", "MaxLayerDepth", "Layer"), data.out = TRUE)
print(d$test)
expect_true(is.list(d$test))
expect_true("ReportBootstrapByStratum.ReportBootstrapData" %in% names(d$test$data_equal))

# Ignoring the rows with NA in "Biomass_sum_cv" and "Abundance_sum_cv" removes the diffs:
d <- compareProjectToStoredOutputFiles(projectPaths, ignore = c("MinLayerDepth", "MaxLayerDepth", "Layer"), skipNAAt = c("Biomass_sum_cv", "Abundance_sum_cv"), data.out = TRUE)
expect_true(all(lengths(d$test) == 0))

# However, using the lower tolerance identifies diffs in StratumArea that propagates to all abundance and biomass:
d <- compareProjectToStoredOutputFiles(projectPaths, ignore = c("MinLayerDepth", "MaxLayerDepth", "Layer"), skipNAAt = c("Biomass_sum_cv", "Abundance_sum_cv"), data.out = TRUE, tolerance = 1e-12)
expect_true(length(d$test$data_equal) > 0)
d$test

# Using the original StratumArea removes all diffs, so we can conlcude that the change in StratumArea from using rgeos::gArea() to sf::st_area() is the lone cause of the diffs:
oldStratumArea <- data.table::as.data.table(read.table(unz(projectPaths, "BIAS_19_ICES/output/baseline/StratumArea/StratumAreaData.txt"), sep = "\t", header = TRUE))
d <- compareProjectToStoredOutputFiles(projectPaths, ignore = c("MinLayerDepth", "MaxLayerDepth", "Layer"), skipNAAt = c("Biomass_sum_cv", "Abundance_sum_cv"), data.out = TRUE, tolerance = 1e-12, replaceDataList = list(StratumArea = oldStratumArea))
expect_true(all(lengths(d$test) == 0))

# In RstoxFramework 3.6.0 the BIAS_19_ICES test project should be updated and the test reset to the following:
#projectPaths <- system.file("test",  "BIAS_19_ICES.zip", package = "RstoxFramework")
#expect_true(compareProjectToStoredOutputFiles(projectPaths, tolerance = 1e-12))