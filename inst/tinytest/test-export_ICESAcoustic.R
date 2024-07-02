# Run the test project:
projectPath <- system.file("test",  "export_ICESAcoustic.zip", package = "RstoxFramework")

# TEMPORARILY DISABLED DUE TO ICES BEING HACKED. UN-COMMENT THIS WHEN THE ICES PROBLEM IS FIXED:

#expect_true(
#    compareProjectToStoredOutputFiles(
#        projectPath, 
#        tolerance = 1e-12, 
#        returnBootstrapData = TRUE, selection = NA, unlistSingleTable = TRUE
#    )
#)

