# Test copyProject:

projectPath <- system.file("test",  "BIAS_19_ICES.zip", package = "RstoxFramework")
projectPath_unzipped <- file.path(tempdir(), "BIAS_19_ICES")
newProjectPath <- file.path(tempdir(), "BIAS_19_ICES_copy")


# Copy using default settings:
copyProject(projectPath_unzipped, newProjectPath, ow = TRUE)


# Check that we have two biotic files, including the unused one ("Biotic_BIAS2019 copy.xml") when empty.input is FALSE (default):
bioticFiles <- list.files(RstoxFramework::getProjectPaths(newProjectPath, "biotic"))
expected_bioticFiles <- c(
    "Biotic_BIAS2019 copy.xml",
    "Biotic_BIAS2019.xml"
)

expect_true(all(basename(bioticFiles) == expected_bioticFiles))


# Check that the output from both the existing and non-existing Abundance process is present when empty.output is FALSE (default):
baselineOutput <- list.dirs(RstoxFramework::getProjectPaths(newProjectPath, "baseline"), recursive = FALSE)
abundanceProcesses <- c(
    "Abundance", 
    "Abundance copy"
)

expect_true(all(abundanceProcesses %in% basename(baselineOutput)))


# Check that the copy is open, as is the original:
expect_true(RstoxFramework::isOpenProject(projectPath_unzipped))
expect_true(RstoxFramework::isOpenProject(newProjectPath))



# Copy using empty.input = TRUE, empty.output = TRUE, clean.process = TRUE and close = TRUE:
copyProject(projectPath_unzipped, newProjectPath, empty.input = NA, empty.output = NA, clean.process = TRUE, empty.memory = c("analysis", "report"), ow = TRUE)

# Check that we now have the translation file in the root input folder and one biotic file in the biotic folder:
inputFiles <- setdiff(
    list.files(RstoxFramework::getProjectPaths(newProjectPath, "input"), recursive = TRUE), 
    list.dirs(RstoxFramework::getProjectPaths(newProjectPath, "input"), full.names = FALSE)
)
expected_inputFiles <- c(
    "biotic/Biotic_BIAS2019.xml", 
    "IndividualMaturityTranslation.txt"
)

expect_true(all(inputFiles == expected_inputFiles))


# Check that we only have the existing Abundance process:
baselineOutput <- list.dirs(RstoxFramework::getProjectPaths(newProjectPath, "baseline"), recursive = FALSE)
abundanceProcess <- "Abundance"
nonExistingAbundanceProcess <- "Abundance copy"

expect_true(abundanceProcess %in% basename(baselineOutput))
expect_true(! nonExistingAbundanceProcess %in% basename(baselineOutput))


# Check that the analysis and report memory is emptied as requested:
baselineMemory <- list.dirs(RstoxFramework::getProjectPaths(newProjectPath, "dataModelsFolders")["baseline"], recursive = FALSE)
expect_true(length(baselineMemory) == 18)

analysisMemory <- list.dirs(RstoxFramework::getProjectPaths(newProjectPath, "dataModelsFolders")["analysis"], recursive = FALSE)
expect_true(length(analysisMemory) == 0)

reportMemory <- list.dirs(RstoxFramework::getProjectPaths(newProjectPath, "dataModelsFolders")["report"], recursive = FALSE)
expect_true(length(reportMemory) == 0)





# Copy using empty.input = TRUE, empty.output = TRUE, clean.process = TRUE and close = TRUE:
copyProject(projectPath_unzipped, newProjectPath, empty.input = TRUE, empty.output = TRUE, clean.process = TRUE, close = TRUE, ow = TRUE)

# Check that there are no inpup or output files, and only one project.json file:
files <- setdiff(list.files(newProjectPath, recursive = TRUE), list.dirs(newProjectPath, full.names = FALSE))

expect_true(length(files) == 1 && basename(files) == "project.json")


