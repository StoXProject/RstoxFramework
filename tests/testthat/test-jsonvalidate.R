projectValidator <- RstoxFramework::getRstoxFrameworkDefinitions("projectValidatorAJV")

JSON_validationDir <- system.file("test", "JSON_validation", package = "RstoxFramework")
files <- list.files(JSON_validationDir, full.names = TRUE)
names(files) <- basename(tools::file_path_sans_ext(files))

validation <- lapply(files, projectValidator)

expect_true(validation$project)
expect_true(all(!unlist(validation[names(validation) != "project"])))
