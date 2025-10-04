# Run the test project:
projectPaths <- system.file("test",  "export_ICESbiotic.zip", package = "RstoxFramework")


test <- compareProjectToStoredOutputFiles(projectPaths, data.out = TRUE)

xmlRaw <- xml2::read_xml("https://acoustic.ices.dk/Services/Schema/XML/SpecWoRMS.xml")
validCodes <- xml2::xml_text(xml2::xml_find_all(xmlRaw, "//Code//Key"))

stop("Number of species in vocab: ", length(validCodes))

stop(
    "Number of rows of original: ", 
    NROW(test$dat_orig$ICESBiotic$Catch), 
    " ___ \n",
    paste(sort(unique(test$dat_orig$ICESBiotic$Catch$CatchSpeciesCode)), collapse = "\n"),
    " ___ \n",
    "Number of rows of new: ", NROW(test$dat$ICESBiotic$Catch),
    " ___ \n",
    paste(sort(unique(test$dat$ICESBiotic$Catch$CatchSpeciesCode)), collapse = "\n")
)


if(NROW(test$dat$ICESBiotic$Catch) > NROW(test$dat_orig$ICESBiotic$Catch)) {
    warning("There might have been additions to http://vocab.ices.dk/?ref=365 causing more species to be inclcuded in the Catch table. This is accepted for this test of export_ICESbiotic.zip.")
    
    # Defiene here the new species added and remove those rows:
    sp <- c(
        "106790" # Added in 2023
    )
    remove <- test$dat$WriteICESBiotic$WriteICESBioticData[, 5] %in% sp
    
    expect_equal(test$dat_orig$WriteICESBiotic$WriteICESBioticData, test$dat$WriteICESBiotic$WriteICESBioticData[!remove, ])
} else {
    expect_true(all(unlist(test$test)))
}
