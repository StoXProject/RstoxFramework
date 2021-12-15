# RstoxFramework v3.1.19 (2021-12-13)
* Added escaping newline and tab when writing output tables to txt, in order to read back in. Fixed bug in readStoxOutputFile() when no character columns are present.

# RstoxFramework v3.1.18 (2021-12-03)
* Added option of reading empty strings as NA in readModelData() to support output files from StoX <= 3.1.0. 
* Added support for hybrid StoX 2.7 and >= 3 projects, using the same project folder. 
* Fixed bug where only BioticAssignmentWeighting was available for selection in BootstrapMethodTable in Bootstrap() in the GUI, whereas only DefineBioticAssignment is correct. U
* Updated test projects to match changes in RstoxBase.

# RstoxFramework v3.1.17 (2021-11-22)
* Added the requirement jsonvalidate >= 1.3.2, as per changes in JSON definition.
* Modified test projects BIAS_19_ICES, cod_19, export_ICESbiotic, haddock_19, tobis_20 and tobis_20_depth for StoX 3.2.0 (ensuring reproducibility to StoX 3.1.0)
* Added test for SplitNASC versus StoX 2.7s.

# RstoxFramework v3.1.16 (2021-11-08)
* Added tests comparing results to StoX 2.7 for Barents sea cod 2020 and sandeel 2011.

# RstoxFramework v3.1.14 (2021-11-04)
* Updated validation of project.json (using processDataSchema).

# RstoxFramework v3.1.13 (2021-10-24)
* Fixed bug in getProcessOutputFiles() where project paths containing special characters resulted in corrupt file paths, causing View output to crash.
* Added DefineBioticPSU() to convertStoX2.7To3().
* In readProjectDescription(), StratumNameLabel is now set to "StratumName", in case the stratum polygon was added from shapefile or GeoJSON in StoX < 3.2.0.

# RstoxFramework v3.1.11 (2021-10-15)
* Added support for mixed level function outputs, whereas only a list of tables or a list of lists of tables were previously allowed. This fixed bug when a two table output process was included as output from Bootstrap.
* Added support for two table output processes in Bootstrap, where only the table "Data" is used.

# RstoxFramework v3.1.10 (2021-10-05)
* Fixed bugs in getObjectHelpAsHtml() which caused all links to fail.

# RstoxFramework v3.1.9 (2021-09-28)
* Changed output text files (located in the output folder) from un-quoted files for all values (numeric, character, time, logical) to files where all character class values are quoted with ". Also missing values are represented by empty space in the preivous version and NA (un-quoted) in the new version. This ensures equality in class when reading the files back in using RstoxFramework::readModelData(). 
* Modified Versions.R to support installing from source when on Linux. Also, fixed bug where Rstox packages were not sorted hierarchically when installing with installOfficialRstoxPackagesWithDependencies().
* Fixed bug where emptying a field in the filter expression builder caused syntax error. 
* Fixed bug where acoustic PSUs could be added even if the process using DefineAcousticPSU was not active.
* Changed warning to error when processes listed in OutputProcesses in Bootstrap(). 
* Added warning when UseOutputData or RemoveMissingValues is TRUE. 
* Added runProject_ReplaceAcousticFiles(). 
* Added parameter update.functionInputs in modifyProcessName(). 
* Added function findProcess(). 
* First working version of convertStoX2.7To3(). Emptying unused arguments in processes modified by convertStoX2.7To3(). 
* Added check for equality of numeric values in the reports in compareProjectToStoredOutputFiles().

# RstoxFramework v3.1.2 (2021-08-19)
* Updated test projects for ICES export to pass the initial checks on https://acoustic.ices.dk/submit. Moved toJSON_Rstox() from RstoxBase to RstoxFramework, and moved reading AcousticPSU, BioticAssignment and StratumPolygon the other way.

# RstoxFramework v3.0.30 (2021-06-16)
* Added parallel bootstrapping.
* Added BaselineSeedTable in Bootstrap().
* Added testing of full projects (using parallel testthat). Completed documentation. Added readModelData().
* Changed name of output text files to not contain the process name.
* Added runProjects().
* Added warning if Rstox packcages to be installed are not built on the installed R version.
* Fixed bug where NAs were not preserved in project.json. Fixed bug in createProject().
* Added support for mixed OutputDepth (particularly useful in Bootstrap).
* Reintroduced possible values in filter for all-unique variables such as Station and EDSU. Allowing drop down list of only one * unique element.
* Added encoding = UTF-8 in readStoxOutputFile().
* project.json now uses ISO8601 for time..

# RstoxFramework v3.0.0 (2021-02-11)
* Final version for the release of StoX 3.0.0.

# RstoxFramework v1.2.40 (2021-01-30)
* Added %notequal%.

# RstoxFramework v1.2.39 (2021-01-28)
* Fixed bug with possible values of length 1 being dropped.
* Renamed official to certified in the project.json. 
* Removing empty function arguments in runProcess(), in order to use the error "argument ___ is missing, with no default".
* Added warning for non-official project.json

# RstoxFramework v1.2.38 (2021-01-21)
* Fixed bug where Hauls could not be removed from assignment.
* Added support for numeric, integer and logical vectors in addition to the already supported character vector in data types.

# RstoxFramework v1.2.33 (2021-01-12)
* Renamed GUI to Application in project.json.

# RstoxFramework v1.2.33 (2020-12-22)
* Added backwards compatibility (for new projects).
* Increased timeout when downloading packages.
* Replaced geojsonio by geojsonsf to reduce dependencies.

# RstoxFramework v1.2.25 (2020-10-11)
* Introduced project.JSON.

# RstoxFramework v1.2.20 (2020-09-07)
* Added UseOutputData in Bootstrap().

# RstoxFramework v1.2.18 (2020-08-28)
* Removed unit in variable and parameter names

# RstoxFramework v1.2.14 (2020-07-19)
* Added Bootstrap
