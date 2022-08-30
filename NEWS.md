# RstoxFramework v3.5.1 (2022-08-12)
* Fixed bug where EDSUs for StoX projects with data from ICESAcoustic data with and without end position given by Longitude2 resulted in EDSUs not being shown.

# RstoxFramework v3.5.0 (2022-08-12)
* Start of using semantic versioning (https://semver.org/). Before this release the two first version numbers represented the major and minor release number, in accordance with semantic versioning, whereas the third version number identified test versions. The major and minor releases (versions ending with 0.0 or 0) were considered as official versions. From this release and onwards, the third version number will represent patches (bug fixes), and are to be considered equally official as the major and minor releases. In fact, as patches are restricted to fixing bugs and not adding new functionality, the latest patch will be the recommended version.

# RstoxFramework v3.4.6 (2022-08-10)
* Added truncation to 1000 characters for messages, warnings and errors

# RstoxFramework v3.4.4 (2022-08-07)
* Added optional validation of the project.jsos in readProjectDescription(). 
* Added options to compareProjectToStoredOutputFiles() (setNATo0 replaecd by NAReplacement, ignoreEqual to ignore columns where all values are equal, mergeWhenDifferentNumberOfRows to use all.equal_mergeIfDifferentNumberOfRows instead of all.equal, and sort to sort the tables). 
* Added the diffData as output from compareProjectToStoredOutputFiles() to assist identifying the diffs.

# RstoxFramework v3.4.3 (2022-06-22)
* Changed to using null to denote missing values (NAs) in the project.json, instead of "string" in jsonlite::toJSON().
* Added formatting of parameter tables read from the GUI.
* Changed to using only the first primitive type of a table column defined in a process data JSON schema, when formatting process data.
* Modified the test project splitNASC_18.zip according to the correction of the error in "DateTime" in StoxAcoustic (seconds dropped for ICESAcoustic data).

# RstoxFramework v3.4.1 (2022-05-31)
* Added R 4.2. as supported version.
* Changed tolerance in test-versus_2.7.R as per slight differences in StratumArea due to move from rgeos to sf in RstoxBase, forced by https://www.r-bloggers.com/2022/04/r-spatial-evolution-retirement-of-rgdal-rgeos-and-maptools/.

# RstoxFramework v3.4.1 (2022-05-13)
* Fixed bug in Versions.R, which is used by StoX for the tool "Install Rstox packages". The bug was that the package data.table was used but not installed. Also, added support for installing Rstox package binaries built with older R versions than the one installed, allowing for installation of Rstox packages in existing StoX versions even when a new R version is released and installed. In R 4.2. the location of the folder in which user installed packages are saved has changed from the Documents folder to the AppData > Local folder of the user, which is now included in Versions.R.
* Added the parameters onlyStoxWarnings and onlyStoxErrors to runFunction().
* Added the parameter empty.output in copyProject().
* Updated test projects coastalCod_20, export_ICESbiotic, splitNASC_18, tobis_20 and tobis_20_depth according to the change in RstoxBase where AssignmentLengthDistribution now outputs WeightedNumber normalized to 100 per PSU.

# RstoxFramework v3.3.7 (2022-03-22)
* Added expandProcess() and extractErrorIDs()

# RstoxFramework v3.3.6 (2022-03-15)
* Fixed bug in removeEDSU(), where EDSUs to remove were assigned empty string instead of NA as PSU.
* Added export HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK=false in check-full.yaml to prevent Homebrew from re-installing R with .Platform$pkgType = "source" on Mac.

# RstoxFramework v3.3.5 (2022-03-03)
* Modified tests export_ICESbiotic.zip and export_ICESAcoustic.zip for the new ICES formats.

# RstoxFramework v3.3.4 (2022-02-28)
* Reduced time of ReportBootstrap() to a few percent.
* Changed all sd and cv in reports from 0 to NA. Standard deviation = 0 is no longer accepted by StoX, as it implies either insufficient number of bootstraps or only one value to sample from in the bootstrapping.
* Fixed bug where the blue dot marking processes as 'run' was turned on on a newly modified process when immediately modifying a later process.
* Moved setting precision to runProcess() instead of each StoX function.
* Fixed bug when working with a DefineStratumPolygon procecss with no polygons defined (readProcessOutputFile() did nor read deal properly with the empty SpatialPolygonsDataFrame with jsonlite::prettify(geojsonsf::sf_geojson(sf::st_as_sf(data))), but ok when using replaceSpatialFileReference(buildSpatialFileReferenceString(data)) instead).
* Changed to using StratumName instead of the old polygonName in stratum polygons throughout RstoxFramework and the StoX GUI.
* Changed to using RstoxData::firstClass instead of a copy.
* Added order of backward compatibility actions to package first (alphabetically), then change version (numerically), and finally action type with order as given by RstoxFramework::getRstoxFrameworkDefinitions("backwardCompatibilityActionNames"). 

# RstoxFramework v3.3.3 (2022-02-14)
* Added InformationVariables to ReportBootstrap().

# RstoxFramework v3.3.2 (2022-02-10)
* Added reshapeParameter and reshapeProcessData as backward compatibility actions.
* Cleaned up backward compatibility actions and added StoX-messages when verbose = TRUE.
* Removed applyBackwardCompatibility() at saveProject() as the GUI closes the project when updating Rstox packages.
* Updated test projects BIAS_19_ICES, catch_99, export_ICESAcoustic, export_ICESbiotic according to the change in RstoxData::DefineTranslation() in (RstoxData v1.5.7), and the change in the output of ICESAcoustic(), ICESBiotic(), where multiple input files are rbinded resulting in non-file-specific output table name.

# RstoxFramework v3.2.3 (2022-01-12)
* Updated ResampleMeanLengthDistributionData() and ResampleMeanSpeciesCategoryCatchData() to use number instead of count.
* Added close of projects in runModel() and runProject() and delting project copies in Bootstrap() on.exit.

# RstoxFramework v3.2.2 (2022-01-10)
* Added ResampleMeanSpeciesCategoryCatchData(). Added test for functioning help-pages. Updated tests.

# RstoxFramework v3.2.1 (2022-01-07)
* Added renameColumInProcessDataTable as backward compatibility action. Changed to using AJV. Dropped the option of saving and reading project.RData. Added a second json validation after backward compatibility action, in case the first did not pass.

# RstoxFramework v3.1.21 (2021-12-22)
* Added coastal cod test project with empty PSUs, stratum names ordered as "Loppa" before "indre", and NA in Beam for certain acoustic PSUs.

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
