# Test of StoX 2.7 versus current version:


#### 1. Cod 2020: ####

# Define path to the project, which contains both StoX 2.7 and 3:
projectPath3_cod <- system.file("test", "versus_2.7", "cod_20.zip", package = "RstoxFramework")
projectPath3_cod <- unzipProject(projectPath3_cod, exdir = tempdir())

# Run the cod_20 in current StoX:
new_cod <- runModel(projectPath3_cod, modelName = "baseline")

# Get the new results:
# Abundance:
ab_new <- new_cod$Abundance$Data
# super-individuals:
si_new <- new_cod$SuperIndividuals

# Read old results:
# Abundance:
ab_old <- readStoX2.7OutputFile(file.path(projectPath3_cod, "output", "baseline", "data", "13_AbundanceByLength_Abundance.txt"))
# super-individuals:
si_old <- readStoX2.7OutputFile(file.path(projectPath3_cod, "output", "baseline", "data", "16_SuperIndAbundance_SuperIndividuals.txt"))


# Compare abundance:
mab <- merge(ab_new, ab_old, by.x = c("Stratum", "IndividualTotalLength"), by.y = c("SampleUnit", "LengthGroup"), all = TRUE)

# Test:
tolerance <- 3e-3
expect_true(mab[, all(abs(Abundance.x - Abundance.y) < tolerance, na.rm =  TRUE)])


# Compare super-individuals:
# Order both results and cbind, as we do not have corresponding keys to merge by (samplenumber (catchpartnumber in NMDBiotic 3) used in StoX 2.7 and catchsampleid used in StoX 3):
data.table::setorderv(si_new, c("Stratum", "IndividualTotalLength", "HaulKey", "SpeciesCategory", "SampleKey", "IndividualKey"))
data.table::setorderv(si_old, c("Stratum", "LenGrp", "serialno", "species", "samplenumber", "no"))

# Test:
tolerance <- 1e-4
expect_true(all(abs(si_new$Abundance - si_old$Abundance) < tolerance))





#### 2. Tobis 2011: ####

# Define path to the project, which contains both StoX 2.7 and 3:
projectPath3_tobis <- system.file("test", "versus_2.7", "tobis_11.zip", package = "RstoxFramework")
projectPath3_tobis <- unzipProject(projectPath3_tobis, exdir = tempdir())

# Run the tobis_11 in current StoX:
new_tobis <- runModel(projectPath3_tobis, modelName = "baseline")

# Get the new results:
# Abundance:
ab_new <- new_tobis$Abundance$Data
# super-individuals:
si_new <- new_tobis$SuperIndividuals

# Read old results:
# Abundance:
ab_old <- readStoX2.7OutputFile(file.path(projectPath3_tobis, "output", "baseline", "data", "19_AbundanceByLength_Abundance.txt"))
# super-individuals:
si_old <- readStoX2.7OutputFile(file.path(projectPath3_tobis, "output", "baseline", "data", "22_SuperIndAbundance_SuperIndividuals.txt"))


# Compare abundance:
mab <- merge(ab_new, ab_old, by.x = c("Stratum", "IndividualTotalLength"), by.y = c("SampleUnit", "LengthGroup"), all = TRUE)
mab[is.na(Abundance.y), Abundance.y := 0]

# Test:
tolerance <- 20
expect_true(mab[, all(abs(Abundance.x - Abundance.y) < tolerance, na.rm =  TRUE)])


# Compare super-individuals:
# Order both results and cbind, as we do not have corresponding keys to merge by (samplenumber (catchpartnumber in NMDBiotic 3) used in StoX 2.7 and catchsampleid used in StoX 3):
data.table::setorderv(si_new, c("Stratum", "IndividualTotalLength", "HaulKey", "SpeciesCategory", "SampleKey", "IndividualKey"))
data.table::setorderv(si_old, c("Stratum", "LenGrp", "serialno", "species", "samplenumber", "no"))

# Test:
tolerance <- 0.25
expect_true(all(abs(si_new$Abundance - si_old$Abundance) < tolerance))





#### 3. SplitNASC Angola 2015: ####

# Define path to the project, which contains both StoX 2.7 and 3:
projectPath3_Angola <- system.file("test", "versus_2.7", "Angola_15.zip", package = "RstoxFramework")
projectPath3_Angola <- unzipProject(projectPath3_Angola, exdir = tempdir())

# Run the Angola_15 in current StoX:
new_Angola <- runModel(projectPath3_Angola, modelName = "baseline")

# Get the new SplitNASC:
sn_new <- new_Angola$SplitNASC

# Read old SplitNASC:
sn_old <- readStoX2.7OutputFile(file.path(projectPath3_Angola, "output", "baseline", "data", "14_SplitNASC_NASC.txt"))
convertKeys2.7To3(sn_old)


# Compare SplitNASC:
m <- merge(sn_old, sn_new, by.x = c("AcoCat", "SampleUnit", "Layer"), by.y = c("AcousticCategory", "EDSU", "Channel"), all = TRUE)
tolerance <- 5e-5
expect_true(m[, max(abs(NASC.y - NASC.x) / NASC.y, na.rm = TRUE)] < tolerance)

