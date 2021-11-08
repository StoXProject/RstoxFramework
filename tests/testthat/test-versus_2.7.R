# Test of StoX 2.7 versus current version:

#### Cod 2020: ####
projectPath3_cod <- system.file("test", "versus_2.7", "cod_20.zip", package = "RstoxFramework")
projectPath3_cod <- unzipProject(projectPath3_cod, exdir = tempdir())

# Run the cod_20 in current StoX:
new_cod <- RstoxFramework::runModel(projectPath3_cod, modelName = "baseline")
# Get new resutls:
ab_new <- new_cod$Abundance$Data

# Compare abundance:
ab_old <- data.table::fread(file.path(projectPath3_cod, "output2.7", "13_AbundanceByLength_Abundance.txt"))

# Set class of old results to match class new resutls:
ab_old[, SampleUnit := as.character(SampleUnit)] 
ab_old[, LengthGroup := as.numeric(LengthGroup)] 
ab_old[, Abundance := as.numeric(Abundance)]

# Merge and compare:
mab <- merge(ab_new, ab_old, by.x = c("Stratum", "IndividualTotalLength"), by.y = c("SampleUnit", "LengthGroup"), all = TRUE)

# Test:
tolerance <- 2e-3
expect_true(mab[, all(abs(Abundance.x - Abundance.y) < tolerance, na.rm =  TRUE)])


# Compare super-individuals:
si_old <- data.table::fread(file.path(projectPath3_cod, "output2.7", "16_SuperIndAbundance_SuperIndividuals.txt"))

# Get new resutls:
si_new <- new_cod$SuperIndividuals

# Set class of old results to match class new resutls:
si_old[, Stratum := as.character(Stratum)]

# Order both results and cbind, as we do not have corresponding keys to merge by (samplenumber (catchpartnumber in NMDBiotic 3) used in StoX 2.7 and catchsampleid used in StoX 3):
data.table::setorderv(si_new, c("Stratum", "IndividualTotalLength", "HaulKey", "SpeciesCategory", "SampleKey", "IndividualKey"))
data.table::setorderv(si_old, c("Stratum", "LenGrp", "serialno", "species", "samplenumber", "no"))

#si_old[, plot(Abundance)]
#si_new[, points(Abundance, col = 2)]

# Test:
tolerance <- 1e-4
expect_true(all(abs(si_new$Abundance - si_old$Abundance) < tolerance))



#### Tobis 2011: ####
projectPath3_tobis <- system.file("test", "versus_2.7", "tobis_11.zip", package = "RstoxFramework")
projectPath3_tobis <- unzipProject(projectPath3_tobis, exdir = tempdir())

# Run the cod_20 in current StoX:
new_tobis <- RstoxFramework::runModel(projectPath3_tobis, modelName = "baseline")
# Get new resutls:
ab_new <- new_tobis$Abundance$Data

# Compare abundance:
ab_old <- data.table::fread(file.path(projectPath3_tobis, "output2.7", "19_AbundanceByLength_Abundance.txt"))

# Set class of old results to match class new resutls:
ab_old[, SampleUnit := as.character(SampleUnit)] 
ab_old[, LengthGroup := as.numeric(LengthGroup)] 
ab_old[, Abundance := as.numeric(Abundance)]

# Merge and compare:
mab <- merge(ab_new, ab_old, by.x = c("Stratum", "IndividualTotalLength"), by.y = c("SampleUnit", "LengthGroup"), all = TRUE)
mab[is.na(Abundance.y), Abundance.y := 0]

# Test:
tolerance <- 20
expect_true(mab[, all(abs(Abundance.x - Abundance.y) < tolerance, na.rm =  TRUE)])


# Compare super-individuals:
si_old <- data.table::fread(file.path(projectPath3_tobis, "output2.7", "22_SuperIndAbundance_SuperIndividuals.txt"))

# Get new resutls:
si_new <- new_tobis$SuperIndividuals

# Set class of old results to match class new resutls:
si_old[, Stratum := as.character(Stratum)]

# Order both results and cbind, as we do not have corresponding keys to merge by (samplenumber (catchpartnumber in NMDBiotic 3) used in StoX 2.7 and catchsampleid used in StoX 3):
data.table::setorderv(si_new, c("Stratum", "IndividualTotalLength", "HaulKey", "SpeciesCategory", "SampleKey", "IndividualKey"))
data.table::setorderv(si_old, c("Stratum", "LenGrp", "serialno", "species", "samplenumber", "no"))

#si_old[, plot(Abundance)]
#si_new[, points(Abundance, col = 2)]

# Test:
tolerance <- 20
expect_true(max(mab[, abs(Abundance.x - Abundance.y)]) < tolerance)

