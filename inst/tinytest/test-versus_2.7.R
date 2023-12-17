# Test of StoX 2.7 versus current version:


#### 1. Cod 2020: ####

# Define path to the project, which contains both StoX 2.7 and 3:
projectPath3_cod <- system.file("test", "versus_2.7", "cod_20.zip", package = "RstoxFramework")
projectPath3_cod <- unzipProject(projectPath3_cod, exdir = tempdir())

# Run the cod_20 in current StoX:
new_cod <- runModel(projectPath3_cod, modelName = "baseline", msg = FALSE)

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
# mab[, summary(abs(Abundance.x - Abundance.y))]
# From 3.5.0 and onwards:
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.000000 0.000121 0.000260 0.000282 0.000411 0.001242       25 

tolerance <- 2e-3
expect_true(mab[, all(abs(Abundance.x - Abundance.y) < tolerance, na.rm =  TRUE)])


# Compare super-individuals:
# Order both results and cbind, as we do not have corresponding keys to merge by (samplenumber (catchpartnumber in NMDBiotic 3) used in StoX 2.7 and catchsampleid used in StoX 3):
si_new <- subset(si_new, !is.na(SpeciesCategory) & !is.na(Layer) & !is.na(Stratum))
data.table::setorderv(si_new, c("Stratum", "IndividualTotalLength", "HaulKey", "SpeciesCategory", "SampleKey", "IndividualKey"))
data.table::setorderv(si_old, c("Stratum", "LenGrp", "serialno", "species", "samplenumber", "no"))

# Test:
# summary(abs(si_new$Abundance - si_old$Abundance))
# From 3.5.0 and onwards:
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 3.190e-09 1.230e-05 2.526e-05 2.555e-05 3.884e-05 1.232e-04 
tolerance <- 2e-4
expect_true(all(abs(si_new$Abundance - si_old$Abundance) < tolerance))





#### 2. Tobis 2011: ####

# Define path to the project, which contains both StoX 2.7 and 3:
projectPath3_tobis <- system.file("test", "versus_2.7", "tobis_11.zip", package = "RstoxFramework")
projectPath3_tobis <- unzipProject(projectPath3_tobis, exdir = tempdir())

# Run the tobis_11 in current StoX:
new_tobis <- runModel(projectPath3_tobis, modelName = "baseline", msg = FALSE)

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
si_new <- subset(si_new, !is.na(SpeciesCategory) & !is.na(Layer) & !is.na(Stratum))
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
new_Angola <- runModel(projectPath3_Angola, modelName = "baseline", msg = FALSE)

# Get the new SplitNASC:
sn_new <- new_Angola$SplitNASC

# Read old SplitNASC:
sn_old <- readStoX2.7OutputFile(file.path(projectPath3_Angola, "output", "baseline", "data", "14_SplitNASC_NASC.txt"))
convertKeys2.7To3(sn_old)


# Compare SplitNASC:
m <- merge(sn_old, sn_new, by.x = c("AcoCat", "SampleUnit", "Layer"), by.y = c("AcousticCategory", "EDSU", "Channel"), all = TRUE)
tolerance <- 5e-5
expect_true(m[, max(abs(NASC.y - NASC.x) / NASC.y, na.rm = TRUE)] < tolerance)





#### 4. Capelin 2021: ####

# Define path to the project, which contains both StoX 2.7 and 3:
projectPath3_capelin <- system.file("test", "versus_2.7", "capelin_21.zip", package = "RstoxFramework")
projectPath3_capelin <- unzipProject(projectPath3_capelin, exdir = tempdir())

# Run the capelin_21 in current StoX:
new_capelin <- runModel(projectPath3_capelin, modelName = "baseline", msg = FALSE)

# Get the new results:
# Abundance:
ab_new <- new_capelin$Abundance$Data

# Read old results:
# Abundance:
ab_old <- readStoX2.7OutputFile(file.path(projectPath3_capelin, "output", "baseline", "data", "19_Abundance_Abundance.txt"))


# Compare abundance:
mab <- merge(ab_new, ab_old, by.x = c("Stratum", "IndividualTotalLength"), by.y = c("SampleUnit", "LengthGroup"), all = TRUE)
suppressWarnings(diff <- mab[, .( diff = max(abs(Abundance.y - Abundance.x) / Abundance.y, na.rm = TRUE)), by = "Stratum"])


# Test:
tolerance <- data.table::data.table(
    Stratum = c(
        "Shelf edge", 
        "Spitzbergen bank"
    ), 
    tolerance = c(
        5e-11, # No diff.
        6.9e-02 # Difff due to different projection in 2.7 and >= 3.0.0, kicking in close to the turns.
    )
)
diff <- merge(diff, tolerance, by = "Stratum")
expect_true(diff[, all(diff < tolerance)])






#### 5. CoastalCod 2020: ####

# Define path to the project, which contains both StoX 2.7 and 3:
projectPath3_coastalCod <- system.file("test", "versus_2.7", "coastalCod_21.zip", package = "RstoxFramework")
projectPath3_coastalCod <- unzipProject(projectPath3_coastalCod, exdir = tempdir())

# Run the coastalCod_21 in current StoX:
new_coastalCod <- runModel(projectPath3_coastalCod, modelName = "baseline", msg = FALSE)

# Get the new results:
# Abundance:
si_new <- new_coastalCod$SuperIndividuals

# Read old results:
# Abundance:
si_old <- readStoX2.7OutputFile(file.path(projectPath3_coastalCod, "output", "baseline", "data", "25_SuperIndAbundance_SuperIndividuals.txt"))

# Compare super-individual abundance:
data.table::setorderv(si_new,c( "HaulKey",  "IndividualTotalLength"))
data.table::setorderv(si_old, c("serialno",   "LenGrp"))
expect_true(all(si_old$Abundance == si_old$Abundance))

