library(testthat)
library(RstoxFramework)

# We have currently three test projects:
options(Ncpus = min(10L, parallel::detectCores()))

test_check("RstoxFramework")