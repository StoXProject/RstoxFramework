library(testthat)
library(RstoxFramework)

# We have currently 10 test projects:
options(Ncpus = min(10L, parallel::detectCores()))

test_check("RstoxFramework")