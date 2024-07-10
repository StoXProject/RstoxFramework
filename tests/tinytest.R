if ( requireNamespace("tinytest", quietly=TRUE) ){
    # Run in parallel. By default tinytest::test_package() complies with the maximum 2 core policy of CRAN. To use more cores than 2, we need to tell parallel:::.check_ncores(), used by parallel::makePSOCKcluster(), used by parallel::makeCluster(), used by tinytest::test_package() to not throw an error but a warning:
    #Sys.setenv("_R_CHECK_LIMIT_CORES_" = "warn")
    #tinytest::test_package("RstoxFramework", ncpu = 4)
    
    tinytest::test_package("RstoxFramework")
}