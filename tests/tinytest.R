if ( requireNamespace("tinytest", quietly = TRUE) ){
  # check home based on name of development laptop:
  home <- (Sys.info()["nodename"] == "LenovoY700")
  # check home based on package version
  # home <- length(unclass(packageVersion("KNMIr"))[[1]]) == 4

  tinytest::test_package("KNMIr",
                         at_home = home
                         )
}
