if (requireNamespace("tinytest", quietly = TRUE)) {
  # check home based on name of development laptop:
  home <- (Sys.info()["nodename"] == "BvHwork.local")

  tinytest::test_package("KNMIr",
    at_home = home
  )
}
