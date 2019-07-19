
if ( requireNamespace("tinytest", quietly=TRUE) ){
  home <- length(unclass(packageVersion("fastshap"))[[1L]]) == 4
  tinytest::test_package("fastshap", at_home = home)
}

