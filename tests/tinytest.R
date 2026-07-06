# Run the full suite locally and on CI (where NOT_CRAN=true); CRAN-style
# checks without NOT_CRAN run only the ungated tests. Note that
# tinytest::at_home() only checks TT_AT_HOME, which neither CI nor
# `R CMD check` sets, so derive the flag explicitly.
if (requireNamespace("tinytest", quietly = TRUE)) {
  home <- length(unclass(packageVersion("fastshap"))[[1L]]) == 4L ||
    identical(tolower(Sys.getenv("NOT_CRAN")), "true")
  tinytest::test_package("fastshap", at_home = home)
}
