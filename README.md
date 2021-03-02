
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fastshap <img src='man/figures/logo-fastshap.png' align="right" height="139" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/fastshap)](https://CRAN.R-project.org/package=fastshap)
[![R-CMD-check](https://github.com/bgreenwell/fastshap/workflows/R-CMD-check/badge.svg)](https://github.com/bgreenwell/fastshap/actions)
[![Codecov test
coverage](https://codecov.io/gh/bgreenwell/fastshap/branch/master/graph/badge.svg)](https://codecov.io/gh/bgreenwell/fastshap?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of **fastshap** is to provide an efficient and speedy (relative
to other implementations) approach to computing approximate Shapley
values which help explain the predictions from machine learning models.

![](https://media.giphy.com/media/26AHLNr8en8J3ovOo/giphy.gif)

## Installation

``` r
# Install the latest stable version from CRAN:
install.packages("fastshap")

# Install the latest development version from GitHub:
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}
remotes::install_github("bgreenwell/fastshap")
```
