# fastshap ![](reference/figures/logo-fastshap.png)

The goal of **fastshap** is to provide an efficient and speedy approach
(at least relative to other implementations) for computing approximate
Shapley values, which help explain the predictions from any machine
learning model.

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
