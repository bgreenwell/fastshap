# fastshap ![](reference/figures/logo-fastshap.png)

The goal of **fastshap** is to provide an efficient and speedy approach
(at least relative to other implementations) for computing approximate
Shapley values, which help explain the predictions from any machine
learning model.

![](https://media.giphy.com/media/26AHLNr8en8J3ovOo/giphy.gif)

## Installation

**fastshap** is no longer available on CRAN due to CRAN’s stringent and
ever-changing policies. It is now hosted on
[r-universe](https://bgreenwell.r-universe.dev/fastshap), which provides
a reliable alternative for distributing R packages.

``` r

# Install from r-universe (recommended):
install.packages("fastshap", repos = c("https://bgreenwell.r-universe.dev", "https://cloud.r-project.org"))

# Install the latest development version from GitHub:
if (!requireNamespace("pak")) {
  install.packages("pak")
}
pak::pak("bgreenwell/fastshap")
```
