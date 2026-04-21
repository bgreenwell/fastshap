<!--

This file is for AI agents. It describes how to contribute to this repository.
It's based on the AGENTS.md standard: https://agents.md/

-->

# AGENTS.md

This document provides instructions for AI agents on how to contribute to the `fastshap` R package.

## Purpose of the Package

`fastshap` is an R package for computing fast, approximate Shapley values for any supervised learning model. Shapley values are a concept from game theory used to explain the output of machine learning models by attributing the prediction to the input features. The package is designed for performance, using C++ via `Rcpp` and `RcppArmadillo` for the core computations, while providing a user-friendly R interface.

## Development Workflow

The development of this package follows the standard, modern R package development workflow, relying heavily on the `devtools`, `tinytest`, and `pkgdown` packages.

### 1. Core Development with `devtools`

The `devtools` package provides the primary workflow for package development. Common tasks are managed as follows:

-   **Loading Changes:** To load and test new changes during a development session, use `devtools::load_all()`. This simulates installing and loading the package, making new functions and changes available immediately.
-   **Managing Dependencies:** Package dependencies are declared in the `DESCRIPTION` file. Do not add dependencies manually.
-   **Documentation:** The documentation is generated from `roxygen2` comments in the R source files. After adding or updating these comments, run `devtools::document()` to regenerate the `.Rd` files in the `man/` directory and update the `NAMESPACE` file.
-   **Checking the Package:** Before committing any changes, always run a full package check with `devtools::check()`. This command runs `R CMD check`, which is the same process CRAN uses to validate packages. It will run all tests, build vignettes, and check for common problems. Ensure the check completes without any errors, warnings, or notes.

### 2. Testing with `tinytest`

This package uses the `tinytest` framework for unit testing and follows a test-driven development (TDD) approach. `tinytest` is a lightweight, dependency-free testing framework.

-   **Test Location:** All test files are located in the `inst/tinytest/` directory. Each test file should be named `test-*.R`.
-   **Running Tests:** You can run all tests using `tinytest::test_all()` or run specific tests using `tinytest::run_test_file("inst/tinytest/test-*.R")`. The tests are also run automatically as part of `devtools::check()`.
-   **Writing Tests:** When adding new functionality or fixing a bug, a corresponding test should be added *before* the new code is written. Use the `expect_*` functions provided by `tinytest` to make assertions. The existing tests in `inst/tinytest/` should be used as a reference for style and structure. A key testing strategy in this package is to compare the approximate Shapley values against exact values for models where this is possible (e.g., `lm`, `xgboost`).

### 3. Website and Documentation with `pkgdown`

The package website and online documentation are built using `pkgdown`.

-   **Website Configuration:** The website's appearance and structure are configured in the `_pkgdown.yml` file.
-   **Building the Website:** To build the website locally, run `pkgdown::build_site()`. This will generate the site in the `docs/` directory.
-   **Vignettes:** Long-form documentation and examples are provided as vignettes in the `vignettes/` directory. **IMPORTANT:** Due to computational intensity of Shapley value calculations, the main vignette uses a two-file system:
    - `vignettes/fastshap.Rmd.orig` - The source file with executable code chunks that you should edit
    - `vignettes/fastshap.Rmd` - The processed file with pre-computed output for CRAN submission
    
    To update the vignette: (1) Edit `fastshap.Rmd.orig`, (2) Run `knitr::knit("vignettes/fastshap.Rmd.orig", output = "vignettes/fastshap.Rmd")` to generate the final version. The vignettes are compiled and included in the website automatically by `pkgdown`.
-   **Deployment:** The website is deployed automatically via a GitHub Actions workflow defined in `.github/workflows/pkgdown.yaml` whenever changes are pushed to the main branch. You do not need to build the website manually for deployment.

## Summary of Contribution Process

1.  Make changes to the R or C++ source code.
2.  If you add or modify user-facing functions, update the `roxygen2` comments.
3.  Run `devtools::document()` to update documentation.
4.  Add or update tests in `inst/tinytest/`.
5.  Run `devtools::check()` to ensure the package is valid and all tests pass.
6.  Commit your changes.
