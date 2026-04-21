# CLAUDE.md

This repository contains comprehensive development guidelines for AI agents in the `AGENTS.md` file. Please refer to that file for detailed instructions on contributing to the `fastshap` R package.

## Quick Reference

For Claude Code users working on this package:

### Essential Commands
- `devtools::load_all()` - Load changes during development
- `devtools::document()` - Update documentation from roxygen2 comments
- `devtools::check()` - Run full package validation (always run before committing)
- `tinytest::test_all()` - Run all tests
- `tinytest::run_test_file("inst/tinytest/test-*.R")` - Run specific test files

### Key Development Notes
- This is an R package for fast approximate Shapley value computation
- Uses C++ via Rcpp/RcppArmadillo for performance-critical operations
- Follows test-driven development with `tinytest` framework
- Tests are located in `inst/tinytest/`
- Always run `devtools::check()` to ensure package validity before committing

See `AGENTS.md` for complete development workflow and contribution guidelines.