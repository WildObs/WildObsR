## This is a script for remembering what tools can be used to run tests in the R package
## These are often run in the terminal, but this is more of a reference

## Zachary Amir, Z.Amir@uq.edu.au
## Last updated: Oct 9th, 2025

## Load libraries
# library(testthat)    ## For testing functions
library(devtools)    ## For loading all functions in library


## load all functions
devtools::load_all()

## Test one function
devtools::test_active_file("tests/testthat/test-apply_schema_types.R")

# Run all tests in tests/testthat/ (i.e. entire package)
devtools::test()

# Full package check (includes tests + more)
devtools::check()

# Clean compiled code
devtools::clean_dll()

# Update documentation
devtools::document()

# Install package locally
devtools::install()
