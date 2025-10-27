## This is a script for remembering what tools can be used to run tests in the R package
## These are often run in the terminal, but this is more of a reference

## Zachary Amir, Z.Amir@uq.edu.au
## Last updated: Oct 9th, 2025

## Load libraries
library(testthat)    ## For testing functions
library(devtools)    ## For loading all functions in library
library(covr)        ## For checking code coverage

# Typical development cycle:
devtools::load_all()           # Load your code
devtools::test()               # Run all tests
covr::report()                 # Check coverage
devtools::check()              # Full package check before release


#### Devtools ####

## load all functions
devtools::load_all()

## Test one function
devtools::test_active_file("tests/testthat/test-extract_metadata.R")

# Run all tests in tests/testthat/ (i.e. entire package)
devtools::test()

# Full package check (includes tests + more)
# devtools::check()

# Clean compiled code
# devtools::clean_dll()

# Update documentation
# devtools::document()


##### testthat #####

# Run a specific test file
testthat::test_file("tests/testthat/test-utils.R")

# Run tests with different reporters
devtools::test(reporter = "progress")  # More detail
devtools::test(reporter = "stop")      # Stop at first failure

# Common expectations (assertions)
expect_equal(actual, expected)         # Values equal (with tolerance)
expect_identical(actual, expected)     # Exactly identical
expect_true(condition)
expect_false(condition)
expect_error(code, message)            # Expects an error
expect_warning(code, message)          # Expects a warning
expect_null(object)
expect_length(object, n)
expect_type(object, type)
expect_s3_class(object, class)
expect_named(object, names)

# Test structure
test_that("description of what you're testing", {
  # Arrange - set up test data
  # Act - run the function
  # Assert - check results with expect_*()
})

##### covr #####

# Generate interactive HTML coverage report
covr::report()

# Get coverage object
cov <- covr::package_coverage()

# View coverage summary
cov

# Find functions with zero coverage
covr::zero_coverage(cov)

# Coverage for specific file
covr::file_coverage(
  source_files = "R/utils.R",
  test_files = "tests/testthat/test-utils.R"
)

# Set up CI/CD coverage tracking (GitHub)
usethis::use_coverage()
usethis::use_github_action("test-coverage")
