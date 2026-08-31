### Skip harness for the tests that talk to the live WildObs database ----
## Anything in test-wildobs_dp_download.R or test-wildobs_mongo_query.R that
## actually queries WildObs has to opt in through the helper below. Without it a
## missing key reads as an empty string, the call fails on a 401, and that
## failure looks like a package bug rather than a missing credential.

## The one place the API key environment variable is named. Both live test
## files read the key through this, so fixing the name here fixes it everywhere.
wildobs_api_key_var <- "WILDOBSR_API_KEY"

#' Skip a test unless this machine can reach the live WildObs database
#'
#' @description Guards every test that performs a real query against WildObs.
#'   Skips on CRAN, skips in CI, and skips locally whenever the API key
#'   environment variable named by `wildobs_api_key_var` is unset or empty. Each
#'   skip carries a message saying which of those three conditions fired, so a
#'   skipped run is never mistaken for a passing one.
#' @return No return value, called for the skip condition it raises.
#' @author Zachary Amir
skip_if_no_wildobs_api <- function() {
  ## CRAN check machines hold no credentials and have no business reaching our
  ## database, so rule them out before anything else
  testthat::skip_on_cran()

  ## GitHub Actions minutes are limited and a live query on every push burns
  ## them for no benefit, so these never fire in CI even if a key were present
  testthat::skip_on_ci()

  ## read the key fresh from the environment, and never print or store it
  api_key <- Sys.getenv(wildobs_api_key_var)

  ## nzchar is FALSE for an unset variable and for an empty string alike, which
  ## are the same problem from the caller's point of view
  if (!nzchar(api_key)) {
    testthat::skip(paste(
      "No WildObs API key found in the environment variable",
      wildobs_api_key_var,
      "\nThese tests query the live WildObs database. Add",
      "your key to .Renviron to run them."
    ))
  } # end missing key condition
} # end skip helper
