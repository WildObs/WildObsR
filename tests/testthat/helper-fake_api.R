### A fake WildObs API for offline tests ----
## wildobs_mongo_query() and wildobs_dp_download() are mostly code that reshapes API
## responses into data packages. These helpers replay a small recorded slice of the
## public API (fixtures/api_fixture.rds, built by fixtures/make_api_fixture.R) so that
## code runs in CI without a key. The paging helpers themselves are tested separately
## in test-wildobs_dp_download.R.

#' Load the recorded API slice
#'
#' @return A list holding the raw metadata response as JSON text, the open project's
#'   ID, the made-up partial project's ID, and the deployments, covariates,
#'   observations and media tables of three of the open project's deployments.
#' @author Zachary Amir
fake_api_fixture <- function() {
  return(readRDS(testthat::test_path("fixtures", "api_fixture.rds")))
} # end fixture loader

#' Keep the rows of a recorded table that a request's project filter asks for
#'
#' @param tab A recorded table.
#' @param filter The request filter, whose `projectName` is one ID or a `$in` list.
#' @return The rows of `tab` belonging to the requested projects.
#' @author Zachary Amir
fake_api_rows <- function(tab, filter) {
  # one project is sent as a plain string, several as a $in list
  wanted <- filter$projectName
  if (is.list(wanted)) wanted <- unlist(wanted[["$in"]])
  # keep only those projects' rows
  return(tab[tab$projectName %in% wanted, , drop = FALSE])
} # end row filter

#' Point the package at the fake API for the rest of a test
#'
#' @description Replaces the metadata request (`httr::POST()`), the paged table
#'   download, the media batches and the media count with versions that answer from
#'   the recorded slice, and silences the once-a-session version check. Every
#'   replacement ends with the test that called this.
#' @param fx The recorded slice, from `fake_api_fixture()`.
#' @param env The test environment the replacements are tied to.
#' @return The recorded slice, invisibly.
#' @author Zachary Amir
local_fake_wildobs_api <- function(fx = fake_api_fixture(), env = parent.frame()) {
  ## the metadata request answers with the recorded JSON, as a real httr response would
  testthat::local_mocked_bindings(
    POST = function(...) {
      structure(list(url = "https://example.org/find", status_code = 200L,
                     content = charToRaw(fx$metadata_json),
                     headers = list(`Content-Type` = "application/json")),
                class = "response")
    }, # end fake POST
    .package = "httr", .env = env
  )

  ## the package's own API helpers answer from the recorded tables
  testthat::local_mocked_bindings(
    # the version check would reach GitHub, so it stays quiet
    .check_wildobs_version = function() invisible(NULL),
    # each paged table comes back whole for the requested projects
    .wildobs_api_find_paged = function(url, api_key, collection, filter, key, page_size,
                                       shared_key_pattern = NULL) {
      fake_api_rows(fx[[collection]], filter)
    }, # end fake paged find
    # the media count is the number of recorded media rows for those projects
    .wildobs_api_count = function(url, api_key, collection, filter) {
      nrow(fake_api_rows(fx[[collection]], filter))
    }, # end fake count
    # media come in batches after the last mediaID, sorted, up to the batch size
    .wildobs_api_find = function(url, api_key, body, what, attempts = 3) {
      # this project's media
      rows <- fake_api_rows(fx[[body$collection]], body$filter)
      # only those after the last ID already downloaded
      if (!is.null(body$filter$mediaID)) rows <- rows[rows$mediaID > body$filter$mediaID[["$gt"]], ]
      # in key order
      rows <- rows[order(rows$mediaID), , drop = FALSE]
      # an exhausted collection returns an empty list, as the API does
      if (nrow(rows) == 0) return(list())
      # and no more than one batch
      utils::head(rows, body$limit)
    }, # end fake find
    .env = env
  )
  return(invisible(fx))
} # end fake API
