## Tests for wildobs_mongo_query() ----

## Every test below that reaches the database opens with the
## skip_if_no_wildobs_api guard from helper-wildobs.R. The set of projects in WildObs grows over time, so
## these assert structure and membership only: that the anchor project comes
## back, that ids are unique and well formed. Never a length, never a full set.

# General use WildObs API key, named once in helper-wildobs.R
test_api_key <- Sys.getenv(wildobs_api_key_var)

## the curated stable project, the anchor every filter is checked against
stable_project <- "ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001"

## wildobs_mongo_query() has no project id argument, so a query cannot be
## narrowed to one project directly. Filtering by its maintainer is the closest
## stable handle we have on the anchor project.
anchor_contributor <- "Zachary Amir"

## the contributor query is reused across several tests, so run it once and hold
## the result in a deliberate environment rather than repeating the round trip
query_cache <- new.env(parent = emptyenv())

## fetch the anchor query, lazily so a skipped run never hits the network
anchor_query <- function() {
  if (is.null(query_cache$anchor)) {
    query_cache$anchor <- wildobs_mongo_query(
      api_key = test_api_key,
      contributors = anchor_contributor
    )
  } # end first call condition
  return(query_cache$anchor)
} # end anchor query accessor


#
##
### Credential handling, no network required ----

test_that("wildobs_mongo_query errors when neither api_key nor db_url provided", {
  # with no way to reach the database the function refuses up front rather than
  # failing somewhere deeper with a less useful message
  expect_error(
    wildobs_mongo_query(),
    "You have not provided an API key or a database URL"
  )
})

test_that("wildobs_mongo_query errors with invalid db_url format", {
  # a malformed URI is caught by pattern before any connection is attempted
  expect_error(
    wildobs_mongo_query(db_url = "invalid_url"),
    "must be a valid MongoDB URI"
  )
})

test_that("wildobs_mongo_query prioritizes db_url over api_key", {
  # a db_url always wins over an api_key. Supplying both, with the URL
  # malformed, must fail on the URL pattern: if it silently fell back to the API
  # route the user would be querying a database they did not ask for.
  expect_error(
    wildobs_mongo_query(db_url = "invalid_url", api_key = "any_key_at_all"),
    "must be a valid MongoDB URI"
  )
})


#
##
### What a query returns ----

test_that("wildobs_mongo_query returns character vector of project IDs", {
  skip_if_no_wildobs_api()

  result <- anchor_query()

  # the return is a plain character vector of ids, and every entry has to be a
  # real id rather than the empty string the no-match path produces
  expect_type(result, "character")
  expect_true(all(nzchar(result)))
})

test_that("wildobs_mongo_query returns unique project IDs", {
  skip_if_no_wildobs_api()

  result <- anchor_query()

  # filters are combined by intersection across several metadata tables, and a
  # project appearing twice would mean one of those tables was joined wrongly
  expect_identical(result, unique(result))
})

test_that("wildobs_mongo_query project IDs follow naming convention", {
  skip_if_no_wildobs_api()

  result <- anchor_query()

  # every WildObs project id carries the WildObsID stamp, so all of them match,
  # not merely one of them
  expect_true(all(grepl("WildObsID", result)))
})


#
##
### Individual filters ----

test_that("wildobs_mongo_query spatial parameter filters correctly", {
  skip_if_no_wildobs_api()

  # a box over the Queensland Wet Tropics, where the anchor project sits
  spatial_query <- list(xmin = 145.0, xmax = 147.0, ymin = -20.0, ymax = -16.0)

  result <- wildobs_mongo_query(api_key = test_api_key, spatial = spatial_query)

  # a bounding box that covers the anchor project has to return it. Membership
  # rather than length, since new projects in this box would break a count.
  expect_type(result, "character")
  expect_true(stable_project %in% result)
})

test_that("wildobs_mongo_query temporal parameter filters correctly", {
  skip_if_no_wildobs_api()

  # a window spanning the anchor project's field season
  temporal_query <- list(
    minDate = as.Date("2022-01-01"),
    maxDate = as.Date("2025-01-01")
  )

  result <- wildobs_mongo_query(
    api_key = test_api_key,
    temporal = temporal_query
  )

  # a date range overlapping the anchor project's deployments returns it
  expect_type(result, "character")
  expect_true(stable_project %in% result)
})

test_that("wildobs_mongo_query taxonomic parameter filters correctly", {
  skip_if_no_wildobs_api()

  # two species the anchor project detected
  taxa_query <- c("Phascolarctos cinereus", "Tachyglossus aculeatus")

  result <- wildobs_mongo_query(api_key = test_api_key, taxonomic = taxa_query)

  # asking for a species returns every project that detected any of them
  expect_type(result, "character")
  expect_true(stable_project %in% result)
})

test_that("wildobs_mongo_query samplingDesign parameter filters correctly", {
  skip_if_no_wildobs_api()

  sample_query <- c("simpleRandom", "opportunistic", "systematicRandom")

  result <- wildobs_mongo_query(
    api_key = test_api_key,
    samplingDesign = sample_query
  )

  # the anchor project uses one of these designs, so it comes back
  expect_type(result, "character")
  expect_true(stable_project %in% result)
})

test_that("wildobs_mongo_query accepts valid samplingDesign enumerations", {
  skip_if_no_wildobs_api()

  ## the full enumerated vocabulary, which between them cover every project
  valid_designs <- c(
    "simpleRandom", "systematicRandom", "clusteredRandom",
    "experimental", "targeted", "opportunistic"
  )

  result <- wildobs_mongo_query(
    api_key = test_api_key,
    samplingDesign = valid_designs
  )

  # every enumerated value is accepted rather than rejected as unknown, which is
  # what asking for all six at once proves
  expect_type(result, "character")
  expect_true(all(nzchar(result)))
  expect_true(stable_project %in% result)
})

test_that("wildobs_mongo_query contributors parameter filters correctly", {
  skip_if_no_wildobs_api()

  result <- anchor_query()

  # a contributor's name finds the projects they are attached to, whatever role
  # they hold on them
  expect_type(result, "character")
  expect_true(stable_project %in% result)
})

#
##
### Combining filters ----

test_that("wildobs_mongo_query combines multiple filters with intersection", {
  skip_if_no_wildobs_api()

  spatial_query <- list(xmin = 145.0, xmax = 147.0, ymin = -20.0, ymax = -16.0)
  temporal_query <- list(
    minDate = as.Date("2022-01-01"),
    maxDate = as.Date("2025-01-01")
  )

  # each filter on its own, to compare the combination against
  spatial_only <- wildobs_mongo_query(
    api_key = test_api_key,
    spatial = spatial_query
  )
  temporal_only <- wildobs_mongo_query(
    api_key = test_api_key,
    temporal = temporal_query
  )
  # and then both together
  combined <- wildobs_mongo_query(
    api_key = test_api_key,
    spatial = spatial_query,
    temporal = temporal_query
  )

  # filters intersect rather than accumulate, so the combined result can only
  # ever be a subset of what either filter returns alone. Stating it as a subset
  # relationship keeps the test true however many projects are added.
  expect_true(all(combined %in% spatial_only))
  expect_true(all(combined %in% temporal_only))

  # and the anchor project satisfies both, so it survives the intersection
  expect_true(stable_project %in% combined)
})


#
##
### Sharing preferences and access ----

test_that("wildobs_mongo_query warns when requesting closed data without admin", {
  skip_if_no_wildobs_api()

  ## anchored on the contributor filter deliberately: an unfiltered call would
  ## also raise the no-match warning, and expect_warning() captures only one
  ## condition while the other escapes and shows up as a stray WARN
  expect_warning(
    wildobs_mongo_query(
      api_key = test_api_key,
      contributors = anchor_contributor,
      tabularSharingPreference = c("open", "closed")
    ),
    "have not provided admin credentials"
  )
})


#
##
### The no-match contract ----

test_that("wildobs_mongo_query warns and returns an empty string with no filters", {
  skip_if_no_wildobs_api()

  # with nothing to filter on, every branch contributes nothing, so the
  # intersection is empty and the user is told so rather than handed everything
  expect_warning(
    result <- wildobs_mongo_query(api_key = test_api_key),
    "no matches in our database"
  )

  ## NOTE: "" is a questionable way to report "nothing matched". A caller
  ## testing length(result) > 0 gets TRUE, and looping over it gives one turn
  ## holding an empty string, so both are misled. character(0) is the correct
  ## representation. Logged as a future change, not made now; this test pins
  ## today's behaviour so that change shows up in a diff rather than silently.
  expect_type(result, "character")
  expect_length(result, 1)
  expect_identical(result, "")
})

test_that("wildobs_mongo_query warns when no matches found", {
  skip_if_no_wildobs_api()

  # a box in the Gulf of Guinea, where WildObs will never hold a camera
  spatial_query <- list(xmin = 0.0, xmax = 0.01, ymin = 0.0, ymax = 0.01)

  # filters that match nothing land on the same empty-string sentinel as no
  # filters at all, and warn for the same reason
  expect_warning(
    result <- wildobs_mongo_query(
      api_key = test_api_key,
      spatial = spatial_query
    ),
    "no matches in our database"
  )
  expect_identical(result, "")
})

test_that("wildobs_mongo_query treats an empty contributors vector as no filter", {
  skip_if_no_wildobs_api()

  # a zero-length filter is not a filter that matches nothing, it is the absence
  # of a filter, so this lands on the no-match path rather than erroring
  expect_warning(
    result <- wildobs_mongo_query(
      api_key = test_api_key,
      contributors = character(0)
    ),
    "no matches in our database"
  )
  expect_identical(result, "")
})


#
##
### Input validation and failure handling ----

test_that("wildobs_mongo_query validates spatial parameter structure", {
  skip_if_no_wildobs_api()

  # a bounding box missing its latitude bounds
  spatial_incomplete <- list(xmin = 145.0, xmax = 147.0)

  ## no regexp here on purpose: an incomplete box currently fails inside
  ## tibble's row subsetting, so that text belongs to another package and would
  ## change under us. What matters is that it stops, not how it words it.
  expect_error(
    wildobs_mongo_query(api_key = test_api_key, spatial = spatial_incomplete)
  )
})

test_that("wildobs_mongo_query accepts character dates in the temporal filter", {
  skip_if_no_wildobs_api()

  # dates written as strings rather than built with as.Date()
  temporal_character <- list(minDate = "2022-01-01", maxDate = "2025-01-01")

  result <- wildobs_mongo_query(
    api_key = test_api_key,
    temporal = temporal_character
  )

  # these are coerced rather than rejected, so an ecologist who passes the dates
  # the way they would write them still gets their projects back
  expect_type(result, "character")
  expect_true(stable_project %in% result)
})

test_that("wildobs_mongo_query handles API connection failure gracefully", {
  skip_if_no_wildobs_api()

  # a rejected key surfaces as our own message naming the failed step, rather
  # than as whatever the HTTP layer happened to return
  expect_error(
    wildobs_mongo_query(api_key = "invalid_key"),
    "Failed to retrieve metadata"
  )
})
