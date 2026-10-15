## Tests for wildobs_mongo_query() and wildobs_dp_download() against a fake API ----

## These run offline and in CI. The fake API in helper-fake_api.R replays a recorded
## slice of the public API: three deployments of one open project, and a metadata-only
## copy of it relabelled 'partial' under a made-up ID. The live tests in
## test-wildobs_mongo_query.R and test-wildobs_dp_download.R check the same functions
## against the real database.


#
##
### Downloading an open project ----

test_that("wildobs_dp_download builds a typed Camtrap DP package from API records", {
  fx <- local_fake_wildobs_api()

  dl <- suppressMessages(wildobs_dp_download(api_key = "fake", project_ids = fx$project))

  # one package, named by its project and carrying the camtrapdp class
  expect_named(dl, fx$project)
  dp <- dl[[1]]
  expect_s3_class(dp, "camtrapdp")
  # the three tables without media, as asked
  expect_setequal(frictionless::resource_names(dp), c("deployments", "observations", "covariates"))
  # every recorded row arrives, in the resources and in the dp$data copy alike
  expect_equal(nrow(frictionless::read_resource(dp, "deployments")), nrow(fx$deployments))
  expect_equal(nrow(dp$data$observations), nrow(fx$observations))
  expect_equal(nrow(dp$data$covariates), nrow(fx$covariates))
  # the schemas are applied, so date-times are date-times rather than text
  expect_s3_class(dp$data$deployments$deploymentStart, "POSIXct")
  expect_s3_class(dp$data$observations$eventStart, "POSIXct")
  # the taxonomy from the metadata is joined onto the observations
  expect_true(all(c("taxonID", "taxonRank", "vernacularNamesEnglish") %in%
                    names(dp$data$observations)))
  # and the obscured threatened-species rows come through untouched
  expect_true(any(grepl("^obscured_for_", dp$data$observations$deploymentID)))
})

test_that("wildobs_dp_download rebuilds the descriptor in Camtrap DP form", {
  fx <- local_fake_wildobs_api()

  dp <- suppressMessages(wildobs_dp_download(api_key = "fake", project_ids = fx$project))[[1]]

  # the standard identity and provenance keys are there
  expect_identical(dp$id, fx$project)
  expect_true(all(c("title", "contributors", "licenses", "bibliographicCitation",
                    "spatial", "temporal", "taxonomic") %in% names(dp)))
  # contributors and taxa are lists of objects, as Camtrap DP writes them
  expect_type(dp$contributors, "list")
  expect_false(is.null(dp$contributors[[1]]$title))
  expect_false(is.null(dp$taxonomic[[1]]$scientificName))
  # the package-level temporal extent and time zone
  expect_true(all(c("start", "end", "timeZone") %in% names(dp$temporal)))
  # spatial is GeoJSON
  expect_identical(dp$spatial$type, "FeatureCollection")
  # and every table is a v1 tabular resource, as Camtrap DP 1.0.x requires
  profiles <- vapply(dp$resources, function(r) r$profile, character(1))
  expect_true(all(profiles == "tabular-data-resource"))
})

test_that("wildobs_dp_download adds media in batches when asked", {
  fx <- local_fake_wildobs_api()

  # the progress messages report the count the server gave
  expect_message(
    dl <- wildobs_dp_download(api_key = "fake", project_ids = fx$project, media = TRUE),
    sprintf("Downloading %d media records", nrow(fx$media))
  )
  dp <- dl[[1]]
  # media joins the other three tables, with every recorded row and no repeats
  expect_true("media" %in% frictionless::resource_names(dp))
  expect_equal(nrow(dp$data$media), nrow(fx$media))
  expect_false(any(duplicated(dp$data$media$mediaID)))
})

test_that("wildobs_dp_download with metadata_only returns packages without tables", {
  fx <- local_fake_wildobs_api()

  # a quick download says it skipped the tables
  expect_message(
    dl <- wildobs_dp_download(api_key = "fake", project_ids = fx$project, metadata_only = TRUE),
    "metadata_only = TRUE"
  )
  # the descriptor is there, the tables are not
  expect_identical(dl[[1]]$id, fx$project)
  expect_length(dl[[1]]$resources, 0)
})


#
##
### Partial projects and unknown IDs ----

test_that("wildobs_dp_download returns a partial project as metadata only beside an open one", {
  fx <- local_fake_wildobs_api()

  # asking for both sends one $in request per table
  dl <- suppressMessages(wildobs_dp_download(api_key = "fake",
                                             project_ids = c(fx$project, fx$partial_id)))

  # both come back, in the order asked for
  expect_named(dl, c(fx$project, fx$partial_id))
  # the open one has its tables
  expect_length(dl[[fx$project]]$resources, 3)
  # the partial one is metadata only, so code can skip it by its empty resources
  expect_length(dl[[fx$partial_id]]$resources, 0)
  expect_identical(dl[[fx$partial_id]]$WildObsMetadata$tabularSharingPreference, "partial")
})

test_that("wildobs_dp_download skips unknown project IDs with a warning", {
  fx <- local_fake_wildobs_api()

  # the unknown ID is named in the warning
  expect_warning(
    dl <- suppressMessages(wildobs_dp_download(api_key = "fake",
                                               project_ids = c(fx$project, "not_a_project"),
                                               metadata_only = TRUE)),
    "1 of 2 project IDs were not found.*not_a_project"
  )
  # and the known one still downloads
  expect_named(dl, fx$project)
})


#
##
### Querying the metadata ----

## a box around the recorded project's cameras in the Wet Tropics
wet_tropics <- list(xmin = 144.9, xmax = 146.5, ymin = -19.6, ymax = -15.3)

test_that("wildobs_mongo_query returns open and partial projects by default, offline", {
  fx <- local_fake_wildobs_api()

  # both recorded projects sit in the box
  expect_setequal(wildobs_mongo_query(api_key = "fake", spatial = wet_tropics),
                  c(fx$project, fx$partial_id))
  # asking for open projects drops the embargoed copy
  expect_identical(wildobs_mongo_query(api_key = "fake", spatial = wet_tropics,
                                       tabularSharingPreference = "open"),
                   fx$project)
})

test_that("wildobs_mongo_query filters on every criterion, offline", {
  fx <- local_fake_wildobs_api()
  # only the open project, so each filter has exactly one project to keep or drop
  q <- function(...) suppressWarnings(wildobs_mongo_query(api_key = "fake",
                                                          tabularSharingPreference = "open", ...))

  # a window over the 2022-23 field season keeps it, one years before drops it
  expect_identical(q(temporal = list(minDate = as.Date("2022-06-01"),
                                     maxDate = as.Date("2023-06-01"))), fx$project)
  expect_length(q(temporal = list(minDate = as.Date("2015-01-01"),
                                  maxDate = as.Date("2016-01-01"))), 0)
  # a species it recorded keeps it, a species it could not have recorded drops it
  expect_identical(q(taxonomic = "Thylogale stigmatica"), fx$project)
  expect_length(q(taxonomic = "Ailuropoda melanoleuca"), 0)
  # its own sampling design keeps it, another drops it
  expect_identical(q(samplingDesign = "systematicRandom"), fx$project)
  expect_length(q(samplingDesign = "opportunistic"), 0)
  # its maintainer finds it by name, a stranger does not
  expect_identical(q(contributors = "Zachary Amir"), fx$project)
  expect_length(q(contributors = "Nobody Atall"), 0)
  # a box on the other side of the country drops it
  expect_length(q(spatial = list(xmin = 115, xmax = 116, ymin = -32, ymax = -31)), 0)
})

test_that("wildobs_mongo_query drops closed projects without admin access, offline", {
  local_fake_wildobs_api()

  # an API key is never admin, so closed is removed with a warning
  expect_warning(
    wildobs_mongo_query(api_key = "fake", spatial = wet_tropics,
                        tabularSharingPreference = c("open", "closed")),
    "have not provided admin credentials"
  )
})

test_that("wildobs_mongo_query stops when the metadata request fails", {
  # the API refuses the request, as it does for a rejected key
  local_mocked_bindings(.check_wildobs_version = function() invisible(NULL))
  local_mocked_bindings(
    POST = function(...) structure(list(url = "https://example.org/find", status_code = 401L,
                                        content = raw(0), headers = list()),
                                   class = "response"),
    .package = "httr"
  )
  expect_error(wildobs_mongo_query(api_key = "fake", spatial = wet_tropics),
               "Status code: 401")
})
