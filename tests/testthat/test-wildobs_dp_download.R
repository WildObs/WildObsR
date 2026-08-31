## Tests for wildobs_dp_download() ----

## Every test below that reaches the database opens with skip_if_no_wildobs_api()
## from helper-wildobs.R. These assert structure only, never content: the live
## database gains observations and deployments continually, so a test that
## pins a row count or a species name breaks for the wrong reason.

# General use WildObs API key, named once in helper-wildobs.R
test_api_key <- Sys.getenv(wildobs_api_key_var)

## the curated stable project, used as the structural fixture throughout
stable_project <- "ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001"

## a second open project, small enough to pair with the fixture cheaply and to
## carry the media test. Media on the stable project runs to roughly 1.4 million
## records, which is far too slow to download in a test run; this one is ~21k.
second_project <- "QLD_Dwyers_Scrub_ANIM3018_2023_WildObsID_0005"

## Downloads are the slow part of this file, so each distinct one happens once
## and is reused. A deliberate environment rather than <<-, so the data flow
## stays traceable, and lazy rather than eager so a skipped run never fetches.
download_cache <- new.env(parent = emptyenv())

## grab the single-project download, fetching it the first time it is asked for
stable_dp_list <- function() {
  if (is.null(download_cache$stable)) {
    download_cache$stable <- wildobs_dp_download(
      api_key = test_api_key,
      project_ids = stable_project
    )
  } # end first call condition
  return(download_cache$stable)
} # end stable list accessor

## and the data package itself, since most tests want the package not the list
stable_dp <- function() {
  return(stable_dp_list()[[1]])
} # end stable package accessor

## a two-project download, for the tests that compare packages to each other
paired_dp_list <- function() {
  if (is.null(download_cache$paired)) {
    download_cache$paired <- wildobs_dp_download(
      api_key = test_api_key,
      project_ids = c(stable_project, second_project)
    )
  } # end first call condition
  return(download_cache$paired)
} # end paired list accessor

## $resources is an unnamed list, so a resource name has to be read out of each
## element rather than off names(). Getting this wrong is silent: names()
## returns NULL and every %in% check against it quietly reports FALSE.
resource_names <- function(dp) {
  return(vapply(dp$resources, function(r) r$name, character(1)))
} # end resource name helper

## pull one resource out by name, since we can only match on the name field
get_resource <- function(dp, resource) {
  return(dp$resources[[which(resource_names(dp) == resource)]])
} # end resource accessor

## schema fields are likewise an unnamed list of field definitions
schema_field_names <- function(dp, resource) {
  fields <- get_resource(dp, resource)$schema$fields
  return(vapply(fields, function(f) f$name, character(1)))
} # end schema field name helper


#
##
### Credential handling, no network required ----

test_that("wildobs_dp_download errors when neither api_key nor db_url provided", {
  # with no way to reach the database at all the function has to refuse up front
  # rather than fail somewhere deeper with a less useful message
  expect_error(
    wildobs_dp_download(project_ids = stable_project),
    "You have not provided an API key or a database URL"
  )
})

test_that("wildobs_dp_download errors with invalid db_url format", {
  # a malformed URI is caught by pattern before any connection is attempted, so
  # the user is told their string is wrong rather than that the server is down
  expect_error(
    wildobs_dp_download(db_url = "invalid_url", project_ids = stable_project),
    "must be a valid MongoDB URI"
  )
})

test_that("wildobs_dp_download prioritizes db_url over api_key", {
  # a db_url always wins over an api_key. Supplying both, with the URL
  # malformed, must fail on the URL pattern: if it silently fell back to the API
  # route the user would get data from a database they did not ask for.
  expect_error(
    wildobs_dp_download(
      db_url = "invalid_url",
      api_key = "any_key_at_all",
      project_ids = stable_project
    ),
    "must be a valid MongoDB URI"
  )
})


#
##
### Shape of the returned object ----

test_that("wildobs_dp_download returns a list named by project ID", {
  skip_if_no_wildobs_api()

  result <- stable_dp_list()

  # the return is a plain list, keyed so callers can pull a project by its id
  expect_type(result, "list")
  expect_named(result, stable_project)
})

test_that("wildobs_dp_download handles single project ID", {
  skip_if_no_wildobs_api()

  result <- stable_dp_list()

  # one requested id yields exactly one package, never a package plus extras
  expect_length(result, 1)
})

test_that("wildobs_dp_download handles multiple project IDs", {
  skip_if_no_wildobs_api()

  result <- paired_dp_list()

  # asking for two projects returns both, named in the order requested, so a
  # caller can rely on the names rather than on position
  expect_length(result, 2)
  expect_named(result, c(stable_project, second_project))
})

test_that("wildobs_dp_download returns a camtrapdp data package", {
  skip_if_no_wildobs_api()

  dp <- stable_dp()

  # the class is what lets camtrapdp and frictionless operate on the result, so
  # losing it would break every downstream consumer silently
  expect_s3_class(dp, "camtrapdp")
  expect_s3_class(dp, "datapackage")

  # and the Frictionless spec requires these three at the top level
  expect_true(all(c("profile", "name", "resources") %in% names(dp)))
})

test_that("wildobs_dp_download returns consistent structure across projects", {
  skip_if_no_wildobs_api()

  result <- paired_dp_list()

  # two different projects must produce the same top-level shape, otherwise code
  # written against one package breaks on the next one a user downloads
  expect_identical(names(result[[1]]), names(result[[2]]))
})


#
##
### Resources and their schemas ----

test_that("wildobs_dp_download includes required resources", {
  skip_if_no_wildobs_api()

  dp <- stable_dp()

  # these three are the tabular core of a WildObs package and must always arrive
  required <- c("deployments", "observations", "covariates")
  expect_true(all(required %in% resource_names(dp)))
})

test_that("wildobs_dp_download attaches a schema to every resource", {
  skip_if_no_wildobs_api()

  dp <- stable_dp()

  # a resource with no schema cannot be validated or typed, so every one carries
  # a schema with a non-empty field list
  for (resource in resource_names(dp)) {
    expect_true("schema" %in% names(get_resource(dp, resource)))
    expect_gt(length(get_resource(dp, resource)$schema$fields), 0)
  } # end per resource
})

test_that("wildobs_dp_download includes projectName in every resource schema", {
  skip_if_no_wildobs_api()

  dp <- stable_dp()

  # projectName is the WildObs addition that lets resources from several
  # packages be stacked and still be traced back to their project
  for (resource in resource_names(dp)) {
    expect_true("projectName" %in% schema_field_names(dp, resource))
  } # end per resource
})

test_that("wildobs_dp_download removes deprecated schema fields from deployments", {
  skip_if_no_wildobs_api()

  dp <- stable_dp()
  field_names <- schema_field_names(dp, "deployments")

  # these were dropped from the deployments schema, and their reappearance would
  # mean a stale schema had been served
  deprecated <- c("dataSource", "UTM_zone", "X", "Y", "state")
  expect_false(any(deprecated %in% field_names))
})

test_that("wildobs_dp_download column order matches schema", {
  skip_if_no_wildobs_api()

  dp <- stable_dp()

  # the data columns arrive in exactly the schema's field order, which is what
  # lets a reader trust position as well as name when eyeballing a table
  for (resource in resource_names(dp)) {
    expect_identical(
      names(dp$data[[resource]]),
      schema_field_names(dp, resource)
    )
  } # end per resource
})

test_that("wildobs_dp_download applies the types declared in the schema", {
  skip_if_no_wildobs_api()

  dp <- stable_dp()

  ## check the declared type against the class that actually arrived, for every
  ## field of every resource, rather than spot-checking a few columns
  for (resource in resource_names(dp)) {
    fields <- get_resource(dp, resource)$schema$fields
    # the type each field says it is
    declared <- vapply(fields, function(f) f$type, character(1))
    names(declared) <- vapply(fields, function(f) f$name, character(1))

    for (field in names(declared)) {
      # grab the column the schema is describing
      column <- dp$data[[resource]][[field]]

      ## a column holding no values at all cannot demonstrate a type: R gives an
      ## empty column the all-NA logical it uses for everything, whatever the
      ## schema declared. Skip those so this test only claims what it can prove.
      if (all(is.na(column))) next

      # datetimes have to come back timezone-aware, not as bare strings, since
      # every temporal calculation downstream depends on it
      if (declared[[field]] == "datetime") {
        expect_s3_class(column, "POSIXct")
      } # end datetime condition

      # whole-number and decimal fields stay distinct so counts never turn into
      # fractional values
      if (declared[[field]] == "integer") {
        expect_type(column, "integer")
      } # end integer condition
      if (declared[[field]] == "number") {
        expect_type(column, "double")
      } # end number condition

      # booleans arrive as logicals rather than the strings "true" and "false"
      if (declared[[field]] == "boolean") {
        expect_type(column, "logical")
      } # end boolean condition

      # a string field is character, unless the schema constrains it to a set of
      # allowed values, in which case it is read as a factor of those levels
      if (declared[[field]] == "string") {
        expect_true(is.character(column) || is.factor(column))
      } # end string condition
    } # end per field
  } # end per resource
})


#
##
### Media handling ----

test_that("wildobs_dp_download media parameter defaults to FALSE", {
  skip_if_no_wildobs_api()

  dp <- stable_dp()

  # media is the largest and slowest resource, so it stays out unless asked for.
  # Note this has to be checked against the resource names, not names(dp$data):
  # an empty media frame is present in $data either way.
  expect_false("media" %in% resource_names(dp))
})

test_that("wildobs_dp_download includes media when media=TRUE", {
  skip_if_no_wildobs_api()

  ## deliberately the smaller second project, not the stable fixture, because
  ## media on the fixture is far too large to pull inside a test
  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = second_project,
    media = TRUE
  )
  dp <- result[[1]]

  # asking for media adds it as a fourth resource alongside the usual three
  expect_true("media" %in% resource_names(dp))
  expect_length(dp$resources, 4)

  # and it arrives populated, since an empty media table would mean the request
  # was accepted but the records never came
  expect_s3_class(dp$data$media, "data.frame")
  expect_gt(nrow(dp$data$media), 0)
})


#
##
### Tabular data ----

test_that("wildobs_dp_download returns data for open projects", {
  skip_if_no_wildobs_api()

  dp <- stable_dp()

  # an open sharing agreement means the tabular data comes through, so empty
  # deployments here would mean the sharing preference was misread
  expect_s3_class(dp$data$deployments, "data.frame")
  expect_gt(nrow(dp$data$deployments), 0)
})

test_that("wildobs_dp_download returns observations as a data frame", {
  skip_if_no_wildobs_api()

  dp <- stable_dp()

  # observations must always be a frame, even for a project that detected
  # nothing, so callers never have to guard against a NULL
  expect_s3_class(dp$data$observations, "data.frame")
  expect_gt(ncol(dp$data$observations), 0)
})

test_that("wildobs_dp_download returns covariates as a data frame", {
  skip_if_no_wildobs_api()

  dp <- stable_dp()

  # same contract as observations: shape is guaranteed, contents are not
  expect_s3_class(dp$data$covariates, "data.frame")
  expect_gt(ncol(dp$data$covariates), 0)
})


#
##
### Project metadata ----

test_that("wildobs_dp_download includes spatial metadata", {
  skip_if_no_wildobs_api()

  dp <- stable_dp()

  # spatial arrives as GeoJSON, one feature per deployment group
  expect_named(dp$spatial, c("type", "features"))
  expect_identical(dp$spatial$type, "FeatureCollection")
  expect_gt(length(dp$spatial$features), 0)

  # each feature has to carry the location it describes, or the geometry cannot
  # be matched back to anything
  location_named <- vapply(
    dp$spatial$features,
    function(f) !is.null(f$properties$locationName),
    logical(1)
  )
  expect_true(all(location_named))
})

test_that("wildobs_dp_download returns spatial geometry free of missing values", {
  skip_if_no_wildobs_api()

  dp <- stable_dp()

  ## flatten every feature's coordinates into one vector to check them together
  coordinates <- unlist(lapply(
    dp$spatial$features,
    function(f) unlist(f$geometry$coordinates)
  ))

  # an NA or NULL slipping into a polygon breaks any mapping the user attempts,
  # and does so a long way from the download that introduced it
  expect_false(is.null(coordinates))
  expect_false(any(is.na(coordinates)))
})

test_that("wildobs_dp_download includes temporal metadata", {
  skip_if_no_wildobs_api()

  dp <- stable_dp()

  # temporal is keyed by deployment group, and an empty block would leave the
  # package with no date range at all
  expect_type(dp$temporal, "list")
  expect_gt(length(dp$temporal), 0)
})

test_that("wildobs_dp_download infers timezone when missing", {
  skip_if_no_wildobs_api()

  dp <- stable_dp()

  ## the timezone for each deployment group, NA where the entry has none
  zones <- vapply(
    dp$temporal,
    function(x) if (is.null(x$timeZone)) NA_character_ else x$timeZone,
    character(1)
  )

  # a timezone is looked up from the coordinates whenever the project did not
  # supply one, so every entry must end up with a real zone. Checking all of
  # them rather than the first, since only one entry is usually short of data.
  expect_false(any(is.na(zones)))
  expect_true(all(nzchar(zones)))
})

test_that("wildobs_dp_download includes taxonomic metadata", {
  skip_if_no_wildobs_api()

  dp <- stable_dp()

  # the taxonomic block lists what was detected, and each entry needs a name for
  # the package to be usable as a species reference
  expect_type(dp$taxonomic, "list")
  expect_gt(length(dp$taxonomic), 0)
  named <- vapply(
    dp$taxonomic,
    function(x) !is.null(x$scientificName),
    logical(1)
  )
  expect_true(all(named))
})

test_that("wildobs_dp_download handles contributors metadata", {
  skip_if_no_wildobs_api()

  dp <- stable_dp()

  # contributors drive attribution and the RAiD minting downstream, so a package
  # that arrives without them cannot be credited
  expect_type(dp$contributors, "list")
  expect_gt(length(dp$contributors), 0)
})

test_that("wildobs_dp_download handles licenses metadata", {
  skip_if_no_wildobs_api()

  dp <- stable_dp()

  # a package with no licence cannot legally be passed on
  expect_type(dp$licenses, "list")
  expect_gt(length(dp$licenses), 0)
})

test_that("wildobs_dp_download bibliography and citations included", {
  skip_if_no_wildobs_api()

  dp <- stable_dp()

  # users need something to cite, from either of the two places it can live
  expect_true(any(c("bibliographicCitation", "sources") %in% names(dp)))
})

test_that("wildobs_dp_download WildObsMetadata is included", {
  skip_if_no_wildobs_api()

  dp <- stable_dp()

  # WildObsMetadata is our extension to the standard, and the sharing preference
  # inside it is what governs whether tabular data was returned at all
  expect_true("WildObsMetadata" %in% names(dp))
  expect_true("tabularSharingPreference" %in% names(dp$WildObsMetadata))
})


#
##
### Failure handling ----

test_that("wildobs_dp_download handles API connection failure gracefully", {
  skip_if_no_wildobs_api()

  # a rejected key has to surface as our own message naming the failed step,
  # rather than as whatever the HTTP layer happened to return
  expect_error(
    wildobs_dp_download(api_key = "invalid_key", project_ids = stable_project),
    "Failed to retrieve metadata"
  )
})
