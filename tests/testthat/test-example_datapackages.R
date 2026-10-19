## Tests for example_datapackages() ----

## The shipped packages are copies of two open WildObs projects, so these check
## they read back in the shape wildobs_dp_download() returns, offline.

## the two shipped projects
p1 <- "ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001"
p6 <- "QLD_Paluma_cassowary_Mclean_2023-24_WildObsID_0006"

test_that("example_datapackages lists the shipped projects without reading them", {
  expect_setequal(example_datapackages(project_ids = NULL), c(p1, p6))
})

test_that("example_datapackages returns packages shaped like a download", {
  dp_list <- example_datapackages()

  # both projects, named, in the order asked for
  expect_named(dp_list, c(p1, p6))
  dp <- dp_list[[p6]]
  # recognised as a Camtrap DP package
  expect_s3_class(dp, "camtrapdp")
  # the three tables, readable by frictionless without the files on disk
  expect_setequal(frictionless::resource_names(dp), c("deployments", "observations", "covariates"))
  expect_equal(nrow(frictionless::read_resource(dp, "deployments")), nrow(dp$data$deployments))
  # and the same tables in dp$data
  expect_setequal(names(dp$data), c("deployments", "observations", "covariates"))
  # every table is a v1 tabular resource, as Camtrap DP 1.0.x requires
  expect_true(all(vapply(dp$resources, function(r) r$profile, character(1)) == "tabular-data-resource"))
})

test_that("example_datapackages types columns and times as a download does", {
  dp <- example_datapackages(p6)[[1]]
  deps <- frictionless::read_resource(dp, "deployments")

  # date-times are POSIXct in the project's own time zone
  expect_s3_class(deps$deploymentStart, "POSIXct")
  expect_identical(attr(deps$deploymentStart, "tzone"), dp$temporal$timeZone)
  # booleans are logical, as apply_schema_types() makes them in a download
  expect_type(deps$baitUse, "logical")
  # and a camera starts when the live database says it does
  # (compared as instants, since one is shown in local time and the other in UTC)
  start <- deps$deploymentStart[deps$deploymentID == "Bambaroo Track_WildObsID_0006_2023_b"]
  expect_equal(as.numeric(start), as.numeric(as.POSIXct("2023-11-07 05:33:26", tz = "UTC")))
})

test_that("example_datapackages holds animal observations only, obscured records included", {
  obs <- frictionless::read_resource(example_datapackages(p6)[[1]], "observations")

  expect_true(all(obs$observationType == "animal"))
  # threatened species stay obscured, exactly as the public API serves them
  expect_true(any(grepl("^obscured_for_", obs$deploymentID)))
})

test_that("example_datapackages stops on a project that is not shipped", {
  expect_error(example_datapackages("not_a_project"),
               "no shipped copy of: not_a_project")
})
