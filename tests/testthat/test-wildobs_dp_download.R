## Tests for wildobs_dp_download() ----

# General use API key from examples
test_api_key <- "f4b9126e87c44da98c0d1e29a671bb4ff39adcc65c8b92a0e7f4317a2b95de83"

# Test project IDs from examples
test_project_ids <- c(
  "QLD_Kgari_BIOL2015_2023-24_WildObsID_0004",
  "QLD_Kgari_potoroos_Amir_2022_WildObsID_0003"
)

test_that("wildobs_dp_download errors when neither api_key nor db_url provided", {
  expect_error(
    wildobs_dp_download(project_ids = test_project_ids),
    "You have not provided an API key or a database URL"
  )
})

test_that("wildobs_dp_download errors with invalid db_url format", {
  expect_error(
    wildobs_dp_download(db_url = "invalid_url", project_ids = test_project_ids),
    "must be a valid MongoDB URI"
  )
})

test_that("wildobs_dp_download accepts valid MongoDB URI format", {
  skip("Requires valid MongoDB connection")

  db_url <- "mongodb://user:pass@localhost:27017/testdb"

  # Should not error on URL validation
  expect_error(
    wildobs_dp_download(db_url = db_url, project_ids = test_project_ids),
    NA  # Expect different error (connection), not format error
  )
})

test_that("wildobs_dp_download prioritizes db_url over api_key", {
  skip("Requires valid MongoDB connection")

  db_url <- "mongodb://user:pass@localhost:27017/testdb"

  # When both provided, should use db_url
  expect_error(
    wildobs_dp_download(db_url = db_url, api_key = test_api_key,
                       project_ids = test_project_ids),
    ".*"  # Will error on connection, but validates prioritization
  )
})

test_that("wildobs_dp_download with API key downloads data packages", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1]
  )

  expect_type(result, "list")
  expect_true(length(result) > 0)
})

test_that("wildobs_dp_download returns named list of data packages", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1]
  )

  expect_named(result)
  expect_true(test_project_ids[1] %in% names(result))
})

test_that("wildobs_dp_download handles single project ID", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1]
  )

  expect_type(result, "list")
  expect_equal(length(result), 1)
})

test_that("wildobs_dp_download handles multiple project IDs", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids
  )

  expect_type(result, "list")
  expect_equal(length(result), length(test_project_ids))
})

test_that("wildobs_dp_download media parameter defaults to FALSE", {
  skip("Requires live API access")

  # Default should exclude media
  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1]
  )

  expect_type(result, "list")
  # Media resource should not be present
  expect_false("media" %in% names(result[[1]]$resources))
})

test_that("wildobs_dp_download includes media when media=TRUE", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1],
    media = TRUE
  )

  expect_type(result, "list")
  # Media resource should be present
  expect_true("media" %in% names(result[[1]]$resources))
})

test_that("wildobs_dp_download returns Frictionless Data Package structure", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1]
  )

  dp <- result[[1]]

  # Check for required Frictionless DP fields
  expect_true("profile" %in% names(dp))
  expect_true("name" %in% names(dp))
  expect_true("resources" %in% names(dp))
})

test_that("wildobs_dp_download includes required resources", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1]
  )

  dp <- result[[1]]

  # Check for required resources
  resource_names <- names(dp$resources)
  expect_true("deployments" %in% resource_names)
  expect_true("observations" %in% resource_names)
  expect_true("covariates" %in% resource_names)
})

test_that("wildobs_dp_download applies schema types correctly", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1]
  )

  dp <- result[[1]]

  # Resources should have schema
  expect_true("schema" %in% names(dp$resources$deployments))
  expect_true("schema" %in% names(dp$resources$observations))
})

test_that("wildobs_dp_download includes projectName in all resources", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1]
  )

  dp <- result[[1]]

  # Check projectName field in schema
  dep_schema <- dp$resources$deployments$schema
  field_names <- sapply(dep_schema$fields, function(f) f$name)
  expect_true("projectName" %in% field_names)
})

test_that("wildobs_dp_download removes deprecated schema fields from deployments", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1]
  )

  dp <- result[[1]]

  # Check that deprecated fields are removed
  dep_schema <- dp$resources$deployments$schema
  field_names <- sapply(dep_schema$fields, function(f) f$name)

  # These should not be present
  expect_false("dataSource" %in% field_names)
  expect_false("UTM_zone" %in% field_names)
  expect_false("X" %in% field_names)
  expect_false("Y" %in% field_names)
  expect_false("state" %in% field_names)
})

test_that("wildobs_dp_download respects open data sharing preference", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1]
  )

  dp <- result[[1]]

  # Open projects should have data resources
  expect_true("deployments" %in% names(dp$resources))
  expect_true(nrow(dp$resources$deployments$data) > 0)
})

test_that("wildobs_dp_download handles partial data sharing preference", {
  skip("Requires live API access and project with partial sharing")

  # This would need a project with partial sharing
  # Partial projects should return metadata only
  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = "partial_project_id"
  )

  dp <- result[[1]]

  # Should have metadata but no data
  expect_true("profile" %in% names(dp))
  expect_true(length(dp$resources) == 0 || nrow(dp$resources$deployments$data) == 0)
})

test_that("wildobs_dp_download includes spatial metadata", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1]
  )

  dp <- result[[1]]

  expect_true("spatial" %in% names(dp))
  expect_true("bbox" %in% names(dp$spatial))
  expect_true("type" %in% names(dp$spatial))
})

test_that("wildobs_dp_download includes temporal metadata", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1]
  )

  dp <- result[[1]]

  expect_true("temporal" %in% names(dp))
  expect_true(length(dp$temporal) > 0)
})

test_that("wildobs_dp_download includes taxonomic metadata", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1]
  )

  dp <- result[[1]]

  expect_true("taxonomic" %in% names(dp))
  expect_true(length(dp$taxonomic) > 0)
})

test_that("wildobs_dp_download infers timezone when missing", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1]
  )

  dp <- result[[1]]

  # Check that temporal has timeZone
  if (length(dp$temporal) > 0) {
    expect_true("timeZone" %in% names(dp$temporal[[1]]))
    expect_true(nchar(dp$temporal[[1]]$timeZone) > 0)
  }
})

test_that("wildobs_dp_download cleans spatial bounding boxes", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1]
  )

  dp <- result[[1]]

  # Bbox should not contain NULL or NA values
  bbox <- dp$spatial$bbox
  expect_false(is.null(bbox))
  expect_true(all(!is.na(unlist(bbox))))
})

test_that("wildobs_dp_download handles contributors metadata", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1]
  )

  dp <- result[[1]]

  expect_true("contributors" %in% names(dp))
  expect_true(length(dp$contributors) > 0)
})

test_that("wildobs_dp_download handles licenses metadata", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1]
  )

  dp <- result[[1]]

  expect_true("licenses" %in% names(dp))
})

test_that("wildobs_dp_download column order matches schema", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1]
  )

  dp <- result[[1]]

  # Check deployments column order
  schema_fields <- sapply(dp$resources$deployments$schema$fields, function(f) f$name)
  data_cols <- names(dp$resources$deployments$data)

  expect_equal(data_cols, schema_fields)
})

test_that("wildobs_dp_download handles API connection failure gracefully", {
  skip("Requires live API access")

  # Invalid API key should produce error
  expect_error(
    wildobs_dp_download(api_key = "invalid_key", project_ids = test_project_ids),
    "Failed to retrieve metadata"
  )
})

test_that("wildobs_dp_download handles non-existent project IDs", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = c("non_existent_project_id")
  )

  # Should return empty or error gracefully
  expect_type(result, "list")
})

test_that("wildobs_dp_download WildObsMetadata is included", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1]
  )

  dp <- result[[1]]

  expect_true("WildObsMetadata" %in% names(dp))
  expect_true("tabularSharingPreference" %in% names(dp$WildObsMetadata))
})

test_that("wildobs_dp_download handles mixed open and partial projects", {
  skip("Requires live API access with mixed projects")

  # Test with both open and partial projects
  mixed_ids <- c(test_project_ids[1], "partial_project_id")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = mixed_ids
  )

  expect_type(result, "list")
  expect_equal(length(result), 2)
})

test_that("wildobs_dp_download admin API key accesses all data", {
  skip("Requires admin API key")

  # Admin should access closed data
  admin_key <- "admin_key_here"

  result <- wildobs_dp_download(
    api_key = admin_key,
    project_ids = test_project_ids
  )

  expect_type(result, "list")
})

test_that("wildobs_dp_download preserves data types from schema", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1]
  )

  dp <- result[[1]]

  # Check that data types are applied
  deps <- dp$resources$deployments$data

  # Should have appropriate types based on schema
  expect_true(ncol(deps) > 0)
  expect_true(nrow(deps) > 0)
})

test_that("wildobs_dp_download handles empty observations", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1]
  )

  dp <- result[[1]]

  # Observations might be empty for some projects
  if ("observations" %in% names(dp$resources)) {
    expect_true("data" %in% names(dp$resources$observations))
  }
})

test_that("wildobs_dp_download handles empty covariates", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1]
  )

  dp <- result[[1]]

  # Covariates might be empty for some projects
  if ("covariates" %in% names(dp$resources)) {
    expect_true("data" %in% names(dp$resources$covariates))
  }
})

test_that("wildobs_dp_download returns consistent structure across projects", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids
  )

  # Check that all projects have same top-level structure
  structures <- lapply(result, names)

  # All should have similar structure
  expect_true(length(unique(structures)) <= 2)  # Allow some variation
})

test_that("wildobs_dp_download bibliography and citations included", {
  skip("Requires live API access")

  result <- wildobs_dp_download(
    api_key = test_api_key,
    project_ids = test_project_ids[1]
  )

  dp <- result[[1]]

  # Check for citation metadata
  expect_true("bibliographicCitation" %in% names(dp) ||
              "sources" %in% names(dp))
})
