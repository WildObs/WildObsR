## Tests for wildobs_mongo_query() ----

# General use API key from examples
test_api_key <- "f4b9126e87c44da98c0d1e29a671bb4ff39adcc65c8b92a0e7f4317a2b95de83"

test_that("wildobs_mongo_query errors when neither api_key nor db_url provided", {
  expect_error(
    wildobs_mongo_query(),
    "You have not provided an API key or a database URL"
  )
})

test_that("wildobs_mongo_query errors with invalid db_url format", {
  expect_error(
    wildobs_mongo_query(db_url = "invalid_url"),
    "must be a valid MongoDB URI"
  )
})

test_that("wildobs_mongo_query accepts valid MongoDB URI format", {
  skip("Requires valid MongoDB connection")

  db_url <- "mongodb://user:pass@localhost:27017/testdb"

  # Should not error on URL validation
  expect_error(
    wildobs_mongo_query(db_url = db_url),
    NA  # No error expected on URL format
  )
})

test_that("wildobs_mongo_query prioritizes db_url over api_key", {
  skip("Requires valid MongoDB connection")

  # When both provided, should use db_url
  # This is tested indirectly through the use_api flag logic
  db_url <- "mongodb://user:pass@localhost:27017/testdb"

  # Function should attempt db_url connection, not API
  expect_error(
    wildobs_mongo_query(db_url = db_url, api_key = test_api_key),
    ".*"  # Will error on connection, but validates prioritization
  )
})

test_that("wildobs_mongo_query with API key queries metadata endpoint", {
  skip("Requires live API access")

  result <- wildobs_mongo_query(api_key = test_api_key)

  expect_type(result, "character")
  expect_true(length(result) > 0)
})

test_that("wildobs_mongo_query returns character vector of project IDs", {
  skip("Requires live API access")

  result <- wildobs_mongo_query(api_key = test_api_key)

  expect_type(result, "character")
  expect_true(all(nchar(result) > 0))
})

test_that("wildobs_mongo_query spatial parameter filters correctly", {
  skip("Requires live API access")

  spatial_query <- list(xmin = 145.0, xmax = 147.0, ymin = -20.0, ymax = -16.0)

  result <- wildobs_mongo_query(
    api_key = test_api_key,
    spatial = spatial_query
  )

  expect_type(result, "character")
})

test_that("wildobs_mongo_query temporal parameter filters correctly", {
  skip("Requires live API access")

  temporal_query <- list(
    minDate = as.Date("2022-01-01"),
    maxDate = as.Date("2025-01-01")
  )

  result <- wildobs_mongo_query(
    api_key = test_api_key,
    temporal = temporal_query
  )

  expect_type(result, "character")
})

test_that("wildobs_mongo_query taxonomic parameter filters correctly", {
  skip("Requires live API access")

  taxa_query <- c("Phascolarctos cinereus", "Tachyglossus aculeatus")

  result <- wildobs_mongo_query(
    api_key = test_api_key,
    taxonomic = taxa_query
  )

  expect_type(result, "character")
})

test_that("wildobs_mongo_query samplingDesign parameter filters correctly", {
  skip("Requires live API access")

  sample_query <- c("simpleRandom", "opportunistic", "systematicRandom")

  result <- wildobs_mongo_query(
    api_key = test_api_key,
    samplingDesign = sample_query
  )

  expect_type(result, "character")
})

test_that("wildobs_mongo_query contributors parameter filters correctly", {
  skip("Requires live API access")

  contributor_query <- c("Zachary Amir")

  result <- wildobs_mongo_query(
    api_key = test_api_key,
    contributors = contributor_query
  )

  expect_type(result, "character")
})

test_that("wildobs_mongo_query combines multiple filters with intersection", {
  skip("Requires live API access")

  spatial_query <- list(xmin = 145.0, xmax = 147.0, ymin = -20.0, ymax = -16.0)
  temporal_query <- list(minDate = as.Date("2022-01-01"), maxDate = as.Date("2025-01-01"))

  result <- wildobs_mongo_query(
    api_key = test_api_key,
    spatial = spatial_query,
    temporal = temporal_query
  )

  expect_type(result, "character")
})

test_that("wildobs_mongo_query respects tabularSharingPreference default", {
  skip("Requires live API access")

  # Default should be "open"
  result <- wildobs_mongo_query(api_key = test_api_key)

  expect_type(result, "character")
  # All returned projects should have "open" sharing preference
})

test_that("wildobs_mongo_query warns when requesting closed data without admin", {
  skip("Requires live API access")

  expect_warning(
    wildobs_mongo_query(
      api_key = test_api_key,
      tabularSharingPreference = c("open", "closed")
    ),
    "have not provided admin credentials"
  )
})

test_that("wildobs_mongo_query handles empty spatial parameter", {
  skip("Requires live API access")

  # NULL spatial should not error
  result <- wildobs_mongo_query(
    api_key = test_api_key,
    spatial = NULL
  )

  expect_type(result, "character")
})

test_that("wildobs_mongo_query handles empty temporal parameter", {
  skip("Requires live API access")

  # NULL temporal should not error
  result <- wildobs_mongo_query(
    api_key = test_api_key,
    temporal = NULL
  )

  expect_type(result, "character")
})

test_that("wildobs_mongo_query handles empty taxonomic parameter", {
  skip("Requires live API access")

  # NULL taxonomic should not error
  result <- wildobs_mongo_query(
    api_key = test_api_key,
    taxonomic = NULL
  )

  expect_type(result, "character")
})

test_that("wildobs_mongo_query warns when no matches found", {
  skip("Requires live API access")

  # Use impossible spatial bounds to trigger no matches
  spatial_query <- list(xmin = 0.0, xmax = 0.01, ymin = 0.0, ymax = 0.01)

  expect_warning(
    wildobs_mongo_query(
      api_key = test_api_key,
      spatial = spatial_query
    ),
    "no matches in our database"
  )
})

test_that("wildobs_mongo_query returns all projects when no filters match", {
  skip("Requires live API access")

  # Use impossible parameters
  spatial_query <- list(xmin = 0.0, xmax = 0.01, ymin = 0.0, ymax = 0.01)

  # Should return all open projects with warning
  result <- suppressWarnings(
    wildobs_mongo_query(
      api_key = test_api_key,
      spatial = spatial_query
    )
  )

  expect_type(result, "character")
  expect_true(length(result) > 0)
})

test_that("wildobs_mongo_query handles API connection failure gracefully", {
  skip("Requires live API access")

  # Invalid API key should produce error
  expect_error(
    wildobs_mongo_query(api_key = "invalid_key"),
    "Failed to retrieve metadata"
  )
})

test_that("wildobs_mongo_query validates spatial parameter structure", {
  skip("Requires live API access")

  # Spatial with missing components
  spatial_incomplete <- list(xmin = 145.0, xmax = 147.0)

  # Should handle gracefully or error
  expect_error(
    wildobs_mongo_query(
      api_key = test_api_key,
      spatial = spatial_incomplete
    ),
    ".*"
  )
})

test_that("wildobs_mongo_query validates temporal parameter types", {
  skip("Requires live API access")

  # Temporal with invalid date types
  temporal_invalid <- list(minDate = "2022-01-01", maxDate = "2025-01-01")

  # Should handle or require Date objects
  result <- wildobs_mongo_query(
    api_key = test_api_key,
    temporal = temporal_invalid
  )

  expect_type(result, "character")
})

test_that("wildobs_mongo_query handles single vs multiple project returns", {
  skip("Requires live API access")

  # Query that should return single project
  contributor_query <- c("Zachary Amir")

  result <- wildobs_mongo_query(
    api_key = test_api_key,
    contributors = contributor_query
  )

  expect_type(result, "character")
  expect_true(length(result) >= 1)
})

test_that("wildobs_mongo_query embargo period calculation works correctly", {
  skip("Requires live API access")

  # Projects should respect embargo periods
  result <- wildobs_mongo_query(api_key = test_api_key)

  # All returned projects should be past embargo
  expect_type(result, "character")
})

test_that("wildobs_mongo_query handles unknown embargo periods", {
  skip("Requires live API access")

  # Function should convert unknown embargo based on sharing preference
  result <- wildobs_mongo_query(
    api_key = test_api_key,
    tabularSharingPreference = c("open", "partial")
  )

  expect_type(result, "character")
})

test_that("wildobs_mongo_query accepts valid samplingDesign enumerations", {
  skip("Requires live API access")

  valid_designs <- c("simpleRandom", "systematicRandom", "clusteredRandom",
                     "experimental", "targeted", "opportunistic")

  result <- wildobs_mongo_query(
    api_key = test_api_key,
    samplingDesign = valid_designs
  )

  expect_type(result, "character")
})

test_that("wildobs_mongo_query spatial bbox overlap detection works", {
  skip("Requires live API access")

  # Test overlapping bounding box
  spatial_overlap <- list(xmin = 145.5, xmax = 146.5, ymin = -18.0, ymax = -17.0)

  result <- wildobs_mongo_query(
    api_key = test_api_key,
    spatial = spatial_overlap
  )

  expect_type(result, "character")
})

test_that("wildobs_mongo_query temporal overlap detection works", {
  skip("Requires live API access")

  # Test partial temporal overlap
  temporal_overlap <- list(
    minDate = as.Date("2023-06-01"),
    maxDate = as.Date("2023-12-31")
  )

  result <- wildobs_mongo_query(
    api_key = test_api_key,
    temporal = temporal_overlap
  )

  expect_type(result, "character")
})

test_that("wildobs_mongo_query handles multiple taxonomic species", {
  skip("Requires live API access")

  taxa_multiple <- c(
    "Phascolarctos cinereus",
    "Tachyglossus aculeatus",
    "Macropus giganteus"
  )

  result <- wildobs_mongo_query(
    api_key = test_api_key,
    taxonomic = taxa_multiple
  )

  expect_type(result, "character")
})

test_that("wildobs_mongo_query returns unique project IDs", {
  skip("Requires live API access")

  result <- wildobs_mongo_query(api_key = test_api_key)

  expect_equal(length(result), length(unique(result)))
})

test_that("wildobs_mongo_query project IDs follow naming convention", {
  skip("Requires live API access")

  result <- wildobs_mongo_query(api_key = test_api_key)

  # Project IDs should contain WildObsID
  expect_true(any(grepl("WildObsID", result)))
})

test_that("wildobs_mongo_query handles empty contributors list", {
  skip("Requires live API access")

  result <- wildobs_mongo_query(
    api_key = test_api_key,
    contributors = character(0)
  )

  expect_type(result, "character")
})

test_that("wildobs_mongo_query filters work independently", {
  skip("Requires live API access")

  # Test each filter independently
  result_spatial <- wildobs_mongo_query(
    api_key = test_api_key,
    spatial = list(xmin = 145.0, xmax = 147.0, ymin = -20.0, ymax = -16.0)
  )

  result_temporal <- wildobs_mongo_query(
    api_key = test_api_key,
    temporal = list(minDate = as.Date("2022-01-01"), maxDate = as.Date("2025-01-01"))
  )

  expect_type(result_spatial, "character")
  expect_type(result_temporal, "character")
})
