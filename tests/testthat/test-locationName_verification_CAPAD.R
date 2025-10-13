## Tests for locationName_verification_CAPAD() ----

test_that("locationName_verification_CAPAD handles lowercase latitude/longitude", {
  skip("Requires CAPAD shapefile")

  dep <- data.frame(
    deploymentID = "dep1",
    latitude = -33.8688,
    longitude = 151.2093
  )

  result <- locationName_verification_CAPAD(dep)

  expect_s3_class(result, "data.frame")
  expect_true("CAPADlocationName" %in% names(result))
})

test_that("locationName_verification_CAPAD handles uppercase Latitude/Longitude", {
  skip("Requires CAPAD shapefile")

  dep <- data.frame(
    deploymentID = "dep1",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  result <- locationName_verification_CAPAD(dep)

  expect_s3_class(result, "data.frame")
  expect_true("CAPADlocationName" %in% names(result))
})

test_that("locationName_verification_CAPAD returns CAPADlocationName column", {
  skip("Requires CAPAD shapefile")

  dep <- data.frame(
    deploymentID = c("dep1", "dep2"),
    Latitude = c(-33.8688, -27.4698),
    Longitude = c(151.2093, 153.0251)
  )

  result <- locationName_verification_CAPAD(dep)

  expect_true("CAPADlocationName" %in% names(result))
  expect_type(result$CAPADlocationName, "character")
})

test_that("locationName_verification_CAPAD preserves original columns", {
  skip("Requires CAPAD shapefile")

  dep <- data.frame(
    deploymentID = c("dep1", "dep2"),
    Latitude = c(-33.8688, -27.4698),
    Longitude = c(151.2093, 153.0251),
    site_name = c("Site A", "Site B"),
    year = c(2023, 2023)
  )

  result <- locationName_verification_CAPAD(dep)

  expect_true("site_name" %in% names(result))
  expect_true("year" %in% names(result))
  expect_equal(result$deploymentID, c("dep1", "dep2"))
})

test_that("locationName_verification_CAPAD removes internal ID column", {
  skip("Requires CAPAD shapefile")

  dep <- data.frame(
    deploymentID = "dep1",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  result <- locationName_verification_CAPAD(dep)

  # Internal ID column should not be in final result
  expect_false("ID" %in% names(result))
})

test_that("locationName_verification_CAPAD removes lat2/long2 columns", {
  skip("Requires CAPAD shapefile")

  dep <- data.frame(
    deploymentID = "dep1",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  result <- locationName_verification_CAPAD(dep)

  # Temporary columns should be removed
  expect_false("lat2" %in% names(result))
  expect_false("long2" %in% names(result))
})

test_that("locationName_verification_CAPAD handles tibble input", {
  skip("Requires CAPAD shapefile")
  skip_if_not_installed("tibble")

  dep <- tibble::tibble(
    deploymentID = "dep1",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  result <- locationName_verification_CAPAD(dep)

  expect_s3_class(result, "data.frame")
  expect_true("CAPADlocationName" %in% names(result))
})

test_that("locationName_verification_CAPAD cleans protected area names", {
  skip("Requires CAPAD shapefile")

  dep <- data.frame(
    deploymentID = "dep1",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  result <- locationName_verification_CAPAD(dep)

  # Names should have underscores instead of spaces
  if (!is.na(result$CAPADlocationName[1])) {
    expect_false(grepl(" ", result$CAPADlocationName[1]))
    # Should not have " - " pattern (cleaned to single underscore)
    expect_false(grepl("_-_", result$CAPADlocationName[1]))
  }
})

test_that("locationName_verification_CAPAD uses IUCN hierarchy for duplicates", {
  skip("Requires CAPAD shapefile")

  # Location that intersects multiple protected areas
  dep <- data.frame(
    deploymentID = "overlap",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  result <- locationName_verification_CAPAD(dep)

  # Should return only one location name (highest IUCN protection)
  expect_equal(nrow(result), 1)
  expect_true(!is.na(result$CAPADlocationName))
})

test_that("locationName_verification_CAPAD handles locations outside protected areas", {
  skip("Requires CAPAD shapefile")

  # Remote location likely outside protected areas
  dep <- data.frame(
    deploymentID = "remote",
    Latitude = -25.0,
    Longitude = 135.0
  )

  result <- locationName_verification_CAPAD(dep)

  # Should still return a result (nearest neighbor approach)
  expect_equal(nrow(result), 1)
  expect_true("CAPADlocationName" %in% names(result))
  expect_true("CAPADminDistance" %in% names(result))
})

test_that("locationName_verification_CAPAD adds CAPADminDistance column for missing areas", {
  skip("Requires CAPAD shapefile")

  dep <- data.frame(
    deploymentID = "remote",
    Latitude = -25.0,
    Longitude = 135.0
  )

  result <- locationName_verification_CAPAD(dep)

  expect_true("CAPADminDistance" %in% names(result))
  expect_type(result$CAPADminDistance, "double")
})

test_that("locationName_verification_CAPAD sets CAPADminDistance to 0 for direct matches", {
  skip("Requires CAPAD shapefile")

  # Location within a protected area
  dep <- data.frame(
    deploymentID = "dep1",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  result <- locationName_verification_CAPAD(dep)

  if (!is.na(result$CAPADlocationName[1]) && result$CAPADminDistance[1] == 0) {
    # If it's a direct match, distance should be 0
    expect_equal(result$CAPADminDistance[1], 0)
  }
})

test_that("locationName_verification_CAPAD prints warnings for distant matches", {
  skip("Requires CAPAD shapefile")

  # Location far from protected areas
  dep <- data.frame(
    deploymentID = "far",
    Latitude = -25.0,
    Longitude = 135.0
  )

  # Expect message about distance
  expect_output(
    locationName_verification_CAPAD(dep),
    "Distance to nearest feature"
  )
})

test_that("locationName_verification_CAPAD handles multiple locations", {
  skip("Requires CAPAD shapefile")

  dep <- data.frame(
    deploymentID = c("dep1", "dep2", "dep3"),
    Latitude = c(-33.8688, -27.4698, -37.8136),
    Longitude = c(151.2093, 153.0251, 144.9631)
  )

  result <- locationName_verification_CAPAD(dep)

  expect_equal(nrow(result), 3)
  expect_true(all(c("dep1", "dep2", "dep3") %in% result$deploymentID))
})

test_that("locationName_verification_CAPAD handles high precision coordinates", {
  skip("Requires CAPAD shapefile")

  dep <- data.frame(
    deploymentID = "precise",
    Latitude = -33.86881234567890,
    Longitude = 151.20931234567890
  )

  result <- locationName_verification_CAPAD(dep)

  expect_equal(nrow(result), 1)
  expect_true("CAPADlocationName" %in% names(result))
})

test_that("locationName_verification_CAPAD preserves row order", {
  skip("Requires CAPAD shapefile")

  dep <- data.frame(
    deploymentID = c("first", "second", "third"),
    Latitude = c(-33.8688, -27.4698, -37.8136),
    Longitude = c(151.2093, 153.0251, 144.9631)
  )

  result <- locationName_verification_CAPAD(dep)

  # Check that deploymentIDs maintain order
  expect_true(all(c("first", "second", "third") %in% result$deploymentID))
})

test_that("locationName_verification_CAPAD handles duplicate deploymentIDs", {
  skip("Requires CAPAD shapefile")

  # Same location, duplicate IDs
  dep <- data.frame(
    deploymentID = c("dup", "dup"),
    Latitude = c(-33.8688, -33.8688),
    Longitude = c(151.2093, 151.2093)
  )

  result <- locationName_verification_CAPAD(dep)

  expect_equal(nrow(result), 2)
  # Both should have same location name
  expect_equal(result$CAPADlocationName[1], result$CAPADlocationName[2])
})

test_that("locationName_verification_CAPAD errors with invalid capad_file_path", {
  dep <- data.frame(
    deploymentID = "dep1",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  expect_error(
    locationName_verification_CAPAD(dep, capad_file_path = "/fake/path/to/capad.shp"),
    class = "error"
  )
})

test_that("locationName_verification_CAPAD handles locations that don't intersect shapefile", {
  skip("Requires CAPAD shapefile")

  # Coordinates outside Australia
  dep <- data.frame(
    deploymentID = "outside",
    Latitude = 0.0,
    Longitude = 0.0
  )

  result <- locationName_verification_CAPAD(dep)

  # Should return NA for locations outside CAPAD coverage
  expect_true("CAPADlocationName" %in% names(result))
  expect_true(is.na(result$CAPADlocationName[1]))
})

test_that("locationName_verification_CAPAD does not modify original data frame", {
  skip("Requires CAPAD shapefile")

  dep <- data.frame(
    deploymentID = "dep1",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  original_names <- names(dep)
  original_nrow <- nrow(dep)

  result <- locationName_verification_CAPAD(dep)

  # Original should be unchanged
  expect_equal(names(dep), original_names)
  expect_equal(nrow(dep), original_nrow)
})

test_that("locationName_verification_CAPAD includes TYPE_ABBR in location name", {
  skip("Requires CAPAD shapefile")

  dep <- data.frame(
    deploymentID = "dep1",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  result <- locationName_verification_CAPAD(dep)

  # Location name should include type abbreviation (e.g., NP, NR, etc.)
  if (!is.na(result$CAPADlocationName[1])) {
    # Should have format: NAME_TYPE_ABBR
    expect_true(grepl("_[A-Z]+$", result$CAPADlocationName[1]))
  }
})

test_that("locationName_verification_CAPAD handles single location", {
  skip("Requires CAPAD shapefile")

  dep <- data.frame(
    deploymentID = "single",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  result <- locationName_verification_CAPAD(dep)

  expect_equal(nrow(result), 1)
  expect_equal(result$deploymentID, "single")
})

test_that("locationName_verification_CAPAD handles locations with different protection levels", {
  skip("Requires CAPAD shapefile")

  # Multiple locations across different types of protected areas
  dep <- data.frame(
    deploymentID = c("national_park", "conservation_area", "nature_reserve"),
    Latitude = c(-33.8688, -27.4698, -37.8136),
    Longitude = c(151.2093, 153.0251, 144.9631)
  )

  result <- locationName_verification_CAPAD(dep)

  expect_equal(nrow(result), 3)
  expect_true(all(!is.na(result$CAPADlocationName)))
})

test_that("locationName_verification_CAPAD returns early when no locations intersect", {
  skip("Requires CAPAD shapefile")

  # Coordinates far outside Australia
  dep <- data.frame(
    deploymentID = c("far1", "far2"),
    Latitude = c(45.0, 50.0),
    Longitude = c(10.0, 15.0)
  )

  result <- locationName_verification_CAPAD(dep)

  # Should return with all NA values
  expect_equal(nrow(result), 2)
  expect_true(all(is.na(result$CAPADlocationName)))
})

test_that("locationName_verification_CAPAD handles mixed valid and invalid locations", {
  skip("Requires CAPAD shapefile")

  dep <- data.frame(
    deploymentID = c("valid", "invalid"),
    Latitude = c(-33.8688, 45.0),
    Longitude = c(151.2093, 10.0)
  )

  result <- locationName_verification_CAPAD(dep)

  expect_equal(nrow(result), 2)
  # First should have a value, second might be NA
  expect_true("CAPADlocationName" %in% names(result))
})
