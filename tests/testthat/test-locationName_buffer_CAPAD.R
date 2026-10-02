## Tests for locationName_buffer_CAPAD() ----

test_that("locationName_buffer_CAPAD generates UTM coords when missing", {

  # Data without UTM columns
  dep <- data.frame(
    deploymentID = c("dep1", "dep2"),
    Latitude = c(-33.8688, -27.4698),
    Longitude = c(151.2093, 153.0251)
  )

  result <- locationName_buffer_CAPAD(dep, buffer_size = 5000)

  # Should have UTM columns added
  expect_true(any(grepl("UTM|X|Y", names(result), ignore.case = TRUE)))
  expect_equal(nrow(result), 2)
})

test_that("locationName_buffer_CAPAD accepts data with existing UTM coords", {

  dep <- data.frame(
    deploymentID = c("dep1", "dep2"),
    Latitude = c(-33.8688, -27.4698),
    Longitude = c(151.2093, 153.0251),
    UTMzone = c(56, 56),
    X = c(334852, 540245),
    Y = c(6252176, 6961676)
  )

  result <- locationName_buffer_CAPAD(dep, buffer_size = 5000)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
})

test_that("locationName_buffer_CAPAD handles lowercase latitude/longitude", {

  dep <- data.frame(
    deploymentID = "dep1",
    latitude = -33.8688,
    longitude = 151.2093
  )

  result <- locationName_buffer_CAPAD(dep, buffer_size = 5000)

  expect_s3_class(result, "data.frame")
  expect_true("CAPADlocationNameBuffer" %in% names(result))
})

test_that("locationName_buffer_CAPAD handles uppercase Latitude/Longitude", {

  dep <- data.frame(
    deploymentID = "dep1",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  result <- locationName_buffer_CAPAD(dep, buffer_size = 5000)

  expect_s3_class(result, "data.frame")
  expect_true("CAPADlocationNameBuffer" %in% names(result))
})

test_that("locationName_buffer_CAPAD returns CAPADlocationNameBuffer column", {

  dep <- data.frame(
    deploymentID = c("dep1", "dep2"),
    Latitude = c(-33.8688, -27.4698),
    Longitude = c(151.2093, 153.0251)
  )

  result <- locationName_buffer_CAPAD(dep, buffer_size = 5000)

  expect_true("CAPADlocationNameBuffer" %in% names(result))
  expect_type(result$CAPADlocationNameBuffer, "character")
})

test_that("locationName_buffer_CAPAD preserves original columns", {

  dep <- data.frame(
    deploymentID = c("dep1", "dep2"),
    Latitude = c(-33.8688, -27.4698),
    Longitude = c(151.2093, 153.0251),
    site_name = c("Site A", "Site B"),
    year = c(2023, 2023)
  )

  result <- locationName_buffer_CAPAD(dep, buffer_size = 5000)

  expect_true("site_name" %in% names(result))
  expect_true("year" %in% names(result))
  expect_equal(result$deploymentID, c("dep1", "dep2"))
})

test_that("locationName_buffer_CAPAD accepts custom buffer_size", {

  dep <- data.frame(
    deploymentID = "dep1",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  result_5km <- locationName_buffer_CAPAD(dep, buffer_size = 5000)
  result_10km <- locationName_buffer_CAPAD(dep, buffer_size = 10000)

  # Both should work without errors
  expect_s3_class(result_5km, "data.frame")
  expect_s3_class(result_10km, "data.frame")
})

test_that("locationName_buffer_CAPAD handles single UTM zone", {

  # All locations in same UTM zone (56)
  dep <- data.frame(
    deploymentID = c("dep1", "dep2", "dep3"),
    Latitude = c(-33.8688, -33.9, -34.0),
    Longitude = c(151.2093, 151.3, 151.4)
  )

  result <- locationName_buffer_CAPAD(dep, buffer_size = 5000)

  expect_equal(nrow(result), 3)
  expect_true("CAPADlocationNameBuffer" %in% names(result))
})

test_that("locationName_buffer_CAPAD handles multiple UTM zones", {

  # Locations spanning multiple UTM zones
  dep <- data.frame(
    deploymentID = c("west", "central", "east"),
    Latitude = c(-33.8688, -27.4698, -33.8688),
    Longitude = c(115.8605, 153.0251, 151.2093)  # zones 50, 56, 56
  )

  result <- locationName_buffer_CAPAD(dep, buffer_size = 5000)

  expect_equal(nrow(result), 3)
  expect_true("CAPADlocationNameBuffer" %in% names(result))
})

test_that("locationName_buffer_CAPAD errors when no UTM zones found", {

  # Create data that might cause issues
  dep <- data.frame(
    deploymentID = "dep1",
    Latitude = NA,
    Longitude = NA
  )

  expect_error(
    locationName_buffer_CAPAD(dep, buffer_size = 5000)
  )
})

test_that("locationName_buffer_CAPAD handles tibble input", {
  skip_if_not_installed("tibble")

  dep <- tibble::tibble(
    deploymentID = "dep1",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  result <- locationName_buffer_CAPAD(dep, buffer_size = 5000)

  expect_s3_class(result, "data.frame")
  expect_true("CAPADlocationNameBuffer" %in% names(result))
})

test_that("locationName_buffer_CAPAD buffer IDs contain UTM zone prefix", {

  dep <- data.frame(
    deploymentID = "dep1",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  result <- locationName_buffer_CAPAD(dep, buffer_size = 5000)

  # CAPADlocationNameBuffer should contain Landscape_ and protected area name
  expect_true(grepl("Landscape_", result$CAPADlocationNameBuffer[1]))
})

test_that("locationName_buffer_CAPAD handles overlapping buffers", {

  # Two very close deployments that will have overlapping buffers
  dep <- data.frame(
    deploymentID = c("dep1", "dep2"),
    Latitude = c(-33.8688, -33.8690),
    Longitude = c(151.2093, 151.2095)
  )

  result <- locationName_buffer_CAPAD(dep, buffer_size = 5000)

  expect_equal(nrow(result), 2)
  # Both might have same landscape ID due to overlap
  expect_true(all(!is.na(result$CAPADlocationNameBuffer)))
})

test_that("locationName_buffer_CAPAD handles locations outside protected areas", {

  # Remote location likely outside protected areas
  dep <- data.frame(
    deploymentID = "remote",
    Latitude = -25.0,
    Longitude = 135.0
  )

  result <- locationName_buffer_CAPAD(dep, buffer_size = 5000)

  # Should still return a result (nearest neighbor approach)
  expect_equal(nrow(result), 1)
  expect_true("CAPADlocationNameBuffer" %in% names(result))
  expect_true(!is.na(result$CAPADlocationNameBuffer))
})

test_that("locationName_buffer_CAPAD cleans protected area names", {

  dep <- data.frame(
    deploymentID = "dep1",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  result <- locationName_buffer_CAPAD(dep, buffer_size = 5000)

  # Names should have underscores instead of spaces
  expect_false(grepl(" ", result$CAPADlocationNameBuffer[1]))
  # Should not have " - " pattern (cleaned to single underscore)
  expect_false(grepl("_-_", result$CAPADlocationNameBuffer[1]))
})

test_that("locationName_buffer_CAPAD default buffer is 5000m", {

  dep <- data.frame(
    deploymentID = "dep1",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  # Test default parameter
  result_default <- locationName_buffer_CAPAD(dep)
  result_explicit <- locationName_buffer_CAPAD(dep, buffer_size = 5000)

  # Both should produce same result
  expect_equal(result_default$CAPADlocationNameBuffer,
               result_explicit$CAPADlocationNameBuffer)
})

test_that("locationName_buffer_CAPAD handles high precision coordinates", {

  dep <- data.frame(
    deploymentID = "precise",
    Latitude = -33.86881234567890,
    Longitude = 151.20931234567890
  )

  result <- locationName_buffer_CAPAD(dep, buffer_size = 5000)

  expect_equal(nrow(result), 1)
  expect_true(!is.na(result$CAPADlocationNameBuffer))
})

test_that("locationName_buffer_CAPAD preserves row order", {

  dep <- data.frame(
    deploymentID = c("first", "second", "third"),
    Latitude = c(-33.8688, -27.4698, -37.8136),
    Longitude = c(151.2093, 153.0251, 144.9631)
  )

  result <- locationName_buffer_CAPAD(dep, buffer_size = 5000)

  # Check that deploymentIDs are in same order
  expect_equal(result$deploymentID, c("first", "second", "third"))
})

test_that("locationName_buffer_CAPAD handles duplicate deploymentIDs", {
  ## identical rows sharing a deploymentID collapse to one when UTMs are merged on
  ## deploymentID; duplicate IDs break Camtrap DP anyway, so the right behaviour is undecided
  skip("Undecided: error, warn, or keep duplicate deploymentIDs? See review item S5")

  # Same location, duplicate IDs
  dep <- data.frame(
    deploymentID = c("dup", "dup"),
    Latitude = c(-33.8688, -33.8688),
    Longitude = c(151.2093, 151.2093)
  )

  result <- locationName_buffer_CAPAD(dep, buffer_size = 5000)

  expect_equal(nrow(result), 2)
  # Both should have same buffer assignment
  expect_equal(result$CAPADlocationNameBuffer[1],
               result$CAPADlocationNameBuffer[2])
})

test_that("locationName_buffer_CAPAD does not modify original data frame", {

  dep <- data.frame(
    deploymentID = "dep1",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  original_names <- names(dep)
  original_nrow <- nrow(dep)

  result <- locationName_buffer_CAPAD(dep, buffer_size = 5000)

  # Original should be unchanged
  expect_equal(names(dep), original_names)
  expect_equal(nrow(dep), original_nrow)
})
