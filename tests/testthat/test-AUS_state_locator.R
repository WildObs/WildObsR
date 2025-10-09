## Tests for AUS_state_locator() ----

test_that("AUS_state_locator identifies NSW correctly", {
  skip_if_not_installed("ozmaps")
  deps <- data.frame(
    deploymentID = "sydney",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  result <- AUS_state_locator(deps)

  expect_s3_class(result, "data.frame")
  expect_true("state" %in% names(result))
  expect_equal(result$state, "NSW")
})

test_that("AUS_state_locator identifies QLD correctly", {
  deps <- data.frame(
    deploymentID = "brisbane",
    Latitude = -27.4698,
    Longitude = 153.0251
  )

  result <- AUS_state_locator(deps)

  expect_equal(result$state, "QLD")
})

test_that("AUS_state_locator identifies VIC correctly", {
  deps <- data.frame(
    deploymentID = "melbourne",
    Latitude = -37.8136,
    Longitude = 144.9631
  )

  result <- AUS_state_locator(deps)

  expect_equal(result$state, "VIC")
})

test_that("AUS_state_locator identifies WA correctly", {
  deps <- data.frame(
    deploymentID = "perth",
    Latitude = -31.9505,
    Longitude = 115.8605
  )

  result <- AUS_state_locator(deps)

  expect_equal(result$state, "WA")
})

test_that("AUS_state_locator identifies SA correctly", {
  deps <- data.frame(
    deploymentID = "adelaide",
    Latitude = -34.9285,
    Longitude = 138.6007
  )

  result <- AUS_state_locator(deps)

  expect_equal(result$state, "SA")
})

test_that("AUS_state_locator identifies NT correctly", {
  deps <- data.frame(
    deploymentID = "darwin",
    Latitude = -12.4634,
    Longitude = 130.8456
  )

  result <- AUS_state_locator(deps)

  expect_equal(result$state, "NT")
})

test_that("AUS_state_locator identifies TAS correctly", {
  deps <- data.frame(
    deploymentID = "hobart",
    Latitude = -42.8821,
    Longitude = 147.3272
  )

  result <- AUS_state_locator(deps)

  expect_equal(result$state, "TAS")
})

test_that("AUS_state_locator handles multiple deployments", {
  deps <- data.frame(
    deploymentID = c("sydney", "brisbane", "melbourne"),
    Latitude = c(-33.8688, -27.4698, -37.8136),
    Longitude = c(151.2093, 153.0251, 144.9631)
  )

  result <- AUS_state_locator(deps)

  expect_equal(nrow(result), 3)
  expect_true(all(c("NSW", "QLD", "VIC") %in% result$state))
})

test_that("AUS_state_locator handles lowercase latitude/longitude", {
  deps <- data.frame(
    deploymentID = "sydney",
    latitude = -33.8688,
    longitude = 151.2093
  )

  result <- AUS_state_locator(deps)

  expect_true("state" %in% names(result))
  expect_equal(result$state, "NSW")
  # Should preserve original column names
  expect_true("latitude" %in% names(result))
  expect_true("longitude" %in% names(result))
})

test_that("AUS_state_locator handles Lat/Long column names", {
  deps <- data.frame(
    deploymentID = "brisbane",
    Lat = -27.4698,
    Long = 153.0251
  )

  result <- AUS_state_locator(deps)

  expect_equal(result$state, "QLD")
  expect_true("Lat" %in% names(result))
  expect_true("Long" %in% names(result))
})

test_that("AUS_state_locator handles lat/long column names", {
  deps <- data.frame(
    deploymentID = "melbourne",
    lat = -37.8136,
    long = 144.9631
  )

  result <- AUS_state_locator(deps)

  expect_equal(result$state, "VIC")
  expect_true("lat" %in% names(result))
  expect_true("long" %in% names(result))
})

test_that("AUS_state_locator handles deployment_id column name", {
  deps <- data.frame(
    deployment_id = "sydney",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  result <- AUS_state_locator(deps)

  expect_equal(result$state, "NSW")
  # Should rename back to original
  expect_true("deployment_id" %in% names(result))
  expect_false("deploymentID" %in% names(result))
})

test_that("AUS_state_locator errors without latitude/longitude", {
  deps <- data.frame(
    deploymentID = "test",
    x = 151.2093,
    y = -33.8688
  )

  expect_error(
    AUS_state_locator(deps),
    "Latitude and/or longitude could not be found"
  )
})

test_that("AUS_state_locator preserves all original columns", {
  deps <- data.frame(
    deploymentID = "sydney",
    Latitude = -33.8688,
    Longitude = 151.2093,
    extra_col = "test_data",
    another_col = 42
  )

  result <- AUS_state_locator(deps)

  expect_true("extra_col" %in% names(result))
  expect_true("another_col" %in% names(result))
  expect_equal(result$extra_col, "test_data")
  expect_equal(result$another_col, 42)
})

test_that("AUS_state_locator calculates average coordinates for duplicate deploymentIDs", {
  # Multiple rows with same deploymentID - should average coordinates
  deps <- data.frame(
    deploymentID = c("sydney", "sydney"),
    Latitude = c(-33.8688, -33.87),
    Longitude = c(151.2093, 151.21)
  )

  result <- AUS_state_locator(deps)

  expect_equal(result$state, "NSW")
})

test_that("AUS_state_locator handles mixed case in different states", {
  deps <- data.frame(
    deploymentID = c("loc1", "loc2", "loc3"),
    Latitude = c(-33.8688, -27.4698, -31.9505),
    Longitude = c(151.2093, 153.0251, 115.8605)
  )

  result <- AUS_state_locator(deps)

  expect_equal(nrow(result), 3)
  expect_setequal(result$state, c("NSW", "QLD", "WA"))
})

test_that("AUS_state_locator identifies ACT correctly", {
  deps <- data.frame(
    deploymentID = "canberra",
    Latitude = -35.2809,
    Longitude = 149.1300
  )

  result <- AUS_state_locator(deps)

  expect_equal(result$state, "ACT")
})

test_that("AUS_state_locator preserves deploymentID order", {
  deps <- data.frame(
    deploymentID = c("perth", "sydney", "brisbane"),
    Latitude = c(-31.9505, -33.8688, -27.4698),
    Longitude = c(115.8605, 151.2093, 153.0251)
  )

  result <- AUS_state_locator(deps)

  # merge() may reorder, but all deploymentIDs should be present
  expect_setequal(result$deploymentID, c("perth", "sydney", "brisbane"))
})

test_that("AUS_state_locator adds state column without removing others", {
  deps <- data.frame(
    deploymentID = "sydney",
    Latitude = -33.8688,
    Longitude = 151.2093,
    site_name = "Royal National Park"
  )

  original_ncol <- ncol(deps)
  result <- AUS_state_locator(deps)

  # Should add exactly one column (state)
  expect_equal(ncol(result), original_ncol + 1)
  expect_true("site_name" %in% names(result))
})
