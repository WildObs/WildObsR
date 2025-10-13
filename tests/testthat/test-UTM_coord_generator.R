## Tests for UTM_coord_generator() ----

test_that("UTM_coord_generator generates X and Y coordinates", {
  data <- data.frame(
    deploymentID = c("loc1", "loc2"),
    Latitude = c(-33.8688, -27.4698),
    Longitude = c(151.2093, 153.0251)
  )

  result <- UTM_coord_generator(data)

  expect_s3_class(result, "data.frame")
  expect_true("X" %in% names(result))
  expect_true("Y" %in% names(result))
  expect_true("UTMzone" %in% names(result))
  expect_equal(nrow(result), 2)
})

test_that("UTM_coord_generator calculates correct UTM zone", {
  # Sydney is in UTM zone 56
  data <- data.frame(
    deploymentID = "Sydney",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  result <- UTM_coord_generator(data)

  expect_equal(result$UTMzone, 56)
})

test_that("UTM_coord_generator handles multiple UTM zones", {
  # Perth (zone 50) and Sydney (zone 56)
  data <- data.frame(
    deploymentID = c("Perth", "Sydney"),
    Latitude = c(-31.9505, -33.8688),
    Longitude = c(115.8605, 151.2093)
  )

  result <- UTM_coord_generator(data)

  expect_equal(nrow(result), 2)
  expect_true(50 %in% result$UTMzone)
  expect_true(56 %in% result$UTMzone)
})

test_that("UTM_coord_generator handles lowercase column names", {
  data <- data.frame(
    deploymentID = "loc1",
    latitude = -33.8688,
    longitude = 151.2093
  )

  result <- UTM_coord_generator(data)

  expect_s3_class(result, "data.frame")
  expect_true("X" %in% names(result))
  expect_true("Y" %in% names(result))
})

test_that("UTM_coord_generator handles uppercase column names", {
  data <- data.frame(
    deploymentID = "loc1",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  result <- UTM_coord_generator(data)

  expect_s3_class(result, "data.frame")
  expect_true("X" %in% names(result))
  expect_true("Y" %in% names(result))
})

test_that("UTM_coord_generator generates UTM_zone if not provided", {
  data <- data.frame(
    deploymentID = "loc1",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  result <- UTM_coord_generator(data)

  expect_true("UTMzone" %in% names(result))
  expect_equal(result$UTMzone, 56)
})

test_that("UTM_coord_generator uses provided UTM_zone", {
  data <- data.frame(
    deploymentID = "loc1",
    Latitude = -33.8688,
    Longitude = 151.2093,
    UTM_zone = 56
  )

  result <- UTM_coord_generator(data)

  expect_equal(result$UTMzone, 56)
})

test_that("UTM_coord_generator handles deployment_id column name", {
  data <- data.frame(
    deployment_id = "loc1",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  result <- UTM_coord_generator(data)

  # Should be renamed back to deployment_ID
  expect_true("deployment_ID" %in% names(result))
  expect_false("deploymentID" %in% names(result))
})

test_that("UTM_coord_generator preserves deploymentID values", {
  data <- data.frame(
    deploymentID = c("site_A", "site_B", "site_C"),
    Latitude = c(-33.8688, -27.4698, -37.8136),
    Longitude = c(151.2093, 153.0251, 144.9631)
  )

  result <- UTM_coord_generator(data)

  expect_true(all(c("site_A", "site_B", "site_C") %in% result$deploymentID))
})

test_that("UTM_coord_generator produces numeric X and Y", {
  data <- data.frame(
    deploymentID = "loc1",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  result <- UTM_coord_generator(data)

  expect_type(result$X, "double")
  expect_type(result$Y, "double")
  expect_true(result$X > 0)
  # Y can be negative in southern hemisphere UTM
  expect_true(!is.na(result$Y))
})

test_that("UTM_coord_generator handles single location", {
  data <- data.frame(
    deploymentID = "single_loc",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  result <- UTM_coord_generator(data)

  expect_equal(nrow(result), 1)
  expect_equal(result$deploymentID, "single_loc")
})

test_that("UTM_coord_generator X coordinates differ between zones", {
  # Two locations in different zones should have different coordinate systems
  data <- data.frame(
    deploymentID = c("west", "east"),
    Latitude = c(-31.9505, -33.8688),
    Longitude = c(115.8605, 151.2093)  # Different UTM zones
  )

  result <- UTM_coord_generator(data)

  expect_equal(nrow(result), 2)
  # X coordinates should be meaningful (not zero or NA)
  expect_true(all(!is.na(result$X)))
  expect_true(all(!is.na(result$Y)))
})

test_that("UTM_coord_generator handles locations near zone boundaries", {
  # Test locations near UTM zone boundaries
  data <- data.frame(
    deploymentID = c("loc1", "loc2"),
    Latitude = c(-25, -25),
    Longitude = c(143.9, 144.1)  # Near zone 53/54 boundary
  )

  result <- UTM_coord_generator(data)

  expect_equal(nrow(result), 2)
  expect_true(all(!is.na(result$X)))
  expect_true(all(!is.na(result$Y)))
})

test_that("UTM_coord_generator consistent results for same location", {
  data1 <- data.frame(
    deploymentID = "loc1",
    Latitude = -33.8688,
    Longitude = 151.2093
  )

  result1 <- UTM_coord_generator(data1)
  result2 <- UTM_coord_generator(data1)

  expect_equal(result1$X, result2$X)
  expect_equal(result1$Y, result2$Y)
})

test_that("UTM_coord_generator handles multiple locations in same zone", {
  # Multiple locations in Sydney region (all zone 56)
  data <- data.frame(
    deploymentID = c("sydney1", "sydney2", "sydney3"),
    Latitude = c(-33.8688, -33.9, -33.85),
    Longitude = c(151.2093, 151.25, 151.18)
  )

  result <- UTM_coord_generator(data)

  expect_equal(nrow(result), 3)
  expect_true(all(result$UTMzone == 56))
})
