## Tests for ibra_classification() ----

test_that("ibra_classification errors when latitude column missing", {
  data <- data.frame(
    deploymentID = "loc1",
    longitude = 151.2093
  )

  expect_error(
    ibra_classification(data, lat_col = "latitude", long_col = "longitude"),
    "The latitude column you have specified.*is not present"
  )
})

test_that("ibra_classification errors when longitude column missing", {
  data <- data.frame(
    deploymentID = "loc1",
    latitude = -33.8688
  )

  expect_error(
    ibra_classification(data, lat_col = "latitude", long_col = "longitude"),
    "The longitude column you have specified.*is not present"
  )
})

test_that("ibra_classification errors when lat_col name is incorrect", {
  data <- data.frame(
    deploymentID = "loc1",
    lat = -33.8688,
    longitude = 151.2093
  )

  expect_error(
    ibra_classification(data, lat_col = "latitude", long_col = "longitude"),
    "The latitude column you have specified.*is not present"
  )
})

test_that("ibra_classification errors when long_col name is incorrect", {
  data <- data.frame(
    deploymentID = "loc1",
    latitude = -33.8688,
    lon = 151.2093
  )

  expect_error(
    ibra_classification(data, lat_col = "latitude", long_col = "longitude"),
    "The longitude column you have specified.*is not present"
  )
})

test_that("ibra_classification handles custom column names", {

  data <- data.frame(
    deploymentID = "loc1",
    my_lat = -33.8688,
    my_lon = 151.2093
  )

  result <- ibra_classification(data, lat_col = "my_lat", long_col = "my_lon")

  expect_s3_class(result, "data.frame")
  expect_true("IBRAsubRegionName" %in% names(result))
  expect_true("IBRAbioRegionName" %in% names(result))
})

test_that("ibra_classification returns expected columns", {

  data <- data.frame(
    deploymentID = "sydney",
    latitude = -33.8688,
    longitude = 151.2093
  )

  result <- ibra_classification(data, lat_col = "latitude", long_col = "longitude")

  expected_cols <- c("IBRAsubRegionName", "IBRAsubRegioncode",
                     "IBRAbioRegionName", "IBRAbioRegionCode")

  expect_true(all(expected_cols %in% names(result)))
})

test_that("ibra_classification preserves original data columns", {

  data <- data.frame(
    deploymentID = "loc1",
    latitude = -33.8688,
    longitude = 151.2093,
    site_name = "Test Site",
    year = 2023
  )

  result <- ibra_classification(data, lat_col = "latitude", long_col = "longitude")

  expect_true("site_name" %in% names(result))
  expect_true("year" %in% names(result))
  expect_equal(result$site_name, "Test Site")
  expect_equal(result$year, 2023)
})

test_that("ibra_classification removes ID column", {

  data <- data.frame(
    deploymentID = "loc1",
    latitude = -33.8688,
    longitude = 151.2093
  )

  result <- ibra_classification(data, lat_col = "latitude", long_col = "longitude")

  # ID column is created internally but should not be in final result
  expect_false("ID" %in% names(result))
})

test_that("ibra_classification handles multiple locations", {

  data <- data.frame(
    deploymentID = c("loc1", "loc2", "loc3"),
    latitude = c(-33.8688, -27.4698, -37.8136),
    longitude = c(151.2093, 153.0251, 144.9631)
  )

  result <- ibra_classification(data, lat_col = "latitude", long_col = "longitude")

  expect_equal(nrow(result), 3)
  expect_true(all(c("loc1", "loc2", "loc3") %in% result$deploymentID))
})

test_that("ibra_classification handles tibble input", {
  skip_if_not_installed("tibble")

  data <- tibble::tibble(
    deploymentID = "loc1",
    latitude = -33.8688,
    longitude = 151.2093
  )

  result <- ibra_classification(data, lat_col = "latitude", long_col = "longitude")

  expect_s3_class(result, "data.frame")
  expect_true("IBRAbioRegionName" %in% names(result))
})

test_that("ibra_classification produces character IBRA names", {

  data <- data.frame(
    deploymentID = "loc1",
    latitude = -33.8688,
    longitude = 151.2093
  )

  result <- ibra_classification(data, lat_col = "latitude", long_col = "longitude")

  expect_type(result$IBRAsubRegionName, "character")
  expect_type(result$IBRAbioRegionName, "character")
})

test_that("ibra_classification handles single location", {

  data <- data.frame(
    deploymentID = "single",
    latitude = -33.8688,
    longitude = 151.2093
  )

  result <- ibra_classification(data, lat_col = "latitude", long_col = "longitude")

  expect_equal(nrow(result), 1)
  expect_equal(result$deploymentID, "single")
})

test_that("ibra_classification handles decimal precision", {

  data <- data.frame(
    deploymentID = c("precise1", "precise2"),
    latitude = c(-33.868812345, -27.469812345),
    longitude = c(151.209312345, 153.025112345)
  )

  result <- ibra_classification(data, lat_col = "latitude", long_col = "longitude")

  expect_equal(nrow(result), 2)
  expect_true(all(!is.na(result$IBRAbioRegionName)))
})

test_that("ibra_classification handles locations across different bioregions", {

  # Locations in different Australian bioregions
  data <- data.frame(
    deploymentID = c("tropical", "temperate", "arid"),
    latitude = c(-17.1, -33.8688, -25.5),
    longitude = c(145.7, 151.2093, 133.5)
  )

  result <- ibra_classification(data, lat_col = "latitude", long_col = "longitude")

  expect_equal(nrow(result), 3)
  # Should have different bioregion names
  expect_true(length(unique(result$IBRAbioRegionName)) >= 2)
})

test_that("ibra_classification does not modify original data frame", {

  data <- data.frame(
    deploymentID = "loc1",
    latitude = -33.8688,
    longitude = 151.2093
  )

  original_names <- names(data)
  original_nrow <- nrow(data)

  result <- ibra_classification(data, lat_col = "latitude", long_col = "longitude")

  # Original data should be unchanged
  expect_equal(names(data), original_names)
  expect_equal(nrow(data), original_nrow)
})

test_that("ibra_classification handles NA coordinates gracefully", {

  data <- data.frame(
    deploymentID = c("loc1", "loc2"),
    latitude = c(-33.8688, NA),
    longitude = c(151.2093, 153.0251)
  )

  # Function may error or handle NAs, test for expected behavior
  expect_error(
    ibra_classification(data, lat_col = "latitude", long_col = "longitude"),
    class = "error"
  )
})

