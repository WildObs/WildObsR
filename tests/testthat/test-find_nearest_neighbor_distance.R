## Tests for find_nearest_neighbor_distance() ----

test_that("find_nearest_neighbor_distance calculates distances correctly", {
  data <- data.frame(
    locationID = c("loc1", "loc2", "loc3"),
    longitude = c(151.2093, 153.0251, 150.644),
    latitude = c(-33.8688, -27.4698, -34.9285)
  )

  result <- find_nearest_neighbor_distance(data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true("locationID" %in% names(result))
  expect_true("nearestNeighborID" %in% names(result))
  expect_true("distance" %in% names(result))
})

test_that("find_nearest_neighbor_distance produces positive distances", {
  data <- data.frame(
    locationID = c("loc1", "loc2", "loc3"),
    longitude = c(151.2093, 153.0251, 150.644),
    latitude = c(-33.8688, -27.4698, -34.9285)
  )

  result <- find_nearest_neighbor_distance(data)

  expect_true(all(result$distance > 0))
})

test_that("find_nearest_neighbor_distance assigns correct nearest neighbors", {
  # Create a simple linear arrangement where nearest neighbors are clear
  data <- data.frame(
    locationID = c("A", "B", "C"),
    longitude = c(150, 151, 152),
    latitude = c(-30, -30, -30)  # Same latitude, increasing longitude
  )

  result <- find_nearest_neighbor_distance(data)

  # B should be nearest to both A and C
  expect_equal(result$nearestNeighborID[result$locationID == "A"], "B")
  expect_equal(result$nearestNeighborID[result$locationID == "C"], "B")
  # A and C are equidistant from B, so could be either
  expect_true(result$nearestNeighborID[result$locationID == "B"] %in% c("A", "C"))
})

test_that("find_nearest_neighbor_distance handles two locations", {
  data <- data.frame(
    locationID = c("loc1", "loc2"),
    longitude = c(151.2093, 153.0251),
    latitude = c(-33.8688, -27.4698)
  )

  result <- find_nearest_neighbor_distance(data)

  expect_equal(nrow(result), 2)
  # Each should be nearest to the other
  expect_equal(result$nearestNeighborID[result$locationID == "loc1"], "loc2")
  expect_equal(result$nearestNeighborID[result$locationID == "loc2"], "loc1")
  # Distances should be the same
  expect_equal(result$distance[1], result$distance[2])
})

test_that("find_nearest_neighbor_distance handles single location", {
  data <- data.frame(
    locationID = "loc1",
    longitude = 151.2093,
    latitude = -33.8688
  )

  result <- find_nearest_neighbor_distance(data)

  expect_equal(nrow(result), 1)
  # With only one location, it should return itself with Inf distance
  expect_equal(result$distance, Inf)
})

test_that("find_nearest_neighbor_distance removes duplicate locations", {
  # Create data with duplicate locationIDs
  data <- data.frame(
    locationID = c("loc1", "loc1", "loc2"),
    longitude = c(151.2093, 151.2093, 153.0251),
    latitude = c(-33.8688, -33.8688, -27.4698)
  )

  result <- find_nearest_neighbor_distance(data)

  # Should only have 2 unique locations
  expect_equal(nrow(result), 2)
  expect_true(all(c("loc1", "loc2") %in% result$locationID))
})

test_that("find_nearest_neighbor_distance handles clustered locations", {
  # Two close locations and one far
  data <- data.frame(
    locationID = c("close1", "close2", "far"),
    longitude = c(151.20, 151.21, 153.00),
    latitude = c(-33.86, -33.87, -27.47)
  )

  result <- find_nearest_neighbor_distance(data)

  # The close locations should be nearest to each other
  expect_equal(result$nearestNeighborID[result$locationID == "close1"], "close2")
  expect_equal(result$nearestNeighborID[result$locationID == "close2"], "close1")
  # The far location's nearest is one of the close ones
  expect_true(result$nearestNeighborID[result$locationID == "far"] %in% c("close1", "close2"))
})

test_that("find_nearest_neighbor_distance produces numeric distance", {
  data <- data.frame(
    locationID = c("loc1", "loc2"),
    longitude = c(151.2093, 153.0251),
    latitude = c(-33.8688, -27.4698)
  )

  result <- find_nearest_neighbor_distance(data)

  expect_type(result$distance, "double")
})

test_that("find_nearest_neighbor_distance handles locations across different hemispheres", {
  # Test with a mix of coordinates
  data <- data.frame(
    locationID = c("south", "north"),
    longitude = c(151.2093, 151.2093),
    latitude = c(-33.8688, -30.0)
  )

  result <- find_nearest_neighbor_distance(data)

  expect_equal(nrow(result), 2)
  expect_true(all(result$distance > 0))
  expect_true(all(!is.na(result$distance)))
})

test_that("find_nearest_neighbor_distance preserves locationID types", {
  data <- data.frame(
    locationID = c("site_A", "site_B", "site_C"),
    longitude = c(151.2093, 153.0251, 150.644),
    latitude = c(-33.8688, -27.4698, -34.9285),
    stringsAsFactors = FALSE
  )

  result <- find_nearest_neighbor_distance(data)

  expect_type(result$locationID, "character")
  expect_type(result$nearestNeighborID, "character")
})

test_that("find_nearest_neighbor_distance never assigns location as its own neighbor", {
  data <- data.frame(
    locationID = c("loc1", "loc2", "loc3", "loc4"),
    longitude = c(151, 152, 153, 154),
    latitude = c(-30, -30, -30, -30)
  )

  result <- find_nearest_neighbor_distance(data)

  # No location should be its own nearest neighbor
  for (i in 1:nrow(result)) {
    expect_false(result$locationID[i] == result$nearestNeighborID[i])
  }
})

test_that("find_nearest_neighbor_distance handles widely spaced locations", {
  # Perth and Sydney (far apart)
  data <- data.frame(
    locationID = c("Perth", "Sydney", "Brisbane"),
    longitude = c(115.8605, 151.2093, 153.0251),
    latitude = c(-31.9505, -33.8688, -27.4698)
  )

  result <- find_nearest_neighbor_distance(data)

  expect_equal(nrow(result), 3)
  # Brisbane and Sydney should be nearest to each other
  expect_true(result$nearestNeighborID[result$locationID == "Sydney"] == "Brisbane")
  expect_true(result$nearestNeighborID[result$locationID == "Brisbane"] == "Sydney")
})

test_that("find_nearest_neighbor_distance consistent for identical locations", {
  data <- data.frame(
    locationID = c("loc1", "loc2"),
    longitude = c(151.2093, 153.0251),
    latitude = c(-33.8688, -27.4698)
  )

  result1 <- find_nearest_neighbor_distance(data)
  result2 <- find_nearest_neighbor_distance(data)

  expect_equal(result1, result2)
})

test_that("find_nearest_neighbor_distance handles precise coordinates", {
  data <- data.frame(
    locationID = c("precise1", "precise2"),
    longitude = c(151.209300001, 151.209300002),
    latitude = c(-33.868800001, -33.868800002)
  )

  result <- find_nearest_neighbor_distance(data)

  expect_equal(nrow(result), 2)
  # Distance should be very small but positive
  expect_true(all(result$distance > 0))
  expect_true(all(result$distance < 1))  # Less than 1 meter
})

test_that("find_nearest_neighbor_distance handles equatorial locations", {
  data <- data.frame(
    locationID = c("eq1", "eq2", "eq3"),
    longitude = c(0, 1, 2),
    latitude = c(0, 0, 0)
  )

  result <- find_nearest_neighbor_distance(data)

  expect_equal(nrow(result), 3)
  expect_true(all(result$distance > 0))
})
