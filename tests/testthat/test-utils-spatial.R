## Tests for spatial utility functions in utils.R ----

## Tests for area_to_apothem() ----

test_that("area_to_apothem converts 1 km² correctly", {
  result <- area_to_apothem(1e6)

  expect_type(result, "double")
  expect_length(result, 1)
  expect_true(result > 0)
  expect_equal(names(result), "1km2")
})

test_that("area_to_apothem converts multiple areas", {
  areas <- c(1e6, 3e6, 5e6)
  result <- area_to_apothem(areas)

  expect_length(result, 3)
  expect_equal(names(result), c("1km2", "3km2", "5km2"))
  expect_true(all(result > 0))
})

test_that("area_to_apothem names km² areas correctly", {
  result <- area_to_apothem(c(1e6, 10e6, 100e6))

  expect_equal(names(result), c("1km2", "10km2", "100km2"))
})

test_that("area_to_apothem names m² areas correctly", {
  # Areas that are NOT divisible by 1e6 should be named in m²
  result <- area_to_apothem(c(500000, 750000))

  # Function may use scientific notation for large numbers
  expect_true(grepl("m2$", names(result)[1]))
  expect_true(grepl("m2$", names(result)[2]))
})

test_that("area_to_apothem handles mixed km² and m² areas", {
  areas <- c(1e6, 500000, 3e6, 250000)
  result <- area_to_apothem(areas)

  # Check that we have km2 and m2 suffixes
  expect_equal(names(result)[1], "1km2")
  expect_true(grepl("m2$", names(result)[2]))
  expect_equal(names(result)[3], "3km2")
  expect_true(grepl("m2$", names(result)[4]))
})

test_that("area_to_apothem errors on negative areas", {
  expect_error(
    area_to_apothem(-1e6),
    "`scales` must be numeric and positive"
  )
})

test_that("area_to_apothem errors on zero area", {
  expect_error(
    area_to_apothem(0),
    "`scales` must be numeric and positive"
  )
})

test_that("area_to_apothem errors on non-numeric input", {
  expect_error(
    area_to_apothem("1000000"),
    "`scales` must be numeric and positive"
  )
})

test_that("area_to_apothem errors on NA input", {
  expect_error(
    area_to_apothem(NA),
    "`scales` must be numeric and positive"
  )
})

test_that("area_to_apothem errors on NULL input", {
  expect_error(
    area_to_apothem(NULL),
    "`scales` must be numeric and positive"
  )
})

test_that("area_to_apothem returns correct mathematical relationship", {
  # For a hexagon with area A:
  # side = sqrt((2*A) / (3*sqrt(3)))
  # apothem = (side * sqrt(3)) / 2

  area <- 1e6  # 1 km²
  result <- area_to_apothem(area)

  # Calculate expected value
  side <- sqrt((2 * area) / (3 * sqrt(3)))
  expected_apothem <- (side * sqrt(3)) / 2

  expect_equal(result[[1]], expected_apothem)
})

test_that("area_to_apothem handles very small areas", {
  # Test with areas smaller than 1 km²
  small_area <- 100  # 100 m²
  result <- area_to_apothem(small_area)

  expect_true(result > 0)
  expect_equal(names(result), "100m2")
})

test_that("area_to_apothem handles very large areas", {
  # Test with very large areas
  large_area <- 1e9  # 1000 km²
  result <- area_to_apothem(large_area)

  expect_true(result > 0)
  expect_equal(names(result), "1000km2")
})

test_that("area_to_apothem produces increasing apothems for increasing areas", {
  areas <- c(1e6, 2e6, 3e6, 4e6, 5e6)
  result <- area_to_apothem(areas)

  # Apothems should increase with area
  expect_true(all(diff(result) > 0))
})

test_that("area_to_apothem returns named vector", {
  result <- area_to_apothem(1e6)

  expect_true(is.vector(result))
  expect_false(is.null(names(result)))
  expect_length(names(result), 1)
})

test_that("area_to_apothem handles single area value", {
  result <- area_to_apothem(5e6)

  expect_length(result, 1)
  expect_equal(names(result), "5km2")
  expect_type(result, "double")
})

test_that("area_to_apothem errors on vector containing negative values", {
  expect_error(
    area_to_apothem(c(1e6, -500000, 3e6)),
    "`scales` must be numeric and positive"
  )
})

test_that("area_to_apothem errors on vector containing zero", {
  expect_error(
    area_to_apothem(c(1e6, 0, 3e6)),
    "`scales` must be numeric and positive"
  )
})

test_that("area_to_apothem consistent results for same input", {
  result1 <- area_to_apothem(1e6)
  result2 <- area_to_apothem(1e6)

  expect_equal(result1, result2)
})

test_that("area_to_apothem handles decimal km² areas", {
  # 1.5 km² = 1500000 m²
  result <- area_to_apothem(1.5e6)

  # 1.5e6 is not evenly divisible by 1e6, so it will be named in m²
  expect_true(grepl("m2$", names(result)))
  expect_true(result > 0)
})

test_that("area_to_apothem produces numeric output without attributes other than names", {
  result <- area_to_apothem(1e6)

  expect_true(is.numeric(result))
  # Should only have names attribute
  expect_equal(length(attributes(result)), 1)
  expect_true("names" %in% names(attributes(result)))
})
