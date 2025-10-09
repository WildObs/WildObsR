test_that("long_to_UTM_zone converts longitude to correct UTM zone", {
  # Test single longitude values with known UTM zones
  expect_equal(long_to_UTM_zone(0), 31)      # Prime meridian
  expect_equal(long_to_UTM_zone(-180), 1)    # Date line west
  expect_equal(long_to_UTM_zone(180), 1)     # Date line east (wraps to zone 1)
  expect_equal(long_to_UTM_zone(-3), 30)     # West of prime meridian
  expect_equal(long_to_UTM_zone(3), 31)      # East of prime meridian

  # Test Australian coordinates
  expect_equal(long_to_UTM_zone(151.2093), 56)  # Sydney
  expect_equal(long_to_UTM_zone(153.0251), 56)  # Brisbane
  expect_equal(long_to_UTM_zone(144.9631), 55)  # Melbourne
  expect_equal(long_to_UTM_zone(115.8605), 50)  # Perth

  # Test boundary values
  expect_equal(long_to_UTM_zone(-174), 2)    # Near zone 1 boundary
  expect_equal(long_to_UTM_zone(174), 60)    # Zone 60

  # Test vector input
  longitudes <- c(0, 3, 6, 9)
  expected_zones <- c(31, 31, 32, 32)
  expect_equal(long_to_UTM_zone(longitudes), expected_zones)

  # Test example from documentation
  long <- c(120, 60, 21)
  expect_equal(long_to_UTM_zone(long), c(51, 41, 34))
})

test_that("long_to_UTM_zone handles edge cases", {
  # Test near-boundary values
  expect_equal(long_to_UTM_zone(-179.9), 1)
  expect_equal(long_to_UTM_zone(179.9), 60)

  # Test values that require floor operation
  expect_equal(long_to_UTM_zone(5.9), 31)
  expect_equal(long_to_UTM_zone(6.0), 32)
  expect_equal(long_to_UTM_zone(6.1), 32)

  # Test negative longitudes in different zones
  expect_equal(long_to_UTM_zone(-120), 11)   # Western North America
  expect_equal(long_to_UTM_zone(-75), 18)    # Eastern North America
  expect_equal(long_to_UTM_zone(-60), 21)    # South America
})

test_that("long_to_UTM_zone returns numeric values", {
  result <- long_to_UTM_zone(151.2093)
  expect_type(result, "double")

  # Test with vector
  result_vector <- long_to_UTM_zone(c(0, 30, 60))
  expect_type(result_vector, "double")
  expect_length(result_vector, 3)
})

test_that("long_to_UTM_zone zones are always between 1 and 60", {
  # Test random longitudes across the valid range
  test_longs <- seq(-180, 180, by = 10)
  zones <- long_to_UTM_zone(test_longs)

  expect_true(all(zones >= 1 & zones <= 60))
})
