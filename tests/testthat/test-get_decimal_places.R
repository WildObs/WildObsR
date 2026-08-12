## Tests for get_decimal_places() ----

## Whole numbers ----

test_that("get_decimal_places returns zero for integers", {
  # an integer literal has nothing after a decimal point
  expect_equal(get_decimal_places(5L), 0)

  # a double that happens to be whole behaves the same way
  expect_equal(get_decimal_places(5), 0)

  # trailing zeros in the integer part must not be counted as decimals
  expect_equal(get_decimal_places(100), 0)
})

test_that("get_decimal_places returns zero for zero", {
  expect_equal(get_decimal_places(0), 0)

  # negative zero is still zero once the sign is stripped
  expect_equal(get_decimal_places(-0), 0)
})


## Typical decimals ----

test_that("get_decimal_places counts ordinary decimal places", {
  expect_equal(get_decimal_places(1.25), 2)
  expect_equal(get_decimal_places(0.001), 3)
  expect_equal(get_decimal_places(123.456789), 6)
})

test_that("get_decimal_places ignores trailing zeros", {
  # 1.10 is stored as 1.1, so only one decimal place is significant
  expect_equal(get_decimal_places(1.10), 1)

  # the same logic applied to a longer run of trailing zeros
  expect_equal(get_decimal_places(2.500), 1)
})


## Negative numbers ----

test_that("get_decimal_places ignores the sign", {
  # the minus sign is removed along with the integer part
  expect_equal(get_decimal_places(-1.5), 1)

  # a negative and its positive twin must agree
  expect_equal(get_decimal_places(-27.1234), get_decimal_places(27.1234))

  # negative whole numbers still have no decimals
  expect_equal(get_decimal_places(-100), 0)
})


## Missing values ----

test_that("get_decimal_places returns NA for NA input", {
  # a missing coordinate has no measurable precision, so NA propagates
  expect_true(is.na(get_decimal_places(NA)))
  expect_true(is.na(get_decimal_places(NA_real_)))
})

test_that("get_decimal_places propagates NA elementwise within a vector", {
  out <- get_decimal_places(c(1.25, NA, 1.5))

  # the real values are still counted normally either side of the gap
  expect_equal(out[1], 2)
  expect_true(is.na(out[2]))
  expect_equal(out[3], 1)
})


## Long decimals and the scientific-notation edge ----

test_that("get_decimal_places handles very long decimals", {
  # 13 significant decimal places survive as.character() intact
  expect_equal(get_decimal_places(0.1234567890123), 13)

  # a long coordinate-style value, the real use case for this helper
  expect_equal(get_decimal_places(-27.123456789), 9)
})

test_that("get_decimal_places returns zero when as.character uses scientific notation", {
  # 1e-05 has no literal "." in its character form, so nothing is counted.
  # This is a known limitation rather than a bug; documented in @details.
  expect_equal(get_decimal_places(1e-5), 0)
})


## Vectorisation ----

test_that("get_decimal_places is vectorised over its input", {
  # one result per input element, in the same order
  expect_equal(get_decimal_places(c(1.5, 2.25, 3)), c(1, 2, 0))

  # an empty input gives an empty result rather than an error
  expect_equal(length(get_decimal_places(numeric(0))), 0)
})
