## Tests for check_schema() ----

test_that("check_schema creates missing string field with NA", {
  data <- data.frame(field1 = c("a", "b", "c"), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(name = "field1", type = "string", constraints = NULL),
      list(name = "missing_field", type = "string", constraints = NULL)
    )
  )

  result <- suppressMessages(check_schema(schema, data))

  expect_true("missing_field" %in% names(result))
  expect_type(result$missing_field, "character")
  expect_true(all(is.na(result$missing_field)))
})

test_that("check_schema creates missing number field with NA", {
  data <- data.frame(field1 = c(1, 2, 3), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(name = "field1", type = "number", constraints = NULL),
      list(name = "missing_number", type = "number", constraints = NULL)
    )
  )

  result <- suppressMessages(check_schema(schema, data))

  expect_true("missing_number" %in% names(result))
  expect_type(result$missing_number, "double")
  expect_true(all(is.na(result$missing_number)))
})

test_that("check_schema creates missing integer field with NA", {
  data <- data.frame(field1 = c(1, 2, 3), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(name = "field1", type = "integer", constraints = NULL),
      list(name = "missing_int", type = "integer", constraints = NULL)
    )
  )

  result <- suppressMessages(check_schema(schema, data))

  expect_true("missing_int" %in% names(result))
  expect_type(result$missing_int, "integer")
  expect_true(all(is.na(result$missing_int)))
})

test_that("check_schema creates missing boolean field with NA", {
  data <- data.frame(field1 = c(TRUE, FALSE), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(name = "field1", type = "boolean", constraints = NULL),
      list(name = "missing_bool", type = "boolean", constraints = NULL)
    )
  )

  result <- suppressMessages(check_schema(schema, data))

  expect_true("missing_bool" %in% names(result))
  expect_type(result$missing_bool, "logical")
  expect_true(all(is.na(result$missing_bool)))
})

test_that("check_schema creates missing datetime field with NA", {
  data <- data.frame(field1 = as.POSIXct("2023-01-01 12:00:00"), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(name = "field1", type = "datetime", constraints = NULL),
      list(name = "missing_datetime", type = "datetime", constraints = NULL)
    )
  )

  result <- suppressMessages(check_schema(schema, data))

  expect_true("missing_datetime" %in% names(result))
  expect_s3_class(result$missing_datetime, "POSIXct")
  expect_true(all(is.na(result$missing_datetime)))
})

test_that("check_schema handles existing fields correctly", {
  data <- data.frame(
    field1 = c("a", "b", "c"),
    field2 = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  schema <- list(
    fields = list(
      list(name = "field1", type = "string", constraints = NULL),
      list(name = "field2", type = "number", constraints = NULL)
    )
  )

  result <- suppressMessages(check_schema(schema, data))

  expect_equal(nrow(result), nrow(data))
  expect_equal(result$field1, data$field1)
  expect_equal(result$field2, data$field2)
})

test_that("check_schema converts factors to character", {
  data <- data.frame(
    category = factor(c("A", "B", "C")),
    stringsAsFactors = TRUE
  )
  schema <- list(
    fields = list(
      list(name = "category", type = "string", constraints = NULL)
    )
  )

  result <- suppressMessages(check_schema(schema, data))

  expect_type(result$category, "character")
  expect_equal(result$category, c("A", "B", "C"))
})

test_that("check_schema handles POSIXct datetime fields", {
  data <- data.frame(
    timestamp = as.POSIXct(c("2023-01-01 12:00:00", "2023-12-31 23:59:59")),
    stringsAsFactors = FALSE
  )
  schema <- list(
    fields = list(
      list(name = "timestamp", type = "datetime", constraints = NULL)
    )
  )

  result <- suppressMessages(check_schema(schema, data))

  expect_s3_class(result$timestamp, "POSIXct")
})

test_that("check_schema accepts numeric for integer fields", {
  data <- data.frame(count = c(1.0, 2.0, 3.0), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(name = "count", type = "integer", constraints = NULL)
    )
  )

  # Should not produce error message about type mismatch
  output <- capture.output(
    result <- check_schema(schema, data),
    type = "message"
  )

  expect_false(any(grepl("Error: Field count has type", output)))
})

test_that("check_schema accepts integer for numeric fields", {
  data <- data.frame(value = c(1L, 2L, 3L), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(name = "value", type = "number", constraints = NULL)
    )
  )

  # Should not produce error message about type mismatch
  output <- capture.output(
    result <- check_schema(schema, data),
    type = "message"
  )

  expect_false(any(grepl("Error: Field value has type", output)))
})

test_that("check_schema recognizes ISO8601 datetime strings", {
  data <- data.frame(
    timestamp = c("2023-01-01T12:00:00+00:00", "2023-12-31T23:59:59Z"),
    stringsAsFactors = FALSE
  )
  schema <- list(
    fields = list(
      list(name = "timestamp", type = "datetime", constraints = NULL)
    )
  )

  # Should not produce error about type mismatch
  output <- capture.output(
    result <- check_schema(schema, data),
    type = "message"
  )

  expect_false(any(grepl("Error: Field timestamp has type", output)))
})

test_that("check_schema detects type mismatches", {
  data <- data.frame(value = c("a", "b", "c"), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(name = "value", type = "number", constraints = NULL)
    )
  )

  output <- capture.output(
    result <- check_schema(schema, data)
  )

  expect_true(any(grepl("Error: Field value has type character but expected numeric", output)))
})

test_that("check_schema checks minimum constraint for numeric fields", {
  data <- data.frame(latitude = c(-33.8688, -91.0, -35.0), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(
        name = "latitude",
        type = "number",
        constraints = list(minimum = -90, maximum = 90)
      )
    )
  )

  output <- capture.output(
    result <- check_schema(schema, data)
  )

  expect_true(any(grepl("Error: Field latitude has values below the minimum constraint", output)))
})

test_that("check_schema checks maximum constraint for numeric fields", {
  data <- data.frame(longitude = c(151.2093, 200.0, 153.0251), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(
        name = "longitude",
        type = "number",
        constraints = list(minimum = -180, maximum = 180)
      )
    )
  )

  output <- capture.output(
    result <- check_schema(schema, data)
  )

  expect_true(any(grepl("Error: Field longitude has values above the maximum constraint", output)))
})

test_that("check_schema checks required constraint for fields", {
  data <- data.frame(
    id = c("1", NA, "3"),
    stringsAsFactors = FALSE
  )
  schema <- list(
    fields = list(
      list(
        name = "id",
        type = "string",
        constraints = list(required = TRUE)
      )
    )
  )

  output <- capture.output(
    result <- check_schema(schema, data)
  )

  expect_true(any(grepl("Error: Field id is required but contains missing values", output)))
})

test_that("check_schema checks required datetime fields", {
  data <- data.frame(
    timestamp = as.POSIXct(c("2023-01-01 12:00:00", NA)),
    stringsAsFactors = FALSE
  )
  schema <- list(
    fields = list(
      list(
        name = "timestamp",
        type = "datetime",
        constraints = list(required = TRUE)
      )
    )
  )

  output <- capture.output(
    result <- check_schema(schema, data)
  )

  # Should produce error about missing values in datetime field
  expect_true(any(grepl("Error: Field timestamp", output)))
})

test_that("check_schema handles multiple fields with mixed types", {
  data <- data.frame(
    id = c("1", "2", "3"),
    count = c(10, 20, 30),
    active = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  schema <- list(
    fields = list(
      list(name = "id", type = "string", constraints = NULL),
      list(name = "count", type = "number", constraints = NULL),
      list(name = "active", type = "boolean", constraints = NULL)
    )
  )

  result <- suppressMessages(check_schema(schema, data))

  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 3)
})

test_that("check_schema returns modified data frame", {
  data <- data.frame(field1 = c(1, 2, 3), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(name = "field1", type = "number", constraints = NULL),
      list(name = "field2", type = "string", constraints = NULL)
    )
  )

  result <- suppressMessages(check_schema(schema, data))

  expect_s3_class(result, "data.frame")
  expect_true("field1" %in% names(result))
  expect_true("field2" %in% names(result))
})

test_that("check_schema handles empty constraints gracefully", {
  data <- data.frame(value = c(1, 2, 3), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(name = "value", type = "number", constraints = NULL)
    )
  )

  # check_schema uses cat() which produces output, so we just check it doesn't error
  result <- suppressMessages(capture.output(check_schema(schema, data)))
  expect_true(TRUE)  # If we got here without error, test passes
})

test_that("check_schema validates fields within numeric constraints", {
  data <- data.frame(
    latitude = c(-33.8688, -35.0, -40.0),
    longitude = c(151.2093, 153.0251, 145.0)
  )
  schema <- list(
    fields = list(
      list(
        name = "latitude",
        type = "number",
        constraints = list(minimum = -90, maximum = 90)
      ),
      list(
        name = "longitude",
        type = "number",
        constraints = list(minimum = -180, maximum = 180)
      )
    )
  )

  output <- capture.output(
    result <- check_schema(schema, data),
    type = "message"
  )

  # Should not have constraint errors for these valid values
  expect_false(any(grepl("has values below the minimum constraint", output)))
  expect_false(any(grepl("has values above the maximum constraint", output)))
})
