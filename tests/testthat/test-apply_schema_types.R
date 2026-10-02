## Tests for apply_schema_types() ----

test_that("apply_schema_types converts integer type correctly", {
  data <- data.frame(count = c("1", "2", "3"), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(name = "count", type = "integer", format = NULL, constraints = NULL)
    )
  )

  result <- apply_schema_types(data, schema)

  expect_type(result$count, "integer")
  expect_equal(result$count, c(1L, 2L, 3L))
})

test_that("apply_schema_types converts number type correctly", {
  data <- data.frame(value = c("1.5", "2.7", "3.9"), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(name = "value", type = "number", format = NULL, constraints = NULL)
    )
  )

  result <- apply_schema_types(data, schema)

  expect_type(result$value, "double")
  expect_equal(result$value, c(1.5, 2.7, 3.9))
})

test_that("apply_schema_types converts boolean type correctly", {
  data <- data.frame(flag = as.logical(c("TRUE", "FALSE", "T", "F")), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(name = "flag", type = "boolean", format = NULL, constraints = NULL)
    )
  )

  result <- apply_schema_types(data, schema)

  expect_type(result$flag, "logical")
  expect_equal(result$flag, c(TRUE, FALSE, TRUE, FALSE))
})

test_that("apply_schema_types converts string type correctly", {
  data <- data.frame(name = c(1, 2, 3), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(name = "name", type = "string", format = NULL, constraints = NULL)
    )
  )

  result <- apply_schema_types(data, schema)

  expect_type(result$name, "character")
  expect_equal(result$name, c("1", "2", "3"))
})

test_that("apply_schema_types converts string with enum to factor", {
  data <- data.frame(category = c("A", "B", "A", "C"), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(
        name = "category",
        type = "string",
        format = NULL,
        constraints = list(enum = c("A", "B", "C"))
      )
    )
  )

  result <- apply_schema_types(data, schema)

  expect_s3_class(result$category, "factor")
  expect_equal(levels(result$category), c("A", "B", "C"))
})

test_that("apply_schema_types converts factor type correctly", {
  data <- data.frame(status = c("active", "inactive", "active"), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(name = "status", type = "factor", format = NULL, constraints = NULL)
    )
  )

  result <- apply_schema_types(data, schema)

  expect_s3_class(result$status, "factor")
})

test_that("apply_schema_types converts date type correctly", {
  data <- data.frame(date = c("2023-01-01", "2023-12-31", "2024-06-15"), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(name = "date", type = "date", format = "%Y-%m-%d", constraints = NULL)
    )
  )

  result <- apply_schema_types(data, schema)

  expect_s3_class(result$date, "Date")
  expect_equal(result$date[1], as.Date("2023-01-01"))
})

test_that("apply_schema_types converts datetime with standard format", {
  data <- data.frame(
    timestamp = c("2023-01-01 12:00:00", "2023-12-31 23:59:59"),
    stringsAsFactors = FALSE
  )
  schema <- list(
    fields = list(
      list(
        name = "timestamp",
        type = "datetime",
        format = "%Y-%m-%d %H:%M:%S",
        constraints = NULL
      )
    )
  )

  result <- apply_schema_types(data, schema)
  expect_s3_class(result$timestamp, "POSIXct")
  expect_true(grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", as.character(result$timestamp[1])))
})

test_that("apply_schema_types handles datetime with ISO 8601 format", {
  data <- data.frame(
    timestamp = c("2023-01-01T12:00:00", "2023-12-31T23:59:59"),
    stringsAsFactors = FALSE
  )
  schema <- list(
    fields = list(
      list(
        name = "timestamp",
        type = "datetime",
        format = "%Y-%m-%dT%H:%M:%S",
        constraints = NULL
      )
    )
  )

  result <- apply_schema_types(data, schema)

  expect_s3_class(result$timestamp, "POSIXct")
  expect_true(grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", as.character(result$timestamp[1])))
})

test_that("apply_schema_types tries common datetime formats when parsing fails", {
  data <- data.frame(
    timestamp = c("2023-01-01 12:00:00", "2023-12-31 23:59:59"),
    stringsAsFactors = FALSE
  )
  schema <- list(
    fields = list(
      list(
        name = "timestamp",
        type = "datetime",
        format = "%Y/%m/%d %H:%M:%S",  # Wrong format, should fallback
        constraints = NULL
      )
    )
  )

  result <- apply_schema_types(data, schema)

  expect_s3_class(result$timestamp, "POSIXct")
  expect_true(grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", as.character(result$timestamp[1])))
})

test_that("apply_schema_types warns on unknown field type", {
  data <- data.frame(value = c(1, 2, 3), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(name = "value", type = "unknown_type", format = NULL, constraints = NULL)
    )
  )

  expect_warning(
    apply_schema_types(data, schema),
    "Unknown field type: unknown_type for column: value"
  )
})

test_that("apply_schema_types skips fields not present in data", {
  data <- data.frame(field1 = c(1, 2, 3), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(name = "field1", type = "integer", format = NULL, constraints = NULL),
      list(name = "field2", type = "string", format = NULL, constraints = NULL)
    )
  )

  result <- apply_schema_types(data, schema)

  expect_true("field1" %in% names(result))
  expect_false("field2" %in% names(result))
})

test_that("apply_schema_types handles multiple fields", {
  data <- data.frame(
    id = c("1", "2", "3"),
    value = c("1.5", "2.5", "3.5"),
    flag = c("TRUE", "FALSE", "TRUE"),
    stringsAsFactors = FALSE
  )
  schema <- list(
    fields = list(
      list(name = "id", type = "integer", format = NULL, constraints = NULL),
      list(name = "value", type = "number", format = NULL, constraints = NULL),
      list(name = "flag", type = "boolean", format = NULL, constraints = NULL)
    )
  )

  result <- apply_schema_types(data, schema)

  expect_type(result$id, "integer")
  expect_type(result$value, "double")
  expect_type(result$flag, "logical")
})

test_that("apply_schema_types handles NA values in numeric conversion", {
  data <- data.frame(value = c("1", "invalid", "3"), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(name = "value", type = "number", format = NULL, constraints = NULL)
    )
  )

  result <- suppressWarnings(apply_schema_types(data, schema))

  expect_true(is.na(result$value[2]))
  expect_equal(result$value[c(1, 3)], c(1, 3))
})

test_that("apply_schema_types warns when date-time parsing fails", {
  data <- data.frame(date = c("not-a-date", "2023-13-45"), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(name = "date", type = "datetime", format = "%Y-%m-%d %H:%M:%S", constraints = NULL)
    )
  )

  expect_warning(
    apply_schema_types(data, schema),
    "Failed to parse datetime"
  )
})

test_that("apply_schema_types warns when datetime parsing fails completely", {
  data <- data.frame(
    timestamp = c("completely-invalid", "not-a-date"),
    stringsAsFactors = FALSE
  )
  schema <- list(
    fields = list(
      list(
        name = "timestamp",
        type = "datetime",
        format = "%Y-%m-%d %H:%M:%S",
        constraints = NULL
      )
    )
  )

  expect_warning(
    apply_schema_types(data, schema),
    "Failed to parse datetime for column: timestamp"
  )
})

test_that("apply_schema_types gives an all-empty datetime column the POSIXct type", {
  data <- data.frame(ts = c(NA, NA))
  schema <- list(fields = list(list(name = "ts", type = "datetime",
                                    format = "%Y-%m-%dT%H:%M:%S%z")))

  # nothing to parse is not a failure, so no warning
  expect_no_warning(result <- apply_schema_types(data, schema, timezone = "Australia/Brisbane"))
  # an empty datetime is still a datetime, in the requested timezone
  expect_s3_class(result$ts, "POSIXct")
  expect_equal(attr(result$ts, "tzone"), "Australia/Brisbane")
  expect_true(all(is.na(result$ts)))
})

test_that("apply_schema_types converts a partly empty datetime column", {
  data <- data.frame(ts = c("2024-01-15T08:00:00+1000", NA), stringsAsFactors = FALSE)
  schema <- list(fields = list(list(name = "ts", type = "datetime",
                                    format = "%Y-%m-%dT%H:%M:%S%z")))

  # empty cells are not parse failures, so the real value converts and no warning fires
  expect_no_warning(result <- apply_schema_types(data, schema))
  expect_s3_class(result$ts, "POSIXct")
  expect_false(is.na(result$ts[1]))
  expect_true(is.na(result$ts[2]))
})

test_that("apply_schema_types parses a datetime field that has no format", {
  data <- data.frame(ts = c("2024-01-15 08:00:00", "2024-02-01 09:30:00"), stringsAsFactors = FALSE)
  schema <- list(fields = list(list(name = "ts", type = "datetime")))

  result <- apply_schema_types(data, schema)

  # the column survives and is converted using the fallback formats
  expect_true("ts" %in% names(result))
  expect_s3_class(result$ts, "POSIXct")
})

test_that("apply_schema_types never deletes a datetime column it cannot parse", {
  data <- data.frame(ts = c("not a date", "also not"), stringsAsFactors = FALSE)
  schema <- list(fields = list(list(name = "ts", type = "datetime")))

  # a failed parse warns and leaves the original values in place
  expect_warning(result <- apply_schema_types(data, schema), "Failed to parse datetime")
  expect_identical(result$ts, data$ts)
})

test_that("apply_schema_types returns data frame with same structure", {
  data <- data.frame(
    col1 = c(1, 2, 3),
    col2 = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  schema <- list(
    fields = list(
      list(name = "col1", type = "integer", format = NULL, constraints = NULL)
    )
  )

  result <- apply_schema_types(data, schema)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(data))
  expect_equal(ncol(result), ncol(data))
})
