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
  data <- data.frame(flag = c("TRUE", "FALSE", "T", "F"), stringsAsFactors = FALSE)
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

  expect_type(result$timestamp, "character")
  expect_true(grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}", result$timestamp[1]))
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

  expect_type(result$timestamp, "character")
  expect_true(grepl("T", result$timestamp[1]))
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

  expect_type(result$timestamp, "character")
  expect_true(grepl("T", result$timestamp[1]))
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

test_that("apply_schema_types warns when date parsing fails", {
  data <- data.frame(date = c("invalid-date", "2023-01-01"), stringsAsFactors = FALSE)
  schema <- list(
    fields = list(
      list(name = "date", type = "date", format = "%Y-%m-%d", constraints = NULL)
    )
  )

  expect_warning(
    apply_schema_types(data, schema),
    "Failed to parse date for column: date"
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
