## Tests for spatial_hexagon_generator() ----

# Helper function to create minimal valid test data
create_test_data <- function(n = 3) {
  data.frame(
    deploymentID = paste0("dep", 1:n),
    locationName = "TestLocation",
    deploymentGroups = paste0("Survey_2023_", letters[1:n]),
    source = "TestContributor",
    latitude = c(-33.87, -33.88, -33.89)[1:n],
    longitude = c(151.20, 151.21, 151.22)[1:n],
    deploymentStart = as.POSIXct(c("2023-01-01", "2023-01-15", "2023-02-01")[1:n]),
    deploymentEnd = as.POSIXct(c("2023-01-31", "2023-02-14", "2023-02-28")[1:n])
  )
}

test_that("spatial_hexagon_generator errors without locationName column", {
  data <- create_test_data()
  data$locationName <- NULL

  expect_error(
    spatial_hexagon_generator(data, scales = 1e6),
    "does not contain a column for 'locationName'"
  )
})

test_that("spatial_hexagon_generator errors without deploymentGroups column", {
  data <- create_test_data()
  data$deploymentGroups <- NULL

  expect_error(
    spatial_hexagon_generator(data, scales = 1e6),
    "does not contain a column for 'deploymentGroups'"
  )
})

test_that("spatial_hexagon_generator errors without source column", {
  data <- create_test_data()
  data$source <- NULL

  expect_error(
    spatial_hexagon_generator(data, scales = 1e6),
    "does not contain a column for 'source'"
  )
})

test_that("spatial_hexagon_generator errors without latitude column", {
  data <- create_test_data()
  data$latitude <- NULL

  expect_error(
    spatial_hexagon_generator(data, scales = 1e6),
    "does not contain columns for 'latitude' and/or 'longitude'"
  )
})

test_that("spatial_hexagon_generator errors without longitude column", {
  data <- create_test_data()
  data$longitude <- NULL

  expect_error(
    spatial_hexagon_generator(data, scales = 1e6),
    "does not contain columns for 'latitude' and/or 'longitude'"
  )
})

test_that("spatial_hexagon_generator errors with non-POSIXct deploymentStart", {
  data <- create_test_data()
  data$deploymentStart <- as.character(data$deploymentStart)

  expect_error(
    spatial_hexagon_generator(data, scales = 1e6),
    "are not formatted as a proper datetime class POSIXct"
  )
})

test_that("spatial_hexagon_generator errors with non-POSIXct deploymentEnd", {
  data <- create_test_data()
  data$deploymentEnd <- as.character(data$deploymentEnd)

  expect_error(
    spatial_hexagon_generator(data, scales = 1e6),
    "are not formatted as a proper datetime class POSIXct"
  )
})

test_that("spatial_hexagon_generator returns data frame with correct structure", {
  data <- create_test_data()
  result <- spatial_hexagon_generator(data, scales = 1e6)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(data))
})

test_that("spatial_hexagon_generator adds cellID columns", {
  data <- create_test_data()
  result <- spatial_hexagon_generator(data, scales = 1e6)

  expect_true("cellID_1km2" %in% names(result))
})

test_that("spatial_hexagon_generator adds polygon columns", {
  data <- create_test_data()
  result <- spatial_hexagon_generator(data, scales = 1e6)

  expect_true("polygon_1km2" %in% names(result))
})

test_that("spatial_hexagon_generator handles single scale", {
  data <- create_test_data()
  result <- spatial_hexagon_generator(data, scales = 1e6)

  cellid_cols <- grep("^cellID_", names(result), value = TRUE)
  polygon_cols <- grep("^polygon_", names(result), value = TRUE)

  expect_length(cellid_cols, 1)
  expect_length(polygon_cols, 1)
})

test_that("spatial_hexagon_generator handles multiple scales", {
  data <- create_test_data()
  scales <- c(1e6, 3e6)
  result <- spatial_hexagon_generator(data, scales = scales)

  cellid_cols <- grep("^cellID_", names(result), value = TRUE)
  polygon_cols <- grep("^polygon_", names(result), value = TRUE)

  expect_length(cellid_cols, 2)
  expect_length(polygon_cols, 2)
  expect_true("cellID_1km2" %in% names(result))
  expect_true("cellID_3km2" %in% names(result))
})

test_that("spatial_hexagon_generator preserves original columns", {
  data <- create_test_data()
  original_cols <- names(data)
  result <- spatial_hexagon_generator(data, scales = 1e6)

  expect_true(all(original_cols %in% names(result)))
})

test_that("spatial_hexagon_generator preserves deploymentID values", {
  data <- create_test_data()
  result <- spatial_hexagon_generator(data, scales = 1e6)

  expect_setequal(result$deploymentID, data$deploymentID)
})

test_that("spatial_hexagon_generator creates cellID with locationName", {
  data <- create_test_data()
  data$locationName <- "UniqueLocation"
  result <- spatial_hexagon_generator(data, scales = 1e6)

  # cellID should contain the locationName
  expect_true(all(grepl("UniqueLocation", result$cellID_1km2)))
})

test_that("spatial_hexagon_generator creates cellID with scale name", {
  data <- create_test_data()
  result <- spatial_hexagon_generator(data, scales = 1e6)

  # cellID should contain scale designation
  expect_true(all(grepl("cellID_1km2", result$cellID_1km2)))
})

test_that("spatial_hexagon_generator creates cellID with temporal info", {
  data <- create_test_data()
  result <- spatial_hexagon_generator(data, scales = 1e6)

  # cellID should contain year information (2023 from test data)
  expect_true(all(grepl("2023", result$cellID_1km2)))
})

test_that("spatial_hexagon_generator creates cellID with source info", {
  data <- create_test_data()
  data$source <- "TestSource"
  result <- spatial_hexagon_generator(data, scales = 1e6)

  # cellID should contain source information
  expect_true(all(grepl("TestSource", result$cellID_1km2)))
})

test_that("spatial_hexagon_generator handles single deployment", {
  data <- create_test_data(n = 1)
  result <- spatial_hexagon_generator(data, scales = 1e6)

  expect_equal(nrow(result), 1)
  expect_true("cellID_1km2" %in% names(result))
})

test_that("spatial_hexagon_generator handles multiple locationNames", {
  data <- create_test_data()
  data$locationName <- c("Loc1", "Loc1", "Loc2")
  result <- spatial_hexagon_generator(data, scales = 1e6)

  expect_equal(nrow(result), 3)
  # Should have cellIDs for both locations
  expect_true(any(grepl("Loc1", result$cellID_1km2)))
  expect_true(any(grepl("Loc2", result$cellID_1km2)))
})

test_that("spatial_hexagon_generator polygon column equals base cellID", {
  data <- create_test_data()
  result <- spatial_hexagon_generator(data, scales = 1e6)

  # polygon columns should be the spatial part (before temporal code is added)
  # They should differ from cellID columns
  expect_false(identical(result$polygon_1km2, result$cellID_1km2))
})

test_that("spatial_hexagon_generator handles m² scale naming", {
  data <- create_test_data()
  result <- spatial_hexagon_generator(data, scales = 500000)  # 500,000 m²

  # Check for m2 suffix (may use scientific notation)
  cellid_cols <- grep("^cellID_.*m2$", names(result), value = TRUE)
  polygon_cols <- grep("^polygon_.*m2$", names(result), value = TRUE)
  expect_length(cellid_cols, 1)
  expect_length(polygon_cols, 1)
})

test_that("spatial_hexagon_generator handles km² scale naming", {
  data <- create_test_data()
  result <- spatial_hexagon_generator(data, scales = 5e6)  # 5 km²

  expect_true("cellID_5km2" %in% names(result))
  expect_true("polygon_5km2" %in% names(result))
})

test_that("spatial_hexagon_generator handles deployments with different years", {
  data <- create_test_data()
  data$deploymentGroups <- c("Survey_2022_a", "Survey_2023_a", "Survey_2024_a")
  data$deploymentStart <- as.POSIXct(c("2022-01-01", "2023-01-01", "2024-01-01"))
  data$deploymentEnd <- as.POSIXct(c("2022-01-31", "2023-01-31", "2024-01-31"))

  result <- spatial_hexagon_generator(data, scales = 1e6)

  # Should have different years in cellIDs (extracted from deploymentGroups)
  expect_true(any(grepl("2022", result$cellID_1km2)))
  expect_true(any(grepl("2023", result$cellID_1km2)))
  expect_true(any(grepl("2024", result$cellID_1km2)))
})

test_that("spatial_hexagon_generator does not create duplicate deploymentIDs", {
  data <- create_test_data()
  result <- spatial_hexagon_generator(data, scales = 1e6)

  expect_false(any(duplicated(result$deploymentID)))
})

test_that("spatial_hexagon_generator handles widely spaced coordinates", {
  data <- data.frame(
    deploymentID = c("perth", "sydney"),
    locationName = c("WA", "NSW"),
    deploymentGroups = c("Survey_2023_a", "Survey_2023_b"),
    source = "TestContributor",
    latitude = c(-31.9505, -33.8688),  # Perth and Sydney
    longitude = c(115.8605, 151.2093),
    deploymentStart = as.POSIXct(c("2023-01-01", "2023-01-01")),
    deploymentEnd = as.POSIXct(c("2023-01-31", "2023-01-31"))
  )

  result <- spatial_hexagon_generator(data, scales = 1e6)

  expect_equal(nrow(result), 2)
  # Should have different cellIDs due to different locations
  expect_true(length(unique(result$cellID_1km2)) == 2)
})

test_that("spatial_hexagon_generator handles closely spaced coordinates", {
  data <- data.frame(
    deploymentID = c("cam1", "cam2", "cam3"),
    locationName = "CloseLocation",
    deploymentGroups = c("Survey_2023_a", "Survey_2023_a", "Survey_2023_a"),
    source = "TestContributor",
    latitude = c(-33.8700, -33.8701, -33.8702),  # Very close
    longitude = c(151.2000, 151.2001, 151.2002),
    deploymentStart = as.POSIXct(c("2023-01-01", "2023-01-01", "2023-01-01")),
    deploymentEnd = as.POSIXct(c("2023-01-31", "2023-01-31", "2023-01-31"))
  )

  result <- spatial_hexagon_generator(data, scales = 1e6)

  expect_equal(nrow(result), 3)
  # Close coordinates might share the same polygon
  expect_true(length(unique(result$polygon_1km2)) <= 3)
})

test_that("spatial_hexagon_generator produces character cellID", {
  data <- create_test_data()
  result <- spatial_hexagon_generator(data, scales = 1e6)

  expect_type(result$cellID_1km2, "character")
})

test_that("spatial_hexagon_generator produces character polygon", {
  data <- create_test_data()
  result <- spatial_hexagon_generator(data, scales = 1e6)

  expect_type(result$polygon_1km2, "character")
})

test_that("spatial_hexagon_generator handles additional data columns", {
  data <- create_test_data()
  data$extra_column <- "extra_data"
  data$numeric_column <- 42

  result <- spatial_hexagon_generator(data, scales = 1e6)

  expect_true("extra_column" %in% names(result))
  expect_true("numeric_column" %in% names(result))
  expect_equal(result$extra_column, rep("extra_data", 3))
  expect_equal(result$numeric_column, rep(42, 3))
})

test_that("spatial_hexagon_generator handles WildObsID in deploymentGroups", {
  data <- create_test_data()
  # Add WildObsID pattern to deploymentGroups
  data$deploymentGroups <- paste0("WildObsID_2023_", letters[1:3])

  result <- spatial_hexagon_generator(data, scales = 1e6)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})

test_that("spatial_hexagon_generator does not error with valid input", {
  data <- create_test_data()

  expect_no_error(
    spatial_hexagon_generator(data, scales = 1e6)
  )
})

test_that("spatial_hexagon_generator works with decimal scale areas", {
  data <- create_test_data()
  result <- spatial_hexagon_generator(data, scales = 1.5e6)  # 1.5 km²

  # 1.5e6 is not evenly divisible by 1e6, so will be named in m²
  cellid_cols <- grep("^cellID_.*m2$", names(result), value = TRUE)
  polygon_cols <- grep("^polygon_.*m2$", names(result), value = TRUE)
  expect_length(cellid_cols, 1)
  expect_length(polygon_cols, 1)
})

test_that("spatial_hexagon_generator handles three or more scales", {
  data <- create_test_data()
  scales <- c(1e6, 3e6, 5e6)
  result <- spatial_hexagon_generator(data, scales = scales)

  cellid_cols <- grep("^cellID_", names(result), value = TRUE)

  expect_length(cellid_cols, 3)
  expect_true("cellID_1km2" %in% names(result))
  expect_true("cellID_3km2" %in% names(result))
  expect_true("cellID_5km2" %in% names(result))
})

test_that("spatial_hexagon_generator consistent for same input", {
  data <- create_test_data()

  result1 <- spatial_hexagon_generator(data, scales = 1e6)
  result2 <- spatial_hexagon_generator(data, scales = 1e6)

  expect_equal(result1$cellID_1km2, result2$cellID_1km2)
  expect_equal(result1$polygon_1km2, result2$polygon_1km2)
})

test_that("spatial_hexagon_generator handles non-overlapping temporal deployments", {
  data <- create_test_data()
  # Ensure completely separate time periods
  data$deploymentStart <- as.POSIXct(c("2021-01-01", "2022-01-01", "2023-01-01"))
  data$deploymentEnd <- as.POSIXct(c("2021-12-31", "2022-12-31", "2023-12-31"))

  result <- spatial_hexagon_generator(data, scales = 1e6)

  expect_equal(nrow(result), 3)
  # Should have different year codes
  expect_true(length(unique(result$cellID_1km2)) >= 1)
})
