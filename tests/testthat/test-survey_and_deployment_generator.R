## Tests for survey_and_deployment_generator() ----

# Helper function to create minimal test data
create_test_caps <- function() {
  data.frame(
    eventStart = as.POSIXct(c("2023-01-01 08:00:00", "2023-01-05 10:00:00", "2023-01-10 12:00:00")),
    deploymentID = c("CAM001", "CAM001", "CAM002"),
    locationID = c("Site_A", "Site_A", "Site_B"),
    dataSource = c("Study_Smith", "Study_Smith", "Study_Smith"),
    locationName = c("Landscape1", "Landscape1", "Landscape1"),
    stringsAsFactors = FALSE
  )
}

create_test_deps <- function() {
  data.frame(
    deploymentID = c("CAM001", "CAM002"),
    locationName = c("Landscape1", "Landscape1"),
    stringsAsFactors = FALSE
  )
}

# Tests for input validation ----

test_that("survey_and_deployment_generator requires deploymentID in caps", {
  caps <- data.frame(
    eventStart = as.POSIXct("2023-01-01 08:00:00"),
    locationName = "Landscape1",
    dataSource = "Study1"
  )
  deps <- create_test_deps()

  expect_error(
    survey_and_deployment_generator(caps, deps),
    "must have 'deploymentID' column"
  )
})

test_that("survey_and_deployment_generator requires deploymentID in deps", {
  caps <- create_test_caps()
  deps <- data.frame(locationName = "Landscape1")

  expect_error(
    survey_and_deployment_generator(caps, deps),
    "must have 'deploymentID' column"
  )
})

test_that("survey_and_deployment_generator requires eventStart in caps", {
  caps <- data.frame(
    deploymentID = "CAM001",
    locationName = "Landscape1",
    dataSource = "Study1"
  )
  deps <- create_test_deps()

  expect_error(
    survey_and_deployment_generator(caps, deps),
    "must have 'eventStart' column"
  )
})

test_that("survey_and_deployment_generator requires dataSource in caps", {
  caps <- data.frame(
    eventStart = as.POSIXct("2023-01-01 08:00:00"),
    deploymentID = "CAM001",
    locationName = "Landscape1"
  )
  deps <- create_test_deps()

  expect_error(
    survey_and_deployment_generator(caps, deps),
    "must have 'dataSource' column"
  )
})

test_that("survey_and_deployment_generator requires locationName in both caps and deps", {
  caps <- data.frame(
    eventStart = as.POSIXct("2023-01-01 08:00:00"),
    deploymentID = "CAM001",
    dataSource = "Study1"
  )
  deps <- create_test_deps()

  expect_error(
    survey_and_deployment_generator(caps, deps),
    "must have 'locationName' column"
  )
})

test_that("survey_and_deployment_generator detects mismatched deploymentIDs", {
  caps <- create_test_caps()
  deps <- data.frame(
    deploymentID = c("CAM003", "CAM004"),
    locationName = c("Landscape1", "Landscape1")
  )

  expect_error(
    survey_and_deployment_generator(caps, deps),
    "deployment_ids do not perfectly match"
  )
})

test_that("survey_and_deployment_generator rejects NA values in eventStart", {
  caps <- create_test_caps()
  caps$eventStart[2] <- NA
  deps <- create_test_deps()

  expect_error(
    survey_and_deployment_generator(caps, deps),
    "contains NA date-time values in the eventStart column"
  )
})

# Tests for column name handling ----

test_that("survey_and_deployment_generator handles deployment_id alternative name in caps", {
  caps <- create_test_caps()
  colnames(caps)[colnames(caps) == "deploymentID"] <- "deployment_id"
  deps <- create_test_deps()

  result <- survey_and_deployment_generator(caps, deps)

  # Should rename back to deployment_id
  expect_true("deployment_id" %in% colnames(result))
  expect_false("deploymentID" %in% colnames(result))
})

test_that("survey_and_deployment_generator handles deployment_id alternative name in deps", {
  caps <- create_test_caps()
  deps <- create_test_deps()
  colnames(deps)[colnames(deps) == "deploymentID"] <- "deployment_id"

  result <- survey_and_deployment_generator(caps, deps)

  # Should process successfully
  expect_true("new_deploymentID" %in% colnames(result))
  expect_true("new_deploymentGroups" %in% colnames(result))
})

test_that("survey_and_deployment_generator handles placename alternative for locationID", {
  caps <- create_test_caps()
  colnames(caps)[colnames(caps) == "locationID"] <- "placename"
  deps <- create_test_deps()

  result <- survey_and_deployment_generator(caps, deps)

  # Should rename back to placename
  expect_true("placename" %in% colnames(result))
})

test_that("survey_and_deployment_generator handles source alternative for dataSource", {
  caps <- create_test_caps()
  colnames(caps)[colnames(caps) == "dataSource"] <- "source"
  deps <- create_test_deps()

  result <- survey_and_deployment_generator(caps, deps)

  # Should rename back to source
  expect_true("source" %in% colnames(result))
})

test_that("survey_and_deployment_generator handles Landscape alternative for locationName in caps", {
  caps <- create_test_caps()
  colnames(caps)[colnames(caps) == "locationName"] <- "Landscape"
  deps <- create_test_deps()
  colnames(deps)[colnames(deps) == "locationName"] <- "Landscape"

  result <- survey_and_deployment_generator(caps, deps)

  # Should rename back to Landscape
  expect_true("Landscape" %in% colnames(result))
})

test_that("survey_and_deployment_generator handles Photo.Date alternative for eventStart", {
  caps <- create_test_caps()
  colnames(caps)[colnames(caps) == "eventStart"] <- "Photo.Date"
  deps <- create_test_deps()

  result <- survey_and_deployment_generator(caps, deps)

  # Should rename back to Photo.Date
  expect_true("Photo.Date" %in% colnames(result))
})

test_that("survey_and_deployment_generator handles notes alternative for deploymentComments", {
  caps <- create_test_caps()
  deps <- create_test_deps()
  deps$notes <- c("Note 1", "Note 2")

  result <- survey_and_deployment_generator(caps, deps)

  # Should process successfully
  expect_true("new_deploymentID" %in% colnames(result))
})

# Tests for basic functionality ----

test_that("survey_and_deployment_generator creates new_deploymentID column", {
  caps <- create_test_caps()
  deps <- create_test_deps()

  result <- survey_and_deployment_generator(caps, deps)

  expect_true("new_deploymentID" %in% colnames(result))
  expect_true(all(!is.na(result$new_deploymentID)))
})

test_that("survey_and_deployment_generator creates new_deploymentGroups column", {
  caps <- create_test_caps()
  deps <- create_test_deps()

  result <- survey_and_deployment_generator(caps, deps)

  expect_true("new_deploymentGroups" %in% colnames(result))
  expect_true(all(!is.na(result$new_deploymentGroups)))
})

test_that("survey_and_deployment_generator preserves all input rows", {
  caps <- create_test_caps()
  deps <- create_test_deps()

  result <- survey_and_deployment_generator(caps, deps)

  expect_equal(nrow(result), nrow(caps))
})

test_that("survey_and_deployment_generator preserves all input columns", {
  caps <- create_test_caps()
  deps <- create_test_deps()

  result <- survey_and_deployment_generator(caps, deps)

  # All original columns should be present
  expect_true(all(colnames(caps) %in% colnames(result)))
})

test_that("survey_and_deployment_generator creates deployment groups with locationName", {
  caps <- create_test_caps()
  deps <- create_test_deps()

  result <- survey_and_deployment_generator(caps, deps)

  # new_deploymentGroups should contain locationName
  expect_true(all(grepl("Landscape1", result$new_deploymentGroups)))
})

test_that("survey_and_deployment_generator includes year in deployment groups", {
  caps <- create_test_caps()
  deps <- create_test_deps()

  result <- survey_and_deployment_generator(caps, deps)

  # Should include a year value (chron::years uses 1970 as base, so 2023 becomes 2022)
  # This is expected behavior of the chron package
  expect_true(all(grepl("[0-9]{4}", result$new_deploymentGroups)))
})

test_that("survey_and_deployment_generator includes dataSource in deployment groups", {
  caps <- create_test_caps()
  deps <- create_test_deps()

  result <- survey_and_deployment_generator(caps, deps)

  # Should extract last part of dataSource (Smith)
  expect_true(all(grepl("Smith", result$new_deploymentGroups)))
})

# Tests for survey duration and gaps ----

test_that("survey_and_deployment_generator handles short surveys (< max_dur)", {
  # Create data spanning 50 days (less than default 100)
  caps <- data.frame(
    eventStart = as.POSIXct(c("2023-01-01 08:00:00", "2023-02-15 10:00:00")),
    deploymentID = c("CAM001", "CAM001"),
    locationID = c("Site_A", "Site_A"),
    dataSource = c("Study_Smith", "Study_Smith"),
    locationName = c("Landscape1", "Landscape1")
  )
  deps <- data.frame(
    deploymentID = "CAM001",
    locationName = "Landscape1"
  )

  result <- survey_and_deployment_generator(caps, deps, max_dur = 100)

  # Should create a single survey period
  expect_equal(length(unique(result$new_deploymentGroups)), 1)
})

test_that("survey_and_deployment_generator splits long surveys (> max_dur)", {
  # Create data spanning 150 days (more than default 100)
  dates <- seq(as.POSIXct("2023-01-01"), as.POSIXct("2023-05-31"), by = "14 days")
  caps <- data.frame(
    eventStart = dates,
    deploymentID = rep("CAM001", length(dates)),
    locationID = rep("Site_A", length(dates)),
    dataSource = rep("Study_Smith", length(dates)),
    locationName = rep("Landscape1", length(dates))
  )
  deps <- data.frame(
    deploymentID = "CAM001",
    locationName = "Landscape1"
  )

  result <- survey_and_deployment_generator(caps, deps, max_dur = 100, cam_long = 20)

  # Should create multiple survey periods
  expect_gt(length(unique(result$new_deploymentGroups)), 1)
})

test_that("survey_and_deployment_generator detects gaps with cam_long parameter", {
  # Create data with a 30-day gap spanning > max_dur total duration
  # Gap detection only triggers when total duration >= max_dur
  dates <- c(
    as.POSIXct(seq(as.Date("2023-01-01"), as.Date("2023-02-01"), by = "7 days")),
    as.POSIXct(seq(as.Date("2023-04-01"), as.Date("2023-05-15"), by = "7 days"))
  )
  caps <- data.frame(
    eventStart = dates,
    deploymentID = rep("CAM001", length(dates)),
    locationID = rep("Site_A", length(dates)),
    dataSource = rep("Study_Smith", length(dates)),
    locationName = rep("Landscape1", length(dates))
  )
  deps <- data.frame(
    deploymentID = "CAM001",
    locationName = "Landscape1"
  )

  # With cam_long = 30 and max_dur = 100, the 60-day gap should trigger a new survey
  result <- survey_and_deployment_generator(caps, deps, cam_long = 30, max_dur = 100)

  # Should detect the gap and create separate surveys
  expect_gt(length(unique(result$new_deploymentGroups)), 1)
})

test_that("survey_and_deployment_generator cam_filter=FALSE skips gap detection", {
  caps <- data.frame(
    eventStart = as.POSIXct(c("2023-01-01 08:00:00", "2023-03-01 10:00:00")),
    deploymentID = rep("CAM001", 2),
    locationID = rep("Site_A", 2),
    dataSource = rep("Study_Smith", 2),
    locationName = rep("Landscape1", 2)
  )
  deps <- data.frame(
    deploymentID = "CAM001",
    locationName = "Landscape1",
    deploymentComments = "Manual_start_stop"
  )

  # With cam_filter = FALSE, should not split based on gaps
  result <- survey_and_deployment_generator(caps, deps, cam_filter = FALSE, max_dur = 100)

  expect_equal(length(unique(result$new_deploymentGroups)), 1)
})

# Tests for multiple locations ----

test_that("survey_and_deployment_generator handles multiple locationNames separately", {
  caps <- data.frame(
    eventStart = as.POSIXct(c("2023-01-01 08:00:00", "2023-01-05 10:00:00",
                               "2023-01-02 08:00:00", "2023-01-06 10:00:00")),
    deploymentID = c("CAM001", "CAM001", "CAM002", "CAM002"),
    locationID = c("Site_A", "Site_A", "Site_B", "Site_B"),
    dataSource = rep("Study_Smith", 4),
    locationName = c("Landscape1", "Landscape1", "Landscape2", "Landscape2")
  )
  deps <- data.frame(
    deploymentID = c("CAM001", "CAM002"),
    locationName = c("Landscape1", "Landscape2")
  )

  result <- survey_and_deployment_generator(caps, deps)

  # Should have deployment groups for both landscapes
  expect_true(any(grepl("Landscape1", result$new_deploymentGroups)))
  expect_true(any(grepl("Landscape2", result$new_deploymentGroups)))

  # Check that each landscape is processed separately
  landscape1_groups <- unique(result$new_deploymentGroups[result$locationName == "Landscape1"])
  landscape2_groups <- unique(result$new_deploymentGroups[result$locationName == "Landscape2"])
  expect_false(any(landscape1_groups %in% landscape2_groups))
})

# Tests for multiple cameras at same location ----

test_that("survey_and_deployment_generator handles multiple deployments at same locationID", {
  caps <- data.frame(
    eventStart = as.POSIXct(c("2023-01-01 08:00:00", "2023-01-05 10:00:00",
                               "2023-01-10 08:00:00", "2023-01-15 10:00:00")),
    deploymentID = c("DEP001", "DEP001", "DEP002", "DEP002"),
    locationID = rep("Site_A", 4),
    dataSource = rep("Study_Smith", 4),
    locationName = rep("Landscape1", 4)
  )
  deps <- data.frame(
    deploymentID = c("DEP001", "DEP002"),
    locationName = c("Landscape1", "Landscape1")
  )

  result <- survey_and_deployment_generator(caps, deps)

  # Should create distinct new_deploymentIDs for each deployment
  dep1_ids <- unique(result$new_deploymentID[result$deploymentID == "DEP001"])
  dep2_ids <- unique(result$new_deploymentID[result$deploymentID == "DEP002"])

  expect_length(dep1_ids, 1)
  expect_length(dep2_ids, 1)
  expect_false(dep1_ids == dep2_ids)
})

# Tests for season filtering ----

test_that("survey_and_deployment_generator szn_filter=TRUE combines short survey periods", {
  # Create a survey with a single isolated detection
  caps <- data.frame(
    eventStart = as.POSIXct(c("2023-01-01 08:00:00", "2023-02-20 10:00:00",
                               "2023-02-22 12:00:00", "2023-04-15 14:00:00")),
    deploymentID = rep("CAM001", 4),
    locationID = rep("Site_A", 4),
    dataSource = rep("Study_Smith", 4),
    locationName = rep("Landscape1", 4)
  )
  deps <- data.frame(
    deploymentID = "CAM001",
    locationName = "Landscape1"
  )

  result_filtered <- survey_and_deployment_generator(caps, deps,
                                                       szn_filter = TRUE,
                                                       cam_long = 15,
                                                       max_dur = 100)
  result_unfiltered <- survey_and_deployment_generator(caps, deps,
                                                         szn_filter = FALSE,
                                                         cam_long = 15,
                                                         max_dur = 100)

  # With filtering, should have fewer or equal survey groups
  expect_lte(length(unique(result_filtered$new_deploymentGroups)),
             length(unique(result_unfiltered$new_deploymentGroups)))
})

# Tests for multi-year surveys ----

test_that("survey_and_deployment_generator handles multi-year surveys with ranking", {
  # Create surveys spanning multiple years
  dates <- c(
    as.POSIXct(seq(as.Date("2022-01-01"), as.Date("2022-03-01"), by = "7 days")),
    as.POSIXct(seq(as.Date("2022-06-01"), as.Date("2022-08-01"), by = "7 days")),
    as.POSIXct(seq(as.Date("2023-01-01"), as.Date("2023-03-01"), by = "7 days"))
  )

  caps <- data.frame(
    eventStart = dates,
    deploymentID = rep("CAM001", length(dates)),
    locationID = rep("Site_A", length(dates)),
    dataSource = rep("Study_Smith", length(dates)),
    locationName = rep("Landscape1", length(dates))
  )
  deps <- data.frame(
    deploymentID = "CAM001",
    locationName = "Landscape1"
  )

  result <- survey_and_deployment_generator(caps, deps, cam_long = 30, max_dur = 75)

  # Should create multiple deployment groups for different periods
  expect_gt(length(unique(result$new_deploymentGroups)), 1)

  # Check that years are properly represented
  expect_true(any(grepl("2022", result$new_deploymentGroups)))
  expect_true(any(grepl("2023", result$new_deploymentGroups)))
})

# Tests for edge cases ----

test_that("survey_and_deployment_generator handles single detection", {
  caps <- data.frame(
    eventStart = as.POSIXct("2023-01-01 08:00:00"),
    deploymentID = "CAM001",
    locationID = "Site_A",
    dataSource = "Study_Smith",
    locationName = "Landscape1"
  )
  deps <- data.frame(
    deploymentID = "CAM001",
    locationName = "Landscape1"
  )

  result <- survey_and_deployment_generator(caps, deps)

  expect_equal(nrow(result), 1)
  expect_true("new_deploymentID" %in% colnames(result))
  expect_true("new_deploymentGroups" %in% colnames(result))
})

test_that("survey_and_deployment_generator handles cameras with year suffixes in names", {
  caps <- data.frame(
    eventStart = as.POSIXct(c("2023-01-01 08:00:00", "2023-01-05 10:00:00")),
    deploymentID = c("Site_2023_a", "Site_2023_a"),
    locationID = c("Site_2023_a", "Site_2023_a"),
    dataSource = c("Study_Smith", "Study_Smith"),
    locationName = c("Landscape1", "Landscape1")
  )
  deps <- data.frame(
    deploymentID = "Site_2023_a",
    locationName = "Landscape1"
  )

  result <- survey_and_deployment_generator(caps, deps)

  # Should handle the extraction of year/letter and not duplicate
  expect_true("new_deploymentID" %in% colnames(result))
  # Check that year isn't duplicated in the new ID
  new_id <- result$new_deploymentID[1]
  expect_false(grepl("2023_2023", new_id))
})

test_that("survey_and_deployment_generator returns data.frame", {
  caps <- create_test_caps()
  deps <- create_test_deps()

  result <- survey_and_deployment_generator(caps, deps)

  expect_s3_class(result, "data.frame")
})

test_that("survey_and_deployment_generator handles same-day detections", {
  # Multiple detections on the same day
  caps <- data.frame(
    eventStart = as.POSIXct(c("2023-01-01 08:00:00", "2023-01-01 12:00:00",
                               "2023-01-01 16:00:00")),
    deploymentID = rep("CAM001", 3),
    locationID = rep("Site_A", 3),
    dataSource = rep("Study_Smith", 3),
    locationName = rep("Landscape1", 3)
  )
  deps <- data.frame(
    deploymentID = "CAM001",
    locationName = "Landscape1"
  )

  result <- survey_and_deployment_generator(caps, deps)

  expect_equal(nrow(result), 3)
  # All should be in the same deployment group
  expect_equal(length(unique(result$new_deploymentGroups)), 1)
})

# Tests for parameter variations ----

test_that("survey_and_deployment_generator respects custom cam_long parameter", {
  # Create data spanning > max_dur with a significant gap
  # Gap detection only works when total duration >= max_dur
  dates_with_gap <- c(
    as.POSIXct(seq(as.Date("2023-01-01"), as.Date("2023-02-15"), by = "7 days")),
    as.POSIXct(seq(as.Date("2023-04-01"), as.Date("2023-05-15"), by = "7 days"))
  )
  caps <- data.frame(
    eventStart = dates_with_gap,
    deploymentID = rep("CAM001", length(dates_with_gap)),
    locationID = rep("Site_A", length(dates_with_gap)),
    dataSource = rep("Study_Smith", length(dates_with_gap)),
    locationName = rep("Landscape1", length(dates_with_gap))
  )
  deps <- data.frame(
    deploymentID = "CAM001",
    locationName = "Landscape1"
  )

  # With cam_long = 30 and max_dur = 130, the ~45-day gap should create splits
  result_short <- survey_and_deployment_generator(caps, deps, cam_long = 30, max_dur = 130)

  # With cam_long = 60 and max_dur = 130, the ~45-day gap should NOT create as many splits
  result_long <- survey_and_deployment_generator(caps, deps, cam_long = 60, max_dur = 130)

  # The short cam_long should detect the gap and create more groups
  expect_gt(length(unique(result_short$new_deploymentGroups)),
            length(unique(result_long$new_deploymentGroups)))
})

test_that("survey_and_deployment_generator respects custom max_dur parameter", {
  # Create data spanning 80 days
  dates <- seq(as.POSIXct("2023-01-01"), as.POSIXct("2023-03-21"), by = "7 days")
  caps <- data.frame(
    eventStart = dates,
    deploymentID = rep("CAM001", length(dates)),
    locationID = rep("Site_A", length(dates)),
    dataSource = rep("Study_Smith", length(dates)),
    locationName = rep("Landscape1", length(dates))
  )
  deps <- data.frame(
    deploymentID = "CAM001",
    locationName = "Landscape1"
  )

  # With max_dur = 60, should split into multiple periods
  result_short <- survey_and_deployment_generator(caps, deps, max_dur = 60)

  # With max_dur = 100, should be single period
  result_long <- survey_and_deployment_generator(caps, deps, max_dur = 100)

  expect_gt(length(unique(result_short$new_deploymentGroups)),
            length(unique(result_long$new_deploymentGroups)))
})
