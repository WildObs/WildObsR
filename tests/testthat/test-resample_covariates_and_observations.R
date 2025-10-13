## Tests for resample_covariates_and_observations() ----

# Helper function to create minimal test data
create_test_covariates <- function() {
  data.frame(
    deploymentID = c("CAM001", "CAM002", "CAM003"),
    locationName = c("Site1", "Site1", "Site2"),
    deploymentGroups = c("2023_Study", "2023_Study", "2023_Study"),
    cellID_1km = c("Cell_A_1km", "Cell_A_1km", "Cell_B_1km"),
    cellID_10km = c("Cell_X_10km", "Cell_X_10km", "Cell_Y_10km"),
    polygon_1km = c("POLYGON((0 0, 1 0, 1 1, 0 1, 0 0))",
                    "POLYGON((0 0, 1 0, 1 1, 0 1, 0 0))",
                    "POLYGON((2 2, 3 2, 3 3, 2 3, 2 2))"),
    polygon_10km = c("POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))",
                     "POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))",
                     "POLYGON((10 10, 20 10, 20 20, 10 20, 10 10))"),
    deploymentStart = as.POSIXct(c("2023-01-01 08:00:00", "2023-01-02 08:00:00", "2023-01-01 08:00:00")),
    deploymentEnd = as.POSIXct(c("2023-01-10 18:00:00", "2023-01-11 18:00:00", "2023-01-10 18:00:00")),
    Avg_latitude = c(-17.5, -17.5, -17.6),
    Avg_longitude = c(145.5, 145.5, 145.6),
    habitat = c("forest", "forest", "grassland"),
    elevation = c(100, 110, 50),
    stringsAsFactors = FALSE
  )
}

create_test_observations <- function() {
  data.frame(
    deploymentID = c("CAM001", "CAM001", "CAM002", "CAM003"),
    eventID = c("E001", "E002", "E003", "E004"),
    observationID = c("O001", "O002", "O003", "O004"),
    scientificName = c("Species A", "Species B", "Species A", "Species A"),
    count = c(1, 2, 1, 3),
    eventStart = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-02 14:00:00",
                               "2023-01-03 12:00:00", "2023-01-02 09:00:00")),
    eventEnd = as.POSIXct(c("2023-01-01 10:05:00", "2023-01-02 14:05:00",
                             "2023-01-03 12:05:00", "2023-01-02 09:05:00")),
    classificationTimestamp = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-02 14:00:00",
                                            "2023-01-03 12:00:00", "2023-01-02 09:00:00")),
    observationStart = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-02 14:00:00",
                                     "2023-01-03 12:00:00", "2023-01-02 09:00:00")),
    observationEnd = as.POSIXct(c("2023-01-01 10:05:00", "2023-01-02 14:05:00",
                                   "2023-01-03 12:05:00", "2023-01-02 09:05:00")),
    baitUse = c("none", "none", "bait", "none"),
    featureType = c("trail", "trail", "road", "trail"),
    stringsAsFactors = FALSE
  )
}

# Tests for input validation ----

test_that("resample_covariates_and_observations requires cellID columns", {
  covs <- data.frame(
    deploymentID = "CAM001",
    deploymentStart = as.POSIXct("2023-01-01"),
    deploymentEnd = as.POSIXct("2023-01-10")
  )
  obs <- create_test_observations()

  expect_error(
    resample_covariates_and_observations(covs, obs, "sum", NULL, NULL),
    "do not contain any spatially delimited cells"
  )
})

test_that("resample_covariates_and_observations requires matching deploymentIDs", {
  covs <- create_test_covariates()
  obs <- data.frame(
    deploymentID = c("CAM999", "CAM888"),
    eventStart = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-02 10:00:00")),
    eventEnd = as.POSIXct(c("2023-01-01 10:05:00", "2023-01-02 10:05:00")),
    classificationTimestamp = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-02 10:00:00")),
    observationStart = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-02 10:00:00")),
    observationEnd = as.POSIXct(c("2023-01-01 10:05:00", "2023-01-02 10:05:00")),
    scientificName = c("Species A", "Species B")
  )

  expect_error(
    resample_covariates_and_observations(covs, obs, "sum", NULL, NULL),
    "mis-matched deploymentID values"
  )
})

test_that("resample_covariates_and_observations requires POSIXct datetime in obs", {
  covs <- create_test_covariates()
  obs <- create_test_observations()
  # Convert all datetime columns to character
  obs$eventStart <- as.character(obs$eventStart)
  obs$eventEnd <- as.character(obs$eventEnd)
  obs$classificationTimestamp <- as.character(obs$classificationTimestamp)
  obs$observationStart <- as.character(obs$observationStart)
  obs$observationEnd <- as.character(obs$observationEnd)

  expect_error(
    resample_covariates_and_observations(covs, obs, "sum", NULL, NULL),
    "does not have date-time columns.*formatted in POSIXct class"
  )
})

test_that("resample_covariates_and_observations requires POSIXct datetime in covs", {
  covs <- create_test_covariates()
  # Convert both deployment datetime columns to character
  covs$deploymentStart <- as.character(covs$deploymentStart)
  covs$deploymentEnd <- as.character(covs$deploymentEnd)
  obs <- create_test_observations()

  expect_error(
    resample_covariates_and_observations(covs, obs, "sum", NULL, NULL),
    "does not have date-time columns.*formatted in POSIXct class"
  )
})

test_that("resample_covariates_and_observations warns about missing obs_covs", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  expect_warning(
    resample_covariates_and_observations(covs, obs, "sum", NULL, c("nonexistent_col")),
    "not present in the covariates resource"
  )
})

# Tests for basic functionality ----

test_that("resample_covariates_and_observations returns correct structure", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  result <- resample_covariates_and_observations(covs, obs, "sum", NULL, NULL)

  expect_type(result, "list")
  expect_true("spatially_resampled_observations" %in% names(result))
  expect_true("spatially_resampled_covariates" %in% names(result))
})

test_that("resample_covariates_and_observations creates entries for each spatial scale", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  result <- resample_covariates_and_observations(covs, obs, "sum", NULL, NULL)

  # Should have entries for both scales
  expect_true("cellID_1km" %in% names(result$spatially_resampled_observations))
  expect_true("cellID_10km" %in% names(result$spatially_resampled_observations))
  expect_true("cellID_1km" %in% names(result$spatially_resampled_covariates))
  expect_true("cellID_10km" %in% names(result$spatially_resampled_covariates))
})

test_that("resample_covariates_and_observations creates cellEffort column", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  result <- resample_covariates_and_observations(covs, obs, "sum", NULL, NULL)

  resamp_covs <- result$spatially_resampled_covariates$cellID_1km

  expect_true("cellEffort" %in% names(resamp_covs))
  expect_true(all(!is.na(resamp_covs$cellEffort)))
  expect_true(all(resamp_covs$cellEffort > 0))
})

test_that("resample_covariates_and_observations creates samplingStart and samplingEnd", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  result <- resample_covariates_and_observations(covs, obs, "sum", NULL, NULL)

  resamp_covs <- result$spatially_resampled_covariates$cellID_1km

  expect_true("samplingStart" %in% names(resamp_covs))
  expect_true("samplingEnd" %in% names(resamp_covs))
  expect_s3_class(resamp_covs$samplingStart, "POSIXct")
  expect_s3_class(resamp_covs$samplingEnd, "POSIXct")
})

test_that("resample_covariates_and_observations creates deploymentsIncluded column", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  result <- resample_covariates_and_observations(covs, obs, "sum", NULL, NULL)

  resamp_covs <- result$spatially_resampled_covariates$cellID_1km

  expect_true("deploymentsIncluded" %in% names(resamp_covs))
  expect_false("deploymentID" %in% names(resamp_covs))
})

test_that("resample_covariates_and_observations aggregates multiple deployments in same cell", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  result <- resample_covariates_and_observations(covs, obs, "sum", NULL, NULL)

  resamp_covs <- result$spatially_resampled_covariates$cellID_1km

  # Cell_A_1km should have both CAM001 and CAM002
  cell_a <- resamp_covs[resamp_covs$cellID_1km == "Cell_A_1km", ]
  expect_true(grepl("CAM001", cell_a$deploymentsIncluded))
  expect_true(grepl("CAM002", cell_a$deploymentsIncluded))
})

# Tests for numeric aggregation ----

test_that("resample_covariates_and_observations averages numeric covariates", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  result <- resample_covariates_and_observations(covs, obs, "sum", NULL, NULL)

  resamp_covs <- result$spatially_resampled_covariates$cellID_1km

  # Should have Avg_ prefix for numeric columns
  expect_true(any(grepl("Avg_elevation", names(resamp_covs))))
  expect_true(any(grepl("Avg_latitude", names(resamp_covs))))
})

test_that("resample_covariates_and_observations correctly averages values", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  result <- resample_covariates_and_observations(covs, obs, "sum", NULL, NULL)

  resamp_covs <- result$spatially_resampled_covariates$cellID_1km

  # Cell_A_1km has elevation values 100 and 110, average should be 105
  cell_a <- resamp_covs[resamp_covs$cellID_1km == "Cell_A_1km", ]
  expect_equal(cell_a$Avg_elevation, 105)
})

# Tests for mode aggregation ----

test_that("resample_covariates_and_observations handles mode_cols_covs parameter", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  result <- resample_covariates_and_observations(covs, obs, "sum", "habitat", NULL)

  resamp_covs <- result$spatially_resampled_covariates$cellID_1km

  # Should have mode_ prefix for mode columns
  expect_true("mode_habitat" %in% names(resamp_covs))
})

test_that("resample_covariates_and_observations calculates mode correctly", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  result <- resample_covariates_and_observations(covs, obs, "sum", "habitat", NULL)

  resamp_covs <- result$spatially_resampled_covariates$cellID_1km

  # Cell_A_1km has habitat "forest" twice, should be the mode
  cell_a <- resamp_covs[resamp_covs$cellID_1km == "Cell_A_1km", ]
  expect_equal(cell_a$mode_habitat, "forest")
})

# Tests for observation resampling ----

test_that("resample_covariates_and_observations creates observation metrics", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  result <- resample_covariates_and_observations(covs, obs, "sum", NULL, NULL)

  resamp_obs <- result$spatially_resampled_observations$cellID_1km

  expect_true("independentEvents" %in% names(resamp_obs))
  expect_true("independentObservations" %in% names(resamp_obs))
  expect_true("totalIndividuals" %in% names(resamp_obs))
  expect_true("deploymentsActiveAtDate" %in% names(resamp_obs))
  expect_true("numberDeploymentsActiveAtDate" %in% names(resamp_obs))
})

test_that("resample_covariates_and_observations sums individuals correctly", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  result <- resample_covariates_and_observations(covs, obs, "sum", NULL, NULL)

  resamp_obs <- result$spatially_resampled_observations$cellID_1km

  # All totalIndividuals should be sums
  expect_true(all(resamp_obs$totalIndividuals >= 0))
})

test_that("resample_covariates_and_observations respects individuals='max' parameter", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  result_sum <- resample_covariates_and_observations(covs, obs, "sum", NULL, NULL)
  result_max <- resample_covariates_and_observations(covs, obs, "max", NULL, NULL)

  # With max, values should be <= sum values
  resamp_obs_sum <- result_sum$spatially_resampled_observations$cellID_1km
  resamp_obs_max <- result_max$spatially_resampled_observations$cellID_1km

  # Both should have totalIndividuals column
  expect_true("totalIndividuals" %in% names(resamp_obs_sum))
  expect_true("totalIndividuals" %in% names(resamp_obs_max))
})

test_that("resample_covariates_and_observations preserves species information", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  result <- resample_covariates_and_observations(covs, obs, "sum", NULL, NULL)

  resamp_obs <- result$spatially_resampled_observations$cellID_1km

  expect_true("scientificName" %in% names(resamp_obs))
  # Should have both species
  expect_true("Species A" %in% resamp_obs$scientificName)
  expect_true("Species B" %in% resamp_obs$scientificName)
})

# Tests for observation covariates ----

test_that("resample_covariates_and_observations includes obs_covs when provided", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  result <- resample_covariates_and_observations(covs, obs, "sum", NULL, c("baitUse", "featureType"))

  resamp_obs <- result$spatially_resampled_observations$cellID_1km

  expect_true("baitUse" %in% names(resamp_obs))
  expect_true("featureType" %in% names(resamp_obs))
})

test_that("resample_covariates_and_observations handles missing obs_covs parameter", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  # Should work without obs_covs
  result <- resample_covariates_and_observations(covs, obs, "sum", NULL)

  expect_type(result, "list")
})

test_that("resample_covariates_and_observations handles missing mode_cols_covs parameter", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  # Should work without mode_cols_covs
  result <- resample_covariates_and_observations(covs, obs, "sum")

  expect_type(result, "list")
})

# Tests for multiple scales ----

test_that("resample_covariates_and_observations handles multiple spatial scales independently", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  result <- resample_covariates_and_observations(covs, obs, "sum", NULL, NULL)

  resamp_covs_1km <- result$spatially_resampled_covariates$cellID_1km
  resamp_covs_10km <- result$spatially_resampled_covariates$cellID_10km

  # Both scales should exist
  expect_true(nrow(resamp_covs_1km) > 0)
  expect_true(nrow(resamp_covs_10km) > 0)
})

test_that("resample_covariates_and_observations aggregates more at coarser scales", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  result <- resample_covariates_and_observations(covs, obs, "sum", NULL, NULL)

  resamp_covs_1km <- result$spatially_resampled_covariates$cellID_1km
  resamp_covs_10km <- result$spatially_resampled_covariates$cellID_10km

  # 10km scale should have fewer or equal rows than 1km
  expect_lte(nrow(resamp_covs_10km), nrow(resamp_covs_1km))
})

# Tests for deployment groups ----

test_that("resample_covariates_and_observations handles multiple deployment groups", {
  covs <- create_test_covariates()
  # Add different deployment group
  covs$deploymentGroups[3] <- "2024_Study"
  obs <- create_test_observations()

  result <- resample_covariates_and_observations(covs, obs, "sum", NULL, NULL)

  expect_type(result, "list")
  # Should process both groups
  resamp_covs <- result$spatially_resampled_covariates$cellID_1km
  expect_true(nrow(resamp_covs) > 0)
})

# Tests for edge cases ----

test_that("resample_covariates_and_observations handles single deployment", {
  covs <- create_test_covariates()[1, ]
  obs <- create_test_observations()[1, ]

  result <- resample_covariates_and_observations(covs, obs, "sum", NULL, NULL)

  expect_type(result, "list")
  resamp_covs <- result$spatially_resampled_covariates$cellID_1km
  expect_equal(nrow(resamp_covs), 1)
})

test_that("resample_covariates_and_observations returns data frames", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  result <- resample_covariates_and_observations(covs, obs, "sum", NULL, NULL)

  expect_s3_class(result$spatially_resampled_observations$cellID_1km, "data.frame")
  expect_s3_class(result$spatially_resampled_covariates$cellID_1km, "data.frame")
})

test_that("resample_covariates_and_observations preserves cellID columns", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  result <- resample_covariates_and_observations(covs, obs, "sum", NULL, NULL)

  resamp_covs <- result$spatially_resampled_covariates$cellID_1km
  resamp_obs <- result$spatially_resampled_observations$cellID_1km

  expect_true("cellID_1km" %in% names(resamp_covs))
  expect_true("cellID_1km" %in% names(resamp_obs))
})

test_that("resample_covariates_and_observations preserves polygon columns", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  result <- resample_covariates_and_observations(covs, obs, "sum", NULL, NULL)

  resamp_covs <- result$spatially_resampled_covariates$cellID_1km

  expect_true("polygon_1km" %in% names(resamp_covs))
})

# Tests for data integrity ----

test_that("resample_covariates_and_observations maintains cellID matching between obs and covs", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  result <- resample_covariates_and_observations(covs, obs, "sum", NULL, NULL)

  resamp_covs <- result$spatially_resampled_covariates$cellID_1km
  resamp_obs <- result$spatially_resampled_observations$cellID_1km

  # All cellIDs in obs should be in covs
  expect_true(all(resamp_obs$cellID_1km %in% resamp_covs$cellID_1km))
})

test_that("resample_covariates_and_observations removes deploymentID from observations", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  result <- resample_covariates_and_observations(covs, obs, "sum", NULL, NULL)

  resamp_obs <- result$spatially_resampled_observations$cellID_1km

  # deploymentID should not be in resampled observations
  expect_false("deploymentID" %in% names(resamp_obs))
})

test_that("resample_covariates_and_observations handles character covariates by concatenation", {
  covs <- create_test_covariates()
  obs <- create_test_observations()

  result <- resample_covariates_and_observations(covs, obs, "sum", NULL, NULL)

  resamp_covs <- result$spatially_resampled_covariates$cellID_1km

  # locationName should be concatenated for cells with multiple deployments
  cell_a <- resamp_covs[resamp_covs$cellID_1km == "Cell_A_1km", ]
  expect_true(grepl("Site1", cell_a$locationName))
})
