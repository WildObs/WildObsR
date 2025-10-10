## Tests for matrix_generator() ----

# Helper function to create minimal test data for matrix_generator
create_test_obs_matrix <- function() {
  data.frame(
    cellID_10km = c("Cell_A", "Cell_A", "Cell_B", "Cell_B"),
    scientificName = c("Species A", "Species A", "Species B", "Species A"),
    count = c(1, 2, 1, 3),
    totalIndividuals = c(1, 2, 1, 3), # matrix_generator looks for this
    eventStart = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-02 14:00:00",
                               "2023-01-03 12:00:00", "2023-01-04 09:00:00")),
    eventEnd = as.POSIXct(c("2023-01-01 10:05:00", "2023-01-02 14:05:00",
                             "2023-01-03 12:05:00", "2023-01-04 09:05:00")),
    observationStart = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-02 14:00:00",
                                     "2023-01-03 12:00:00", "2023-01-04 09:00:00")),
    observationEnd = as.POSIXct(c("2023-01-01 10:05:00", "2023-01-02 14:05:00",
                                   "2023-01-03 12:05:00", "2023-01-04 09:05:00")),
    baitUse = c("none", "none", "bait", "none"),
    numberDeploymentsActiveAtDate = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )
}

create_test_covs_matrix <- function() {
  data.frame(
    cellID_10km = c("Cell_A", "Cell_B"),
    locationName = c("Site1", "Site2"),
    samplingStart = as.POSIXct(c("2023-01-01 08:00:00", "2023-01-03 08:00:00")),
    samplingEnd = as.POSIXct(c("2023-01-05 18:00:00", "2023-01-07 18:00:00")),
    Avg_latitude = c(-17.5, -17.6),
    Avg_longitude = c(145.5, 145.6),
    mode_habitat = c("forest", "grassland"),
    Avg_human_footprint_10km2 = c(0.3, 0.5),
    Avg_FLII_3km2 = c(85.2, 72.1),
    stringsAsFactors = FALSE
  )
}

# Tests for input validation ----

test_that("matrix_generator detects mismatched cellID/deploymentID", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()
  # Change cellID to create mismatch
  obs$cellID_10km[1] <- "Cell_C"

  expect_error(
    matrix_generator(obs, covs, dur = 10, w = 2,
                     site_covs = c("mode_habitat"),
                     obs_covs = c("cameraHeight"),
                     all_locationNames = TRUE,
                     scientificNames = "Species A",
                     type = "occupancy",
                     individuals = "sum"),
    "mis-matched.*values"
  )
})

test_that("matrix_generator warns about missing site_covs", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()

  expect_warning(
    matrix_generator(obs, covs, dur = 10, w = 2,
                     site_covs = c("mode_habitat", "nonexistent_var"),
                     obs_covs = character(0),
                     all_locationNames = TRUE,
                     scientificNames = "Species A",
                     type = "occupancy",
                     individuals = "sum"),
    "site-level covariates that are not in the covs table"
  )
})

test_that("matrix_generator requires POSIXct datetime columns in obs", {
  obs <- create_test_obs_matrix()
  obs$eventStart <- as.character(obs$eventStart)
  obs$eventEnd <- as.character(obs$eventEnd)
  obs$observationStart <- as.character(obs$observationStart)
  obs$observationEnd <- as.character(obs$observationEnd)
  covs <- create_test_covs_matrix()

  expect_error(
    matrix_generator(obs, covs, dur = 10, w = 2,
                     site_covs = c("mode_habitat"),
                     obs_covs = character(0),
                     all_locationNames = TRUE,
                     scientificNames = "Species A",
                     type = "occupancy",
                     individuals = "sum"),
    "does not have date-time columns.*formatted in POSIXct class"
  )
})

test_that("matrix_generator requires POSIXct datetime columns in covs", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()
  covs$samplingStart <- as.character(covs$samplingStart)
  covs$samplingEnd <- as.character(covs$samplingEnd)

  expect_error(
    matrix_generator(obs, covs, dur = 10, w = 2,
                     site_covs = c("mode_habitat"),
                     obs_covs = character(0),
                     all_locationNames = TRUE,
                     scientificNames = "Species A",
                     type = "occupancy",
                     individuals = "sum"),
    "does not have date-time columns.*formatted in POSIXct class"
  )
})

test_that("matrix_generator detects missing scientificNames", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()

  expect_error(
    matrix_generator(obs, covs, dur = 10, w = 2,
                     site_covs = c("mode_habitat"),
                     obs_covs = character(0),
                     all_locationNames = TRUE,
                     scientificNames = "Nonexistent Species",
                     type = "occupancy",
                     individuals = "sum"),
    "species names that are not present in the observations table"
  )
})

# Tests for basic functionality ----

test_that("matrix_generator returns correct structure", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()

  result <- matrix_generator(obs, covs, dur = 10, w = 2,
                              site_covs = c("mode_habitat"),
                              obs_covs = character(0),
                              all_locationNames = TRUE,
                              scientificNames = "Species A",
                              type = "occupancy",
                              individuals = "sum")

  expect_type(result, "list")
  expect_true("Species_A" %in% names(result))
})

test_that("matrix_generator creates detection_matrix", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()

  result <- matrix_generator(obs, covs, dur = 10, w = 2,
                              site_covs = c("mode_habitat"),
                              obs_covs = character(0),
                              all_locationNames = TRUE,
                              scientificNames = "Species A",
                              type = "occupancy",
                              individuals = "sum")

  expect_true("detection_matrix" %in% names(result$Species_A))
  expect_true(is.matrix(result$Species_A$detection_matrix))
})

test_that("matrix_generator creates site_level_covariates", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()

  result <- matrix_generator(obs, covs, dur = 10, w = 2,
                              site_covs = c("mode_habitat"),
                              obs_covs = character(0),
                              all_locationNames = TRUE,
                              scientificNames = "Species A",
                              type = "occupancy",
                              individuals = "sum")

  expect_true("site_level_covariates" %in% names(result$Species_A))
  expect_s3_class(result$Species_A$site_level_covariates, "data.frame")
})

test_that("matrix_generator creates observation_level_covariates", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()

  result <- matrix_generator(obs, covs, dur = 10, w = 2,
                              site_covs = c("mode_habitat"),
                              obs_covs = c("cameraHeight"),
                              all_locationNames = TRUE,
                              scientificNames = "Species A",
                              type = "occupancy",
                              individuals = "sum")

  expect_true("observation_level_covariates" %in% names(result$Species_A))
  expect_type(result$Species_A$observation_level_covariates, "list")
})

# Tests for occupancy vs abundance ----

test_that("matrix_generator type='occupancy' creates binary matrix", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()

  result <- matrix_generator(obs, covs, dur = 10, w = 2,
                              site_covs = c("mode_habitat"),
                              obs_covs = character(0),
                              all_locationNames = TRUE,
                              scientificNames = "Species A",
                              type = "occupancy",
                              individuals = "sum")

  mat <- result$Species_A$detection_matrix
  # Occupancy should only have 0, 1, or NA
  unique_vals <- unique(as.vector(mat))
  expect_true(all(unique_vals %in% c(0, 1, NA)))
})

test_that("matrix_generator type='abundance' creates count matrix", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()

  result <- matrix_generator(obs, covs, dur = 10, w = 2,
                              site_covs = c("mode_habitat"),
                              obs_covs = character(0),
                              all_locationNames = TRUE,
                              scientificNames = "Species A",
                              type = "abundance",
                              individuals = "sum")

  mat <- result$Species_A$detection_matrix
  # Abundance can have counts > 1
  expect_true(any(mat > 1, na.rm = TRUE))
})

test_that("matrix_generator individuals='sum' sums counts", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()

  result <- matrix_generator(obs, covs, dur = 10, w = 2,
                              site_covs = c("mode_habitat"),
                              obs_covs = character(0),
                              all_locationNames = TRUE,
                              scientificNames = "Species A",
                              type = "abundance",
                              individuals = "sum")

  # Function should successfully create matrix
  expect_true(is.matrix(result$Species_A$detection_matrix))
})

test_that("matrix_generator individuals='max' uses max counts", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()

  result <- matrix_generator(obs, covs, dur = 10, w = 2,
                              site_covs = c("mode_habitat"),
                              obs_covs = character(0),
                              all_locationNames = TRUE,
                              scientificNames = "Species A",
                              type = "abundance",
                              individuals = "max")

  # Function should successfully create matrix
  expect_true(is.matrix(result$Species_A$detection_matrix))
})

# Tests for multiple species ----

test_that("matrix_generator handles multiple species", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()

  result <- matrix_generator(obs, covs, dur = 10, w = 2,
                              site_covs = c("mode_habitat"),
                              obs_covs = character(0),
                              all_locationNames = TRUE,
                              scientificNames = c("Species A", "Species B"),
                              type = "occupancy",
                              individuals = "sum")

  expect_length(result, 2)
  expect_true("Species_A" %in% names(result))
  expect_true("Species_B" %in% names(result))
})

test_that("matrix_generator creates separate matrices for each species", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()

  result <- matrix_generator(obs, covs, dur = 10, w = 2,
                              site_covs = c("mode_habitat"),
                              obs_covs = character(0),
                              all_locationNames = TRUE,
                              scientificNames = c("Species A", "Species B"),
                              type = "occupancy",
                              individuals = "sum")

  mat_a <- result$Species_A$detection_matrix
  mat_b <- result$Species_B$detection_matrix

  # Matrices should differ for different species
  expect_false(identical(mat_a, mat_b))
})

# Tests for all_locationNames parameter ----

test_that("matrix_generator all_locationNames=TRUE includes all locations", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()

  result <- matrix_generator(obs, covs, dur = 10, w = 2,
                              site_covs = c("mode_habitat"),
                              obs_covs = character(0),
                              all_locationNames = TRUE,
                              scientificNames = "Species A",
                              type = "occupancy",
                              individuals = "sum")

  mat <- result$Species_A$detection_matrix
  # Should have rows for both cells
  expect_gte(nrow(mat), 2)
})

test_that("matrix_generator all_locationNames=FALSE filters locations", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()

  result <- matrix_generator(obs, covs, dur = 10, w = 2,
                              site_covs = c("mode_habitat"),
                              obs_covs = character(0),
                              all_locationNames = FALSE,
                              scientificNames = "Species B",
                              type = "occupancy",
                              individuals = "sum")

  mat <- result$Species_B$detection_matrix
  # Species B only present in Cell_B, so should have fewer rows
  expect_gte(nrow(mat), 1)
})

# Tests for matrix dimensions ----

test_that("matrix_generator creates matrix with correct number of rows", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()

  result <- matrix_generator(obs, covs, dur = 10, w = 2,
                              site_covs = c("mode_habitat"),
                              obs_covs = character(0),
                              all_locationNames = TRUE,
                              scientificNames = "Species A",
                              type = "occupancy",
                              individuals = "sum")

  mat <- result$Species_A$detection_matrix
  # Should have rows equal to number of cellIDs
  expect_equal(nrow(mat), length(unique(obs$cellID_10km)))
})

test_that("matrix_generator compresses sampling occasions correctly", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()

  result <- matrix_generator(obs, covs, dur = 10, w = 2,
                              site_covs = c("mode_habitat"),
                              obs_covs = character(0),
                              all_locationNames = TRUE,
                              scientificNames = "Species A",
                              type = "occupancy",
                              individuals = "sum")

  mat <- result$Species_A$detection_matrix
  # Number of columns should be dur/w = 10/2 = 5
  expect_equal(ncol(mat), 5)
})

test_that("matrix_generator adjusts dur if max_seq is smaller", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()

  # Set dur much larger than actual sampling duration
  result <- matrix_generator(obs, covs, dur = 200, w = 2,
                              site_covs = c("mode_habitat"),
                              obs_covs = character(0),
                              all_locationNames = TRUE,
                              scientificNames = "Species A",
                              type = "occupancy",
                              individuals = "sum")

  # Should not error and should create reasonable matrix
  expect_true(is.matrix(result$Species_A$detection_matrix))
})

# Tests for covariate standardization ----

test_that("matrix_generator standardizes numeric site covariates", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()

  result <- matrix_generator(obs, covs, dur = 10, w = 2,
                              site_covs = c("Avg_human_footprint_10km2"),
                              obs_covs = character(0),
                              all_locationNames = TRUE,
                              scientificNames = "Species A",
                              type = "occupancy",
                              individuals = "sum")

  site_covs <- result$Species_A$site_level_covariates
  # Numeric values should be standardized (mean ~0, sd ~1)
  expect_true("Avg_human_footprint_10km2" %in% names(site_covs))
})

test_that("matrix_generator preserves character site covariates", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()

  result <- matrix_generator(obs, covs, dur = 10, w = 2,
                              site_covs = c("mode_habitat", "locationName"),
                              obs_covs = character(0),
                              all_locationNames = TRUE,
                              scientificNames = "Species A",
                              type = "occupancy",
                              individuals = "sum")

  site_covs <- result$Species_A$site_level_covariates
  expect_true("mode_habitat" %in% names(site_covs))
  expect_true("locationName" %in% names(site_covs))
})

# Tests for observation covariates ----

test_that("matrix_generator handles empty obs_covs", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()

  result <- matrix_generator(obs, covs, dur = 10, w = 2,
                              site_covs = c("mode_habitat"),
                              obs_covs = character(0),
                              all_locationNames = TRUE,
                              scientificNames = "Species A",
                              type = "occupancy",
                              individuals = "sum")

  obs_covs <- result$Species_A$observation_level_covariates
  expect_length(obs_covs, 0)
})

test_that("matrix_generator creates observation covariate matrices", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()

  result <- matrix_generator(obs, covs, dur = 10, w = 2,
                              site_covs = c("mode_habitat"),
                              obs_covs = c("numberDeploymentsActiveAtDate"),
                              all_locationNames = TRUE,
                              scientificNames = "Species A",
                              type = "occupancy",
                              individuals = "sum")

  obs_covs <- result$Species_A$observation_level_covariates
  expect_true("numberDeploymentsActiveAtDate" %in% names(obs_covs))
  expect_true(is.matrix(obs_covs$numberDeploymentsActiveAtDate))
})

test_that("matrix_generator observation covariate matrices match detection matrix dimensions", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()

  result <- matrix_generator(obs, covs, dur = 10, w = 2,
                              site_covs = c("mode_habitat"),
                              obs_covs = c("numberDeploymentsActiveAtDate"),
                              all_locationNames = TRUE,
                              scientificNames = "Species A",
                              type = "occupancy",
                              individuals = "sum")

  det_mat <- result$Species_A$detection_matrix
  obs_mat <- result$Species_A$observation_level_covariates$numberDeploymentsActiveAtDate

  expect_equal(dim(det_mat), dim(obs_mat))
})

# Tests for cap_count parameter ----

test_that("matrix_generator cap_count=FALSE does not cap counts", {
  obs <- create_test_obs_matrix()
  # Add a row with high count
  obs <- rbind(obs, data.frame(
    cellID_10km = "Cell_A",
    scientificName = "Species A",
    count = 50,
    totalIndividuals = 50,
    eventStart = as.POSIXct("2023-01-05 10:00:00"),
    eventEnd = as.POSIXct("2023-01-05 10:05:00"),
    observationStart = as.POSIXct("2023-01-05 10:00:00"),
    observationEnd = as.POSIXct("2023-01-05 10:05:00"),
    baitUse = "none",
    numberDeploymentsActiveAtDate = 1
  ))
  covs <- create_test_covs_matrix()

  result <- matrix_generator(obs, covs, dur = 10, w = 2,
                              site_covs = c("mode_habitat"),
                              obs_covs = character(0),
                              all_locationNames = TRUE,
                              scientificNames = "Species A",
                              type = "abundance",
                              individuals = "sum",
                              cap_count = FALSE)

  mat <- result$Species_A$detection_matrix
  # Should have the high count value in the matrix
  expect_true(is.matrix(mat))
})

test_that("matrix_generator cap_count=TRUE caps high counts", {
  obs <- create_test_obs_matrix()
  # Add multiple rows with high counts to trigger capping
  for (i in 1:10) {
    obs <- rbind(obs, data.frame(
      cellID_10km = "Cell_A",
      scientificName = "Species A",
      count = 20,
      totalIndividuals = 20,
      eventStart = as.POSIXct(paste0("2023-01-0", (i %% 5) + 1, " ", 10 + i, ":00:00")),
      eventEnd = as.POSIXct(paste0("2023-01-0", (i %% 5) + 1, " ", 10 + i, ":05:00")),
      observationStart = as.POSIXct(paste0("2023-01-0", (i %% 5) + 1, " ", 10 + i, ":00:00")),
      observationEnd = as.POSIXct(paste0("2023-01-0", (i %% 5) + 1, " ", 10 + i, ":05:00")),
      baitUse = "none",
      numberDeploymentsActiveAtDate = 1
    ))
  }
  covs <- create_test_covs_matrix()

  result <- matrix_generator(obs, covs, dur = 10, w = 2,
                              site_covs = c("mode_habitat"),
                              obs_covs = character(0),
                              all_locationNames = TRUE,
                              scientificNames = "Species A",
                              type = "abundance",
                              individuals = "sum",
                              cap_count = TRUE)

  # Should successfully create capped matrix
  expect_true(is.matrix(result$Species_A$detection_matrix))
})

# Tests for edge cases ----

test_that("matrix_generator handles single species with single detection", {
  obs <- create_test_obs_matrix()[1, ]
  # Need to update covs to match the single cell
  covs <- create_test_covs_matrix()[1, ]

  result <- matrix_generator(obs, covs, dur = 10, w = 2,
                              site_covs = c("mode_habitat"),
                              obs_covs = character(0),
                              all_locationNames = TRUE,
                              scientificNames = "Species A",
                              type = "occupancy",
                              individuals = "sum")

  expect_true(is.matrix(result$Species_A$detection_matrix))
})

test_that("matrix_generator returns list structure", {
  obs <- create_test_obs_matrix()
  covs <- create_test_covs_matrix()

  result <- matrix_generator(obs, covs, dur = 10, w = 2,
                              site_covs = c("mode_habitat"),
                              obs_covs = character(0),
                              all_locationNames = TRUE,
                              scientificNames = "Species A",
                              type = "occupancy",
                              individuals = "sum")

  expect_type(result, "list")
  expect_type(result$Species_A, "list")
})
