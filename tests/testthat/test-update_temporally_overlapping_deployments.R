# Test file for update_temporally_overlapping_deployments.R
# Testing the function that groups temporally overlapping deployment IDs within locations

library(testthat)
library(WildObsR)

# Helper function to create minimal test data
create_test_deployments <- function() {
  data.frame(
    deploymentID = c("loc1_dep1", "loc1_dep2", "loc2_dep1"),
    locationID = c("loc1", "loc1", "loc2"),
    deploymentStart = as.POSIXct(c("2023-01-01 00:00:00",
                                     "2023-01-15 00:00:00",
                                     "2023-01-01 00:00:00"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-10 23:59:59",
                                   "2023-01-25 23:59:59",
                                   "2023-01-31 23:59:59"), tz = "UTC"),
    deploymentGroups = c("group1", "group1", "group2"),
    stringsAsFactors = FALSE
  )
}

create_test_observations <- function(deployment_ids) {
  # Create locationID based on deployment_ids
  location_ids <- sapply(deployment_ids, function(x) {
    if (grepl("^loc[0-9]", x)) {
      # Extract locationID from deploymentID (e.g., "loc1_dep1" -> "loc1")
      sub("_.*", "", x)
    } else if (grepl("^camera", x)) {
      "site_A"
    } else if (grepl("^cam[0-9]", x)) {
      "loc1"
    } else if (grepl("^dep[0-9]", x)) {
      "loc1"
    } else {
      "loc1"
    }
  })

  data.frame(
    deploymentID = deployment_ids,
    locationID = location_ids,
    observationID = paste0("obs_", 1:length(deployment_ids)),
    stringsAsFactors = FALSE
  )
}

create_test_media <- function(deployment_ids) {
  data.frame(
    deploymentID = deployment_ids,
    mediaID = paste0("media_", 1:length(deployment_ids)),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# INPUT VALIDATION TESTS
# =============================================================================

test_that("update_temporally_overlapping_deployments requires deploymentStart column", {
  deps <- create_test_deployments()
  deps$deploymentStart <- NULL
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2", "loc2_dep1"))
  media <- create_test_media(c("loc1_dep1", "loc1_dep2", "loc2_dep1"))

  expect_error(
    update_temporally_overlapping_deployments(deps, obs, media),
    "Input deployments must have 'deploymentStart' column"
  )
})

test_that("update_temporally_overlapping_deployments requires deploymentEnd column", {
  deps <- create_test_deployments()
  deps$deploymentEnd <- NULL
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2", "loc2_dep1"))
  media <- create_test_media(c("loc1_dep1", "loc1_dep2", "loc2_dep1"))

  expect_error(
    update_temporally_overlapping_deployments(deps, obs, media),
    "Input deployments must have 'deploymentEnd' column"
  )
})

test_that("update_temporally_overlapping_deployments requires locationID column", {
  deps <- create_test_deployments()
  deps$locationID <- NULL
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2", "loc2_dep1"))
  media <- create_test_media(c("loc1_dep1", "loc1_dep2", "loc2_dep1"))

  expect_error(
    update_temporally_overlapping_deployments(deps, obs, media),
    "Input deployments must have 'locationID' column"
  )
})

test_that("update_temporally_overlapping_deployments requires deploymentID column", {
  deps <- create_test_deployments()
  deps$deploymentID <- NULL
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2", "loc2_dep1"))
  obs$deploymentID <- NULL
  media <- create_test_media(c("loc1_dep1", "loc1_dep2", "loc2_dep1"))
  media$deploymentID <- NULL

  expect_error(
    update_temporally_overlapping_deployments(deps, obs, media),
    "Input deployments must have 'deploymentID' column"
  )
})

test_that("update_temporally_overlapping_deployments requires deploymentGroups column", {
  deps <- create_test_deployments()
  deps$deploymentGroups <- NULL
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2", "loc2_dep1"))
  media <- create_test_media(c("loc1_dep1", "loc1_dep2", "loc2_dep1"))

  expect_error(
    update_temporally_overlapping_deployments(deps, obs, media),
    "Input deployments must have 'deploymentGroups' column"
  )
})

test_that("update_temporally_overlapping_deployments checks deploymentID matching between deps and obs", {
  deps <- create_test_deployments()
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2", "wrong_id"))
  media <- create_test_media(c("loc1_dep1", "loc1_dep2", "loc2_dep1"))

  expect_error(
    update_temporally_overlapping_deployments(deps, obs, media),
    "deploymentID values between the deployments and observations.*do not match"
  )
})

test_that("update_temporally_overlapping_deployments checks deploymentID matching between deps and media", {
  deps <- create_test_deployments()
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2", "loc2_dep1"))
  media <- create_test_media(c("loc1_dep1", "loc1_dep2", "wrong_id"))

  expect_error(
    update_temporally_overlapping_deployments(deps, obs, media),
    "deploymentID values between the deployments and media.*do not match"
  )
})

test_that("update_temporally_overlapping_deployments checks deploymentID matching between media and obs", {
  deps <- create_test_deployments()
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2", "wrong_id"))
  media <- create_test_media(c("loc1_dep1", "loc1_dep2", "different_wrong_id"))

  expect_error(
    update_temporally_overlapping_deployments(deps, obs, media),
    "deploymentID values.*do not match"
  )
})

test_that("update_temporally_overlapping_deployments rejects duplicate deploymentID values", {
  deps <- create_test_deployments()
  deps <- rbind(deps, deps[1,])  # Duplicate the first row
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2", "loc2_dep1", "loc1_dep1"))
  media <- create_test_media(c("loc1_dep1", "loc1_dep2", "loc2_dep1", "loc1_dep1"))

  expect_error(
    update_temporally_overlapping_deployments(deps, obs, media),
    "repeated deploymentID values"
  )
})

# =============================================================================
# BASIC FUNCTIONALITY TESTS
# =============================================================================

test_that("update_temporally_overlapping_deployments returns a list with correct structure", {
  deps <- create_test_deployments()
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2", "loc2_dep1"))
  media <- create_test_media(c("loc1_dep1", "loc1_dep2", "loc2_dep1"))

  result <- update_temporally_overlapping_deployments(deps, obs, media)

  expect_type(result, "list")
  expect_named(result, c("deployments", "observations", "media"))
  expect_s3_class(result$deployments, "data.frame")
  expect_s3_class(result$observations, "data.frame")
  expect_s3_class(result$media, "data.frame")
})

test_that("update_temporally_overlapping_deployments preserves columns in all data frames", {
  deps <- create_test_deployments()
  deps$extra_col <- "extra_data"
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2", "loc2_dep1"))
  obs$obs_extra <- "obs_data"
  media <- create_test_media(c("loc1_dep1", "loc1_dep2", "loc2_dep1"))
  media$media_extra <- "media_data"

  result <- update_temporally_overlapping_deployments(deps, obs, media)

  expect_true("extra_col" %in% colnames(result$deployments))
  expect_true("obs_extra" %in% colnames(result$observations))
  expect_true("media_extra" %in% colnames(result$media))
})

test_that("update_temporally_overlapping_deployments handles single deployment per location", {
  deps <- data.frame(
    deploymentID = c("loc1_dep1", "loc2_dep1"),
    locationID = c("loc1", "loc2"),
    deploymentStart = as.POSIXct(c("2023-01-01", "2023-02-01"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-31", "2023-02-28"), tz = "UTC"),
    deploymentGroups = c("group1", "group2"),
    stringsAsFactors = FALSE
  )
  obs <- create_test_observations(c("loc1_dep1", "loc2_dep1"))
  media <- create_test_media(c("loc1_dep1", "loc2_dep1"))

  result <- update_temporally_overlapping_deployments(deps, obs, media)

  # Should remain unchanged
  expect_equal(nrow(result$deployments), 2)
  expect_equal(sort(result$deployments$deploymentID), sort(c("loc1_dep1", "loc2_dep1")))
})

# =============================================================================
# TEMPORAL OVERLAP DETECTION TESTS
# =============================================================================

test_that("update_temporally_overlapping_deployments groups overlapping deployments at same location", {
  deps <- data.frame(
    deploymentID = c("loc1_dep1", "loc1_dep2"),
    locationID = c("loc1", "loc1"),
    deploymentStart = as.POSIXct(c("2023-01-01 00:00:00", "2023-01-05 00:00:00"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-10 23:59:59", "2023-01-15 23:59:59"), tz = "UTC"),
    deploymentGroups = c("group1", "group1"),
    stringsAsFactors = FALSE
  )
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2"))
  media <- create_test_media(c("loc1_dep1", "loc1_dep2"))

  result <- suppressMessages(
    update_temporally_overlapping_deployments(deps, obs, media)
  )

  # Should be combined into one deployment
  expect_equal(nrow(result$deployments), 1)
  expect_true(grepl("loc1_deployment", result$deployments$deploymentID[1]))

  # Check that observations and media were updated
  expect_equal(length(unique(result$observations$deploymentID)), 1)
  expect_equal(length(unique(result$media$deploymentID)), 1)
})

test_that("update_temporally_overlapping_deployments groups deployments within 24 hours", {
  deps <- data.frame(
    deploymentID = c("loc1_dep1", "loc1_dep2"),
    locationID = c("loc1", "loc1"),
    deploymentStart = as.POSIXct(c("2023-01-01 00:00:00", "2023-01-02 12:00:00"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-01 23:59:59", "2023-01-10 23:59:59"), tz = "UTC"),
    deploymentGroups = c("group1", "group1"),
    stringsAsFactors = FALSE
  )
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2"))
  media <- create_test_media(c("loc1_dep1", "loc1_dep2"))

  result <- suppressMessages(
    update_temporally_overlapping_deployments(deps, obs, media)
  )

  # Should be combined (within 24 hours)
  expect_equal(nrow(result$deployments), 1)
  expect_true(grepl("loc1_deployment", result$deployments$deploymentID[1]))
})

test_that("update_temporally_overlapping_deployments does NOT group deployments beyond 24 hours", {
  deps <- data.frame(
    deploymentID = c("loc1_dep1", "loc1_dep2"),
    locationID = c("loc1", "loc1"),
    deploymentStart = as.POSIXct(c("2023-01-01 00:00:00", "2023-01-03 12:00:00"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-01 23:59:59", "2023-01-10 23:59:59"), tz = "UTC"),
    deploymentGroups = c("group1", "group1"),
    stringsAsFactors = FALSE
  )
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2"))
  media <- create_test_media(c("loc1_dep1", "loc1_dep2"))

  result <- update_temporally_overlapping_deployments(deps, obs, media)

  # Should remain separate (> 24 hours gap)
  expect_equal(nrow(result$deployments), 2)
  expect_equal(sort(result$deployments$deploymentID), sort(c("loc1_dep1", "loc1_dep2")))
})

test_that("update_temporally_overlapping_deployments handles exact temporal overlap", {
  deps <- data.frame(
    deploymentID = c("loc1_dep1", "loc1_dep2"),
    locationID = c("loc1", "loc1"),
    deploymentStart = as.POSIXct(c("2023-01-01 00:00:00", "2023-01-01 00:00:00"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-10 23:59:59", "2023-01-10 23:59:59"), tz = "UTC"),
    deploymentGroups = c("group1", "group1"),
    stringsAsFactors = FALSE
  )
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2"))
  media <- create_test_media(c("loc1_dep1", "loc1_dep2"))

  result <- suppressMessages(
    update_temporally_overlapping_deployments(deps, obs, media)
  )

  # Should be combined (exact overlap)
  expect_equal(nrow(result$deployments), 1)
})

test_that("update_temporally_overlapping_deployments handles partial temporal overlap chain", {
  deps <- data.frame(
    deploymentID = c("loc1_dep1", "loc1_dep2", "loc1_dep3"),
    locationID = c("loc1", "loc1", "loc1"),
    deploymentStart = as.POSIXct(c("2023-01-01", "2023-01-05", "2023-01-12"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-10", "2023-01-15", "2023-01-20"), tz = "UTC"),
    deploymentGroups = c("group1", "group1", "group1"),
    stringsAsFactors = FALSE
  )
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2", "loc1_dep3"))
  media <- create_test_media(c("loc1_dep1", "loc1_dep2", "loc1_dep3"))

  result <- suppressMessages(
    update_temporally_overlapping_deployments(deps, obs, media)
  )

  # All three should be combined into one (chain of overlaps: dep1 overlaps dep2, dep2 overlaps dep3)
  expect_equal(nrow(result$deployments), 1)
})

test_that("update_temporally_overlapping_deployments separates deployments with gap > 24 hours", {
  deps <- data.frame(
    deploymentID = c("loc1_dep1", "loc1_dep2", "loc1_dep3"),
    locationID = c("loc1", "loc1", "loc1"),
    deploymentStart = as.POSIXct(c("2023-01-01", "2023-01-05", "2023-01-20"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-10", "2023-01-15", "2023-01-25"), tz = "UTC"),
    deploymentGroups = c("group1", "group1", "group1"),
    stringsAsFactors = FALSE
  )
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2", "loc1_dep3"))
  media <- create_test_media(c("loc1_dep1", "loc1_dep2", "loc1_dep3"))

  result <- suppressMessages(
    update_temporally_overlapping_deployments(deps, obs, media)
  )

  # First two should be combined (overlap), third should be separate (gap from Jan 15 to Jan 20 = 5 days > 24h)
  expect_equal(nrow(result$deployments), 2)
})

# =============================================================================
# MULTIPLE LOCATION TESTS
# =============================================================================

test_that("update_temporally_overlapping_deployments handles overlaps at different locations independently", {
  deps <- data.frame(
    deploymentID = c("loc1_dep1", "loc1_dep2", "loc2_dep1", "loc2_dep2"),
    locationID = c("loc1", "loc1", "loc2", "loc2"),
    deploymentStart = as.POSIXct(c("2023-01-01", "2023-01-05", "2023-01-01", "2023-01-05"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-10", "2023-01-15", "2023-01-10", "2023-01-15"), tz = "UTC"),
    deploymentGroups = c("group1", "group1", "group2", "group2"),
    stringsAsFactors = FALSE
  )
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2", "loc2_dep1", "loc2_dep2"))
  media <- create_test_media(c("loc1_dep1", "loc1_dep2", "loc2_dep1", "loc2_dep2"))

  result <- suppressMessages(
    update_temporally_overlapping_deployments(deps, obs, media)
  )

  # Should have 2 deployments (one per location)
  expect_equal(nrow(result$deployments), 2)
  expect_equal(length(unique(result$deployments$locationID)), 2)
})

test_that("update_temporally_overlapping_deployments handles three locations with different overlap patterns", {
  deps <- data.frame(
    deploymentID = c("loc1_dep1", "loc1_dep2", "loc2_dep1", "loc3_dep1", "loc3_dep2"),
    locationID = c("loc1", "loc1", "loc2", "loc3", "loc3"),
    deploymentStart = as.POSIXct(c("2023-01-01", "2023-01-05", "2023-01-01", "2023-01-01", "2023-02-01"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-10", "2023-01-15", "2023-01-31", "2023-01-10", "2023-02-28"), tz = "UTC"),
    deploymentGroups = c("g1", "g1", "g2", "g3", "g3"),
    stringsAsFactors = FALSE
  )
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2", "loc2_dep1", "loc3_dep1", "loc3_dep2"))
  media <- create_test_media(c("loc1_dep1", "loc1_dep2", "loc2_dep1", "loc3_dep1", "loc3_dep2"))

  result <- suppressMessages(
    update_temporally_overlapping_deployments(deps, obs, media)
  )

  # loc1: 2 overlapping -> 1
  # loc2: 1 single -> 1
  # loc3: 2 non-overlapping -> 2
  # Total: 4 deployments
  expect_equal(nrow(result$deployments), 4)
})

# =============================================================================
# DEPLOYMENT GROUP ASSIGNMENT TESTS
# =============================================================================

test_that("update_temporally_overlapping_deployments assigns earliest deploymentGroups to merged deployment", {
  deps <- data.frame(
    deploymentID = c("loc1_dep1", "loc1_dep2"),
    locationID = c("loc1", "loc1"),
    deploymentStart = as.POSIXct(c("2023-01-01", "2023-01-05"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-10", "2023-01-15"), tz = "UTC"),
    deploymentGroups = c("early_group", "late_group"),
    stringsAsFactors = FALSE
  )
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2"))
  media <- create_test_media(c("loc1_dep1", "loc1_dep2"))

  result <- suppressMessages(
    update_temporally_overlapping_deployments(deps, obs, media)
  )

  # Should use earliest deploymentGroups
  expect_equal(result$deployments$deploymentGroups[1], "early_group")
})

test_that("update_temporally_overlapping_deployments sets min deploymentStart for merged deployment", {
  deps <- data.frame(
    deploymentID = c("loc1_dep1", "loc1_dep2"),
    locationID = c("loc1", "loc1"),
    deploymentStart = as.POSIXct(c("2023-01-05", "2023-01-01"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-15", "2023-01-10"), tz = "UTC"),
    deploymentGroups = c("group1", "group1"),
    stringsAsFactors = FALSE
  )
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2"))
  media <- create_test_media(c("loc1_dep1", "loc1_dep2"))

  result <- suppressMessages(
    update_temporally_overlapping_deployments(deps, obs, media)
  )

  # Should use minimum start date
  expect_equal(result$deployments$deploymentStart[1],
               as.POSIXct("2023-01-01", tz = "UTC"))
})

test_that("update_temporally_overlapping_deployments sets max deploymentEnd for merged deployment", {
  deps <- data.frame(
    deploymentID = c("loc1_dep1", "loc1_dep2"),
    locationID = c("loc1", "loc1"),
    deploymentStart = as.POSIXct(c("2023-01-01", "2023-01-05"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-20", "2023-01-10"), tz = "UTC"),
    deploymentGroups = c("group1", "group1"),
    stringsAsFactors = FALSE
  )
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2"))
  media <- create_test_media(c("loc1_dep1", "loc1_dep2"))

  result <- suppressMessages(
    update_temporally_overlapping_deployments(deps, obs, media)
  )

  # Should use maximum end date
  expect_equal(result$deployments$deploymentEnd[1],
               as.POSIXct("2023-01-20", tz = "UTC"))
})

# =============================================================================
# DEPLOYMENT ID NAMING TESTS
# =============================================================================

test_that("update_temporally_overlapping_deployments creates new deploymentID with locationID prefix", {
  deps <- data.frame(
    deploymentID = c("camera1", "camera2"),
    locationID = c("site_A", "site_A"),
    deploymentStart = as.POSIXct(c("2023-01-01", "2023-01-05"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-10", "2023-01-15"), tz = "UTC"),
    deploymentGroups = c("group1", "group1"),
    stringsAsFactors = FALSE
  )
  obs <- create_test_observations(c("camera1", "camera2"))
  media <- create_test_media(c("camera1", "camera2"))

  result <- suppressMessages(
    update_temporally_overlapping_deployments(deps, obs, media)
  )

  # New deploymentID should start with locationID
  expect_true(grepl("^site_A_deployment_", result$deployments$deploymentID[1]))
})

test_that("update_temporally_overlapping_deployments numbers multiple groups at same location", {
  deps <- data.frame(
    deploymentID = c("cam1", "cam2", "cam3", "cam4"),
    locationID = c("loc1", "loc1", "loc1", "loc1"),
    deploymentStart = as.POSIXct(c("2023-01-01", "2023-01-02", "2023-02-01", "2023-02-02"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-05", "2023-01-10", "2023-02-05", "2023-02-10"), tz = "UTC"),
    deploymentGroups = c("g1", "g1", "g2", "g2"),
    stringsAsFactors = FALSE
  )
  obs <- create_test_observations(c("cam1", "cam2", "cam3", "cam4"))
  media <- create_test_media(c("cam1", "cam2", "cam3", "cam4"))

  result <- suppressMessages(
    update_temporally_overlapping_deployments(deps, obs, media)
  )

  # Should have 2 merged deployments with group numbers 1 and 2
  expect_equal(nrow(result$deployments), 2)
  expect_true(any(grepl("_deployment_1$", result$deployments$deploymentID)))
  expect_true(any(grepl("_deployment_2$", result$deployments$deploymentID)))
})

# =============================================================================
# OBSERVATIONS AND MEDIA UPDATE TESTS
# =============================================================================

test_that("update_temporally_overlapping_deployments updates observations deploymentID", {
  deps <- data.frame(
    deploymentID = c("loc1_dep1", "loc1_dep2"),
    locationID = c("loc1", "loc1"),
    deploymentStart = as.POSIXct(c("2023-01-01", "2023-01-05"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-10", "2023-01-15"), tz = "UTC"),
    deploymentGroups = c("group1", "group1"),
    stringsAsFactors = FALSE
  )
  obs <- data.frame(
    deploymentID = c("loc1_dep1", "loc1_dep1", "loc1_dep2"),
    locationID = c("loc1", "loc1", "loc1"),
    observationID = c("obs1", "obs2", "obs3"),
    stringsAsFactors = FALSE
  )
  media <- create_test_media(c("loc1_dep1", "loc1_dep2"))

  result <- suppressMessages(
    update_temporally_overlapping_deployments(deps, obs, media)
  )

  # All observations should have the new deploymentID
  expect_equal(length(unique(result$observations$deploymentID)), 1)
  expect_true(grepl("loc1_deployment", result$observations$deploymentID[1]))
})

test_that("update_temporally_overlapping_deployments updates media deploymentID", {
  deps <- data.frame(
    deploymentID = c("loc1_dep1", "loc1_dep2"),
    locationID = c("loc1", "loc1"),
    deploymentStart = as.POSIXct(c("2023-01-01", "2023-01-05"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-10", "2023-01-15"), tz = "UTC"),
    deploymentGroups = c("group1", "group1"),
    stringsAsFactors = FALSE
  )
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2"))
  media <- data.frame(
    deploymentID = c("loc1_dep1", "loc1_dep1", "loc1_dep1", "loc1_dep2", "loc1_dep2"),
    mediaID = paste0("media_", 1:5),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(
    update_temporally_overlapping_deployments(deps, obs, media)
  )

  # All media should have the new deploymentID
  expect_equal(length(unique(result$media$deploymentID)), 1)
  expect_true(grepl("loc1_deployment", result$media$deploymentID[1]))
})

test_that("update_temporally_overlapping_deployments preserves observations for non-merged deployments", {
  deps <- data.frame(
    deploymentID = c("loc1_dep1", "loc1_dep2", "loc2_dep1"),
    locationID = c("loc1", "loc1", "loc2"),
    deploymentStart = as.POSIXct(c("2023-01-01", "2023-01-05", "2023-01-01"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-10", "2023-01-15", "2023-01-31"), tz = "UTC"),
    deploymentGroups = c("g1", "g1", "g2"),
    stringsAsFactors = FALSE
  )
  obs <- data.frame(
    deploymentID = c("loc1_dep1", "loc1_dep2", "loc2_dep1", "loc2_dep1"),
    locationID = c("loc1", "loc1", "loc2", "loc2"),
    observationID = c("obs1", "obs2", "obs3", "obs4"),
    stringsAsFactors = FALSE
  )
  media <- create_test_media(c("loc1_dep1", "loc1_dep2", "loc2_dep1"))

  result <- suppressMessages(
    update_temporally_overlapping_deployments(deps, obs, media)
  )

  # Should have 4 observations total
  expect_equal(nrow(result$observations), 4)
  # loc2_dep1 should remain unchanged
  expect_equal(sum(result$observations$deploymentID == "loc2_dep1"), 2)
})

# =============================================================================
# EDGE CASES AND COMPLEX SCENARIOS
# =============================================================================

test_that("update_temporally_overlapping_deployments handles three consecutive overlapping deployments", {
  deps <- data.frame(
    deploymentID = c("dep1", "dep2", "dep3"),
    locationID = c("loc1", "loc1", "loc1"),
    deploymentStart = as.POSIXct(c("2023-01-01", "2023-01-05", "2023-01-10"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-08", "2023-01-12", "2023-01-20"), tz = "UTC"),
    deploymentGroups = c("g1", "g1", "g1"),
    stringsAsFactors = FALSE
  )
  obs <- create_test_observations(c("dep1", "dep2", "dep3"))
  media <- create_test_media(c("dep1", "dep2", "dep3"))

  result <- suppressMessages(
    update_temporally_overlapping_deployments(deps, obs, media)
  )

  # All three should be merged into one
  expect_equal(nrow(result$deployments), 1)
  expect_true(grepl("loc1_deployment", result$deployments$deploymentID[1]))
})

test_that("update_temporally_overlapping_deployments handles chain of deployments with 24hr gaps", {
  deps <- data.frame(
    deploymentID = c("dep1", "dep2", "dep3"),
    locationID = c("loc1", "loc1", "loc1"),
    deploymentStart = as.POSIXct(c("2023-01-01 00:00:00", "2023-01-02 23:00:00", "2023-01-04 22:00:00"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-02 22:00:00", "2023-01-04 21:00:00", "2023-01-10 00:00:00"), tz = "UTC"),
    deploymentGroups = c("g1", "g1", "g1"),
    stringsAsFactors = FALSE
  )
  obs <- create_test_observations(c("dep1", "dep2", "dep3"))
  media <- create_test_media(c("dep1", "dep2", "dep3"))

  result <- suppressMessages(
    update_temporally_overlapping_deployments(deps, obs, media)
  )

  # All should be grouped (each gap < 24 hours)
  expect_equal(nrow(result$deployments), 1)
})

test_that("update_temporally_overlapping_deployments handles reversed deployment order", {
  deps <- data.frame(
    deploymentID = c("dep2", "dep1"),  # Reversed order
    locationID = c("loc1", "loc1"),
    deploymentStart = as.POSIXct(c("2023-01-10", "2023-01-01"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-15", "2023-01-12"), tz = "UTC"),
    deploymentGroups = c("g1", "g1"),
    stringsAsFactors = FALSE
  )
  obs <- create_test_observations(c("dep2", "dep1"))
  media <- create_test_media(c("dep2", "dep1"))

  result <- suppressMessages(
    update_temporally_overlapping_deployments(deps, obs, media)
  )

  # Should still detect overlap (function sorts by deploymentStart)
  expect_equal(nrow(result$deployments), 1)
  # Should use earliest deploymentGroups
  expect_equal(result$deployments$deploymentGroups[1], "g1")
})

test_that("update_temporally_overlapping_deployments handles same start, different end times", {
  deps <- data.frame(
    deploymentID = c("dep1", "dep2"),
    locationID = c("loc1", "loc1"),
    deploymentStart = as.POSIXct(c("2023-01-01", "2023-01-01"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-10", "2023-01-20"), tz = "UTC"),
    deploymentGroups = c("g1", "g1"),
    stringsAsFactors = FALSE
  )
  obs <- create_test_observations(c("dep1", "dep2"))
  media <- create_test_media(c("dep1", "dep2"))

  result <- suppressMessages(
    update_temporally_overlapping_deployments(deps, obs, media)
  )

  # Should be merged
  expect_equal(nrow(result$deployments), 1)
  # Should use latest end time
  expect_equal(result$deployments$deploymentEnd[1], as.POSIXct("2023-01-20", tz = "UTC"))
})

test_that("update_temporally_overlapping_deployments maintains deploymentID consistency after update", {
  deps <- data.frame(
    deploymentID = c("loc1_dep1", "loc1_dep2", "loc2_dep1"),
    locationID = c("loc1", "loc1", "loc2"),
    deploymentStart = as.POSIXct(c("2023-01-01", "2023-01-05", "2023-01-01"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-10", "2023-01-15", "2023-01-31"), tz = "UTC"),
    deploymentGroups = c("g1", "g1", "g2"),
    stringsAsFactors = FALSE
  )
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2", "loc2_dep1"))
  media <- create_test_media(c("loc1_dep1", "loc1_dep2", "loc2_dep1"))

  result <- suppressMessages(
    update_temporally_overlapping_deployments(deps, obs, media)
  )

  # Check all deploymentIDs match across data frames
  deps_ids <- sort(unique(result$deployments$deploymentID))
  obs_ids <- sort(unique(result$observations$deploymentID))
  media_ids <- sort(unique(result$media$deploymentID))

  expect_equal(deps_ids, obs_ids)
  expect_equal(deps_ids, media_ids)
  expect_equal(obs_ids, media_ids)
})

test_that("update_temporally_overlapping_deployments prints informative messages", {
  deps <- data.frame(
    deploymentID = c("loc1_dep1", "loc1_dep2"),
    locationID = c("loc1", "loc1"),
    deploymentStart = as.POSIXct(c("2023-01-01", "2023-01-05"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-10", "2023-01-15"), tz = "UTC"),
    deploymentGroups = c("group1", "group1"),
    stringsAsFactors = FALSE
  )
  obs <- create_test_observations(c("loc1_dep1", "loc1_dep2"))
  media <- create_test_media(c("loc1_dep1", "loc1_dep2"))

  expect_output(
    update_temporally_overlapping_deployments(deps, obs, media),
    "temporally overlapping deployments"
  )
})
