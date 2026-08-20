# Test file for reclassify_eventID.R
# Testing the function that recalculates eventID using a new temporal threshold
# applied to the pre-calculated deltaTime_event column

library(testthat)
library(WildObsR)

# Helper function to create minimal test observations
create_test_obs <- function() {
  data.frame(
    deploymentID = "Cam01",
    scientificName = c("Canis lupus", "Canis lupus", "Canis lupus", "Canis lupus", "Canis lupus"),
    eventStart = as.POSIXct(c("2023-01-01 08:00:00",
                              "2023-01-01 08:00:05", # continuation row of first event (NA delta)
                              "2023-01-01 08:20:00", # 20 min gap, below a 60min threshold
                              "2023-01-01 09:10:00", # 50 min gap from prior row, above a 60min? no still under; used per test
                              "2023-01-02 08:00:00"),# next day, clearly a new event
                            tz = "UTC"),
    eventEnd = as.POSIXct(c("2023-01-01 08:00:02",
                            "2023-01-01 08:00:07",
                            "2023-01-01 08:20:02",
                            "2023-01-01 09:10:02",
                            "2023-01-02 08:00:02"),
                          tz = "UTC"),
    deltaTime_event = c(0, NA, 1200, 3000, 82200),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# INPUT VALIDATION TESTS
# =============================================================================

test_that("reclassify_eventID requires deploymentID column", {
  obs <- create_test_obs()
  obs$deploymentID <- NULL
  expect_error(reclassify_eventID(obs, new_event_time = 60), "deploymentID")
})

test_that("reclassify_eventID requires scientificName column", {
  obs <- create_test_obs()
  obs$scientificName <- NULL
  expect_error(reclassify_eventID(obs, new_event_time = 60), "scientificName")
})

test_that("reclassify_eventID requires eventStart column", {
  obs <- create_test_obs()
  obs$eventStart <- NULL
  expect_error(reclassify_eventID(obs, new_event_time = 60), "eventStart")
})

test_that("reclassify_eventID requires deltaTime_event column", {
  obs <- create_test_obs()
  obs$deltaTime_event <- NULL
  expect_error(reclassify_eventID(obs, new_event_time = 60), "deltaTime_event")
})

test_that("reclassify_eventID errors on non-numeric new_event_time", {
  obs <- create_test_obs()
  expect_error(reclassify_eventID(obs, new_event_time = "60"), "new_event_time")
})

test_that("reclassify_eventID errors on new_event_time of length > 1", {
  obs <- create_test_obs()
  expect_error(reclassify_eventID(obs, new_event_time = c(30, 60)), "new_event_time")
})

test_that("reclassify_eventID errors on NA new_event_time", {
  obs <- create_test_obs()
  expect_error(reclassify_eventID(obs, new_event_time = NA_real_), "new_event_time")
})

test_that("reclassify_eventID errors on non-positive new_event_time", {
  obs <- create_test_obs()
  expect_error(reclassify_eventID(obs, new_event_time = 0), "new_event_time")
  expect_error(reclassify_eventID(obs, new_event_time = -10), "new_event_time")
})

test_that("reclassify_eventID warns (but does not error) when new_event_time is below 5 minutes", {
  obs <- create_test_obs()
  expect_warning(reclassify_eventID(obs, new_event_time = 2), "5 minutes")
})

test_that("reclassify_eventID does not warn when new_event_time is at or above 5 minutes", {
  obs <- create_test_obs()
  expect_warning(reclassify_eventID(obs, new_event_time = 5), NA)
})

# =============================================================================
# EVENT RECLASSIFICATION LOGIC
# =============================================================================

test_that("reclassify_eventID embeds deploymentID, scientificName, and the new threshold tag in eventID", {
  obs <- create_test_obs()
  result <- reclassify_eventID(obs, new_event_time = 60)

  expect_true(all(grepl("^Cam01_Canis_lupus_60min_event_\\d+$", result$eventID)))
})

test_that("reclassify_eventID increments the event counter only at genuine boundaries", {
  obs <- create_test_obs()
  result <- reclassify_eventID(obs, new_event_time = 60)

  # First 4 rows are all within 60 min of one another (or NA/continuation), so should share event 1
  expect_equal(result$eventID[1:4], rep("Cam01_Canis_lupus_60min_event_1", 4))
  # The final row (next day) is a new, second event
  expect_equal(result$eventID[5], "Cam01_Canis_lupus_60min_event_2")
})

test_that("reclassify_eventID creates more distinct events with a stricter threshold", {
  obs <- create_test_obs()
  result <- reclassify_eventID(obs, new_event_time = 10)

  # With only a 10 min threshold, the 20-min and 50-min gaps should each start new events
  expect_equal(length(unique(result$eventID)), 4)
})

test_that("reclassify_eventID does not modify eventStart or eventEnd", {
  obs <- create_test_obs()
  result <- reclassify_eventID(obs, new_event_time = 60)

  expect_equal(result$eventStart, obs$eventStart)
  expect_equal(result$eventEnd, obs$eventEnd)
})

test_that("reclassify_eventID preserves the original row order", {
  obs <- create_test_obs()
  # shuffle rows so the function has to actively restore order
  shuffled <- obs[c(3, 1, 5, 2, 4), ]
  result <- reclassify_eventID(shuffled, new_event_time = 60)

  expect_equal(result$eventStart, shuffled$eventStart)
  expect_equal(result$deltaTime_event, shuffled$deltaTime_event)
})

test_that("reclassify_eventID resets event counters independently per deploymentID and scientificName", {
  obs <- rbind(
    create_test_obs(),
    within(create_test_obs(), {
      deploymentID <- "Cam02"
      scientificName <- "Vulpes vulpes"
    })
  )
  result <- reclassify_eventID(obs, new_event_time = 60)

  cam01_events <- unique(result$eventID[result$deploymentID == "Cam01"])
  cam02_events <- unique(result$eventID[result$deploymentID == "Cam02"])

  expect_equal(length(cam01_events), 2)
  expect_equal(length(cam02_events), 2)
  expect_true(all(grepl("^Cam02_Vulpes_vulpes_", cam02_events)))
})

test_that("reclassify_eventID does not drop or reorder any columns present in the input", {
  obs <- create_test_obs()
  obs$extraCovariate <- letters[1:nrow(obs)]
  result <- reclassify_eventID(obs, new_event_time = 60)

  expect_true(all(names(obs) %in% names(result)))
  expect_equal(result$extraCovariate, obs$extraCovariate)
})
