## Tests for detection_matrix() ----

## The first cases are the ones matrix_generator() got wrong (issue #140 and the review
## behind the new functions), with their true answers worked out by hand. Then come
## metamorphic tests, which change the detections in ways that must not change the result,
## and a cross-check against camtrapR on a Camtrap DP package from outside WildObs.

## midnight on day n of February 2023 in Brisbane, plus h hours
bris_day <- function(day, h = 0) {
  as.POSIXct("2023-02-01 00:00:00", tz = "Australia/Brisbane") + (day - 1) * 86400 + h * 3600
} # end day helper

## cell C1 holds cameras A and B, cell C2 holds camera C; all run days 1 to 10 in full
three_cameras <- function() {
  data.frame(deploymentID = c("A", "B", "C"), cell = c("C1", "C1", "C2"),
             deploymentStart = bris_day(c(1, 1, 1)), deploymentEnd = bris_day(c(11, 11, 11)))
} # end three cameras

## species X: day 2 at A, day 8 at A and at B in the same event; species Y: day 1 at B and C
detections <- function() {
  data.frame(deploymentID = c("B", "A", "A", "B", "C"),
             eventID = c("e1", "e2", "e3", "e4", "e5"),
             eventStart = bris_day(c(1, 2, 8, 8, 1), 12),
             scientificName = c("Y", "X", "X", "X", "Y"),
             count = c(1, 1, 1, 1, 1), observationType = "animal")
} # end detections

## the effort for those cameras, in 5-day occasions
five_day_effort <- function() effort_matrix(three_cameras(), site_col = "cell", w = 5)


#
##
### Cases with known answers ----

test_that("detection_matrix finds a detection by any camera in the cell", {
  y <- detection_matrix(detections(), five_day_effort(), c("X", "Y"))

  # X was seen in cell C1 in both occasions
  expect_equal(unname(y["X", "C1", ]), c(1, 1))
  # Y was seen once in each cell, in the first occasion only
  expect_equal(unname(y["Y", "C1", ]), c(1, 0))
  expect_equal(unname(y["Y", "C2", ]), c(1, 0))
})

test_that("detection_matrix gives zeros, not NA, while cameras run without detections", {
  y <- detection_matrix(detections(), five_day_effort(), "X")

  # camera C ran all ten days and never saw X
  expect_equal(unname(y["X", "C2", ]), c(0, 0))
})

test_that("detection_matrix gives NA wherever no camera was running", {
  # camera C stops after day 5
  deps <- three_cameras()
  deps$deploymentEnd[3] <- bris_day(6)
  eff <- effort_matrix(deps, site_col = "cell", w = 5, align = "calendar")

  y <- detection_matrix(detections(), eff, "Y")

  # observed in occasion 1, no sampling in occasion 2
  expect_equal(unname(y["Y", "C2", ]), c(1, NA))
})

test_that("detection_matrix counts individuals per event, then sums or maxes over events", {
  obs <- detections()
  # the day-8 event at camera A saw three animals across two observations
  obs <- rbind(obs, transform(obs[3, ], count = 3))

  summed <- detection_matrix(obs, five_day_effort(), "X", type = "abundance")
  maxed <- detection_matrix(obs, five_day_effort(), "X", type = "abundance", individuals = "max")

  # occasion 2: event e3 counts its largest observation (3), event e4 counts 1
  expect_equal(summed["X", "C1", "o2"], 4)
  expect_equal(maxed["X", "C1", "o2"], 3)
})

test_that("detection_matrix keeps species that were never detected, as zeros", {
  expect_message(y <- detection_matrix(detections(), five_day_effort(), c("X", "Z")),
                 "no detections of Z")
  expect_equal(unname(y["Z", , ]), matrix(0, 2, 2))
})

test_that("detection_matrix can keep each camera separate", {
  y <- detection_matrix(detections(), five_day_effort(), "X", by_camera = TRUE)

  # four dimensions, matching the effort array
  expect_length(dim(y), 4)
  # camera A (slot 1) saw X in both occasions, camera B (slot 2) in the second only
  expect_equal(unname(y["X", "C1", , "deployment_1"]), c(1, 1))
  expect_equal(unname(y["X", "C1", , "deployment_2"]), c(0, 1))
  # cell C2 has no second camera, so that slot is NA
  expect_true(all(is.na(y["X", "C2", , "deployment_2"])))
})


#
##
### Records that are set aside, and records that are errors ----

test_that("detection_matrix sets obscured and non-animal records aside, saying so", {
  obs <- rbind(detections(),
               data.frame(deploymentID = "obscured_for_endangered_species", eventID = "x",
                          eventStart = bris_day(3), scientificName = "X", count = 1,
                          observationType = "animal"),
               data.frame(deploymentID = "A", eventID = "b", eventStart = bris_day(4),
                          scientificName = "X", count = NA, observationType = "blank"))

  # both kinds of record are reported
  msgs <- testthat::capture_messages(y <- detection_matrix(obs, five_day_effort(), "X"))
  expect_true(any(grepl("set aside 1 obscured", msgs)))
  expect_true(any(grepl("1 observation\\(s\\) that are not of animals", msgs)))
  # and the histories are those of the animal records alone
  expect_equal(y, suppressMessages(detection_matrix(detections(), five_day_effort(), "X")))
})

test_that("detection_matrix stops on a detection when its camera was not running", {
  obs <- detections()
  # a detection on day 20, after every camera was collected
  obs$eventStart[1] <- bris_day(20)
  expect_error(detection_matrix(obs, five_day_effort(), "Y"),
               "1 detection\\(s\\) on days their camera was not running")
})

test_that("detection_matrix stops on a deployment the effort matrix does not know", {
  obs <- detections()
  obs$deploymentID[1] <- "D"
  expect_error(detection_matrix(obs, five_day_effort(), "Y"), "not in effort")
})


#
##
### Changes to detections that must not change the result ----

test_that("removing detections never changes where the histories are NA", {
  eff <- effort_matrix(three_cameras(), site_col = "cell", w = 5, align = "calendar")

  all_obs <- detection_matrix(detections(), eff, c("X", "Y"))
  no_obs <- suppressMessages(detection_matrix(detections()[0, ], eff, c("X", "Y")))

  # sampled cells depend on effort only, so the NA pattern is identical
  expect_identical(is.na(all_obs), is.na(no_obs))
})

test_that("which camera in a cell made a detection does not change the cell's history", {
  swapped <- detections()
  # move camera A's detections to camera B, in the same cell, on the same days
  swapped$deploymentID[swapped$deploymentID == "A"] <- "B"

  expect_equal(detection_matrix(swapped, five_day_effort(), "X"),
               detection_matrix(detections(), five_day_effort(), "X"))
})


#
##
### Agreement with camtrapR ----

test_that("detection_matrix and effort_matrix agree with camtrapR on single-camera stations", {
  skip_if_not_installed("camtrapdp", minimum_version = "0.5.0")
  skip_if_not_installed("camtrapR")
  x <- camtrapdp::example_dataset()
  # both tables as camtrapdp returns them, tibbles, as read_resource() also returns
  deps <- camtrapdp::deployments(x)
  obs <- camtrapdp::observations(x)
  # the most often detected species
  animals <- obs[obs$observationType == "animal", ]
  sp <- names(sort(table(animals$scientificName), decreasing = TRUE))[1]

  ## WildObsR: one site per deployment, weekly occasions starting at each station's setup
  eff <- effort_matrix(deps, w = 7, time_zone = "UTC")
  y <- suppressMessages(detection_matrix(obs, eff, sp))[1, , ]

  ## camtrapR, from the same deployments and records
  ct <- data.frame(Station = deps$deploymentID,
                   Setup_date = format(deps$deploymentStart, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                   Retrieval_date = format(deps$deploymentEnd, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  camop <- camtrapR::cameraOperation(ct, stationCol = "Station", setupCol = "Setup_date",
                                     retrievalCol = "Retrieval_date", dateFormat = "ymd HMS",
                                     writecsv = FALSE)
  rec <- animals[animals$scientificName == sp, ]
  rec <- data.frame(Station = rec$deploymentID, Species = rec$scientificName,
                    DateTimeOriginal = format(rec$eventStart, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  dh <- camtrapR::detectionHistory(rec, camOp = camop, stationCol = "Station",
                                   speciesCol = "Species", recordDateTimeCol = "DateTimeOriginal",
                                   recordDateTimeFormat = "ymd HMS", species = sp,
                                   occasionLength = 7, day1 = "station", includeEffort = TRUE,
                                   scaleEffort = FALSE, timeZone = "UTC")

  # identical detection histories, NA included
  expect_equal(unname(y), unname(dh$detection_history[rownames(y), ]))
  # the same camera-days wherever cameras ran, to within 0.0001 camera-days (camtrapR's
  # values differ in the fifth decimal place); camtrapR writes NA where we write 0
  their_effort <- dh$effort[rownames(y), ]
  their_effort[is.na(their_effort)] <- 0
  expect_equal(unname(eff$effort_total), unname(their_effort), tolerance = 1e-4)
})
