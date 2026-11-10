## Tests for effort_matrix() ----

## Hand-built deployments with effort that can be worked out on paper, plus the camtrapdp
## package's example dataset as a Camtrap DP package from outside WildObs.

## midnight on day n of February 2023 in Brisbane, plus h hours
bris_day <- function(day, h = 0) {
  as.POSIXct("2023-02-01 00:00:00", tz = "Australia/Brisbane") + (day - 1) * 86400 + h * 3600
} # end day helper

## two cameras in cell C1 and one in C2, every one running days 1 to 10 in full
three_cameras <- function() {
  data.frame(deploymentID = c("A", "B", "C"), cell = c("C1", "C1", "C2"),
             featureType = c("trail", "none", "none"),
             deploymentStart = bris_day(c(1, 1, 1)), deploymentEnd = bris_day(c(11, 11, 11)))
} # end three cameras


#
##
### Camera-days ----

test_that("effort_matrix counts every camera running in a cell", {
  eff <- effort_matrix(three_cameras(), site_col = "cell", w = 5)

  # two cameras for five full days is ten camera-days per occasion
  expect_equal(unname(eff$effort_total["C1", ]), c(10, 10))
  expect_equal(unname(eff$effort_total["C2", ]), c(5, 5))
  # and each camera keeps its own slot
  expect_equal(unname(eff$effort["C1", , ]), matrix(5, 2, 2))
  expect_s3_class(eff, "wildobs_effort")
})

test_that("effort_matrix counts part days as fractions of a camera-day", {
  # a camera set at noon on day 1 and collected at 06:00 on day 3
  deps <- data.frame(deploymentID = "A", deploymentStart = bris_day(1, 12),
                     deploymentEnd = bris_day(3, 6))

  eff <- effort_matrix(deps, w = 1)

  # half a day, a full day, and a quarter day
  expect_equal(unname(eff$effort_total[1, ]), c(0.5, 1, 0.25))
})

test_that("effort_matrix keeps every day in exactly one occasion, with a short last occasion", {
  # one camera for 12 full days, in occasions of 5
  deps <- data.frame(deploymentID = "A", deploymentStart = bris_day(1), deploymentEnd = bris_day(13))

  eff <- effort_matrix(deps, w = 5)

  # 5 + 5 + 2 days, so no day is counted twice or lost
  expect_equal(unname(eff$effort_total[1, ]), c(5, 5, 2))
  expect_equal(sum(eff$effort_total), 12)
  # the occasions table says the last occasion is two days long
  expect_equal(eff$occasions$days, c(5, 5, 5))
  expect_equal(eff$occasions$start[3], as.Date("2023-02-11"))
})

test_that("effort_matrix truncates at dur", {
  deps <- data.frame(deploymentID = "A", deploymentStart = bris_day(1), deploymentEnd = bris_day(13))

  eff <- effort_matrix(deps, w = 5, dur = 7)

  # seven days kept: a full occasion and a two-day one
  expect_equal(unname(eff$effort_total[1, ]), c(5, 2))
  expect_equal(eff$occasions$days, c(5, 2))
})

test_that("effort_matrix uses local days, including a 23-hour daylight-saving day", {
  # Sydney moved its clocks forward on 1 October 2023, so that day had 23 hours
  deps <- data.frame(deploymentID = "A",
                     deploymentStart = as.POSIXct("2023-09-30 00:00", tz = "Australia/Sydney"),
                     deploymentEnd = as.POSIXct("2023-10-03 00:00", tz = "Australia/Sydney"))

  eff <- effort_matrix(deps, w = 1)

  # three full local days, each one camera-day, whatever its length in hours
  expect_equal(unname(eff$effort_total[1, ]), c(1, 1, 1))
  expect_identical(eff$settings$time_zone, "Australia/Sydney")
})


#
##
### Alignment, splitting and long-term deployments ----

test_that("effort_matrix aligns occasions by site or by calendar", {
  # station 2 starts six days after station 1
  deps <- data.frame(deploymentID = c("A", "B"), deploymentStart = bris_day(c(1, 7)),
                     deploymentEnd = bris_day(c(11, 17)))

  by_site <- effort_matrix(deps, w = 5)
  by_date <- effort_matrix(deps, w = 5, align = "calendar")

  # by site, both stations start their own first occasion with five days
  expect_equal(unname(by_site$effort_total["B", ]), c(5, 5))
  # by calendar, station 2 is idle in occasion 1 and starts part way through occasion 2
  expect_equal(unname(by_date$effort_total["B", ]), c(0, 4, 5, 1))
  # and occasion 2 covers the same dates at both stations
  occ2 <- by_date$occasions[by_date$occasions$occasion == 2, ]
  expect_equal(occ2$start[1], occ2$start[2])
})

test_that("effort_matrix splits effort by a deployment column", {
  eff <- effort_matrix(three_cameras(), site_col = "cell", w = 5, split_by = "featureType")

  # cell C1 had one trail camera and one off-trail camera
  expect_setequal(dimnames(eff$effort)[[3]], c("none", "trail"))
  expect_equal(unname(eff$effort["C1", , "trail"]), c(5, 5))
  expect_equal(unname(eff$effort["C1", , "none"]), c(5, 5))
  # the levels add up to the total
  expect_equal(apply(eff$effort, c(1, 2), sum), eff$effort_total)
})

test_that("effort_matrix counts the gap between recombined pieces as no effort", {
  # one long deployment, recombined from two pieces with a five-day gap between them
  deps <- data.frame(deploymentID = "A_ms1", deploymentStart = bris_day(1),
                     deploymentEnd = bris_day(16))
  crosswalk <- data.frame(deploymentID = "A_ms1", original_deploymentID = c("A_a", "A_b"),
                          pieceStart = bris_day(c(1, 11)), pieceEnd = bris_day(c(6, 16)))

  eff <- effort_matrix(deps, w = 5, crosswalk = crosswalk)

  # days 1-5 run, days 6-10 are the gap, days 11-15 run
  expect_equal(unname(eff$effort_total[1, ]), c(5, 0, 5))
})


#
##
### Input checks ----

test_that("effort_matrix stops on inputs it cannot use", {
  deps <- three_cameras()

  # a site column that does not exist
  expect_error(effort_matrix(deps, site_col = "cellID_1km2", w = 5), "could not find these columns")
  # an occasion length that is not a whole number of days
  expect_error(effort_matrix(deps, w = 2.5), "whole number of days")
  # dates stored as text
  bad <- deps
  bad$deploymentStart <- format(bad$deploymentStart)
  expect_error(effort_matrix(bad, w = 5), "POSIXct")
  # a deployment that ends before it starts
  bad <- deps
  bad$deploymentEnd[1] <- bad$deploymentStart[1] - 3600
  expect_error(effort_matrix(bad, w = 5), "end before they start")
  # the same deployment twice
  expect_error(effort_matrix(rbind(deps, deps[1, ]), w = 5), "repeated deploymentID")
})


#
##
### A Camtrap DP package from outside WildObs ----

test_that("effort_matrix runs on the camtrapdp example dataset", {
  skip_if_not_installed("camtrapdp", minimum_version = "0.5.0")
  # the deployments as camtrapdp returns them, a tibble, as read_resource() also returns
  deps <- camtrapdp::deployments(camtrapdp::example_dataset())

  # one site per station, weekly occasions
  eff <- effort_matrix(deps, site_col = "locationID", w = 7)

  # one row per station, and total effort equals the deployments' running time
  expect_equal(nrow(eff$effort_total), length(unique(deps$locationID)))
  running <- sum(as.numeric(difftime(deps$deploymentEnd, deps$deploymentStart, units = "days")))
  expect_equal(sum(eff$effort_total), running, tolerance = 1e-6)
})

test_that("effort_matrix() uses UTC days when the cameras span several time zones", {
  # two cameras ten days each, one near Brisbane and one near Perth
  deps <- data.frame(deploymentID = c("A", "B"), locationID = c("a", "b"),
                     latitude = c(-27.5, -31.9), longitude = c(153.0, 115.9),
                     deploymentStart = as.POSIXct("2024-01-01 00:00", tz = "Australia/Brisbane"),
                     deploymentEnd = as.POSIXct("2024-01-11 00:00", tz = "Australia/Brisbane"))

  # found from the coordinates, with a message naming both zones
  expect_message(eff <- effort_matrix(deps, site_col = "locationID", w = 5),
                 "span 2 time zones \\(Australia/Brisbane, Australia/Perth\\)")
  expect_equal(eff$settings$time_zone, "UTC")
  # the same when several zones are given
  expect_message(eff <- effort_matrix(deps, site_col = "locationID", w = 5,
                                      time_zone = c("Australia/Brisbane", "Australia/Perth")),
                 "span 2 time zones")
  expect_equal(eff$settings$time_zone, "UTC")
  # one zone given keeps local days
  eff <- effort_matrix(deps, site_col = "locationID", w = 5, time_zone = "Australia/Brisbane")
  expect_equal(eff$settings$time_zone, "Australia/Brisbane")
  # zones with the same clock count as one: Brisbane and the Whitsundays
  deps$latitude <- c(-27.5, -20.3)
  deps$longitude <- c(153.0, 148.9)
  eff <- effort_matrix(deps, site_col = "locationID", w = 5)
  expect_equal(eff$settings$time_zone, "Australia/Brisbane")
})
