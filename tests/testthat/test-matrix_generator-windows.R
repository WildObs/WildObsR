### Tests for matrix_generator() sampling occasions (#140) ----

## two cells that ran for `duration` days, with one record a day each
## species A on detected_days and species B otherwise; the count equals the day number,
## so a sum over an occasion shows exactly which days it holds
window_fixture <- function(duration, detected_days = seq_len(duration)) {
  # one date-time per day, at noon local time
  dates <- as.POSIXct("2023-02-01 12:00:00", tz = "Australia/Brisbane") +
    (seq_len(duration) - 1) * 86400
  # the observations, one per cell per day
  obs <- data.frame(
    cellID_10km = rep(c("Cell_A", "Cell_B"), each = duration),
    scientificName = rep(ifelse(seq_len(duration) %in% detected_days,
                                "Species A", "Species B"), 2),
    totalIndividuals = rep(seq_len(duration), 2),
    eventStart = rep(dates, 2),
    eventEnd = rep(dates + 60, 2),
    numberDeploymentsActiveAtDate = rep(rep(c(1, 2), length.out = duration), 2)
  )
  # the two cells, both sampled for the whole period
  covs <- data.frame(
    cellID_10km = c("Cell_A", "Cell_B"),
    locationName = c("Site_A", "Site_B"),
    samplingStart = rep(dates[1], 2),
    samplingEnd = rep(dates[duration], 2),
    Avg_latitude = c(-17.5, -17.6),
    Avg_longitude = c(145.5, 145.6),
    habitat = c("forest", "grassland"),
    elevation = c(10, 20)
  )
  list(obs = obs, covs = covs)
} # end fixture

## species A's matrices from a fixture, with occasions of `width` days
generate_window_fixture <- function(fixture, duration, width,
                                    type = "abundance", individuals = "sum",
                                    obs_covs = character()) {
  matrix_generator(
    fixture$obs, fixture$covs, dur = duration, w = width,
    site_covs = c("habitat", "elevation"), obs_covs = obs_covs,
    all_locationNames = TRUE, scientificNames = "Species A",
    type = type, individuals = individuals
  )$Species_A
} # end helper

test_that("every day is counted once, including short and partial occasions", {
  # for survey lengths that do and do not divide evenly into occasions
  for (duration in c(1, 2, 3, 5, 10, 91, 94)) {
    fixture <- window_fixture(duration)
    # and for daily and five-day occasions
    for (width in c(1, 5)) {
      result <- generate_window_fixture(fixture, duration, width)
      ## the expected sums, built from which occasion each day falls in,
      ## independently of the windows matrix_generator() builds
      occasion <- (seq_len(duration) - 1) %/% width + 1
      expected <- as.numeric(tapply(seq_len(duration), occasion, sum))
      # one column per occasion, numbered from 1
      expect_equal(ncol(result$detection_matrix), length(expected))
      expect_equal(colnames(result$detection_matrix), as.character(seq_along(expected)))
      # and each occasion holds exactly its own days
      expect_equal(as.numeric(result$detection_matrix["Cell_A", ]), expected)
      expect_equal(as.numeric(result$detection_matrix["Cell_B", ]), expected)
    } # end per width
  } # end per duration
})

test_that("boundary detections do not populate the preceding occasion", {
  # detections on day 6, the first day of occasion 2, and day 10, its last
  fixture <- window_fixture(10, detected_days = c(6, 10))
  sum_result <- generate_window_fixture(fixture, 10, 5)
  max_result <- generate_window_fixture(fixture, 10, 5, individuals = "max")
  occupancy <- generate_window_fixture(fixture, 10, 5, type = "occupancy")
  # both fall in occasion 2 only, whichever way the occasion is summarised
  expect_equal(as.numeric(sum_result$detection_matrix["Cell_A", ]), c(0, 16))
  expect_equal(as.numeric(max_result$detection_matrix["Cell_A", ]), c(0, 10))
  expect_equal(as.numeric(occupancy$detection_matrix["Cell_A", ]), c(0, 1))
})

test_that("effort and detections use the same complete and partial occasions", {
  # for survey lengths that do and do not divide evenly into occasions
  for (duration in c(3, 10, 91, 94)) {
    fixture <- window_fixture(duration)
    result <- generate_window_fixture(
      fixture, duration, 5, obs_covs = "numberDeploymentsActiveAtDate"
    )
    effort <- result$observation_level_covariates$numberDeploymentsActiveAtDate
    # the expected effort: the daily values, summed over each occasion's days
    occasion <- (seq_len(duration) - 1) %/% 5 + 1
    expected <- as.numeric(tapply(rep(c(1, 2), length.out = duration), occasion, sum))
    # the effort matrix lines up with the detection matrix
    expect_equal(dim(effort), dim(result$detection_matrix))
    expect_equal(dimnames(effort), dimnames(result$detection_matrix))
    # and holds the same days in each occasion
    expect_equal(as.numeric(effort["Cell_A", ]), expected)
    expect_equal(as.numeric(effort["Cell_B", ]), expected)
  } # end per duration
})

test_that("character covariates keep their existing modal aggregation", {
  # a covariate that alternates low and high by day
  fixture <- window_fixture(10)
  fixture$obs$cameraHeight <- rep(rep(c("low", "high"), 5), 2)
  result <- generate_window_fixture(fixture, 10, 5, obs_covs = "cameraHeight")
  height <- result$observation_level_covariates$cameraHeight
  # it lines up with the detection matrix
  expect_equal(dimnames(height), dimnames(result$detection_matrix))
  # and each occasion takes its most common value
  expect_equal(as.character(height["Cell_A", ]), c("low", "high"))
  expect_equal(as.character(height["Cell_B", ]), c("low", "high"))
})

test_that("unobserved occasions retain NA detections and zero numeric effort", {
  fixture <- window_fixture(10)
  ## Cell B's survey ends after the first occasion; Cell A keeps the full duration
  # drop Cell B's records after day 5
  fixture$obs <- fixture$obs[!(fixture$obs$cellID_10km == "Cell_B" &
    fixture$obs$eventStart > fixture$covs$samplingStart[2] + 4 * 86400), ]
  # and end its sampling on day 5
  fixture$covs$samplingEnd[2] <- fixture$covs$samplingStart[2] + 4 * 86400
  # for both kinds of model
  for (model in c("abundance", "occupancy")) {
    result <- generate_window_fixture(
      fixture, 10, 5, type = model, obs_covs = "numberDeploymentsActiveAtDate"
    )
    effort <- result$observation_level_covariates$numberDeploymentsActiveAtDate
    # occasion 2 is NA where Cell B was not sampling
    expect_true(is.na(result$detection_matrix["Cell_B", 2]))
    # with zero effort
    expect_equal(as.numeric(effort["Cell_B", ]), c(7, 0))
    expect_equal(dimnames(effort), dimnames(result$detection_matrix))
  } # end per model
})
