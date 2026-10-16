### Regression tests for matrix_generator() defects found in review ----

## the small two-cell fixture from test-matrix_generator.R, built here so this file stands alone
reg_obs <- function() {
  data.frame(
    cellID_10km = c("Cell_A", "Cell_A", "Cell_A", "Cell_B", "Cell_B"),
    scientificName = c("Species A", "Species A", "Species B", "Species A", "Species B"),
    totalIndividuals = c(1, 2, 1, 3, 1),
    eventStart = as.POSIXct(c("2023-02-01 10:00:00", "2023-02-01 12:00:00", "2023-02-06 14:00:00",
                              "2023-02-09 12:00:00", "2023-02-22 09:00:00"), tz = "Australia/Brisbane"),
    eventEnd = as.POSIXct(c("2023-02-01 10:05:00", "2023-02-01 12:05:00", "2023-02-06 14:05:00",
                            "2023-02-09 12:05:00", "2023-02-22 09:05:00"), tz = "Australia/Brisbane"),
    baitUse = c("none", "none", "bait", "bait", "bait"),
    numberDeploymentsActiveAtDate = c(1, 1, 1, 2, 1)
  )
} # end observations

reg_covs <- function() {
  data.frame(
    cellID_10km = c("Cell_A", "Cell_B"),
    locationName = c("Site1", "Site2"),
    samplingStart = as.POSIXct(c("2023-02-01 08:00:00", "2023-02-02 08:00:00"), tz = "Australia/Brisbane"),
    samplingEnd = as.POSIXct(c("2023-02-23 18:00:00", "2023-02-24 18:00:00"), tz = "Australia/Brisbane"),
    Avg_latitude = c(-17.5, -17.6),
    Avg_longitude = c(145.5, 145.6),
    mode_habitat = c("forest", "grassland"),
    Avg_FLII_3km2 = c(85.2, 72.1)
  )
} # end covariates

## matrix_generator() with the fixture's usual settings, any of which can be overridden
run_mg <- function(obs = reg_obs(), covs = reg_covs(), ...) {
  # the usual settings
  args <- list(obs = obs, covs = covs, dur = 30, w = 5, site_covs = c("mode_habitat", "Avg_FLII_3km2"),
               obs_covs = character(0), all_locationNames = TRUE, scientificNames = "Species A",
               type = "occupancy", individuals = "sum")
  # replaced by whatever the test passes
  args[names(list(...))] <- list(...)
  do.call(matrix_generator, args)
} # end helper


#
##
### Input checks that used to pass anything ----

test_that("matrix_generator stops on NA values in a site covariate", {
  covs <- reg_covs()
  covs$Avg_FLII_3km2[2] <- NA
  expect_error(run_mg(covs = covs), "NA values in the site covariate\\(s\\) Avg_FLII_3km2")
})

test_that("matrix_generator stops on NA values in an observation covariate", {
  obs <- reg_obs()
  obs$baitUse[1] <- NA
  expect_error(run_mg(obs = obs, obs_covs = "baitUse"),
               "NA values in the observation covariate\\(s\\) baitUse")
})

test_that("matrix_generator stops on an observation covariate that is not a column", {
  expect_error(run_mg(obs_covs = "cameraHeight"), "could not find the observation covariate")
})

test_that("matrix_generator stops when only one of the two date columns is a date-time", {
  # eventEnd as text, eventStart still a date-time
  obs <- reg_obs()
  obs$eventEnd <- as.character(obs$eventEnd)
  expect_error(run_mg(obs = obs), "eventStart and eventEnd in obs as POSIXct")
  # and the same for the sampling end
  covs <- reg_covs()
  covs$samplingEnd <- as.character(covs$samplingEnd)
  expect_error(run_mg(covs = covs), "samplingStart and samplingEnd in covs as POSIXct")
})
