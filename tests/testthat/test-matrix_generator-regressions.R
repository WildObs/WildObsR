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

test_that("matrix_generator warns about, and keeps, NA values in a site covariate", {
  covs <- reg_covs()
  covs$Avg_FLII_3km2[2] <- NA
  expect_warning(res <- run_mg(covs = covs), "NA values in the site covariate\\(s\\) Avg_FLII_3km2")
  # the NA stays, for the modelling package to handle
  expect_true(anyNA(res$Species_A$site_level_covariates$Avg_FLII_3km2))
})

test_that("matrix_generator warns about NA values in an observation covariate", {
  obs <- reg_obs()
  obs$baitUse[1] <- NA
  expect_warning(run_mg(obs = obs, obs_covs = "baitUse"),
                 "NA values in the observation covariate\\(s\\) baitUse")
})

test_that("matrix_generator stops on an observation covariate that is not a column", {
  expect_error(run_mg(obs_covs = "cameraHeight"), "could not find the observation covariate")
})

test_that("matrix_generator converts text date columns, and stops on values it cannot read", {
  # eventEnd as readable text, eventStart still a date-time
  obs <- reg_obs()
  obs$eventEnd <- format(obs$eventEnd, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  expect_message(run_mg(obs = obs), "converted eventEnd in obs from text to date-times")
  # a value that is not a date stops, naming it
  covs <- reg_covs()
  covs$samplingEnd <- c("2023-02-23 18:00:00", "not a date")
  expect_error(run_mg(covs = covs), "could not read 1 value\\(s\\) of samplingEnd in covs.*not a date")
})


#
##
### Matrix building ----

## one cell per landscape, each with its own sampling length and records on chosen days
day_fixture <- function(days_a = 10, days_b = 30, rec_a = 1, rec_b = 1,
                        sp_a = "Species A", sp_b = "Species B") {
  # noon local time on day n
  at <- function(n) as.POSIXct("2023-02-01 12:00:00", tz = "Australia/Brisbane") + (n - 1) * 86400
  # the records
  obs <- data.frame(
    cellID_10km = c(rep("Cell_A", length(rec_a)), rep("Cell_B", length(rec_b))),
    scientificName = c(rep(sp_a, length(rec_a)), rep(sp_b, length(rec_b))),
    totalIndividuals = 1,
    eventStart = c(at(rec_a), at(rec_b)),
    eventEnd = c(at(rec_a), at(rec_b)) + 60
  )
  # each cell's sampling period, from the morning of day 1
  covs <- data.frame(
    cellID_10km = c("Cell_A", "Cell_B"),
    locationName = c("Site1", "Site2"),
    samplingStart = at(1) - 4 * 3600,
    samplingEnd = c(at(days_a), at(days_b)),
    Avg_latitude = -17.5,
    Avg_longitude = 145.5,
    elevation = c(10, 20)
  )
  list(obs = obs, covs = covs)
} # end fixture

test_that("matrix_generator marks days as sampled up to the end of sampling, not the last record", {
  # Cell_A ran 10 days with its only record on day 1
  f <- day_fixture(days_a = 10, rec_a = 1)
  y <- run_mg(obs = f$obs, covs = f$covs, site_covs = "elevation", dur = 10, w = 5,
              scientificNames = "Species A")$Species_A$detection_matrix
  # so its second occasion is a sampled zero, not NA
  expect_equal(as.numeric(y["Cell_A", ]), c(1, 0))
})

test_that("matrix_generator does not carry a shortened dur from one species to the next", {
  ## the first species is only in the 10-day landscape, the second only in the 30-day one
  f <- day_fixture(days_a = 10, days_b = 30)
  res <- run_mg(obs = f$obs, covs = f$covs, site_covs = "elevation", dur = 30, w = 5,
                all_locationNames = FALSE, scientificNames = c("Species A", "Species B"))
  # the first is cut to its 10 days
  expect_equal(ncol(res$Species_A$detection_matrix), 2)
  # but the second keeps all 30
  expect_equal(ncol(res$Species_B$detection_matrix), 6)
})

test_that("matrix_generator with all_locationNames = FALSE drops undetected landscapes (#105)", {
  f <- day_fixture()
  res <- run_mg(obs = f$obs, covs = f$covs, site_covs = "elevation", dur = 30, w = 5,
                all_locationNames = FALSE, scientificNames = "Species A")$Species_A
  # only the landscape Species A was seen in, in both the detections and the site covariates
  expect_equal(rownames(res$detection_matrix), "Cell_A")
  expect_equal(res$site_level_covariates$cellID_10km, "Cell_A")
})

test_that("matrix_generator keeps site covariates in the same order as the matrix rows", {
  # covariates in a different order from the observations
  f <- day_fixture()
  f$covs <- f$covs[2:1, ]
  res <- run_mg(obs = f$obs, covs = f$covs, site_covs = "elevation", dur = 30, w = 5,
                scientificNames = "Species A")$Species_A
  expect_equal(res$site_level_covariates$cellID_10km, rownames(res$detection_matrix))
})

test_that("matrix_generator still returns a species whose observation covariates do not vary", {
  # a covariate with one value on every record
  obs <- reg_obs()
  obs$featureType <- "roadDirt"
  expect_warning(res <- run_mg(obs = obs, obs_covs = "featureType",
                               scientificNames = c("Species A", "Species B")),
                 "no variation in the observation-level covariate")
  # both species come back, with no observation covariates
  expect_named(res, c("Species_A", "Species_B"))
  expect_length(res$Species_A$observation_level_covariates, 0)
})

test_that("matrix_generator places a record on its local day, not its UTC day", {
  ## 6 am in Brisbane on day 1 is still the previous day in UTC, before sampling began
  f <- day_fixture(days_a = 10, rec_a = 1)
  f$obs$eventStart[1] <- as.POSIXct("2023-02-01 06:00:00", tz = "Australia/Brisbane")
  f$obs$eventEnd[1] <- f$obs$eventStart[1] + 60
  expect_no_warning(y <- run_mg(obs = f$obs, covs = f$covs, site_covs = "elevation", dur = 10,
                                w = 5, scientificNames = "Species A")$Species_A$detection_matrix)
  # so the record lands in the first occasion
  expect_equal(as.numeric(y["Cell_A", ]), c(1, 0))
})

test_that("matrix_generator keeps a sampling unit with no observations, as zeros", {
  # a third cell that ran without recording anything
  f <- day_fixture()
  f$covs <- rbind(f$covs, transform(f$covs[1, ], cellID_10km = "Cell_C", elevation = 30))
  expect_message(res <- run_mg(obs = f$obs, covs = f$covs, site_covs = "elevation", dur = 10,
                               w = 5, scientificNames = "Species A"),
                 "1 sampling unit\\(s\\) have no observations")
  # in the matrix, sampled with no detections
  expect_equal(as.numeric(res$Species_A$detection_matrix["Cell_C", ]), c(0, 0))
})

test_that("matrix_generator keeps site covariates that do not vary", {
  # a habitat and a forest integrity recorded as the same everywhere
  covs <- reg_covs()
  covs$mode_habitat <- "forest"
  covs$Avg_FLII_3km2 <- 80
  sc <- run_mg(covs = covs)$Species_A$site_level_covariates
  # both are kept, the numeric one standardised to 0
  expect_equal(sc$mode_habitat, c("forest", "forest"))
  expect_equal(sc$Avg_FLII_3km2, c(0, 0))
})

test_that("matrix_generator keeps logical site covariates", {
  # a modal bait use, as the resample now returns it
  covs <- reg_covs()
  covs$mode_baitUse <- c(TRUE, FALSE)
  sc <- run_mg(covs = covs, site_covs = c("mode_habitat", "mode_baitUse"))$Species_A$site_level_covariates
  expect_equal(sc$mode_baitUse, c(TRUE, FALSE))
})

test_that("matrix_generator uses UTC days when sampling units span time zones", {
  # one cell near Cairns and one near Perth
  covs <- reg_covs()
  covs$Avg_latitude <- c(-17.5, -31.9)
  covs$Avg_longitude <- c(145.5, 115.9)
  expect_message(run_mg(covs = covs), "span 2 time zones \\(Australia/Brisbane, Australia/Perth\\)")
})

test_that("matrix_generator works with Camtrap DP deployments, using their own coordinates", {
  ## deploymentID rows and deployment dates, with latitude and longitude rather than averages
  f <- day_fixture()
  names(f$obs)[1] <- "deploymentID"
  names(f$covs)[c(1, 3:6)] <- c("deploymentID", "deploymentStart", "deploymentEnd", "latitude", "longitude")
  f$obs$count <- f$obs$totalIndividuals
  f$obs$totalIndividuals <- NULL
  y <- run_mg(obs = f$obs, covs = f$covs, site_covs = "elevation", dur = 10, w = 5,
              scientificNames = "Species A")$Species_A$detection_matrix
  expect_equal(rownames(y), c("Cell_A", "Cell_B"))
})

test_that("matrix_generator warns that numberDeploymentsActiveAtDate is not effort", {
  expect_warning(run_mg(obs_covs = "numberDeploymentsActiveAtDate"),
                 "counts only the deployments with a record that day")
})
