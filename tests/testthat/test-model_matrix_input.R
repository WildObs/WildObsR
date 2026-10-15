## Tests for model_matrix_input() ----

## A small simulated survey: 30 stations, each with one camera running 30 days, two species,
## and a covariate table deliberately in a different order to the stations. The format
## tests compare the structure of each output with the example data the modelling packages
## ship; the fit tests check the packages accept the output (tiny runs, skipped on CRAN).

## the simulated survey, the same every time
simulated_survey <- function(n_site = 30) {
  set.seed(42)
  # one camera per station, started over the first three days, each running 30 days
  start <- as.POSIXct("2024-03-01 00:00", tz = "UTC") + sample(0:2, n_site, replace = TRUE) * 86400
  deps <- data.frame(deploymentID = sprintf("cam%02d", seq_len(n_site)),
                     locationID = sprintf("s%02d", seq_len(n_site)),
                     deploymentStart = start, deploymentEnd = start + 30 * 86400,
                     baitUse = rep(c(TRUE, FALSE), length.out = n_site))
  # random detections of two species
  n_obs <- 120
  cam <- sample(deps$deploymentID, n_obs, replace = TRUE)
  obs <- data.frame(deploymentID = cam, eventID = sprintf("e%03d", seq_len(n_obs)),
                    eventStart = deps$deploymentStart[match(cam, deps$deploymentID)] +
                      runif(n_obs, 0, 29.9 * 86400),
                    scientificName = sample(c("Sus scrofa", "Thylogale stigmatica"), n_obs,
                                            replace = TRUE),
                    count = sample(1:3, n_obs, replace = TRUE))
  # elevation per station, listed in reverse order
  covs <- data.frame(locationID = rev(deps$locationID), elevation = rnorm(n_site, 700, 100))
  # projected coordinates per station
  coords <- data.frame(locationID = deps$locationID, X = runif(n_site, 0, 5000),
                       Y = runif(n_site, 0, 5000))
  list(deps = deps, obs = obs, covs = covs, coords = coords)
} # end simulated survey

## effort and detections for the simulated survey, in weekly occasions
survey_inputs <- function(type = "occupancy", species = c("Sus scrofa", "Thylogale stigmatica")) {
  x <- simulated_survey()
  eff <- effort_matrix(x$deps, site_col = "locationID", w = 7, split_by = "baitUse")
  y <- detection_matrix(x$obs, eff, species, type = type)
  c(x, list(eff = eff, y = y))
} # end survey inputs


#
##
### Matching sites and covariates ----

test_that("model_matrix_input matches site covariates by name, not by row order", {
  x <- survey_inputs()
  inp <- model_matrix_input(x$y, x$eff, site_covs = x$covs, output = "array")

  # the covariates follow the detections' sites
  expect_identical(rownames(inp$site_covs), dimnames(x$y)[[2]])
  expect_equal(inp$site_covs["s01", "elevation"],
               x$covs$elevation[x$covs$locationID == "s01"])
})

test_that("model_matrix_input stops when a site has no covariates", {
  x <- survey_inputs()
  expect_error(model_matrix_input(x$y, x$eff, site_covs = x$covs[-1, ], output = "array"),
               "1 site\\(s\\) have no row in site_covs")
})

test_that("model_matrix_input adds effort and the share under each split level", {
  x <- survey_inputs()
  inp <- model_matrix_input(x$y, x$eff, output = "array")

  # camera-days, and the share of baited and unbaited effort
  expect_setequal(names(inp$obs_covs), c("effort", "baitUse_FALSE", "baitUse_TRUE"))
  # one camera per station, so each station is wholly baited or not
  expect_true(all(inp$obs_covs$baitUse_TRUE + inp$obs_covs$baitUse_FALSE == 1, na.rm = TRUE))
  # covariates are NA exactly where the detections are
  expect_identical(is.na(inp$obs_covs$effort), is.na(x$y[1, , ]))
})

test_that("model_matrix_input checks the shape of the user's observation covariates", {
  x <- survey_inputs()
  # a covariate with one occasion too few
  bad <- list(rain = matrix(0, nrow = 30, ncol = ncol(x$eff$effort_total) - 1))
  expect_error(model_matrix_input(x$y, x$eff, obs_covs = bad, output = "array"),
               "obs_covs\\$rain is 30 x")
})


#
##
### Formats, compared with the packages' own example data ----

test_that("spOccupancy output has the structure of spOccupancy's example data", {
  skip_if_not_installed("spOccupancy")
  x <- survey_inputs()
  inp <- model_matrix_input(x$y, x$eff, site_covs = x$covs, coords = x$coords,
                            output = "spOccupancy")
  ref <- get(utils::data("hbef2015", package = "spOccupancy", envir = environment()))

  # the same elements, a species x site x occasion y, and a site x 2 coordinate matrix
  expect_setequal(names(inp), names(ref))
  expect_length(dim(inp$y), length(dim(ref$y)))
  expect_equal(dim(inp$coords), c(30, 2))
  # detection covariates are site x occasion matrices
  expect_equal(dim(inp$det.covs$effort), dim(inp$y)[2:3])
})

test_that("spAbundance output has the structure of spAbundance's example data", {
  skip_if_not_installed("spAbundance")
  x <- survey_inputs(type = "abundance")
  inp <- model_matrix_input(x$y, x$eff, site_covs = x$covs, coords = x$coords,
                            output = "spAbundance")
  ref <- get(utils::data("hbefCount2015", package = "spAbundance", envir = environment()))

  expect_setequal(names(inp), names(ref))
  expect_s3_class(inp$abund.covs, "data.frame")
  expect_length(dim(inp$y), length(dim(ref$y)))
})

test_that("multi-season output groups occasions by season", {
  x <- simulated_survey()
  # calendar-aligned, so occasion k covers the same dates at every station
  eff <- effort_matrix(x$deps, site_col = "locationID", w = 7, align = "calendar")
  y <- detection_matrix(x$obs, eff, c("Sus scrofa", "Thylogale stigmatica"))
  seasons <- rep(c("a", "b"), length.out = ncol(eff$effort_total))
  seasons <- sort(seasons)

  inp <- model_matrix_input(y, eff, site_covs = x$covs, output = "spOccupancy", seasons = seasons)

  # species x site x season x occasion, as tMsPGOcc() expects
  expect_length(dim(inp$y), 4)
  expect_equal(dim(inp$y)[3], 2)
  # site-aligned effort cannot be split into seasons
  eff_site <- effort_matrix(x$deps, site_col = "locationID", w = 7)
  y_site <- detection_matrix(x$obs, eff_site, "Sus scrofa")
  expect_error(model_matrix_input(y_site, eff_site, seasons = seq_len(ncol(eff_site$effort_total)),
                                  output = "spOccupancy"),
               "align = \"calendar\"")
})


#
##
### The packages accept the output ----

test_that("unmarked fits single-species occupancy and N-mixture models to the output", {
  skip_on_cran()
  skip_if_not_installed("unmarked")
  occ <- survey_inputs(species = "Sus scrofa")
  umf <- model_matrix_input(occ$y, occ$eff, site_covs = occ$covs, output = "unmarked")
  expect_s4_class(umf, "unmarkedFrameOccu")
  expect_no_error(unmarked::occu(~ scale(effort) ~ scale(elevation), data = umf))

  ab <- survey_inputs(type = "abundance", species = "Sus scrofa")
  umf_n <- model_matrix_input(ab$y, ab$eff, site_covs = ab$covs, output = "unmarked")
  expect_s4_class(umf_n, "unmarkedFramePCount")
  expect_no_error(unmarked::pcount(~ effort ~ 1, data = umf_n, K = 30))
})

test_that("unmarked fits a community occupancy model to multi-species output", {
  skip_on_cran()
  skip_if_not_installed("unmarked")
  x <- survey_inputs()
  umf <- model_matrix_input(x$y, x$eff, site_covs = x$covs, output = "unmarked")
  expect_s4_class(umf, "unmarkedFrameOccuComm")
  expect_no_error(suppressWarnings(unmarked::occuComm(~ 1 ~ 1, data = umf)))
})

test_that("spOccupancy and spAbundance fit multi-species models to the output", {
  skip_on_cran()
  skip_if_not_installed("spOccupancy")
  skip_if_not_installed("spAbundance")
  x <- survey_inputs()
  occ <- model_matrix_input(x$y, x$eff, site_covs = x$covs, output = "spOccupancy")
  expect_no_error(utils::capture.output(
    spOccupancy::msPGOcc(occ.formula = ~ 1, det.formula = ~ scale(effort), data = occ,
                         n.samples = 200, n.burn = 100, n.thin = 1, n.chains = 1,
                         verbose = FALSE)))

  ab <- survey_inputs(type = "abundance")
  abund <- model_matrix_input(ab$y, ab$eff, site_covs = ab$covs, output = "spAbundance")
  expect_no_error(utils::capture.output(
    spAbundance::msNMix(abund.formula = ~ 1, det.formula = ~ scale(effort), data = abund,
                        n.batch = 4, batch.length = 25, n.burn = 50, n.thin = 1,
                        n.chains = 1, verbose = FALSE)))
})
