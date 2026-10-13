## Tests for recombine_deployment_groups() ----

## Build a small data package with the shape wildobs_dp_download() returns.
## Station S1 ran camera A for three 100-day pieces in two single-season surveys
## that belong to one multi-season group, and station S2 held two cameras
## (_Cam1 and _Cam2) side by side for one survey.
make_recombine_dp <- function(with_media = TRUE, with_covariates = TRUE) {

  ## start and end of each piece, in UTC to avoid timezone surprises
  t0 <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  day <- 86400

  ## deployments: S1 has three consecutive pieces, S2 has two cameras
  deps <- data.frame(
    deploymentID = c("S1_a", "S1_b", "S1_c", "S2_Cam1", "S2_Cam2"),
    locationID = c("S1", "S1", "S1", "S2", "S2"),
    deploymentStart = t0 + day * c(0, 100, 210, 0, 0),
    deploymentEnd = t0 + day * c(100, 200, 300, 50, 50),
    deploymentGroups = c("survey1", "survey1", "survey2", "survey1", "survey1"),
    multiSeason_deploymentGroup = c("ms_S1", "ms_S1", "ms_S1", "ms_S2", "ms_S2"),
    stringsAsFactors = FALSE
  )

  ## covariates: one row per piece, with a numeric value that differs by piece
  covs <- data.frame(
    deploymentID = deps$deploymentID,
    deploymentGroups = deps$deploymentGroups,
    mean_monthly_precipitation = c(10, 20, 30, 5, 5),
    notes = c("wet", "dry", "dry", "same", "same"),
    stringsAsFactors = FALSE
  )

  ## observations: one animal record per piece, plus one extra in S1_b
  obs <- data.frame(
    observationID = paste0("obs", 1:6),
    deploymentID = c("S1_a", "S1_b", "S1_b", "S1_c", "S2_Cam1", "S2_Cam2"),
    scientificName = "Macropus rufus",
    stringsAsFactors = FALSE
  )

  ## bundle into a frictionless package
  dp <- frictionless::create_package()
  dp$id <- "test_dp"
  # temporal metadata as WildObs stores it: package extent plus one block per survey
  dp$temporal <- list(
    start = "2020-01-01",
    end = "2020-10-27",
    timeZone = "Australia/Brisbane",
    survey1 = list(start = "2020-01-01", end = "2020-07-19"),
    survey2 = list(start = "2020-07-29", end = "2020-10-27")
  )
  dp <- frictionless::add_resource(dp, "deployments", data = deps)
  if (with_covariates) dp <- frictionless::add_resource(dp, "covariates", data = covs)
  dp <- frictionless::add_resource(dp, "observations", data = obs)
  if (with_media) {
    media <- data.frame(mediaID = paste0("m", 1:3),
                        deploymentID = c("S1_a", "S1_c", "S2_Cam1"),
                        stringsAsFactors = FALSE)
    dp <- frictionless::add_resource(dp, "media", data = media)
  } # end media condition

  return(dp)
} # end make_recombine_dp


test_that("recombine_deployment_groups chains consecutive pieces into one deployment", {
  out <- recombine_deployment_groups(make_recombine_dp())
  deps <- frictionless::read_resource(out, "deployments")

  # S1 collapses three pieces to one, S2 keeps its two cameras apart
  expect_equal(nrow(deps), 3)
  expect_setequal(deps$deploymentID, c("S1_ms1", "S2_ms1_Cam1", "S2_ms1_Cam2"))

  # the long deployment spans first start to last end
  s1 <- deps[deps$deploymentID == "S1_ms1", ]
  expect_equal(as.numeric(difftime(s1$deploymentEnd, s1$deploymentStart, units = "days")), 300)

  # survey labels now carry the multi-season group
  expect_equal(s1$deploymentGroups, "ms_S1")
})

test_that("recombine_deployment_groups writes a crosswalk with every original piece", {
  dp <- make_recombine_dp()
  out <- recombine_deployment_groups(dp)
  cw <- frictionless::read_resource(out, "deployment_crosswalk")

  expect_equal(nrow(cw), 5)
  expect_setequal(names(cw), c("original_deploymentID", "deploymentID",
                               "original_deploymentGroups", "pieceStart", "pieceEnd"))
  expect_setequal(cw$original_deploymentID,
                  frictionless::read_resource(dp, "deployments")$deploymentID)

  # effort summed from the pieces excludes the 10 day gap in S1
  s1 <- cw[cw$deploymentID == "S1_ms1", ]
  expect_equal(sum(as.numeric(difftime(s1$pieceEnd, s1$pieceStart, units = "days"))), 290)
})

test_that("recombine_deployment_groups remaps deploymentID without losing rows", {
  out <- recombine_deployment_groups(make_recombine_dp())
  deps <- frictionless::read_resource(out, "deployments")
  obs <- frictionless::read_resource(out, "observations")
  media <- frictionless::read_resource(out, "media")

  # every record is kept and still joins to a deployment
  expect_equal(nrow(obs), 6)
  expect_equal(nrow(media), 3)
  expect_true(all(obs$deploymentID %in% deps$deploymentID))
  expect_true(all(media$deploymentID %in% deps$deploymentID))

  # the three S1 records now share the long deployment
  expect_equal(sum(obs$deploymentID == "S1_ms1"), 4)

  # observationID is untouched
  expect_equal(obs$observationID, paste0("obs", 1:6))
})

test_that("recombine_deployment_groups weights numeric covariates by duration", {
  out <- recombine_deployment_groups(make_recombine_dp())
  covs <- frictionless::read_resource(out, "covariates")

  expect_equal(nrow(covs), 3)

  # pieces ran 100, 100 and 90 days with values 10, 20 and 30
  s1 <- covs[covs$deploymentID == "S1_ms1", ]
  expect_equal(s1$mean_monthly_precipitation,
               (10 * 100 + 20 * 100 + 30 * 90) / 290)

  # differing text is joined, identical text is kept
  expect_equal(s1$notes, "dry | wet")
  expect_equal(covs$notes[covs$deploymentID == "S2_ms1_Cam1"], "same")
})

test_that("recombine_deployment_groups works without media or covariates", {
  dp <- make_recombine_dp(with_media = FALSE, with_covariates = FALSE)
  out <- recombine_deployment_groups(dp)

  expect_false("media" %in% frictionless::resource_names(out))
  expect_false("covariates" %in% frictionless::resource_names(out))
  expect_true("deployment_crosswalk" %in% frictionless::resource_names(out))
})

test_that("recombine_deployment_groups splits overlapping pieces and warns", {
  dp <- make_recombine_dp()
  deps <- frictionless::read_resource(dp, "deployments")
  # make S1_b start before S1_a has finished
  deps$deploymentStart[deps$deploymentID == "S1_b"] <-
    deps$deploymentStart[deps$deploymentID == "S1_b"] - 86400 * 20
  dp <- .replace_resource(dp, "deployments", deps)

  expect_warning(out <- recombine_deployment_groups(dp), "overlapping an earlier piece")

  new_ids <- frictionless::read_resource(out, "deployments")$deploymentID
  expect_true(any(grepl("_part2$", new_ids)))
})

## add a fourth S1 piece in the same multi-season group, starting a year after S1_c ended
add_late_piece <- function(dp) {
  deps <- frictionless::read_resource(dp, "deployments")
  late <- deps[deps$deploymentID == "S1_c", ]
  late$deploymentID <- "S1_d"
  late$deploymentStart <- late$deploymentEnd + 86400 * 365
  late$deploymentEnd <- late$deploymentStart + 86400 * 60
  .replace_resource(dp, "deployments", rbind(deps, late))
} # end add_late_piece

test_that("recombine_deployment_groups splits runs at gaps longer than max_gap_days", {
  dp <- add_late_piece(make_recombine_dp(with_covariates = FALSE))

  # a long gap is expected, so it splits without warning
  expect_no_warning(out <- recombine_deployment_groups(dp))
  cw <- frictionless::read_resource(out, "deployment_crosswalk")

  # the first three pieces stay together, the late one becomes part 2
  expect_equal(unique(cw$deploymentID[cw$original_deploymentID %in% c("S1_a", "S1_b", "S1_c")]),
               "S1_ms1")
  expect_equal(cw$deploymentID[cw$original_deploymentID == "S1_d"], "S1_ms1_part2")

  # the 10 day gap between S1_b and S1_c is under the default, so it never splits
  expect_false(any(grepl("_part3$", cw$deploymentID)))
})

test_that("recombine_deployment_groups keeps runs together under a larger max_gap_days", {
  dp <- add_late_piece(make_recombine_dp(with_covariates = FALSE))
  out <- recombine_deployment_groups(dp, max_gap_days = 400)
  cw <- frictionless::read_resource(out, "deployment_crosswalk")

  # all four S1 pieces form one run
  expect_equal(unique(cw$deploymentID[grepl("^S1_", cw$original_deploymentID)]), "S1_ms1")
})

test_that("recombine_deployment_groups errors on an invalid max_gap_days", {
  dp <- make_recombine_dp()
  expect_error(recombine_deployment_groups(dp, max_gap_days = -1), "invalid max_gap_days")
  expect_error(recombine_deployment_groups(dp, max_gap_days = c(10, 20)), "invalid max_gap_days")
  expect_error(recombine_deployment_groups(dp, max_gap_days = "30"), "invalid max_gap_days")
})

test_that("recombine_deployment_groups numbers multi-season groups in time order", {
  dp <- make_recombine_dp()
  deps <- frictionless::read_resource(dp, "deployments")
  # give S1_c its own later multi-season group
  deps$multiSeason_deploymentGroup[deps$deploymentID == "S1_c"] <- "ms_S1_later"
  dp <- .replace_resource(dp, "deployments", deps)

  out <- recombine_deployment_groups(dp)
  new_ids <- frictionless::read_resource(out, "deployments")$deploymentID

  expect_true(all(c("S1_ms1", "S1_ms2") %in% new_ids))
  cw <- frictionless::read_resource(out, "deployment_crosswalk")
  expect_equal(cw$deploymentID[cw$original_deploymentID == "S1_c"], "S1_ms2")
})

test_that("recombine_deployment_groups errors on missing resources and columns", {
  # no deployments resource
  empty <- frictionless::create_package()
  empty$id <- "empty_dp"
  expect_error(recombine_deployment_groups(empty), "no deployments resource")

  # a required column is missing; build a fresh package, since the old schema
  # would still declare the dropped column
  deps <- frictionless::read_resource(make_recombine_dp(), "deployments")
  deps$multiSeason_deploymentGroup <- NULL
  dp <- frictionless::create_package()
  dp$id <- "no_ms_dp"
  dp <- frictionless::add_resource(dp, "deployments", data = deps)
  expect_error(recombine_deployment_groups(dp), "multiSeason_deploymentGroup")
})

test_that("recombine_deployment_groups rebuilds temporal blocks per multi-season group", {
  out <- recombine_deployment_groups(make_recombine_dp())
  tmp <- out$temporal

  # package-level extent and timeZone are untouched
  expect_equal(tmp$start, "2020-01-01")
  expect_equal(tmp$end, "2020-10-27")
  expect_equal(tmp$timeZone, "Australia/Brisbane")

  # survey blocks are replaced by one block per multi-season group
  expect_setequal(names(tmp), c("start", "end", "timeZone", "ms_S1", "ms_S2"))

  # ms_S1 absorbs survey1 and survey2, so spans both
  expect_equal(tmp$ms_S1, list(start = "2020-01-01", end = "2020-10-27"))
  # ms_S2 absorbs survey1 only
  expect_equal(tmp$ms_S2, list(start = "2020-01-01", end = "2020-07-19"))
})

test_that("recombine_deployment_groups warns when a survey has no temporal block", {
  dp <- make_recombine_dp()
  dp$temporal$survey2 <- NULL

  expect_warning(out <- recombine_deployment_groups(dp), "no temporal block")
  # ms_S1 falls back to survey1's dates only
  expect_equal(out$temporal$ms_S1$end, "2020-07-19")
})

test_that("recombine_deployment_groups errors when observations have orphan deploymentIDs", {
  dp <- make_recombine_dp()
  obs <- frictionless::read_resource(dp, "observations")
  obs$deploymentID[1] <- "not_a_deployment"
  dp <- .replace_resource(dp, "observations", obs)

  expect_error(recombine_deployment_groups(dp), "missing from deployments")
})
