## Tests for as_camtrapdp() ----

## Everything here runs offline against a small real package: three deployments of an
## open WildObs project, built by fixtures/make_as_camtrapdp_fixture.R. It carries every
## extension the function handles, so each rule is exercised on real data.

## a fresh, empty folder inside the session temp folder R deletes on exit
withr_free_tempdir <- function() {
  dir <- tempfile("as_camtrapdp_test_")
  dir.create(dir)
  return(dir)
} # end temp folder helper

## the trimmed WildObs package
fixture <- function() {
  return(readRDS(test_path("fixtures", "as_camtrapdp_dp.rds")))
} # end fixture

## convert quietly, since most tests are about the result rather than the warnings
convert <- function(dp = fixture(), ...) {
  return(suppressWarnings(as_camtrapdp(dp, validate = FALSE, ...)))
} # end convert helper

## the in-memory table of one resource
table_of <- function(pkg, name) {
  i <- which(vapply(pkg$resources, function(r) r$name, character(1)) == name)
  return(pkg$resources[[i]]$data)
} # end table helper

## the field names a vendored Camtrap DP table schema defines, in order
target_fields <- function(name, version = "1.0.2") {
  schema <- jsonlite::fromJSON(
    system.file("profiles", sprintf("camtrap-dp-%s-table-schema-%s.json", name, version), package = "WildObsR"),
    simplifyVector = FALSE)
  return(vapply(schema$fields, function(f) f$name, character(1)))
} # end target fields helper


### Descriptor ----

test_that("as_camtrapdp removes every WildObs descriptor extension at any depth", {
  pkg <- convert()$package

  expect_false(any(c("WildObsMetadata", "versionControlWildObs") %in% names(pkg)))
  expect_null(pkg$project[["DPID"]])
  expect_false(any(vapply(pkg$contributors, function(x) "ROR" %in% names(x), logical(1))))
  expect_false(any(vapply(pkg$taxonomic, function(x) "vernacularNamesEnglish" %in% names(x), logical(1))))
  # temporal keeps only the package-level extent
  expect_setequal(names(pkg$temporal), c("start", "end"))
})

test_that("as_camtrapdp keeps the standard descriptor content", {
  dp <- fixture()
  pkg <- convert(dp)$package

  # standard keys and values survive untouched
  expect_identical(pkg$id, dp$id)
  expect_identical(pkg$title, dp$title)
  expect_identical(pkg$temporal$start, dp$temporal$start)
  # contributors keep their names, emails and roles
  expect_identical(vapply(pkg$contributors, function(x) x$title, character(1)),
                   vapply(dp$contributors, function(x) x$title, character(1)))
  # spatial is GeoJSON and is left exactly as it was
  expect_identical(pkg$spatial, dp$spatial)
})

test_that("as_camtrapdp points the package at the target version", {
  pkg <- convert()$package
  expect_identical(pkg$version, "1.0.2")
  expect_match(pkg$profile, "tdwg/camtrap-dp/1.0.2/camtrap-dp-profile.json$")

  pkg101 <- convert(version = "1.0.1")$package
  expect_identical(pkg101$version, "1.0.1")
  expect_match(pkg101$profile, "tdwg/camtrap-dp/1.0.1/")
})

test_that("as_camtrapdp keeps one-value arrays as arrays when written", {
  pkg <- convert()$package
  dir <- withr_free_tempdir()
  frictionless::write_package(pkg, dir)
  json <- jsonlite::fromJSON(file.path(dir, "datapackage.json"), simplifyVector = FALSE)

  # Camtrap DP types these as arrays, even when they hold a single value
  expect_type(json$project$captureMethod, "list")
  expect_type(json$project$observationLevel, "list")
  # and the WildObsR in-memory copy of the tables is not written into the descriptor
  expect_false("data" %in% names(json))
})


### Value changes ----

test_that("as_camtrapdp moves English common names to vernacularNames$eng", {
  dp <- fixture()
  pkg <- convert(dp)$package

  before <- vapply(dp$taxonomic, function(x) x$vernacularNamesEnglish %||% NA_character_, character(1))
  after <- vapply(pkg$taxonomic, function(x) x$vernacularNames$eng %||% NA_character_, character(1))
  expect_identical(after, before)
})

test_that("as_camtrapdp rounds non-standard ranks up to a broader standard rank", {
  dp <- fixture()
  res <- convert(dp)
  ranks <- vapply(res$package$taxonomic, function(x) x$taxonRank, character(1))

  # every rank is one Camtrap DP lists, and the subclass became class
  expect_true(all(ranks %in% c("kingdom", "phylum", "class", "order", "family",
                               "genus", "species", "subspecies")))
  was_subclass <- vapply(dp$taxonomic, function(x) identical(x$taxonRank, "subclass"), logical(1))
  expect_true(all(ranks[was_subclass] == "class"))
  expect_true(any(res$report$action == "recoded subclass -> class"))
})

test_that("as_camtrapdp records RAiD identifiers as Handle", {
  pkg <- convert()$package
  types <- vapply(pkg$relatedIdentifiers, function(x) x$relatedIdentifierType, character(1))
  expect_false("RAiD" %in% types)
  expect_true("Handle" %in% types)
})

test_that("as_camtrapdp removes media rows that break a Camtrap DP pattern", {
  dp <- fixture()
  i <- which(vapply(dp$resources, function(r) r$name, character(1)) == "media")
  # put one media file on a contributor's own computer, which the pattern forbids
  bad_id <- dp$resources[[i]]$data$mediaID[1]
  dp$resources[[i]]$data$filePath[1] <- "/Users/someone/camera/IMG_0001.JPG"
  # and make an observation point at it
  j <- which(vapply(dp$resources, function(r) r$name, character(1)) == "observations")
  dp$resources[[j]]$data$mediaID[1] <- bad_id

  expect_warning(res <- as_camtrapdp(dp, validate = FALSE), "removed 1 of 263 media rows")
  expect_false(bad_id %in% table_of(res$package, "media")$mediaID)
  # the observation no longer points at a media row that does not exist
  expect_true(is.na(table_of(res$package, "observations")$mediaID[1]))
})

test_that("as_camtrapdp warns when every media row is removed", {
  dp <- fixture()
  i <- which(vapply(dp$resources, function(r) r$name, character(1)) == "media")
  dp$resources[[i]]$data$filePath <- "/Users/someone/camera/IMG.JPG"

  expect_warning(res <- as_camtrapdp(dp, validate = FALSE), "The media table is now empty")
  expect_equal(nrow(table_of(res$package, "media")), 0)
})


### Tables ----

test_that("as_camtrapdp keeps exactly the target fields, in order", {
  pkg <- convert()$package
  for (name in c("deployments", "media", "observations")) {
    expect_identical(names(table_of(pkg, name)), target_fields(name))
  } # end per table
})

test_that("as_camtrapdp leaves every kept value unchanged", {
  dp <- fixture()
  pkg <- convert(dp)$package
  for (name in c("deployments", "media", "observations")) {
    before <- table_of(dp, name)
    after <- table_of(pkg, name)
    expect_identical(after, before[, names(after)])
  } # end per table
})

test_that("as_camtrapdp cites the TDWG table schemas", {
  pkg <- convert()$package
  for (r in pkg$resources) {
    expect_match(r$schema, sprintf("tdwg/camtrap-dp/1.0.2/%s-table-schema.json$", r$name))
  } # end per resource
})

test_that("as_camtrapdp drops covariates unless asked to keep them", {
  dropped <- convert()$package
  kept <- convert(keep_covariates = TRUE)$package
  names_of <- function(pkg) vapply(pkg$resources, function(r) r$name, character(1))

  expect_false("covariates" %in% names_of(dropped))
  expect_true("covariates" %in% names_of(kept))
  expect_warning(as_camtrapdp(fixture(), validate = FALSE), "dropped the covariates table")
})


### Report and warnings ----

test_that("as_camtrapdp labels where each removal comes from", {
  report <- convert()$report
  origin_of <- function(path) report$origin[report$path == path][1]

  # declared in the WildObs flavour
  expect_identical(origin_of("media.observationID"), "WildObs extension")
  expect_identical(origin_of("WildObsMetadata"), "WildObs extension")
  expect_identical(origin_of("contributors[].ROR"), "WildObs extension")
  # added by wildobs_dp_download(), not part of the WildObs flavour
  expect_identical(origin_of("deployments.projectName"), "not in WildObs profile")
})

test_that("as_camtrapdp warns that the media-observation link is lost", {
  expect_warning(as_camtrapdp(fixture(), validate = FALSE), "media.observationID, which links tables")
})

test_that("as_camtrapdp is silent with warn = 'none' and deterministic", {
  expect_no_warning(first <- as_camtrapdp(fixture(), validate = FALSE, warn = "none"))
  second <- as_camtrapdp(fixture(), validate = FALSE, warn = "none")
  # each run reads its camtrapdp object from its own temporary folder, so set the path aside
  attr(first$camtrapdp, "directory") <- NULL
  attr(second$camtrapdp, "directory") <- NULL
  # the conversion itself must be byte for byte the same
  expect_identical(first[c("package", "report")], second[c("package", "report")])
  # and the camtrapdp object holds the same content, though its internals are rebuilt each read
  expect_equal(first$camtrapdp, second$camtrapdp)
})

test_that("as_camtrapdp never modifies the package it is given", {
  dp <- fixture()
  copy <- dp
  convert(dp)
  expect_identical(dp, copy)
})


### Input checks ----

test_that("as_camtrapdp stops on an unknown version, listing the available ones", {
  expect_error(as_camtrapdp(fixture(), version = "2.0"), "Available versions: 1.0.1, 1.0.2")
})

test_that("as_camtrapdp stops when a Camtrap DP table is missing", {
  dp <- fixture()
  dp$resources <- dp$resources[vapply(dp$resources, function(r) r$name, character(1)) != "media"]
  expect_error(as_camtrapdp(dp, validate = FALSE), "has no media table.*media = TRUE")
})

test_that("as_camtrapdp stops on something that is not a data package", {
  expect_error(as_camtrapdp(list(a = 1)), "must be a single data package")
})


### Works with the camtrapdp package ----

test_that("as_camtrapdp output validates and works with camtrapdp", {
  skip_if_not_installed("camtrapdp", minimum_version = "0.5.0")
  skip_if_not_installed("jsonvalidate")

  res <- suppressWarnings(as_camtrapdp(fixture()))
  # both checks the function runs pass
  expect_true(res$validation$descriptor$valid)
  expect_true(res$validation$camtrapdp$valid)

  # and camtrapdp can read it and export it for GBIF, which the raw package could not
  dir <- withr_free_tempdir()
  frictionless::write_package(res$package, dir)
  x <- suppressMessages(camtrapdp::read_camtrapdp(file.path(dir, "datapackage.json")))
  expect_true("vernacularNames.eng" %in% names(camtrapdp::taxa(x)))
  expect_no_error(suppressMessages(camtrapdp::write_dwc(x, withr_free_tempdir())))
})

test_that("as_camtrapdp returns a camtrapdp object camtrapdp functions accept directly", {
  skip_if_not_installed("camtrapdp", minimum_version = "0.5.0")

  res <- suppressWarnings(as_camtrapdp(fixture(), validate = FALSE))
  # the read-back object carries its tables where camtrapdp expects them
  expect_s3_class(res$camtrapdp, "camtrapdp")
  expect_s3_class(camtrapdp::deployments(res$camtrapdp), "data.frame")
  # and goes straight into a GBIF export, with no save and reload first
  dwc_dir <- withr_free_tempdir()
  expect_no_error(suppressMessages(camtrapdp::write_dwc(res$camtrapdp, dwc_dir)))
  expect_true(file.exists(file.path(dwc_dir, "occurrence.csv")))
  # the folder it was read from is still there, so frictionless can read its resources too
  expect_s3_class(frictionless::read_resource(res$camtrapdp, "deployments"), "data.frame")
})
