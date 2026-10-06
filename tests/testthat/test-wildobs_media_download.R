## Tests for wildobs_media_download() ----

## Most tests here use files on this computer, so they need no network. The
## last block reaches real WildObs and Google Cloud addresses and is skipped
## offline, on CRAN and in CI.

## a fresh, empty output folder per test, inside the session temp folder
fresh_out <- function() {
  # a unique path that does not exist yet
  out <- tempfile("media_out_")
  return(out)
} # end helper

## a small stand-in image on this computer, playing a provider's original file
local_image <- function(name = "IMG_0001.JPG") {
  # put it in its own folder so names never collide between tests
  src_dir <- tempfile("provider_drive_")
  dir.create(src_dir)
  src <- file.path(src_dir, name)
  # the JPEG start-of-image marker is enough to stand in for an image
  writeBin(as.raw(c(0xFF, 0xD8, 0xFF, 0xE0)), src)
  return(src)
} # end helper

## a minimal Camtrap DP media table
make_media <- function(filePath, mediaID = paste0("m", seq_along(filePath))) {
  data.frame(
    mediaID = mediaID,
    deploymentID = "dep_1",
    projectName = "proj_1",
    filePath = filePath,
    fileMediatype = "image/jpeg",
    stringsAsFactors = FALSE
  )
} # end helper


### Input checks ----

test_that("wildobs_media_download stops on a non data frame", {
  expect_error(wildobs_media_download(list(a = 1), out_dir = fresh_out()),
               "must be a data frame")
})

test_that("wildobs_media_download names the missing columns", {
  bad <- data.frame(mediaID = "m1", filePath = "x")
  expect_error(wildobs_media_download(bad, out_dir = fresh_out()), "deploymentID")
})

test_that("wildobs_media_download requires an out_dir", {
  expect_error(wildobs_media_download(make_media("not_provided")), "out_dir")
  expect_error(wildobs_media_download(make_media("not_provided"), out_dir = c("a", "b")),
               "out_dir")
})


### Sorting rows by where the file lives ----

test_that("placeholders and other people's drive paths are skipped", {
  media <- make_media(c("not_provided", "F:/someone_else/IMG_1.JPG", NA))

  res <- suppressMessages(wildobs_media_download(media, out_dir = fresh_out()))

  # nothing reachable, so nothing written and every row explains why
  expect_equal(res$downloadStatus, rep("skipped", 3))
  expect_false(any(file.exists(res$localPath)))
})

test_that("files on this computer are copied into project/deployment folders", {
  out <- fresh_out()
  media <- make_media(local_image())

  res <- suppressMessages(wildobs_media_download(media, out_dir = out))

  expect_equal(res$downloadStatus, "copied")
  # saved as out_dir/projectName/deploymentID/mediaID.ext, keeping the extension
  expect_equal(res$localPath, file.path(out, "proj_1", "dep_1", "m1.jpg"))
  expect_true(file.exists(res$localPath))
})

test_that("the folder layout drops projectName when that column is absent", {
  out <- fresh_out()
  media <- make_media(local_image())
  media$projectName <- NULL

  res <- suppressMessages(wildobs_media_download(media, out_dir = out))

  expect_equal(res$localPath, file.path(out, "dep_1", "m1.jpg"))
})

test_that("characters not allowed in file names are replaced", {
  out <- fresh_out()
  # some WildObs mediaIDs contain dates written with slashes
  media <- make_media(local_image(), mediaID = "Grid 7A 04/22/2024_mediaID_1")

  res <- suppressMessages(wildobs_media_download(media, out_dir = out))

  # the slashes must not create extra folders
  expect_equal(basename(res$localPath), "Grid 7A 04_22_2024_mediaID_1.jpg")
  expect_true(file.exists(res$localPath))
})

test_that("the extension falls back on fileMediatype when the path has none", {
  out <- fresh_out()
  media <- make_media(local_image(name = "IMG_0001"))
  media$fileMediatype <- "video/mp4"

  res <- suppressMessages(wildobs_media_download(media, out_dir = out))

  expect_match(res$localPath, "\\.mp4$")
})


### Re-running ----

test_that("files already in out_dir are kept, not fetched again", {
  out <- fresh_out()
  media <- make_media(local_image())
  first <- suppressMessages(wildobs_media_download(media, out_dir = out))
  # mark the saved copy so we can tell whether it gets replaced
  writeLines("kept", first$localPath)

  second <- suppressMessages(wildobs_media_download(media, out_dir = out))

  expect_equal(second$downloadStatus, "already_exists")
  expect_equal(readLines(second$localPath), "kept")
})

test_that("overwrite = TRUE fetches files again", {
  out <- fresh_out()
  media <- make_media(local_image())
  first <- suppressMessages(wildobs_media_download(media, out_dir = out))
  writeLines("stale", first$localPath)

  second <- suppressMessages(wildobs_media_download(media, out_dir = out, overwrite = TRUE))

  expect_equal(second$downloadStatus, "copied")
  # the stale marker has been replaced by the original file
  expect_false(identical(suppressWarnings(readLines(second$localPath)), "stale"))
})

test_that("the input columns are returned untouched with three new ones", {
  media <- make_media(c("not_provided", local_image()))

  res <- suppressMessages(wildobs_media_download(media, out_dir = fresh_out()))

  expect_equal(res[, names(media)], media)
  expect_true(all(c("localPath", "downloadStatus", "downloadNote") %in% names(res)))
})


### Live downloads ----

test_that("a public WildObs image downloads as a real JPEG", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline("data.wildobs.org.au")

  media <- make_media(paste0(
    "https://data.wildobs.org.au/tir/NSW_Blue_Mountains_Fire_recovery_Greenville_2021_22_WildObsID_0013/",
    "B1_01_12_2022/B1_01_12_2022_A_Greenville_triggerObs_1/B1_01_12_2022_A_Greenville_mediaID_1.JPG"))

  res <- suppressMessages(wildobs_media_download(media, out_dir = fresh_out()))

  expect_equal(res$downloadStatus, "downloaded")
  # every JPEG starts with the bytes FF D8
  expect_equal(readBin(res$localPath, "raw", 2), as.raw(c(0xFF, 0xD8)))
})

test_that("a private Google Cloud path fails cleanly without leaving a file", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline("storage.googleapis.com")

  media <- make_media(paste0(
    "gs://145625598251_2006048_607_btrw_survey__main/deployment/2219414/prod/",
    "directUpload/0078b3db-8aa7-4ca9-9156-78d2f56a64f7.JPG"))

  res <- suppressMessages(wildobs_media_download(media, out_dir = fresh_out()))

  expect_equal(res$downloadStatus, "failed")
  expect_match(res$downloadNote, "no permission")
  # the error body Google sends back must not be left posing as an image
  expect_false(file.exists(res$localPath))
})

test_that("a web page is rejected rather than saved as an image", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline("volunteer.ala.org.au")

  # DigiVol filePaths point at a volunteer task page, not an image file
  media <- make_media("https://volunteer.ala.org.au/validate/task/367253742")

  res <- suppressMessages(wildobs_media_download(media, out_dir = fresh_out()))

  expect_equal(res$downloadStatus, "failed")
  expect_match(res$downloadNote, "web page")
  expect_false(file.exists(res$localPath))
})
