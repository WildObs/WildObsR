### dashboard_screenshot.R -- capture the camtrapR survey dashboard for the interoperability vignette
##
## Opens camtrapR::surveyDashboard() on the Paluma project in a background R session, then
## takes a screenshot of its Data Summary page with a headless Chrome browser. Writes
## vignettes/figures/interoperability-dashboard-summary.png. Public data only.
## Needs Chrome, and the chromote package (not a WildObsR dependency). Run from the package
## root with WILDOBSR_API_KEY set.

## Load libraries
library(here) # For project-relative paths so this runs on any machine

## the package under development, so the export matches the vignette
devtools::load_all(here(), quiet = TRUE, export_all = FALSE, helpers = FALSE,
                   attach_testthat = FALSE)

## the project, exported to canonical Camtrap DP and written to disk for camtrapR
id <- "QLD_Paluma_cassowary_Mclean_2023-24_WildObsID_0006"
dp <- wildobs_dp_download(api_key = Sys.getenv("WILDOBSR_API_KEY"), project_ids = id,
                          media = TRUE)[[1]]
out <- as_camtrapdp(dp, warn = "none")
pkg_dir <- file.path(tempdir(), "paluma_camtrapdp")
frictionless::write_package(out$package, pkg_dir)

### Start the dashboard in a background session
## surveyDashboard() runs a Shiny app, which blocks the session that starts it
port <- 8765
app <- callr::r_bg(function(pkg_dir, port) {
  # serve on a fixed local port, without opening a browser
  options(shiny.port = port, shiny.host = "127.0.0.1", shiny.launch.browser = FALSE)
  # camtrapR's tables, as in the vignette
  ct <- camtrapR::readcamtrapDP(
    deployments_file = file.path(pkg_dir, "deployments.csv"),
    media_file = file.path(pkg_dir, "media.csv"),
    observations_file = file.path(pkg_dir, "observations.csv"),
    datapackage_file = file.path(pkg_dir, "datapackage.json"))
  # animal detections that can be placed on a station
  rec <- ct$recordTable[ct$recordTable$observationType == "animal" &
                          !grepl("^obscured_for_", ct$recordTable$deploymentID), ]
  rec$Station <- rec$locationID
  camtrapR::surveyDashboard(
    CTtable = ct$CTtable, recordTable = rec,
    stationCol = "Station", setupCol = "Setup_date", retrievalCol = "Retrieval_date",
    hasProblems = TRUE, CTdateFormat = "ymd HMS",
    xcol = "longitude", ycol = "latitude", crs = "epsg:4326",
    speciesCol = "scientificName", recordDateTimeCol = "DateTimeOriginal",
    recordDateTimeFormat = "ymd HMS", timeZone = "Australia/Brisbane")
}, args = list(pkg_dir = pkg_dir, port = port))
# stop the dashboard however this script ends
on.exit(app$kill(), add = TRUE)

## wait up to two minutes for the app to answer
url <- sprintf("http://127.0.0.1:%d", port)
for (i in 1:40) {
  # stop waiting once the page loads
  ok <- tryCatch(curl::curl_fetch_memory(url)$status_code == 200, error = function(e) FALSE)
  if (ok) break
  Sys.sleep(3)
} # end wait
# a dashboard that never answered has nothing to capture
if (!ok) stop("dashboard_screenshot.R: the dashboard did not start within two minutes.\n",
              "Check that camtrapR and its dashboard dependencies are installed.")

### Capture the Data Summary page
b <- chromote::ChromoteSession$new(width = 1400, height = 950)
b$Page$navigate(url)
# the summary boxes and plots take a few seconds to render
Sys.sleep(15)
b$screenshot(here("vignettes", "figures", "interoperability-dashboard-summary.png"))
b$close()
