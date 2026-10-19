### make_example_data.R -- build the Wet Tropics example data packages shipped in inst/extdata
##
## Two open WildObs projects from the Queensland Wet Tropics, downloaded through the
## public API exactly as any key holder would get them, then written to disk as Camtrap DP
## packages (datapackage.json plus gzipped CSVs). example_datapackages() reads them back,
## so examples, tests and vignettes run offline on real data.
##
## Both projects are open and CC-BY-4.0, and their providers approved bundling them.
## Media are left out: they are the largest table and no example needs them. Contributor
## email addresses are left out too. Coordinates are kept as the public API serves them,
## and threatened-species records stay obscured.
##
## Re-run after a public database rebuild to refresh the copies, then re-run the tests.
## Run from the package root with WILDOBSR_API_KEY set; not run by the package itself.

## Load libraries
library(here) # For project-relative paths so this runs on any machine

## load the package under development, so the copies match the code that reads them
devtools::load_all(here(), quiet = TRUE)

## a public API key, exactly as a user would connect
api_key <- Sys.getenv("WILDOBSR_API_KEY")
# cant download anything without one
if (!nzchar(api_key)) {
  stop("WILDOBSR_API_KEY is not set, so make_example_data.R cannot reach the API.\n",
       "Add your key to ~/.Renviron and restart R.")
} # end API key check

### The projects to bundle
## add an approved project's ID here and re-run to ship it too
projects <- c(
  "ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001",          # systematic, unbaited, roads and trails
  "QLD_Paluma_cassowary_Mclean_2023-24_WildObsID_0006"  # targeted, baited game trails
)

## keep only animal observations, which is all the detection examples need
animal_only <- TRUE

## where the packages are written, one folder per project, named by its WildObsID
## (a full project ID would make file paths too long for an R package tarball)
out_dir <- here("inst", "extdata", "wet_tropics")


#
##
### Download and write each project ----

# download the tables of every project in one call, without media
dp_list <- wildobs_dp_download(api_key = api_key, project_ids = projects)

# for each project
for (id in names(dp_list)) {
  # grab it
  dp <- dp_list[[id]]

  ## thin the observations to animals, keeping the resource and dp$data in step
  if (animal_only) {
    # which resource holds the observations
    i <- which(frictionless::resource_names(dp) == "observations")
    # keep the animal rows, obscured threatened species included
    obs <- dp$resources[[i]]$data
    dp$resources[[i]]$data <- obs[obs$observationType %in% "animal", , drop = FALSE]
  } # end animal condition

  ## personal email addresses are not shipped inside the package; names, roles,
  ## ORCIDs and organisations stay, so the projects can still be cited and credited
  dp$contributors <- lapply(dp$contributors, function(c) {
    # drop the address, keep everything else
    c$email <- NULL
    c
  }) # end per contributor

  ## the in-memory copy of the tables is rebuilt on reading, so it is not written
  dp$data <- NULL

  ## start from an empty folder, so a dropped table never lingers
  dir <- file.path(out_dir, sub("^.*_(WildObsID_[0-9]+)$", "\\1", id))
  unlink(dir, recursive = TRUE)
  # write the descriptor and gzipped CSVs
  frictionless::write_package(dp, dir, compress = TRUE)
  # say what was written
  message(sprintf("%s: %s", id, paste(sprintf("%s %d rows", frictionless::resource_names(dp),
    vapply(dp$resources, function(r) nrow(r$data), integer(1))), collapse = ", ")))
} # end per project

## report the size shipped with the package
sizes <- file.info(list.files(out_dir, recursive = TRUE, full.names = TRUE))$size
message(sprintf("inst/extdata/wet_tropics: %.2f MB", sum(sizes) / 1e6))
