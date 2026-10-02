##### Add data to R package
#### Primarily using the function usethis::use_data()
### and saving a traceable log about how and which data was uploaded

### Zachary Amir, Z.Amir@uq.edu.au
### Last updated: Dec 1st, 2025

## start fresh
rm(list = ls())

## Set up personal file paths to dropbox
## Set WILDOBS_DROPBOX in your .Renviron, e.g. WILDOBS_DROPBOX=/Users/you/Dropbox
personal_path <- Sys.getenv("WILDOBS_DROPBOX")


##### Species traits data #####

## Create working directory to where species data lives
wd = file.path(personal_path, "WildObs master folder/WildObs GitHub Data Storage/data_tools/")

#
##
### Import the most recent verified species taxonomy DP
ver_files = list.files(wd)
ver_files = ver_files[grepl("taxonomy", ver_files)] # only want species data
# Extract the date parts of the filenames and convert to integers
dates <- as.numeric(gsub("verified_taxonomy_(\\d{8})", "\\1", ver_files))
# Order the filenames based on the extracted dates (from most recent to least recent)
ver_file <- ver_files[order(dates, decreasing = TRUE)]
# Import the most recent file
taxa_dp = frictionless::read_package(paste(wd, ver_file[1], "/datapackage.json", sep = ""))
rm(ver_file, ver_files, dates)

### Extract species traits from the DP
species_traits = frictionless::read_resource(taxa_dp, "species_traits")

### Inspect data
dplyr::glimpse(species_traits) # looks good!
anyNA(species_traits$binomial_verified) # MUST BE F
anyNA(species_traits$epbc_category) # MUST BE F

### Save data to the R package
usethis::use_data(species_traits, overwrite = TRUE)





#
##
### Spatial layers: IBRA7 subregions and CAPAD 2022 terrestrial ----

## Bundles the two layers used by ibra_classification(), locationName_buffer_CAPAD()
## and locationName_verification_CAPAD(), so users no longer need the ECL Dropbox (#11, #122).
## Each layer is trimmed to the columns those functions read, simplified so the package
## stays installable, and stored as an sf object in EPSG:4326 (WGS84) to match
## deployment coordinates. sf is used rather than terra because a SpatVector cannot be
## saved to .rda; the functions convert with terra::vect() when they run.

### Simplification tolerances, in metres
## chosen 2026-10-02 by comparing point lookups for all 19,736 LOCAL deployments:
## IBRA at 25 m changed none; CAPAD at 5 m changed 4 (no park-to-park switches)
ibra_tolerance_m <- 25
capad_tolerance_m <- 5

## folder holding both shapefiles in the shared ECL Dropbox
spatial_dir <- file.path(personal_path, "ECL spatial layers repository",
                         "Australian spatial layers GIS data", "AUS")

## a quick helper to fix, simplify and reproject one layer
## simplifying happens in Australian Albers (EPSG:3577) so the tolerance is in metres
prepare_layer <- function(layer, keep_cols, tolerance_m) {
  simple <- layer |>
    # keep only the columns the package functions use
    dplyr::select(dplyr::all_of(keep_cols)) |>
    # repair the few self-intersecting polygons in the source data
    sf::st_make_valid() |>
    # move to an equal-area projection measured in metres
    sf::st_transform(3577) |>
    # drop vertices closer than the tolerance, without breaking polygon topology
    sf::st_simplify(preserveTopology = TRUE, dTolerance = tolerance_m) |>
    # and back to WGS84 longitude/latitude, matching deployment coordinates
    sf::st_transform(4326)

  ## simplifying and reprojecting can leave a few self-crossing rings; repair them
  ## with terra, since terra's flat-geometry validity is what the package functions use
  simple <- sf::st_as_sf(terra::makeValid(terra::vect(simple)))

  return(simple)
} # end layer helper

## IBRA7 subregions (419 polygons, GDA94 in the source file)
ibra_raw <- sf::st_read(file.path(spatial_dir, "IBRA7_bioregions", "ibra7_subregions.shp"),
                        quiet = TRUE)
ibra <- prepare_layer(ibra_raw,
                      keep_cols = c("SUB_CODE_7", "SUB_NAME_7", "REG_CODE_7", "REG_NAME_7", "HECTARES"),
                      tolerance_m = ibra_tolerance_m)

## CAPAD 2022 terrestrial protected areas (14,234 polygons, web Mercator in the source file)
capad_raw <- sf::st_read(file.path(spatial_dir, "CAPAD_Terrestrial_land_use",
                                   "Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Terrestrial",
                                   "Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Terrestrial.shp"),
                         quiet = TRUE)
capad <- prepare_layer(capad_raw,
                       keep_cols = c("NAME", "TYPE_ABBR", "IUCN"),
                       tolerance_m = capad_tolerance_m)

### Inspect data
## every source polygon must survive, with nothing empty or invalid
nrow(ibra) == nrow(ibra_raw)     # MUST BE TRUE
nrow(capad) == nrow(capad_raw)   # MUST BE TRUE
any(sf::st_is_empty(ibra))       # MUST BE FALSE
any(sf::st_is_empty(capad))      # MUST BE FALSE
## validity as terra sees it (flat geometry), since that is what the functions use
all(terra::is.valid(terra::vect(ibra)))   # MUST BE TRUE
all(terra::is.valid(terra::vect(capad)))  # MUST BE TRUE
sf::st_crs(ibra)$epsg            # MUST BE 4326
sf::st_crs(capad)$epsg           # MUST BE 4326

### Save data to the R package
## xz compression gives the smallest .rda for polygon data
usethis::use_data(ibra, capad, overwrite = TRUE, compress = "xz")



#
##
### Camtrap DP profiles for as_camtrapdp() ----

## as_camtrapdp() converts a WildObs data package to canonical Camtrap DP (#113). It
## reads what "canonical" means from the official TDWG profile and table schemas, and
## which removals are expected WildObs extensions from the WildObs flavour generated in
## camDB. Both are vendored into inst/profiles/ so the conversion runs offline and gives
## the same answer on every machine.

## camDB's profiles folder, in the cam-DB repo cloned beside this one:
## upstream copies at the top, the WildObs flavour in wildobs/
## TODO: point this at the public camDB repo once it is published (cam-DB #259)
camdb_profiles <- normalizePath(here::here("..", "WildObs_cam-DB", "code_data cleaning", "utils", "profiles"),
                                mustWork = TRUE)

## where the package keeps them
pkg_profiles <- here::here("inst", "profiles")
dir.create(file.path(pkg_profiles, "wildobs"), recursive = TRUE, showWarnings = FALSE)

### Copy the upstream and WildObs files from camDB
# the upstream TDWG and frictionless copies camDB validates against
upstream_files <- c("camtrap-dp-profile-1.0.1.json", "camtrap-dp-profile-1.0.2.json",
                    "camtrap-dp-deployments-table-schema-1.0.2.json",
                    "camtrap-dp-media-table-schema-1.0.2.json",
                    "camtrap-dp-observations-table-schema-1.0.2.json",
                    "frictionless-data-package.json", "geojson.json")
file.copy(file.path(camdb_profiles, upstream_files), pkg_profiles, overwrite = TRUE)
# and every generated WildObs flavour file, with its README
wildobs_files <- list.files(file.path(camdb_profiles, "wildobs"), pattern = "\\.(json|md)$")
file.copy(file.path(camdb_profiles, "wildobs", wildobs_files),
          file.path(pkg_profiles, "wildobs"), overwrite = TRUE)

### camDB does not vendor the 1.0.1 table schemas, so fetch them from TDWG
## they are field-for-field identical to 1.0.2, but each version cites its own URLs
for (resource in c("deployments", "media", "observations")) {
  download.file(sprintf("https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0.1/%s-table-schema.json", resource),
                file.path(pkg_profiles, sprintf("camtrap-dp-%s-table-schema-1.0.1.json", resource)),
                quiet = TRUE)
} # end per resource

### Inspect: every upstream copy must be byte-identical to what TDWG publishes
## a vendored file that has drifted would make the conversion quietly wrong
tdwg_url <- function(f) {
  # the version and file name TDWG publishes it under
  v <- sub(".*-(1\\.0\\.[0-9])\\.json$", "\\1", f)
  base <- sub("^camtrap-dp-", "", sub("-1\\.0\\.[0-9]\\.json$", ".json", f))
  sprintf("https://raw.githubusercontent.com/tdwg/camtrap-dp/%s/%s",
          v, if (grepl("^profile", base)) "camtrap-dp-profile.json" else base)
} # end url helper
tdwg_files <- list.files(pkg_profiles, pattern = "^camtrap-dp-.*-1\\.0\\.[0-9]\\.json$")
identical_to_tdwg <- vapply(tdwg_files, function(f) {
  # fetch TDWG's copy and compare bytes
  tmp <- tempfile(fileext = ".json")
  download.file(tdwg_url(f), tmp, quiet = TRUE)
  identical(unname(tools::md5sum(tmp)), unname(tools::md5sum(file.path(pkg_profiles, f))))
}, logical(1))
identical_to_tdwg               # MUST BE all TRUE
all(identical_to_tdwg)          # MUST BE TRUE
list.files(pkg_profiles, recursive = TRUE)
