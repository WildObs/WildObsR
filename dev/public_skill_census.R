### dev/public_skill_census.R -- measure what a public API user actually receives
##
## The public wildobsr-data skill (inst/claude-skills/wildobsr-data/) describes the data
## most users download: the public MongoDB, reached through an API key with WildObsR
## functions only. This script measures that view, so the skill's numbers come from what
## users see rather than from an admin connection.
##
## It writes dev/public_skill_census/census_<YYYYMMDD>.md (aggregate numbers only) and a
## matching _metrics.csv, and prints any metric that moved since the previous census.
##
## How to refresh the skill after the public database is rebuilt:
##   1. Rscript dev/public_skill_census.R   (media for every open project; can take an hour)
##   2. read the "Changed since" table at the bottom of the new census file
##   3. update the "Scale" block and any moved percentages in the skill, then commit both
##
## Privacy: never print coordinates, and never name the species behind obscured records.

## Load libraries
library(here)      # For project-relative paths so this runs on any machine

## load the package under development, so the census tests the code about to ship
devtools::load_all(here(), quiet = TRUE)

## a personal API key, exactly as a public user would connect
api_key <- Sys.getenv("WILDOBSR_API_KEY")
# cant measure the public view without one
if (!nzchar(api_key)) {
  stop("WILDOBSR_API_KEY is not set, so public_skill_census.R cannot reach the API.\n",
       "Add your key to ~/.Renviron and restart R.")
} # end API key check

### Which open projects to measure
## empty measures them all; a comma-separated list of project IDs is a quick pilot
## that checks the script runs and writes nothing
pilot_ids <- strsplit(Sys.getenv("CENSUS_PILOT_IDS"), ",")[[1]]
is_pilot <- length(pilot_ids) > 0

## where the census files are written
out_dir <- here("dev", "public_skill_census")
dir.create(out_dir, showWarnings = FALSE)
# stamp this run with today's date
stamp <- format(Sys.Date(), "%Y%m%d")

## one row per measured number, so two censuses can be compared by name
metrics <- data.frame(metric = character(0), value = character(0))
## small helper to record a metric as text
add_metric <- function(metrics, name, value) {
  # store the value as text so numbers and labels share one column
  rbind(metrics, data.frame(metric = name, value = as.character(value)))
} # end add_metric helper


#
##
### Query every filter through wildobs_mongo_query() ----
## a call with no filters returns nothing, so a box over all of Australia lists every project

# a bounding box covering all of Australia and its islands
aus_box <- list(xmin = 110, xmax = 160, ymin = -45, ymax = -9)
# every project a public user can see, open or partial
all_ids <- wildobs_mongo_query(api_key = api_key, spatial = aus_box,
                               tabularSharingPreference = c("open", "partial"))
# only the projects whose tables are released
open_ids <- wildobs_mongo_query(api_key = api_key, spatial = aus_box,
                                tabularSharingPreference = "open")
# a pilot run measures only the projects it names
if (is_pilot) open_ids <- intersect(open_ids, trimws(pilot_ids))
metrics <- add_metric(metrics, "projects_visible", length(all_ids))
metrics <- add_metric(metrics, "projects_open", length(open_ids))

## one quick call per filter type, to confirm each still answers on the public data
filter_checks <- list(
  # surveys running at any time in 2022
  temporal_2022 = list(temporal = list(minDate = as.Date("2022-01-01"),
                                       maxDate = as.Date("2022-12-31"))),
  # a box over most of Queensland
  spatial_qld   = list(spatial = list(xmin = 138, xmax = 154, ymin = -29, ymax = -10)),
  # koalas
  taxonomic     = list(taxonomic = "Phascolarctos cinereus"),
  # targeted sampling designs
  sampling      = list(samplingDesign = "targeted")
) # end filter list
# run each filter over open and partial projects and count what comes back
for (f in names(filter_checks)) {
  # add the shared arguments to this filter's own
  args <- c(list(api_key = api_key, tabularSharingPreference = c("open", "partial")),
            filter_checks[[f]])
  # an empty match warns by design, so keep the count and drop the warning
  ids <- suppressWarnings(do.call(wildobs_mongo_query, args))
  metrics <- add_metric(metrics, paste0("query_", f), length(ids))
} # end per filter


#
##
### Metadata of every visible project ----

## metadata-only download is fast and covers partial projects too
meta_list <- wildobs_dp_download(api_key = api_key, project_ids = all_ids,
                                 metadata_only = TRUE)

## each project's sharing preference, as recorded in its own metadata
prefs <- vapply(meta_list, function(dp) {
  # a missing preference is counted as unknown rather than failing
  p <- dp$WildObsMetadata$tabularSharingPreference
  if (is.null(p)) NA_character_ else as.character(p)
}, character(1))
metrics <- add_metric(metrics, "projects_partial", sum(prefs %in% "partial"))

### Which database shape the API is serving
## the updated shape stores sources as an unnamed list of sources;
## the older shape stored a single named source
sources_shape <- vapply(meta_list, function(dp) {
  # an unnamed list means one entry per source
  if (is.null(names(dp$sources))) "list_of_sources" else "single_source"
}, character(1))
metrics <- add_metric(metrics, "shape_sources",
                      paste(names(table(sources_shape)), table(sources_shape),
                            sep = "=", collapse = "; "))
# the Camtrap DP profile versions the packages declare
profiles <- vapply(meta_list, function(dp) {
  # the version is the folder name in the profile URL
  if (is.null(dp$profile)) NA_character_ else basename(dirname(dp$profile))
}, character(1))
metrics <- add_metric(metrics, "profile_versions",
                      paste(names(table(profiles)), table(profiles), sep = "=", collapse = "; "))
# the database release each package was built from
db_versions <- vapply(meta_list, function(dp) {
  # the updated shape is a block holding currentVersion; the older shape is the version alone
  v <- dp$versionControlWildObs
  if (is.list(v)) v <- v$currentVersion
  if (is.null(v)) NA_character_ else as.character(v)[1]
}, character(1))
metrics <- add_metric(metrics, "database_version",
                      paste(unique(db_versions), collapse = ", "))

## extract_metadata() on every element it offers, recording rows or the error
elements <- c("contributors", "sources", "licenses", "references", "spatial",
              "temporal", "taxonomic", "WildObsMetadata", "project", "relatedIdentifiers")
for (el in elements) {
  # asking for one element returns its table directly; a failure is recorded, not fatal
  res <- tryCatch(suppressWarnings(extract_metadata(meta_list, el)),
                  error = function(e) paste("ERROR:", conditionMessage(e)))
  # count rows when we got a table back
  val <- if (is.data.frame(res)) nrow(res) else if (length(res) == 0) "empty" else res[1]
  metrics <- add_metric(metrics, paste0("extract_", el, "_rows"), val)
} # end per element

## distinct taxa listed across all visible projects
taxa <- suppressWarnings(extract_metadata(meta_list, "taxonomic"))
metrics <- add_metric(metrics, "taxa_listed_all_projects", length(unique(taxa$scientificName)))
# keep the metadata list small from here on
rm(meta_list)


#
##
### Tables of every open project, one project at a time ----
## media for the larger projects runs to hundreds of thousands of rows,
## so each project is summarised and dropped before the next is fetched

## running totals across projects
tot <- list(deployments = 0, covariates = 0, observations = 0, media = 0,
            animal = 0, blank = 0, human = 0, vehicle = 0, unknown = 0,
            obs_obscured = 0, obs_obscured_animal = 0, projects_with_obscured = 0,
            obs_joined = 0, obs_id_dupes_unobscured = 0,
            class_human = 0, class_machine = 0, animal_count_na = 0,
            rank_species = 0, rank_coarser = 0, taxa = character(0),
            file_wildobs = 0, file_gcs = 0, file_digivol = 0, file_local = 0, file_other = 0,
            file_public = 0, file_name_filled = 0, timezones = character(0),
            first_date = as.Date(NA), last_date = as.Date(NA))
## coverage of the sparse fields, as filled values out of animal observations
sparse_fields <- c("individualID", "sex", "lifeStage", "behavior",
                   "individualPositionRadius", "individualSpeed")
sparse_filled <- setNames(rep(0, length(sparse_fields)), sparse_fields)
## covariate gaps, as missing values per column, per family
cov_na <- list()
## projects whose download failed, with the reason
failed <- character(0)
## the obscured category labels and how often each occurs
obscured_categories <- integer(0)

# for each open project
for (id in open_ids) {
  # say which project is running, since the media download is long
  message(sprintf("[%s] downloading %s", format(Sys.time(), "%H:%M"), id))
  # download it exactly as a user would, media included
  dp <- tryCatch(wildobs_dp_download(api_key = api_key, project_ids = id, media = TRUE)[[1]],
                 error = function(e) conditionMessage(e))
  # record a failure and move on
  if (is.character(dp)) {
    failed <- c(failed, sprintf("%s: %s", id, dp))
    next
  } # end failed download

  ## pull the four tables
  deps <- dp$data$deployments
  covs <- dp$data$covariates
  obs  <- dp$data$observations
  med  <- dp$data$media

  ## table sizes
  tot$deployments  <- tot$deployments + nrow(deps)
  tot$covariates   <- tot$covariates + nrow(covs)
  tot$observations <- tot$observations + nrow(obs)
  tot$media        <- tot$media + nrow(med)

  ## observation types
  for (ty in c("animal", "blank", "human", "vehicle", "unknown")) {
    # add this project's rows of each type
    tot[[ty]] <- tot[[ty]] + sum(obs$observationType == ty, na.rm = TRUE)
  } # end per type

  ### Obscured records
  ## threatened species have their join keys replaced with obscured_for_<category>_species
  is_obscured <- grepl("^obscured_for_", obs$deploymentID)
  tot$obs_obscured <- tot$obs_obscured + sum(is_obscured)
  tot$obs_obscured_animal <- tot$obs_obscured_animal +
    sum(is_obscured & obs$observationType == "animal", na.rm = TRUE)
  tot$projects_with_obscured <- tot$projects_with_obscured + as.integer(any(is_obscured))
  # tally the category labels without touching species names
  cats <- table(obs$deploymentID[is_obscured])
  for (k in names(cats)) {
    # start a new category at zero, then add this project's count
    obscured_categories[k] <- sum(obscured_categories[k], cats[[k]], na.rm = TRUE)
  } # end per category

  ## how many observations find their deployment
  tot$obs_joined <- tot$obs_joined + sum(obs$deploymentID %in% deps$deploymentID)
  # observationID should be unique once obscured rows are set aside
  tot$obs_id_dupes_unobscured <- tot$obs_id_dupes_unobscured +
    sum(duplicated(obs$observationID[!is_obscured]))

  ## classification and counts on animal observations
  animal <- obs[obs$observationType %in% "animal", ]
  tot$class_human   <- tot$class_human + sum(animal$classificationMethod == "human", na.rm = TRUE)
  tot$class_machine <- tot$class_machine + sum(animal$classificationMethod == "machine", na.rm = TRUE)
  tot$animal_count_na <- tot$animal_count_na + sum(is.na(animal$count))
  # identified to species, or only to something coarser
  tot$rank_species <- tot$rank_species + sum(animal$taxonRank %in% c("species", "subspecies"))
  tot$rank_coarser <- tot$rank_coarser + sum(!animal$taxonRank %in% c("species", "subspecies"))
  # distinct taxa actually recorded
  tot$taxa <- union(tot$taxa, unique(animal$scientificName[!is.na(animal$scientificName)]))
  # how filled the sparse fields are
  for (sf in sparse_fields) {
    # count values that are present and not empty text
    if (sf %in% names(animal)) {
      sparse_filled[[sf]] <- sparse_filled[[sf]] +
        sum(!is.na(animal[[sf]]) & animal[[sf]] != "")
    } # end field present
  } # end per sparse field

  ## dates covered and time zones used
  tot$first_date <- min(c(tot$first_date, as.Date(min(deps$deploymentStart, na.rm = TRUE))), na.rm = TRUE)
  tot$last_date  <- max(c(tot$last_date, as.Date(max(deps$deploymentEnd, na.rm = TRUE))), na.rm = TRUE)
  tot$timezones  <- union(tot$timezones, dp$temporal$timeZone)

  ## covariate gaps for the two families the skill warns about
  for (fam in c("FLII_", "days_since_recent_fire_")) {
    # every buffer scale of this family
    cols <- grep(paste0("^", fam), names(covs), value = TRUE)
    for (cl in cols) {
      # missing values and rows, summed across projects
      prev <- cov_na[[cl]]
      if (is.null(prev)) prev <- c(na = 0, n = 0)
      cov_na[[cl]] <- prev + c(na = sum(is.na(covs[[cl]])), n = nrow(covs))
    } # end per column
  } # end per family

  ### Where the media files live
  ## classify filePath by its form, which decides whether wildobs_media_download() can fetch it
  fp <- med$filePath
  is_wildobs <- grepl("^https?://data\\.wildobs\\.org\\.au", fp)
  is_gcs     <- grepl("^gs://", fp)
  is_digivol <- grepl("volunteer\\.ala\\.org\\.au", fp)
  is_local   <- grepl("^[A-Za-z]:[\\\\/]|^/", fp)
  tot$file_wildobs <- tot$file_wildobs + sum(is_wildobs)
  tot$file_gcs     <- tot$file_gcs + sum(is_gcs)
  tot$file_digivol <- tot$file_digivol + sum(is_digivol)
  tot$file_local   <- tot$file_local + sum(is_local)
  tot$file_other   <- tot$file_other + sum(!(is_wildobs | is_gcs | is_digivol | is_local))
  tot$file_public  <- tot$file_public + sum(med$filePublic %in% TRUE)
  # fileName should be withheld for every public row
  tot$file_name_filled <- tot$file_name_filled + sum(!is.na(med$fileName))

  # drop this project before fetching the next
  rm(dp, deps, covs, obs, med, animal)
  gc(verbose = FALSE)
} # end per open project

## a share as a percentage with one decimal, for readable metrics
pct <- function(x, n) sprintf("%.1f", 100 * x / n)

## record the totals
metrics <- add_metric(metrics, "rows_deployments", tot$deployments)
metrics <- add_metric(metrics, "rows_covariates", tot$covariates)
metrics <- add_metric(metrics, "rows_observations", tot$observations)
metrics <- add_metric(metrics, "rows_media", tot$media)
for (ty in c("animal", "blank", "human", "vehicle", "unknown")) {
  metrics <- add_metric(metrics, paste0("pct_obs_", ty), pct(tot[[ty]], tot$observations))
} # end per type
metrics <- add_metric(metrics, "obs_obscured", tot$obs_obscured)
metrics <- add_metric(metrics, "pct_animal_obs_obscured", pct(tot$obs_obscured_animal, tot$animal))
metrics <- add_metric(metrics, "projects_with_obscured", tot$projects_with_obscured)
metrics <- add_metric(metrics, "obscured_categories",
                      paste(names(obscured_categories), obscured_categories, sep = "=", collapse = "; "))
metrics <- add_metric(metrics, "pct_obs_join_deployment", pct(tot$obs_joined, tot$observations))
metrics <- add_metric(metrics, "obs_id_duplicates_unobscured", tot$obs_id_dupes_unobscured)
metrics <- add_metric(metrics, "pct_animal_class_human", pct(tot$class_human, tot$animal))
metrics <- add_metric(metrics, "pct_animal_class_machine", pct(tot$class_machine, tot$animal))
metrics <- add_metric(metrics, "pct_animal_count_na", pct(tot$animal_count_na, tot$animal))
metrics <- add_metric(metrics, "pct_animal_rank_coarser_than_species", pct(tot$rank_coarser, tot$animal))
metrics <- add_metric(metrics, "taxa_recorded_open_projects", length(tot$taxa))
for (sf in sparse_fields) {
  metrics <- add_metric(metrics, paste0("pct_animal_filled_", sf), pct(sparse_filled[[sf]], tot$animal))
} # end per sparse field
metrics <- add_metric(metrics, "date_first", tot$first_date)
metrics <- add_metric(metrics, "date_last", tot$last_date)
metrics <- add_metric(metrics, "timezones", paste(sort(tot$timezones), collapse = ", "))
# covariate gaps as a range across buffer scales, per family
for (fam in c("FLII_", "days_since_recent_fire_")) {
  # the missing share of each column in this family
  shares <- vapply(cov_na[grep(paste0("^", fam), names(cov_na))],
                   function(v) 100 * v[["na"]] / v[["n"]], numeric(1))
  metrics <- add_metric(metrics, paste0("pct_deployments_na_", fam),
                        if (length(shares) == 0) "absent" else
                          sprintf("%.1f to %.1f", min(shares), max(shares)))
} # end per family
metrics <- add_metric(metrics, "pct_media_wildobs_https", pct(tot$file_wildobs, tot$media))
metrics <- add_metric(metrics, "pct_media_gcs", pct(tot$file_gcs, tot$media))
metrics <- add_metric(metrics, "pct_media_digivol", pct(tot$file_digivol, tot$media))
metrics <- add_metric(metrics, "pct_media_local_path", pct(tot$file_local, tot$media))
metrics <- add_metric(metrics, "pct_media_other", pct(tot$file_other, tot$media))
metrics <- add_metric(metrics, "pct_media_filePublic", pct(tot$file_public, tot$media))
metrics <- add_metric(metrics, "media_fileName_filled", tot$file_name_filled)
metrics <- add_metric(metrics, "download_failures", length(failed))


#
##
### What the analysis workflow does with obscured records ----
## run the resampling step on the first open project that has obscured records,
## first as downloaded, then with those records filtered out
pipeline_note <- "no open project has obscured records"
# for each open project
for (id in open_ids) {
  # tables only, which is all the workflow needs
  dp <- wildobs_dp_download(api_key = api_key, project_ids = id, media = FALSE)[[1]]
  # skip projects without obscured records
  if (!any(grepl("^obscured_for_", dp$data$observations$deploymentID))) next

  ## build the inputs the way the README workflow does: deployments plus covariates
  deps <- dp$data$deployments
  covs <- dp$data$covariates
  covs <- merge(deps, covs, by = intersect(names(deps), names(covs)))
  # spatial_hexagon_generator() needs a source column; use the project when it is absent
  if (!"source" %in% names(covs)) covs$source <- covs$projectName
  # one 1 km2 grid is enough to exercise the step
  covs_cells <- spatial_hexagon_generator(data = covs, scales = 1e6)
  obs <- dp$data$observations

  ## as downloaded, obscured records included
  as_downloaded <- tryCatch({
    resample_covariates_and_observations(covs = covs_cells, obs = obs, individuals = "max")
    "runs"
  }, error = function(e) paste("stops:", trimws(conditionMessage(e))))

  ## after keeping only observations whose deployment is in the table
  obs_kept <- obs[obs$deploymentID %in% covs_cells$deploymentID, ]
  filtered <- tryCatch({
    resample_covariates_and_observations(covs = covs_cells, obs = obs_kept, individuals = "max")
    "runs"
  }, error = function(e) paste("stops:", trimws(conditionMessage(e))))

  # record the outcome without naming species
  pipeline_note <- sprintf(
    "%s: as downloaded, resampling %s | after dropping the %d obscured rows, resampling %s",
    id, as_downloaded, nrow(obs) - nrow(obs_kept), filtered)
  # one project is enough
  break
} # end search for a project with obscured records
metrics <- add_metric(metrics, "pipeline_obscured_check", pipeline_note)


#
##
### Write the census and compare with the last one ----

## find the most recent earlier census, if any
earlier <- sort(list.files(out_dir, pattern = "^census_\\d{8}_metrics\\.csv$", full.names = TRUE))
earlier <- earlier[!grepl(stamp, earlier)]
# compare metric by metric with the newest earlier census
changes <- NULL
if (length(earlier) > 0) {
  # read the previous metrics as text
  prev <- utils::read.csv(earlier[length(earlier)], colClasses = "character")
  # line them up by metric name
  both <- merge(prev, metrics, by = "metric", all = TRUE, suffixes = c("_before", "_now"))
  # keep only the metrics whose value moved
  changes <- both[is.na(both$value_before) | is.na(both$value_now) |
                    both$value_before != both$value_now, ]
} # end comparison

## a pilot run only checks the script works, so print and stop before writing
if (is_pilot) {
  print(metrics)
  stop("Pilot run finished; nothing written. Unset CENSUS_PILOT_IDS for the full census.")
} # end pilot

## save the metrics for the next comparison
utils::write.csv(metrics, file.path(out_dir, sprintf("census_%s_metrics.csv", stamp)),
                 row.names = FALSE)

## and a readable markdown report
md <- c(
  sprintf("# Public API census, %s", format(Sys.Date(), "%Y-%m-%d")),
  "",
  "Measured with `dev/public_skill_census.R` through the public API (`api_key`), using",
  "WildObsR functions only. Aggregate numbers only; no coordinates or obscured species.",
  "",
  "| Metric | Value |",
  "|---|---|",
  sprintf("| %s | %s |", metrics$metric, gsub("\\|", "/", metrics$value)),
  ""
)
# list download failures, if any
if (length(failed) > 0) {
  md <- c(md, "## Download failures", "", paste("-", failed), "")
} # end failures
# and what moved since the last census
if (!is.null(changes)) {
  md <- c(md, sprintf("## Changed since %s", basename(earlier[length(earlier)])), "",
          "| Metric | Before | Now |", "|---|---|---|",
          sprintf("| %s | %s | %s |", changes$metric, changes$value_before, changes$value_now), "")
} # end changes
writeLines(md, file.path(out_dir, sprintf("census_%s.md", stamp)))
message(sprintf("Census written to %s", file.path(out_dir, sprintf("census_%s.md", stamp))))
