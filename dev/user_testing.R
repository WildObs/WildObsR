##### WildObsR user testing script
#### Unlike machine based unit testing, this is for humans to test out code
### and ensure it works logically and is useful for research.

## This will not be sent to CRAN, so feel free to go wild here!

### Zachary Amir, Z.Amir@uq.edu.au
## code initalized: March 31st, 2025

# start fresh!
rm(list = ls())

## this will load all functions if you are working out of a R library for this package!
devtools::load_all()
# library(WildObsR)

## load other relevant libraries
library(tidyverse)

##### Data access workflow ####

#
##
### create a query using wildobs_mongo_query()

### First, grab the DB connection string
db_url <- Sys.getenv("MONGODB_PROD_RO_URL")
# db_url <- Sys.getenv("MONGODB_PUB_ADMIN_URL")
# db_url <- Sys.getenv("MONGODB_LOCAL_RO_URL")
# db_url <- Sys.getenv("MONGODB_DEV_RO_URL")

### First, grab the API key from R environ
# api_key <- Sys.getenv("WILDOBSR_API_KEY")


## Define a temporal range to query
temporal = list() #list(minDate = as.Date("2000-01-01"), maxDate = as.Date("2026-12-01")) #

## Define a spatial bounding box to query
spatial = spatial = list(xmin = 112.9, ymin = -43.7, xmax = 153.6, ymax = -9.1) # all of AUS

## Define taxonomic query
taxonomic = c() #c("Casuarius casuarius")

## Define sampling design query
samplingDesign <- c() #c("simpleRandom", "opportunistic", "systematicRandom")

## Define contributor query only want data from WildObsR team
contributors = c() #c("Zachry Amir", "Tom Bruce")
# contributors <- c("Emma Spencer")

## Define sharing status
tabularSharingPreference = c("open", "partial", "closed")

## Gather relevant project_ids using the mongo query function
project_ids = wildobs_mongo_query(db_url = db_url, # api_key = api_key,
                                  temporal = temporal,
                                  spatial = spatial,
                                  taxonomic = taxonomic,
                                  samplingDesign = samplingDesign,
                                  contributors = contributors,
                                  tabularSharingPreference = tabularSharingPreference
                                  )

## Who did we get?
sort(project_ids)
# subset to few datasets for testing
project_ids <- project_ids[grepl("WildObsID_0002|WildObsID_0040|WildObsID_00034|WildObsID_0053", project_ids)]
## clean up query info
rm(tabularSharingPreference, contributors, samplingDesign, taxonomic, spatial, temporal)

#
##
### use the output to access data from wildobs_dp_download()
# set media to FALSE to make a quicker download
start = Sys.time()
dp_list = wildobs_dp_download(db_url = db_url, #api_key = api_key,  
                              project_ids = project_ids, media = F,
                              metadata_only = F)
end = Sys.time()

## how long did this take?
end-start # 10 seconds for metadata only, 1.5 min for full dat w/ general API
# almost 10 min for 2 DPs w/ full media.
rm(end, start)

# inspect class to make sure its a DP
class(dp_list[[1]])
# make sure all project_ids were downloads
length(dp_list) == length(project_ids) # MUST BE T


#
##
### Extract metadata across all projects per project, safely
# working example for contributors:
cont_table = extract_metadata(dp_list, "contributors")
glimpse(cont_table)
anyNA(cont_table$title) #MUST BE F

# works for single dp too and/or multiple DPs
cont_table_1 = extract_metadata(dp_list[[1]], c("contributors", "relatedIdentifiers"))
cont_table_1$contributors
cont_table_1$relatedIdentifiers

## Need to extract metadata per project safely b/c trouble w/ sources, project, WildObsMetadata, and spatial, so inspect those
# verify problems and tweak function in the background.
check = extract_metadata(dp_list, "sources") # working now!
check = extract_metadata(dp_list, "project") # working now!
check = extract_metadata(dp_list, "WildObsMetadata") # working now!
check = extract_metadata(dp_list, "spatial") # working now!
check = extract_metadata(dp_list, "temporal") # working w/ updated structure! 
check = extract_metadata(dp_list, "licenses") # working 
check = extract_metadata(dp_list, "relatedIdentifiers") # working
check = extract_metadata(dp_list, "references") # working, but nothing present. 
## check all at once to ensure no failure
check = extract_metadata(dp_list) # all g
rm(check)

#
##
### Extract deployments covaraites observations and media if present 
covs = list();deps = list();obs = list(); med = list() # store results here
for(i in 1:length(dp_list)){
  ## add a condition to skip partial and closed datasets (i.e., no resources)
  if(length(frictionless::resources(dp_list[[i]])) == 0){
    # skip to the next if not open
    next
  }
  # covaraties
  c = frictionless::read_resource(dp_list[[i]], "covariates")
  covs[[i]] = c
  # deployments
  d = frictionless::read_resource(dp_list[[i]], "deployments")
  deps[[i]] = d
  # observations
  o = frictionless::read_resource(dp_list[[i]], "observations")
  obs[[i]] = o
  # media, but only if present
  if("media" %in% frictionless::resources(dp_list[[1]])){
    m = frictionless::read_resource(dp_list[[i]], "media")
    med[[i]] = m
  }
}
rm(d,i,c,o,m)
## Combine into one df
obs   = do.call(rbind, obs)
deps  = do.call(rbind, deps)
covs  = do.call(rbind, covs)
media = do.call(rbind, med)
# inspect
table(deps$projectName)

## check that its safe to merge
verify_col_match(deps, covs, "deploymentID") # all g!

## merge both deps and covs based on shared cols
covs = merge(deps, covs, by = names(covs)[names(covs) %in% names(deps)])
# dont need deps anymore
rm(deps)


##### Access camera trap images directly ------

### access both public and private (google cloud storage) images
## This works b/c I (Zachary Amir) have authorized access to these project on WI

## inspect public and non-public files
head(media$filePath[!media$filePublic]) # GSC 
head(media$filePath[media$filePublic])  # iTIR

### Test for private GCS pics 
## gather my authenticated google cloude authorization token (set up in google CLI before)
token <- system2("gcloud", c("auth", "print-access-token"), stdout = TRUE)

## subset media for non-public animal images
media_sub <- media[!media$filePublic & 
  media$observationID %in% obs$observationID[which(obs$observationType == "animal")], ]
## and only download 25
# media_sub <- head(media_sub, 25)
## try 200 now
media_sub <- head(media_sub, 200)

## run the download function for private media, specifying the GCS token
res <- wildobs_media_download(media_sub, out_dir = "~/Desktop/wi_test", gcs_token = token)
## what was returned?
res[, c("mediaID", "downloadStatus", "downloadNote", "localPath")] # all good! 
table(res$downloadStatus)
unique(res$downloadNote[res$downloadStatus == "failed"]) # only should be 404/3's 
rm(res)

### Test for public pics 
## grab observationIDs for interesting animals
obs_ids <- obs$observationID[obs$scientificName %in% c("Varanus rosenbergi", 
                              "Varanus varius", "Turnix melanogaster", "Felis catus",
                              "Phascolarctos cinereus")]
## subset media for them 
media_sub <- media[media$observationID %in% obs_ids, ]

## run the download function w/out the token
res <- wildobs_media_download(media_sub, out_dir = "~/Desktop/wi_test")
res[, c("mediaID", "downloadStatus", "downloadNote", "localPath")] # seemingly good 
table(res$downloadStatus)
unique(res$downloadNote[res$downloadStatus == "failed"]) # only should be 404/3's 

##### Change camtrapDP versions --------

## we can "downgrade" the WildObs extensions to match canonical camtrapdp 
canon <- as_camtrapdp(dp_list[[1]]) # test w/ defaults
# inspect the dp 
canon_dp <- canon$camtrapdp
canon_dp #got the tables
check <- frictionless::read_resource(canon_dp, "deployments")
glimpse(check) #present! 

# inspect what changes
canon$report

# inspect verification
canon$validation

## recheck by keeping covariates
canon <- as_camtrapdp(dp_list[[1]], keep_covariates = TRUE)
canon$camtrapdp # covs are present 
test = frictionless::read_resource(canon$camtrapdp, "observations")
glimpse(test) # it works! 

## try saving as darwin Core
# camtrapdp::write_dwc(canon$camtrapdp, directory = "~/Downloads/testing_dwc_datapackage/")
# it works! 

##### Test recombined short deployments into long deployments -----

## This function operates per DP, so inspect one a time
dp <- dp_list[[3]]
# recombine it (any overlap warnings print here, naming the deployments)
dp_long <- recombine_deployment_groups(dp)

## pull every resource from both versions
deps_short <- frictionless::read_resource(dp, "deployments")
deps_long  <- frictionless::read_resource(dp_long, "deployments")
covs_short <- frictionless::read_resource(dp, "covariates")
covs_long  <- frictionless::read_resource(dp_long, "covariates")
obs_short  <- frictionless::read_resource(dp, "observations")
obs_long   <- frictionless::read_resource(dp_long, "observations")
cw         <- frictionless::read_resource(dp_long, "deployment_crosswalk")

#
##
### Inspect effort and duration 

## first, gather baseline stats about deployment length, short vs long
dur_short <- deps_short |>
  # one row per deployment already, so just add the duration
  mutate(duration = as.numeric(difftime(deploymentEnd, deploymentStart, units = "days")))
# and the same for the recombined deployments
dur_long <- deps_long |>
  mutate(duration = round(as.numeric(difftime(deploymentEnd, deploymentStart, units = "days")), 1))
# compare the spread
summary(dur_short$duration)
summary(dur_long$duration)
# no deployment should be longer than 100 days in the short version 
max(dur_short$duration) <= 100 # should be TRUE

## how each long deployment was built from its shorter deployments
chains <- cw |>
  # sort short deps in time order within each long deployment
  arrange(deploymentID, pieceStart) |>
  group_by(deploymentID) |>
  summarise(
    # how many 100-day deployments went into it
    n_pieces = n(),
    # the days the camera actually ran
    effort_days = sum(as.numeric(difftime(pieceEnd, pieceStart, units = "days"))),
    # the days from first start to last end
    span_days = as.numeric(difftime(max(pieceEnd), min(pieceStart), units = "days")),
    # the biggest gap between one short deployment ending and the next starting
    max_gap_days = if (n() > 1) {
      max(as.numeric(difftime(pieceStart[-1], pieceEnd[-n()], units = "days")))
    } else {
      0
    },
    .groups = "drop"
  )
# how many short deployments per long deployment
table(chains$n_pieces) # should be mostly few cams, occasionally many 
hist(chains$n_pieces)  # should be Poission 

# total effort is unchanged
isTRUE(all.equal(sum(chains$effort_days), sum(dur_short$duration))) # MUST BE TRUE
# span always covers the effort
all(chains$span_days >= chains$effort_days - 1e-6) # MUST BE TRUE
# multi-season groups are 30 days apart or less, so no gap should exceed that
all(chains$max_gap_days <= 30) # should be TRUE
# inspect any gap that break it (negative gaps mean two pieces overlap in one
filter(chains, max_gap_days > 30 | max_gap_days < 0)

#
##
### Ensure nothing was lost or invented 

## row counts
# new depID
nrow(obs_short) == nrow(obs_long) # MUST BE TRUE
nrow(covs_long) == nrow(deps_long) # MUST BE TRUE
verify_col_match(covs_long, deps_long, "deploymentID") # MUST BE TRUE
# old depID
nrow(cw) == nrow(deps_short) # MUST BE TRUE
setequal(cw$original_deploymentID, deps_short$deploymentID) # MUST BE TRUE
anyDuplicated(cw$original_deploymentID) == 0 # MUST BE TRUE, since several short deps go in one long one
# every observation still joins to a deployment
all(obs_long$deploymentID %in% deps_long$deploymentID) # MUST BE TRUE

## animal detections per long deployment and species, rebuilt from the short deps 
by_piece <- obs_short |>
  # animal records only
  filter(observationType == "animal") |>
  # point each record at the long deployment its short one now belongs to
  mutate(deploymentID = cw$deploymentID[match(deploymentID, cw$original_deploymentID)]) |>
  # count records per long deployment and species
  count(deploymentID, scientificName) |>
  # sort so the comparison doesnt depend on row order
  arrange(deploymentID, scientificName)
# the same counts straight from the recombined package
by_long <- obs_long |>
  filter(observationType == "animal") |>
  count(deploymentID, scientificName) |>
  arrange(deploymentID, scientificName)
# must be identical
isTRUE(all.equal(as.data.frame(by_piece), as.data.frame(by_long))) # MUST BE TRUE

## a quick helper to count observations outside their deployment's window
count_outside <- function(obs, deps) {
  obs |>
    # attach each deployment's start and end
    left_join(select(deps, deploymentID, deploymentStart, deploymentEnd), by = "deploymentID") |>
    # count records that fall outside it
    summarise(outside = sum(eventStart < deploymentStart | eventStart > deploymentEnd,
                            na.rm = TRUE)) |>
    pull(outside)
} # end count_outside
# baseline in the original package, then the recombined one
count_outside(obs_short, deps_short) 
count_outside(obs_long, deps_long) # should be no larger than the baseline

#
##
### Verify temporal metadata is unchanged

## ensure overall project-level metadata is consistent 
identical(dp$temporal[c("start", "end", "timeZone")],
          dp_long$temporal[c("start", "end", "timeZone")]) # MUST BE TRUE

## a quick helper to line up temporal blocks against the deployments they describe
temporal_vs_deps <- function(dp_x) {
  # grab the temporal metadata
  tmp <- dp_x$temporal
  # a deploymentGroup block is any entry whose value is a list
  blocks <- tmp[vapply(tmp, is.list, logical(1))]
  # one row per block, as dates
  block_df <- data.frame(
    deploymentGroups = names(blocks),
    block_start = as.Date(vapply(blocks, function(b) as.character(b[["start"]]), character(1))),
    block_end = as.Date(vapply(blocks, function(b) as.character(b[["end"]]), character(1))),
    stringsAsFactors = FALSE
  )
  # the deployment dates per group, in the package's local time zone
  dep_dates <- frictionless::read_resource(dp_x, "deployments") |>
    mutate(deploymentGroups = as.character(deploymentGroups)) |>
    group_by(deploymentGroups) |>
    summarise(dep_start = as.Date(min(deploymentStart), tz = tmp$timeZone),
              dep_end = as.Date(max(deploymentEnd), tz = tmp$timeZone),
              .groups = "drop")
  # line them up, keeping groups missing from either side
  full_join(block_df, dep_dates, by = "deploymentGroups") |>
    # positive differences mean the block is wider than the deployments
    mutate(start_diff_days = as.numeric(dep_start - block_start),
           end_diff_days = as.numeric(block_end - dep_end))
} # end temporal_vs_deps

## run it on both versions
temporal_short <- temporal_vs_deps(dp)
temporal_long <- temporal_vs_deps(dp_long)
# every group in the data has a block and vice versa
anyNA(temporal_long$block_start) == FALSE # MUST BE TRUE
anyNA(temporal_long$dep_start) == FALSE # MUST BE TRUE
# no block ends before it starts
all(temporal_long$block_start <= temporal_long$block_end) # MUST BE TRUE
# blocks that dont match their deployments; compare with the baseline,
# since any mismatch in the original carries straight through
filter(temporal_short, start_diff_days != 0 | end_diff_days != 0)
filter(temporal_long, start_diff_days != 0 | end_diff_days != 0)
# extract_metadata still reads it, one row per multi-season group
nrow(extract_metadata(dp_long, "temporal")) == nrow(temporal_long) # MUST BE TRUE

## clean up
rm(dp, dp_long, deps_short, deps_long, covs_short, covs_long, obs_short, obs_long, cw,
   dur_short, dur_long, chains, by_piece, by_long, temporal_short, temporal_long,
   count_outside, temporal_vs_deps)

##### Occupancy/Abundance modelling functions #####

# Dont forget to thin obs to the relevant deploymentIDs now that we have excluded some
obs = obs[obs$deploymentID %in% covs$deploymentID, ]

## format datetimes
covs$deploymentEnd = as.POSIXct(covs$deploymentEnd, format = "%Y-%m-%dT%H:%M:%S%z")
covs$deploymentStart = as.POSIXct(covs$deploymentStart, format = "%Y-%m-%dT%H:%M:%S%z")



# inspect deploymentTags
unique(covs$deploymentTags) # fix one typo
covs$deploymentTags[covs$deploymentTags %in% c("bait:none | predatorManagement: No management",
                                               " | predatorManagement: No management")] = "bait:none | predatorManagement: No management"

## Extract all tags into separate columns in covaraites
covs_with_tags <- covs %>%
  # create new cols to track original rows and split tags
  mutate(
    row_id = dplyr::row_number(),  # To track original rows
    tag_list = stringr::str_split(deploymentTags, "\\s*\\|\\s*")
  ) %>%
  tidyr::unnest(tag_list) %>%
  # Remove blanks
  filter(!is.na(tag_list), tag_list != "") %>%
  # split  keys and values
  separate(tag_list, into = c("tag_key", "tag_value"), sep = ":", fill = "right") %>%
  mutate(
    tag_key = str_trim(tag_key),
    tag_value = str_trim(tag_value)
  ) %>%
  pivot_wider(
    id_cols = row_id,
    names_from = tag_key,
    values_from = tag_value
  ) %>%
  right_join(
    covs %>% mutate(row_id = dplyr::row_number()),
    by = "row_id"
  ) %>%
  select(-row_id)

# inspect
table(covs_with_tags$bait)
table(covs_with_tags$predatorManagement) # can clean one value here
covs_with_tags$predatorManagement[covs_with_tags$predatorManagement %in% c("Control deployments no management", "No management")] = "No management"
table(covs_with_tags$experiment)

## now save these extra column names that we will use later
tag_cols = setdiff(names(covs_with_tags), names(covs))


## format their datetimes
dt_cols = c("eventStart", "eventEnd", "observationStart", "observationEnd")
for(i in 1:length(dt_cols)){
  obs[[dt_cols[i]]] = as.POSIXct(obs[[dt_cols[i]]], format = "%Y-%m-%dT%H:%M:%S%z")
}
rm(dt_cols, i, obs1,obs2)
dplyr::glimpse(obs)

## inspect species
sort(table(obs$scientificName))
# Canis dingo, Felis catus, Trichosurus caninus, Pitta versicolor, Geopelia humeralis
sp = c("Felis catus", # common in both sources
       "Ocyphaps lophotes", "Pitta versicolor", # somewhat common but detected in different sources
       "Tachyglossus aculeatus") # rare but in both sources
#
##
### Import species traits information to contextualize these critters

# # set wd to where external data file lives.
# wd = file.path(Sys.getenv("WILDOBS_DROPBOX"), "WildObs master folder/WildObs GitHub Data Storage/data_tools/")
# # list availible files
# ver_files = list.files(wd)
# ver_files = ver_files[grepl("taxonomy", ver_files)] # only want species data
# # Extract the date parts of the filenames and convert to integers
# dates <- as.numeric(gsub("verified_taxonomy_(\\d{8})", "\\1", ver_files))
# # Order the filenames based on the extracted dates (from most recent to least recent)
# ver_file <- ver_files[order(dates, decreasing = TRUE)]
# # Import the most recent file
# taxa_dp = frictionless::read_package(paste(wd, ver_file[1], "/datapackage.json", sep = ""))
# rm(ver_file, ver_files, dates)

## load the data internally
taxa_dp = data(taxonomy_dp)

### Extract the species traits from the DP
frictionless::resources(taxa_dp) # two options
traits = frictionless::read_resource(taxa_dp, "species_traits")

## check if species are present
setdiff(sp, traits$binomial_verified) # all are present!
## reduce to the relevant species
traits = traits[traits$binomial_verified %in% sp, ]
# check for NAs in home range
anyNA(traits$home_range_km2) # true!
traits[, c("binomial_verified", "home_range_km2")] # three are missing.
# convert to m2
traits$home_range_m2 = ceiling(traits$home_range_km2 * 1e6 / 1e6) * 1e6 # but round to the nearest 1km2
# # Make k'gari dingo home-ranges same as biggest option
# traits$home_range_m2[traits$binomial_verified == "Canis dingo"] <-  max(traits$home_range_m2, na.rm = T)
# and all others default to 1km
traits$home_range_m2[is.na(traits$home_range_m2)] = 1e6

## Save this info as scales to resample
scales = unique(sort(traits$home_range_m2))

## use spatial hexagon generator to make hexagons suitable for each species
covs_cellIDs = spatial_hexagon_generator(data = covs_with_tags, scales = scales)
# inspect
names(covs_cellIDs[grepl('cellID', names(covs_cellIDs))]) # two new cols, good!
length(unique(covs_cellIDs$cellID_1km2)) # 368
length(unique(covs_cellIDs$cellID_6km2)) # 602, checks out!

# Clean up enviro
rm(covs, taxa_dp, sp)

#
##
### Decide which covariates a relevant to include in re-sampled data

# ## Inspect the schema of covariates
# covs_schema = frictionless::get_schema(dp_list[[1]], "covariates")
#
# # Convert schema fields to data frame for easier manipulation
# library(rlang) # to use  %||%
# covs_schema_df <- purrr::map_df(covs_schema$fields, ~{
#   # Extract known fields safely
#   tibble::tibble(
#     name = .x$name %||% NA,
#     description = .x$description %||% NA,
#     type = .x$type %||% NA,
#     example = as.character(.x$example %||% NA),
#     required = .x$constraints$required %||% NA,
#     unique = .x$constraints$unique %||% NA,
#     enum = paste(.x$constraints$enum %||% NA, collapse = "|")
#   )
# })
#
# ## inspect
# dplyr::glimpse(covs_schema_df)
# ## which variables are just characters
# unique(covs_schema_df$name[covs_schema_df$type == "string"])
# # inspect non-deployment values
# table(covs_cellIDs$IBRAsubRegionName)
# table(covs_cellIDs$Olson_global_ecoregion) # only 1 level
# table(covs_cellIDs$source) # 2 levels here!

## Define columns for mode aggregation (example: 'source' and 'habitat')
mode_cols_covs <- names(covs_cellIDs)[grepl("source|IBRAsubRegionName", names(covs_cellIDs))] # leaving sub region for shits n gigs

## Set the method for aggregating the total number of individuals detected:
individuals <- "max"  # Alternative: "sum"

## Specify observation-level covariate variables derived from deployments.
# These variables capture information that varies in space and time.
obs_covs <- c("baitUse", "featureType", "setupBy", "cameraModel", "cameraDelay",
              "cameraHeight", "cameraDepth", "cameraTilt", "cameraHeading",
              "detectionDistance", "deploymentTags")

## run the new spatial resampling function
start = Sys.time()
resampled_data = resample_covariates_and_observations(covs = covs_cellIDs, obs = obs,
                                                      individuals = individuals,
                                                      mode_cols_covs = mode_cols_covs,
                                                      obs_covs = obs_covs)
end = Sys.time()
end-start # 2.2 min for 3 projects. Much faster than expected

## Finally, combine species and scales into small DF
sp_scales = data.frame("scientificName" = traits$binomial_verified,
                       "home_range_m2" = traits$home_range_m2)
# now convert the scales to km2 values
sp_scales$scale_name = paste(sp_scales$home_range_m2/1e6, "km2", sep = "")

#
##
### Extract UMFs per critter

## thin sp_scales to only include

# Repeat per species
umf_list = list()
for(i in 1:length(sp_scales$scientificName)){

  ## grab sp and scale
  sp = sp_scales$scientificName[i]
  scale = sp_scales$scale_name[sp_scales$scientificName == sp]

  ## extract re-sampled data at the correct scale
  resamp_obs = resampled_data$spatially_resampled_observations[[paste("cellID", scale, sep = "_")]]
  resamp_covs = resampled_data$spatially_resampled_covariates[[paste("cellID", scale, sep = "_")]]
  # and rename column to be standardized
  names(resamp_obs)[grepl("cellID", names(resamp_obs))] = "cellID"
  names(resamp_covs)[grepl("cellID", names(resamp_covs))] = "cellID"

  ## create a simple on/off trail variable based off featureType
  resamp_obs$trailStatus[grepl("road|trail", resamp_obs$featureType)] = "onTrail"
  resamp_obs$trailStatus[resamp_obs$featureType == ""] = "offTrail"

  # ## finally, grab all locations where the critter was detected
  # lands = unique(resamp_covs$locationName[resamp_covs$cellID %in% resamp_obs$cellID[resamp_obs$scientificName == sp]])
  # # and thin data to those locationNames
  # resamp_covs = resamp_covs[resamp_covs$locationName %in% lands, ]
  # resamp_obs = resamp_obs[resamp_obs$cellID %in% resamp_covs$cellID, ]

  ### Thin covariates to variables at relevant scale
  # Set your target scale
  target_scale <- as.numeric(gsub("km2", "", scale))

  # Get all km2-related columns
  km2_cols <- names(resamp_covs)[stringr::str_detect(names(resamp_covs), "_\\d+km2$|_point$")]

  # Extract base variable name and numeric scale
  scale_info <- tibble::tibble(
    col = km2_cols
  ) %>%
    dplyr::mutate(
      # Extract scale number, or assign 0 for "point"
      scale_val = dplyr::if_else(stringr::str_detect(col, "_point$"), 0,
                          as.numeric(stringr::str_extract(col, "\\d+(?=km2$)"))),
      # Extract base name by removing suffix
      base = stringr::str_remove(col, "_(point|\\d+km2)$")
    )

  # For each base variable, keep the column closest to the target scale
  closest_cols <- scale_info %>%
    dplyr::group_by(base) %>%
    dplyr::slice_min(base::abs(scale_val - target_scale), n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  # Grab modal cols
  mode_cols = names(resamp_covs[grepl("mode_", names(resamp_covs))])
  ## and combine with extra important columns
  site_covs = c(tag_cols,         # deploymentTags calculated earlier
                closest_cols$col, # correct spatial scale
                "deploymentGroups", "locationName") # context.
  ## and grab the observation covariates
  obs_covs = c("numberDeploymentsActiveAtDate", "trailStatus", mode_cols)
  # TESTING
  # obs_covs = c()


  ## run models for both occupancy and abundance
  states = c("occupancy", "abundance")
  temp = list()# store matricies here
  for(s in 1:length(states)){
    ## run the custom function to create the matrix
    mat = matrix_generator(obs = resamp_obs, covs = resamp_covs, dur = 100, w = 2,
                           site_covs = site_covs, obs_covs = obs_covs,
                           all_locationNames = F, scientificNames = sp,
                           type = states[s], individuals = "max",cap_count = F)
    ## save this info as a UMF object
    # then construct the UMF
    if(states[s] == "occupancy"){
      if(length(obs_covs)>0){
        umf = unmarked::unmarkedFrameOccu(y = mat[[1]]$detection_matrix,
                                          siteCovs = mat[[1]]$site_level_covariates,
                                          obsCovs = mat[[1]]$observation_level_covariates)
      }else{
        # dont include obs_covs if not present!
        umf = unmarked::unmarkedFrameOccu(y = mat[[1]]$detection_matrix,
                                          siteCovs = mat[[1]]$site_level_covariates)
      } # end obs_cov condition
    } # end occu condition
    if(states[s] == "abundance"){
      if(length(obs_covs)>0){
        umf = unmarked::unmarkedFramePCount(y = mat[[1]]$detection_matrix,
                                          siteCovs = mat[[1]]$site_level_covariates,
                                          obsCovs = mat[[1]]$observation_level_covariates)
      }else{
        # dont include obs_covs if not present!
        umf = unmarked::unmarkedFramePCount(y = mat[[1]]$detection_matrix,
                                          siteCovs = mat[[1]]$site_level_covariates)
      } # end obs_cov condition
    } # end abund condition

    # save the matrix
    temp[[s]] =umf
    names(temp)[s] = states[s]
  } # end per state

  ## Save temp per critter
  umf_list[[i]] = temp
  names(umf_list)[i] = sp

} # end per critter
# clean up
rm(i,s,mat,temp,states,obs_covs, resamp_obs, resamp_covs, site_covs, closest_cols,
   covs_with_tags, scale_info, traits, km2_cols, mode_cols, mode_cols_covs,
   scale, scales, sp, start, end, target_scale)

## inspect results
names(umf_list) # all species present
names(umf_list$`Pitta versicolor`) # both abundance and occupancy!

## lets run some models?
# cats abundance
umf = umf_list$`Ocyphaps lophotes`$occupancy
# inspect data
summary(umf)
plot(umf)

mod1 = unmarked::pcount(~1 ~1, umf)





# make sure were good
verify_col_match(resamp_obs, resamp_covs, "cellID_3km") # all good! Now use this to make a matrix.

#### use matrix generation function to format data for occu mods
matrix_list_occu = matrix_generator(obs = resamp_obs, covs = resamp_covs, dur = 100, w = 5, site_covs = c("Avg_FLII_3km2", "Avg_precipitation_3km", "Avg_altitude_3km2", "locationName"), obs_covs = c("numberDeploymentsActiveAtDate", "trailStatus"), all_locationNames = T, scientificNames = "Casuarius casuarius", type = "occupancy", individuals = "max")

### make an occupancy UMF
casso_occu_umf = unmarked::unmarkedFrameOccu(y = matrix_list_occu$Casuarius_casuarius$detection_matrix,
                                             siteCovs = matrix_list_occu$Casuarius_casuarius$site_level_covariates,
                                             obsCovs = matrix_list_occu$Casuarius_casuarius$observation_level_covariates)
## test the thang
occuRN_m1 = unmarked::occuRN(~numberDeploymentsActiveAtDate + trailStatus ~ Avg_precipitation_3km + locationName, casso_occu_umf)
summary(occuRN_m1)

occu_m1 = unmarked::occu(~numberDeploymentsActiveAtDate + trailStatus ~ Avg_precipitation_3km + locationName, casso_occu_umf)
summary(occu_m1)

## try abundance
matrix_list_abund = matrix_generator(obs = resamp_obs, covs = resamp_covs, dur = 100, w = 5, site_covs = c("Avg_FLII_3km2", "Avg_precipitation_3km", "Avg_altitude_3km2", "locationName"), obs_covs = c("numberDeploymentsActiveAtDate", "trailStatus"), all_locationNames = T, scientificNames = "Casuarius casuarius", type = "abundance", individuals = "sum")

### make an abundance umf
casso_abund_umf = unmarked::unmarkedFramePCount(y = matrix_list_abund$Casuarius_casuarius$detection_matrix,
                                                siteCovs = matrix_list_abund$Casuarius_casuarius$site_level_covariates,
                                                obsCovs = matrix_list_abund$Casuarius_casuarius$observation_level_covariates)
## test the mod
abund_m1 = unmarked::pcount(~numberDeploymentsActiveAtDate + trailStatus ~ Avg_precipitation_3km + locationName, casso_abund_umf)
summary(abund_m1)
