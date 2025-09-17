##### WildObsR user testing script
#### Unlike machine based unit testing, this is for humans to test out code
### and ensure it works logically and is useful for research.

## This will not be sent to CRAN, so feel free to go wild here!

### Zachary Amir, Z.Amir@uq.edu.au
## code initalized: March 31st, 2025
## last updated: September 17th, 2025

# start fresh!
rm(list = ls())

## this will load all functions if you are working out of a R library for this package!
devtools::load_all()
# library(WildObsR)

## load other relevant libraries
library(tidyverse)

##### Data analysis workflow ####

#
##
### create a query using wildobs_mongo_query()

### First, gather relevant information to inform the query

## read in environment file with confidential DB access info
# readRenviron("inst/config/.Renviron.prod.admin") # remote access for admin, but authentification isnt working!
# readRenviron("inst/config/.Renviron.prod.ro") # remote access for read-only
# Contact Zach if you want access to this file.

# ## load information from enviromnet
# HOST <- Sys.getenv("HOST")
# PORT <- Sys.getenv("PORT")
# DATABASE <- Sys.getenv("DATABASE")
# USER <- Sys.getenv("USER")
# PASS <- Sys.getenv("PASS")
#
# ## combine all the information into a database-url to enable access
# db_url <- sprintf("mongodb://%s:%s@%s:%s/%s", USER, PASS, HOST, PORT, DATABASE)
# rm(USER, PASS, HOST, PORT, DATABASE)

## Instead of the DB_url, test out the new admin api key
# readRenviron("inst/config/.Renviron.admin.api")
# api_key = Sys.getenv("API_KEY")

## Test out general use API key
api_key = "f4b9126e87c44da98c0d1e29a671bb4ff39adcc65c8b92a0e7f4317a2b95de83"

## Define a temporal range to query
temporal = list(minDate = as.Date("2010-01-01"),
            maxDate = as.Date("2023-12-01")) # data from 2021-2023

## Define a spatial bounding box to query
spatial = list(xmin = 137.995, xmax = 153.552, ymin = -28.999, ymax = -9.142)

## Define taxonomic query
taxonomic = c() #c("Casuarius casuarius")

## Define sampling design query
samplingDesign = c() # dont care right now

## Define contributor query only want data from WildObsR team
contributors = c() #c("Zachry Amir", "Tom Bruce")

## Define sharing status
tabularSharingPreference = c("open", "partial", "closed")

## Gather relevant project_ids using the mongo query function
project_ids = wildobs_mongo_query(api_key = api_key, #db_url = db_url,
                                  temporal = temporal,
                                  spatial = spatial,
                                  taxonomic = taxonomic,
                                  samplingDesign = samplingDesign,
                                  contributors = contributors,
                                  tabularSharingPreference = tabularSharingPreference
                                  )
## OR, can access all open data when not querying
# project_ids = wildobs_mongo_query(db_url = db_url)

## Who did we get?
sort(project_ids)
# for testing
project_ids = c("QLD_Wet_Tropics_feral_cats_Bruce_2019-20_WildObsID_0007", "QLD_Simpson_Desert_Greenville_2010-15_WildObsID_0002")  # closed and partial data!

## clean up query info
rm(tabularSharingPreference, contributors, samplingDesign, taxonomic, spatial, temporal)

#
##
### use the output to access data from wildobs_dp_download()
# set media to FALSE to make a quicker download
start = Sys.time()
dp_list = wildobs_dp_download(api_key = api_key, #db_url = db_url,
                              project_ids = project_ids, media = F)
end = Sys.time()

## how long did this take?
end-start # 5 min now

# inspect class to make sure its a DP
class(dp_list[[1]])
# make sure all project_ids were downloads
length(dp_list) == length(project_ids) # MUST BE T
# check if we have data resources
frictionless::resources(dp_list[[1]]) # no media, but the rest is there!

#
##
### Extract deployments covaraites and observations
covs = list();deps = list();obs = list() # store results here
for(i in 1:length(dp_list)){
  ## add a condition to skip partial and closed datasets (i.e., no resources)
  if(length(frictionless::resources(dp_list[[i]])) == 0){
    # skip to the next if not open
    next
  }
  # covaraties
  c = frictionless::read_resource(dp_list[[i]], "covariates")
  c$source = dp_list[[i]]$id
  covs[[i]] = c
  # deployments
  d = frictionless::read_resource(dp_list[[i]], "deployments")
  d$source = dp_list[[i]]$id
  deps[[i]] = d
  # observations
  o = frictionless::read_resource(dp_list[[i]], "observations")
  o$source = dp_list[[i]]$id
  obs[[i]] = o
}
rm(d,i,c,o)
## Combine into one df
obs = do.call(rbind, obs)
deps = do.call(rbind, deps)
covs = do.call(rbind, covs)
# inspect
table(deps$projectName)

# ### b/c there is so much data in Taggart, remove a few Dgs for speed
# sample_dg = sample(unique(covs$deploymentGroups[grepl("Taggart", covs$deploymentGroups)]), 7) # remove 7 at random
# covs = covs[! covs$deploymentGroups %in% sample_dg, ]
# deps = deps[deps$deploymentGroups %in% covs$deploymentGroups, ]
# rm(sample_dg)

## check that its safe to merge
verify_col_match(deps, covs, "deploymentID") # all g!

## merge the two based on shared cols
covs = merge(deps, covs, by = names(covs)[names(covs) %in% names(deps)])
# dont need deps anymore
rm(deps)

# and dont forget to thin obs to the relevant deploymentIDs now that we have excluded some
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

# set wd to where external data file lives.
wd = "/Users/zachary_amir/Dropbox/WildObs master folder/WildObs GitHub Data Storage/data_tools/"
# list availible files
ver_files = list.files(wd)
ver_files = ver_files[grepl("taxonomy", ver_files)] # only want species data
# Extract the date parts of the filenames and convert to integers
dates <- as.numeric(gsub("verified_taxonomy_(\\d{8})", "\\1", ver_files))
# Order the filenames based on the extracted dates (from most recent to least recent)
ver_file <- ver_files[order(dates, decreasing = TRUE)]
# Import the most recent file
taxa_dp = frictionless::read_package(paste(wd, ver_file[1], "/datapackage.json", sep = ""))
rm(ver_file, ver_files, dates)

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

