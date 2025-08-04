##### WildObsR user testing script
#### Unlike machine based unit testing, this is for humans to test out code
### and ensure it works logically and is useful for research.

## This will not be sent to CRAN, so feel free to go wild here!

### Zachary Amir, Z.Amir@uq.edu.au
## code initalized: March 31st, 2025
## last updated: August 4th, 2025

# start fresh!
rm(list = ls())

## this will load all functions if you are working out of a R library for this package!
devtools::load_all()
# library(WildObsR)

## load other relevant libraries
# library(tidyverse)

##### Data analysis workflow ####

#
##
### create a query using wildobs_mongo_query()
### CURRENTLY REQUIRES LOCAL MONGO, will update in future when remote DB is ready
project_ids = wildobs_mongo_query(temporal = list(minDate = as.Date("2022-01-01"), maxDate = as.Date("2023-12-01")))
project_ids
# select IDs for Kgari (small DPs but multiple)
project_ids = project_ids[grepl("Kgari", project_ids)]

#
##
### use the output to access data from wildobs_dp_download()
### CURRENTLY REQUIRES LOCAL MONGO, will update in future when remote DB is ready
dp_list = wildobs_dp_download(project_ids) # not a super quick function, mainly due to extracting the media resources.
# grab both as separate DPs
dp1 = dp_list[[1]]
dp2 = dp_list[[2]]
class(dp1)

#
##
### generate cell IDs from the covariates using spatial_hexagon_generator()
# make a combined covaraites from the two data packages.
a = frictionless::read_resource(dp1, "covariates")
# save data source
a$source = dp1$contributors[[1]]$tag
# second
b = frictionless::read_resource(dp2, "covariates")
# save data source
b$source = dp2$contributors[[1]]$tag

## combine into one
covs = rbind(a,b)
rm(a,b)

## format datetimes
covs$deploymentEnd = as.POSIXct(covs$deploymentEnd, format = "%Y-%m-%dT%H:%M:%S%z")
covs$deploymentStart = as.POSIXct(covs$deploymentStart, format = "%Y-%m-%dT%H:%M:%S%z")

## use the custom function to make cellIDs
scales = c(1000000, 3000000) # hexagon apothems
covs_cellIDs = spatial_hexagon_generator(covs, scales)
# inspect
sort(unique(covs_cellIDs$cellID_3km)) # all good
rm(covs_cellIDs, dp_list, dp1, dp2, covs)



#obs
obs1  <- frictionless::read_resource(dp1, "observations")
obs2  <- frictionless::read_resource(dp2, "observations")
# combine
obs = rbind(obs1,obs2)
obs[grepl("DBNP_18",obs$deploymentID) & obs$scientificName == "Casuarius casuarius" ,] # no detections at this cam anyway
# deps
dep1 <- frictionless::read_resource(dp1, "deployments")
dep2 <- frictionless::read_resource(dp2, "deployments")
# combine
deps = rbind(dep1,dep2)
# clean up
rm(covs1, covs2, obs1, obs2, dep1, dep2)

## Generate spatial hexagons based on the provided scales
scales = c("3km" = 1861.2) # hexagon apothems
covs <- WildObsR::spatial_hexagon_generator(covs, scales)
rm(scales)
# merge deps to covs
cols <- names(deps)[names(deps) %in% names(covs)]
covs <- merge(deps, covs, by = cols)  # Note: This overwrites the original covs data

### thin to landscapes where cassowaries were detected to speed up resampling
locs = obs$deploymentID[obs$scientificName == "Casuarius casuarius"]
lands = unique(covs$locationName[covs$deploymentID %in% locs])
# thin
covs = covs[covs$locationName %in% lands, ]
obs = obs[obs$deploymentID %in% covs$deploymentID, ]


## Define columns for mode aggregation (example: 'source' and 'habitat')
mode_cols_covs <- names(covs)[grepl("source|habitat", names(covs))]

## Set the method for aggregating the total number of individuals detected:
individuals <- "sum"  # Alternative: "max"

## Specify observation-level covariate variables derived from deployments.
# These variables capture information that varies in space and time.
obs_covs <- c("baitUse", "featureType", "setupBy", "cameraModel", "cameraDelay",
              "cameraHeight", "cameraDepth", "cameraTilt", "cameraHeading",
              "detectionDistance", "deploymentTags")

## run the new spatial resampling function
start = Sys.time()
resampled_data = resample_covariates_and_observations(covs, obs, individuals,
                                                      mode_cols_covs, obs_covs)
end = Sys.time()
resamp_obs = resampled_data$spatially_resampled_observations$cellID_3km
resamp_covs = resampled_data$spatially_resampled_covariates$cellID_3km

end-start # 2 min! Much faster than expected

## make a combined trail-status variable in obs
sort(table(resamp_obs$featureType))
resamp_obs$trailStatus[grepl("road|trail", resamp_obs$featureType)] = "onTrail"
resamp_obs$trailStatus[resamp_obs$featureType == ""] = "offTrail"
table(resamp_obs$trailStatus)


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

