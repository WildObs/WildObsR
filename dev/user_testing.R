##### WildObsR user testing script
#### Unlike machine based unit testing, this is for humans to test out code
### and ensure it works logically and is useful for research.

## This will not be sent to CRAN, so feel free to go wild here!

### Zachary Amir, Z.Amir@uq.edu.au
## code initalized: March 31st, 2025
## last updated: April 2nd, 2025

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
project_ids = wildobs_mongo_query(temporal = list(minDate = as.Date("2023-01-01"), maxDate = as.Date("2023-12-01")))
project_ids

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
# a = read_resource(dp1, "covariates") # not in DB yet!!
a = read_resource(dp1, "deployments")
# save data source
a$source = dp1$contributors[[1]]$tag
# second
b = read_resource(dp2, "deployments")
# save data source
b$source = dp2$contributors[[1]]$tag

## combine into one
deps = rbind(a,b)
rm(a,b)

## format datetimes
deps$deploymentEnd = as.POSIXct(deps$deploymentEnd)
deps$deploymentStart = as.POSIXct(deps$deploymentStart) ## Surprised these arent already formatted....

## use the custom function to make cellIDs
scales = c("1km" = 1074.6, "3km" = 1861.2) # hexagon apothems
deps_cellIDs = spatial_hexagon_generator(deps, scales)
# inspect
sort(unique(deps_cellIDs$cellID_3km)) # all good
rm(deps_cellIDs, dp_list, dp1, dp2, deps)

#
##
###
#### Manually import DP w/ covariates
dp = frictionless::read_package("/Users/zachary_amir/Dropbox/WildObs master folder/WildObs GitHub Data Storage/data_clean/Step 4 DataPackages/QLD_Dwyers_Scrub_ANIM3018_2023/datapackage.json")

# Extract resources from the data package
covs <- frictionless::read_resource(dp, "covariates")
obs  <- frictionless::read_resource(dp, "observations")
deps <- frictionless::read_resource(dp, "deployments")

## Add data source to the covariates
covs$source <- dp$contributors[[1]]$tag

## Generate spatial hexagons based on the provided scales
covs <- WildObsR::spatial_hexagon_generator(covs, scales)
rm(scales)
# merge deps to covs
cols <- names(deps)[names(deps) %in% names(covs)]
covs <- merge(deps, covs, by = cols)  # Note: This overwrites the original covs data

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
resampled_data = resample_covariates_and_observations(covs, obs, individuals,
                                                      mode_cols_covs, obs_covs)
resamp_obs = resampled_data$spatially_resampled_observations$cellID_3km
resamp_covs = resampled_data$spatially_resampled_covariates$cellID_3km

# make sure were good
verify_col_match(resamp_obs, resamp_covs, "cellID_3km") # all good! Now use this to make a matrix.



#
##
### spatially resample the data using the new function

## generate detection history matrix from resampled data using XX

## combine into an unmarked frame occu/pcount and then run models!

