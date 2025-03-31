##### WildObsR user testing script
#### Unlike machine based unit testing, this is for humans to test out code
### and ensure it works logically and is useful for research.

## This will not be sent to CRAN, so feel free to go wild here!

### Zachary Amir, Z.Amir@uq.edu.au
## code initalized: March 31st, 2025
## last updated: XXX

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

#
##
### spatially resample covs and obs using spatially_resample_covariates() and spatiall_resample_observations() (respectivly)

## generate detection history matrix from resampled data using XX

## combine into an unmarked frame occu/pcount and then run models!

