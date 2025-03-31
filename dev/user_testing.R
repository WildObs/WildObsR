##### WildObsR user testing script
#### Unlike machine based unit testing, this is for humans to test out code
### and ensure it works logically and is useful for research.

## This will not be sent to CRAN, so feel free to go wild here!

### Zachary Amir, Z.Amir@uq.edu.au
## code initalized: March 31st, 2025
## last updated: XXX


##### Data analysis workflow ####

## create a query using wildobs_mongo_query()

## use the output to access data from wildobs_dp_download()

## generate cell IDs from the covariates using spatial_hexagon_generator()

## spatially resample covs and obs using spatialyl_resample_covariates() and spatiall_resample_observations() (respectivly)

## generate detection history matrix from resampled data using XX

## combine into an unmarked frame occu/pcount and then run models!

