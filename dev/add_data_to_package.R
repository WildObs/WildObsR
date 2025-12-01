##### Add data to R package
#### Primarily using the function usethis::use_data()
### and saving a traceable log about how and which data was uploaded

### Zachary Amir, Z.Amir@uq.edu.au
### Last updated: Dec 1st, 2025

## start fresh
rm(list = ls())

## Set up personal file paths to dropbox
personal_path = "/Users/zachary_amir/Dropbox/" # for Zach


##### Species traits data #####

## Create working directory to where species data lives
wd = paste(personal_path, "WildObs master folder/WildObs GitHub Data Storage/data_tools/", sep = "")

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


