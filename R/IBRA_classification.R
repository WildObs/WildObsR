#' Determine Interim Biogeographic Regionalisation for Australia (IBRA) bio-regions and subregions
#'
#'#' This function identifies the IBRA bio-region and sub-region for a set of coordinates using the IBRA7 sub- and bio-regions shapefile. It validates the geometries of the shapefile, reprojects the data for compatibility, and performs a spatial join. If any coordinates fall outside valid bio-regions, the nearest neighbor's attributes are used to fill in missing values.
#'
#' @param data A dataframe containing at least latitude and longitude columns.
#' @param lat_col A character string specifying the column name for latitude in the dataframe.
#' @param long_col A character string specifying the column name for longitude in the dataframe.
#' @param ibra_file_path A character string specifying the file path to the IBRA7 subregions shapefile. Defaults to "~/Dropbox/ECL spatial layers repository/Australian spatial layers GIS data/AUS/IBRA7_bioregions/ibra7_subregions.shp".
#'
#'#' @details
#' The function begins by verifying the presence of the specified latitude and longitude columns in the input dataframe. It then creates a spatial vector from the input coordinates and clips the IBRA shapefile to the extent of the data points for improved performance. Next, the function performs a spatial join to associate each location with its corresponding IBRA bio-region and sub-region.
#'
#' If any locations are not assigned a bio-region (i.e., have missing values), the nearest neighbor approach is used to calculate these values. Finally, the function prints a summary table of the number of deployments per IBRA bio-region and sub-region and returns an updated dataframe with additional columns containing IBRA information.
#'
#' Note that you must have access to the IBRA7 shapefile for this function to work. Either ensure the default pathway is accurrate and downloaded on your computer, or edit the pathway to work for your local computer.
#'
#' @return A dataframe with the original data and additional columns:
#' - `IBRAsubRegionName`: Name of the IBRA sub-region.
#' - `IBRAsubRegioncode`: Code of the IBRA sub-region.
#' - `IBRAbioRegionName`: Name of the IBRA bio-region.
#' - `IBRAbioRegionCode`: Code of the IBRA bio-region.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' data <- data.frame(
#'   deploymentID = 1:3,
#'   lat = c(-15.5, -23.2, -17.1),
#'   lon = c(145.7, 133.5, 141.8)
#' )
#' result <- ibra_classification(data, lat_col = "lat", long_col = "lon")
#' }
#'
#' @author Zachary Amir
#' @import terra
#' @importFrom plyr ddply summarize
#' @importFrom knitr kable
#' @export
#'

ibra_classification = function(data, lat_col, long_col, ibra_file_path = "~/Dropbox/ECL spatial layers repository/Australian spatial layers GIS data/AUS/IBRA7_bioregions/ibra7_subregions.shp") {

  ## First, ensure lat and long cols are present in data
  if(! lat_col %in% names(data)){
    stop(paste("The latitude column you have specified:", lat_col, "is not present in the provided dataframe.\n",
               "Please ensure you have specified the correct column name for latitude before using this function."))
  } # end lat conditional
  if(! long_col %in% names(data)){
    stop(paste("The longitude column you have specified:", long_col, "is not present in the provided dataframe.\n",
               "Please ensure you have specified the correct column name for longitude before using this function."))
  } # end long conditional

  ## Make sure data is a data frame and not a tibble to work w/ terra functions
  data = as.data.frame(data)

  #Create an ID row to help with surveys that have duplicate values of placename
  ## e.g. TB and ZDA work will have a duplicate placenames for road and bush cameras
  data$ID = seq_len(nrow(data))

  # create copies of lat/longs to be used to make spatial vector
  data$long2 = data[, long_col]
  data$lat2 = data[, lat_col]

  ## Create a spatial vector
  data_sp =  terra::vect(data , geom = c("long2", "lat2"), "EPSG:4326")

  ## import IBRA SUB-regions shape file
  ibra = terra::vect(file.path(ibra_file_path))

  ## make sure the shapefile has valid geometries.
  ibra = terra::makeValid(ibra)

  ## re-project our data so it matches the shape file
  data_sp = terra::project(data_sp, terra::crs(ibra))

  # ## preform the intersection to verify they are overlapping
  # intersection <- terra::intersect(data_sp, ibra)
  #
  # # Check if the intersection result has any features
  # if (nrow(intersection) > 0) {
  #   print("Provided locations and IBRA7 BioRegions shapefile intersect.")
  # } else {
  #   print("Provided locations and IBRA7 BioRegions shapefile do not intersect.")
  # } # end intersection statement
  # # rm(intersection)

  # Determine the extent of your data points
  data_extent <- terra::ext(data_sp)

  # Crop the IBRA shapefile to the extent of your data points to speed up the process.
  ibra_clipped <- terra::crop(ibra, data_extent)

  # Perform the spatial join using extract
  result <- terra::extract(ibra_clipped, data_sp)

  ## now that we match, bring back the ID column from data_sp
  result$ID = data_sp$ID[result$id.y]

  ## select the relevant info from the IBRA dataset
  result2 = select(result, ID, SUB_NAME_7,SUB_CODE_7, REG_NAME_7, REG_CODE_7, HECTARES)

  ## and merge w/ data_sp
  # but make sure its safe!
  if(length(setdiff(result2$ID, data_sp$ID) +
     setdiff(data_sp$ID, result2$ID)) == 0){
    dat_sp_bioregion = merge(result2, data_sp, by = "ID")
  }else{
    stop(print("Not all locations were found in IBRA shapefile, please inspect this data manually."))
  } # end merging condition

  ## Now verify there are no NA values
  if(anyNA(dat_sp_bioregion$REG_NAME_7)){

    ## give us an update
    print(paste(nrow(dat_sp_bioregion[is.na(dat_sp_bioregion$REG_NAME_7),]), "locations produced NA values for bio-region. These values will be replaced with their nearest neighbors."))

    ## isolate which cams have NA values
    na_values = dat_sp_bioregion[which(is.na(dat_sp_bioregion$REG_NAME_7)), ]
    ## and make it a spatVect
    na_values$latitude2 = na_values[, lat_col]; na_values$longitude2 = na_values[, long_col]
    na_values = terra::vect(na_values , geom = c("longitude2", "latitude2"), "EPSG:4326")

    ## Then gather all the other cams with good values
    valid_values = dat_sp_bioregion[! is.na(dat_sp_bioregion$REG_NAME_7), ]
    ## and also make this a spatVect
    valid_values$latitude2 = valid_values[, lat_col]; valid_values$longitude2 = valid_values[, long_col]
    valid_values = terra::vect(valid_values , geom = c("longitude2", "latitude2"), "EPSG:4326")

    ## then find the nearest neighbors
    near = nearest(na_values, valid_values)
    ## replace from_id w/ na_values ID
    near$from_id = na_values$ID[near$from_id]
    ## and the same w/ to_id and valid_values
    near$to_id = valid_values$ID[near$to_id]

    ## now grab the relevant information form the valid values
    add = valid_values[valid_values$ID %in% near$to_id, c("ID", "SUB_NAME_7", "SUB_CODE_7", "REG_NAME_7",
                                                          "REG_CODE_7", "HECTARES")]
    ## and replace the ID with the missing values
    add$ID = near$from_id

    ## remove the NA cols from na_values
    na_values = na_values[, !(names(na_values) %in% c("SUB_NAME_7", "SUB_CODE_7",
                                                      "REG_NAME_7", "REG_CODE_7",
                                                      "HECTARES"))]
    # now merge add to na_values
    # but ensure its safe
    if(length(setdiff(na_values$ID, add$ID)) +
       length(setdiff(add$ID, na_values$ID)) == 0){
      # do the merge
      added_values = merge(add, na_values, by = "ID")
      added_values = as.data.frame(added_values) # no more need for spatvect
    }else{
      stop(print("A problem occurred when searching the nearest neighbor, please inspect manually."))
    } # end merging condition.

    ## Finally, replace these in the final dataset
    # first remove
    dat_sp_bioregion = dat_sp_bioregion[! dat_sp_bioregion$ID %in% added_values$ID, ]
    # and remove duplicated lat/long cols for a clean rbind
    dat_sp_bioregion$latitude2 = NULL ; dat_sp_bioregion$longitude2 = NULL
    # and then rbind
    dat_bioregion = rbind(added_values, dat_sp_bioregion)


  } # end NA condition

  ## remove duplicated lat/long cols
  dat_sp_bioregion$latitude2 = NULL ; dat_sp_bioregion$longitude2 = NULL
  # and and re-name to match NA condition values and remove the spatial part of datafram e
  dat_bioregion = terra::as.data.frame(dat_sp_bioregion, geom = F)

  ## now re-name columns to be informative
  names(dat_bioregion)[grepl("SUB_NAME", names(dat_bioregion))] = "IBRAsubRegionName"
  names(dat_bioregion)[grepl("SUB_CODE", names(dat_bioregion))] = "IBRAsubRegioncode"
  names(dat_bioregion)[grepl("REG_NAME", names(dat_bioregion))] = "IBRAbioRegionName"
  names(dat_bioregion)[grepl("REG_CODE", names(dat_bioregion))] = "IBRAbioRegionCode"

  ## and delete superfluous cols
  dat_bioregion$ID = NULL

  ## isolate the key results to be printed.
  check = ddply(dat_bioregion, .(IBRAbioRegionName, IBRAsubRegionName), summarize,
                number_of_DeploymentIDs = length(deploymentID))
  # display
  print(knitr::kable(check[order(check$number_of_DeploymentIDs, decreasing = T),], row.names = F))

  ## Finally, return the updated dataframe w/ IBRA info
  return(dat_bioregion)

} # end function

# clean up for testing
# rm(add, added_values, check, dat_bioregion, dat_sp_bioregion, data, data_extent, data_sp, ibra,
# ibra_clipped, intersection, na_values, near, result, result2, valid_values, lat_col, long_col, ibra_file_path)
