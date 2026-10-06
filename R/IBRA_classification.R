#' Determine Interim Biogeographic Regionalisation for Australia (IBRA) bio-regions and subregions
#'
#' This function identifies the IBRA bio-region and sub-region for a set of coordinates using the IBRA7 subregions layer bundled with the package (\code{\link{ibra}}). It performs a spatial join, and if any coordinates fall outside every subregion (e.g. just offshore), the nearest matched location's IBRA values are used instead.
#'
#' @param data A dataframe containing at least latitude and longitude columns.
#' @param lat_col A character string specifying the column name for latitude in the dataframe.
#' @param long_col A character string specifying the column name for longitude in the dataframe.
#'
#' @details
#' The function begins by verifying the presence of the specified latitude and longitude columns in the input dataframe. It then creates a spatial vector from the input coordinates and clips the IBRA shapefile to the extent of the data points for improved performance. Next, the function performs a spatial join to associate each location with its corresponding IBRA bio-region and sub-region.
#'
#' If any locations are not assigned a bio-region (i.e., have missing values), each takes the IBRA values of its nearest location that was assigned one, and every input row is returned. Finally, the function prints a summary table of the number of deployments per IBRA bio-region and sub-region and returns an updated dataframe with additional columns containing IBRA information.
#'
#' @return A dataframe with the original data and additional columns:
#' - `IBRAsubRegionName`: Name of the IBRA sub-region.
#' - `IBRAsubRegioncode`: Code of the IBRA sub-region.
#' - `IBRAbioRegionName`: Name of the IBRA bio-region.
#' - `IBRAbioRegionCode`: Code of the IBRA bio-region.
#'
#' @examples
#' data <- data.frame(
#'   deploymentID = 1:3,
#'   lat = c(-15.5, -23.2, -17.1),
#'   lon = c(145.7, 133.5, 141.8)
#' )
#' result <- ibra_classification(data, lat_col = "lat", long_col = "lon")
#'
#' @author Zachary Amir
#' @importFrom terra as.data.frame nearest extract crop intersect project vect
#' @importFrom plyr ddply summarize
#' @importFrom knitr kable
#' @export
#'

ibra_classification = function(data, lat_col, long_col) {

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

  ## grab the IBRA subregions layer bundled with the package (already validated)
  ibra = terra::vect(WildObsR::ibra)

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
  result2 = dplyr::select(result, ID, SUB_NAME_7,SUB_CODE_7, REG_NAME_7, REG_CODE_7, HECTARES)

  ## and merge w/ data_sp
  # but make sure its safe!
  # count IDs missing from either side, since a location dropped by the join must stop here
  if(length(setdiff(result2$ID, data_sp$ID)) +
     length(setdiff(data_sp$ID, result2$ID)) == 0){
    dat_sp_bioregion = merge(result2, data_sp, by = "ID")
  }else{
    stop("Not all locations were found in IBRA shapefile, please inspect this data manually.")
  } # end merging condition

  ## Locations that fall outside every subregion (e.g. just offshore) take the
  ## IBRA values of their nearest location that did land in one
  # the IBRA columns to borrow
  ibra_cols = c("SUB_NAME_7", "SUB_CODE_7", "REG_NAME_7", "REG_CODE_7", "HECTARES")
  # flag the rows that missed every subregion
  is_na = is.na(dat_sp_bioregion$REG_NAME_7)
  if(any(is_na)){

    # cant borrow from neighbours if there are none
    if(all(is_na)){
      stop("None of the provided coordinates fall inside an IBRA7 subregion.\n",
           "Please check that ", lat_col, " and ", long_col, " hold decimal-degree coordinates in Australia.")
    } # end all NA condition

    ## give us an update
    message(sum(is_na), " locations produced NA values for bio-region. These values will be replaced with their nearest neighbors.")

    # make spatial points for the unmatched and the matched locations
    na_sp = terra::vect(dat_sp_bioregion[is_na, ], geom = c(long_col, lat_col), crs = "EPSG:4326", keepgeom = TRUE)
    ok_sp = terra::vect(dat_sp_bioregion[!is_na, ], geom = c(long_col, lat_col), crs = "EPSG:4326", keepgeom = TRUE)

    # find the nearest matched location for each unmatched one
    near = terra::nearest(na_sp, ok_sp)
    # convert those positions back to rows of dat_sp_bioregion
    to_row = which(is_na)[near$from_id]
    from_row = which(!is_na)[near$to_id]
    # and copy the IBRA values across, row by row, so shared neighbours are handled correctly
    dat_sp_bioregion[to_row, ibra_cols] = dat_sp_bioregion[from_row, ibra_cols]

  } # end NA condition

  ## remove the spatial part of the dataframe
  dat_bioregion = terra::as.data.frame(dat_sp_bioregion, geom = FALSE)

  ## now re-name columns to be informative
  names(dat_bioregion)[grepl("SUB_NAME", names(dat_bioregion))] = "IBRAsubRegionName"
  names(dat_bioregion)[grepl("SUB_CODE", names(dat_bioregion))] = "IBRAsubRegioncode"
  names(dat_bioregion)[grepl("REG_NAME", names(dat_bioregion))] = "IBRAbioRegionName"
  names(dat_bioregion)[grepl("REG_CODE", names(dat_bioregion))] = "IBRAbioRegionCode"

  ## and delete superfluous cols
  dat_bioregion$ID = NULL

  ## isolate the key results to be printed.
  check = plyr::ddply(dat_bioregion, c("IBRAbioRegionName", "IBRAsubRegionName"), plyr::summarize,
                number_of_DeploymentIDs = length(deploymentID))
  # display
  print(knitr::kable(check[order(check$number_of_DeploymentIDs, decreasing = T),], row.names = F))

  ## Finally, return the updated dataframe w/ IBRA info
  return(dat_bioregion)

} # end function

# clean up for testing
# rm(check, dat_bioregion, dat_sp_bioregion, data, data_extent, data_sp, ibra, ibra_clipped,
#    is_na, na_sp, ok_sp, near, to_row, from_row, result, result2, lat_col, long_col)
