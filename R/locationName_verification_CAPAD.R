#' Verify Location Names Using the Collaborative Australian Protected Areas Database
#'
#' This function verifies whether a set of coordinates intersects with the Collaborative Australian Protected Areas Database (CAPAD) 2022 shapefile and assigns the corresponding protected area's name. For coordinates that do not match any protected area, the nearest protected area name is determined using Euclidean distance.
#'
#' @param dep A dataframe containing coordinates (in latitude and longitude), and a unique identifier for each sampling location.
#' @param capad_file_path A character string specifying the file path to the CAPAD shapefile.
#' Defaults to `~/Dropbox/ECL spatial layers repository/Australian spatial layers GIS data/AUS/CAPAD_Terrestrial_land_use/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Terrestrial/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Terrestrial.shp`.
#'
#' @details
#' The function performs the following steps:
#' 1. Imports the CAPAD shapefile and standardizes its column names.
#' 2. Converts the input dataframe `dep` into a spatial object using latitude and longitude.
#' 3. Re-projects the spatial object to match the CAPAD shapefile's CRS.
#' 4. Determines the intersection of the coordinates with the CAPAD shapefile.
#' 5. For coordinates with no match, finds the nearest protected area using Euclidean distance and prints out warning messages if the updated location name is far from the original coordinate (1-5 km, 5-10 km, >10 km).
#' 6. Returns the modified dataframe with a new column `CAPADlocationName` containing the matched or nearest protected area's name.
#'
#' #' @return A dataframe with the original columns and two additional columns:
#' - `CAPADlocationName`: The name of the protected area assigned to each coordinate. Names are formatted to replace spaces with underscores.
#' - `CAPADminDistance`: The minimum distance (in meters) to the nearest feature in the CAPAD shapefile. Defaults to zero is a value is produced and is useful for inspecting values that generated NA.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' dep <- data.frame(
#'   locationID = c("Loc1", "Loc2", "Loc3"),
#'   Latitude = c(-35.5, -23.2, -30.7),
#'   Longitude = c(149.0, 133.5, 141.8)
#' )
#' updated_dep <- locationName_verification_CAPAD(dep)
#' head(updated_dep)
#' }
#'
#' @note
#' The CAPAD shapefile must be downloaded and available at the specified `capad_file_path`. If you have a CAPAD shapefile saved in a different locaiton on your computer, please edit the path accordingly.
#' This function assumes the `NAME` and `TYPE_ABBR` fields exist in the CAPAD shapefile for location name extraction.
#'
#' @importFrom terra distance extract crop ext makeValid intersect project crs as.data.frame vect
#'
#' @author Tom Bruce & Zachary Amir
#'
#' @export
locationName_verification_CAPAD <- function(dep, capad_file_path = "~/Dropbox/ECL spatial layers repository/Australian spatial layers GIS data/AUS/CAPAD_Terrestrial_land_use/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Terrestrial/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Terrestrial.shp") {

  ## Import the CAPAD terrestrial database which is a nationwide database of protected areas and their names.
  layer_data <- terra::vect(file.path(capad_file_path))  # Assuming shapefile format

  # Rename columns to our standardised format.
  layer_data$name_extract <- layer_data$NAME
  # Now paste the type at the end of the name
  layer_data$name_extract <- paste(layer_data$name_extract, layer_data$TYPE_ABBR, sep = "_")
  layer_data$name_extract <- gsub(" ","_",layer_data$name_extract)

  ## Make sure dep is a data frame and not a tibble to work w/ terra functions
  dep = terra::as.data.frame(dep)

  #Create an ID row for our dep_file to help with sites that have duplicate values of placename
  #e.g. TB's work will have a duplicate placename, but a road and bush camera.
  dep$ID = seq_len(nrow(dep))

  # Extract the CRS information for the layer using terra's crs() function
  crs_info <- terra::crs(layer_data)

  # Check for correct capitalization of Latitude and Longitude
  lat_col <- if ("Latitude" %in% names(dep)) "Latitude" else "latitude"
  lon_col <- if ("Longitude" %in% names(dep)) "Longitude" else "longitude"

  ## create copies of lat/longs to be used to make spatial vector
  dep$long2 = dep[, lon_col]
  dep$lat2 = dep[, lat_col]

  # Convert dep data.frame to a SpatVector using the appropriate lat/lon column names
  dep_sp =  terra::vect(dep , geom = c("long2", "lat2"), "EPSG:4326")

  # reproject to the proper CRS
  dep_sp <- terra::project(dep_sp,crs_info)

  # # Now check for intersection
  # intersection_result <- terra::intersect(dep_sp, layer_data)
  #
  # # Check if the intersection result has any features
  # if (nrow(intersection_result) > 0) {
  #   print("Provided locations and CAPAD shape file intersect.")
  # } else {
  #   print("Provided locations and CAPAD shape file do not intersect.")
  #   dep[1:nrow(dep),"CAPADlocationName"] = as.character(NA)
  #   return(dep)
  #   stop("Returning NA values for CAPAD locationName")
  # }
  ### Not useful b/c there are more rigorous NA checks below.

  # Clean layer_data geometries using terra's makeValid function
  layer_data <- terra::makeValid(layer_data)

  # Determine the extent of your data points
  data_extent <- terra::ext(dep_sp)

  # Crop the IBRA shapefile to the extent of your data points to speed up the process.
  layer_data <- terra::crop(layer_data, data_extent)

  ## check if the crop makes it lose features
  # Check if the intersection result has any features
  if(! nrow(layer_data) > 0) {
    print("Provided locations and CAPAD shape file do not intersect.")
    dep[1:nrow(dep),"CAPADlocationName"] = as.character(NA)
    return(dep)
    stop("Returning NA values for CAPAD locationName")
  } # end 0 crop condition

  # Perform the spatial join using extract
  result <- terra::extract(layer_data, dep_sp) # this one takes a min

  #We then need to retain one value per id as we can't have multiple values.
  #We will use IUCN status as our breaking factor, only retaining the highest value.

  # Define the IUCN hierarchy as a factor with levels in descending order of protection
  iucn_levels <- c("Ia", "Ib", "II", "III", "IV", "V", "VI", "NR", "NAS", NA)

  # Convert IUCN column to a factor with specified levels
  result$IUCN <- factor(result$IUCN, levels = iucn_levels, ordered = TRUE)

  # Arrange the data by id.y and IUCN in descending order of protection level
  result <- result[order(result$id.y, result$IUCN), ]

  # Remove duplicates in id.y, keeping the row with the highest IUCN level for each unique id.y
  result <- result[!duplicated(result$id.y), ]

  # Bring back our placenames from deployments
  result$ID <- dep$ID[result$id.y]

  # Extract the column you need for the result and modify it if necessary
  result_column <- ifelse(is.na(result$name_extract), # check if the value is NA
                          NA, # Leave NA is true,
                          gsub(" ", "_", paste0(result$name_extract, ""))) # but otherwise swap out spaces for underscore in the name.
  # and one more annoying typo fix
  result_column = gsub("_-_", "_", result_column)

  # Create a new column in the deployments to add the location names for all coordinates.
  dep$area_name =  result_column

  ## Now we will use Euclidean Distance to determine the nearest protected area in the CAPAD database.
  # Check if there are any missing values in area_name
  if (any(is.na(dep$area_name))) {

    # Identify the rows with missing values
    missing_rows <- which(is.na(dep$area_name))

    ## give us an update
    print(paste("There are", length(missing_rows), "coordinates that did not produce a value from the CAPAD shape file. Now calcuating Euclidian distances to the nearest feature in the shape file."))

    ## assign a new column to save the minimum distance between the coordinate and new feature
    dep$CAPADminDistance = as.numeric(NA)
    dep[-missing_rows, "CAPADminDistance"] = 0

    #missing_row = 160
    # Loop over each missing row
    for (missing_row in missing_rows) {

      # Extract coordinates of the point with missing area_name
      missing_coords <- c(dep[missing_row, lon_col], dep[missing_row, lat_col])
      missing_placename <- dep[missing_row, "deploymentID"]

      # Create a SpatVector for the point with the missing area_name
      missing_point <- terra::vect(data.frame(longitude = missing_coords[1], latitude = missing_coords[2]),
                                   geom = c("longitude", "latitude"), crs = "EPSG:4326")

      #Re-project to match the CAPAD shapefile
      missing_point = terra::project(missing_point,crs_info)

      # Calculate distances to all components in CAPAD layer_data and find the nearest one
      distances <- terra::distance(missing_point, layer_data)
      min_index <- which.min(distances)
      min_distance <- distances[min_index]

      # Retrieve the nearest component's information from layer_data
      closest_match <- layer_data[min_index, ]
      closest_match_df <- as.data.frame(closest_match)

      # Check if the nearest neighbor distance is within a threshold between 1-5 km
      if (min_distance > 1000 &
          min_distance < 5000) {
        cat("Note: Distance to nearest feature in CAPAD shape file is between 1-5 km for locationID",
            missing_placename, "to", closest_match_df$name_extract, "\n")
      } # end 1-5

      # Check if the nearest neighbor distance is within a threshold between 5-10 km
      if (min_distance > 5000 &
          min_distance < 10000) {
        cat("Note: Distance to nearest feature in CAPAD shape file is between 5-10 km for locationID",
            missing_placename, "to", closest_match_df$name_extract, "\n")
      } # end 5-10

      # Check if the nearest neighbor distance is greater than 10 km
      if (min_distance > 10000) {
        cat("Note: Distance to nearest feature in CAPAD shape file is greater than 10km for locationID",
            missing_placename, "to", closest_match_df$name_extract, "\n")
      } # end > 10 km

      ## dont forget to clean up name_extract!
      closest_match_df$name_extract = gsub(" ", "_", closest_match_df$name_extract)
      closest_match_df$name_extract = gsub("_-_", "_", closest_match_df$name_extract)

      # Update the missing area_name with the nearest feature's area_name from layer_data
      dep$area_name[missing_row] <- closest_match_df$name_extract
      # and save the distance away from the feature
      dep$CAPADminDistance[missing_row] = min_distance

    } # end per missing row
  } # end NA condition

  ## rename the column to be more informative
  names(dep)[grepl("area_name", names(dep))] = "CAPADlocationName"

  # Return the modified dataframe
  return(dep) #This should always be last.

} # Closing brace for the overall function

# testing clean up
# rm(closest_match, closest_match_df, distances, intersection_result, check,
#    layer_data, missing_point, capad_file_path, crs_info, iucn_levels, dep_sp,
#    lat_col, lon_col, min_distance, min_index, result_column, data_extent,
#    missing_coords, missing_placename, missing_row, missing_rows, result, dep)
