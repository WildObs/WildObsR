#' Verify Location Names Using State-Specific Landscape Shapefiles
#'
#' This function verifies camera deployment locations by intersecting provided coordinates with landscape shapefiles
#' specific to Australian states. It operates at multiple levels, starting with a nationwide CAPAD dataset and then
#' checking National Parks, State Forests, and other land types based on the provided state.
#' Optionally, the function can also use Euclidean distance to find the nearest known landscape feature for deployments
#' with missing values and visualize results using a Leaflet map.
#' #'
#' @details
#' The function searches for landscape shapefiles in a specified directory, organized by state. To perform a national-level
#' check, it loads the CAPAD Terrestrial shapefile, found at:
#' \code{"~/Dropbox/ECL spatial layers repository/Australian spatial layers GIS data/AUS/CAPAD_Terrestrial_land_use/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Terrestrial/"}
#' This path should be modified if the CAPAD data is stored in a different directory. The function follows these steps:
#' \enumerate{
#'   \item Users specify a state (e.g., "QLD", "NSW") to search for relevant shapefiles in the \code{Landscapes_path} directory.
#'   \item The function processes National Parks, State Forests, and other land types within each state, renaming columns to
#'   standardized formats for consistency.
#'   \item For deployments without a landscape designation, users can opt to find the nearest landscape feature based on
#'   Euclidean distance, with a configurable threshold.
#' }
#'
#' @param dep The deployments tabular data containing information about each camera deployment, including latitude and longitude columns (either `Latitude`/`Longitude` or `latitude`/`longitude`).
#' @param dist A logical value (`TRUE` or `FALSE`). If `TRUE`, the function uses Euclidean distance to find the nearest known landscape feature for deployments with missing values.
#'
#' @return A new column appended to the deployments data frame with an updated `locationName` column, indicating the verified landscape (e.g., National Park, State Forest) for each deployment.
#'
#' @importFrom terra vect project extract makeValid distance crs
#' @importFrom stringr str_to_title str_extract
#' @examples
#' # Example usage:
#' dep <- data.frame(
#'   deploymentID = c("dep1", "dep2"),
#'   Latitude = c(-33.8688, -27.4698),
#'   Longitude = c(151.2093, 153.0251)
#' )
#' dep$locationName <- locationName_verification_national(dep, dist = TRUE)
#' table(deps$locationName)
#'
#' @author Tom Bruce
#'
#' @export
locationName_verification_national <- function(dep, dist) {

  ##TB is now adding a check using the nationwide data layer first THEN progress to more 'regional ones'
  #Check the CAPAD terrestrial database which is a nationwide database of protected area and their names.
  # Open the layer using terra::vect
  layer_data <- terra::vect(file.path("~/Dropbox/ECL spatial layers repository/Australian spatial layers GIS data/AUS/CAPAD_Terrestrial_land_use/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Terrestrial/", paste0("Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Terrestrial.shp")))  # Assuming shapefile format

  layer_name = "CAPAD"

  # Rename columns to our standardised format.
  layer_data$name_extract <- layer_data$NAME
  # Now paste the type at the end of the name
  layer_data$name_extract <- paste(layer_data$name_extract, layer_data$TYPE_ABBR, sep = "_")
  layer_data$name_extract <- gsub(" ","_",layer_data$name_extract)

  #Create our results list
  results_list <- list()

  #Assign area_name
  dep$area_name = NA

  #Create an ID row for our dep_file to help with sites that have duplicate values of placename e.g. TB's work will have a duplicate placename, but a road and bush camera.
  dep$ID = seq_len(nrow(dep))

  #Rename layer_data to what we are expecting
  gdb_data = layer_data

  # Extract the CRS information for the layer using terra's crs() function
  crs_info <- terra::crs(gdb_data)

  # Check for correct capitalization of Latitude and Longitude
  lat_col <- if ("Latitude" %in% names(dep)) "Latitude" else "latitude"
  lon_col <- if ("Longitude" %in% names(dep)) "Longitude" else "longitude"

  #create dep_unique
  # Keep only unique rows based on placename, X, and Y columns
  #dep_unique<- dep[!duplicated(dep[, c("placename", "X", "Y")]), ]


  # Convert dep data.frame to a SpatVector using the appropriate lat/lon column names
  dep_sp <- terra::vect(dep , geom = c(lon_col, lat_col), "EPSG:4326")  # crs_info is the projection you want to use (e.g. "EPSG:4326")

  # reproject to the proper CRS
  dep_sp <- terra::project(dep_sp,crs_info)

  # # Check the extents again to confirm they align for testing
  # print(terra::ext(dep_sp))
  # print(terra::ext(gdb_data))

  # Now check for intersection
  intersection_result <- terra::intersect(dep_sp, gdb_data)

  # Check if the intersection result has any features
  if (nrow(intersection_result) > 0) {
    print("dep_sp and gdb_data intersect.")
  } else {
    print("dep_sp and gdb_data do not intersect.")
  }

  # Convert gdb_data to a terra SpatVector obect to let terra functions work with it
  #gdb_data = terra::vect(gdb_data)

  # Clean gdb_data geometries using terra's makeValid function
  gdb_data <- terra::makeValid(gdb_data)

  # Perform the spatial join using terra's `intersect` or `extract`
  result <- terra::extract(gdb_data, dep_sp)

  #We then need to retain one value per id as we can't have multiple values. We will use IUCN status as our breaking factor retaining the highest value.
  # Define the IUCN hierarchy as a factor with levels in descending order of protection
  iucn_levels <- c("Ia", "Ib", "II", "III", "IV", "V", "VI", "NR", "NAS", NA)

  # Convert IUCN column to a factor with specified levels
  result$IUCN <- factor(result$IUCN, levels = iucn_levels, ordered = TRUE)

  # Arrange the data by id.y and IUCN in descending order of protection level
  result <- result[order(result$id.y, result$IUCN), ]

  # Remove duplicates in id.y, keeping the row with the highest IUCN level for each unique id.y
  result <- result[!duplicated(result$id.y), ]

  #We need our placenames
  result$ID <- dep$ID[result$id.y]  # Replace `ID` with the correct indexing column if different


  # Extract the column you need for the result and modify it if necessary
  result_column <- ifelse(is.na(result$name_extract), NA, gsub(" ", "_", paste0(result$name_extract, "")))

  # Store the result in the results_list
  results_list[[layer_name]] <- result_column

  # Check if dep$area_name is NA
  na_rows <- is.na(dep$area_name)

  # Update NA values with the results of the current layer
  dep$area_name[na_rows] <- result_column[na_rows]

  # Clean up to avoid carrying unwanted objects to the next iteration
  rm(dep_sp, result, result_column)


  #Then if there are still NA's, take the nearest distance...
  ## Now we will use Euclidean Distance to determine the nearest National Park or Rural property
  # Check if NA is present
  if(dist) {  # If the user has requested to use nearest neighbor

    # Check if there are any missing values in area_name
    if (any(is.na(dep$area_name))) {

      #crs_info <- terra::crs("EPSG:4326")  # Latitude/Longitude; terra distances output in meters

      # Identify the rows with missing values
      missing_rows <- which(is.na(dep$area_name))

      #missing_row = 160
      # Loop over each missing row
      for (missing_row in missing_rows) {

        # Extract coordinates of the point with missing area_name
        missing_coords <- c(dep[missing_row, lon_col], dep[missing_row, lat_col])
        missing_placename <- dep[missing_row, "placename"]

        # Create a SpatVector for the point with the missing area_name
        missing_point <- terra::vect(data.frame(longitude = missing_coords[1], latitude = missing_coords[2]),
                                     geom = c("longitude", "latitude"), crs = "EPSG:4326")

        #Reproject to match the CAPA shapefile
        missing_point = terra::project(missing_point,crs_info)

        # Calculate distances to all components in gdb_data and find the nearest one
        distances <- terra::distance(missing_point, gdb_data)
        min_index <- which.min(distances)
        min_distance <- distances[min_index]

        # Retrieve the nearest component's information from gdb_data
        closest_match <- gdb_data[min_index, ]
        closest_match_df <- as.data.frame(closest_match)

        # Check if the nearest neighbor distance is within a threshold (e.g., 5000 meters)
        if (min_distance > 5000) {
          cat("Note: Distance to nearest feature in gdb_data is greater than 5km for placename",
              missing_placename, "to", closest_match_df$placename, "\n")
        }

        # Update the missing area_name with the nearest feature's area_name from gdb_data
        dep$area_name[missing_row] <- closest_match_df$name_extract
      }
    }
  }  # Close nearest neighbor NA filling brace

  # Return the modified dep$area_name column
  return(dep$area_name) #This should always be last.

} # Closing brace for the overall function

# testing clean up
# rm(closest_match, closest_match_df, distances, gdb_data, intersection_result,
#    layer_data, missing_point, results_list, capad_path, crs_info, dist, iucn_levels,
#    lat_col, layer_name, locationName_path, lon_col, min_distance, min_index,
#    missing_coords, missing_placename, missing_row, missing_rows, na_rows)
