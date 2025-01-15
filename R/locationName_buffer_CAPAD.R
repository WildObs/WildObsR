#' Generate Location Buffers and Assign CAPAD Protected Area Names
#'
#' This function buffers deployment locations, identifies overlapping or nearby protected areas from the Collaborative Australian Protected Areas Database (CAPAD) database,
#' and assigns dominant protected area names to each buffer. It handles deployments across multiple UTM zones
#' and incorporates a nearest-neighbor approach for locations outside CAPAD boundaries.
#'
#' @param dep A dataframe containing deployment data, including columns for `latitude`, `longitude`, and `deploymentID`.
#' It may also include `UTMzone`, `X`, and `Y` coordinates. If these are missing, they will be generated using the `WildObsR::UTM_coord_generator` function.
#' @param buffer_size Numeric value representing the size of the buffer (in meters) around each deployment location.
#' @param capad_file_path File path to the CAPAD shapefile containing protected area boundaries. Defaults to ~/Dropbox/ECL spatial layers repository/, so please ensure you have access or modify the pathway to your local computer to the shapefile location.
#'
#'@details
#' This function performs the following steps:
#' \enumerate{
#'   \item Validates and generates UTM coordinates for the deployment locations, if necessary.
#'   \item Buffers deployment points within their respective UTM zones, merging results across zones.
#'   \item Dissolves overlapping buffers and assigns unique IDs to each disaggregated polygon.
#'   \item Extracts the dominant protected area name for each buffer using the CAPAD shapefile.
#'   \item For buffers that do not intersect with any protected area, determines the nearest protected area based on centroid distance.
#'   \item Assigns the final protected area name and buffer ID to the original deployment dataframe.
#' }
#'
#' @return An updated dataframe containing the original deployment data and an additional column,
#' `CAPADlocationNameBuffer`, which specifies the buffer ID and the corresponding protected area name.
#'
#' @importFrom terra vect project aggregate disagg expanse centroids intersect extract ext crop buffer
#' @importFrom WildObsR UTM_coord_generator
#' @importFrom dplyr distinct
#'
#' @examples
#' # Example usage:
#' dep <- data.frame(
#'   deploymentID = c("dep1", "dep2"),
#'   Latitude = c(-33.8688, -27.4698),
#'   Longitude = c(151.2093, 153.0251)
#' )
#'
#' # Run the function with a 10 km buffer
#' buffered_dep <- locationName_buffer_CAPAD(dep, buffer_size = 10000,
#'   capad_file_path = "~/path/to/CAPAD.shp")
#'
#' # View results
#' head(buffered_dep)
#'
#' @note This function assumes that the CAPAD shapefile is properly formatted and contains fields such as `NAME` and `TYPE_ABBR`.
#' The function uses the `terra` package for spatial processing.
#'
#' @author Tom Bruce & Zachary Amir
#' @export
locationName_buffer_CAPAD <- function(dep, buffer_size, capad_file_path = "~/Dropbox/ECL spatial layers repository/Australian spatial layers GIS data/AUS/CAPAD_Terrestrial_land_use/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Terrestrial/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Terrestrial.shp") {

  ## first check if UTMs are present in the DF
  if(! any(grepl("X|Y|UTM", names(dep)))){

    ## If missing, we need UTM for buffers, so use WildObsR function to generate them
    dep_utm = UTM_coord_generator(dep)

    # and merge back into the original deps
    # but make sure its a safe merge!
    if(length(setdiff(dep_utm$deploymentID, dep$deploymentID)) +
       length(setdiff(dep$deploymentID, dep_utm$deploymentID)) == 0){
      dep = distinct(merge(dep, dep_utm, by = "deploymentID", all.x = TRUE))
    }else{
      stop("There was an error in generating UTM values for the coordinates provided. Please inspect your latitudes and longitudes before proceeding further, or generate X, Y, and UTMzone before using this function with the WildObsR::UTM_coord_generator() function.")
    } # end merging condition

  } # end UTM condition

  # Check for capitalization of Latitude and Longitude
  lat_col <- if ("Latitude" %in% names(dep)) "Latitude" else "latitude"
  lon_col <- if ("Longitude" %in% names(dep)) "Longitude" else "longitude"

  ## create copies of lat/longs to be used to make spatial vector
  dep$long2 = dep[, lon_col]
  dep$lat2 = dep[, lat_col]

  # Convert dep data.frame to a SpatVector using the appropriate lat/lon column names
  dep_sp =  terra::vect(dep, geom = c("long2", "lat2"), "EPSG:4326")

  # Split the dataframe based on UTM_Zone to obtain the values we need to work with
  utm_zones <- unique(dep$UTMzone)

  #Add a check incase there are not utm_zones
  if (length(utm_zones) == 0) {
    stop("No unique UTM zones found in the dataframe.")
  }

  #Split dataframe into it's UTMzone based components.
  dfs <- split(dep, dep$UTMzone)

  # Initialize an empty list to store the buffered polygons in.
  buffered_dfs <- list()

  #zone = 55
  # Iterate over UTM zones
  for (zone in utm_zones) {

    # Extract the dataframe for the current UTM zone
    current_df <- dfs[[as.character(zone)]]

    # Define the CRS we are going to use with the relavent zone
    utm_crs <- paste0("+epsg=4087 +proj=utm +zone=", zone, " +datum=WGS84 +units=m")

    # Convert the dataframe to a spatial feature with the intial lat long projection
    current_sf = terra::vect(current_df, geom = c("long2", "lat2"), "EPSG:4326")

    # Project the spatial vector dataframe to UTM so that we can use meters as our buffer
    current_sf <- terra::project(current_sf, utm_crs)

    # Buffer the points using meters
    #buffer_size = 5000
    buffered_sf = terra::buffer(current_sf, width = buffer_size)

    # Reproject buffered points back to geographic coordinate system
    buffered_sf = terra::project(buffered_sf, "EPSG:4326")

    # Assign unique IDs incorporating UTM zone information
    # This prevents collisions for surveys where polygons might straddle UTM zones on the border

    #Create prefix from the UTM Zone
    prefix <- paste0("UTM_", zone, "_Buffer_")
    #Assign to each polygon
    buffered_sf$ID <- paste0(prefix, seq_len(nrow(buffered_sf)))

    # Add the buffered dataframe to the list
    buffered_dfs[[as.character(zone)]] <- buffered_sf

  } # end per UTM zone

  ## combine buffered_dfs, but only if there are multiple zones!
  if(length(buffered_dfs) > 1){
    # Combine the buffered dataframes into one single sf object
    ## but do this manually b/c do.call isnt liking spatial vectors
    combined_buffered_vect <- buffered_dfs[[1]]  # Start with the first SpatVector
    for (i in 2:length(buffered_dfs)) {
      combined_buffered_vect <- rbind(combined_buffered_vect, buffered_dfs[[i]])
    } # end rbind loop.
  }else{
    # if ther eis only one, just take the first value
    combined_buffered_vect = buffered_dfs[[1]]
  } # end length > 1 condition


  ## Break up situations where there are polygons with matching IDs in each UTM zone

  # Dissolve overlapping polygons (this will merge into a multipolygon), where overlapping polygons are simplified into one.
  dissolved_polygons <- terra::aggregate(combined_buffered_vect)

  # Disaggregate the dissolved polygons into individual polygons to seperate them again.
  disaggregated_polygons <- terra::disagg(dissolved_polygons)

  # Assign unique IDs to each disaggregated polygon representing the spatial landscape
  disaggregated_polygons$Unique_ID <- paste0("Landscape_", 1:nrow(disaggregated_polygons))

  #
  ##
  ### Assign majority landscape ID name
  ##
  #

  #Read in the file specified in the function
  vector = terra::vect(file.path(capad_file_path))

  # Extract the CRS information for the layer using terra's crs() function
  crs_info <- terra::crs(vector)

  # Transform the polygons to match the vector
  disaggregated_polygons = terra::project(disaggregated_polygons, crs_info)

  # Initialize a vector to hold the dominant protected area for each buffer, with the same length as unique IDs
  unique_land_ids = unique(disaggregated_polygons$Unique_ID)
  dominant_park = rep(NA, length(unique_land_ids))  # Initialize as NA

  # Then for each unique landscape ID in disaggregated polygons
  for (j in seq_along(unique_land_ids)) {

    # Extract the landscape ID
    current_id = unique_land_ids[j]

    # Extract the landscape we are working on
    land = disaggregated_polygons[disaggregated_polygons$Unique_ID == current_id, ]

    ## Grab the extent of this area
    data_extent <- terra::ext(land)

    ### TESTING
    # {
    #   # Reproject the polygon to EPSG:4326
    #   polygon_latlon <- terra::project(land, "EPSG:4326")
    #   # Convert to Lat/Lon for visualization
    #   coords <- st_bbox(polygon_latlon)
    #   # Create an interactive map
    #   leaflet() %>%
    #     addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    #     addTiles() %>%
    #     addRectangles(
    #       lng1 = coords["xmin"], lat1 = coords["ymin"],
    #       lng2 = coords["xmax"], lat2 = coords["ymax"],
    #       color = "blue", fillOpacity = 0.2
    #     )
    #   ## UGH, just outside a protected area boundary...
    # }

    # Crop the shapefile to the extent of your data points to speed up the process.
    vector_clip <- terra::crop(vector, data_extent)

    # Perform intersection with the areas shapefile to extract relevant areas
    # Now check for intersection
    intersection_result <- suppressWarnings(terra::intersect(land, vector_clip)) # suppress warnings b/c some wont fit in anyway.

    # Check if the intersection result has any features
    if (nrow(intersection_result) == 0) {
      # Give us a message (maybe not tho?)
      print(paste("No intersection in CAPAD for buffered locationName:", current_id))

      # Find the nearest CAPAD polygon
      vector_centroids <- terra::centroids(vector)  # Get centroids of CAPAD polygons
      nearest <- terra::distance(land, vector_centroids)  # Calculate distances to centroids

      # Find the index of the nearest centroid
      nearest_index <- which.min(nearest)

      # Get the name of the nearest protected area
      nearest_park <- paste(vector_centroids$NAME[nearest_index], vector_centroids$TYPE_ABBR[nearest_index], sep = "_")
      nearest_park <- gsub(" ", "_", nearest_park)  # Clean the name
      nearest_park <- gsub("_-_", "_", nearest_park)

      dominant_park[j] <- nearest_park  # Assign the nearest park as the dominant park

      ## And let us know the resolution
      print(paste("We searched the nearest neighboring protected area for this locationName and determined it was:",
                  nearest_park))

      next  # Skip to the next landscape
    }

    # Re-project intersection_result back into a suitable CRS (assuming meters)
    intersection_result = terra::project(intersection_result, "EPSG:4087")

    # Change the names of intersection_result to make sense
    intersection_result$NAME = paste0(intersection_result$NAME, "_", intersection_result$TYPE_ABBR)
    # and a bit more cleaning
    intersection_result$NAME = gsub(" ", "_", intersection_result$NAME)
    intersection_result$NAME = gsub("_-_", "_", intersection_result$NAME)

    # Calculate areas for each protected area
    park_areas = terra::expanse(intersection_result)
    park_names = intersection_result$NAME

    # Convert park areas to numeric for comparison
    park_areas_numeric = as.numeric(park_areas)

    # Check if there are any parks in the intersection; if not, skip to the next landscape
    if (length(park_areas_numeric) == 0) {
      print(paste("The following landscape did not have any associated parks or reserves:", current_id))
      dominant_park[j] = NA  # No park found
      next
    }

    # Identify the index of the largest park area
    max_area_index = which.max(park_areas_numeric)

    # Assign the dominant park name to the respective landscape in dominant_park
    dominant_park[j] = park_names[max_area_index]
  } # end per buffered landscape

  # Add the dominant parks back to the disaggregated polygons
  disaggregated_polygons$dominant_park = dominant_park[match(disaggregated_polygons$Unique_ID, unique_land_ids)]

  #
  ##
  ###
  ####Extract values to points
  ###
  ##
  #

  # Ensure both layers have the same CRS
  dep_sp <- terra::project(dep_sp, crs(disaggregated_polygons))

  # Extract Unique IDs from polygons to points
  extracted_ids <- terra::extract(disaggregated_polygons, dep_sp)

  #Assign the ID to the dep frame in the column Landscape_buff
  dep$CAPADlocationNameBuffer = paste0(extracted_ids$Unique_ID,"_",extracted_ids$dominant_park)

  # Return the updated dataframe
  return(dep)
}

# ## clean up testing environment
# rm(b, buffered_dfs, buffered_sf, combined_buffered_sf, combined_buffered_vect,
#    current_df, current_sf, data_extent, dep_sp, dep_utm, dfs, dissolved_polygons,
#    disaggregated_polygons, extracted_ids, intersection_result, land, vector,
#    vector_clip, buffer_size, capad_file_path, crs_info, current_id, dominant_park,
#    i, j, lat_col, lon_col, max_area_index, park_areas, park_areas_numeric,
#    park_names, unique_land_ids, utm_crs, utm_zones, zone, prefix)


