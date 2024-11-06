#' Group Deployments by UTM Zones and Buffers
#'
#' This function buffers camera trap deployment points, accounting for multiple UTM zones. It splits the data by UTM zone, applies a buffer in meters, and re-merges the buffered polygons across zones.
#' The function also handles overlapping polygons, ensuring unique locationName IDs are assigned to each buffered polygon.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Splits the deployment data by UTM zone to handle buffering separately for each zone.
#'   \item Buffers points within each UTM zone using the specified buffer size (in meters).
#'   \item Merges buffered polygons from different UTM zones and assigns unique IDs to each polygon.
#'   \item Dissolves and disaggregates overlapping polygons, ensuring that each landscape is represented by a unique buffer.
#'   \item Extracts the unique landscape buffer ID for each deployment point.
#' }
#'
#' @param dep The deployments tabular data describing each camera deployment, including latitude and longitude columns (either `Latitude`/`Longitude` or `latitude`/`longitude`) and a `UTM_zone` column.
#' @param buffer_size A numeric value representing the size of the buffer around each point in meters.
#'
#' @return An updated deployments data frame with an additional `Landscape_buff` column that contains unique IDs for each buffered landscape around the camera deployments.
#'
#' @importFrom terra vect project aggregate disagg extract
#' @importFrom sf st_as_sf st_transform st_buffer
#' @importFrom stringr str_to_title
#'
#' @examples
#' # Example usage:
#' dep <- data.frame(
#'   deploymentID = c("dep1", "dep2"),
#'   Latitude = c(-33.8688, -27.4698),
#'   Longitude = c(151.2093, 153.0251),
#'   UTM_zone = c(56, 55)
#' )
#'
#' # Apply buffer of 5000 meters
#' buffered_dep <- landscape_buffer(dep, buffer_size = 5000)
#' table(buffered_dep$Landscape_buff)
#'
#' @author Tom Bruce
#'
#' @export
landscape_buffer_namer <- function(dep, buffer_size) {

  ## require specific libraries
  # require(terra)
  # require(sf)

  # Check if UTM_zone column exists and has valid values
  if (!("UTM_zone" %in% colnames(dep))) {
    stop("UTM_zone column not found in the dataframe.")
  }

  if (any(is.na(dep$UTM_zone))) {
    stop("Some values in UTM_zone column are NA.")
  }


  # Check for correct capitalization of Latitude and Longitude
  lat_col <- if ("Latitude" %in% names(dep)) "Latitude" else "latitude"
  lon_col <- if ("Longitude" %in% names(dep)) "Longitude" else "longitude"

  # Convert the dep data.frame to a SpatialPointsDataFrame we need this to buffer our points
  dep_sp <- st_as_sf(dep, coords = c(lon_col, lat_col), crs = 4326)  # This uses the standard WGS84 (EPSG:4326) LatLong projection


  # Split the dataframe based on UTM_Zone to obtain the values we need to work with
  utm_zones <- unique(dep$UTM_zone)

  #Add a check incase there are not utm_zones
  if (length(utm_zones) == 0) {
    stop("No unique UTM zones found in the dataframe.")
  }

  #Split dataframe into it's UTM_zone based components.
  dfs <- split(dep, dep$UTM_zone)

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
    current_sf <- st_as_sf(current_df, coords = c(lon_col, lat_col), crs = 4326)  # EPSG:4326 is WGS84


    # Project the spatialpoints dataframe to UTM so that we can use meters as our buffer
    current_sf <- st_transform(current_sf, crs = utm_crs)


    # Buffer the points using meters
    #buffer_size = 5000
    buffered_sf = st_buffer(current_sf, dist = buffer_size)

    # Reproject buffered points back to geographic coordinate system
    buffered_sf <- st_transform(buffered_sf, crs = 4326)

    # Assign unique IDs incorporating UTM zone information this prevents collisions for surveys where polygons might straddle UTM zones on the border
    #Create prefix from the UTM Zone
    prefix <- paste0("UTM_", zone, "_Buffer_")
    #Assign to each polygon
    buffered_sf$ID <- paste0(prefix, seq_len(nrow(buffered_sf)))

    # Add the buffered dataframe to the list
    buffered_dfs[[as.character(zone)]] <- buffered_sf
  }

  #We need to add code to break up situations where there are polygons with matching IDs in each UTM zone

  # Combine the buffered dataframes into one single sf object
  combined_buffered_sf <- do.call(rbind, buffered_dfs)

  #Assign unique IDs across each buffer
  combined_buffered_sf$Unique_ID <- paste0("Buffer_", seq_len(nrow(combined_buffered_sf)))

  # Convert combined buffer sf object to terra vector
  combined_buffered_vect <- vect(combined_buffered_sf)
  dep_sp_vect <- vect(dep_sp) #We also need the dep_sp to be a terra object to use it.

  # Dissolve overlapping polygons (this will merge into a multipolygon), where overlapping polygons are simplified into one.
  dissolved_polygons <- aggregate(combined_buffered_vect)

  # Disaggregate the dissolved polygons into individual polygons this now seperates them again.
  disaggregated_polygons <- disagg(dissolved_polygons)

  # Assign unique IDs to each disaggregated polygon representing the spatial landscape
  disaggregated_polygons$Unique_ID <- paste0("Landscape_", 1:nrow(disaggregated_polygons))

  ###Assign majority landscape ID name####

  #User must define the path to the CAPAD file.
  path = "~/Dropbox/ECL spatial layers repository/Australian spatial layers GIS data/AUS/CAPAD_Terrestrial_land_use/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Terrestrial/"

  #Read in the file
  vector =  terra::vect(file.path(path, paste0("Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Terrestrial.shp")))

  # Check the CRS of the vector
  print(crs(vector, describe=TRUE, proj=TRUE)) # CRS is EPSG 3857
  print(crs(disaggregated_polygons, describe=TRUE, proj=TRUE)) # CRS is EPSG 4326 - and the polygons

  # Ensure the vector is converted to sf if needed
  vector_sf = st_as_sf(vector)

  # Transform the polygons to match the vector
  disaggregated_polygons = project(disaggregated_polygons, "EPSG:3857")

  # Initialize a vector to hold the dominant biome for each buffer, with the same length as unique IDs
  unique_land_ids = unique(disaggregated_polygons$Unique_ID)
  dominant_park = rep(NA, length(unique_land_ids))  # Initialize as NA

  # Then for each unique landscape ID in disaggregated polygons
  for (j in seq_along(unique_land_ids)) {

    # Extract the landscape ID
    current_id = unique_land_ids[j]

    # Extract the landscape we are working on
    land = disaggregated_polygons[disaggregated_polygons$Unique_ID == current_id, ]
    # Make sure it's the right kind of object
    land = st_as_sf(land)

    # Perform intersection with the areas shapefile to extract relevant areas
    intersection = st_intersection(land, vector_sf)

    # Debugging: Check if intersection is empty
    if (nrow(intersection) == 0) {
      print(paste("No intersection for landscape", current_id))
      dominant_park[j] = NA  # No dominant park found for this ID
      next  # Skip to the next landscape
    }

    # Reproject intersection back into a suitable CRS (assuming meters)
    intersection = st_transform(intersection, crs = "EPSG:4087")

    # Change the names of intersection to make sense
    intersection$NAME = paste0(intersection$NAME, "_", intersection$TYPE_ABBR)

    # Calculate areas for each protected area
    park_areas = st_area(intersection)
    park_names = intersection$NAME

    # Convert park areas to numeric for comparison
    park_areas_numeric = as.numeric(park_areas)

    # Check if there are any parks in the intersection; if not, skip to the next landscape
    if (length(park_areas_numeric) == 0) {
      print(paste("The following landscape did not have any associated parks or reserves:", current_id))
      dominant_park[j] = NA  # No biome found
      next
    }

    # Identify the index of the largest park area
    max_area_index = which.max(park_areas_numeric)

    # Assign the dominant park name to the respective landscape in dominant_park
    dominant_park[j] = park_names[max_area_index]
  }

  # Add the dominant parks back to the disaggregated polygons
  disaggregated_polygons$dominant_park = dominant_park[match(disaggregated_polygons$Unique_ID, unique_land_ids)]
  ###END majority Landscape ID####

  ####Then for NA values try and locate a rural property equivalent?######

  ###THIS IS GOING TO TAKE TIME -- I think we should read in rural property shapefiles



  #####End rural property search#####

  #####Extract values to points####
  # Ensure both layers have the same CRS
  dep_sp_vect <- project(dep_sp_vect, crs(disaggregated_polygons))

  # Extract Unique IDs from polygons to points
  extracted_ids <- terra::extract(disaggregated_polygons, dep_sp_vect)

  #Assign the ID to the dep frame in the column Landscape_buff
  dep$Landscape_buff = paste0(extracted_ids$Unique_ID,"_",extracted_ids$dominant_park)

  # Clean dep
  dep$circle_color <- NULL

  # Return the updated dataframe
  return(dep)
}



