#' Generate UTMs when latitude,longitude,and UTM_Zone have been provided in a dataframe
#'
#' This function works out the X and Y coordinates for datasets in the approiate UTM zones. It takes the geopgraphic coordinates (latitude, longitude), and the UTM Zone to reproject points into a standardised format. By default it uses the
#'
#' @details
#' The function works in the following steps:
#' \enumerate{
#'   \item Ensure that UTM Zone is present from using the long_to_UTMzone function.
#'   \item A spatial data frame is created from the longitude and latitude coordinates of each `locationID`.
#'   \item The dataframe is then split between UTM zones to relaibily assign the X and Y coordiantes
#'   \item The datafarmes are then remerged together with their new geographic information, and if required columns renamed
#' }
#'
#' @param data The deployments tabular data containing information about each camera trap location. It must include the columns `locationID`, `longitude`,`latitude`, and `UTM_zone`
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{locationID}{The unique identifier for each camera trap location.}
#'   \item{X}{The `X` coordinate for the defined UTM zone.}
#'   \item{Y}{The `Y` coordinate for the defined UTM zone.}
#' }
#'
#' @importFrom sf st_crs st_as_sf st_transform
#' @importFrom dplyr filter bind_rows
#'
#' @examples
#' # Example usage:
#' dep <- data.frame(
#'   locationID = c("location1", "location2"),
#'   Latitude = c(-33.8688, -27.4698),
#'   Longitude = c(151.2093, 153.0251)
#' )
#' utm_dep <- UTM_coord_generator(data = dep)
#' head(utm_dep)
#'
#' @author Tom Bruce
#'
#' @export
UTM_coord_generator <- function(data) {

# Create flags to track if we need to rename columns back at the end
changed_placename_data = FALSE

# Check and rename columns if necessary
  if ("placename" %in% colnames(data)) {
    colnames(data)[colnames(data) == "placename"] = "locationID"
    changed_placename_data = TRUE
  }

#Check for latitude and longitude columns which could be captials or not
lat_col <- if ("Latitude" %in% names(data)) "Latitude" else "latitude"
lon_col <- if ("Longitude" %in% names(data)) "Longitude" else "longitude"


if (!("UTM_zone" %in% colnames(data))){ #Check dep has the UTM zones defines

print("Provided dataframe does not have UTM zone which this functions required, generating it now.")

### Generate UTM from coordinates
data$UTM_zone = (floor((data[,lon_col] + 180)/6) %% 60) + 1 # cant have circular dependency! Cant call in wildobsr function here.

}#close check for utm_zone in data

#print a message to show the UTM zone
cat("The cameras come from the following UTM zones\n")
print(unique(data$UTM_zone)) #


#ECL_latlong needs to be defined to be used.
ECL_latlong = st_crs("+epsg=4087 +proj=longlat +datum=WGS84")

## Convert lat/long into sf object
data_sf = st_as_sf(data, coords = c(lon_col, lat_col),
                  crs = ECL_latlong)
#head(data_sf)

# Split the dataframe based on UTM_Zone to obtain the values we need to work with
utm_zones <- unique(data$UTM_zone)

# Initialize an empty list to store the buffered polygons in.
utm_dfs <- list()

# Iterate over each UTM zone
for (zone in utm_zones) {
  # Select points in the current UTM zone
  points_zone <- data_sf %>% dplyr::filter(UTM_zone == zone)

  # Define the UTM CRS for the current zone using EPSG:4087 zone in here allows this to be unique per zone
  utm_crs <- paste0("+epsg=4087 +proj=utm +zone=", zone, " +datum=WGS84 +units=m")

  ## transform sf lat/long into UTM, specify the zone!!
  data_utm = st_transform(points_zone,
                          crs = utm_crs)

  # Convert to regular numbers
  data_utm = data.frame("locationID" = data_utm$locationID,
                        "UTMzone" = zone,
                        "X" = st_coordinates(data_utm)[,1],
                        "Y" = st_coordinates(data_utm)[,2])

  # Add the buffers to the list
  utm_dfs[[as.character(zone)]] <- data_utm
}

data_utm = bind_rows(utm_dfs)

#Rename our columns if needed
if (changed_placename_data) {
  colnames(data_utm)[colnames(data_utm) == "locationID"] <- "placename"
}

return(data_utm)

} # Closing brace for the overall function
