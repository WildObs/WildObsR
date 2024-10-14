#' Calculate Nearest Neighbor Distances for Camera Trap Locations
#'
#' This function calculates the distance between each camera trap location (`locationID`) and its nearest neighbor within the dataset.
#' It uses geographic coordinates (longitude, latitude) to create a spatial data frame, computes the distance matrix between all locations, and returns the nearest neighbor and distance for each location.
#'
#' @details
#' The function works in the following steps:
#' \enumerate{
#'   \item It ensures that only distinct `locationID` values are considered by removing duplicates.
#'   \item A spatial data frame is created from the longitude and latitude coordinates of each `locationID`.
#'   \item A distance matrix is computed between all locations, and for each `locationID`, the nearest neighbor and the distance to it are identified.
#'   \item The diagonal of the distance matrix is set to infinity to avoid a location being considered its own nearest neighbor.
#' }
#'
#' @param data The deployments tabular data containing information about each camera trap location. It must include the columns `locationID`, `longitude`, and `latitude`.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{locationID}{The unique identifier for each camera trap location.}
#'   \item{nearestNeighborID}{The `locationID` of the nearest neighboring camera trap.}
#'   \item{distance}{The distance (in meters) between the camera trap and its nearest neighbor.}
#' }
#'
#' @importFrom sf st_as_sf st_distance
#' @importFrom dplyr select distinct
#'
#' @examples
#' # Example usage:
#' data <- data.frame(
#'   locationID = c("loc1", "loc2", "loc3"),
#'   longitude = c(151.2093, 153.0251, 150.644),
#'   latitude = c(-33.8688, -27.4698, -34.9285)
#' )
#'
#' # Find the nearest neighbor for each location
#' nearest_neighbors <- find_nearest_neighbor_distance(data)
#' print(nearest_neighbors)
#'
#' @author Zachary Amir & ChatGPT
#' @export
find_nearest_neighbor_distance <- function(data) {

  ## first make sure we are only working with distinct locations
  data = distinct(select(data, locationID, longitude, latitude))

  # Create a spatial data frame
  df_sf <- st_as_sf(data,
                    coords = c("longitude", "latitude"),
                    crs = "+proj=longlat")

  # Calculate the distance matrix
  dist_matrix <- st_distance(df_sf)

  # Convert the distance matrix to a data frame
  dist_df <- as.data.frame(as.matrix(dist_matrix))

  # Set the diagonal values to infinity to ensure that each locationID is compared to all others
  diag(dist_df) <- Inf

  # Find the minimum distance for each camera_id
  nearest_neighbor <- apply(dist_df, 1, which.min)

  # Extract the distance and nearest neighbor camera_id for each camera_id
  distances <- dist_df[cbind(1:nrow(dist_df), nearest_neighbor)]
  nearest_neighbor_id <- row.names(dist_df)[nearest_neighbor]

  # Create a named vector that maps the row numbers to the locationID values
  id_map <- setNames(data$locationID, row.names(dist_df))

  # Convert the nearest neighbor ids to text values
  nearest_neighbor_id <- id_map[nearest_neighbor_id]


  # Create a data frame with the camera_id of the point of origin, the camera_id of the nearest neighbor, and the distance between them
  result_df <- data.frame(locationID = data$locationID, nearestNeighborID = nearest_neighbor_id, distance = distances)

  # Return the result data frame
  return(result_df)
}
