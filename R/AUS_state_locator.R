#' Determine Australian State from Coordinates
#'
#' This function uses the `ozmaps` library to determine the Australian state in
#' which each camera deployment is located, based on the average coordinates
#' (latitude and longitude) for each deployment.
#' It assigns state codes (e.g., "NSW", "QLD") to the input data based on the
#' geographic location of the deployments.
#'
#' @details
#' The function expects the deployment data to have columns for latitude and
#' longitude, either named `Latitude`/`Longitude` or `latitude`/`longitude`.
#' It calculates the average coordinates for each deployment and uses the `ozmaps`
#' package to intersect these with Australian state boundaries.
#' If any deployments cannot be matched to a state, the function will search for
#' the nearest state and assign it that value (e.g., coordinates near coast)
#'
#' @param deps A data frame containing camera deployment data. This must include columns for `deploymentID`, `Latitude` or `latitude`, and `Longitude` or `longitude`.
#'
#' @return A data frame with the original deployment data, along with an additional `state` column that contains the abbreviated Australian state or territory code (e.g., "NSW", "QLD").
#'
#' @importFrom ozmaps ozmap
#' @importFrom plyr summarize ddply
#' @importFrom sf st_as_sf st_crs st_intersection
#' @examples
#' # Example data
#' deps <- data.frame(
#'   deploymentID = c("dep1", "dep2"),
#'   Latitude = c(-33.8688, -27.4698),
#'   Longitude = c(151.2093, 153.0251)
#' )
#'
#' # Run the function to find states
#' deps_with_states <- AUS_state_locator(deps)
#' print(deps_with_states)
#'
#' @author Zachary Amir & Tom Bruce
#' @export
AUS_state_locator = function (deps){

  # Store the original column names
  original_colnames = colnames(deps)

  # Create flags to track if we need to rename columns back at the end
  changed_deployment_id = FALSE
  changed_lat1 = FALSE
  changed_lat2 = FALSE
  changed_lat3 = FALSE
  changed_lon1 = FALSE
  changed_lon2 = FALSE
  changed_lon3 = FALSE


  # Check and rename columns if necessary
  if ("deployment_id" %in% colnames(deps)) {
    colnames(deps)[colnames(deps) == "deployment_id"] = "deploymentID"
    changed_deployment_id = TRUE
  }

  ## Add code-checks for lat/long col names
  # TBH, might be easier in future iterations to specify lat/long in function
  # or tolower() all colnames and search for the lat/long cols

  ## Lats
  if ("Latitude" %in% colnames(deps)) {
    colnames(deps)[colnames(deps) == "Latitude"] = "latitude"
    changed_lat1 = TRUE
  }
  if ("Lat" %in% colnames(deps)) {
    colnames(deps)[colnames(deps) == "Lat"] = "latitude"
    changed_lat2 = TRUE
  }
  if ("lat" %in% colnames(deps)) {
    colnames(deps)[colnames(deps) == "lat"] = "latitude"
    changed_lat3 = TRUE
  }

  ## Longs
  if ("Longitude" %in% colnames(deps)) {
    colnames(deps)[colnames(deps) == "Longitude"] = "longitude"
    changed_lon1 = TRUE
  }
  if ("Long" %in% colnames(deps)) {
    colnames(deps)[colnames(deps) == "Long"] = "longitude"
    changed_lon2 = TRUE
  }
  if ("long" %in% colnames(deps)) {
    colnames(deps)[colnames(deps) == "long"] = "longitude"
    changed_lon3 = TRUE
  }

  ## print a warning if we cant get lat/longs
  if(!any(grepl("longitude|latitude", colnames(deps)))){
    stop("Latitude and/or longitude could not be found in your dataframe. Please make sure to provide latitdue as 'lat', 'Lat', 'Latitude', or 'latitdue', and please make sure to provide longitude as 'long', 'Long', 'Longitude', or 'longitude'.")
  }

  ## calculate average coordinates per landscape
  avg_land = plyr::ddply(deps, "deploymentID", plyr::summarize,
                          avg_long = mean(longitude),
                          avg_lat = mean(latitude))
  # then import a map of the states
  aus = ozmaps::ozmap_states
  # then make average coordinates a spatial object, matching the CRS of the states
  avg_land = sf::st_as_sf(avg_land, coords = c("avg_long", "avg_lat"), crs = sf::st_crs(aus))
  # then intersect avg coords and states
  states = sf::st_intersection(avg_land, aus)

  # Find missing deployments
  missing_deps <- setdiff(avg_land$deploymentID, states$deploymentID)

  ## if there are missing deployments
  if(length(missing_deps) > 0) {
    # Get the missing points
    missing_points <- avg_land[avg_land$deploymentID %in% missing_deps, ]

    # Find nearest state for each missing point
    nearest_states <- sf::st_nearest_feature(missing_points, aus)

    # Create dataframe for missing points
    missing_states <- data.frame(
      deploymentID = missing_points$deploymentID,
      NAME = aus$NAME[nearest_states]
    )

    # Combine with successful intersections
    states <- rbind(
      sf::st_drop_geometry(states[, c("deploymentID", "NAME")]),
      missing_states
    )
  } # end missing deps condition

  ## clean up the states dataframe
  states$geometry = NULL # ensure no geom is left!
  states$state[states$NAME == "New South Wales"] = "NSW"
  states$state[states$NAME == "Queensland"] = "QLD"
  states$state[states$NAME == "South Australia"] = "SA"
  states$state[states$NAME == "Victoria"] = "VIC"
  states$state[states$NAME == "Western Australia"] = "WA"
  states$state[states$NAME == "Northern Territory"] = "NT"
  states$state[states$NAME == "Australian Capital Territory"] = "ACT"
  states$state[states$NAME == "Tasmania"] = "TAS"
  states$state[states$NAME == "Other Territories"] = "OTHER"
  ## remove long name
  states$NAME = NULL

  # verify all landscapes are present
  if(length(setdiff(states$deploymentID, deps$deploymentID)) +
     length(setdiff(deps$deploymentID, states$deploymentID)) != 0){
    stop("Not all landscapes could be found in OzMaps database, inspect manually")
  }

  # # now safley merge
  deps = merge(deps, states, by = "deploymentID")

  #Rename our columns if needed
  # Change the column names back to original if they were changed
  if (changed_deployment_id) {
    colnames(deps)[colnames(deps) == "deploymentID"] <- "deployment_id"
  }

  if (changed_lat1) {
    colnames(deps)[colnames(deps) == "latitude"] <- "Latitude"
  }
  if (changed_lat2) {
    colnames(deps)[colnames(deps) == "latitude"] <- "Lat"
  }
  if (changed_lat3) {
    colnames(deps)[colnames(deps) == "latitude"] <- "lat"
  }

  if (changed_lon1) {
    colnames(deps)[colnames(deps) == "longitude"] <- "Longitude"
  }
  if (changed_lon2) {
    colnames(deps)[colnames(deps) == "longitude"] <- "Long"
  }
  if (changed_lon3) {
    colnames(deps)[colnames(deps) == "longitude"] <- "long"
  }

  # return(unique(states$state))
  return(deps)

}

# for testing
# rm(states, aus, avg_land, changed_deployment_id, changed_lat, changed_lon, original_colnames)
