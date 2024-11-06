#' Determine Australian State from Coordinates
#'
#' This function uses the `ozmaps` library to determine the Australian state in which each camera deployment is located, based on the average coordinates (latitude and longitude) for each deployment.
#' It assigns state codes (e.g., "NSW", "QLD") to the input data based on the geographic location of the deployments.
#'
#' @details
#' The function expects the deployment data to have columns for latitude and longitude, either named `Latitude`/`Longitude` or `latitude`/`longitude`.
#' It calculates the average coordinates for each deployment and uses the `ozmaps` package to intersect these with Australian state boundaries.
#' If any deployments cannot be matched to a state, an error will be thrown.
#'
#' @param deps A data frame containing camera deployment data. This must include columns for `deploymentID`, `Latitude` or `latitude`, and `Longitude` or `longitude`.
#'
#' @return A data frame with the original deployment data, along with an additional `state` column that contains the abbreviated Australian state or territory code (e.g., "NSW", "QLD").
#'
#' @importFrom ozmaps ozmap
#' @importFrom dplyr summarize
#' @importFrom sf st_as_sf st_crs st_intersection
#' @importFrom plyr ddply
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

  ###First make our changes to column names between OG format and camtrapDP
  #deploymentID = deployment_id
  #dataSource = source

  # Store the original column names
  original_colnames = colnames(deps)

  # Create flags to track if we need to rename columns back at the end
  changed_deployment_id = FALSE
  changed_lat = FALSE
  changed_lon = FALSE

  # Check and rename columns if necessary
  if ("deployment_id" %in% colnames(deps)) {
    colnames(deps)[colnames(deps) == "deployment_id"] = "deploymentID"
    changed_deployment_id = TRUE
  }

  if ("Latitude" %in% colnames(deps)) {
    colnames(deps)[colnames(deps) == "Latitude"] = "latitude"
    changed_lat = TRUE
  }

  if ("Longitude" %in% colnames(deps)) {
    colnames(deps)[colnames(deps) == "Longitude"] = "longitude"
    changed_lon = TRUE
  }

  ## Add a blank empty col to group evertthing togeth
  # deps$var = "blank"

  ## calculate average coordinates per landscape
  avg_land = ddply(deps, .(deploymentID), summarize,
                   avg_long = mean(longitude),
                   avg_lat = mean(latitude))
  # then import a map of the states
  aus = ozmaps::ozmap("states")
  # then make average coordinates a spatial object, matching the CRS of the states
  avg_land = st_as_sf(avg_land, coords = c("avg_long", "avg_lat"), crs = st_crs(aus))
  # then intersect avg coords and states
  states = st_intersection(avg_land, aus)

  ## clean up the states dataframe
  states$geometry = NULL
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

  if (changed_lat) {
    colnames(deps)[colnames(deps) == "latitude"] <- "Latitude"
  }

  if (changed_lon) {
    colnames(deps)[colnames(deps) == "longitude"] <- "Longitude"
  }

  # return(unique(states$state))
  return(deps)

}

# for testing
#rm(states, aus, avg_land)
