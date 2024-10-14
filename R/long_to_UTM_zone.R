#' Convert Longitude to UTM Zone
#'
#' This simple function takes a vector of longitude values and returns the corresponding UTM zone(s) for the coordinates.
#'
#' @param long A numeric vector containing longitude values.
#'
#' @return The function prints out the relevant UTM zones based off the longitudes provided.
#'
#' @author Zachary Amir & ChatGPT
#'
#' @examples
#' long = c(120, 60, 21)
#' long_to_UTM_zone(long) # output is 3 different utm zones.
#'
#' @export
long_to_UTM_zone <- function(long) {
  (floor((long + 180)/6) %% 60) + 1
}
