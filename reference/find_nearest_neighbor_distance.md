# Calculate Nearest Neighbor Distances for Camera Trap Locations

This function calculates the distance between each camera trap location
(`locationID`) and its nearest neighbor within the dataset. It uses
geographic coordinates (longitude, latitude) to create a spatial data
frame, computes the distance matrix between all locations, and returns
the nearest neighbor and distance for each location.

## Usage

``` r
find_nearest_neighbor_distance(data)
```

## Arguments

- data:

  The deployments tabular data containing information about each camera
  trap location. It must include the columns `locationID`, `longitude`,
  and `latitude`.

## Value

A data frame with the following columns:

- locationID:

  The unique identifier for each camera trap location.

- nearestNeighborID:

  The `locationID` of the nearest neighboring camera trap.

- distance:

  The distance (in meters) between the camera trap and its nearest
  neighbor.

## Details

The function works in the following steps:

1.  It ensures that only distinct `locationID` values are considered by
    removing duplicates.

2.  A spatial data frame is created from the longitude and latitude
    coordinates of each `locationID`.

3.  A distance matrix is computed between all locations, and for each
    `locationID`, the nearest neighbor and the distance to it are
    identified.

4.  The diagonal of the distance matrix is set to infinity to avoid a
    location being considered its own nearest neighbor.

## Author

Zachary Amir & ChatGPT

## Examples

``` r
# Example usage:
data <- data.frame(
  locationID = c("loc1", "loc2", "loc3"),
  longitude = c(151.2093, 153.0251, 150.644),
  latitude = c(-33.8688, -27.4698, -34.9285)
)

# Find the nearest neighbor for each location
nearest_neighbors <- find_nearest_neighbor_distance(data)
print(nearest_neighbors)
#>   locationID nearestNeighborID distance
#> 1       loc1              loc3 128742.5
#> 2       loc2              loc1 732380.0
#> 3       loc3              loc1 128742.5
```
