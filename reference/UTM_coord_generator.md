# Generate UTMs when latitude,longitude,and UTM_Zone have been provided in a dataframe

This function works out the X and Y coordinates for datasets in the
approiate UTM zones. It takes the geopgraphic coordinates (latitude,
longitude), and the UTM Zone to reproject points into a standardised
format. By default it uses the

## Usage

``` r
UTM_coord_generator(data)
```

## Arguments

- data:

  The deployments tabular data containing information about each camera
  trap location. It must include the columns `locationID`,
  `longitude`,`latitude`, and `UTM_zone`

## Value

A data frame with the following columns:

- deploymentID:

  The unique identifier for each camera trap deployment

- X:

  The `X` coordinate for the defined UTM zone.

- Y:

  The `Y` coordinate for the defined UTM zone.

## Details

The function works in the following steps:

1.  Ensure that UTM Zone is present from using the long_to_UTMzone
    function.

2.  A spatial data frame is created from the longitude and latitude
    coordinates of each `locationID`.

3.  The dataframe is then split between UTM zones to relaibily assign
    the X and Y coordiantes

4.  The datafarmes are then remerged together with their new geographic
    information, and if required columns renamed

## Author

Tom Bruce & Zachary Amir

## Examples

``` r
# Example usage:
dep <- data.frame(
  deploymentID = c("location1", "location2"),
  Latitude = c(-33.8688, -27.4698),
  Longitude = c(151.2093, 153.0251)
)
utm_dep <- UTM_coord_generator(data = dep)
head(utm_dep)
#>   deploymentID UTMzone        X        Y
#> 1    location1      56 334368.6 -3749052
#> 2    location2      56 502479.9 -3038472
```
