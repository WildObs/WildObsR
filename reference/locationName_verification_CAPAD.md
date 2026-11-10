# Verify Location Names Using the Collaborative Australian Protected Areas Database

This function verifies whether a set of coordinates intersects with the
Collaborative Australian Protected Areas Database (CAPAD) 2022
terrestrial layer bundled with the package
([`capad`](https://wildobs.github.io/WildObsR/reference/capad.md)) and
assigns the corresponding protected area's name. For coordinates that do
not match any protected area, the nearest protected area is used
instead.

## Usage

``` r
locationName_verification_CAPAD(dep)
```

## Arguments

- dep:

  A dataframe containing coordinates (in latitude and longitude), and a
  unique identifier for each sampling location.

## Value

A dataframe with the original columns and two additional columns:

- `CAPADlocationName`: The name of the protected area assigned to each
  coordinate. Names are formatted to replace spaces with underscores.

- `CAPADminDistance`: The distance (in meters) to the nearest protected
  area, for coordinates outside every protected area; zero for
  coordinates inside one. Only added when at least one coordinate falls
  outside every protected area.

## Details

The function performs the following steps:

1.  Loads the bundled CAPAD layer and standardizes its column names.

2.  Converts the input dataframe `dep` into a spatial object using
    latitude and longitude.

3.  Determines the intersection of the coordinates with the CAPAD layer.

4.  For coordinates with no match, finds the nearest protected area
    (distance in metres) and prints out warning messages if the updated
    location name is far from the original coordinate (1-5 km, 5-10 km,
    \>10 km).

5.  Returns the modified dataframe with a new column `CAPADlocationName`
    containing the matched or nearest protected area's name.

## Author

Tom Bruce & Zachary Amir

## Examples

``` r
# two cameras of the Woomargama project shipped with WildObsR
dp <- example_datapackages("NSW_Woomargama_wombats_Linley_2021-22_WildObsID_0010")[[1]]
dep <- frictionless::read_resource(dp, "deployments")
dep <- dep[c(1, 40), c("deploymentID", "latitude", "longitude", "locationName")]

# the protected area each camera is in, beside the recorded locationName; both are
# inside Woomargama National Park, so no CAPADminDistance column is added
updated_dep <- locationName_verification_CAPAD(dep)
updated_dep[, c("deploymentID", "locationName", "CAPADlocationName")]
#>                    deploymentID  locationName CAPADlocationName
#> 1 10_WildObsID_0010_2021_a_Cam1 Woomargama_NP     Woomargama_NP
#> 2 18_WildObsID_0010_2021_a_Cam1 Woomargama_NP     Woomargama_NP
```
