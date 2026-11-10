# Generate Location Buffers and Assign CAPAD Protected Area Names

This function buffers deployment locations, identifies overlapping or
nearby protected areas from the Collaborative Australian Protected Areas
Database (CAPAD) database, and assigns dominant protected area names to
each buffer. It handles deployments across multiple UTM zones and
incorporates a nearest-neighbor approach for locations outside CAPAD
boundaries.

## Usage

``` r
locationName_buffer_CAPAD(dep, buffer_size = 5000)
```

## Arguments

- dep:

  A dataframe containing deployment data, including columns for
  `latitude`, `longitude`, and `deploymentID`. It may also include
  `UTMzone`, `X`, and `Y` coordinates. If these are missing, they will
  be generated using the
  [`WildObsR::UTM_coord_generator`](https://wildobs.github.io/WildObsR/reference/UTM_coord_generator.md)
  function.

- buffer_size:

  Numeric value representing the size of the buffer (in meters) around
  each deployment location. Defaults to 5,000 m (5 km).

## Value

An updated dataframe containing the original deployment data and an
additional column, `CAPADlocationNameBuffer`, which specifies the buffer
ID and the corresponding protected area name. Stops if any
`deploymentID` appears more than once.

## Details

This function performs the following steps:

1.  Validates and generates UTM coordinates for the deployment
    locations, if necessary.

2.  Buffers deployment points within their respective UTM zones, merging
    results across zones.

3.  Dissolves overlapping buffers and assigns unique IDs to each
    disaggregated polygon.

4.  Extracts the dominant protected area name for each buffer using the
    CAPAD 2022 terrestrial layer bundled with the package
    ([`capad`](https://wildobs.github.io/WildObsR/reference/capad.md)).

5.  For buffers that do not intersect with any protected area,
    determines the nearest protected area based on centroid distance.

6.  Assigns the final protected area name and buffer ID to the original
    deployment dataframe.

## Note

The function uses the `terra` package for spatial processing.

## Author

Tom Bruce & Zachary Amir

## Examples

``` r
# two cameras of the Woomargama project shipped with WildObsR
dp <- example_datapackages("NSW_Woomargama_wombats_Linley_2021-22_WildObsID_0010")[[1]]
dep <- frictionless::read_resource(dp, "deployments")
dep <- dep[c(1, 40), c("deploymentID", "latitude", "longitude")]

# group cameras whose 1 km buffers overlap, named by the main protected area
buffered_dep <- locationName_buffer_CAPAD(dep, buffer_size = 1000)
buffered_dep[, c("deploymentID", "CAPADlocationNameBuffer")]
#>                    deploymentID   CAPADlocationNameBuffer
#> 1 10_WildObsID_0010_2021_a_Cam1 Landscape_1_Woomargama_NP
#> 2 18_WildObsID_0010_2021_a_Cam1 Landscape_2_Woomargama_NP
```
