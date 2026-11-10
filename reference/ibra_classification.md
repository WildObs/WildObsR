# Determine Interim Biogeographic Regionalisation for Australia (IBRA) bio-regions and subregions

This function identifies the IBRA bio-region and sub-region for a set of
coordinates using the IBRA7 subregions layer bundled with the package
([`ibra`](https://wildobs.github.io/WildObsR/reference/ibra.md)). It
performs a spatial join, and if any coordinates fall outside every
subregion (e.g. just offshore), the nearest matched location's IBRA
values are used instead.

## Usage

``` r
ibra_classification(data, lat_col, long_col)
```

## Arguments

- data:

  A dataframe containing at least latitude and longitude columns.

- lat_col:

  A character string specifying the column name for latitude in the
  dataframe.

- long_col:

  A character string specifying the column name for longitude in the
  dataframe.

## Value

A dataframe with the original data and additional columns:

- `IBRAsubRegionName`: Name of the IBRA sub-region.

- `IBRAsubRegioncode`: Code of the IBRA sub-region.

- `IBRAbioRegionName`: Name of the IBRA bio-region.

- `IBRAbioRegionCode`: Code of the IBRA bio-region.

## Details

The function begins by verifying the presence of the specified latitude
and longitude columns in the input dataframe. It then creates a spatial
vector from the input coordinates and clips the IBRA shapefile to the
extent of the data points for improved performance. Next, the function
performs a spatial join to associate each location with its
corresponding IBRA bio-region and sub-region.

If any locations are not assigned a bio-region (i.e., have missing
values), each takes the IBRA values of its nearest location that was
assigned one, and every input row is returned. Finally, the function
prints a summary table of the number of deployments per IBRA bio-region
and sub-region and returns an updated dataframe with additional columns
containing IBRA information.

## Author

Zachary Amir

## Examples

``` r
data <- data.frame(
  deploymentID = 1:3,
  lat = c(-15.5, -23.2, -17.1),
  lon = c(145.7, 133.5, 141.8)
)
result <- ibra_classification(data, lat_col = "lat", long_col = "lon")
#> 1 locations produced NA values for bio-region. These values will be replaced with their nearest neighbors.
#> 
#> 
#> |IBRAbioRegionName |IBRAsubRegionName     | number_of_DeploymentIDs|
#> |:-----------------|:---------------------|-----------------------:|
#> |Gulf Plains       |Mitchell-Gilbert Fans |                       2|
#> |Burt Plain        |Atartinga             |                       1|
```
