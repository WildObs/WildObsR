# Extract Metadata Elements from Data Packages

Extracts specified metadata elements from one or more Camera Trap Data
Packages (camtrap DP) and returns them as tidy data frames. Handles
flexible list structures and automatically replaces NULL values with NA
to ensure consistent data types.

## Usage

``` r
extract_metadata(
  dp_list,
  elements = c("contributors", "sources", "licenses", "relatedIdentifiers", "references",
    "project", "WildObsMetadata", "spatial", "temporal", "taxonomic")
)
```

## Arguments

- dp_list:

  A single data package (list) or a list of data packages to extract
  metadata from. Each data package should be a named list with an `id`
  field.

- elements:

  A character vector specifying which metadata elements to extract.
  Supported elements are: `"contributors"`, `"sources"`, `"licenses"`,
  `"relatedIdentifiers"`, `"references"`, `"project"`,
  `"WildObsMetadata"`, `"spatial"`, `"temporal"`, `"taxonomic"`.

## Value

If a single element is requested: A data frame with one row per record
(or one row for flat objects) and columns for each field in that
element. Includes a `DPID` column with the source data package ID.

If multiple elements are requested: A named list of data frames, one for
each requested element. Each data frame includes a `DPID` column to
distinguish different data packages

When multiple data packages are provided, results are accumulated across
all DPs using
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html),
so each data frame in the result contains records from all input data
packages.

## Details

This function is designed for extracting and standardizing metadata from
camtrap DP packages. It handles:

- Single or multiple data packages

- Flexible list structures (lists of objects vs. flat objects)

- NULL values (replaced with NA for consistency)

- Multiple data packages (rows combined via
  [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html))

## Spatial data

The `"spatial"` element returns a dataframe of bounding box coordinates
(locationName, xmin, ymin, xmax, ymax) for convenient inspection. Users
who need the full GeoJSON geometry for spatial analysis or mapping can
access it directly via `dp$spatial` and convert it to an sf object with
`geojsonsf::geojson_sf(jsonlite::toJSON(dp$spatial, auto_unbox = TRUE))`.

## Temporal data

The `"temporal"` element returns one row per deploymentGroup, with
columns `deploymentGroup`, `start`, `end`, `timeZone`, `packageStart`,
`packageEnd` and `DPID`. `packageStart` and `packageEnd` hold the
package-level temporal extent that Camtrap DP requires, repeated on
every row, and are `NA` for packages that do not carry it. A package
with no deploymentGroup blocks returns a single row with
`deploymentGroup = NA`. Packages saved by earlier WildObsR versions,
which stored `timeZone` inside each deploymentGroup, are still read
correctly.

## Author

Zachary Amir

## Examples

``` r
# the two Wet Tropics projects shipped with WildObsR
dp_list <- example_datapackages()

# one element, as a table with a row per contributor per project
contributors <- extract_metadata(dp_list, "contributors")
contributors[, c("title", "role", "DPID")]
#>            title                  role
#> 1   Zachary Amir principalInvestigator
#> 2      Tom Bruce               contact
#> 3 Matthew Luskin          rightsHolder
#> 4   Grant Linley principalInvestigator
#> 5    Chris Jolly           contributor
#> 6 Emma E Spencer           contributor
#>                                                   DPID
#> 1            ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001
#> 2            ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001
#> 3            ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001
#> 4 NSW_Woomargama_wombats_Linley_2021-22_WildObsID_0010
#> 5 NSW_Woomargama_wombats_Linley_2021-22_WildObsID_0010
#> 6 NSW_Woomargama_wombats_Linley_2021-22_WildObsID_0010

# several elements at once, as a named list of tables
meta <- extract_metadata(dp_list, c("licenses", "project"))
meta$licenses
#>        name                                         path scope
#> 1 CC-BY-4.0 https://creativecommons.org/licenses/by/4.0/  data
#> 2 CC-BY-4.0 https://creativecommons.org/licenses/by/4.0/ media
#> 3 CC-BY-4.0 https://creativecommons.org/licenses/by/4.0/  data
#> 4 CC-BY-4.0 https://creativecommons.org/licenses/by/4.0/ media
#>                                                   DPID
#> 1            ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001
#> 2            ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001
#> 3 NSW_Woomargama_wombats_Linley_2021-22_WildObsID_0010
#> 4 NSW_Woomargama_wombats_Linley_2021-22_WildObsID_0010

# a single package works too
extract_metadata(dp_list[[1]], "WildObsMetadata")
#>   thinnedMedia deploymentClusters               fundingAgency
#> 1        false               true federalGovernment; ngoGrant
#>   tabularSharingPreference desiredOutputs groupSizes
#> 1                     open     everything       true
#>                                        DPID embargoPeriodMonths
#> 1 ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001                   0
#>     WildObsContribution          deploymentTags
#> 1 dingo-cat-fox project bait;predatorManagement
```
