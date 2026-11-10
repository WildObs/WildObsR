# Example WildObs Data Packages

Reads the two WildObs projects shipped with the package, so examples,
tests and tutorials run offline on real data. They come back in the same
shape as
[`wildobs_dp_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
returns: a named list of Camtrap DP data packages, each with its tables
in `resources` and date-times in the project's local time zone. Read the
tables with
[`frictionless::read_resource()`](https://docs.ropensci.org/frictionless/reference/read_resource.html).
A copy also sits in `dp$data`, because the `camtrapdp` class expects
one, but other tools do not always keep it up to date.

The two projects have different designs, regions and seasons:

- `ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001`:

  Systematic, unbaited cameras on roads, trails and off-trail across six
  national parks in the Queensland Wet Tropics, 2022-23: 275 deployments
  at 145 stations, most with two cameras.

- `NSW_Woomargama_wombats_Linley_2021-22_WildObsID_0010`:

  Unbaited cameras at wombat burrows and at nearby controls in
  Woomargama National Park, New South Wales, over four surveys in
  2021-22 that form one multi-season group: 219 deployments at 28
  stations, in the time zone Australia/Sydney.

Both are open and licensed CC-BY-4.0; cite them with
[`cite_datapackages()`](https://wildobs.github.io/WildObsR/reference/cite_datapackages.md).
They are copies of the public data at the time the package was built
(see `versionControlWildObs`), with three differences from a live
download: only animal observations are kept, there is no media table,
and the email addresses of contributors and sources are left out.
Threatened-species records stay obscured, exactly as the public API
serves them. The copies are rebuilt by `dev/make_example_data.R` in the
package source.

## Usage

``` r
example_datapackages(
  project_ids = c("ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001",
    "NSW_Woomargama_wombats_Linley_2021-22_WildObsID_0010")
)
```

## Arguments

- project_ids:

  Which projects to read. Defaults to both. Use
  `example_datapackages(project_ids = NULL)` to list the available IDs
  without reading anything.

## Value

A named list of data packages of class `camtrapdp`, one per project,
with `deployments`, `observations` and `covariates` tables. With
`project_ids = NULL`, the character vector of available project IDs.
Stops if an ID is not one of them.

## See also

[`wildobs_dp_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
for the full, current data from the WildObs database.

## Author

Zachary Amir

## Examples

``` r
# the projects shipped with WildObsR
example_datapackages(project_ids = NULL)
#> [1] "NSW_Woomargama_wombats_Linley_2021-22_WildObsID_0010"
#> [2] "ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001"           

# read both, exactly as wildobs_dp_download() would return them
dp_list <- example_datapackages()
names(dp_list)
#> [1] "ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001"           
#> [2] "NSW_Woomargama_wombats_Linley_2021-22_WildObsID_0010"
deps <- frictionless::read_resource(dp_list[[1]], "deployments")
head(deps[, c("deploymentID", "deploymentStart", "baitUse")])
#> # A tibble: 6 × 3
#>   deploymentID                      deploymentStart     baitUse
#>   <chr>                             <dttm>              <lgl>  
#> 1 CFRAG_01_WildObsID_0001_2022_Cam1 2022-12-15 08:52:45 FALSE  
#> 2 CFRAG_01_WildObsID_0001_2022_Cam2 2022-12-15 08:19:27 FALSE  
#> 3 CFRAG_02_WildObsID_0001_2022_Cam1 2022-12-15 09:58:36 FALSE  
#> 4 CFRAG_02_WildObsID_0001_2022_Cam2 2022-12-15 09:40:56 FALSE  
#> 5 CFRAG_03_WildObsID_0001_2022_Cam1 2022-12-15 11:00:58 FALSE  
#> 6 CFRAG_03_WildObsID_0001_2022_Cam2 2022-12-15 10:36:47 FALSE  

# and cite them
cite_datapackages(dp_list)
#>                                                                                                                                                                                                                                                                                                                                                                                       ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001 
#> "Zachary Amir, Tom Bruce & Matthew Luskin (2022). WildObs Camera Trap Data Package: Zachary Amir's camera trap surveys from 6 national parks in QLD's Wet Tropics rainforests from late 2022 to early 2023 targeting feral and native vertebrates [Camera Trap Data Package]. WildObs National Wildlife Camera Database. https://wildobs.org.au. RAiD: https://raid.org/10.83062/cb5829ab. WildObs data package version 1.1.1." 
#>                                                                                                                                                                                                                                                                                                                                                                            NSW_Woomargama_wombats_Linley_2021-22_WildObsID_0010 
#>                                                                        "Grant Linley, Chris Jolly & Emma E Spencer (2021). WildObs Camera Trap Data Package: Wombat burrows are hotspots for small vertebrates in a landscape subject to gigafire [Camera Trap Data Package]. WildObs National Wildlife Camera Database. https://wildobs.org.au. RAiD: https://raid.org/10.83062/9eec588a. WildObs data package version 1.0.3." 
```
