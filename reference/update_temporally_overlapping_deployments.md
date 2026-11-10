# Group Temporally Overlapping Deployment IDs

This function checks for and consolidates temporally overlapping
deployment IDs within the same location. It is designed to group
deployments that are active at overlapping or nearly adjacent times,
ensuring that each deploymentID represents a distinct time period. This
function is recommended for use in step 1 to validate deploymentID
assignments and in step 3 to verify temporal and spatial distinctions
between deploymentIDs.

## Usage

``` r
update_temporally_overlapping_deployments(deps, obs, media)
```

## Arguments

- deps:

  A data frame of camera deployments, containing columns `deploymentID`,
  `locationID`, `deploymentStart`, `deploymentEnd`, and
  `deploymentGroups`. The `deploymentStart` and `deploymentEnd` columns
  should contain date-time information.

- obs:

  A data frame of observations with `deploymentID` and `locationID`
  columns that match with the `deps` data frame.

- media:

  A data frame of media records with `deploymentID` column matching the
  `deps` data frame.

## Value

A list with three updated data frames:

- deployments:

  The updated `deps` data frame with consolidated deploymentIDs for
  temporally overlapping records.

- observations:

  The updated `obs` data frame with deploymentID changes to match those
  in `deps`.

- media:

  The updated `media` data frame with deploymentID changes to match
  those in `deps`.

## Details

This function assumes that the provided `locationID` values are already
spatially distinct. Deployments at the same location with overlapping or
closely spaced start and end times are grouped together under a new
deploymentID, ensuring temporal and spatial distinctness. After updating
deploymentIDs in `deps`, the function also updates related deploymentIDs
in `obs` and `media` to maintain consistency across data files. The
function includes checks to ensure matching deploymentIDs across the
provided data frames before and after processing, with detailed warnings
if mismatches occur.

## Author

Zachary Amir

## Examples

``` r
# Minimal toy example to demonstrate how overlapping deployments are merged

# Create a deployments data frame with overlapping start and end dates
deps <- data.frame(
  deploymentID = c("Cam01_A", "Cam01_B"),
  locationID = c("Loc1", "Loc1"),
  deploymentStart = as.POSIXct(c("2020-01-01", "2020-01-05")),
  deploymentEnd   = as.POSIXct(c("2020-01-10", "2020-01-15")),
  deploymentGroups = NA_character_
)

# Observations linked to those deployments
obs <- data.frame(
  observationID = 1:4,
  deploymentID = rep(c("Cam01_A", "Cam01_B"), each = 2),
  locationID = "Loc1"
)

# Media linked to the same deployments
media <- data.frame(
  mediaID = paste0("m", 1:4),
  deploymentID = rep(c("Cam01_A", "Cam01_B"), each = 2)
)

# Run the function — overlapping deployments will be consolidated
results <- update_temporally_overlapping_deployments(
  deps = deps,
  obs = obs,
  media = media
)
#> [1] "The locationID: Loc1 had 2 temporally overlapping deployments in group 1. These have now been condensed into one row with the updated deploymentID: Loc1_deployment_1"

# View updated deployments
results$deployments
#>        deploymentID locationID deploymentStart deploymentEnd deploymentGroups
#> 1 Loc1_deployment_1       Loc1      2020-01-01    2020-01-15             <NA>

# Check updated observation and media deployment IDs
results$observations
#>   observationID      deploymentID locationID
#> 1             1 Loc1_deployment_1       Loc1
#> 2             2 Loc1_deployment_1       Loc1
#> 3             3 Loc1_deployment_1       Loc1
#> 4             4 Loc1_deployment_1       Loc1
results$media
#>   mediaID      deploymentID
#> 1      m1 Loc1_deployment_1
#> 2      m2 Loc1_deployment_1
#> 3      m3 Loc1_deployment_1
#> 4      m4 Loc1_deployment_1
```
