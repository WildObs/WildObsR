# Spatially resample the covariates and observations resources Spatially Resample Covariates and Observations Across Spatial Scales

This function resamples covariate and observation data across spatial
scales generated from the
[`WildObsR::spatial_hexagon_generator()`](https://wildobs.github.io/WildObsR/reference/spatial_hexagon_generator.md)
function. It validates the input data by ensuring that all necessary
spatial, temporal, and deployment identifiers are present and correctly
formatted. Numeric covariates are aggregated using means weighted by
each deployment's camera-days (see `weights`), while categorical
covariates are combined by concatenating unique values. Logical and
factor columns in either table, such as `baitUse` and `featureType` from
[`frictionless::read_resource()`](https://docs.ropensci.org/frictionless/reference/read_resource.html),
keep their class (and factor levels) when every cell holds one value;
where some cell holds several, the column is pasted as text, for example
`"FALSE - TRUE"`, with a message naming it. Modal columns always keep
their class.

With `obs = NULL`, only the covariates are resampled, which is all the
occupancy and abundance workflow needs, and is much faster (see "How the
tables connect").

In addition, the function computes several key metrics:

- Sampling effort per spatial unit (trap-nights),

- Resampled start and end dates for each sampling unit.

Users can optionally specify which covariate columns should be
aggregated using the mode (i.e., the most frequent value) instead of
concatenation, and which covariate variables that vary in space and time
(i.e., observation covariates) should be incorporated into the resampled
observations. Example values provided in the documentation
(`c("habitat")` for mode aggregation and `c("baitUse", "featureType")`
for observation covariates) serve as examples that work with any camtrap
DP formatted data.

The function generates new columns in the output data as follows:

**Resampled Covariates** include:

- `deploymentsIncluded`:

  A concatenated string of `deploymentID` values representing all
  deployments included within each spatial cell.

- `cellEffort`:

  The total sampling effort per cell, calculated as trap-nights (i.e.,
  the number of active deployments times the duration they were active,
  in days).

- `samplingStart`:

  The earliest `deploymentStart` date-time among the deployments in a
  cell, marking the beginning of the sampling period per cellID.

- `samplingEnd`:

  The latest `deploymentEnd` date-time among the deployments in a cell,
  marking the end of the sampling period per cellID.

**Resampled Observations** include:

- `deploymentsActiveAtDate`:

  A concatenated string of `deploymentID` values that were active in a
  given resampled cellID on each day.

- `numberDeploymentsActiveAtDate`:

  The number of deployments active in a given resampled cell on each
  day.

- `independentEvents`:

  The total count of unique `eventID` values per cell per day per
  species, representing independent events.

- `independentObservations`:

  The total count of unique `observationID` values per cell per day per
  species.

- `totalIndividuals`:

  The aggregated count of individuals per cell per day per species,
  computed based on the `individuals` parameter (either by summing
  counts or taking the maximum value).

## Usage

``` r
resample_covariates_and_observations(
  covs,
  obs = NULL,
  individuals,
  mode_cols_covs,
  obs_covs,
  weights = c("camera_days", "none")
)
```

## Arguments

- covs:

  A data frame or tibble of covariate data from a camtrapDP formatted
  frictionless data package, which can be accessed from the
  [`WildObsR::wildobs_dp_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
  function. It must include columns defining spatial cells (with names
  starting with "cellID"), and date-time columns (e.g.,
  `deploymentStart` and `deploymentEnd`) formatted as POSIXct.

- obs:

  A data frame or tibble of observation data from a camtrapDP formatted
  frictionless data package, which can be accessed from the
  [`WildObsR::wildobs_dp_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
  function. It must include a `deploymentID` column and several
  date-time columns (e.g., `eventStart`, `eventEnd`,
  `classificationTimestamp` formatted as POSIXct. Or `NULL` (default) to
  resample the covariates only; `individuals` and `obs_covs` are then
  not used.

- individuals:

  A character string specifying how to aggregate the number of
  individuals counted per spatially re sampled cell per day. Options are
  `"sum"` to take the sum total number of counts or `"max"` to use the
  maximum count. `"sum"` adds the count of every observation of the
  species in the cell that day, across all its cameras and repeat
  triggers, so one cat triggering a camera three times in 10 minutes
  counts 3, and a cat passing two cameras counts 2: an index of animal
  detections, not a number of individuals. `"max"` is the most
  individuals in a single observation, a conservative minimum present.
  This only affects the resampled observations;
  [`detection_matrix()`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md)
  counts from the observations themselves, with its own `individuals`.

- mode_cols_covs:

  (Optional) A character vector specifying the names of covariate
  columns for which the mode should be computed rather than
  concatenating all unique values. (Example: `c("habitat")`). With
  `weights = "camera_days"`, the mode is the value covering the most
  camera-days in the cell.

- obs_covs:

  (Optional) A character vector specifying observation-level covariate
  column names that may vary in space and time and will be combined with
  the resampled observations. (Example: `c("baitUse", "featureType")`).
  These are used only by
  [`matrix_generator()`](https://wildobs.github.io/WildObsR/reference/matrix_generator.md);
  for detection covariates with
  [`effort_matrix()`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md),
  use its `split_by` instead.

- weights:

  How each deployment counts towards its cell's averaged (`Avg_*`) and
  modal (`mode_*`) covariates. `"camera_days"` (default) weights each
  deployment by how long it ran, `deploymentEnd - deploymentStart` in
  days; `"none"` counts every deployment once, the plain mean and most
  frequent value. See "Weighting by camera-days".

## Value

A nested list with two elements:

- `spatially_resampled_observations`:

  A list of data frames corresponding to different spatial scales (e.g.,
  "cellID_1km", "cellID_10km") containing all resampled observation
  data. Left out when `obs = NULL`.

- `spatially_resampled_covariates`:

  A list of data frames corresponding to different spatial scales
  containing all resampled covariate data, one row per cell.

Each table carries a `wildobs_resampled` attribute naming its scale, so
[`effort_matrix()`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md)
and
[`detection_matrix()`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md)
can recognise them and say what to pass instead.

## Details

The function performs the following steps to resample the covariate and
observation data:

1.  **Data Validation**:

    - Verifies that the covariates data frame contains at least one
      spatial cellID column generated from the
      [`WildObsR::spatial_hexagon_generator()`](https://wildobs.github.io/WildObsR/reference/spatial_hexagon_generator.md)
      function (i.e., a column whose name starts with "cellID"). If not,
      the function stops with an error.

    - Checks that the `deploymentID` values match between the covariates
      and observation data. If there is a mismatch, the function stops
      with an error.

    - Ensures that the specified observation-level covariate columns
      (provided in `obs_covs`) exist in the covariates data. If some are
      missing, they are omitted from further processing with a warning.

    - Confirms that all required date-time columns in both datasets are
      formatted as `POSIXct`. This includes columns such as
      `eventStart`, `eventEnd`, and `classificationTimestamp` in the
      observations, and `deploymentStart` and `deploymentEnd` in the
      covariates.

2.  **Data Preparation**:

    - Removes any columns that contain only `NA` values from both
      datasets.

    - For the covariates:

      - Extracts character columns (excluding those that will be
        processed by mode aggregation) and numeric columns that will be
        averaged.

      - Adds sequential Julian date values derived from
        `deploymentStart` and `deploymentEnd` to standardize the
        temporal resolution. These columns are removed before returning
        the final datasets.

    - For the observations:

      - Extracts date-time columns (identified by the `POSIXct` class),
        character/factor columns, and numeric columns (excluding those
        with names like "deltaTime" or "count").

      - Computes a sequential Julian date from `eventStart` to serve as
        the daily resolution for resampling. This column is removed
        before returning the final datasets.

3.  **Resampling by Deployment Group and Spatial Scale**:

    - The function loops over each unique `deploymentGroup` present in
      the covariates data. This speeds up the process.

    - For each `deploymentGroup`:

      - Subsets the covariates and observations to include only the
        relevant deployments.

      - Merges spatial cell information (i.e., the `cellID` columns)
        from the covariates into the observations based on the
        `deploymentID`.

4.  **Resampling of Covariates**:

    - Within each `deploymentGroup`, the function iterates over each
      spatial scale (columns containing "cellID\_").

    - For each spatial scale, it loops through each unique sampling unit
      (i.e., each unique cell value):

      - A new row is created for each sampling unit, starting as an
        empty data frame with the same structure as the covariates data.

      - Numeric covariate values are aggregated by computing the mean,
        weighted by camera-days unless `weights = "none"` (or left as
        `NA` if all values are missing). The column names are updated to
        indicate that these are averaged values (e.g.,
        `"Avg_<column>"`).

      - Sampling effort is calculated as the total active duration (in
        days) summed over all unique deployments in the cell.

      - The sampling period is defined by the minimum `deploymentStart`
        (as `samplingStart`) and maximum `deploymentEnd` (as
        `samplingEnd`) for the cell.

      - Character variables are processed by pasting together sorted
        unique values.

      - For any covariate specified in `mode_cols_covs`, the function
        calculates the mode (the value covering the most camera-days, or
        the most frequent value with `weights = "none"`) instead of
        concatenating all values.

      - Spatial information is preserved by keeping the cell identifier
        and any associated polygon geometry.

5.  **Resampling of Observations**:

    - For each spatial scale, the function further processes the
      observations by iterating over each sampling unit, each day (based
      on the sequential Julian date), and each species
      (`scientificName`).

    - For each combination:

      - The function merges relevant observation-level covariate
        information from the covariates dataset.

      - Date-time columns are aggregated: start times are set to the
        minimum value and end times to the maximum.

      - Active deployment information is compiled, including a
        concatenated list of active deployments and the count of these
        deployments.

      - Event-level metrics are computed, such as the number of unique
        events and observations.

      - Individual counts are aggregated according to the user-specified
        method (either `"sum"` or `"max"`).

      - Species-level variables are aggregated by concatenating sorted
        unique values.

      - Numeric observation columns are averaged.

6.  **Compilation of Resampled Data**:

    - After processing all `deploymentGroups` and spatial scales, a
      final verification is implemented to ensure all cellID values are
      present and matching in the resampled covariates and observations.

    - Finally, the function assembles the resampled data into two nested
      lists:

      - One list contains the resampled covariates, organized by spatial
        scale.

      - The other list contains the resampled observations, also
        organized by spatial scale.

7.  **Output**:

    - The function returns a single list with two elements:

      - `"spatially resampled observations"`: a nested list of data
        frames for each spatial scale.

      - `"spatially resampled covariates"`: a nested list of data frames
        for each spatial scale.

## How the tables connect

The occupancy and abundance workflow uses three tables, each at the
level its function needs, joined by two keys: `deploymentID` links
observations to deployments, and the cellID column (for example
`cellID_3km2`) links deployments to cells.

|  |  |  |  |
|----|----|----|----|
| Table | One row per | Made by | Goes into |
| Deployments with cellIDs (`covs_cellID`) | deployment | [`spatial_hexagon_generator()`](https://wildobs.github.io/WildObsR/reference/spatial_hexagon_generator.md) | [`effort_matrix()`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md) with `site_col = "cellID_3km2"` |
| Observations | observation | [`frictionless::read_resource()`](https://docs.ropensci.org/frictionless/reference/read_resource.html) | [`detection_matrix()`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md), which places each one in its cell through the effort matrix |
| Cell covariates | cell | this function, with `obs = NULL` | [`model_matrix_input()`](https://wildobs.github.io/WildObsR/reference/model_matrix_input.md) as `site_covs` |

Effort needs each camera's own start and end, which only the deployment
table keeps, so the resampled covariates are used for site covariates
only. The resampled observations are for
[`matrix_generator()`](https://wildobs.github.io/WildObsR/reference/matrix_generator.md)
and for summaries per cell and day.

## Weighting by camera-days

A covariate that does not change over time, such as elevation, still
differs between the cameras in a cell, and the cell's value is an
average over them. Weighting by camera-days makes that average describe
the habitat the cameras actually sampled, the same camera-days the
effort and detections come from. Two examples:

- Camera A at 200 m ran 90 days; camera B at 800 m failed after 2 days.
  The plain mean is 500 m; the weighted mean is (200 x 90 + 800 x 2) /
  92 = 213 m, where nearly all the detections came from.

- Station X at 200 m was redeployed four times for 25 days each; station
  Y at 800 m ran once for 100 days. The plain mean over the five rows is
  320 m, counting X four times only because its cards were changed more
  often; the weighted mean is 500 m, treating both stations' equal
  effort equally.

When the cameras in a cell ran for similar lengths the two agree
closely: on three public WildObs projects at 3 km² cells, weighting
moved values by a median of under 5% of the spread between cameras in
the same cell. The difference matters for failed cameras, redeployed
stations and large cells.

Coordinates (`latitude`, `longitude`) are always plain means, since they
describe where the cameras are rather than what they sampled. A
deployment with a missing or negative length has no weight; a cell where
no deployment has any falls back to the plain mean, with a warning.
`cellEffort` is unaffected: it is always the total camera-days.

## Author

Zachary Amir

## Examples

``` r
# load the Woomargama wombat project shipped with WildObsR
dp <- example_datapackages("NSW_Woomargama_wombats_Linley_2021-22_WildObsID_0010")[[1]]

# extract one table of deployments and their covariates
deps <- frictionless::read_resource(dp, "deployments")
covs <- frictionless::read_resource(dp, "covariates")
# merge both tables together based on several shared columns 
covs <- merge(deps, covs, by = intersect(names(deps), names(covs)))

# assign every camera to a 1 km2 hexagonal cell (scales are cell areas in m2)
covs_cellID <- spatial_hexagon_generator(covs, scales = 1e6)

# for occupancy and abundance models, resample the covariates only: one row per cell
cell <- resample_covariates_and_observations(covs_cellID, mode_cols_covs = "baitUse")
cell_covs <- cell$spatially_resampled_covariates$cellID_1km2
head(cell_covs[, c("cellID_1km2", "cellEffort", "mode_baitUse")])
#>                                           cellID_1km2 cellEffort mode_baitUse
#> 1 Woomargama_NP_177_cellID_1km2_2021_a_WildObsID_0010   190.5010        FALSE
#> 2 Woomargama_NP_183_cellID_1km2_2021_a_WildObsID_0010   191.9194        FALSE
#> 3 Woomargama_NP_130_cellID_1km2_2021_a_WildObsID_0010   188.6898        FALSE
#> 4 Woomargama_NP_124_cellID_1km2_2021_a_WildObsID_0010   187.9236        FALSE
#> 5  Woomargama_NP_11_cellID_1km2_2021_a_WildObsID_0010   192.9283        FALSE
#> 6  Woomargama_NP_23_cellID_1km2_2021_a_WildObsID_0010   196.2188        FALSE

# observations that can be placed on a camera (obscured records cannot)
obs <- frictionless::read_resource(dp, "observations")
obs <- obs[obs$deploymentID %in% covs_cellID$deploymentID, ]

# or resample the observations to cells and days too, for matrix_generator() or summaries
resamp <- resample_covariates_and_observations(covs_cellID, obs, individuals = "sum",
                                               obs_covs = c("baitUse", "featureType"))
#> Warning: resample_covariates_and_observations() found sampled cells with no observations:
#>   Woomargama_NP_2022_b_Linley_WildObsID_0010 at cellID_1km2: 1 of 27 cells
#> This is expected when obs is filtered to a few species, or when a cell detected nothing: the cells stay in the resampled covariates, and effort_matrix() and matrix_generator() count them as sampled with no detections (zeros). It is a problem if obs was filtered by deployment, date or observationType in a way that removed real records, since those zeros would then be false absences. Check that obs covers every deployment in covs.
#> resample_covariates_and_observations(): lifeStage (e.g. "adult - juvenile"), sex (e.g. "female - male") kept as text, because some cells hold more than one value.
#> For one value per cell, add the column to mode_cols_covs; to model detection by it, use effort_matrix(split_by = ...).
head(resamp$spatially_resampled_observations$cellID_1km2[, c("cellID_1km2", "scientificName",
                                                             "totalIndividuals")])
#>                                           cellID_1km2        scientificName
#> 1 Woomargama_NP_177_cellID_1km2_2021_a_WildObsID_0010 Trichosurus vulpecula
#> 2 Woomargama_NP_177_cellID_1km2_2021_a_WildObsID_0010 Trichosurus vulpecula
#> 3 Woomargama_NP_177_cellID_1km2_2021_a_WildObsID_0010      Vombatus ursinus
#> 4 Woomargama_NP_177_cellID_1km2_2021_a_WildObsID_0010 Trichosurus vulpecula
#> 5 Woomargama_NP_177_cellID_1km2_2021_a_WildObsID_0010      Vombatus ursinus
#> 6 Woomargama_NP_177_cellID_1km2_2021_a_WildObsID_0010      Vombatus ursinus
#>   totalIndividuals
#> 1                1
#> 2                3
#> 3                1
#> 4                1
#> 5                1
#> 6                1
```
