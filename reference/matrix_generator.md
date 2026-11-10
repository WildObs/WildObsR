# Generate Detection History and Covariate Matrices from camtrapDP Formatted Data

This function processes camera trapping data formatted using the
camtrapDP data standard to generate detection history matrices along
with site-level and observation-level covariate matrices. These outputs
can then be used for hierarchical occupancy or abundance modeling (e.g.,
with the `unmarked` package).

## Usage

``` r
matrix_generator(
  obs,
  covs,
  dur,
  w,
  site_covs,
  obs_covs,
  all_locationNames,
  scientificNames,
  type,
  individuals,
  cap_count = FALSE
)
```

## Arguments

- obs:

  A `data.frame` of observations. This must include columns for
  date-time information (e.g., `eventStart`, `eventEnd`, a species
  identifier (`scientificName`), and a unique identifier for sampling
  units (e.g., `cellID_10km` in spatially resampled data from
  [`WildObsR::resample_covariates_and_observations()`](https://wildobs.github.io/WildObsR/reference/resample_covariates_and_observations.md)
  or `deploymentID` in regular camtrap DP data).

- covs:

  A `data.frame` of covariates. This must include sampling start/end
  dates (e.g., `samplingStart`, `samplingEnd` in spatially resampled
  data from
  [`WildObsR::resample_covariates_and_observations()`](https://wildobs.github.io/WildObsR/reference/resample_covariates_and_observations.md)
  or `deploymentStart`, `deploymentEnd` in regular camtrap DP data),
  spatial identifiers (matching `obs`), and additional site-level
  covariate columns.

- dur:

  A numeric value indicating the maximum sampling duration (in days)
  where a population is assumed to be closed to changes in size.

- w:

  A numeric value specifying the length (in days) of each sampling
  occasion window that is used to compress the detection history matrix
  to improve model performance by increasing detection probability.

- site_covs:

  A character vector of column names in `covs` to be used as site-level
  covariates.

- obs_covs:

  A character vector of column names in `obs` to be used as
  observation-level covariates.

- all_locationNames:

  A logical indicating whether all locationName values should be
  included (`TRUE`) or only locationNames with species detections
  (`FALSE`) should be included.

- scientificNames:

  A character vector of species names to generate matrices for. Each
  species will be processed separately.

- type:

  A character string indicating the type of model. Use `"abundance"` to
  generate a count history matrix using count data or `"occupancy"` to
  generate a detection history matrix using presence/absence data.

- individuals:

  A character string specifying the method for aggregating individual
  counts used in the count history matrix. Use `"sum"` to sum detections
  across occasions or `"max"` to take the maximum value.

- cap_count:

  Logical. If `TRUE`, the function will automatically cap observed
  counts for each species based on its mean and maximum observed group
  size to prevent extremely large counts from negatively affecting model
  fit. Species with high mean or maximum individual counts will have
  their values capped (e.g., at 3, 5, or 7), while others retain full
  counts. Default is `FALSE`, meaning no capping is applied.

## Value

A named `list` where each element corresponds to a species. Each element
is itself a list with the following components:

- `detection_matrix`:

  A matrix of detection (or count) data with rows representing sampling
  units and columns representing sampling occasion windows.

- `site_level_covariates`:

  A `data.frame` of site-level covariates used in the analysis.

- `observation_level_covariates`:

  A `list` of matrices containing observation-level covariate, where
  each matrix is aligned with the detection history matrix.

## Details

The function performs several key steps:

1.  **Data Validation:** Checks are performed to ensure that:

    - The identifier column (e.g., `cellID` or `deploymentID`) is
      consistent across `obs` and `covs`.

    - Every sampling unit in `obs` has a row in `covs`, and every
      observation covariate is a column of `obs`; otherwise the function
      stops.

    - NA values in site-level or observation-level covariates are kept,
      with a warning: unmarked drops the affected sampling units or
      occasions with its own warning.

    - Date-time columns in both `obs` and `covs` are `POSIXct`. Text
      columns are converted, with a message; a value that cannot be read
      stops the function.

    - No site-level covariate is dropped. One with the same value
      everywhere is kept, standardised to 0 if numeric.

    - The provided species names (`scientificNames`) exist in the `obs`
      table.

    Sampling units with covariates but no observations are kept, and
    enter every species' matrices as sampled zeros, with a message
    naming them.

2.  **Timezone Adjustment:** Date-time values are put in the local time
    zone of the sampling units, looked up from their coordinates with
    [`lutz::tz_lookup_coords`](http://andyteucher.ca/lutz/reference/tz_lookup_coords.md),
    so records fall on their local day. When the sampling units span
    time zones with different clocks, every unit uses UTC days instead,
    with a message.

3.  **Date Sequence and Sampling Occasions:** A complete sequence of
    dates is generated for each sampling unit and an index is assigned
    to each sampling occasion.

4.  **Matrix Generation:** For each species, a detection history matrix
    is created by aligning observations to their respective sampling
    occasions. The matrix can contain either count data (for abundance
    models) or binary presence/absence data (for occupancy models).

5.  **Observation-level Covariate Formatting:** Observation covariates
    are organized into matrices that align with the detection history
    matrix and are compressed to match sampling occasion windows.

## Known limitations

These come from how the inputs are built, and are why
[`effort_matrix`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md),
[`detection_matrix`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md)
and
[`model_matrix_input`](https://wildobs.github.io/WildObsR/reference/model_matrix_input.md)
exist. Prefer them for new analyses.

- **Effort depends on detections.** `numberDeploymentsActiveAtDate` from
  [`resample_covariates_and_observations`](https://wildobs.github.io/WildObsR/reference/resample_covariates_and_observations.md)
  counts the deployments with a record that day, and a day without any
  record counts as one camera. Used as an observation covariate, it
  understates effort where several cameras share a sampling unit and
  varies with what was detected. `matrix_generator()` warns when it is
  used.
  [`effort_matrix`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md)
  calculates camera-days from the deployments alone.

- **Observation covariates come from the species' own records.** On a
  day the species was detected, a covariate takes the values of the
  cameras that detected it; on other days, every value recorded at the
  sampling unit. So the covariate differs between detection and
  non-detection occasions for reasons unrelated to detection.
  [`model_matrix_input`](https://wildobs.github.io/WildObsR/reference/model_matrix_input.md)
  builds observation covariates from the deployments, weighted by
  camera-days.

- **Character observation covariates are pasted together** (for example
  `"none - trail"`) where several values meet, and summarised by their
  most common value per occasion, so they enter models as many-level
  factors.

- **A sampling unit is assumed to run continuously** from its first
  start to its last end, so gaps between deployments in a cell count as
  sampled days.

## See also

[`effort_matrix`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md),
[`detection_matrix`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md)
and
[`model_matrix_input`](https://wildobs.github.io/WildObsR/reference/model_matrix_input.md),
which build effort from deployments and format the result for unmarked,
spOccupancy and spAbundance.

## Author

Zachary Amir

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage with pre-processed camera trap data, accessed from WildObs:

# Load the general use WildObs API key
api_key <- Sys.getenv("WILDOBSR_API_KEY")
# use WildObsR function to access data
dp <- WildObsR::wildobs_dp_download(api_key = api_key, project_ids = "ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001")
# extract first DP from the list
dp <- dp[[1]]

# Extract resources from the data package
covs <- frictionless::read_resource(dp, "covariates")
obs  <- frictionless::read_resource(dp, "observations")
deps <- frictionless::read_resource(dp, "deployments")

# Assign spatial scales
# 'scales' defines the area (in square meters) covered by the hexagonal cell
scales <- c(1000000, 3000000)

# Generate spatial hexagons based on the provided scales
covs <- WildObsR::spatial_hexagon_generator(covs, scales)

# Identify matching columns between deployments and covariates
cols <- names(deps)[names(deps) %in% names(covs)]
# Merge deployment information to the covariates
covs <- merge(deps, covs, by = cols)  # Note: This overwrites the original covs data

# Define columns for mode aggregation (example: 'projectName' and 'habitat')
mode_cols_covs <- names(covs)[grepl("projectName|habitat", names(covs))]
# Set the method for aggregating the total number of individuals detected:
individuals <- "sum"  # Alternative: "max"
# Specify observation-level covariate variables derived from deployments.
# These variables capture information that varies in space and time.
obs_covs <- c("baitUse", "featureType", "setupBy", "cameraModel", "cameraDelay","cameraHeight", "cameraDepth", "cameraTilt", "cameraHeading", "detectionDistance", "deploymentTags")

# now spatially resample the data, noting that this function may take a few minutes to run
resamp_data = WildObsR::resample_covariates_and_observations(covs, obs, individuals = "sum", mode_cols_covs, obs_covs)

# Select the resampled observations and covs at the 1 km scale
resamp_obs = resamp_data$spatially_resampled_observations$cellID_1km
resamp_covs = resamp_data$spatially_resampled_covariates$cellID_1km

# Run the matrix generator function
res <- matrix_generator(
  obs = resamp_obs,
  covs = resamp_covs,
  dur = 110,
  w = 3,
  site_covs = c("mode_habitat", "Avg_human_footprint_10km2", "locationName", "Avg_FLII_3km2"),
  obs_covs = c("cameraHeight", "featureType", "cameraModel"),
  all_locationNames = TRUE,
  scientificNames = c("Orthonyx spaldingii", "Uromys caudimaculatus"),
  type = "abundance",
  individuals = "sum"
)
# Access the detection matrix for one species:
res[["Orthonyx_spaldingii"]][["detection_matrix"]]
} # }
```
