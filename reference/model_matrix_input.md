# Format Detection Histories and Covariates for Occupancy and Abundance Models

Combines the output of
[`detection_matrix()`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md)
and
[`effort_matrix()`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md)
with site and observation covariates, in the exact form each modelling
package expects. Sites are matched to covariates by name, never by row
order, so a reordered covariate table cannot misalign them.

Two observation covariates are always added from the effort matrix:

- `effort`, the camera-days in each site and occasion, for modelling
  detection probability as a function of effort;

- when
  [`effort_matrix()`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md)
  was given `split_by`, one covariate per level, `<split_by>_<level>`,
  holding the share of that occasion's camera-days under the level (for
  example the share of effort at trail cameras).

Where no camera ran, observation covariates are `NA`, matching the
detections.

One site covariate is always added too:

- `total_effort`, each site's camera-days summed over the modelled
  occasions (the row sums of `effort`). It is in the site covariates for
  every output, and also in `det.covs` for single-season spOccupancy and
  spAbundance output, since those packages read detection covariates
  only from there.

## Usage

``` r
model_matrix_input(
  detections,
  effort,
  site_covs = NULL,
  obs_covs = NULL,
  coords = NULL,
  output = c("unmarked", "spOccupancy", "spAbundance", "array"),
  seasons = NULL
)
```

## Arguments

- detections:

  The array returned by
  [`detection_matrix()`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md)
  (species x site x occasion).

- effort:

  The
  [`effort_matrix()`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md)
  the detections were built from.

- site_covs:

  A data frame of site covariates, or `NULL`. Sites are matched by a
  column with the same name as the effort matrix's `site_col` (for
  example `"locationID"`) or a `site` column, else by row names.

- obs_covs:

  A named list of observation covariates, each a site x occasion matrix
  with the same site and occasion names as `detections`, or `NULL`.
  These are for covariates you build yourself, such as rainfall per
  occasion. `effort` and the `<split_by>_<level>` shares are always
  added from the effort matrix, so leave `split_by` columns out; a name
  that clashes with an automatic covariate stops the function. Use one
  share per split variable in a model formula, since the shares of all
  levels add up to 1.

- coords:

  Site coordinates for spatial models, or `NULL`: a data frame with the
  site column and two coordinate columns (for example projected `X` and
  `Y` in metres), or a two-column matrix with sites as row names.

- output:

  `"unmarked"` (default), `"spOccupancy"`, `"spAbundance"` or `"array"`.

- seasons:

  A vector or `NULL`. For multi-season models, the season of every
  occasion (one value per column of `detections`), for example the year
  each occasion starts in. Needs an effort matrix built with
  `align = "calendar"`, so every site's occasion `k` covers the same
  dates.

## Value

The model input described for `output`. Stops if a site in `detections`
has no row in `site_covs` or `coords`, if `obs_covs` do not match the
detections, or if a needed modelling package is not installed.

## effort or total_effort?

Both describe sampling effort, at different resolutions:

- `effort` varies by occasion, so an occasion when one of a cell's three
  cameras ran is not treated like one when all three ran. It is the
  better choice for detection probability whenever the model accepts
  observation covariates.

- `total_effort` is one value per site. Use it in models or formulas
  that take site covariates only, or to compare with `effort`: if
  detection depends on `total_effort` but not on `effort`, sites differ
  in how much they were sampled more than occasions do.

`total_effort` counts only the occasions in the detections, so it leaves
out the days after `dur` that
[`effort_matrix()`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md)
dropped. It therefore differs from `cellEffort` in the resampled
covariates of
[`resample_covariates_and_observations()`](https://wildobs.github.io/WildObsR/reference/resample_covariates_and_observations.md),
which counts every day each camera ran. The two give the same total only
when `dur` keeps every day. A `site_covs` column named `total_effort`
stops the function.

The outputs are:

- `"unmarked"`:

  An `unmarkedFrameOccu` (occupancy) or `unmarkedFramePCount`
  (abundance) for one species; an `unmarkedFrameOccuComm` for several
  species with occupancy data, for
  [`unmarked::occuComm()`](https://ecoverseR.github.io/unmarked/reference/occuComm.html);
  a named list of `unmarkedFramePCount` for several species with
  abundance data; and an `unmarkedMultFrame` (one species) when
  `seasons` is given, for
  [`unmarked::colext()`](https://ecoverseR.github.io/unmarked/reference/colext.html)
  or
  [`unmarked::pcountOpen()`](https://ecoverseR.github.io/unmarked/reference/pcountOpen.html).

- `"spOccupancy"`:

  A list with `y`, `occ.covs`, `det.covs` and `coords`: `y` is site x
  occasion for one species (`PGOcc()`, `spPGOcc()`), species x site x
  occasion for several (`msPGOcc()`, `sfMsPGOcc()`), and gains a season
  dimension before the occasions when `seasons` is given (`tPGOcc()`,
  `tMsPGOcc()`).

- `"spAbundance"`:

  A list with `y`, `abund.covs`, `det.covs` and `coords`, for `NMix()`
  (one species) or `msNMix()` (several).

- `"array"`:

  Plain arrays and tables for custom models in JAGS, NIMBLE or Stan:
  `y`, `effort`, `effort_by_unit`, `site_covs`, `obs_covs`, `coords`,
  the dimensions, and `observed`, which is `TRUE` where a camera ran.

## See also

[`effort_matrix()`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md)
and
[`detection_matrix()`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md),
which build its inputs.

## Author

Zachary Amir

## Examples

``` r
# one camera at each of three stations for 10 days
deps <- data.frame(
  deploymentID = c("A", "B", "C"),
  locationID = c("s1", "s2", "s3"),
  deploymentStart = as.POSIXct("2024-03-01 00:00", tz = "UTC"),
  deploymentEnd = as.POSIXct("2024-03-11 00:00", tz = "UTC"),
  baitUse = c(TRUE, FALSE, TRUE)
)
eff <- effort_matrix(deps, site_col = "locationID", w = 5, split_by = "baitUse")

# a feral pig at two stations
obs <- data.frame(
  deploymentID = c("A", "C"), eventID = c("e1", "e2"), scientificName = "Sus scrofa",
  eventStart = as.POSIXct(c("2024-03-02 03:00", "2024-03-08 21:00"), tz = "UTC")
)
y <- detection_matrix(obs, eff, "Sus scrofa")

# site covariates, deliberately in a different order to the detections
covs <- data.frame(locationID = c("s3", "s1", "s2"), elevation = c(820, 640, 700))

# plain arrays for a custom model
inp <- model_matrix_input(y, eff, site_covs = covs, output = "array")
inp$site_covs
#>    elevation total_effort
#> s1       640           10
#> s2       700           10
#> s3       820           10
names(inp$obs_covs)
#> [1] "effort"        "baitUse_FALSE" "baitUse_TRUE" 

# an unmarked occupancy frame
if (requireNamespace("unmarked", quietly = TRUE)) {
  umf <- model_matrix_input(y, eff, site_covs = covs, output = "unmarked")
  umf
}
#> Data frame representation of unmarkedFrame object.
#>    y.1 y.2 elevation total_effort effort.1 effort.2 baitUse_FALSE.1
#> s1   1   0       640           10        5        5               0
#> s2   0   0       700           10        5        5               1
#> s3   0   1       820           10        5        5               0
#>    baitUse_FALSE.2 baitUse_TRUE.1 baitUse_TRUE.2
#> s1               0              1              1
#> s2               1              0              0
#> s3               0              1              1
```
