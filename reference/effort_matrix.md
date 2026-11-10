# Build a Sampling Effort Matrix from Camera Deployments

Calculates how long cameras were running, as camera-days, for every site
and sampling occasion, from the deployments table alone. Sampling effort
happens regardless of what was detected, so it cannot depend on where or
when animals were recorded, which is the requirement for using effort as
a detection covariate or offset.

The steps are:

1.  Each deployment's running time is split into local calendar days (in
    `time_zone`), and each day gets the fraction of it the camera was
    running: a camera set at noon contributes 0.5 camera-days on its
    first day.

2.  Days are grouped into non-overlapping occasions of `w` days.
    Occasion 1 covers days 1 to `w`, occasion 2 days `w + 1` to `2w`,
    and so on; a final, shorter occasion is kept, and its length is
    reported in `occasions`.

3.  Camera-days are summed for every site, occasion and deployment. A
    site can be a single deployment (`site_col = "deploymentID"`), a
    camera station (`"locationID"`) with several deployments, or a grid
    cell from
    [`spatial_hexagon_generator()`](https://wildobs.github.io/WildObsR/reference/spatial_hexagon_generator.md).

With `align = "site"` (default), each site's occasions start on the
first day any of its cameras ran, as in most single-season occupancy
designs. With `align = "calendar"`, every site's occasions start on the
first day any camera in `deps` ran, so occasion `k` covers the same
dates everywhere, which multi-season and time-series models need.

For long-term deployments rebuilt with
[`recombine_deployment_groups()`](https://wildobs.github.io/WildObsR/reference/recombine_deployment_groups.md),
pass its `deployment_crosswalk` as `crosswalk`: effort is then taken
from the original cameras, so the gaps between them (up to
`max_gap_days`) count as no effort.

## Usage

``` r
effort_matrix(
  deps,
  site_col = "deploymentID",
  w,
  dur = NULL,
  align = c("site", "calendar"),
  split_by = NULL,
  crosswalk = NULL,
  time_zone = NULL
)
```

## Arguments

- deps:

  A data frame of deployments, one row per deployment, with at least
  `deploymentID`, `deploymentStart` and `deploymentEnd` (POSIXct), and
  the column named in `site_col`. For grid cells, pass the output of
  [`spatial_hexagon_generator()`](https://wildobs.github.io/WildObsR/reference/spatial_hexagon_generator.md),
  never the resampled covariates, which have one row per cell and stop
  the function.

- site_col:

  Character. The column that defines a site. Defaults to
  `"deploymentID"`, one site per deployment. Sites with multiple camera
  deployments can be a station (`"locationID"`) or a grid cell from
  [`spatial_hexagon_generator()`](https://wildobs.github.io/WildObsR/reference/spatial_hexagon_generator.md),
  for example `"cellID_3km2"`; see "How the tables connect".

- w:

  Integer. Occasion length in days.

- dur:

  Integer or `NULL`. Number of days, counted from each site's (or the
  calendar) start, to keep. Defaults to `NULL`, which keeps every day
  with effort.

- align:

  `"site"` (default) or `"calendar"`; see Description.

- split_by:

  Character or `NULL`. A deployments column, such as `"featureType"` or
  `"baitUse"`, to split effort by. When given, the third dimension of
  `effort` holds its levels instead of individual deployments, so the
  share of effort under each level can be used as an observation
  covariate. One column only: to split by two, combine them into one
  column first, for example
  `deps$bait_trail <- paste(deps$baitUse, deps$trailStatus, sep = "_")`,
  then `split_by = "bait_trail"`. Missing values become their own level,
  `"NA"`, so decide what they mean before splitting.

- crosswalk:

  A data frame or `NULL`. The `deployment_crosswalk` from
  [`recombine_deployment_groups()`](https://wildobs.github.io/WildObsR/reference/recombine_deployment_groups.md),
  with `deploymentID`, `pieceStart` and `pieceEnd`.

- time_zone:

  Character or `NULL`. The time zone that defines a calendar day, such
  as `dp$temporal$timeZone`. Defaults to the time zone of
  `deploymentStart`, then UTC. When the deployments span several time
  zones (several values given, or, with `NULL`, cameras whose
  coordinates fall in zones with different clocks, such as Queensland
  and New South Wales in summer), every camera uses UTC days, with a
  message. Effort and detections share the same day boundaries, so the
  matrices stay consistent; the day simply starts at 10 am in AEST, so
  an early-morning detection falls on the previous UTC day. That is
  harmless for occasions of several days. Zone names with the same
  clock, such as `Australia/Brisbane` and `Australia/Lindeman`, count as
  one.

## Value

A list of class `wildobs_effort`:

- `effort`:

  Array of camera-days, site x occasion x deployment (or x level of
  `split_by`). A site's deployments fill the first slots of the third
  dimension; `deployments` says which deployment is in which slot.

- `effort_total`:

  Matrix of camera-days, site x occasion. Zero means no camera was
  running.

- `occasions`:

  One row per site and occasion: `site`, `occasion`, `start` and `end`
  dates, `days` in the occasion and `camera_days` of effort.

- `deployments`:

  One row per deployment: `deploymentID`, `site`, `slot` (its position
  in the third dimension) and the `split_by` level.

- `daily`:

  One row per deployment and active day: `deploymentID`, `site`, `date`,
  `occasion` and `camera_days`.
  [`detection_matrix()`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md)
  uses it to place detections.

- `settings`:

  The arguments used, and each site's first day (`origin`).

Stops if a required column is missing, a date-time is not POSIXct, a
deployment ends before it starts, or `w` is not a positive whole number.

## How the tables connect

The occupancy and abundance workflow uses three tables, each at the
level its function needs, joined by two keys: `deploymentID` links
observations to deployments, and the site column (for example
`cellID_3km2`) links deployments to sites.

|  |  |  |  |
|----|----|----|----|
| Table | One row per | Made by | Goes into |
| Deployments with sites (`covs_cellID`) | deployment | [`frictionless::read_resource()`](https://docs.ropensci.org/frictionless/reference/read_resource.html), plus [`spatial_hexagon_generator()`](https://wildobs.github.io/WildObsR/reference/spatial_hexagon_generator.md) for cells | `effort_matrix()` |
| Observations | observation | [`frictionless::read_resource()`](https://docs.ropensci.org/frictionless/reference/read_resource.html) | [`detection_matrix()`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md), which places each one in its site through the effort matrix |
| Site covariates | site | [`resample_covariates_and_observations()`](https://wildobs.github.io/WildObsR/reference/resample_covariates_and_observations.md) with `obs = NULL`, for cells | [`model_matrix_input()`](https://wildobs.github.io/WildObsR/reference/model_matrix_input.md) as `site_covs` |

Effort needs each camera's own start and end, which only the deployment
table keeps. The resampled covariates have one row per cell, so they are
for site covariates only.

## See also

[`detection_matrix()`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md)
for the matching detections, and
[`recombine_deployment_groups()`](https://wildobs.github.io/WildObsR/reference/recombine_deployment_groups.md)
for long-term deployments.

## Author

Zachary Amir

## Examples

``` r
# two stations: one camera running 10 days, one starting at noon and running 4.5 days
deps <- data.frame(
  deploymentID = c("A", "B"),
  locationID = c("station_1", "station_2"),
  deploymentStart = as.POSIXct(c("2024-03-01 00:00", "2024-03-01 12:00"), tz = "UTC"),
  deploymentEnd = as.POSIXct(c("2024-03-11 00:00", "2024-03-06 00:00"), tz = "UTC")
)

# camera-days per station in 5-day occasions
eff <- effort_matrix(deps, site_col = "locationID", w = 5)
eff$effort_total
#>            occasion
#> site         o1 o2
#>   station_1 5.0  5
#>   station_2 4.5  0
eff$occasions
#>        site occasion      start        end days camera_days
#> 1 station_1        1 2024-03-01 2024-03-05    5         5.0
#> 2 station_1        2 2024-03-06 2024-03-10    5         5.0
#> 3 station_2        1 2024-03-01 2024-03-05    5         4.5
#> 4 station_2        2 2024-03-06 2024-03-10    5         0.0
```
