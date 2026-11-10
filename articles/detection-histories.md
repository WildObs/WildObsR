# Detection histories and sampling effort

Hierarchical occupancy and abundance models that account for imperfect
detection of wildlife require two things from camera trap data: a
**detection history** (was the species detected at each site on each
sampling occasion?) and the **sampling effort** behind it (how long were
cameras running there?). This vignette builds both from a WildObs data
package, first with camera stations as sites and then across spatially
resampled hexagonal grid cells. It uses project 0001, a systematic
survey of six Wet Tropics landscapes shipped with WildObsR, so
everything runs offline.

The next vignettes, *Fit single-season models* and *Fit multi-season
models*, take these matrices into unmarked, spOccupancy, spAbundance and
NIMBLE.

## Which function should I use?

| You want to | Use |
|----|----|
| Camera-days per site and occasion, from the deployments alone | [`effort_matrix()`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md) |
| Detections that line up exactly with that effort | [`detection_matrix()`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md) |
| The format a modelling package expects, with covariates matched by site | [`model_matrix_input()`](https://wildobs.github.io/WildObsR/reference/model_matrix_input.md) |
| One row of covariates per grid cell | `resample_covariates_and_observations(obs = NULL)` |
| To reproduce an analysis built before WildObsR v0.4 | [`matrix_generator()`](https://wildobs.github.io/WildObsR/reference/matrix_generator.md) (legacy) |

In the latest v0.4 release,
[`effort_matrix()`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md),
[`detection_matrix()`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md)
and
[`model_matrix_input()`](https://wildobs.github.io/WildObsR/reference/model_matrix_input.md)
replace
[`matrix_generator()`](https://wildobs.github.io/WildObsR/reference/matrix_generator.md)
for new work. Their sampling effort comes from the deployments, so it
never depends on where animals were detected, and they work on any
dataset formatted in Camtrap DP.

## Three tables, two keys

Each function takes the table at the level it needs:

| Table | One row per | Goes into |
|----|----|----|
| Deployments | deployment | [`effort_matrix()`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md) |
| Observations | observation | [`detection_matrix()`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md) |
| Site-level covariates | site | `model_matrix_input(site_covs = )` |

`deploymentID` links observations to deployments, and the site column
(`locationID` here, or a cellID after spatial resampling later) links
deployments to sites. Nothing needs to be summarised beforehand except
the site covariates, because a model needs exactly one value per site.

``` r

# load the R package 
library(WildObsR)

# grab the Wet Tropics survey shipped with WildObsR, the first example dataset 
dp <- example_datapackages("ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001")[[1]]

# extract the three tables, read with their schema types
deps <- frictionless::read_resource(dp, "deployments")
covs <- frictionless::read_resource(dp, "covariates")
obs <- frictionless::read_resource(dp, "observations")
# merge deployments and their environmental covariates as one table, based on shared columns
deps <- merge(deps, covs, by = intersect(names(deps), names(covs)))

# verify deployments and covariates didn't change rows
nrow(deps) == nrow(covs) # must be TRUE
#> [1] TRUE

# view deployments, camera stations and observations
c(deployments = nrow(deps), stations = length(unique(deps$locationID)), observations = nrow(obs))
#>  deployments     stations observations 
#>          275          145        33522
```

There are nearly twice as many deployments as stations, because most
stations held two cameras placed on different `featureTypes`. A station
is the natural site when cameras were placed on purpose at fixed points.

## Sampling effort

[`effort_matrix()`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md)
splits every deployment into the local calendar days it ran, with the
fraction of each day covered, and sums those camera-days per site and
occasion. Here an occasion is 7 days (`w = 7`) and only the first 70
days at each station are kept (`dur = 70`), so every station has at most
10 occasions.

``` r

# camera-days per station and 7-day occasion, over each station's first 70 days
eff <- effort_matrix(deps, site_col = "locationID", w = 7, dur = 70,
                    # specify the timezone from temporal metadata in the descriptor 
                     time_zone = dp$temporal$timeZone)

# inspect the number of stations x occasions x deployment slots
dim(eff$effort)
#> [1] 145  10   2
# view the total effort over a station's cameras, for the first four stations
round(eff$effort_total[1:4, ], 1)
#>                          occasion
#> site                        o1 o2   o3 o4 o5 o6 o7 o8   o9 o10
#>   CFRAG_01_WildObsID_0001 13.3 14 14.0 14 14 14 14 14 13.4   7
#>   CFRAG_02_WildObsID_0001 13.2 14 14.0 14 14 14 14 14 10.3   7
#>   CFRAG_03_WildObsID_0001 13.1 14 14.0 14 14 14 14 14 14.0  14
#>   CFRAG_04_WildObsID_0001 13.0 14  9.6  7  7  7  7  7  7.0   7
```

A full week with both cameras running gives 14 camera-days; a week where
one camera stopped gives 7, and the first week is a little short because
cameras were set up during the day. An occasion of 0 means no camera
ran, which happens at the end of a station’s sampling, or in gaps
between deployments. Effort is calculated from deployment start and end
times only, so it is the same for whatever species you later model.

The `occasions` table gives each occasion’s dates, and `deployments`
says which deployment sits in which slot of the third dimension:

``` r

# view the dates of each station's occasions
head(eff$occasions, 3)
#>                      site occasion      start        end days camera_days
#> 1 CFRAG_01_WildObsID_0001        1 2022-12-15 2022-12-21    7    13.28319
#> 2 CFRAG_01_WildObsID_0001        2 2022-12-22 2022-12-28    7    14.00000
#> 3 CFRAG_01_WildObsID_0001        3 2022-12-29 2023-01-04    7    14.00000
# view which deployment is in which slot at each station, which links observations
head(eff$deployments, 3)
#>                        deploymentID                    site slot
#> 1 CFRAG_01_WildObsID_0001_2022_Cam1 CFRAG_01_WildObsID_0001    1
#> 2 CFRAG_01_WildObsID_0001_2022_Cam2 CFRAG_01_WildObsID_0001    2
#> 3 CFRAG_02_WildObsID_0001_2022_Cam1 CFRAG_02_WildObsID_0001    1
```

By default each station’s occasions start on its own first day
(`align = "site"`). With `align = "calendar"`, occasion `k` covers the
same dates at every station, which multi-season and time-series models
need; see *Long-term monitoring series*.

## Detection histories

[`detection_matrix()`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md)
places every detection on its deployment’s site and local day, then
fills the same sites and occasions as the effort. A cell is `NA` where
no camera ran and 0 where cameras ran without detecting the species, so
a non-detection is never confused with no sampling.

``` r

# specify three common species of the Wet Tropics rainforest
species <- c("Thylogale stigmatica", "Hypsiprymnodon moschatus", "Sus scrofa")
# create a matrix filled by detected (1) or not (0) per station and occasion
y <- detection_matrix(obs, eff, species)
#> detection_matrix(): set aside 452 obscured observation(s), which carry no camera.
#> Detection histories from public data under-count those species where they are listed.
#> detection_matrix(): left out 671 detection(s) after the first 70 days (dur).

# view the number of species x stations x occasions
dim(y)
#> [1]   3 145  10
# view red-legged pademelon at the first four stations
y["Thylogale stigmatica", 1:4, ]
#>                          occasion
#> site                      o1 o2 o3 o4 o5 o6 o7 o8 o9 o10
#>   CFRAG_01_WildObsID_0001  1  1  1  1  1  1  1  1  1   1
#>   CFRAG_02_WildObsID_0001  0  1  0  1  1  1  0  0  0   0
#>   CFRAG_03_WildObsID_0001  1  1  1  1  0  1  1  1  1   1
#>   CFRAG_04_WildObsID_0001  0  0  1  1  1  1  0  0  0   1
```

The messages say what was set aside before counting:

- **Obscured records.** Threatened-species records in public data carry
  no `deploymentID`, so they cannot be placed on a site. Detection
  histories built from public data may under-count those species where
  they are listed.
- **Records that are not of animals**, such as blanks, people and
  vehicles.

A detection on a day its camera was not running stops the function,
because it means the deployment dates and the observations disagree.

### Counts for abundance models

In the
[`detection_matrix()`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md)
function, you can include the parameter `type = "abundance"`, to produce
a count history matrix for modelling abundance, where the `count` field
from the observations table is used to populate the matrix. Counts are
combined in two steps:

1.  Within each event (`eventID`, a 30-minute window at one camera), the
    *largest* `count` is the number of animals seen. A single pig that
    triggers a camera three times in ten minutes is counted as one
    individual.
2.  Events in an occasion are then combined by specifying a function in
    the `individuals` parameter. `"max"` takes the most animals seen at
    one occasion, which is the conservative option. `"sum"` adds the
    events, which gives more variation but could count one animal twice
    if it passes two cameras at a station, violating the assumptions of
    N-mixture models.

``` r

# create an abundance matrix for the most feral pigs seen together per station and occasion
n <- detection_matrix(obs, eff, "Sus scrofa", type = "abundance", individuals = "max")
#> detection_matrix(): set aside 452 obscured observation(s), which carry no camera.
#> Detection histories from public data under-count those species where they are listed.
#> detection_matrix(): left out 157 detection(s) after the first 70 days (dur).
# view the first 4 sites
n["Sus scrofa", 1:4, ]
#>                          occasion
#> site                      o1 o2 o3 o4 o5 o6 o7 o8 o9 o10
#>   CFRAG_01_WildObsID_0001  0  0  0  0  0  0  0  0  0   0
#>   CFRAG_02_WildObsID_0001  0  0  0  0  0  0  0  0  0   0
#>   CFRAG_03_WildObsID_0001  0  1  0  0  0  0  0  0  0   0
#>   CFRAG_04_WildObsID_0001  0  1  0  0  1  0  0  0  0   0

# compare with the sum number of pigs detected per station and occasion
s <- detection_matrix(obs, eff, "Sus scrofa", type = "abundance", individuals = "sum")
#> detection_matrix(): set aside 452 obscured observation(s), which carry no camera.
#> Detection histories from public data under-count those species where they are listed.
#> detection_matrix(): left out 157 detection(s) after the first 70 days (dur).
# view the first 4 sites
s["Sus scrofa", 1:4, ]
#>                          occasion
#> site                      o1 o2 o3 o4 o5 o6 o7 o8 o9 o10
#>   CFRAG_01_WildObsID_0001  0  0  0  0  0  0  0  0  0   0
#>   CFRAG_02_WildObsID_0001  0  0  0  0  0  0  0  0  0   0
#>   CFRAG_03_WildObsID_0001  0  2  0  0  0  0  0  0  0   0
#>   CFRAG_04_WildObsID_0001  0  1  0  0  1  0  0  0  0   0
```

Note, the default behavior of
[`detection_matrix()`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md)
is to generate a detection history matrix, (*i.e.*,
`type = "occupancy"`) not a count history matrix. Make sure to include
the parameter `type = "abundance"` to generate a matrix for modelling
abundance.

## Detection that depends on where the camera points

Cameras on roads and trails often detect animals at different rates from
cameras off them. `split_by` splits the effort by a deployment column,
so the share of each occasion’s camera-days on trail can be used as an
observation covariate. In this survey, `featureType` is empty for
cameras placed off trail.

``` r

# create a new two-level trail status: any road or trail, or none
deps$trailStatus <- ifelse(is.na(deps$featureType), "offTrail", "onTrail")
# view the new column
table(deps$trailStatus)
#> 
#> offTrail  onTrail 
#>      136      139

# recreate the same effort matrix, but now split by trail status
eff_trail <- effort_matrix(deps, site_col = "locationID", w = 7, dur = 70,
                          # explicitly define which column is used to split the effort matrix 
                           split_by = "trailStatus", time_zone = dp$temporal$timeZone)
# the third dimension now holds the on vs off trail levels, not the cameras
dimnames(eff_trail$effort)[[3]]
#> [1] "offTrail" "onTrail"
```

[`model_matrix_input()`](https://wildobs.github.io/WildObsR/reference/model_matrix_input.md)
turns the split into observation covariates: `effort` (camera-days) and
one share per level, here `trailStatus_offTrail` and
`trailStatus_onTrail`. This survey paired one camera on a trail with one
off it at most stations, so the share on trail is 0.5 while both run,
and changes when one of them stops. The shares of a station’s levels add
up to 1, so a model formula uses one of them, for example
`~ effort + trailStatus_onTrail`. It also adds `total_effort`, each
station’s total camera-days over the modelled occasions and splits, as a
site covariate.

``` r

# create the detections for this effort, as plain arrays
inp <- model_matrix_input(detection_matrix(obs, eff_trail, species), eff_trail, output = "array")
#> detection_matrix(): set aside 452 obscured observation(s), which carry no camera.
#> Detection histories from public data under-count those species where they are listed.
#> detection_matrix(): left out 671 detection(s) after the first 70 days (dur).
# view the observation covariates it adds
names(inp$obs_covs)
#> [1] "effort"               "trailStatus_offTrail" "trailStatus_onTrail"
# view the share of camera-days on trail at the first four stations
round(inp$obs_covs$trailStatus_onTrail[1:4, 1:5], 2)
#>                          occasion
#> site                       o1  o2   o3  o4  o5
#>   CFRAG_01_WildObsID_0001 0.5 0.5 0.50 0.5 0.5
#>   CFRAG_02_WildObsID_0001 0.5 0.5 0.50 0.5 0.5
#>   CFRAG_03_WildObsID_0001 0.5 0.5 0.50 0.5 0.5
#>   CFRAG_04_WildObsID_0001 0.5 0.5 0.27 0.0 0.0
# do the same for off trail
round(inp$obs_covs$trailStatus_offTrail[1:4, 1:5], 2)
#>                          occasion
#> site                       o1  o2   o3  o4  o5
#>   CFRAG_01_WildObsID_0001 0.5 0.5 0.50 0.5 0.5
#>   CFRAG_02_WildObsID_0001 0.5 0.5 0.50 0.5 0.5
#>   CFRAG_03_WildObsID_0001 0.5 0.5 0.50 0.5 0.5
#>   CFRAG_04_WildObsID_0001 0.5 0.5 0.73 1.0 1.0
```

`split_by` takes one column. To split by two, combine them into one
first, for example `paste(deps$baitUse, deps$trailStatus, sep = "_")`.

## Grid cells as sites

When cameras were not placed at fixed points, or when projects with
different designs are pooled, a regular grid makes a common site.
[`spatial_hexagon_generator()`](https://wildobs.github.io/WildObsR/reference/spatial_hexagon_generator.md)
assigns every deployment to a hexagonal cell. **The scale is a cell area
in square metres**, so `3e6` gives 3 km² cells; `3000` would give cells
about 60 m across. The
[`spatial_hexagon_generator()`](https://wildobs.github.io/WildObsR/reference/spatial_hexagon_generator.md)
function can generate many different hexagonal cells with one call by
specifying multiple areas in the `scales` parameter.

``` r

# assign every deployment to both 3 km2 and 6 km2 hexagonal cells
deps_cellID <- spatial_hexagon_generator(deps, scales = c(3e6, 6e6))
# ensure its still one row per deployment, but now with two different cellIDs
c(deployments = nrow(deps_cellID), 
  # expect to see fewer cells as area increases 
  cells_3km2 = length(unique(deps_cellID$cellID_3km2)),
  cells_6km2 = length(unique(deps_cellID$cellID_6km2)))
#> deployments  cells_3km2  cells_6km2 
#>         275          96          67
```

Effort and detections are built using exactly the same function as
before, with cellID specified as the site. The observations need no
changes:
[`detection_matrix()`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md)
finds each record’s cell through its `deploymentID` stored in the
`deployments` table of the output from
[`effort_matrix()`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md).

``` r

# create the sampling matrix filled with camera-days per cell and occasion, split by trail status
eff_cell <- effort_matrix(deps_cellID, site_col = "cellID_3km2", w = 7, dur = 70,
                          split_by = "trailStatus", time_zone = dp$temporal$timeZone)

# create the matrix filled with detections (0/1) per cell and occasion for all species
y_cell <- detection_matrix(obs, eff_cell, species)
#> detection_matrix(): set aside 452 obscured observation(s), which carry no camera.
#> Detection histories from public data under-count those species where they are listed.
#> detection_matrix(): left out 684 detection(s) after the first 70 days (dur).
# inspect number of species, rows, and columns 
dim(y_cell)
#> [1]  3 96 10
```

Site-level covariates need one value per cell, which does not vary
through the sampling duration. The function
[`resample_covariates_and_observations()`](https://wildobs.github.io/WildObsR/reference/resample_covariates_and_observations.md)
uses the cellIDs generated by
[`spatial_hexagon_generator()`](https://wildobs.github.io/WildObsR/reference/spatial_hexagon_generator.md)
to spatially resample both the observations and covariates. Since
[`detection_matrix()`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md)
matches observations via `deploymentID` to fill a detection history
matrix, we only need resampled covariates to match each cell in the
matrix. Use
[`resample_covariates_and_observations()`](https://wildobs.github.io/WildObsR/reference/resample_covariates_and_observations.md)
without including the observations (*i.e.*, `obs = NULL`) to quickly
average each cell’s numeric covariates, weighting every deployment by
its camera-days, and take the modal value covering the most camera-days
for the columns specified in `mode_cols_covs`. It takes about a second.

``` r

# resample covariates to produce one row of covariates per cellID
cell <- resample_covariates_and_observations(deps_cellID, mode_cols_covs = "trailStatus")
#> resample_covariates_and_observations(): featureType (e.g. "roadDirt - roadPaved - trailHiking") kept as text, because some cells hold more than one value.
#> For one value per cell, add the column to mode_cols_covs; to model detection by it, use effort_matrix(split_by = ...).
# pull out the resampled covariates from the returned list 
cell_covs <- cell$spatially_resampled_covariates$cellID_3km2
# inspect a few of them for the first three cells
head(cell_covs[, c("cellEffort", "Avg_altitude_point", "mode_trailStatus")], 3)
#>   cellEffort Avg_altitude_point mode_trailStatus
#> 1   284.3457           800.6066          onTrail
#> 2   542.3550           771.5814         offTrail
#> 3   147.6362           749.8398          onTrail
```

`cellEffort` is every day each camera in the cell ran, while
`total_effort` from
[`model_matrix_input()`](https://wildobs.github.io/WildObsR/reference/model_matrix_input.md)
counts only the modelled occasions, so the two differ when `dur` in
[`effort_matrix()`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md)
cuts sampling short.

Do not pass the resampled covariates to
[`effort_matrix()`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md).
When multiple cameras are grouped into a single cell, each individual
camera’s start and end dates have been collapsed to an overall cell
start and end date, which reduces resolution for sampling effort within
the cell. If you supply resampled covariates to the
[`effort_matrix()`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md)
function, it will stop and say which table to use instead.

## The legacy route, for comparison

The first matrix function created in WildObsR was
[`matrix_generator()`](https://wildobs.github.io/WildObsR/reference/matrix_generator.md),
which builds detection histories from resampled observations. Comparing
the two on this survey confirms that both routes give the same detection
histories, which is a useful check:

``` r

# subset observations for pademelon records that can be placed on a camera
pademelon <- obs[obs$deploymentID %in% deps_cellID$deploymentID &
                   obs$scientificName == "Thylogale stigmatica", ]
# resampled to cells and days, as matrix_generator() needs
resamp <- suppressWarnings(
  # Note, resampling the observations is slow, takes ~45 seconds. 
  resample_covariates_and_observations(deps_cellID, pademelon, individuals = "max"))
#> resample_covariates_and_observations(): featureType (e.g. "roadDirt - roadPaved - trailHiking") kept as text, because some cells hold more than one value.
#> For one value per cell, add the column to mode_cols_covs; to model detection by it, use effort_matrix(split_by = ...).

# create the legacy detection history, over the same 70 days in 7-day occasions
legacy <- suppressWarnings(matrix_generator(
  # specify the resampled observations and covariates 
  obs = resamp$spatially_resampled_observations$cellID_3km2,
  covs = resamp$spatially_resampled_covariates$cellID_3km2,
  # assign the same sampling duration and occasions
  dur = 70, w = 7, 
  # select a single site-level covariate and no observation covariates 
  site_covs = "Avg_altitude_point", obs_covs = character(0),
  # include all landscapes and specify only one species 
  all_locationNames = TRUE, scientificNames = "Thylogale stigmatica",
  # make a matrix for occupancy models
  type = "occupancy"))
#> 9 sampling unit(s) have no observations and enter the matrices as zeros: Daintree_NP_100_cellID_3km2_2022_WildObsID_0001, Daintree_NP_122_cellID_3km2_2022_WildObsID_0001, Daintree_NP_44_cellID_3km2_2022_WildObsID_0001, Daintree_NP_87_cellID_3km2_2022_WildObsID_0001, Daintree_NP_90_cellID_3km2_2022_WildObsID_0001
# pull out the detection matrix for a closer look
y_legacy <- legacy$Thylogale_stigmatica$detection_matrix

# pull out the new history, in the same row order
y_new <- y_cell["Thylogale stigmatica", rownames(y_legacy), ]
# are the detections the same, and occasions the same without effort
identical(unname(y_legacy == 1), unname(y_new == 1))
#> [1] TRUE
```

The difference is in effort.
[`matrix_generator()`](https://wildobs.github.io/WildObsR/reference/matrix_generator.md)
has no camera-days: it treats a cell as sampled continuously from its
first deployment start to its last end, including gaps between
deployments, and its `numberDeploymentsActiveAtDate` counts only the
cameras that recorded something that day.
[`?matrix_generator`](https://wildobs.github.io/WildObsR/reference/matrix_generator.md)
lists these limits. The legacy function may be deprecated in a future
release.

## Choices to make

- **Occasion length (`w`).** Longer occasions raise detection
  probability per occasion and give fewer, less autocorrelated
  occasions. A week is common for medium-sized mammals.
- **Survey length (`dur`).** Occupancy models assume the population is
  closed: the population does not increase or decrease at a site during
  the survey. Cut each site’s sampling to a period where that is
  plausible, based on the biology of your study species.
- **Site size.** A station suits cameras placed at fixed points with
  regular spacing between points; a grid cell suits cameras with unequal
  spacing, and species whose home range spans several cameras. The cell
  area sets what “occupied” means.
- **Time zone.** Days run in the package’s time zone
  (`dp$temporal$timeZone`). When pooled cameras span time zones with
  different clocks, both functions use UTC days instead, with a message,
  so effort and detections still share the same day boundaries.

## Next steps

*Fit single-season models* fits these matrices in unmarked, spOccupancy,
spAbundance and NIMBLE, including how site and observation covariates
enter each model. *Fit multi-season models* follows sites across seasons
with `align = "calendar"`.
