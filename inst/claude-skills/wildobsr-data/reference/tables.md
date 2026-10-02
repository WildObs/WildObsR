# The four data tables

Field reference for `dp$data$deployments`, `$observations`, `$media` and `$covariates` as
returned by `wildobs_dp_download()`. "Filled" is the approximate share of rows with a value
across the whole database (as of WildObsR 0.3.0); individual projects vary widely. Fields
marked **WildObs** are additions to Camtrap DP. Every table also has `projectName`, the
package `id`.

The schema for each table, with full descriptions, types and allowed values, travels with the
data: `dp$resources[[i]]$schema$fields`.

## `deployments` — one camera at one place for one continuous period

| Field | R type | Filled | Meaning |
|---|---|---|---|
| `deploymentID` | character | 100% | Primary key. |
| `locationID` | character | 100% | Camera station. A station can host several deployments over time. |
| `locationName` | character | 100% | Named landscape, usually a protected area. |
| `latitude`, `longitude` | numeric | 100% | WGS84 decimal degrees. |
| `coordinateUncertainty` | integer | 100% | Metres. |
| `deploymentStart`, `deploymentEnd` | POSIXct | 100% | In the project's local time zone (`dp$temporal$timeZone`). |
| `setupBy` | character | ~30% | Who deployed the camera. |
| `cameraID` | character | ~59% | Camera serial or ID. |
| `cameraModel` | character | ~58% | Free text, **not standardised** — clean before grouping. |
| `cameraDelay` | integer | ~51% | Seconds between triggers. |
| `cameraHeight` | numeric | ~66% | Metres above ground. |
| `cameraDepth` | numeric | 0% | Underwater only; never used. |
| `cameraTilt` | integer | ~59% | Degrees; −90 down, 0 horizontal, 90 up. |
| `cameraHeading` | integer | ~11% | Degrees clockwise from north. |
| `detectionDistance` | numeric | ~1% | Metres. Too sparse for REM or distance sampling. |
| `timestampIssues` | logical | 100% | `TRUE` if media timestamps are known to be wrong. Rare. |
| `baitUse` | logical | 100% | Bait or lure used. Roughly half of deployments. |
| `featureType` | character | ~20% | Targeted feature: `roadDirt`, `trailGame`, `waterSource`, `trailHiking`, `burrow`, `roadPaved`, `nestSite`, `roadUnderpass`; empty otherwise. |
| `habitat` | character | ~42% | Free-text habitat description. |
| `deploymentGroups` | character | 100% | Survey: one `locationName`, at most 100 days. |
| `multiSeason_deploymentGroup` | character | 100% | **WildObs.** Parent of `deploymentGroups`; split at a 30-day sampling gap. |
| `deploymentTags` | character | 100% | `key: value \| key: value` pairs (e.g. predator management, lure type). Keys differ between projects. |
| `deploymentComments` | character | ~35% | Free-text notes. |

## `observations` — one taxon at one deployment in a 5-minute window

| Field | R type | Filled | Meaning |
|---|---|---|---|
| `observationID` | character | 100% | Primary key; the **5-minute** window. |
| `deploymentID` | character | 100% | Links to `deployments`. |
| `mediaID` | character | 100% | One **representative** image for this observation. |
| `eventID` | character | 100% | The **30-minute** window; groups 1–6 observations. |
| `eventStart`, `eventEnd` | POSIXct | 100% | First and last image of the observation, in local time. |
| `deltaTime_event` | integer | ~98% | **WildObs.** Seconds since the previous event at this deployment. |
| `observationLevel` | character | 100% | Always `event`. |
| `observationType` | character | 100% | `blank` (~49%), `animal` (~37%), `unknown` (~10%), `vehicle` (~3%), `human` (~2%). |
| `cameraSetupType` | character | ~0% | `setup` on a handful of rows. |
| `scientificName` | character | 100% | Taxon, at the rank in `taxonRank`. |
| `taxonID` | character | — | Added from metadata: verified taxon URI. |
| `taxonRank` | character | — | Added from metadata: `species`, `genus`, `family`, … |
| `vernacularNamesEnglish` | character | — | **WildObs.** Added from metadata: English common name. |
| `count` | integer | ~86% | Individuals; `NA` on blanks and unknowns. |
| `lifeStage`, `sex`, `behavior` | character | ~0% | Essentially empty. |
| `individualID` | character | 0% | Empty. |
| `individualPositionRadius`, `individualPositionAngle`, `individualSpeed` | numeric | 0% | Empty. |
| `bboxX`, `bboxY`, `bboxWidth`, `bboxHeight` | numeric | ~6% | Bounding box, relative to image size (0–1). Machine classifications only. |
| `classificationMethod` | character | 100% | `human` (~94%) or `machine`. |
| `classifiedBy` | character | ~65% | Person or model. |
| `classificationTimestamp` | POSIXct | ~4% | When classified. |
| `classificationProbability` | numeric | ~10% | 0–1; mostly machine classifications. |
| `observationTags` | character | ~10% | `key: value \| …` tags. |
| `observationComments` | character | ~1% | Free-text notes. |

## `media` — one image or video (only with `media = TRUE`)

| Field | R type | Filled | Meaning |
|---|---|---|---|
| `mediaID` | character | 100% | Primary key. |
| `deploymentID` | character | 100% | Links to `deployments`. |
| `observationID` | character | 100% | **WildObs.** The observation this image belongs to (Camtrap DP has no such link). |
| `captureMethod` | character | ~96% | `activityDetection` (motion-triggered); `timeLapse` has not been observed. |
| `timestamp` | POSIXct | 100% | Capture time, local. |
| `filePath` | character | 100% | Path or URL of the file. |
| `filePublic` | logical | 100% | ~99% `FALSE`: the file itself is not publicly available. |
| `fileName` | character | — | Withheld (`NA`) for public users. |
| `fileMediatype` | character | 100% | `image/jpeg` (~94%), `video/mp4` (rare), or a `…not_provided` placeholder (~5%). |
| `exifData` | character | ~0% | EXIF metadata as text. |
| `favorite` | logical | 100% | Exemplar image flag; rare. |
| `mediaComments` | character | ~3% | Free-text notes. |
| `TIR` | logical | 100% | **WildObs.** In the WildObs Tagged Image Repository (~7%). |

## `covariates` — environmental predictors, one row per deployment (all **WildObs**)

Context columns copied from `deployments`: `deploymentID`, `locationID`, `locationName`,
`latitude`, `longitude`, `deploymentStart`, `deploymentEnd`, `deploymentGroups`.

Each family below comes in five buffer sizes: `_point`, `_1km2`, `_3km2`, `_5km2`, `_10km2`
(the suffix is the buffer **area**; see SKILL.md for radii).

| Family | Range | Meaning |
|---|---|---|
| `FLII_*` | 0–10 | Forest Landscape Integrity Index; higher = more intact. ~22–25% `NA`. |
| `human_footprint_*` | 0–50 | Cumulative human pressure; higher = more pressure. |
| `altitude_*` | m | Elevation (SRTM-derived DEM). |
| `terrain_ruggedness_index_*` | — | Local elevation heterogeneity. |
| `ecoregion_intactness_*` | 0–1 | Habitat extent, quality and fragmentation combined. |
| `mean_monthly_precipitation_*` | mm | ANU Climate 2.0; dates after 2022 use the 2022 value. |
| `mean_monthly_temperature_*` | °C | ANU Climate 2.0; same 2022 carry-forward. |
| `standardized_precipitation_index_*` | about −3.7 to 3.7 | Drought / wet anomaly against the long-term normal. |
| `nighttime_lights_*` | ≥ 0 | VIIRS night-time radiance; an urbanisation proxy. |
| `human_population_density_*` | people/km² | ABS 2023 population grid. |
| `protected_areas_*` | 0–1 | Share of the buffer inside a protected area. |
| `HCAS_static_*` | 0–1 | Habitat Condition Assessment System score. |
| `NDVI_*` | 0–1 | Vegetation greenness. |
| `HIF_*`, `EII_*` | — | Human Influence Factor and Ecosystem Integrity Index. |
| `fire_events_count_*` | ≥ 0 | Number of distinct mapped fires in the buffer. |
| `days_since_recent_fire_*` | days | Days from the deployment back to the most recent mapped fire. ~37–49% `NA`. |
| `GEEBAM_fire_severity_2020_*` | 0–5 | Most common 2019/20 bushfire severity class (0 unburnt … 5 very high/extreme). |

Also:

- `GEEBAM_fire_severity_<0–5>_percent_<1km2|3km2|5km2|10km2>` — percentage of the buffer in
  each 2019/20 severity class (no `_point` variant); the six classes sum to 100 at each scale.
- `IBRAbioRegionName`, `IBRAsubRegionName` — IBRA7 bioregion and subregion.
- `Olson_global_ecoregion` — Olson et al. terrestrial ecoregion (~7% `NA`).

Source citations for every covariate are in each field's `custom$source` in the covariates
schema.
