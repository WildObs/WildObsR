---
name: wildobsr-data
description: >
  How to understand and use WildObs camera-trap data downloaded with the WildObsR R package.
  Use when working with data packages from wildobs_dp_download() or project IDs from
  wildobs_mongo_query(); when reading the deployments, observations, media, or covariates
  tables or the package metadata (contributors, sources, temporal, taxonomic, spatial,
  WildObsMetadata); when joining those tables or counting detections; when interpreting
  deploymentID, observationID, eventID, mediaID, locationID, locationName, deploymentGroups,
  multiSeason_deploymentGroup, observationType, deltaTime_event, or covariates at the
  _point/_1km2/_3km2/_5km2/_10km2 buffer scales; when using extract_metadata(); or when asked
  what WildObs data can and cannot support (occupancy, abundance, activity, REM, distance
  sampling). Covers Camtrap DP and the WildObs extensions to it.
---

# WildObs data in WildObsR

WildObs is Australia's national camera-trap database. WildObsR downloads it as
**[Camtrap DP](https://camtrap-dp.tdwg.org/) data packages**, one per project, built on
[Frictionless Data Packages](https://specs.frictionlessdata.io/data-package/). Every
standard Camtrap DP field is present with its standard meaning; WildObs only **adds**
fields, a `covariates` table, and a few metadata blocks on top.

Field-by-field tables: [`reference/tables.md`](reference/tables.md) (the four data tables) and
[`reference/metadata.md`](reference/metadata.md) (package-level metadata).

## Getting data

```r
library(WildObsR)
api_key <- Sys.getenv("WILDOBSR_API_KEY")   # personal key from dashboard.wildobs.org.au

# 1. find projects: every filter is optional, and filters combine as an intersection
ids <- wildobs_mongo_query(
  api_key  = api_key,
  spatial  = list(xmin = 145, xmax = 154, ymin = -29, ymax = -10),          # bounding box, WGS84
  temporal = list(minDate = as.Date("2020-01-01"), maxDate = as.Date("2024-12-31")),
  taxonomic = c("Phascolarctos cinereus"),                                    # binomials
  tabularSharingPreference = c("open", "partial")
)

# 2. download them: a named list of data packages, one per project ID
dp_list <- wildobs_dp_download(api_key = api_key, project_ids = ids,
                               media = FALSE, metadata_only = FALSE)

# 3. use them
dp   <- dp_list[[1]]
deps <- frictionless::read_resource(dp, "deployments")     # or dp$data$deployments    
obs  <- frictionless::read_resource(dp, "observations") 
covs <- frictionless::read_resource(dp, "covaraites")  
meta <- extract_metadata(dp_list, c("contributors", "temporal", "taxonomic"))
```

- `wildobs_mongo_query()` returns a character vector of project IDs (an empty vector,
  `character(0)`, with a warning, if nothing matches). The temporal filter matches a project if **any of its
  deploymentGroups** overlaps the window, not just its overall date range.
- `media = TRUE` adds the media table. It is by far the largest table (often hundreds of
  thousands of rows per project) and is downloaded in batches, so leave it off unless you
  need image-level data.
- `metadata_only = TRUE` skips the tables for a fast look at what exists.
- Each package is a list with class `camtrapdp`. The tables are in `dp$data` and also as
  Frictionless resources in `dp$resources`, each with a schema describing every field.
  `frictionless::write_package(dp, "folder")` saves it to disk as CSVs plus `datapackage.json`.

## What the public API gives you

An API key reaches the **public** WildObs database. It differs from the full database in
three ways: data-sharing agreements decide what each project releases, threatened-species
records are obscured, and media file names are withheld.

### Data-sharing agreements

Each project carries a data-sharing preference in `WildObsMetadata$tabularSharingPreference`:

| Preference | What you get |
|---|---|
| `open` | Metadata **and** the data tables. |
| `partial` | **Metadata only.** The tables are withheld, usually under an embargo that will expire. |
| `closed` | Nothing. Not returned through the public API. |

A project's embargo is already reflected in its preference: an embargoed project reads
`partial`, and becomes `open` once the embargo (`WildObsMetadata$embargoPeriodMonths` from
`created`) has passed and the database is next refreshed. Open data also needs a RAiD in
`bibliographicCitation`; a project without one downloads as metadata only.

`wildobs_dp_download()` returns `partial` projects as metadata-only packages with no
tables, rather than failing. Check `length(dp$resources)` or `nrow(dp$data$deployments)`
before analysis.

`wildobs_mongo_query()` needs at least one filter: called with none, it returns
`character(0)`. To list every project you can see, pass a box over all of Australia,
`spatial = list(xmin = 110, xmax = 160, ymin = -45, ymax = -9)`.

### Obscured threatened-species records

Observations of species listed as threatened under the EPBC Act are obscured **in the
states where the species is listed** (a species listed only in South Australia is obscured
only at South Australian cameras). On those rows, `deploymentID`, `observationID`,
`eventID` and `mediaID` are all replaced by one placeholder naming the listing category:

| Placeholder | Category |
|---|---|
| `obscured_for_vulnerable_species` | Vulnerable |
| `obscured_for_endangered_species` | Endangered |
| `obscured_for_critically_endangered_species` | Critically endangered |

The rest of the row is kept: `scientificName`, `eventStart`/`eventEnd`, `count`,
`observationType`, the classification fields and `projectName`. So the record still says
which species was seen, when and how many, but carries no camera identifier. In practice:

- A join to `deployments`, `covariates` or `media` silently drops obscured rows.
- `resample_covariates_and_observations()` stops with "mis-matched deploymentID values"
  until they are removed. After removal, a camera whose only records were obscured has no
  observations left, which can make the same function stop with "cellID values do not
  perfectly match", which is more likely in projects that recorded no `blank` observations.
- Detection histories from public data **under-count listed species in their listed
  states**. Do not estimate occupancy or abundance for a threatened species from public
  data without accounting for this.
- `observationID` is not unique across obscured rows, since they share a placeholder.

Count them, then set them aside before any spatial analysis:

```r
obscured <- grepl("^obscured_for_", obs$deploymentID)
table(obs$deploymentID[obscured])   # how many, by listing category
obs <- obs[!obscured, ]
```

Species listed nowhere, or listed only in other states, are not obscured.
`WildObsR::species_traits` carries each species' category (`epbc_category`) and listing
states (`epbc_location`).

### Media file names

In the media table, `fileName` is withheld (all `NA`) for public users.

## How the tables fit together

```
package (one project)          id  ──►  projectName on every table
 └─ deployments    one camera, one place, one continuous period       key: deploymentID
     ├─ covariates      exactly one row per deployment                 join: deploymentID (1:1)
     ├─ observations    detection events at that deployment            join: deploymentID (1:many)
     │    └─ media      the images in each observation                 join: observationID (1:many)
     └─ media           every image from that deployment               join: deploymentID (1:many)
```

- `deploymentID`, `observationID` and `mediaID` are unique primary keys, except on obscured
  observations, which share a placeholder and link to nothing (see above).
- `projectName` on every table equals the package `id`, so tables from several packages can be
  stacked with `dplyr::bind_rows()` and still be told apart.
- `observations$mediaID` points at **one representative image** per observation, not every
  image; use `media$observationID` to get all of them.

### Spatial and survey structure

| Field | Grain |
|---|---|
| `locationID` | One physical camera station. A station can host several deployments over time. |
| `locationName` | A named landscape, usually a protected area. Holds many stations. |
| `deploymentGroups` | One **survey**: a spatio-temporal sampling unit within a single `locationName`, lasting at most **100 days**. |
| `multiSeason_deploymentGroup` | The parent of `deploymentGroups` for long-term sampling. Continuous sampling is split into a new multi-season group wherever there is a **30-day** gap. |

`survey_and_deployment_generator()`, `spatial_hexagon_generator()`,
`resample_covariates_and_observations()` and `matrix_generator()` build on these to produce
detection histories for `unmarked` and similar packages.

## Observations: read this before counting anything

- **One observation = one taxon at one deployment within a 5-minute window** (`observationID`).
- **One event = a 30-minute window** (`eventID`), grouping 1–6 observations. Use
  `observationID` for independent detections, or `eventID` for coarser encounters.
  `reclassify_eventID()` rebuilds `eventID` at a different threshold.
- `deltaTime_event` is the seconds since the previous event at that deployment, for your own
  independence filtering.
- **Filter `observationType == "animal"`.** About half of all observations are `blank`
  (~52%) and about a third `animal` (~32%); the rest are `vehicle`, `unknown` and `human`. A
  raw row count is not an animal count. Non-animal rows still fill `scientificName`, with
  placeholders such as `Blank`, `Homo sapiens-vehicle`, `unidentified` or `Ghost`.
- `count` is the number of individuals. It is `NA` on blanks and unknowns, and on about 13%
  of animal records.
- `observationLevel` is always `event`.
- Downloaded observations also carry `taxonID`, `taxonRank` and `vernacularNamesEnglish`,
  joined from the package's taxonomic metadata, and empty on non-animal rows. Check
  `taxonRank`: about 77% of animal records are identified to species, and the rest only to
  genus, family, order, class or phylum.
- `classificationMethod` is `human` (about 88% of animal records) or `machine`.

### What the data can and cannot support

| Analysis | Supported? | Why |
|---|---|---|
| Occupancy, relative abundance / detection rates, activity patterns, species richness | **Yes** | Deployment effort, timestamps, taxa and counts are complete. |
| N-mixture / abundance from counts | Check first | `count` is recorded on animal observations, but only some projects recorded group sizes (`WildObsMetadata$groupSizes`). |
| Individual ID / SCR, sex or age structure | **No** | `individualID`, `sex`, `lifeStage`, `behavior` are essentially empty. |
| REM, camera-trap distance sampling | **No** | `detectionDistance`, `individualPositionRadius/Angle`, `individualSpeed` are essentially empty. |

## Time

- `deploymentStart`, `deploymentEnd`, `eventStart`, `eventEnd`, `timestamp` arrive as
  `POSIXct` in the **project's local time zone**, which is in the package metadata at
  `dp$temporal$timeZone` (for example `Australia/Brisbane`). Convert explicitly before
  combining projects from different time zones.
- `dp$temporal` holds the package's overall `start` and `end`, its `timeZone`, and one
  `{start, end}` block per deploymentGroup. `extract_metadata(dp, "temporal")` turns that into a
  table with one row per deploymentGroup plus `packageStart`/`packageEnd` columns.
- `timestampIssues = TRUE` flags the few deployments whose clock was known to be wrong.

## Covariates

One row per deployment of environmental predictors, pre-extracted around each camera at five
buffer sizes. **The suffix names the buffer's area, not its radius:**

| Suffix | Radius | Area |
|---|---|---|
| `_point` | 1 m | the pixel under the camera |
| `_1km2` | 564.2 m | ~1 km² |
| `_3km2` | 977.2 m | ~3 km² |
| `_5km2` | 1,261.6 m | ~5 km² |
| `_10km2` | 1,784 m | ~10 km² |

Families include forest integrity (`FLII_`), human footprint, elevation, ecoregion
intactness, monthly rainfall and temperature, night-time lights, human population density,
protected-area cover, habitat condition (`HCAS_static_`), `NDVI_`, terrain ruggedness,
standardised precipitation index, `HIF_`/`EII_`, fire history (`fire_events_count_`,
`days_since_recent_fire_`, 2019/20 `GEEBAM_fire_severity_`), plus IBRA bioregion/subregion
and Olson ecoregion names. Coverage gaps: `FLII_*` is `NA` for about a third of deployments
and `days_since_recent_fire_*` for ~46–64%, depending on buffer size; most other covariates
are complete.

Each covariate's schema field (in `dp$resources`, covariates resource) has a `custom` block
with the **source citation** (`doi`, `url`, `citation`) and native `resolution` of the
spatial product. Cite those sources when you use the covariates. Read the buffer size from
the field-name suffix, not from `custom$spatial$buffer_m`, which is not reliable for every field.
See [`reference/tables.md`](reference/tables.md) for ranges and meanings.

## Getting the image files

`wildobs_media_download(media, out_dir)` fetches the files listed in a media table and
returns the table with `localPath`, `downloadStatus` (`downloaded`, `copied`,
`already_exists`, `skipped`, `failed`) and `downloadNote` columns. Files are saved as
`out_dir/<projectName>/<deploymentID>/<mediaID>.<ext>`, and re-running it only fetches
what is missing.

Through the public API, `filePath` takes one of two forms:

| `filePath` | Share of public media | What happens |
|---|---|---|
| `https://data.wildobs.org.au/...` | ~3% | Downloads. These are exactly the `filePublic = TRUE` rows. |
| `not_publicly_accessible` | ~97% | The image exists but is not shared publicly. Skipped. |

So filter to public files first: `media[media$filePublic, ]`. Start with a few rows
(`head(..., 20)`) to check the result before downloading thousands. Users with direct
database access see each file's original location instead (for example a Wildlife Insights
`gs://` bucket, which needs `gcs_token`); see `?wildobs_media_download`.

## Exporting to standard Camtrap DP

`as_camtrapdp(dp)` converts a WildObs package to canonical Camtrap DP (1.0.2 by default,
or `version = "1.0.1"`) for tools that expect the standard exactly, such as the
`camtrapdp` R package (>= 0.5.0) and its `write_dwc()` GBIF export, which fails on an
unconverted WildObs package. It returns `list(package, camtrapdp, report, validation)`.

- It needs all three tables, so download with `media = TRUE`. `partial` projects have no
  tables and can't be converted.
- It removes every WildObs addition: `WildObsMetadata`, `versionControlWildObs`,
  `project$DPID`, contributors' `ROR`, the per-deploymentGroup `temporal` blocks and
  `timeZone`, and the columns `multiSeason_deploymentGroup`, `deltaTime_event`,
  `media$observationID`, `TIR` and `projectName`. `covariates` is dropped unless
  `keep_covariates = TRUE`.
- **Lost in conversion:** the media-to-observation link (`media$observationID`), since
  Camtrap DP has no field for it. A warning says so.
- A few values change so the result validates: English names move to
  `vernacularNames$eng`; ranks such as `subclass` round up to `class`; RAiD identifiers
  become `Handle`; and media rows whose `filePath` is a path on a contributor's computer
  are removed. That can be a large share of some projects' media; the report counts it.
- `out$camtrapdp` is the result already read by `camtrapdp::read_camtrapdp()`, so pass it
  straight to camtrapdp functions: `camtrapdp::write_dwc(out$camtrapdp, dir)`. `out$package`
  is the frictionless package, for saving with `frictionless::write_package(out$package,
  dir)`; camtrapdp prints it as having 0 tables because its tables sit in `resources`.

## Practical cautions

- **Sensitive species.** Only EPBC-listed species in their listed states are obscured (see
  "Obscured threatened-species records"). Deployment coordinates are exact, and other
  species of conservation concern (state-listed, or not on the EPBC list) are not obscured.
  Do not publish precise locations of sensitive taxa; `WildObsR::species_traits` carries
  EPBC status.
- **Free-text fields need cleaning** before grouping: `cameraModel` (many spellings of one
  model), `habitat`, `deploymentTags` and `observationTags` (`key: value | key: value` pairs
  whose keys differ between projects).
- **Media files are mostly private.** The media table describes images; it does not contain
  them. See "Getting the image files" below.
- **Projects differ in effort and design.** Check `project$samplingDesign`, `baitUse`, and
  `WildObsMetadata` (`deploymentClusters`, `thinnedMedia`, `groupSizes`) before pooling.
- **Cite the data.** Each package's `bibliographicCitation` is the citation to use, and
  `licenses` gives the terms.

## Scale through the public API

Measured through the public API on 2026-10-05 (`dev/public_skill_census.R` in the WildObsR
repository); percentages elsewhere in this skill come from the same census. These numbers
grow with every database release:

- **Projects:** 44 visible: 18 `open` with tables, 26 `partial` (metadata only).
- **Tables of the open projects:** 4,088 deployments, ~730,000 observations and ~6.8
  million media records.
- **Taxa:** ~380 recorded in the open projects' observations, and ~620 listed across all 44
  projects' metadata.
- **Time span:** surveys from 2010 to 2024, in six Australian time zones.
- **Obscured:** ~3,700 observations (about 1.6% of animal records), in 14 projects.
