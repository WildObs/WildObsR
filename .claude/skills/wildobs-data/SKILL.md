---
name: wildobs-data
description: >
  The WildObs camera-trap database (`wildobs_camdb`) data model — collections, fields, join
  keys, controlled vocabularies, and how WildObs extends the Camtrap DP standard. Use when
  querying or reasoning about `deployments`, `observations`, `media`, `covariates`, or
  `metadata`; when writing MongoDB queries or aggregations against wildobs_camdb; when
  joining camera-trap tables or deciding cardinality; when working with camtrap-dp / Camtrap
  DP data packages, deploymentID / observationID / mediaID / eventID / locationID /
  projectName / deploymentGroups; when interpreting detection events, independence windows,
  blank frames, or spatial covariates at 1km2/3km2/5km2/10km2 buffer scales; or when using
  WildObsR functions such as wildobs_mongo_query(), wildobs_dp_download(), or
  matrix_generator() that consume this schema.
---

# WildObs camera-trap database (`wildobs_camdb`)

> ## ⚠️ This documents the PUBLIC mirror
>
> Every count, coverage percentage, and vocabulary here was measured against the **local
> public mirror** of `wildobs_camdb`. **PROD may contain additional fields, additional
> projects, and unobscured or higher-precision values.** Treat coverage figures as a floor,
> not a guarantee: a field that is 0% populated here may be populated in PROD. Re-measure
> before concluding a field is unusable in a PROD context.
>
> **Never print coordinate values** (`latitude`, `longitude`, `spatial` geometry) or records
> for sensitive taxa into logs, reports, or chat. Document the field, not the value.

## Conceptual model

A **project** (`metadata`) is one Camtrap DP data package. It contains many **deployments** —
a single camera at a single location over one continuous time window. Each deployment
produces **media** (images), which are grouped into **observations** (detection events).
Separately, each deployment carries one row of **covariates**: pre-extracted environmental
predictors at five spatial buffer scales.

```
metadata (48 projects)
   │  id ──────────────► projectName  (on every other collection)
   │
   └─ deployments (18,770)
         │  deploymentID ─── 1:1 ──► covariates (18,770)
         │  deploymentID ─── 1:many ─► observations (1,883,177)
         │  deploymentID ─── 1:many ─► media (21,667,502)
         │
         └─ observations ─ observationID ─ 1:many ─► media
                          ─ mediaID ────── 1:1 ───► media (representative file)
```

Collection sizes and what one document represents:

| Collection | Docs | One document = |
|---|---|---|
| `metadata` | 48 | One project / Camtrap DP data package. |
| `deployments` | 18,770 | One camera at one location for one continuous window. |
| `covariates` | 18,770 | Environmental predictors for one deployment (1:1 with it). |
| `observations` | 1,883,177 | One detection event: a taxon at a deployment in a 5-minute window. |
| `media` | 21,667,502 | One media file (nearly always a single image). |
| `versions` | 1 | Database-level version marker (not documented further). |

Temporal extent: **2009-03-11 → 2025-06-27**.

Per-field tables live in `reference/`:
[`metadata.md`](reference/metadata.md) ·
[`deployments.md`](reference/deployments.md) ·
[`observations.md`](reference/observations.md) ·
[`media.md`](reference/media.md) ·
[`covariates.md`](reference/covariates.md)

## Join keys and cardinality

All cardinalities below were verified by distinct-count and `$lookup` orphan checks, not
assumed.

| From | Key | To | Cardinality | Evidence |
|---|---|---|---|---|
| `metadata.id` | string | `*.projectName` | **1:many** | 48 distinct `id`; 48 distinct `projectName` in each of the other four collections. |
| `deployments.deploymentID` | string | `covariates.deploymentID` | **1:1, total** | 18,770 distinct on both sides, 18,770 docs each, **0 orphans** via `$lookup`. |
| `deployments.deploymentID` | string | `observations.deploymentID` | **1:many** | 18,770 distinct in observations — every deployment has observations. Mean ~100 obs/deployment. |
| `deployments.deploymentID` | string | `media.deploymentID` | **1:many** | 18,770 distinct in media. Mean ~1,154 media/deployment. |
| `observations.observationID` | string | `media.observationID` | **1:many** | 1,883,177 distinct in media = exactly the observation count. Every observation has media; mean ~11.5 media/observation. |
| `observations.mediaID` | string | `media.mediaID` | **1:1 into a subset** | 1,883,177 distinct `mediaID` across 1,883,177 observations — one unique representative file each, out of 21.7M media. |
| `observations.eventID` | string | `observations.observationID` | **1:many** | 1,344,410 events vs 1,883,177 observations; mean 1.40, max 6. |
| `deployments.locationID` | string | `deployments.deploymentID` | **1:many** | 6,219 stations; mean 3.0, max 50 deployments per station. |
| `deployments.locationName` | string | `deployments.locationID` | **1:many** | 129 place names; up to 648 station IDs share one name. |

`deploymentID` is unique in `deployments`, `observationID` is unique in `observations`, and
`mediaID` is unique in `media` — all three are true primary keys.

### The temporal hierarchy — the most common modelling mistake

Two nested windows, easy to confuse:

- **`observationID` = a 5-minute window.** The finer grain, and the primary key.
- **`eventID` = a 30-minute window.** The *coarser* grouping. One event holds 1–6 observations.

Count independent detections with `observationID`. Collapse to coarser encounters with
`eventID`. `deltaTime_event` (seconds between consecutive events) supports independence
filtering.

### Query-performance warning

`media` (21.7M docs) is indexed only on `_id`, `mediaID`, and `{projectName, mediaID, _rowHash}`.
There is **no index on `media.deploymentID` or `media.observationID`**, so grouping or joining
media by either scans the whole collection — over two minutes in testing. Always `$match` on
`projectName`/`mediaID` first, or drive the join from the smaller side.

Indexes elsewhere: `deployments` and `covariates` on `deploymentID` and
`{projectName, deploymentID, _rowHash}`; `observations` on `observationID` and
`{projectName, observationID, _rowHash}`; `metadata` on `id`.

## Collection summaries

**`metadata`** — 25 fields. One Camtrap DP package per project. `id` is the join key. Carries
two WildObs-only blocks: `WildObsMetadata` (governance and data sharing) and
`versionControlWildObs`.

**`deployments`** — 27 fields. Camera placements. All 24 Camtrap DP deployment fields are
implemented as specified. Sparse hardware fields: `cameraDepth` **0%**, `detectionDistance`
**1.2%**, `cameraHeading` 11.2%. `cameraModel` (60.3%) is free text and unnormalised —
`Reconyx`, `Reconyx_HP2x`, `Reconyx-HC600 HYPERFIRE` all coexist.

**`observations`** — 31 fields. Detection events. **48.4% are `blank`**; filter to
`observationType == "animal"` for ecological work. 668 distinct `scientificName`. The entire
individual-level block (`sex`, `behavior`, `individualID`, `individualPosition*`,
`individualSpeed`) is ~0% populated, so distance sampling and REM are not supported here.

**`media`** — 15 fields. Image files. **98.8% have `filePublic = false`.** `fileMediatype` is
dirty: ~5% are placeholders like `image/not_provided`, and casing is inconsistent
(`image/JPG` vs `image/jpg`). Adds two WildObs-only fields: `observationID` and `TIR`.

**`covariates`** — 127 fields. **Entirely WildObs-only; no Camtrap DP equivalent.** One row
per deployment. 18 environmental families × 5 buffer scales, plus 24 GEEBAM fire-severity
class percentages, 3 categorical bioregion fields, and 10 join/context fields.

### Covariate buffer scales

The suffix names the **area** of the buffer, not its radius:

| Suffix | Radius | Area |
|---|---|---|
| `_point` | 1 m | the pixel under the camera |
| `_1km2` | 564.2 m | ~1 km² |
| `_3km2` | 977.2 m | ~3 km² |
| `_5km2` | 1,261.6 m | ~5 km² |
| `_10km2` | 1,784 m | ~10 km² |

Families: `FLII`, `human_footprint`, `altitude`, `ecoregion_intactness`,
`mean_monthly_precipitation`, `mean_monthly_temperature`, `nighttime_lights`,
`human_population_density`, `protected_areas`, `HCAS_static`, `NDVI`,
`terrain_ruggedness_index`, `standardized_precipitation_index`, `HIF`, `EII`,
`GEEBAM_fire_severity_2020`, `days_since_recent_fire`, `fire_events_count`.

Most are 100% populated. Exceptions: `days_since_recent_fire_*` 52–62%, `FLII_*` 75–78%,
`Olson_global_ecoregion` 92.5%.

## Camtrap DP vs WildObs — three-way diff

Checked against the Camtrap DP specification at <https://camtrap-dp.tdwg.org/> (fetched, not
recalled). Packages here declare profile `1.0.1` (11) or `1.0.2` (37).

### 1. Camtrap DP fields implemented as specified

**All of them.** Every field the standard defines for its three tables is present in WildObs
with the same name and compatible type:

- **deployments** — all 24: `deploymentID`, `locationID`, `locationName`, `latitude`,
  `longitude`, `coordinateUncertainty`, `deploymentStart`, `deploymentEnd`, `setupBy`,
  `cameraID`, `cameraModel`, `cameraDelay`, `cameraHeight`, `cameraDepth`, `cameraTilt`,
  `cameraHeading`, `detectionDistance`, `timestampIssues`, `baitUse`, `featureType`,
  `habitat`, `deploymentGroups`, `deploymentTags`, `deploymentComments`.
- **media** — all 11: `mediaID`, `deploymentID`, `captureMethod`, `timestamp`, `filePath`,
  `filePublic`, `fileName`, `fileMediatype`, `exifData`, `favorite`, `mediaComments`.
- **observations** — all 28: `observationID`, `deploymentID`, `mediaID`, `eventID`,
  `eventStart`, `eventEnd`, `observationLevel`, `observationType`, `cameraSetupType`,
  `scientificName`, `count`, `lifeStage`, `sex`, `behavior`, `individualID`,
  `individualPositionRadius`, `individualPositionAngle`, `individualSpeed`, `bboxX`, `bboxY`,
  `bboxWidth`, `bboxHeight`, `classificationMethod`, `classifiedBy`,
  `classificationTimestamp`, `classificationProbability`, `observationTags`,
  `observationComments`.
- **package metadata** — all 22 top-level properties, and all 9 `project` properties.

**Nothing from the standard is omitted or renamed.** WildObs is a strict superset by field name.

### 2. Camtrap DP fields redefined in meaning or type

These carry the standard's name but not the standard's semantics — the important column.

| Field | Camtrap DP says | WildObs does |
|---|---|---|
| `metadata.sources` | An **array** of source objects | Stores a single **object**. Type change. |
| `metadata.version` | The **package** version | Holds the **Camtrap DP standard** version (`1.0.1` / `1.0.2`), mirroring `profile`. Package versioning moved to `versionControlWildObs`. |
| `metadata.temporal` | A flat `{start, end}` object | Keyed **by `deploymentGroup`**, plus a `timeZone` string. |
| `metadata.coordinatePrecision` | A numeric value | Accepts **string or double or null**; validator explicitly relaxed because mobilisation writes it via `format()`. Both types occur. Not used analytically. |
| `metadata.created` | ISO 8601 date | Stored as a **string**, not a BSON `date` (unlike `deploymentStart` etc., which are real dates). |
| `observations.mediaID` | "Only applicable for media-based observations" | Populated on **100%** of rows even though `observationLevel` is always `event`. Points at a representative file, not a media-level classification. |
| `observations.observationLevel` | Enum `media` \| `event` | Only **`event`** occurs. No media-level observations exist in the public mirror. |
| `deployments.deploymentGroups` | Free-form spatial/temporal grouping | Constrained: one `locationName`, max **100-day** duration. |

### 3. WildObs-only additions with no Camtrap DP equivalent

| Addition | Where | Purpose |
|---|---|---|
| **`covariates` collection (127 fields)** | whole collection | Pre-extracted environmental predictors per deployment at 5 buffer scales. No Camtrap DP analogue. |
| `projectName` | all 4 data collections | WildObs project scoping; joins to `metadata.id`. Backs the compound update-diff indexes. |
| `_rowHash` | all 4 data collections | Canonicalised row hash for insert-vs-update change detection. Pipeline machinery, not data. |
| `multiSeason_deploymentGroup` | `deployments` | Parent grouping above `deploymentGroups`; splits continuous sampling at a 30-day gap. |
| `deltaTime_event` | `observations` | Seconds between consecutive events; supports independence filtering. |
| `observationID` | `media` | Reverse link media → observation. Camtrap DP only links observation → media. |
| `TIR` | `media` | Membership in the WildObs Tagged Image Repository. |
| `WildObsMetadata` (11 keys) | `metadata` | Governance and data-sharing block — see below. |
| `versionControlWildObs` | `metadata` | WildObs package version-control identifier. |
| `directory` | `metadata` | Filesystem directory of the package. **Undeclared in the validator.** |
| `DPID` | `metadata.project` | WildObs data-package identifier inside the otherwise-standard `project` object. |

### Data access, sharing gates, and obscuring machinery

Verified against `R/wildobs_mongo_query.R` and `R/wildobs_dp_download.R` (branch `api_update`)
and by live calls through the public API.

**Two access paths, and only one can ever be admin:**

| Path | Argument | Admin? | Set where |
|---|---|---|---|
| Public HTTP API | `api_key` | **Never** | `use_admin <- FALSE`, hard-coded for all API access (`wildobs_mongo_query.R:186`). A source `TODO` notes no API key currently grants admin. |
| Direct MongoDB | `db_url` | Only for the PROD host, and only if a VPN ping succeeds | `use_admin <- TRUE` (`wildobs_mongo_query.R:178`). Any other host → `FALSE`. |

If both are supplied, `db_url` wins. If neither, the function stops.

**What the public API exposes.** A query with `tabularSharingPreference = c("open","partial","closed")`
returned metadata for **44 projects — not the 48 in this mirror**. The 4 `closed` projects were
never served over the API. Of the 44: **18 `open`, 26 `partial`.** Requesting `closed` without
admin also strips it client-side and raises a `warning()`, so `closed` is gated twice.

**Three gates decide whether you get tabular data.** For non-admin callers, a project must pass
all three (`wildobs_dp_download.R:459`):

1. `WildObsMetadata.tabularSharingPreference == "open"`
2. `bibliographicCitation` contains `https://raid.org/` — a **RAiD identifier must be present**
3. `bibliographicCitation` does **not** contain `DEMO`

Fail any one and the project silently downgrades to metadata-only. **2 of the 18 `open`
projects fail gate 2 or 3**, so only 16 projects actually yield tabular data through the API —
"open" alone is not sufficient, which is the single most surprising thing here.

**Verified end-to-end** (`media = TRUE, metadata_only = FALSE`, one project of each type):

| Project preference | RAiD | Resources returned |
|---|---|---|
| `open` | yes | `deployments` (32), `observations` (2,885), `media` (21,570), `covariates` (32) |
| `partial` | yes | **none** — metadata only, no resources attached |

`partial` therefore means *metadata visible, tabular data withheld*; it is not a partial row or
column subset. Media downloads page over the API in batches of 5,000 rows.

**Embargo is enforced at ingest, not at query time.** This is why the query and download
functions contain no embargo logic — by the time data reaches MongoDB, the embargo has already
been resolved into `tabularSharingPreference`. The rule lives in
`WildObs_cam-DB/code_data cleaning/Step6_QAQC/qaqc_tests/08_WildObsMetadata.R:387-449`:

```r
embargo_end  <- lubridate::add_with_rollback(as.POSIXct(dp$created),
                                             months(embargo_months))
embargo_live <- embargo_end > Sys.time()
```

and then rewrites the preference in place:

| Stored preference | Embargo live? | QAQC action |
|---|---|---|
| `open` | yes | **downgraded to `partial`** |
| `partial` | no (expired) | **upgraded to `open`** |
| `closed` | either | left `closed` — a source `TODO` flags "decide if closed gets exposed w/ expired embargo" |

Three consequences worth holding onto:

- **`tabularSharingPreference` is a derived, materialised value**, not an author's raw choice.
  Read it as "embargo state as of the last QAQC run."
- **Expiry is measured from `metadata.created`**, not from deployment or observation dates.
- **It is a snapshot.** A project whose embargo lapsed since the last Step 6 run still reads
  `partial` until QAQC re-runs. Staleness here shows up as data that *should* be open but isn't.

A missing or non-numeric `embargoPeriodMonths` defaults to **19 months** (`unk_embargo`),
deliberately an odd value so it is recognisable as a placeholder.

**EPBC obscuring is NOT implemented.** No taxon-driven obscuring exists anywhere in the
ingest pipeline, the query path (`wildobs_mongo_query()`, `wildobs_dp_download()`), or the
public-profile generator. `code_public_profiles/generate_profiles.py:706-711` marks it as a
future hook and states plainly: *"Nothing is obscured today."* EPBC status lives only in
`WildObsR::species_traits` (`epbc_category`, `epbc_location`) as a trait lookup, not as a
redaction mechanism. **Assume `scientificName` and coordinates are unredacted** and apply your
own care when handling threatened-taxa records.

## Validator vs reality — known disagreements

The validators in `WildObs_cam-DB/code_mongoDB/apply_*_schema.js` are authoritative for stated
rules. These are the places the database departs from or exceeds them.

1. **`metadata.directory` is undeclared.** Present and populated on all 48 documents but
   absent from `apply_metadata_schema.js`. The validator is not `additionalProperties: false`,
   so it passes silently. **The validator is out of date here, not the data.**
2. **`media.exifData` type mismatch.** The validator describes "a valid JSON object" and sets
   no `bsonType`; stored values are **strings** (0.28% populated).
3. **`observations.mediaID` semantics inverted.** Validator text says it is "only applicable"
   when `observationLevel = media`, but `observationLevel` is `event` on 100% of rows while
   `mediaID` is populated on 100% of rows.
4. **`required` is weaker than it looks.** MongoDB `required` only means *the key is present*.
   Every covariate also permits `null`, so all 84 "required" covariate fields are satisfied by
   a null. `FLII_point` is required yet only 75.4% non-null. Never infer non-nullness from
   `required`.
5. **Unused enum values.** `featureType` permits `roadOverpass`, `roadBridge`, `culvert`,
   `carcass`, `fruitingTree` — none occur. `cameraSetupType` permits `calibration` — never
   occurs. `captureMethod` permits `timeLapse` — never occurs in a 200k sample.
6. **`fileMediatype` pattern is too permissive.** `^(image|video|audio)/.*$` admits
   `image/must_confirm_not_provided`, `image/not_provided`, `image/files_not_provided`
   (~5% of media), and `image/AVI` (a video mislabelled as an image).
7. **`WildObsMetadata.desiredOutputs` is type-inconsistent** — object in 7 documents, string
   in 41. The validator does not constrain the block's interior.
8. **Covariate descriptions are copy-paste damaged.** All `GEEBAM_fire_severity_<n>_percent_*`
   descriptions and several others say "calculated with a **NA** meter buffer"; the `_point`
   family descriptions all say "with a meter buffer" with the number missing. The scale is
   carried by the field name suffix, not the prose.

No field required by a validator is missing from the data, and no collection contains an
undeclared field other than `metadata.directory`.

## Practical query notes

- **Always filter `observationType`.** 48.4% of observations are `blank`; a naive count of
  observations is not a count of animals.
- **Scope by `projectName` early.** It is the leading key of the compound index on all four
  data collections.
- **`$sample`, never `find(limit=n)`,** for exploration — insertion order is grouped by
  project and is not representative.
- **Dates are BSON `date`** in `deployments`, `observations`, `media` — but `metadata.created`
  is a string. Do not mix them in a comparison.
- **Read-only.** This skill documents a mirror; do not write, update, or drop anything.
