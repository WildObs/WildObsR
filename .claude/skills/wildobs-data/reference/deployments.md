# `deployments` — full field reference

Source of truth: `WildObs_cam-DB/code_mongoDB/apply_deployments_schema.js`.
Empirical coverage: `$sample` of 3,000 of 18,770 documents (16%) on the PUBLIC mirror.

**27 fields + `_id`.** One document = one camera deployment (one camera at one location for
one continuous time window). `deploymentID` is unique — 18,770 distinct across 18,770 docs.

Validator `required` (8): `deploymentID`, `latitude`, `longitude`, `deploymentStart`,
`deploymentEnd`, `deploymentGroups`, `locationName`, `projectName`.

"Coverage" = % of sampled docs where the value is non-null and non-empty-string.

## Identity and project scoping

| Field | BSON type | Required | Coverage | Meaning |
|---|---|---|---|---|
| `deploymentID` | string | yes | 100% | Unique deployment identifier. Primary key; foreign key target for `observations`, `media`, `covariates`. |
| `projectName` | string | yes | 100% | WildObs project identifier. Joins to `metadata.id`. 48 distinct values. |
| `_rowHash` | string | no | 100% | WildObs pipeline: canonicalised row hash for insert-vs-update change detection. Not ecological. |

## Location

| Field | BSON type | Required | Coverage | Meaning |
|---|---|---|---|---|
| `locationID` | string/null | no | 100% | Identifier of the physical camera station. 6,219 distinct → mean 3.0 deployments per station, max 50. |
| `locationName` | string | yes | 100% | Human-readable place, generally a CAPAD protected area. 129 distinct. Nearby parks are merged (e.g. adjacent crater-lake parks combined); one very large park has its lowland sampling split out. Tracked as `CAPADlocationName` in the WildObs spatial hierarchy. |
| `latitude` | double/int | yes | 100% | WGS84 latitude, decimal degrees. Validator bounds −90..90. **Do not print values.** |
| `longitude` | double/int | yes | 100% | WGS84 longitude, decimal degrees. Validator bounds −180..180. **Do not print values.** |
| `coordinateUncertainty` | int/double/null | no | 100% | Radius in metres of the smallest circle containing the deployment location. Validator requires an integer ≥ 1. |

## Time

| Field | BSON type | Required | Coverage | Meaning |
|---|---|---|---|---|
| `deploymentStart` | date | yes | 100% | Deployment start. Stored as BSON `date` (UTC instant), not an ISO string. |
| `deploymentEnd` | date | yes | 100% | Deployment end. Full corpus spans 2009-03-11 → 2025-06-27. |
| `timestampIssues` | bool/null | no | 100% | `true` if media timestamps are known-bad (unknown timezone, am/pm flip). Only **2 of 18,770** deployments are flagged. |

## Survey grouping — WildObs-specific semantics

| Field | BSON type | Required | Coverage | Meaning |
|---|---|---|---|---|
| `deploymentGroups` | string | yes | 100% | Spatio-temporal survey unit. 574 distinct. WildObs constrains a group to a single `locationName` and a maximum duration of **100 days**. Format: `Landscape_<n>_<Place>_<year>_<letter>_<Surname>_WildObsID_<NNNN>`. |
| `multiSeason_deploymentGroup` | string/null | no | 100% | Parent of `deploymentGroups` for long-term sampling. 195 distinct. Continuous sampling is split into a new multi-season group at a **30-day** gap. Many `deploymentGroups` nest inside one of these. **WildObs-only, not Camtrap DP.** |

## Camera hardware

| Field | BSON type | Required | Coverage | Meaning |
|---|---|---|---|---|
| `cameraID` | string/null | no | 56.7% | Camera device identifier / serial number. |
| `cameraModel` | string/null | no | 60.3% | `manufacturer-model`. Free text and **not standardised** — `Reconyx`, `Reconyx_HP2x`, `Reconyx-HC600 HYPERFIRE`, `Reconyx_Hyperfire`, `Reconyx Hyperfire HC600` all coexist. Normalise before grouping. |
| `cameraDelay` | int/double/null | no | 47.7% | Quiet period after a trigger, in seconds. |
| `cameraHeight` | double/int/null | no | 66.3% | Camera height above ground, metres. |
| `cameraDepth` | double/int/null | no | **0.0%** | Depth below surface, metres. **Never populated** — all null. Present only for Camtrap DP conformance (underwater deployments). |
| `cameraTilt` | int/double/null | no | 61.1% | Vertical angle: −90 straight down, 0 horizontal, 90 straight up. |
| `cameraHeading` | int/double/null | no | 11.2% | Horizontal bearing, decimal degrees clockwise from north (0 = N, 90 = E). Sparse. |
| `detectionDistance` | double/int/null | no | **1.2%** | Maximum reliable detection distance, metres. Effectively unpopulated; do not rely on it for distance sampling or REM. |

## Survey context

| Field | BSON type | Required | Coverage | Meaning |
|---|---|---|---|---|
| `baitUse` | bool/null | no | 100% | `true` if bait/lure was used. Near-even split: 9,694 true / 9,076 false. Details go in `deploymentTags`. |
| `featureType` | string/null | no | 21.0% | Landscape feature the camera targets. Closed vocabulary — see below. 79% are empty string. |
| `habitat` | string/null | no | 44.9% | Free-text habitat description. 24 distinct values; not a controlled vocabulary. |
| `setupBy` | string/null | no | 30.1% | Person or organisation that deployed the camera. |
| `deploymentTags` | string/null | no | 100% | Pipe-separated `key: value` pairs. Carries the ecologically important predator-management and lure context, e.g. `predatorManagement: noManagement \| lure: standard mammal bait`. Keys are **not** standardised across projects — one project uses `predatorManagement: No mention` repeated six times. Parse defensively. |
| `deploymentComments` | string/null | no | 37.7% | Free-text notes. |

### `featureType` controlled vocabulary

Validator enum: `roadPaved`, `roadDirt`, `trailHiking`, `trailGame`, `roadUnderpass`,
`roadOverpass`, `roadBridge`, `culvert`, `burrow`, `nestSite`, `carcass`, `waterSource`,
`fruitingTree`, `""`.

Observed across all 18,770 documents (full `$group`, not a sample):

| Value | Count |
|---|---|
| `""` (empty) | 14,845 |
| `roadDirt` | 2,579 |
| `trailGame` | 573 |
| `waterSource` | 412 |
| `trailHiking` | 223 |
| `burrow` | 118 |
| `roadPaved` | 12 |
| `nestSite` | 7 |
| `roadUnderpass` | 1 |

Five enum values are permitted but **never used**: `roadOverpass`, `roadBridge`, `culvert`,
`carcass`, `fruitingTree`.
