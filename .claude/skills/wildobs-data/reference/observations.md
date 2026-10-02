# `observations` — full field reference

Source of truth: `camdb_mongodb/code_mongoDB/apply_observations_schema.js`.
Empirical coverage: `$sample` of 20,000 of 1,973,401 documents (1.0%) on LOCAL (2026-10-02).
Controlled vocabularies below come from a **full `$group` over all 1,973,401 documents**, not a sample.

**31 fields + `_id`.** One document = one *independent detection event* — a taxon seen at a
deployment within a 5-minute temporal window.

Validator `required` (8): `observationID`, `deploymentID`, `eventStart`, `eventEnd`,
`observationLevel`, `observationType`, `classificationMethod`, `projectName`.

## The temporal hierarchy — read this before aggregating

WildObs uses two nested windows, which is the single most important thing to get right:

- **`observationID`** — a **5-minute** window. The primary key: 1,973,401 distinct across
  1,973,401 documents, so one row per `observationID`.
- **`eventID`** — a **30-minute** window, the *coarser* grouping. 1,405,124 distinct, mean
  1.40 observations per event, max 6.

So `eventID` is 1:many to `observationID`. To count independent detections use
`observationID`; to collapse to coarser encounters group by `eventID`.

## Identity and joins

| Field | BSON type | Required | Coverage | Meaning |
|---|---|---|---|---|
| `observationID` | string | yes | 100% | Primary key. 5-minute detection window. |
| `deploymentID` | string | yes | 100% | Foreign key to `deployments.deploymentID`. 19,736 distinct — every deployment has observations. |
| `eventID` | string/null | no | 100% | 30-minute grouping window. See hierarchy above. |
| `mediaID` | string/null | no | 100% | Foreign key to `media.mediaID`. **One representative media per observation.** See the caveat below. |
| `projectName` | string | yes | 100% | WildObs project identifier; joins to `metadata.id`. 54 distinct. |
| `_rowHash` | string | no | 100% | WildObs pipeline: content hash for insert-vs-update diffing. |

> **`mediaID` caveat.** Camtrap DP says `mediaID` is "only applicable for media-based
> observations (`observationLevel` = `media`)". Here `observationLevel` is `event` for
> **100%** of rows, yet `mediaID` is populated on **100%** of rows. WildObs uses it to point
> at one representative media file per observation, not to mark a media-level classification.
> Do not read a populated `mediaID` as evidence of a media-level observation.

## Time

| Field | BSON type | Required | Coverage | Meaning |
|---|---|---|---|---|
| `eventStart` | date | yes | 100% | Timestamp of the first media file in the observation. BSON `date`. |
| `eventEnd` | date | yes | 100% | Timestamp of the last media file in the observation. |
| `deltaTime_event` | double/int/null | no | 97.7% | Seconds between consecutive distinct `eventID`s. **WildObs-only, not Camtrap DP.** Used for independence filtering. |

## Classification content

| Field | BSON type | Required | Coverage | Meaning |
|---|---|---|---|---|
| `observationLevel` | string | yes | 100% | Enum `media` \| `event`. **Only `event` occurs** (1,973,401 / 1,973,401). |
| `observationType` | string | yes | 100% | What was recorded. Full distribution below. |
| `scientificName` | string/null | no | 100% | Binomial of the observed taxon. **692 distinct.** High cardinality — treat as free-ish text; validate against `WildObsR::species_traits`. |
| `count` | int/double/null | no | 86.1% | Number of individuals, minimum 1. Null on blanks/unknowns, which is why coverage is not 100%. |
| `classificationMethod` | string | **yes** | 100% | Enum `human` \| `machine` (no empty string). Observed: `human` 1,851,194 (93.8%), `machine` 122,207 (6.2%). |
| `classifiedBy` | string/null | no | 64.7% | Person or AI algorithm that made the most recent classification. |
| `classificationTimestamp` | date/null | no | **4.0%** | When the classification was made. Largely unpopulated. |
| `classificationProbability` | double/int/null | no | 10.4% | Confidence 0–1. Populated mainly for machine classifications. |

### `observationType` — full distribution (all 1,973,401 docs)

| Value | Count | Share |
|---|---|---|
| `blank` | 959,098 | 48.6% |
| `animal` | 720,123 | 36.5% |
| `unknown` | 187,606 | 9.5% |
| `vehicle` | 64,486 | 3.3% |
| `human` | 42,088 | 2.1% |

`unclassified` is permitted by the validator but no longer occurs. **Filter to `observationType == "animal"` for almost any
ecological analysis** — nearly half the collection is blank frames.

## Individual-level fields — effectively unused

Every field in this block is present on all documents but essentially never populated. They
exist for Camtrap DP conformance. Do not build analyses on them without checking PROD first.

| Field | BSON type | Coverage | Note |
|---|---|---|---|
| `sex` | string/null | **0.0%** | Enum `female` \| `male` \| `""`. Only 424 female + 62 male in the entire 1.97M collection (0.025%). |
| `lifeStage` | string/null | **0.2%** | Enum `adult` \| `subadult` \| `juvenile` \| `""`. Whole-collection counts: adult 1,800, juvenile 1,221, subadult **1**. |
| `behavior` | string/null | **0.0%** | 2 non-null in a 20k sample. |
| `individualID` | string/null | **0.0%** | All null. No individual re-identification in the public mirror. |
| `individualPositionRadius` | double/int/null | **0.0%** | All null. Distance sampling not supported. |
| `individualPositionAngle` | double/int/null | **0.0%** | All null. |
| `individualSpeed` | double/int/null | **0.0%** | All null. REM not supported. |
| `cameraSetupType` | string/null | **0.02%** | Enum `setup` \| `calibration` \| `""`. Only 123 `setup` rows collection-wide; `calibration` never occurs. |

## Bounding boxes

| Field | BSON type | Coverage | Meaning |
|---|---|---|---|
| `bboxX` | double/int/null | 5.9% | Left edge of the box, relative to media width (0–1). |
| `bboxY` | double/int/null | 5.9% | Top edge, relative to media height. |
| `bboxWidth` | double/int/null | 5.9% | Box width, relative to media width. |
| `bboxHeight` | double/int/null | 5.9% | Box height, relative to media height. |

All four move together (1,188 of 20,000 sampled). Present only for machine-classified subsets.

## Free text

| Field | BSON type | Coverage | Meaning |
|---|---|---|---|
| `observationTags` | string/null | 10.4% | Pipe-separated tags, optionally `key: value`. |
| `observationComments` | string/null | 0.7% | Free-text notes. |
