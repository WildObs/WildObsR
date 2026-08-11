# `observations` — full field reference

Source of truth: `WildObs_cam-DB/code_mongoDB/apply_observations_schema.js`.
Empirical coverage: `$sample` of 5,000 of 1,883,177 documents (0.27%) on the PUBLIC mirror.
Controlled vocabularies below come from a **full `$group` over all 1,883,177 documents**, not a sample.

**31 fields + `_id`.** One document = one *independent detection event* — a taxon seen at a
deployment within a 5-minute temporal window.

Validator `required` (7): `observationID`, `deploymentID`, `eventStart`, `eventEnd`,
`observationLevel`, `observationType`, `projectName`.

## The temporal hierarchy — read this before aggregating

WildObs uses two nested windows, which is the single most important thing to get right:

- **`observationID`** — a **5-minute** window. The primary key: 1,883,177 distinct across
  1,883,177 documents, so one row per `observationID`.
- **`eventID`** — a **30-minute** window, the *coarser* grouping. 1,344,410 distinct, mean
  1.40 observations per event, max 6.

So `eventID` is 1:many to `observationID`. To count independent detections use
`observationID`; to collapse to coarser encounters group by `eventID`.

## Identity and joins

| Field | BSON type | Required | Coverage | Meaning |
|---|---|---|---|---|
| `observationID` | string | yes | 100% | Primary key. 5-minute detection window. |
| `deploymentID` | string | yes | 100% | Foreign key to `deployments.deploymentID`. 18,770 distinct — every deployment has observations. |
| `eventID` | string/null | no | 100% | 30-minute grouping window. See hierarchy above. |
| `mediaID` | string/null | no | 100% | Foreign key to `media.mediaID`. **1,883,177 distinct — one unique media per observation.** See the caveat below. |
| `projectName` | string | yes | 100% | WildObs project identifier; joins to `metadata.id`. 48 distinct. |
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
| `deltaTime_event` | double/int/null | no | 97.8% | Seconds between consecutive distinct `eventID`s. **WildObs-only, not Camtrap DP.** Used for independence filtering. |

## Classification content

| Field | BSON type | Required | Coverage | Meaning |
|---|---|---|---|---|
| `observationLevel` | string | yes | 100% | Enum `media` \| `event`. **Only `event` occurs** (1,883,177 / 1,883,177). |
| `observationType` | string | yes | 100% | What was recorded. Full distribution below. |
| `scientificName` | string/null | no | 100% | Binomial of the observed taxon. **668 distinct.** High cardinality — treat as free-ish text; validate against `WildObsR::species_traits`. |
| `count` | int/double/null | no | 86.4% | Number of individuals, minimum 1. Null on blanks/unknowns, which is why coverage is not 100%. |
| `classificationMethod` | string/null | no | 100% | Enum `human` \| `machine` \| `""`. Observed: `human` 1,760,970 (93.5%), `machine` 122,207 (6.5%). |
| `classifiedBy` | string/null | no | 62.3% | Person or AI algorithm that made the most recent classification. |
| `classificationTimestamp` | date/null | no | **3.6%** | When the classification was made. Largely unpopulated. |
| `classificationProbability` | double/int/null | no | 12.2% | Confidence 0–1. Populated mainly for machine classifications. |

### `observationType` — full distribution (all 1,883,177 docs)

| Value | Count | Share |
|---|---|---|
| `blank` | 911,880 | 48.4% |
| `animal` | 689,679 | 36.6% |
| `unknown` | 186,523 | 9.9% |
| `vehicle` | 64,265 | 3.4% |
| `human` | 30,624 | 1.6% |
| `unclassified` | 206 | 0.01% |

All six validator enum values occur. **Filter to `observationType == "animal"` for almost any
ecological analysis** — nearly half the collection is blank frames.

## Individual-level fields — effectively unused

Every field in this block is present on all documents but essentially never populated. They
exist for Camtrap DP conformance. Do not build analyses on them without checking PROD first.

| Field | BSON type | Coverage | Note |
|---|---|---|---|
| `sex` | string/null | **0.0%** | Enum `female` \| `male` \| `""`. Only 269 female + 54 male in the entire 1.88M collection (0.017%). |
| `lifeStage` | string/null | **0.2%** | Enum `adult` \| `subadult` \| `juvenile` \| `""`. Whole-collection counts: adult 1,797, juvenile 1,032, subadult **1**. |
| `behavior` | string/null | **0.0%** | All null in sample. |
| `individualID` | string/null | **0.0%** | All null. No individual re-identification in the public mirror. |
| `individualPositionRadius` | double/int/null | **0.0%** | All null. Distance sampling not supported. |
| `individualPositionAngle` | double/int/null | **0.0%** | All null. |
| `individualSpeed` | double/int/null | **0.0%** | All null. REM not supported. |
| `cameraSetupType` | string/null | **0.02%** | Enum `setup` \| `calibration` \| `""`. Only 123 `setup` rows collection-wide; `calibration` never occurs. |

## Bounding boxes

| Field | BSON type | Coverage | Meaning |
|---|---|---|---|
| `bboxX` | double/int/null | 4.8% | Left edge of the box, relative to media width (0–1). |
| `bboxY` | double/int/null | 4.8% | Top edge, relative to media height. |
| `bboxWidth` | double/int/null | 4.8% | Box width, relative to media width. |
| `bboxHeight` | double/int/null | 4.8% | Box height, relative to media height. |

All four move together (240 of 5,000 sampled). Present only for machine-classified subsets.

## Free text

| Field | BSON type | Coverage | Meaning |
|---|---|---|---|
| `observationTags` | string/null | 11.4% | Pipe-separated tags, optionally `key: value`. |
| `observationComments` | string/null | 0.9% | Free-text notes. |
