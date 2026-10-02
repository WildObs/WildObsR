# `media` — full field reference

Source of truth: `camdb_mongodb/code_mongoDB/apply_media_schema.js`.
Empirical coverage and vocabularies: `$sample` of 200,000 of 23,194,220 documents (0.86%) on LOCAL
(2026-10-02) — **sampled, not exhaustive**,
so rare values may be missing.

**15 fields + `_id`.** One document = one media file (almost always a single image) captured
during a deployment. This is by far the largest collection: 23.2M documents.

Validator `required` (8): `mediaID`, `deploymentID`, `timestamp`, `filePath`, `filePublic`,
`fileMediatype`, `observationID`, `projectName`.

## Identity and joins

| Field | BSON type | Required | Coverage | Meaning |
|---|---|---|---|---|
| `mediaID` | string | yes | 100% | Primary key; unique. |
| `deploymentID` | string | yes | 100% | Foreign key to `deployments.deploymentID`. Mean ~1,175 media per deployment. |
| `observationID` | string | yes | 100% | Foreign key to `observations.observationID`. Every media belongs to one observation. Mean ~11.8 media per observation. **WildObs-only: Camtrap DP has no `observationID` on the media table.** |
| `projectName` | string | yes | 100% | WildObs project identifier; joins to `metadata.id`. |
| `_rowHash` | string | no | 100% | WildObs pipeline: content hash for insert-vs-update diffing. |

> **Indexing warning.** `media` is indexed only on `_id`, `mediaID`, and the compound
> `{projectName, mediaID, _rowHash}`. There is **no index on `deploymentID` or
> `observationID`**, so joining or grouping `media` by either is a collection scan over 23.2M
> documents. A `$group` on those keys took over two minutes in testing. Always `$match` on
> `projectName` or `mediaID` first, or drive the join from the smaller collection.

## Time and capture

| Field | BSON type | Required | Coverage | Meaning |
|---|---|---|---|---|
| `timestamp` | date | yes | 100% | When the media file was recorded. BSON `date`. |
| `captureMethod` | string/null | no | 98.5% | Enum `activityDetection` \| `timeLapse` \| `""`. Observed: `activityDetection` 96.2%, `""` 3.8%. **`timeLapse` never occurs** in the 200k sample — this corpus is entirely motion-triggered. |

## File attributes

| Field | BSON type | Required | Coverage | Meaning |
|---|---|---|---|---|
| `filePath` | string | yes | 100% | URL or package-relative path to the file. |
| `filePublic` | bool | yes | 100% | `false` if the file is not publicly accessible. **98.8% are `false`** — only ~1.2% of media are public. Treat public media as the exception. |
| `fileName` | string/null | no | 82.9% | File name. Where present, sorting by `timestamp` then `fileName` gives chronological order within a deployment. |
| `fileMediatype` | string | yes | 100% | IANA media type. **Not clean** — see below. |
| `exifData` | (unconstrained) | no | **0.37%** | EXIF metadata. Validator says "a valid JSON object" but the stored type is **string** where present. Essentially unpopulated. |
| `favorite` | bool/null | no | 100% | `true` if flagged as an exemplar image. 0.15% true. |
| `mediaComments` | string/null | no | 3.4% | Free-text notes. |
| `TIR` | bool/null | no | 100% | **WildObs-only, not Camtrap DP.** `true` if the file is included in the WildObs Tagged Image Repository. 7.1% true in the 200k sample. |

## `fileMediatype` observed values

Validator pattern: `^((image|video|audio)/.*|not_provided)$`. Because the pattern permits any
subtype after `image/`, several placeholder values still pass validation. From a 200,000-doc
sample:

| Value | Count | Note |
|---|---|---|
| `image/jpeg` | 188,794 | Standard IANA type. Casing variants (`image/JPG`, `image/jpg`) no longer occur. |
| `image/must_confirm_not_provided` | 6,860 | Placeholder, not a real media type. |
| `image/not_provided` | 3,599 | Placeholder. |
| `image/files_not_provided` | 538 | Placeholder. |
| `video/mp4` | 209 | The only video type present. |

**About 5.5% of media carry a placeholder rather than a real media type.** The profile's
intended placeholder, bare `not_provided`, did not appear in the sample. Treat any
`*not_provided*` value as "unknown" before filtering on file type.
