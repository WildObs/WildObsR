# `media` — full field reference

Source of truth: `WildObs_cam-DB/code_mongoDB/apply_media_schema.js`.
Empirical coverage: `$sample` of 5,000 of 21,667,502 documents (0.023%) on the PUBLIC mirror.
Vocabularies below come from a `$sample` of 200,000 documents (0.92%) — **sampled, not exhaustive**,
so rare values may be missing.

**15 fields + `_id`.** One document = one media file (almost always a single image) captured
during a deployment. This is by far the largest collection: 21.7M documents, ~5 GB.

Validator `required` (8): `mediaID`, `deploymentID`, `timestamp`, `filePath`, `filePublic`,
`fileMediatype`, `observationID`, `projectName`.

## Identity and joins

| Field | BSON type | Required | Coverage | Meaning |
|---|---|---|---|---|
| `mediaID` | string | yes | 100% | Primary key. 21,667,502 distinct across 21,667,502 docs — unique. |
| `deploymentID` | string | yes | 100% | Foreign key to `deployments.deploymentID`. 18,770 distinct → every deployment has media. Mean ~1,154 media per deployment. |
| `observationID` | string | yes | 100% | Foreign key to `observations.observationID`. 1,883,177 distinct — exactly the observation count, so **every observation has media and every media belongs to one observation**. Mean ~11.5 media per observation. **WildObs-only: Camtrap DP has no `observationID` on the media table.** |
| `projectName` | string | yes | 100% | WildObs project identifier; joins to `metadata.id`. 48 distinct. |
| `_rowHash` | string | no | 100% | WildObs pipeline: content hash for insert-vs-update diffing. |

> **Indexing warning.** `media` is indexed only on `_id`, `mediaID`, and the compound
> `{projectName, mediaID, _rowHash}`. There is **no index on `deploymentID` or
> `observationID`**, so joining or grouping `media` by either is a collection scan over 21.7M
> documents. A `$group` on those keys took over two minutes in testing. Always `$match` on
> `projectName` or `mediaID` first, or drive the join from the smaller collection.

## Time and capture

| Field | BSON type | Required | Coverage | Meaning |
|---|---|---|---|---|
| `timestamp` | date | yes | 100% | When the media file was recorded. BSON `date`. |
| `captureMethod` | string/null | no | 98.5% | Enum `activityDetection` \| `timeLapse` \| `""`. Observed: `activityDetection` 98.0%, `""` 2.0%. **`timeLapse` never occurs** in the 200k sample — this corpus is entirely motion-triggered. |

## File attributes

| Field | BSON type | Required | Coverage | Meaning |
|---|---|---|---|---|
| `filePath` | string | yes | 100% | URL or package-relative path to the file. |
| `filePublic` | bool | yes | 100% | `false` if the file is not publicly accessible. **98.8% are `false`** — only ~1.2% of media are public. Treat public media as the exception. |
| `fileName` | string/null | no | 82.6% | File name. Where present, sorting by `timestamp` then `fileName` gives chronological order within a deployment. |
| `fileMediatype` | string | yes | 100% | IANA media type. **Not clean** — see below. |
| `exifData` | (unconstrained) | no | **0.28%** | EXIF metadata. Validator says "a valid JSON object" but the stored type is **string** where present. Essentially unpopulated. |
| `favorite` | bool/null | no | 100% | `true` if flagged as an exemplar image. 0.07% true. |
| `mediaComments` | string/null | no | 3.6% | Free-text notes. |
| `TIR` | bool/null | no | 100% | **WildObs-only, not Camtrap DP.** `true` if the file is included in the WildObs Tagged Image Repository. 6.7% true in the 200k sample. |

## `fileMediatype` observed values

Validator pattern: `^(image|video|audio)/.*$|^not_provided$`. Because the pattern permits any
subtype after `image/`, several placeholder values pass validation. From a 200,000-doc sample:

| Value | Count | Note |
|---|---|---|
| `image/JPG` | 188,868 | Non-standard casing; IANA type is `image/jpeg`. |
| `image/must_confirm_not_provided` | 6,868 | Placeholder, not a real media type. |
| `image/not_provided` | 2,057 | Placeholder. |
| `not_provided` | 927 | Placeholder (explicitly allowed by the pattern). |
| `image/jpg` | 635 | Lowercase variant of the same thing as `image/JPG`. |
| `image/files_not_provided` | 403 | Placeholder. |
| `video/mp4` | 241 | The only video type present. |
| `image/AVI` | 1 | Mislabelled — AVI is a video container, typed as `image/`. |

**Roughly 5% of media carry a placeholder rather than a real media type**, and casing is
inconsistent. Normalise (lowercase, then map `jpg` → `jpeg`) and treat any `*not_provided*`
value as "unknown" before filtering on file type.
