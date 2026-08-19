# `metadata` — full field reference

Source of truth: `WildObs_cam-DB/code_mongoDB/apply_metadata_schema.js`.
Empirical coverage: **all 48 documents** (`$sample` size 48 = full collection, so coverage is exact).

**25 fields + `_id`.** One document = one Camtrap DP data package, i.e. one WildObs project.
48 projects, matching the 48 distinct `projectName` values in every other collection.

Validator `required` (1): `id` only. Everything else is optional at the validator level even
though almost all fields are populated in practice.

## Camtrap DP package identity

| Field | BSON type | Required | Coverage | Meaning |
|---|---|---|---|---|
| `id` | string | **yes** | 100% | Unique data package identifier including the persistent WildObs ID. **The join key: matches `projectName` in `deployments`, `observations`, `media`, `covariates`.** Indexed. |
| `name` | string | no | 100% | Short machine-friendly package name. |
| `title` | string | no | 100% | One-sentence package title. |
| `description` | string | no | 100% | Full package description. |
| `created` | string | no | 100% | Creation date-time. Stored as **string**, not BSON `date`. |
| `profile` | string | no | 100% | URL of the Camtrap DP profile. Two values: `.../1.0.2/camtrap-dp-profile.json` (37 packages) and `.../1.0.1/...` (11 packages). |
| `version` | string | no | 100% | **Redefined by WildObs**: holds the *Camtrap DP standard* version (`1.0.1` ×11, `1.0.2` ×37), not the package's own version. Camtrap DP intends this as the package version. |
| `homepage` | string | no | 100% | Project homepage URL. |
| `image` | string | no | **2.1%** | Representative image. Key present on 46 of 48 docs but only **1** has a value. Effectively unused. |
| `keywords` | array | no | 100% | Keyword strings. |
| `bibliographicCitation` | string | no | 100% | Citation or placeholder. |
| `licenses` | array | no | 100% | Data and media licences. Item structure not validated. |
| `sources` | object | no | 100% | **Redefined by WildObs**: Camtrap DP specifies an *array* of source objects; WildObs stores a single *object*. |
| `relatedIdentifiers` | array | no | 100% | DOIs / URLs as objects. |
| `references` | array | no | 97.9% | Related references. Missing on 1 of 48. |
| `contributors` | array | no | 100% | Contributor objects. Item structure not validated. |
| `coordinatePrecision` | double/string/null | no | 95.8% | Coordinate precision in decimal degrees. Validator is **explicitly relaxed** to accept string or double because mobilisation writes it through `format()`; both types occur. Not used analytically. |
| `resources` | array | no | 100% | Data resource descriptors (deployments / observations / media / covariates). |

## Coverage blocks

| Field | BSON type | Required | Coverage | Meaning |
|---|---|---|---|---|
| `spatial` | object | no | 100% | GeoJSON spatial coverage (type + features). Contains bounding geometry — **do not print coordinate values**. |
| `temporal` | object | no | 100% | **Redefined by WildObs**: Camtrap DP specifies a simple `{start, end}` object. WildObs keys this block **by `deploymentGroup`** and adds a `timeZone` string. |
| `taxonomic` | array | no | 100% | Taxonomic coverage entries. |

## `project` sub-object

Camtrap DP defines 9 properties; WildObs stores 10. All 48 documents carry all 10.

| Key | Camtrap DP? | Meaning |
|---|---|---|
| `id` | yes | Project identifier. |
| `title` | yes (required) | Project title. |
| `acronym` | yes | Project acronym. |
| `description` | yes | Project description. |
| `path` | yes | Project website URL. |
| `samplingDesign` | yes (required) | Sampling layout type. |
| `captureMethod` | yes (required) | Media capture method(s). |
| `individualAnimals` | yes (required) | Whether individuals are marked. |
| `observationLevel` | yes (required) | Observation level. |
| `DPID` | **no — WildObs only** | WildObs data-package identifier. |

## `WildObsMetadata` sub-object — WildObs only, no Camtrap DP equivalent

The governance and data-sharing block. All 11 keys present on all 48 documents.

| Key | BSON type | Meaning |
|---|---|---|
| `DPID` | string | WildObs data-package identifier. |
| `tabularSharingPreference` | string | **Access control, enforced.** `partial` (26), `open` (18), `closed` (4) in this mirror. The public API serves only the 44 open+partial projects; `closed` is never returned and is also stripped client-side with a `warning()`. `open` is necessary but **not sufficient** for tabular data — a RAiD citation is also required. See the gate table in SKILL.md. |
| `embargoPeriodMonths` | int | Months from `created` before the package becomes shareable. Observed: 0 (×18), 4 (×1), 6 (×2), 12 (×2), 17 (×5), 24 (×4), 48 (×16). **Enforced at ingest, not at query time**: Step 6 QAQC computes `created + embargoPeriodMonths` and rewrites `tabularSharingPreference` accordingly, so the embargo is already baked into that field. A missing/non-numeric value defaults to 19 months as a deliberate placeholder flag. See SKILL.md. |
| `WildObsContribution` | string | How the project contributes to WildObs. |
| `fundingAgency` | string | Funding body. |
| `desiredOutputs` | object *or* string | **Type-inconsistent**: object in 7 documents, string in 41. Handle both. |
| `deploymentClusters` | bool | Whether deployments are spatially clustered. `true` ×17, `false` ×31. |
| `deploymentTags` | string | Project-level deployment tagging notes. |
| `groupSizes` | bool | Whether group sizes were recorded. `true` ×30, `false` ×18. |
| `thinnedMedia` | bool | Whether media were thinned before ingest. `true` ×25, `false` ×23. Relevant when interpreting media counts per observation. |

## WildObs-only top-level fields

| Field | BSON type | Coverage | Meaning |
|---|---|---|---|
| `WildObsMetadata` | object | 100% | Governance block, above. |
| `versionControlWildObs` | string | 100% | WildObs package version-control identifier. Carries the package versioning that Camtrap DP's `version` would normally hold. |
| `directory` | string | 100% | Filesystem directory for the package. **Not in the validator** — see the disagreement note in SKILL.md. |
