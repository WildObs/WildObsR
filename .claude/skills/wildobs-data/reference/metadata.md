# `metadata` — full field reference

Source of truth: `camdb_mongodb/code_mongoDB/apply_metadata_schema.js`, generated from the WildObs
profile `1.0.2-wildobs.1`.
Empirical coverage: **all 54 documents** on LOCAL (full scan, so coverage is exact).

**24 fields + `_id`.** One document = one Camtrap DP data package, i.e. one WildObs project.
54 projects, matching the 54 distinct `projectName` values in the data collections.

Validator `required` (11): `resources`, `profile`, `created`, `contributors`, `project`,
`spatial`, `temporal`, `taxonomic`, `id`, `WildObsMetadata`, `versionControlWildObs`.
The validator is not `additionalProperties: false`.

## Camtrap DP package identity

| Field | BSON type | Required | Coverage | Meaning |
|---|---|---|---|---|
| `id` | string | **yes** | 100% | Unique package identifier including the persistent WildObs ID. **The join key: matches `projectName` in `deployments`, `observations`, `media`, `covariates`.** Indexed. |
| `name` | string | no | 100% | Short machine-friendly package name. |
| `title` | string | no | 100% | One-sentence package title. |
| `description` | string | no | 100% | Full package description. |
| `created` | string | **yes** | 100% | Creation date-time with offset (e.g. `2025-11-05T16:46:03+10:00`). Stored as **string**, not BSON `date`. Embargo expiry is measured from here. |
| `profile` | string | **yes** | 100% | Always the upstream `https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0.2/camtrap-dp-profile.json`. |
| `version` | string | no | 100% | The *Camtrap DP standard* version, always `1.0.2`. WildObs convention, documented in the profile. |
| `versionControlWildObs` | string | **yes** | 100% | WildObs version of this package, bumped by the pipeline when a correction changes the data. Observed `1.0.1`–`2.1.1`. **WildObs extension.** |
| `homepage` | string | no | 100% | Project homepage URL. |
| `image` | string | no | 1.9% | Key present on 46 of 54 docs but only **1** has a value. Effectively unused. |
| `keywords` | array | no | 100% | Keyword strings. |
| `bibliographicCitation` | string | no | 100% | Citation. Contains `RAiD: https://raid.org/...` for all open and partial projects; closed projects carry a placeholder. Read by the data-sharing gate. |
| `licenses` | array | no | 100% | Data and media licences. |
| `sources` | array | no | 100% | **Array** of `{title, path?, email?}` objects, as Camtrap DP specifies. 53 docs have one source, 1 has two. `title` 100%, `path` 96%, `email` 89% of entries; absent keys are **omitted**, not null. Most common title: `Wildlife Insights` (31). |
| `relatedIdentifiers` | array | no | 94.4% (51/54) | `{relationType, relatedIdentifier, relatedIdentifierType, resourceTypeGeneral?}`. Types: `RAiD` 49 (all `IsPartOf`), `DOI` 37, `URL` 4. |
| `references` | array | no | 98.1% (53/54) | Related references. |
| `contributors` | array | **yes** | 100% | `{title, role, email?, path?, organization?, ROR?}`. 160 entries. `ROR` (WildObs extension) on 127; `path` on 139. Absent keys are **omitted**, not null. Roles: `contributor` 91, `principalInvestigator` 54 (exactly one per package), `rightsHolder` 7, `contact` 6, `publisher` 2. |
| `coordinatePrecision` | double | no | 96.3% (52/54) | Least precise coordinate precision, decimal degrees. Validator `bsonType: [double, int]`. |
| `resources` | array | **yes** | 100% | Exactly four per package: `deployments`, `observations`, `media`, `covariates`. Each `{name, path, profile, format, mediatype, encoding, schema}` with `schema` an **inline object** — see below. |

## Coverage blocks

| Field | BSON type | Required | Coverage | Meaning |
|---|---|---|---|---|
| `spatial` | object | **yes** | 100% | GeoJSON `FeatureCollection` (`type`, `features`); one feature per `locationName`. **Do not print coordinate values.** |
| `temporal` | object | **yes** | 100% | See the shape below. |
| `taxonomic` | array | **yes** | 100% | `{scientificName, taxonID, taxonRank, vernacularNamesEnglish}` on all 3,211 entries. `taxonRank` includes WildObs intermediate ranks (`subclass`, `suborder`, `subfamily`). |

### `temporal` shape

```json
{
  "start": "2022-04-06",                      // package-level extent (Camtrap DP, required)
  "end":   "2022-11-23",
  "timeZone": "Australia/Sydney",             // WildObs extension
  "<deploymentGroup>": { "start": "2022-04-06", "end": "2022-07-15" },   // WildObs extension, one per group
  ...
}
```

- All values are **`YYYY-MM-DD` strings**. Package `start`/`end` are the min/max over the groups.
- 625 deploymentGroup blocks across 54 packages, matching the 625 distinct
  `deployments.deploymentGroups`.
- `timeZone` on all 54: `Australia/Sydney` 16, `Australia/Brisbane` 15, `Australia/Perth` 8,
  `Australia/Melbourne` 5, `Australia/Adelaide` 4, `Australia/Darwin` 3, `Australia/Hobart` 2,
  `Etc/GMT-8` 1.
- Identify a deploymentGroup **by structure** (its value is an object), not by excluding known
  names — `start`, `end` and `timeZone` are scalars.

### Inline resource schemas

`resources[].schema` = `{name, title, description, fields[], primaryKey, missingValues, foreignKeys?}`.
`primaryKey` is a string; `missingValues` is `["", "NA", "NaN", "nan"]`; `foreignKeys` appears on
`observations` (→ deployments, → media) and `media` (→ deployments).

Field counts: deployments 25, observations 29, media 13, covariates 125. `projectName` and
`_rowHash` are **not** declared (`wildobs_dp_download()` adds `projectName` itself).

Each field: `{name, description, type, format?, example?, unit?, constraints?, skos:exactMatch?,
skos:broadMatch?, skos:narrowMatch?, custom?}`.
- `constraints` keys: `required` (bool), `unique`, `minimum`, `maximum`, `enum`, `pattern`
  (`pattern` on `media.filePath` and `media.fileMediatype`).
- `skos:narrowMatch` is a string on some fields and an array on others.
- `example` may be string, int, double, bool or object.
- `custom` (covariates only, WildObs extension): `{spatial: {resolution, buffer_m}, source:
  {doi, url, citation}}`. **`buffer_m` is unreliable on the GEEBAM and IBRA fields** — see
  SKILL.md, validator disagreement 6.

## `project` sub-object

Camtrap DP defines 9 properties; WildObs stores 10. All 54 documents carry all 10.

| Key | Camtrap DP? | Meaning |
|---|---|---|
| `id` | yes | Project identifier. |
| `title` | yes (required) | Project title. |
| `acronym` | yes | Project acronym. |
| `description` | yes | Project description. |
| `path` | yes | Project website URL. |
| `samplingDesign` | yes (required) | `systematicRandom` 16, `targeted` 14, `experimental` 9, `clusteredRandom` 8, `simpleRandom` 6, `opportunistic` 1. |
| `captureMethod` | yes (required) | Array; `activityDetection` on all 54. |
| `individualAnimals` | yes (required) | Bool. |
| `observationLevel` | yes (required) | Array. |
| `DPID` | **no — WildObs only** | The package id, repeated. |

## `WildObsMetadata` sub-object — WildObs only

The governance and data-sharing block. 10 keys.

| Key | BSON type | Coverage | Meaning |
|---|---|---|---|
| `DPID` | string | 100% | WildObs data-package identifier. |
| `tabularSharingPreference` | string | 100% | **Access control, enforced.** `partial` 26, `open` 23, `closed` 5. `open` is necessary but not sufficient for tabular data — a RAiD citation is also required. See SKILL.md. |
| `embargoPeriodMonths` | int | 100% | Months from `created` before the package becomes shareable. Observed: 0 (×19), 1, 4, 6 (×1 each), 12 (×3), 16 (×2), 17 (×5), 24 (×5), 48 (×17). **Enforced at ingest**: baked into `tabularSharingPreference`. |
| `WildObsContribution` | string | 100% | How the project contributes to WildObs. |
| `fundingAgency` | string | 100% | Funding body. |
| `desiredOutputs` | string | 74% (40/54) | What the provider wants back. Absent (key omitted) on 14. |
| `deploymentClusters` | bool | 100% | Deployments spatially clustered. `true` ×19. |
| `deploymentTags` | string | 100% | Project-level deployment tagging notes. |
| `groupSizes` | bool | 100% | Group sizes recorded. `true` ×35. |
| `thinnedMedia` | bool | 100% | Media thinned before ingest. `true` ×30. Relevant when interpreting media per observation. |
