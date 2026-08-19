# `covariates` — full field reference

Source of truth: `WildObs_cam-DB/code_mongoDB/apply_covariates_schema.js`.
Empirical coverage: `$sample` of 3,000 of 18,770 documents (16%) on the PUBLIC mirror.

**127 fields.** In the validator, `required` means *the key must be present* — every covariate
also permits `null`, so a required field can still be null. "Coverage" below is the % of
sampled documents where the value is non-null and non-empty.

## Join and context fields

| Field | BSON type | Required | Coverage | Meaning |
|---|---|---|---|---|
| `deploymentID` | string | yes | 100% | Camera deployment this row describes. **Join key to `deployments`, exactly 1:1.** |
| `locationID` | string/null | no | 100% | Identifier of the physical camera station. |
| `locationName` | string/null | no | 100% | Human-readable place, typically a CAPAD protected area. |
| `latitude` | double/int/null | yes | 100% | WGS84 latitude, copied from `deployments`. |
| `longitude` | double/int/null | yes | 100% | WGS84 longitude, copied from `deployments`. |
| `deploymentStart` | date/null | yes | 100% | Deployment start, copied from `deployments`. |
| `deploymentEnd` | date/null | yes | 100% | Deployment end, copied from `deployments`. |
| `deploymentGroups` | string/null | no | 100% | Spatio-temporal survey grouping, copied from `deployments`. |
| `projectName` | string | yes | 100% | WildObs project identifier; join key to `metadata.id`. |
| `_rowHash` | string | no | 100% | WildObs pipeline field: content hash used for insert-vs-update diffing. |

## Buffer scales

Each environmental family exists at five buffer radii around the deployment point. The suffix
names the **area** of the buffer, not its radius:

| Suffix | Buffer radius | Approx. area |
|---|---|---|
| `_point` | 1 m | the pixel under the camera |
| `_1km2` | 564.2 m | ~1 km² |
| `_3km2` | 977.2 m | ~3 km² |
| `_5km2` | 1,261.6 m | ~5 km² |
| `_10km2` | 1,784 m | ~10 km² |

## Environmental covariate families

Coverage is listed in scale order: point / 1km2 / 3km2 / 5km2 / 10km2.

| Family | BSON type | Valid range | Required | Coverage | Meaning |
|---|---|---|---|---|---|
| `FLII_*` | double/int/null | 0–10 | yes | 75.4% / 77.5% / 77.5% / 77.9% / 77.9% | Forest Landscape Integrity Index — composite of direct and indirect pressure on forest ecosystems. Higher = more intact. |
| `human_footprint_*` | double/int/null | 0–50 | yes | 100% / 100% / 100% / 100% / 100% | Human Footprint Index — cumulative human pressure on nature. Higher = more pressure. |
| `altitude_*` | double/int/null | 0–2250 | yes | 100% / 100% / 100% / 100% / 100% | Elevation in metres, from 3-second SRTM derived DEM v1.0. |
| `ecoregion_intactness_*` | double/int/null | 0–1 | yes | 100% / 100% / 100% / 100% / 100% | Ecoregion Intactness Index — habitat extent, quality and fragmentation combined. |
| `mean_monthly_precipitation_*` | double/int/null | ≥0 | yes | 100% / 100% / 100% / 100% / 100% | Mean monthly rainfall (mm), ANU Climate 2.0. Values after Dec 2022 are held at the 2022 value. |
| `mean_monthly_temperature_*` | double/int/null | ≥0 | yes | 100% / 100% / 100% / 100% / 100% | Mean monthly temperature (°C), ANU Climate 2.0. Same 2022 carry-forward caveat. |
| `nighttime_lights_*` | double/int/null | ≥0 | yes | 100% / 100% / 100% / 100% / 100% | VIIRS Day/Night Band annual mean radiance, stray-light/moonlight/fire removed. Urbanisation proxy. |
| `human_population_density_*` | double/int/null | ≥0 | yes | 100% / 100% / 100% / 100% / 100% | Resident population per 1 km² cell, ABS 2023 reference year, modelled to the National Nested Grid. |
| `protected_areas_*` | double/int/null | 0–1 | yes | 100% / 100% / 100% / 100% / 100% | Proportion of the buffer inside a WDPA protected area: 0 = none, 1 = fully inside. |
| `GEEBAM_fire_severity_2020_*` | double/int/null | — | no | 99.8% / 100% / 99.8% / 99.7% / 99.8% | Modal 2019/20 bushfire severity class in the buffer: 0 unburnt, 1 very low, 2 low, 3 moderate, 4 high, 5 very high/extreme. |
| `fire_events_count_*` | int/double/null | ≥0 | no | 100% / 100% / 100% / 100% / 100% | Count of distinct fire events detected in the buffer. |
| `days_since_recent_fire_*` | int/double/null | ≥0 | no | 52.4% / 58.2% / 60.2% / 61.2% / 62.4% | Days between the deployment and the most recent detected fire in the buffer. |
| `HCAS_static_*` | double/int/null | 0–1 | yes | 100% / 100% / 100% / 100% / 100% | Habitat Condition Assessment System — static habitat condition score. |
| `NDVI_*` | double/int/null | 0–1 | yes | 100% / 100% / 100% / 100% / 100% | Normalised Difference Vegetation Index — greenness / productivity proxy. |
| `terrain_ruggedness_index_*` | double/int/null | 0–2250 | yes | 100% / 100% / 100% / 100% / 100% | Terrain Ruggedness Index — local elevation heterogeneity. |
| `standardized_precipitation_index_*` | double/int/null | -3.7–3.7 | yes | 100% / 100% / 100% / 100% / 100% | Standardised Precipitation Index — drought / wet anomaly vs long-term normal. |
| `HIF_*` | double/int/null | 0–160 | yes | 100% / 100% / 100% / 100% / 100% | Human Influence Factor. |
| `EII_*` | double/int/null | 0–160 | yes | 100% / 100% / 100% / 100% / 100% | Ecosystem Integrity Index. |

## GEEBAM fire-severity class percentages

`GEEBAM_fire_severity_<class>_percent_<scale>`, where `<class>` is 0–5 and `<scale>` is
`1km2`, `3km2`, `5km2`, `10km2` — **there is no `point` variant** — giving 24 fields.
Each holds the percentage of buffer area in that severity class; across the six classes at a
given scale the values sum to 100. Type `int/double`, not required, 100% coverage.

Note the validator descriptions for these 24 fields are copy-paste damaged: several state
"calculated with a NA meter buffer". The scale is carried by the field name, not the prose.

## Categorical covariates

| Field | BSON type | Required | Coverage | Distinct | Meaning |
|---|---|---|---|---|---|
| `IBRAbioRegionName` | string | yes | 100% | 41 | IBRA7 bioregion (Australian bioregionalisation). |
| `IBRAsubRegionName` | string | yes | 100% | 85 | IBRA7 subregion, nested inside the bioregion. |
| `Olson_global_ecoregion` | string/null | yes | 92.5% | 26 | Olson et al. global terrestrial ecoregion. |
