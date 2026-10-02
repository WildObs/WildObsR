# Package-level metadata

Everything in a downloaded data package outside its tables: `dp$title`, `dp$contributors`,
and so on. `extract_metadata(dp_list, element)` turns the list-shaped elements into data
frames, with a `DPID` column naming the package. Supported elements: `contributors`,
`sources`, `licenses`, `relatedIdentifiers`, `references`, `project`, `WildObsMetadata`,
`spatial`, `temporal`, `taxonomic`.

## Identity and citation

| Field | Meaning |
|---|---|
| `id` | The project ID, e.g. `QLD_Kgari_BIOL2015_2023-24_WildObsID_0004`. Equals `projectName` in every table. |
| `name`, `title`, `description` | Short name, one-line title, full description. |
| `created` | When the package was created (ISO 8601 text with offset). Embargo periods run from here. |
| `profile` | The Camtrap DP profile URL. |
| `version` | The **Camtrap DP version** the package follows (`1.0.2`) — not the dataset version. |
| `versionControlWildObs` | **WildObs.** The dataset's own version; it increases when WildObs corrects the data. |
| `bibliographicCitation` | **The citation to use**, including the project's RAiD. |
| `licenses` | Licences for the data and for the media. |
| `homepage`, `keywords`, `image` | As named. |
| `coordinatePrecision` | Precision of the coordinates, in decimal degrees, where recorded. |

## People and provenance

| Element | Meaning |
|---|---|
| `contributors` | `title` (name), `role` (exactly one `principalInvestigator` per project; also `contributor`, `rightsHolder`, `contact`, `publisher`), and where known `email`, `path` (often an ORCID), `organization`, `ROR` (**WildObs**: the organisation's ROR ID). Missing entries come back as `NA`. |
| `sources` | A list of sources the data came from (`title`, and where known `path`, `email`), e.g. Wildlife Insights. One row per source in `extract_metadata()`. |
| `relatedIdentifiers` | Linked papers and records: `relationType`, `relatedIdentifier`, `relatedIdentifierType` (`DOI`, `URL`, or **`RAiD`**, a WildObs addition identifying the research activity). |
| `references` | Free-text references. |

## Coverage

| Element | Meaning |
|---|---|
| `spatial` | GeoJSON FeatureCollection with one feature per `locationName`. `extract_metadata(dp, "spatial")` gives a bounding-box table; `geojsonsf::geojson_sf(jsonlite::toJSON(dp$spatial, auto_unbox = TRUE))` gives an `sf` object. |
| `temporal` | `start` and `end` of the whole package, the survey `timeZone`, and one `{start, end}` block per deploymentGroup (**WildObs**). Dates are `YYYY-MM-DD` text. `extract_metadata(dp, "temporal")` returns `deploymentGroup`, `start`, `end`, `timeZone`, `packageStart`, `packageEnd`, `DPID`. |
| `taxonomic` | Every taxon recorded: `scientificName`, `taxonID`, `taxonRank` (including intermediate ranks such as `subclass` and `suborder`), and `vernacularNamesEnglish` (**WildObs**). |

## `project`

| Field | Meaning |
|---|---|
| `id`, `title`, `acronym`, `description`, `path` | The originating project. |
| `samplingDesign` | `simpleRandom`, `systematicRandom`, `clusteredRandom`, `experimental`, `targeted`, or `opportunistic`. Filterable in `wildobs_mongo_query(samplingDesign = )`. |
| `captureMethod` | `activityDetection`. |
| `individualAnimals` | Whether individuals were identified. |
| `observationLevel` | `event`. |
| `DPID` | **WildObs.** The package `id`, repeated. |

## `WildObsMetadata` — WildObs only

| Field | Meaning |
|---|---|
| `tabularSharingPreference` | `open` (tables available), `partial` (metadata only, usually embargoed), or `closed`. |
| `embargoPeriodMonths` | Months after `created` before the tables can become `open`. |
| `WildObsContribution` | How the project contributes to WildObs. |
| `fundingAgency` | Who funded the work. |
| `desiredOutputs` | What the data provider would like to see come from the data (may be absent). |
| `deploymentClusters` | `TRUE` if cameras were deployed in spatial clusters. |
| `groupSizes` | `TRUE` if group sizes were recorded. |
| `thinnedMedia` | `TRUE` if images were thinned before upload, which lowers media per observation. |
| `deploymentTags` | Project-level notes on how `deploymentTags` were used. |
| `DPID` | The package `id`, repeated. |
