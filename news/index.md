# Changelog

## WildObsR 0.4.0

WildObsR now builds model-ready inputs for occupancy and abundance
models.
[`effort_matrix()`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md),
[`detection_matrix()`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md)
and
[`model_matrix_input()`](https://wildobs.github.io/WildObsR/reference/model_matrix_input.md)
turn downloaded data packages into detection histories with camera-day
effort, formatted for unmarked, spOccupancy, spAbundance, or JAGS,
NIMBLE and Stan. Downloads of many projects at once no longer time out.
**Check the breaking changes below:**
[`wildobs_mongo_query()`](https://wildobs.github.io/WildObsR/reference/wildobs_mongo_query.md)
now returns partial (metadata-only) projects by default, and R 4.1 or
later is required.

### New

- [`effort_matrix()`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md)
  and
  [`detection_matrix()`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md)
  build detection histories with sampling effort for occupancy and
  abundance models
  ([\#140](https://github.com/WildObs/WildObsR/issues/140),
  [\#110](https://github.com/WildObs/WildObsR/issues/110)). Effort is
  calculated from the deployments alone, as fractional camera-days per
  local calendar day, so it never depends on where animals were
  detected, and every camera in a site counts. Occasions are
  non-overlapping runs of `w` days, aligned per site or to the calendar
  for multi-season models; effort can be split by a deployment column
  (for example `featureType`) for use as an observation covariate, and
  long-term deployments rebuilt with
  [`recombine_deployment_groups()`](https://wildobs.github.io/WildObsR/reference/recombine_deployment_groups.md)
  exclude the gaps between their pieces. Detection histories are `NA`
  only where no camera ran, and a detection on a day its camera was not
  running stops with an error. On single-camera stations both agree with
  [`camtrapR::detectionHistory()`](https://jniedballa.github.io/camtrapR/reference/detectionHistory.html).
  They work on any Camtrap DP package.
- [`effort_matrix()`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md)
  uses UTC days, with a message, when the deployments span several time
  zones, given as several `time_zone` values or found from the cameras’
  coordinates. Zone names that keep the same clock over the survey count
  as one. To split effort by two columns, combine them into one first
  (see
  [`?effort_matrix`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md)).
- [`model_matrix_input()`](https://wildobs.github.io/WildObsR/reference/model_matrix_input.md)
  adds `total_effort`, each site’s camera-days over the modelled
  occasions, as a site covariate (and to `det.covs` for single-season
  spOccupancy and spAbundance output).
  [`?model_matrix_input`](https://wildobs.github.io/WildObsR/reference/model_matrix_input.md)
  explains when to use it rather than the per-occasion `effort`, and how
  it differs from the resample’s `cellEffort`.
- [`model_matrix_input()`](https://wildobs.github.io/WildObsR/reference/model_matrix_input.md)
  formats those detection histories, with site and observation
  covariates matched to sites by name, for unmarked
  ([`occu()`](https://ecoverseR.github.io/unmarked/reference/occu.html),
  [`pcount()`](https://ecoverseR.github.io/unmarked/reference/pcount.html),
  [`occuComm()`](https://ecoverseR.github.io/unmarked/reference/occuComm.html),
  and
  [`colext()`](https://ecoverseR.github.io/unmarked/reference/colext.html)
  with `seasons`), spOccupancy
  ([`PGOcc()`](https://www.doserlab.com/files/spoccupancy-web/reference/PGOcc.html),
  [`msPGOcc()`](https://www.doserlab.com/files/spoccupancy-web/reference/msPGOcc.html),
  [`tMsPGOcc()`](https://www.doserlab.com/files/spoccupancy-web/reference/tMsPGOcc.html)
  and their spatial versions), spAbundance
  ([`NMix()`](https://www.doserlab.com/files/spabundance-web/reference/NMix.html),
  [`msNMix()`](https://www.doserlab.com/files/spabundance-web/reference/msNMix.html)),
  or as plain arrays for JAGS, NIMBLE or Stan. Camera-days of effort,
  and the share of effort under each level of an
  [`effort_matrix()`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md)
  `split_by` column, are added as observation covariates.
- [`example_datapackages()`](https://wildobs.github.io/WildObsR/reference/example_datapackages.md)
  reads two open projects shipped with the package
  (`ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001`, systematic and unbaited
  cameras in the Queensland Wet Tropics;
  `NSW_Woomargama_wombats_Linley_2021-22_WildObsID_0010`, wombat burrows
  and controls in New South Wales over four surveys in 2021-22), in the
  same shape
  [`wildobs_dp_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
  returns. Examples, tests and tutorials now run offline on real data.
  The copies hold animal observations only, no media and no email
  addresses, and are rebuilt from the public API by
  `dev/make_example_data.R`. The examples of
  [`extract_metadata()`](https://wildobs.github.io/WildObsR/reference/extract_metadata.md),
  [`extract_tags()`](https://wildobs.github.io/WildObsR/reference/extract_tags.md),
  [`recombine_deployment_groups()`](https://wildobs.github.io/WildObsR/reference/recombine_deployment_groups.md),
  [`locationName_buffer_CAPAD()`](https://wildobs.github.io/WildObsR/reference/locationName_buffer_CAPAD.md)
  and
  [`locationName_verification_CAPAD()`](https://wildobs.github.io/WildObsR/reference/locationName_verification_CAPAD.md)
  now run on them.
- [`cite_datapackages()`](https://wildobs.github.io/WildObsR/reference/cite_datapackages.md)
  turns a list of downloaded data packages into a reference list:
  formatted text, BibTeX
  ([`utils::bibentry`](https://rdrr.io/r/utils/bibentry.html)) or a
  table. It uses each package’s own `bibliographicCitation` word for
  word (WildObs citations end with the project’s RAiD) and adds the
  project’s WildObs data package version, which rises whenever WildObs
  corrects that project’s data, so a reader can tell which version was
  analysed. Packages without a citation, such as other Camtrap DP
  datasets, get one built from their contributors, year, title and link.
  `covariates = TRUE` also cites, once each, the products behind the
  covariates table (forest integrity, human footprint, climate and so
  on), each named by the covariates it is behind (e.g. `FLII`), and
  `accessed` adds the download date.
- [`recombine_deployment_groups()`](https://wildobs.github.io/WildObsR/reference/recombine_deployment_groups.md)
  rebuilds long-term deployments from the 100-day pieces the curation
  pipeline cuts them into, for multi-season occupancy, trend indices and
  other long-term analyses
  ([\#109](https://github.com/WildObs/WildObsR/issues/109)). A data
  package goes in and a data package comes out:
  - Pieces of one camera (`locationID` × `multiSeason_deploymentGroup` ×
    the `_CamN` suffix) are joined into one deployment named
    `<locationID>_ms<N>[_CamN][_partN]`.
  - A gap longer than `max_gap_days` (default 30) between pieces starts
    a new `_partN` run, since one station can sit idle much longer than
    the 30 days that separate multi-season groups. Pieces of one camera
    that overlap in time stop the function with an error naming them.
  - Deployments and covariates are collapsed, and `deploymentID` is
    remapped in observations and media. Differing values are merged by a
    rule per column type: duration-weighted means for numbers, the value
    covering the most time for factors and date-times, and
    `deploymentTags` keeps each key once, preferring real values over
    placeholders such as `unspecified`. Missing values stay `NA`.
  - A new `deployment_crosswalk` resource keeps every original piece
    with its `pieceStart` and `pieceEnd`. Calculate sampling effort from
    it, since the recombined start-to-end span includes gaps of up to
    `max_gap_days`.
  - `dp$temporal` holds one block per `multiSeason_deploymentGroup`; the
    package-level `start`, `end` and `timeZone` are unchanged. `dp$data`
    is refreshed to match.
  - Obscured threatened-species records
    (`obscured_for_<category>_species`) are kept unchanged, with a
    message saying how many.

### Documentation

- A pkgdown website, <https://wildobs.github.io/WildObsR/>, with twelve
  tutorials in three groups. *Get the data*: Get started, Discover what
  exists, Inside a WildObs data package, For data providers. *Use the
  data*: Images for computer vision, Camtrap DP interoperability,
  Spatial context. *Model the data*: Detection histories and sampling
  effort, Fit single-season models (unmarked, spOccupancy, spAbundance
  and NIMBLE), Fit multi-season models (colonisation and extinction, and
  trends), and Combine projects to ask one question. Working with Claude
  introduces the bundled Claude skill.
- Tutorials that need the WildObs API are knitted ahead of time by
  `vignettes/precompute.R`, so the website and R CMD check run without a
  key.
- `CITATION.cff` and an updated `citation("WildObsR")`.

### Bug fixes

These change the matrices
[`matrix_generator()`](https://wildobs.github.io/WildObsR/reference/matrix_generator.md)
returns; regenerate any results built with it.

- [`matrix_generator()`](https://wildobs.github.io/WildObsR/reference/matrix_generator.md)
  uses non-overlapping sampling occasions and retains the final day and
  partial occasion. Detection and observation covariate matrices use the
  same windows
  ([\#140](https://github.com/WildObs/WildObsR/issues/140)). This
  changes results previously affected by duplicated boundary days or
  omitted final days; regenerate downstream effort matrices that
  mirrored the old windows.
- [`matrix_generator()`](https://wildobs.github.io/WildObsR/reference/matrix_generator.md)
  with `all_locationNames = FALSE` now drops the landscapes where a
  species was not detected from the site covariates as well as the
  detections ([\#105](https://github.com/WildObs/WildObsR/issues/105)).
- [`matrix_generator()`](https://wildobs.github.io/WildObsR/reference/matrix_generator.md)
  counts a sampling unit as sampled until its sampling ends, not until
  its last record, so a camera that kept running gives zeros rather than
  `NA`.
- [`matrix_generator()`](https://wildobs.github.io/WildObsR/reference/matrix_generator.md)
  places records on their local day rather than their UTC day, which
  moved morning records in eastern Australia to the day before.
- [`matrix_generator()`](https://wildobs.github.io/WildObsR/reference/matrix_generator.md)
  returns site covariates in the same order as the matrix rows, keeps
  sampling units with no observations as zeros (with a message), no
  longer carries a shortened `dur` from one species to the next, and no
  longer drops a species whose observation covariates do not vary.
- [`matrix_generator()`](https://wildobs.github.io/WildObsR/reference/matrix_generator.md)’s
  input checks now run; they used to pass any input. Observation
  covariates that are not columns, and sampling units in `obs` without
  covariates, stop the function. `NA` values in site or observation
  covariates are kept, with a warning, since unmarked drops the affected
  units or occasions itself. Text date columns are converted to
  date-times, with a message, and a value that cannot be read stops the
  function.
- [`resample_covariates_and_observations()`](https://wildobs.github.io/WildObsR/reference/resample_covariates_and_observations.md)
  groups observations by their local day, in the time zone their
  `eventStart` is stored in, rather than the UTC day. Records between
  midnight and 10 am AEST used to be merged into the previous day, which
  moved some detections into the previous occasion in
  [`matrix_generator()`](https://wildobs.github.io/WildObsR/reference/matrix_generator.md).
  On project 0001, the legacy and new detection histories now agree
  exactly.
- [`matrix_generator()`](https://wildobs.github.io/WildObsR/reference/matrix_generator.md)
  uses UTC days, with a message, when the sampling units span time zones
  with different clocks; otherwise records fall on their local day, as
  before.
- [`matrix_generator()`](https://wildobs.github.io/WildObsR/reference/matrix_generator.md)
  no longer drops site covariates: one with the same value everywhere is
  kept (standardised to 0 if numeric), and logical and factor
  covariates, such as the resample’s `mode_baitUse`, are kept too.
- [`matrix_generator()`](https://wildobs.github.io/WildObsR/reference/matrix_generator.md)
  warns when `numberDeploymentsActiveAtDate` is used as an observation
  covariate. It counts only the deployments with a record that day, so
  it understates effort and depends on detections. Use
  [`effort_matrix()`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md)
  for effort.
- [`resample_covariates_and_observations()`](https://wildobs.github.io/WildObsR/reference/resample_covariates_and_observations.md)
  keeps logical and factor columns in both tables (for example `baitUse`
  and `featureType` from
  [`frictionless::read_resource()`](https://docs.ropensci.org/frictionless/reference/read_resource.html)).
  They used to be dropped silently. A column keeps its class and factor
  levels when every cell holds one value, and modal columns always do; a
  column where some cell holds several values is pasted as text (for
  example `"FALSE - TRUE"`), with a message naming it.
- [`spatial_hexagon_generator()`](https://wildobs.github.io/WildObsR/reference/spatial_hexagon_generator.md)
  now warns when several projects sample one cell at overlapping times,
  listing the cells at each scale by their `polygon_<scale>` value and
  the projects involved. The check used to group by project as well, so
  it could never fire. Each project keeps its own `cellID`, as before.
- [`resample_covariates_and_observations()`](https://wildobs.github.io/WildObsR/reference/resample_covariates_and_observations.md)
  warns, rather than stops, when a sampled cell has no observations, for
  example after filtering `obs` to a few species. The cell stays in the
  resampled covariates as sampled with no detections. Observations in a
  cell with no covariates still stop with an error.
- [`resample_covariates_and_observations()`](https://wildobs.github.io/WildObsR/reference/resample_covariates_and_observations.md)
  now weights each deployment by its camera-days when averaging
  covariates (`Avg_*`) and taking modes (`mode_*`) within a cell, so a
  camera that failed after 2 days no longer counts as much as one that
  ran 90, and a redeployed station no longer counts once per
  redeployment. Coordinates stay plain means. On typical projects values
  move by a few percent of the spread between cameras in a cell;
  regenerate resampled covariates to pick this up, or pass
  `weights = "none"` for the previous plain means.
- `resample_covariates_and_observations(obs = NULL)` resamples the
  covariates only, which is all
  [`model_matrix_input()`](https://wildobs.github.io/WildObsR/reference/model_matrix_input.md)
  needs for site covariates and takes seconds rather than minutes. Its
  tables are tagged with their scale, and
  [`effort_matrix()`](https://wildobs.github.io/WildObsR/reference/effort_matrix.md)
  and
  [`detection_matrix()`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md)
  now stop with a message naming the right table when given resampled
  covariates or observations: effort needs the deployments with their
  cellIDs, and detections the observations themselves.
- `detection_matrix(by_camera = TRUE)` now stops, with an explanation,
  when the effort matrix was built with `split_by`. It used to place
  detections by camera number in a dimension that holds the split
  levels, misplacing them or failing with “subscript out of bounds”.
  [`?detection_matrix`](https://wildobs.github.io/WildObsR/reference/detection_matrix.md)
  now spells out how abundance counts are combined (the largest count
  per event, then summed or maximised over events), and
  [`?resample_covariates_and_observations`](https://wildobs.github.io/WildObsR/reference/resample_covariates_and_observations.md)
  what its `individuals = "sum"` does and does not count.

### Breaking changes

- WildObsR now requires R 4.1 or later, which the native pipe `|>` used
  in the package already needed.
- [`wildobs_mongo_query()`](https://wildobs.github.io/WildObsR/reference/wildobs_mongo_query.md)
  now returns open **and** partial projects by default
  (`tabularSharingPreference = c("open", "partial")`), so a search shows
  every project a public key can see, including embargoed ones whose
  data exist but are not yet released. Partial projects download as
  metadata only: their data package has no resources, so code that calls
  [`frictionless::read_resource()`](https://docs.ropensci.org/frictionless/reference/read_resource.html)
  on every downloaded package stops on them. Either ask for open
  projects only,
  `wildobs_mongo_query(..., tabularSharingPreference = "open")`, or skip
  the metadata-only packages before reading their tables:
  `dp_list <- Filter(function(dp) length(dp$resources) > 0, dp_list)`.

### Fixed

- [`resample_covariates_and_observations()`](https://wildobs.github.io/WildObsR/reference/resample_covariates_and_observations.md)
  no longer stops when a column named in `mode_cols_covs` or `obs_covs`
  is entirely empty (for example `habitat` in a project that never
  recorded it). Such columns are left out with a warning naming them.
- [`resample_covariates_and_observations()`](https://wildobs.github.io/WildObsR/reference/resample_covariates_and_observations.md)
  no longer stops when a cell’s cameras recorded no observations, which
  happens whenever observations are filtered (to animals, or after
  setting obscured records aside). Those cells are kept in the resampled
  covariates as effort without detections, and a message names them.
  Observations in a cell with no camera still stop the function.
- Date-times downloaded over the API could be shifted by the computer’s
  offset from UTC (ten hours in Queensland). The API sends dates as
  MongoDB `{"$date": ...}` objects, and when an optional date field held
  both dates and empty values in one response, jsonlite read the dates
  in local time rather than UTC. Required fields (`deploymentStart`,
  `eventStart`, media `timestamp`) are never empty and were not
  affected; `classificationTimestamp` was, depending on where you were
  and which projects were downloaded together. WildObsR now reads every
  API date as UTC before applying the project’s time zone.
- Ready for frictionless 2.0.0
  ([\#136](https://github.com/WildObs/WildObsR/issues/136)). From that
  version
  [`frictionless::add_resource()`](https://docs.ropensci.org/frictionless/reference/add_resource.html)
  writes Data Package v2 tables, which lack the
  `profile = "tabular-data-resource"` that Camtrap DP 1.0.x requires, so
  [`as_camtrapdp()`](https://wildobs.github.io/WildObsR/reference/as_camtrapdp.md)
  exports would have failed the Camtrap DP profile.
  [`wildobs_dp_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md),
  [`recombine_deployment_groups()`](https://wildobs.github.io/WildObsR/reference/recombine_deployment_groups.md)
  and
  [`as_camtrapdp()`](https://wildobs.github.io/WildObsR/reference/as_camtrapdp.md)
  now keep every table in the v1 form with either frictionless version.
  The full test suite passes against both frictionless 1.3.0 and 2.0.1.
  WildObsR now requires frictionless 1.3.0 or later, the first version
  with `resource_names()` and `schema()`.
- The
  [`wildobs_mongo_query()`](https://wildobs.github.io/WildObsR/reference/wildobs_mongo_query.md)
  help no longer says the function filters out projects still under
  embargo. Embargoed projects are stored as `partial` until the embargo
  ends, and the help now says so. Its example uses `api_key` rather than
  an undefined `db_url`.
- [`wildobs_dp_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
  over the API now downloads any number of projects in one call.
  Previously each table came back in a single request, and asking for
  many projects at once failed with `HTTP 504` because the response was
  too large to arrive before the API timed out. Each table is now
  fetched in pages: 10,000 deployments, 50,000 observations or 5,000
  covariate rows at a time. All 23 open projects now download in one
  call in about three minutes.
- After the pages are in, the number of rows in each table is checked
  against the server’s own count, and
  [`wildobs_dp_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
  stops with an error rather than return part of a table. Obscured
  threatened-species observations, which share one placeholder
  `observationID`, are fetched separately so paging cannot skip them.

## WildObsR 0.3.0

The WildObs database now follows the Camtrap DP standard more closely,
and earlier versions of WildObsR cannot read the updated metadata:
[`wildobs_dp_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
stops with `incorrect number of dimensions`. **Update WildObsR to
download data again.**

``` r

devtools::install_github("WildObs/WildObsR")
```

### Breaking changes

#### `dp$temporal` keeps the database’s shape

Downloaded packages now store `temporal` exactly as the database does:
the package-level `start`, `end` and `timeZone` once at the top, then
one `{start, end}` block per deploymentGroup. Previously `timeZone` was
copied into every group. Code that read `dp$temporal[[1]]$timeZone`
should read `dp$temporal$timeZone` instead, or use
`extract_metadata(dp, "temporal")`, which handles both shapes.

#### Spatial functions no longer take a shapefile path

[`ibra_classification()`](https://wildobs.github.io/WildObsR/reference/ibra_classification.md),
[`locationName_verification_CAPAD()`](https://wildobs.github.io/WildObsR/reference/locationName_verification_CAPAD.md)
and
[`locationName_buffer_CAPAD()`](https://wildobs.github.io/WildObsR/reference/locationName_buffer_CAPAD.md)
now use the IBRA7 and CAPAD 2022 layers that ship with the package, so
they work on any computer. The `ibra_file_path` and `capad_file_path`
arguments are gone: remove them from your calls. The layers are
available directly as the datasets `ibra` and `capad`
([\#11](https://github.com/WildObs/WildObsR/issues/11),
[\#122](https://github.com/WildObs/WildObsR/issues/122)).

[`locationName_verification_CAPAD()`](https://wildobs.github.io/WildObsR/reference/locationName_verification_CAPAD.md)
now measures `CAPADminDistance` in true metres. Previously it measured
in web Mercator units, which overstate distances in Australia by roughly
10–40% depending on latitude, so values will be smaller than before and
the 1–5 km / 5–10 km / \>10 km notes may change category.

#### `wildobs_mongo_query()` returns an empty vector when nothing matches

When no project matches,
[`wildobs_mongo_query()`](https://wildobs.github.io/WildObsR/reference/wildobs_mongo_query.md)
now returns `character(0)` rather than `""`, still with a warning.
`length(result) > 0` is now `FALSE`, and loops over the result no longer
run once with a blank ID. Code that tested `result == ""` should test
`length(result) == 0` instead.
[`wildobs_dp_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
stops with a clear message when given no IDs, including the old `""`
([\#129](https://github.com/WildObs/WildObsR/issues/129)).

#### `dp$sources` is a list of sources

`sources` is now an array of source objects, as Camtrap DP specifies, so
`dp$sources[[1]]$title` replaces `dp$sources$title`.
`extract_metadata(dp, "sources")` returns one row per source.

### New

- [`as_camtrapdp()`](https://wildobs.github.io/WildObsR/reference/as_camtrapdp.md)
  converts a WildObs data package to canonical Camtrap DP 1.0.2 (or
  1.0.1), so it works with the `camtrapdp` R package, including
  [`camtrapdp::write_dwc()`](https://inbo.github.io/camtrapdp/reference/write_dwc.html)
  for GBIF. It removes every WildObs addition, read from the official
  Camtrap DP profiles vendored in the package rather than a fixed list,
  and returns a report of every change and the result of validating it
  ([\#113](https://github.com/WildObs/WildObsR/issues/113)). Its
  `camtrapdp` element is the result already read by
  [`camtrapdp::read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.html),
  so it goes straight into camtrapdp functions:
  `camtrapdp::write_dwc(out$camtrapdp, dir)`.
- [`wildobs_media_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_media_download.md)
  downloads the image files listed in a media table into
  `out_dir/<projectName>/<deploymentID>/<mediaID>.<ext>`, and reports
  what happened to each file. Publicly hosted files
  (`filePublic = TRUE`) download for anyone; files on your own computer
  are copied; private Google Cloud files can be fetched with a
  `gcs_token` if you have access. Re-running fetches only what is
  missing.
- [`install_claude_skill()`](https://wildobs.github.io/WildObsR/reference/install_claude_skill.md)
  installs `wildobsr-data`, a Claude skill describing how WildObs data
  packages are structured and what the data can support, so Claude can
  help with your analysis. See “Help Claude understand WildObs data” in
  the README. It describes the data as API users receive it, including
  how obscured threatened-species records behave and how to set them
  aside.
- `extract_metadata(dp, "temporal")` gains `packageStart` and
  `packageEnd` columns holding the package-level temporal extent
  ([\#137](https://github.com/WildObs/WildObsR/issues/137)).
- Downloaded packages now include `versionControlWildObs`, the WildObs
  version of the package.
- Covariate fields in downloaded schemas now carry a `custom` block with
  the source citation and resolution of the spatial product behind each
  covariate, and media fields keep their `pattern` constraints.

### Fixed

- [`wildobs_dp_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
  skips project IDs it cannot find, with one warning naming them, and
  downloads the rest. It stops only when none are found. Previously an
  unknown ID failed with “subscript out of bounds”
  ([\#129](https://github.com/WildObs/WildObsR/issues/129)).
- [`apply_schema_types()`](https://wildobs.github.io/WildObsR/reference/apply_schema_types.md)
  gives empty `datetime` columns the `POSIXct` type, and converts partly
  empty ones. Previously a column with any empty cell was treated as
  unparseable and left as text, or as `logical` if wholly empty
  ([\#129](https://github.com/WildObs/WildObsR/issues/129)).
- [`apply_schema_types()`](https://wildobs.github.io/WildObsR/reference/apply_schema_types.md)
  no longer deletes a `datetime` column whose schema field has no
  `format`: it falls back to ISO 8601, and leaves the column unchanged
  with a warning if it still cannot parse it
  ([\#129](https://github.com/WildObs/WildObsR/issues/129)).
- [`wildobs_dp_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
  no longer hides warnings from typing the tables, so a column that
  fails to convert is reported rather than passed on silently
  ([\#129](https://github.com/WildObs/WildObsR/issues/129)).
- [`apply_schema_types()`](https://wildobs.github.io/WildObsR/reference/apply_schema_types.md)
  accepts the Frictionless `any` type and leaves such columns unchanged,
  so media downloads no longer warn about `exifData`.
- [`wildobs_mongo_query()`](https://wildobs.github.io/WildObsR/reference/wildobs_mongo_query.md)
  checks a `spatial` bounding box before querying and names any missing
  or non-numeric corner, instead of failing with an internal tibble
  error ([\#129](https://github.com/WildObs/WildObsR/issues/129)).
- [`ibra_classification()`](https://wildobs.github.io/WildObsR/reference/ibra_classification.md)
  no longer drops locations that fall just outside every IBRA subregion.
  They now take the IBRA values of their nearest matched location, as
  documented ([\#97](https://github.com/WildObs/WildObsR/issues/97)).
- [`locationName_buffer_CAPAD()`](https://wildobs.github.io/WildObsR/reference/locationName_buffer_CAPAD.md)
  generates UTM coordinates when they are missing. Its check matched any
  column name containing “x” or “y”, including `deploymentID`, so it
  always skipped generation and stopped with “No unique UTM zones
  found”.
- [`locationName_verification_CAPAD()`](https://wildobs.github.io/WildObsR/reference/locationName_verification_CAPAD.md)
  no longer leaves its internal `ID`, `lat2` and `long2` columns in the
  output.
- [`locationName_buffer_CAPAD()`](https://wildobs.github.io/WildObsR/reference/locationName_buffer_CAPAD.md)
  stops with an error naming any `deploymentID` used more than once.
  Previously rows sharing an ID were silently merged into one.
- [`wildobs_dp_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
  reads the updated database structure.
- `wildobs_mongo_query(temporal = ...)` no longer errors on the
  package-level temporal extent, and still matches projects on when each
  deploymentGroup ran.
- [`extract_metadata()`](https://wildobs.github.io/WildObsR/reference/extract_metadata.md)
  still reads data packages saved by earlier versions.
- `wildobs_dp_download(api_key = ..., media = TRUE)` no longer fails
  after the media download with `argument is of length zero`. Public
  media tables, which withhold `fileName`, get an empty `fileName`
  column on every route.
- [`wildobs_dp_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
  over the API retries a request that fails or returns an error, up to
  three times, instead of failing with `subscript out of bounds`. A
  large media download makes hundreds of requests, so one dropped
  request used to end it. If a request still fails, the error names it
  and gives the server’s reason; a media download no longer returns
  partial results silently.
- [`wildobs_dp_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
  no longer duplicates observations of a taxon listed twice in a
  project’s taxonomic metadata. It keeps the first listing and warns
  which taxa were repeated.

### Internal

- The update notice now only appears for a new major or minor release,
  not for patch releases.

## WildObsR 0.2.0

This is a breaking release. Some functions you may have called directly
are no longer available, and the bundled API key has been removed. The
three changes most likely to affect you are listed first, each with what
you need to do.

### Breaking changes

#### The bundled API key is gone — create your own

Earlier versions shipped a shared API key as a dataset called
`wildobsr_api_key`. That dataset has been removed and the key it
contained no longer works.

**What to do:** create a personal API key on the [WildObs
Dashboard](https://dashboard.wildobs.org.au/), store it in your
`.Renviron` file, and read it in R with:

``` r

api_key <- Sys.getenv("WILDOBSR_API_KEY")
```

Keys are tied to you personally rather than to a project. The README has
a step-by-step walkthrough under “Getting Database Access”. Treat your
key like a password: do not paste it into a script that you share or
commit.

#### Nine helper functions are now internal

These were previously callable with `WildObsR::function_name()`. They
are still in the package and still used by it, but they are no longer
part of the public interface, because they are helpers for other
functions rather than tools meant to be run on their own:

[`clean_list_recursive()`](https://wildobs.github.io/WildObsR/reference/clean_list_recursive.md),
[`convert_df_to_list()`](https://wildobs.github.io/WildObsR/reference/convert_df_to_list.md),
[`extract_classif()`](https://wildobs.github.io/WildObsR/reference/extract_classif.md),
[`find_closest_match()`](https://wildobs.github.io/WildObsR/reference/find_closest_match.md),
`is_empty_spatial()`,
[`is_empty_temporal()`](https://wildobs.github.io/WildObsR/reference/is_empty_temporal.md),
[`long_to_UTM_zone()`](https://wildobs.github.io/WildObsR/reference/long_to_UTM_zone.md),
[`reformat_fields()`](https://wildobs.github.io/WildObsR/reference/reformat_fields.md),
[`reformat_schema()`](https://wildobs.github.io/WildObsR/reference/reformat_schema.md)

**What to do:** if a script of yours calls one of these with
`WildObsR::`, it will now fail. Most of them were only ever used inside
other WildObsR functions, so in practice you probably do not call them
at all. If you genuinely need one, it can still be reached with three
colons, as in `WildObsR:::convert_df_to_list()`, but that is a stopgap
rather than a promise: internal functions can change or disappear
without notice. Tell us which one you need and we will consider making
it public, as we did this release for the two functions listed under
**New** below.

Two of the nine have changed further since: `is_empty_spatial()` has
been removed outright, and
[`find_closest_match()`](https://wildobs.github.io/WildObsR/reference/find_closest_match.md)
is deprecated. Both are covered below.

#### MongoDB document helpers removed

`mongo_clean_df()`, `mongo_format_dates()` and `mongo_prepare_doc()`
have been removed. All three prepared data for *writing into* the
WildObs database, which is not something this package does — it only
reads. They now live in the private repository used to run database
updates.

**What to do:** nothing, unless you were calling them directly, which
would only be the case if you help maintain the database itself.
Downloading and querying are unaffected.

### Deprecated

Both still work exactly as before, and both now print a warning the
first time you use them in a session. They will be removed in a future
release.

- **[`check_schema()`](https://wildobs.github.io/WildObsR/reference/check_schema.md)**
  has been retired from the data intake workflow. Schema validation now
  happens inside the download itself, so you no longer need to run this
  as a separate step.
- **[`find_closest_match()`](https://wildobs.github.io/WildObsR/reference/find_closest_match.md)**
  is no longer used by the package. It warns only once per session
  rather than once per call, because it is often passed to
  [`sapply()`](https://rdrr.io/r/base/lapply.html) where a per-call
  warning would flood your console.

**What to do:** if either appears in a script, plan to remove the call.
Nothing breaks today.

### Removed

- **`gbif_check()`** — had no callers anywhere in the package or in our
  other repositories.
- **`is_empty_spatial()`** — had no callers and no known external usage.
  [`is_empty_temporal()`](https://wildobs.github.io/WildObsR/reference/is_empty_temporal.md)
  is unaffected and still works.

### New

#### Two helpers are now public

Both were already being used from other WildObs projects, so they have
been given proper documentation, examples, and unit tests, and are now
supported:

- **`rename_or_add_column(df, new_name, old_name)`** renames a column,
  or adds a new all-`NA` column if you pass `""`, `NA`, or nothing as
  the old name. **Note the argument order: the new name comes before the
  old name.** Some mobilisation markdowns define their own copy of this
  function with the arguments the other way round. Those local copies
  still take precedence in your own script, so nothing changes for you
  until you delete one — at which point check the order at every call
  site.
- **`get_decimal_places(x)`** counts significant decimal places in a
  number, ignoring trailing zeros. Used to work out coordinate
  precision.

#### WildObsR now tells you when it is out of date

The first time you call
[`wildobs_mongo_query()`](https://wildobs.github.io/WildObsR/reference/wildobs_mongo_query.md)
or
[`wildobs_dp_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
in a session, the package checks whether a newer version has been
released and warns you if so. It never stops your work, and it stays
completely silent when you are up to date or when GitHub cannot be
reached.

Attaching the package with
[`library(WildObsR)`](https://wildobs.github.io/WildObsR/) also runs one
quick check that the WildObs database is reachable. It prints nothing
unless something is actually wrong.

The README now explains what major, minor and patch version numbers
mean, so you can judge whether a given update is urgent.

### Internal

None of these change how the package behaves, but they make it install
more reliably:

- **`geojsonsf` added to Imports.** It was already being used but never
  declared, so anyone who did not happen to have it installed hit an
  error.
- **`tidyselect` and `httr2` removed from Imports.** Neither was used
  any more.
- **`utils` added to Imports**, for the version check.
- The `Author` field in DESCRIPTION was malformed and has been replaced
  with a proper `Authors@R` entry including an ORCID.
- A standard R-CMD-check GitHub Actions workflow now runs on every push
  and pull request, across macOS, Windows and Linux.
- `R CMD check` is now clean: no errors and no warnings, down from three
  warnings.
- The test suite grew from 819 to 890 passing tests, including the first
  tests for
  [`rename_or_add_column()`](https://wildobs.github.io/WildObsR/reference/rename_or_add_column.md)
  and
  [`get_decimal_places()`](https://wildobs.github.io/WildObsR/reference/get_decimal_places.md).
