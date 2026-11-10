# Convert a WildObs Data Package to Canonical Camtrap DP

What counts as "canonical" is read from the official TDWG Camtrap DP
profile and table schemas vendored in the package (`inst/profiles/`),
not from a hard-coded list. So the function stays complete as WildObs
adds extensions, and new Camtrap DP versions only need their files
vendored. The WildObs flavour of Camtrap DP (also vendored) is used to
label each removal as an expected WildObs extension or as something
unexpected.

## Usage

``` r
as_camtrapdp(
  dp,
  version = "1.0.2",
  keep_covariates = FALSE,
  validate = TRUE,
  warn = c("important", "all", "none")
)
```

## Arguments

- dp:

  A WildObs data package, as returned (one element of the list) by
  [`wildobs_dp_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md).

- version:

  Character string. The Camtrap DP version to convert to: `"1.0.2"`
  (default) or `"1.0.1"`. Reading 1.0.2 with camtrapdp needs camtrapdp
  \>= 0.5.0.

- keep_covariates:

  Logical. Keep the WildObs `covariates` table? Defaults to `FALSE`.

- validate:

  Logical. Check the result after converting? Validates the descriptor
  against the target profile (needs jsonvalidate) and checks that
  [`camtrapdp::read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.html)
  and
  [`camtrapdp::check_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/check_camtrapdp.html)
  accept it (needs camtrapdp). Defaults to `TRUE`; skipped with a
  warning when a package is missing.

- warn:

  Character string. `"important"` (default) warns when information with
  no Camtrap DP home is lost: a relationship between tables
  (`media$observationID`), a populated `covariates` table, removed media
  rows, or a failed validation. `"all"` also summarises every removal in
  one warning. `"none"` never warns; everything is still in the report.

## Value

A list with four elements:

- package:

  The converted data package, of the same class as `dp`. Save it with
  [`frictionless::write_package()`](https://docs.ropensci.org/frictionless/reference/write_package.html).
  Its tables are in `resources`, so the camtrapdp print method reports 0
  tables; use `camtrapdp` below with that package's functions.

- camtrapdp:

  The same package read back with
  [`camtrapdp::read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.html),
  ready for camtrapdp functions such as
  [`camtrapdp::write_dwc()`](https://inbo.github.io/camtrapdp/reference/write_dwc.html).
  `NULL` if camtrapdp is not installed or could not read it (see
  `validation`).

- report:

  A data frame with one row per change: `level` (`package`, `resource`,
  `field`, `value` or `row`), `path`, `action`, `n` (how many values or
  rows), `origin`, and `note`. `origin` is `WildObs extension` when the
  WildObs flavour of Camtrap DP declares what was changed,
  `not in WildObs profile` for anything else removed (e.g.
  `projectName`, which
  [`wildobs_dp_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
  adds), `empty value` for dropped `NA`s, and `conversion` for steps the
  conversion itself takes (repointing schemas, removing rows that break
  a pattern).

- validation:

  `NULL` when `validate = FALSE`, otherwise a list with `descriptor` and
  `camtrapdp`, each holding `valid` (logical) and `messages`
  (character).

Stops if `dp` is not a data package with tables in memory, or `version`
is not vendored.

## Details

Removes every WildObs addition from a data package so it matches a
published Camtrap DP version exactly, and can be read by standard
consumers such as the camtrapdp R package, GBIF (via
[`camtrapdp::write_dwc()`](https://inbo.github.io/camtrapdp/reference/write_dwc.html))
and Agouti. Returns the converted package with a report of every change.

The conversion works in this order:

1.  **Descriptor**: every key the target profile does not define is
    removed, at any depth, e.g. `WildObsMetadata`,
    `versionControlWildObs`, `project$DPID`, `contributors[]$ROR`, and
    the per-deploymentGroup blocks and `timeZone` in `temporal`. Empty
    (`NA`) values are dropped too, since Camtrap DP treats a missing
    property as absent rather than null. `profile` and `version` are
    pointed at the target version. `spatial` is left untouched: it is
    GeoJSON, which allows extra members.

2.  **Tables**: `deployments`, `media` and `observations` keep only the
    target schema's fields, in the target order, and their inline
    schemas are replaced with the TDWG table-schema URL. The values in
    every kept column are unchanged. The `covariates` table is removed
    unless `keep_covariates = TRUE` (Camtrap DP allows extra tables, so
    keeping it still validates).

Four value changes are made, because the values would otherwise fail
validation or break camtrapdp functions:

- `taxonomic[]$vernacularNamesEnglish` becomes `vernacularNames$eng`,
  the Camtrap DP form, so common names are kept.

- A `taxonRank` outside the Camtrap DP list (e.g. `subclass`) is rounded
  up to the nearest broader rank in it (e.g. `class`), so a record never
  claims more precision than its identification had.

- A `relatedIdentifierType` of `RAiD`, which DataCite does not list,
  becomes `Handle`.

- Media rows whose `filePath` or `fileMediatype` break the Camtrap DP
  pattern (e.g. a path on a contributor's own computer) are removed, and
  any observation whose `mediaID` pointed at one has its `mediaID` set
  to `NA`. This can empty the media table, in which case a warning says
  so.

The input must hold all three Camtrap DP tables in memory, as packages
from `wildobs_dp_download(..., media = TRUE)` do; a package without one
stops with an explanation. The original `dp` is never modified. The
result keeps its tables in `resources` only (read them with
[`frictionless::read_resource()`](https://docs.ropensci.org/frictionless/reference/read_resource.html)),
without the extra `dp$data` copy, so
[`frictionless::write_package()`](https://docs.ropensci.org/frictionless/reference/write_package.html)
writes a clean Camtrap DP package.

## See also

[`wildobs_dp_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md);
the Camtrap DP standard at <https://camtrap-dp.tdwg.org/>.

## Author

Zachary Amir & Claude Opus 5.5

## Examples

``` r
if (FALSE) { # \dontrun{
api_key <- Sys.getenv("WILDOBSR_API_KEY")
dp <- wildobs_dp_download(api_key = api_key,
                          project_ids = "ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001")[[1]]

# Convert to canonical Camtrap DP 1.0.2
out <- as_camtrapdp(dp)

# What was changed, and whether the result validates
out$report
out$validation

# Use it with the camtrapdp package, e.g. to export Darwin Core for GBIF
camtrapdp::write_dwc(out$camtrapdp, "dwc_export")

# Or save it as a Camtrap DP package on disk
frictionless::write_package(out$package, "camtrapdp_export")
} # }
```
