# WildObsR 0.3.0

The WildObs database now follows the Camtrap DP standard more closely, and
earlier versions of WildObsR cannot read it: `wildobs_dp_download()` stops with
`incorrect number of dimensions`. **Update WildObsR to download data again.**

```r
devtools::install_github("WildObs/WildObsR")
```

## Breaking changes

### `dp$temporal` keeps the database's shape

Downloaded packages now store `temporal` exactly as the database does: the
package-level `start`, `end` and `timeZone` once at the top, then one
`{start, end}` block per deploymentGroup. Previously `timeZone` was copied into
every group. Code that read `dp$temporal[[1]]$timeZone` should read
`dp$temporal$timeZone` instead, or use `extract_metadata(dp, "temporal")`, which
handles both shapes.

### Spatial functions no longer take a shapefile path

`ibra_classification()`, `locationName_verification_CAPAD()` and
`locationName_buffer_CAPAD()` now use the IBRA7 and CAPAD 2022 layers that ship
with the package, so they work on any computer. The `ibra_file_path` and
`capad_file_path` arguments are gone: remove them from your calls. The layers
are available directly as the datasets `ibra` and `capad` (#11, #122).

`locationName_verification_CAPAD()` now measures `CAPADminDistance` in true metres.
Previously it measured in web Mercator units, which overstate distances in
Australia by roughly 10–40% depending on latitude, so values will be smaller
than before and the 1–5 km / 5–10 km / >10 km notes may change category.

### `wildobs_mongo_query()` returns an empty vector when nothing matches

When no project matches, `wildobs_mongo_query()` now returns `character(0)` rather
than `""`, still with a warning. `length(result) > 0` is now `FALSE`, and loops over
the result no longer run once with a blank ID. Code that tested
`result == ""` should test `length(result) == 0` instead. `wildobs_dp_download()`
stops with a clear message when given no IDs, including the old `""` (#129).

### `dp$sources` is a list of sources

`sources` is now an array of source objects, as Camtrap DP specifies, so
`dp$sources[[1]]$title` replaces `dp$sources$title`.
`extract_metadata(dp, "sources")` returns one row per source.

## New

- `wildobs_media_download()` downloads the image files listed in a media table into
  `out_dir/<projectName>/<deploymentID>/<mediaID>.<ext>`, and reports what happened
  to each file. Publicly hosted files (`filePublic = TRUE`) download for anyone;
  files on your own computer are copied; private Google Cloud files can be fetched
  with a `gcs_token` if you have access. Re-running fetches only what is missing.
- `install_claude_skill()` installs `wildobsr-data`, a Claude skill describing how
  WildObs data packages are structured and what the data can support, so Claude can
  help with your analysis. See "Help Claude understand WildObs data" in the README.
- `extract_metadata(dp, "temporal")` gains `packageStart` and `packageEnd`
  columns holding the package-level temporal extent (#137).
- Downloaded packages now include `versionControlWildObs`, the WildObs version
  of the package.
- Covariate fields in downloaded schemas now carry a `custom` block with the
  source citation and resolution of the spatial product behind each covariate,
  and media fields keep their `pattern` constraints.

## Fixed

- `wildobs_dp_download()` skips project IDs it cannot find, with one warning naming
  them, and downloads the rest. It stops only when none are found. Previously an
  unknown ID failed with "subscript out of bounds" (#129).
- `apply_schema_types()` gives empty `datetime` columns the `POSIXct` type, and
  converts partly empty ones. Previously a column with any empty cell was treated as
  unparseable and left as text, or as `logical` if wholly empty (#129).
- `apply_schema_types()` no longer deletes a `datetime` column whose schema field has
  no `format`: it falls back to ISO 8601, and leaves the column unchanged with a
  warning if it still cannot parse it (#129).
- `wildobs_dp_download()` no longer hides warnings from typing the tables, so a
  column that fails to convert is reported rather than passed on silently (#129).
- `ibra_classification()` no longer drops locations that fall just outside every
  IBRA subregion. They now take the IBRA values of their nearest matched
  location, as documented (#97).
- `locationName_buffer_CAPAD()` generates UTM coordinates when they are missing.
  Its check matched any column name containing "x" or "y", including
  `deploymentID`, so it always skipped generation and stopped with
  "No unique UTM zones found".
- `locationName_verification_CAPAD()` no longer leaves its internal `ID`, `lat2`
  and `long2` columns in the output.
- `wildobs_dp_download()` reads the updated database structure.
- `wildobs_mongo_query(temporal = ...)` no longer errors on the package-level
  temporal extent, and still matches projects on when each deploymentGroup ran.
- `extract_metadata()` still reads data packages saved by earlier versions.

## Internal

- The update notice now only appears for a new major or minor release, not for
  patch releases.

# WildObsR 0.2.0

This is a breaking release. Some functions you may have called directly are no
longer available, and the bundled API key has been removed. The three changes
most likely to affect you are listed first, each with what you need to do.

## Breaking changes

### The bundled API key is gone — create your own

Earlier versions shipped a shared API key as a dataset called `wildobsr_api_key`.
That dataset has been removed and the key it contained no longer works.

**What to do:** create a personal API key on the
[WildObs Dashboard](https://dashboard.wildobs.org.au/), store it in your
`.Renviron` file, and read it in R with:

```r
api_key <- Sys.getenv("WILDOBSR_API_KEY")
```

Keys are tied to you personally rather than to a project. The README has a
step-by-step walkthrough under "Getting Database Access". Treat your key like a
password: do not paste it into a script that you share or commit.

### Nine helper functions are now internal

These were previously callable with `WildObsR::function_name()`. They are still
in the package and still used by it, but they are no longer part of the public
interface, because they are helpers for other functions rather than tools meant
to be run on their own:

`clean_list_recursive()`, `convert_df_to_list()`, `extract_classif()`,
`find_closest_match()`, `is_empty_spatial()`, `is_empty_temporal()`,
`long_to_UTM_zone()`, `reformat_fields()`, `reformat_schema()`

**What to do:** if a script of yours calls one of these with `WildObsR::`, it will
now fail. Most of them were only ever used inside other WildObsR functions, so in
practice you probably do not call them at all. If you genuinely need one, it can
still be reached with three colons, as in `WildObsR:::convert_df_to_list()`, but
that is a stopgap rather than a promise: internal functions can change or
disappear without notice. Tell us which one you need and we will consider making
it public, as we did this release for the two functions listed under **New** below.

Two of the nine have changed further since: `is_empty_spatial()` has been removed
outright, and `find_closest_match()` is deprecated. Both are covered below.

### MongoDB document helpers removed

`mongo_clean_df()`, `mongo_format_dates()` and `mongo_prepare_doc()` have been
removed. All three prepared data for *writing into* the WildObs database, which is
not something this package does — it only reads. They now live in the private
repository used to run database updates.

**What to do:** nothing, unless you were calling them directly, which would only
be the case if you help maintain the database itself. Downloading and querying are
unaffected.

## Deprecated

Both still work exactly as before, and both now print a warning the first time you
use them in a session. They will be removed in a future release.

- **`check_schema()`** has been retired from the data intake workflow. Schema
  validation now happens inside the download itself, so you no longer need to run
  this as a separate step.
- **`find_closest_match()`** is no longer used by the package. It warns only once
  per session rather than once per call, because it is often passed to `sapply()`
  where a per-call warning would flood your console.

**What to do:** if either appears in a script, plan to remove the call. Nothing
breaks today.

## Removed

- **`gbif_check()`** — had no callers anywhere in the package or in our other
  repositories.
- **`is_empty_spatial()`** — had no callers and no known external usage.
  `is_empty_temporal()` is unaffected and still works.

## New

### Two helpers are now public

Both were already being used from other WildObs projects, so they have been given
proper documentation, examples, and unit tests, and are now supported:

- **`rename_or_add_column(df, new_name, old_name)`** renames a column, or adds a
  new all-`NA` column if you pass `""`, `NA`, or nothing as the old name.
  **Note the argument order: the new name comes before the old name.** Some
  mobilisation markdowns define their own copy of this function with the arguments
  the other way round. Those local copies still take precedence in your own script,
  so nothing changes for you until you delete one — at which point check the order
  at every call site.
- **`get_decimal_places(x)`** counts significant decimal places in a number,
  ignoring trailing zeros. Used to work out coordinate precision.

### WildObsR now tells you when it is out of date

The first time you call `wildobs_mongo_query()` or `wildobs_dp_download()` in a
session, the package checks whether a newer version has been released and warns
you if so. It never stops your work, and it stays completely silent when you are
up to date or when GitHub cannot be reached.

Attaching the package with `library(WildObsR)` also runs one quick check that the
WildObs database is reachable. It prints nothing unless something is actually
wrong.

The README now explains what major, minor and patch version numbers mean, so you
can judge whether a given update is urgent.

## Internal

None of these change how the package behaves, but they make it install more
reliably:

- **`geojsonsf` added to Imports.** It was already being used but never declared,
  so anyone who did not happen to have it installed hit an error.
- **`tidyselect` and `httr2` removed from Imports.** Neither was used any more.
- **`utils` added to Imports**, for the version check.
- The `Author` field in DESCRIPTION was malformed and has been replaced with a
  proper `Authors@R` entry including an ORCID.
- A standard R-CMD-check GitHub Actions workflow now runs on every push and pull
  request, across macOS, Windows and Linux.
- `R CMD check` is now clean: no errors and no warnings, down from three warnings.
- The test suite grew from 819 to 890 passing tests, including the first tests for
  `rename_or_add_column()` and `get_decimal_places()`.
