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
