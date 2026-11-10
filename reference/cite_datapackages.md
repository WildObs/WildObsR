# Cite the Camera Trap Data Packages Used in an Analysis

Builds a reference for each data package in a list, ready to paste into
a manuscript, import into a reference manager, or keep as a table. Works
on any Camtrap DP package, from WildObs or elsewhere:

- When a package carries a `bibliographicCitation`, that text is the
  reference, word for word, because it is the one the data providers
  asked for. WildObs citations end with the project's RAiD.

- Otherwise the reference is built from the Camtrap DP descriptor:
  authors from `contributors` (people with the role `contact`,
  `principalInvestigator` or `contributor`, or no role; never
  `rightsHolder`, `publisher` or `funder`), the year `created`, `title`,
  and `homepage`, else `id`.

With `include_version = TRUE` the version of the data is appended: for
WildObs packages the data package version (`versionControlWildObs`),
which is specific to each project and rises whenever WildObs corrects
its data; otherwise the package `version`, unless it is only the Camtrap
DP standard version named in `profile` or the citation already gives it.

With `covariates = TRUE`, the products behind the WildObs covariates
(e.g. the Forest Landscape Integrity Index or the human footprint) are
cited too, once each. They are read from the `custom$source$citation` of
each field in the covariates table schema, so only packages downloaded
with their tables have them.

## Usage

``` r
cite_datapackages(
  dp_list,
  format = c("text", "bibtex", "data.frame"),
  sort_by = c("author", "year", "none"),
  include_version = TRUE,
  accessed = NULL,
  covariates = FALSE
)
```

## Arguments

- dp_list:

  A list of data packages, such as the result of
  [`wildobs_dp_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md),
  or a single data package. Packages downloaded with
  `metadata_only = TRUE` can be cited too.

- format:

  The form of the result: `"text"` (default) for one formatted reference
  per package, `"bibtex"` for a
  [`utils::bibentry()`](https://rdrr.io/r/utils/bibentry.html) object
  that [`utils::toBibtex()`](https://rdrr.io/r/utils/toLatex.html)
  writes as BibTeX, or `"data.frame"` for one row per reference.

- sort_by:

  How to order the references: `"author"` (default, alphabetically by
  the first author's family name, as most reference lists are), `"year"`
  (oldest first, then by author), or `"none"` (the order of `dp_list`).

- include_version:

  Logical. Append the version of the data to each reference? Defaults to
  `TRUE`, because a project's data can be corrected between downloads.

- accessed:

  Optional date the data were downloaded, as a `Date` or a
  `"YYYY-MM-DD"` string, appended as "Accessed YYYY-MM-DD.". Defaults to
  `NULL` (left out).

- covariates:

  Logical. Also cite the products behind the covariates table? Defaults
  to `FALSE`.

## Value

Depends on `format`:

- `"text"`:

  A character vector of references, named by package `id`. The covariate
  product citations, if asked for, follow, each named by the covariates
  it is behind, without their spatial scale (e.g. `"FLII"`, or
  `"mean_monthly_precipitation; mean_monthly_temperature"` for a product
  behind several).

- `"bibtex"`:

  A [`utils::bibentry()`](https://rdrr.io/r/utils/bibentry.html) object:
  one `@misc` entry per package, keyed by package `id`, plus one per
  covariate product, keyed by its first covariate.

- `"data.frame"`:

  One row per reference, with columns `type` (`"data package"` or
  `"covariate product"`), `id`, `authors`, `year`, `title`, `version`,
  `identifier` (RAiD or DOI where there is one), `url` (the identifier,
  else the package `homepage` or `id`), `licenses` and `citation`.

Stops if `dp_list` holds something that is not a data package, or if a
package has neither a `bibliographicCitation` nor a `title` to build one
from.

## See also

[`wildobs_dp_download()`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
to download the packages;
[`extract_metadata()`](https://wildobs.github.io/WildObsR/reference/extract_metadata.md)
for their contributors, licences and related identifiers as tables.

## Author

Zachary Amir

## Examples

``` r
# any Camtrap DP package can be cited, here the camtrapdp package's example
if (requireNamespace("camtrapdp", quietly = TRUE)) {
  x <- camtrapdp::example_dataset()
  cite_datapackages(x)
  # as a table, with the date the data were accessed
  cite_datapackages(x, format = "data.frame", accessed = "2026-10-15")
  # or as BibTeX for a reference manager
  utils::toBibtex(cite_datapackages(x, format = "bibtex"))
}
#> @Misc{7cca70f5-ef8c-4f86-85fb-8f070937d7ab,
#>   title = {Sample from: MICA - Muskrat and coypu camera trap observations in Belgium, the Netherlands and Germany},
#>   author = {P Desmet and A Neukermans and D {Van der beeck} and E Cartuyvels},
#>   year = {2022},
#>   note = {Licence: CC0-1.0; http://creativecommons.org/licenses/by/4.0/},
#>   url = {https://camtrap-dp.tdwg.org/example/},
#> }

if (FALSE) { # \dontrun{
# cite every WildObs project downloaded for an analysis, and the covariate products
api_key <- Sys.getenv("WILDOBSR_API_KEY")
wet_tropics <- list(xmin = 144.9, xmax = 146.5, ymin = -19.6, ymax = -15.3)
ids <- wildobs_mongo_query(api_key = api_key, spatial = wet_tropics,
                           tabularSharingPreference = "open")
dp_list <- wildobs_dp_download(api_key = api_key, project_ids = ids)
writeLines(cite_datapackages(dp_list, covariates = TRUE, accessed = Sys.Date()))
} # }
```
