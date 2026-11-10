# Extract Tag Key:Value Pairs into Individual Columns

Parses pipe-delimited `key:value` tag columns (such as `deploymentTags`
or `observationTags`) in a Camtrap DP-formatted dataframe and expands
each unique key into its own column, appended to the right-hand side of
the input dataframe.

## Usage

``` r
extract_tags(df, col = NULL, keep_original = TRUE)
```

## Arguments

- df:

  A dataframe containing a tags column. Typically a Camtrap DP
  `deployments` or `observations` table.

- col:

  Optional character string naming the tags column to parse. If `NULL`
  (the default), the function auto-detects by looking for a column named
  `deploymentTags` or `observationTags`. If neither or both are found
  and `col` is `NULL`, an informative error is thrown asking the user to
  specify `col` explicitly. A dataframe containing both columns is
  non-standard Camtrap DP; a warning is issued even when `col` is
  specified.

- keep_original:

  Logical. Default `TRUE`. If `TRUE`, the original tags column is
  retained in the output. If `FALSE`, it is dropped.

## Value

The input dataframe with new tag columns appended at the right-hand
side. The original column order is preserved. Each unique key found
across all tag strings becomes a new column. Rows with no tags (NA or
empty string) receive `NA` in all new tag columns. Literal `"NA"` values
within tag strings are converted to true `NA`. Each new column is
auto-coerced to the most specific type possible (numeric → logical →
date-time); if coercion would introduce new `NA` values beyond those
already present, the column is retained as character and a
[`warning()`](https://rdrr.io/r/base/warning.html) is emitted
identifying the column name and the values that prevented coercion.

## Details

Tag strings are expected to follow the pipe-delimited key:value format
used by Camtrap DP, for example:

    "temperature:13 | moonphase:New_Crescent"

Pairs are split first on `" | "` (space-pipe-space) and then on the
first `":"` to separate key from value. Only the first colon is used as
the delimiter, so values may themselves contain colons (e.g. datetime
strings).

Type coercion is attempted in order: numeric, then logical, then
date-time (ISO 8601 formats). Coercion is only applied if it does not
introduce new `NA` values beyond those already present in the column.

## Author

Zachary Amir & Claude Opus 4.5

## Examples

``` r
# deployments of the Woomargama wombat project shipped with WildObsR
dp <- example_datapackages("NSW_Woomargama_wombats_Linley_2021-22_WildObsID_0010")[[1]]
dep <- frictionless::read_resource(dp, "deployments")

# deploymentTags holds "key: value" pairs, e.g. the lure used and predator management
head(dep$deploymentTags, 3)
#> [1] "lure: none | predatorManagement: Posion bait"
#> [2] "lure: none | predatorManagement: Posion bait"
#> [3] "lure: none | predatorManagement: Posion bait"

# expand each key into its own column
dep_expanded <- extract_tags(dep)
#> Warning: Column 'lure' could not be safely coerced from character; retaining as character. Values that prevented numeric coercion: none
#> Warning: Column 'predatorManagement' could not be safely coerced from character; retaining as character. Values that prevented numeric coercion: Posion bait
head(dep_expanded[, c("deploymentID", "lure", "predatorManagement")], 3)
#> # A tibble: 3 × 3
#>   deploymentID                  lure  predatorManagement
#>   <chr>                         <chr> <chr>             
#> 1 10_WildObsID_0010_2021_a_Cam1 none  Posion bait       
#> 2 10_WildObsID_0010_2021_a_Cam2 none  Posion bait       
#> 3 10_WildObsID_0010_2021_b_Cam1 none  Posion bait       

# name the column explicitly, and drop the original tags
dep_expanded <- extract_tags(dep, col = "deploymentTags", keep_original = FALSE)
#> Warning: Column 'lure' could not be safely coerced from character; retaining as character. Values that prevented numeric coercion: none
#> Warning: Column 'predatorManagement' could not be safely coerced from character; retaining as character. Values that prevented numeric coercion: Posion bait
```
