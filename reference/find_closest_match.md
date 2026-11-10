# Find Closest Matches in Two Character Vectors

**Deprecated as of WildObsR 0.2.0.** `find_closest_match()` will be
removed in a future release. The function still runs normally; it emits
a warning once per session (it is commonly passed to
[`sapply()`](https://rdrr.io/r/base/lapply.html), so an unguarded
warning would fire once per element).

This function finds the closest match between species names provided in
an unverified list and a verified species taxonomy database. It handles
differences in spelling or capitalization.

## Usage

``` r
find_closest_match(string, target_strings)
```

## Arguments

- string:

  This is a character vector of species names in the observations that
  have no matching value in the verified species taxonomy database.

- target_strings:

  This is a character vector of the species names in the verified
  species taxonomy database to compare against.

## Value

A data frame containing the original species names and their closest
match from the verified species taxonomy database.

## Details

This function internally uses
[`sapply()`](https://rdrr.io/r/base/lapply.html) to apply string
distance calculations across all species names in the `string` vector.
The closest match for each name is determined using the Jaro-Winkler
string distance method from the `stringdist` package.

## Author

Zachary Amir & ChatGPT
