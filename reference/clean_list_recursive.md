# Recursively Clean a List by Removing NULL, NA, and Empty Lists

This function is used internally in the wildobs_dp_download() function
to recursively traverses a list and removes any elements that are:

- `NULL`

- `NA`

- Empty lists ([`list()`](https://rdrr.io/r/base/list.html))

## Usage

``` r
clean_list_recursive(x)
```

## Arguments

- x:

  A nested list containing various data types.

## Value

A cleaned version of the list with all `NULL`, `NA`, and empty lists
removed.

## See also

[`wildobs_dp_download`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
for downloading and bundling data packages.

## Author

Zachary Amir & ChatGPT
