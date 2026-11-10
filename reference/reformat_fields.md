# Reformat Schema Fields for Frictionless Data Package

This function is used internally in the wildobs_dp_download() function
to take a set of schema fields and reformats them to ensure proper
structure for use in a Frictionless Data Package. It processes
constraints dynamically (including `pattern`), removing `NULL` and `NA`
values, and retains relevant metadata such as descriptions, units,
formats, and the WildObs `custom` block (each covariate's source
citation and spatial resolution).

## Usage

``` r
reformat_fields(fields)
```

## Arguments

- fields:

  A dataframe containing schema field information, including
  constraints, descriptions, and other metadata.

## Value

A list of formatted schema fields, with cleaned constraints and
metadata.

## See also

[`wildobs_dp_download`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
for downloading and bundling data packages.

[`clean_list_recursive`](https://wildobs.github.io/WildObsR/reference/clean_list_recursive.md)
for removing empty list elements.

## Author

Zachary Amir & ChatGPT
