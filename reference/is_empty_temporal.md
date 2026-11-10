# Check if Temporal Data is Empty

This function is used internally in the wildobs_dp_download() function
to check whether a temporal dataset (e.g., a dataframe or vector)
contains only `NA` values, indicating it is empty.

## Usage

``` r
is_empty_temporal(x)
```

## Arguments

- x:

  A dataframe or atomic vector representing temporal data.

## Value

`TRUE` if the temporal data is entirely `NA`, otherwise `FALSE`.

## See also

[`wildobs_dp_download`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
for downloading and bundling data packages.

## Author

Zachary Amir & ChatGPT
