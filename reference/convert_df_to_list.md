# Convert a Data Frame to a Nested List

This function is used internally in the wildobs_dp_download() function
to convert a data frame into a nested list, ensuring that vector fields
are flattened while preserving list-like structures.

## Usage

``` r
convert_df_to_list(df)
```

## Arguments

- df:

  A data frame or a list containing a data frame.

## Value

A list representation of the input data frame. If the input is a
single-row data frame, it returns a named list. If the input is a list
containing a data frame, it returns a list where each row is represented
as a nested list.

## Details

The function performs the following transformations:

- Converts the first row of a data frame into a named list.

- If the input is a list containing a data frame, it processes each row
  separately and returns a list of lists.

- Ensures that vector fields (e.g., `path`, `deploymentGroups`) are
  flattened to prevent unnecessary nested lists.

- Stops execution if the input is neither a data frame nor a list
  containing a data frame.

## See also

[`wildobs_dp_download`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
for downloading and bundling data packages.

## Author

Zachary Amir & ChatGPT
