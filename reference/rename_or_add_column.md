# Rename or Add a Column

Renames an existing column, or creates a new all-`NA` column when no old
column name is supplied. Used throughout the WildObs mobilisation
pipeline to coerce incoming spreadsheets into Camtrap DP column names.

## Usage

``` r
rename_or_add_column(df, new_name, old_name)
```

## Arguments

- df:

  A data frame to modify.

- new_name:

  Character string. The name the column should end up with. This is the
  **second** argument.

- old_name:

  Character string. The existing column to rename. This is the **third**
  argument. If `NA`, `""`, or length zero, a new all-`NA` column is
  created instead of renaming.

## Value

A data frame with the column renamed or added. The number of rows is
always unchanged.

## Details

**The argument order is `df`, `new_name`, `old_name` — the new name
comes before the old name.** Some collaborator markdowns define their
own inline copy of this helper using the opposite order. Those local
copies shadow this one and are unaffected, but if you delete a local
copy in favour of the package version, check the order at every call
site before assuming it still works.

The function branches on `old_name`:

1.  If `old_name` is `NA`, an empty string, or length zero, a new column
    named `new_name` is appended and filled with `NA`.

2.  Otherwise the column currently named `old_name` is renamed to
    `new_name`.

3.  If no column matches `old_name`, the data frame is returned
    unchanged.

No collision check is performed. If `new_name` already names a different
column, renaming leaves two columns sharing that name.

## Author

Zachary Amir

## Examples

``` r
df <- data.frame(lat = c(-27.5, -28.1), lon = c(153.0, 152.8))

# Rename an existing column: new name second, old name third
rename_or_add_column(df, "latitude", "lat")
#>   latitude   lon
#> 1    -27.5 153.0
#> 2    -28.1 152.8

# Passing "" as old_name adds a new all-NA column instead
rename_or_add_column(df, "coordinateUncertainty", "")
#>     lat   lon coordinateUncertainty
#> 1 -27.5 153.0                    NA
#> 2 -28.1 152.8                    NA

# An old_name that matches nothing returns the data frame untouched
rename_or_add_column(df, "latitude", "not_a_column")
#>     lat   lon
#> 1 -27.5 153.0
#> 2 -28.1 152.8
```
