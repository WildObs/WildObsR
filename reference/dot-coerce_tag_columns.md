# Attempt Type Coercion for Expanded Tag Columns

Internal helper for
[`extract_tags`](https://wildobs.github.io/WildObsR/reference/extract_tags.md).
Iterates over each character column and attempts to coerce it to a more
specific type in priority order: numeric → logical → date-time (ISO
8601). Coercion is only applied when it does not introduce new `NA`
values. If all coercion attempts fail, the column is retained as
character and a [`warning()`](https://rdrr.io/r/base/warning.html) is
emitted identifying the column name and the values that prevented
coercion.

## Usage

``` r
.coerce_tag_columns(df)
```

## Arguments

- df:

  A dataframe of character columns (the expanded tag columns).

## Value

The same dataframe with columns coerced where it is safe to do so.
