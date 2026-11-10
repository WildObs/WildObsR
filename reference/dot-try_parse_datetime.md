# Try to Parse a Character Vector as Date-Time

Internal helper that attempts to parse a character vector using a list
of date-time format strings. Returns a `POSIXct` vector if one of the
formats parses all non-NA values cleanly, or `NULL` if no format
succeeds.

## Usage

``` r
.try_parse_datetime(x, formats)
```

## Arguments

- x:

  A character vector.

- formats:

  A character vector of format strings (as accepted by
  [`strptime`](https://rdrr.io/r/base/strptime.html)).

## Value

A `POSIXct` vector (UTC) if parsing succeeds, or `NULL`.
