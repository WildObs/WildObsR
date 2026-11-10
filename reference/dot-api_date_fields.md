# Read MongoDB Dates in an API Response as UTC Date-times

The WildObs API sends date-times as MongoDB extended JSON,
`{"$date": "2022-12-20T20:32:18Z"}`. jsonlite converts these itself, but
when a field in one response mixes dates with nulls (an optional field
such as `classificationTimestamp`) it reads the dates in the computer's
local time zone instead of UTC, shifting every one of them by the local
UTC offset. So the dates are unwrapped to plain text before parsing, and
converted here instead. `.api_date_fields()` lists the fields sent as
dates, `.unwrap_api_dates()` turns each date object into its text, and
`.api_dates_to_utc()` converts those fields of the parsed records to
POSIXct in UTC.

## Usage

``` r
.api_date_fields(txt)

.unwrap_api_dates(txt)

.api_dates_to_utc(records, date_fields)
```

## Arguments

- txt:

  Character. The raw JSON text of an API response.

- records:

  The records parsed from the unwrapped text, usually a data frame.

- date_fields:

  Character. The fields sent as dates, from `.api_date_fields()`.

## Value

`.api_date_fields()`: the field names. `.unwrap_api_dates()`: the JSON
text with each date object replaced by its date text.
`.api_dates_to_utc()`: the records, with each top-level date field as
POSIXct in UTC. A field holding text that is not a UTC date is left as
text, for
[`apply_schema_types()`](https://wildobs.github.io/WildObsR/reference/apply_schema_types.md)
to handle.

## Author

Zachary Amir
