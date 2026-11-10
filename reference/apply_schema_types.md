# Apply camtrapDP Schemas to Data

This function applies the camtrapDP schema to a frictionless data
package by converting column types according to the schema definition.
It includes flexible datetime parsing to accommodate multiple common
formats.

## Usage

``` r
apply_schema_types(data, schema, timezone = "UTC")
```

## Arguments

- data:

  A dataframe representing a resource (e.g., observations, deployments).

- schema:

  A list representing the schema that defines the expected column types
  and constraints for the dataset.

- timezone:

  A character string specifying the timezone for datetime conversions.
  Defaults to "UTC". This parameter is particularly important for
  camtrap data to ensure temporal alignment across deployments.

## Value

A dataframe with columns transformed according to the schema.

## Details

The function converts columns based on the `type` field in the schema:

- `"datetime"`: Attempts to parse using multiple common formats and
  maintains as POSIXct with specified timezone.

- `"date"`: Converts to `Date` class.

- `"integer"`: Converts to integer.

- `"number"`: Converts to numeric.

- `"boolean"`: Converts logical-like strings (e.g., 'TRUE', 'FALSE',
  'T', 'F') to logical.

- `"string"`: Converts to character, optionally factoring if an enum
  constraint is present.

- `"factor"`: Converts to factor.

- `"any"`: Left unchanged, since Frictionless allows any type (e.g.
  Camtrap DP's `exifData`).

If an unknown field type is encountered, a warning is issued.

## Author

Zachary Amir & ChatGPT
