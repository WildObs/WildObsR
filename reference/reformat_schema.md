# Reformat a Schema for a Frictionless Data Package

This function is used internally in the wildobs_dp_download() function
to transform a schema into a properly structured list, ensuring
compatibility with the Frictionless Data Package format. It processes
fields using
[`reformat_fields`](https://wildobs.github.io/WildObsR/reference/reformat_fields.md)
to maintain correct metadata structure.

## Usage

``` r
reformat_schema(schema)
```

## Arguments

- schema:

  A list containing schema metadata, typically extracted from a
  Frictionless Data Package.

## Value

A formatted list representing the schema, including name, title,
description, fields, missing values, primary key, and foreign keys.

## See also

[`wildobs_dp_download`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
for downloading and bundling data packages.

[`reformat_fields`](https://wildobs.github.io/WildObsR/reference/reformat_fields.md)
for structuring field metadata,
[`wildobs_dp_download`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
for downloading and bundling data packages.

## Author

Zachary Amir & ChatGPT
