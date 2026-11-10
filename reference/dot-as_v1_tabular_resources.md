# Keep a package's tables as Data Package v1 tabular resources

Camtrap DP 1.0.x, and the WildObs profile built on it, follow the Data
Package v1 specification, which requires every table to declare
`profile = "tabular-data-resource"`. From frictionless 2.0.0,
[`frictionless::add_resource()`](https://docs.ropensci.org/frictionless/reference/add_resource.html)
writes a Data Package v2 resource instead: it sets `$schema` and
`type = "table"` and leaves `profile` out, so a Camtrap DP package built
with it no longer matches its own profile. This helper puts each
resource that has a table schema back in the v1 form. Resources already
in that form, as frictionless 1.x writes them, are returned unchanged.

## Usage

``` r
.as_v1_tabular_resources(dp)
```

## Arguments

- dp:

  A Frictionless data package (a list with `resources`).

## Value

The same data package, with `$schema` and `type` removed from each
tabular resource and `profile` set to `"tabular-data-resource"`.

## Author

Zachary Amir
