# Validate Data Against CamTrap DP Schemas

**Deprecated as of WildObsR 0.2.0.** Schema validation is no longer part
of the WildObs data intake workflow, and `check_schema()` will be
removed in a future release. The function still runs normally; it only
emits a warning.

This function checks that each camtrapDP resource (i.e. `observations`,
`deployments`, `media`, and `covariates`) conforms to the camtrapDP
specified schema by validating field names, data types, and constraints
(e.g., required fields, numeric ranges). It creates missing fields if
necessary, ensures data types are aligned with the schema, and reports
any discrepancies. The modified dataset is returned with updates
applied.

## Usage

``` r
check_schema(chosen_schema, data)
```

## Arguments

- chosen_schema:

  A list object representing the schema to check against (e.g.,
  `obs_schema`, `deps_schema`, `media_schema`).

- data:

  A data frame containing the dataset to be validated.

## Value

The modified tabular data resource with updated fields and any necessary
corrections applied.

## Details

The function follows these steps:

1.  Loops through each field in the provided schema.

2.  If a field does not exist in the dataset, it is created and filled
    with `NA` values of the appropriate type.

3.  Checks whether the data type of each field matches the expected type
    from the schema (e.g., character, numeric, datetime).

4.  If the field is numeric, it checks whether values fall within
    specified constraints (e.g., minimum, maximum).

5.  Checks that required fields are present and non-missing.

## Author

Tom Bruce & Zachary Amir

## Examples

``` r
# Example usage:
deps_schema <- list(
  fields = list(
    list(name = "locationID", type = "string", constraints = list(required = TRUE)),
    list(name = "longitude", type = "number", constraints = list(minimum = -180, maximum = 180)),
    list(name = "latitude", type = "number", constraints = list(minimum = -90, maximum = 90)),
    list(name = "timestamp", type = "datetime", constraints = list(required = TRUE))
  )
)

deps <- data.frame(
  locationID = c("loc1", "loc2"),
  longitude = c(151.2093, 153.0251),
  latitude = c(-33.8688, NA),
  timestamp = as.POSIXct(c("2023-01-01 12:00", NA))
)

# Check the data against the schema
validated_data <- check_schema(deps_schema, deps)
#> Warning: check_schema() is deprecated as of WildObsR 0.2.0 and will be removed in a future release. Schema validation is no longer part of the WildObs data intake workflow.
#> Checking field: locationID 
#> Checking field: longitude 
#> Checking field: latitude 
#> Checking field: timestamp 
#> Error: Field timestamp is required but contains missing values.
#> Error: Field timestamp is required but contains missing values.
print(validated_data)
#>   locationID longitude latitude           timestamp
#> 1       loc1  151.2093 -33.8688 2023-01-01 12:00:00
#> 2       loc2  153.0251       NA                <NA>
```
