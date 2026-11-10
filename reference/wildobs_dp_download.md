# Download Data from WildObs' MongoDB and Format as Frictionless Data Packages

This function connects to the WildObs MongoDB server, retrieves
project-specific metadata and data resources, and bundles them into
Frictionless Data Packages formatted using the camtrap DP data standard.
Note, while neither the db_url or api_key parameters are required, the
function will return an error if at least one of these parameters are
not provided.

## Usage

``` r
wildobs_dp_download(
  db_url = NULL,
  api_key = NULL,
  project_ids,
  media = FALSE,
  metadata_only = FALSE
)
```

## Arguments

- db_url:

  A character string specifying the MongoDB connection URI. This should
  follow the format:`'mongodb://username:password@host:port/database'`.
  If `NULL`, the function will check for an API key, and if the
  `api_key` parameter is `NULL`, the function stop with an error. This
  parameter allows users to specify their own connection string to the
  WildObs MongoDB instance. Defaults to `NULL`, in which case the
  function expects a valid `api_key` to connect directly to MongoDB.

- api_key:

  A character string specifying the API key used for authenticated
  access to the WildObs public API. If provided, the function will query
  the API instead of connecting directly to the MongoDB instance. API
  keys grant read-only access to the public WildObs MongoDB database and
  should be kept confidential (e.g., stored in an `.Renviron` file or
  other secure environment variable). You can create your own API key on
  the [WildObs Dashboard](https://dashboard.wildobs.org.au/). Defaults
  to `NULL`, in which case the function expects a valid `db_url` to
  connect directly to MongoDB.

- project_ids:

  A character vector of project IDs to retrieve from MongoDB, which is
  generated from the light-weight
  [`WildObsR::wildobs_mongo_query()`](https://wildobs.github.io/WildObsR/reference/wildobs_mongo_query.md)
  function.

- media:

  A logical (TRUE/FALSE) value to include the media resource in your
  data package download. This is the largest spreadsheet and
  significantly slows down the download process, so this value defaults
  to FALSE.

- metadata_only:

  Logical; if TRUE, only metadata will be retrieved from each data
  package, without downloading the associated data resources. This can
  significantly improve speed when only project-level information is
  required. Defaults to FALSE, in which case all data (metadata and
  tabular resources) are downloaded. Note: this only applies for data
  with 'open' data sharing agreements, since 'partial' will be returned
  with metadata-only.

## Value

A named list of Frictionless Data Packages formatted using camtrap DP,
where each element corresponds to a project. To learn more about
[camtrapDP, click here](https://camtrap-dp.tdwg.org/), and to learn more
about [Frictionless Data Packages, click
here](https://specs.frictionlessdata.io/data-package/). Note that only
data with an 'open' data sharing agreement will return tabular data,
while data shared with a 'partial' data sharing agreement will return
only the project-level metadata.

## Details

The function performs the following steps:

1.  Connects to MongoDB using read-only credentials.

2.  Retrieves project-level metadata and formats it to match the
    Frictionless Data Package structure.

3.  Queries the `deployments`, `observations`, and `media` collections
    for the requested projects.

4.  Applies schemas to ensure data integrity.

5.  Creates Frictionless Data Packages and returns them as a list.

@seealso
[`convert_df_to_list`](https://wildobs.github.io/WildObsR/reference/convert_df_to_list.md)
for formatting metadata,
[`clean_list_recursive`](https://wildobs.github.io/WildObsR/reference/clean_list_recursive.md)
for removing NULL and empty elements,
[`reformat_fields`](https://wildobs.github.io/WildObsR/reference/reformat_fields.md),
[`reformat_schema`](https://wildobs.github.io/WildObsR/reference/reformat_schema.md),
and
[`apply_schema_types`](https://wildobs.github.io/WildObsR/reference/apply_schema_types.md)
for processing and applying schema fields,
[`is_empty_temporal`](https://wildobs.github.io/WildObsR/reference/is_empty_temporal.md)
for removing empty temporal fields.

## Author

Zachary Amir

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage:
# Load the general use WildObs API key
api_key <- Sys.getenv("WILDOBSR_API_KEY")

# Load relevant project ids
project_ids <- c("QLD_Kgari_BIOL2015_2023-24_WildObsID_0004",
                 "QLD_Kgari_potoroos_Amir_2022_WildObsID_0003")

dp_list <- wildobs_dp_download(project_ids)

# Access the data for a specific project
dp_list[["QLD_Kgari_BIOL2015_2023-24_WildObsID_0004"]]
} # }
```
