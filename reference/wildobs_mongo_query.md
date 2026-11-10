# Query WildObs MongoDB for Relevant Project IDs

This function queries the WildObs MongoDB database for projects matching
specified spatial, temporal, taxonomic, contributor, and data-sharing
criteria. It extracts metadata from the database and filters projects
based on bounding box overlaps, temporal overlaps, species detected,
contributors associated, and data sharing preferences. Embargoes are not
checked here: a project under embargo is stored with a 'partial' sharing
preference until the embargo ends and the database is next refreshed, so
it is found by this function but downloads as metadata only. Only admin
credentials in the `db_url` parameter return projects with a 'closed'
sharing preference.

## Usage

``` r
wildobs_mongo_query(
  db_url = NULL,
  api_key = NULL,
  spatial = NULL,
  temporal = NULL,
  taxonomic = NULL,
  samplingDesign = NULL,
  contributors = NULL,
  tabularSharingPreference = c("open", "partial")
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
  function expects a valid `api_key` and queries the WildObs API
  instead.

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

- spatial:

  A named list specifying spatial query parameters, including:

  xmin

  :   Minimum longitude value.

  xmax

  :   Maximum longitude value.

  ymin

  :   Minimum latitude value.

  ymax

  :   Maximum latitude value.

- temporal:

  A named list specifying temporal query parameters, including:

  minDate

  :   Earliest allowable date as a `Date` object.

  maxDate

  :   Latest allowable date as a `Date` object.

- taxonomic:

  A vector of species names in binomial nomenclature (i,e., Latin
  names), and all projects that detect any of the species listed will be
  returned.

- samplingDesign:

  A vector of enumerated sampling design values, and projects with the
  specific sampling designs will be returned. The enumerated values are:
  "simpleRandom", "systematicRandom", "clusteredRandom", "experimental",
  "targeted", & "opportunistic"

- contributors:

  A vector of identifiers for people associated with any projects. Each
  element can be a full name (first and last, e.g. "Zachary Amir"), an
  email address (e.g. "z.amir(at)uq.edu.au"), or an ORCID (e.g.
  "0000-0002-0113-xxxx"). The type of each value is auto-detected, so
  the vector can mix all three. Regardless of a person's role in a
  project, if any supplied identifier is found in the metadata, the
  relevant projects will be returned.

- tabularSharingPreference:

  A character vector specifying accepted sharing preferences. Defaults
  to `c("open", "partial")`, so every project a public key can see is
  returned. 'open' projects download with their data tables, while
  'partial' projects (usually under embargo) download as metadata only,
  which still tells you the data exist and who to contact. Use `"open"`
  alone to return only projects whose tables can be downloaded. 'closed'
  projects are returned only with admin database credentials; without
  them 'closed' is dropped with a warning.

## Value

A character vector of project IDs matching the specified criteria, or an
empty character vector (`character(0)`), with a warning, if nothing
matches.

## Details

The character vector of project IDs that is returned from this function
is then used in the function @seealso
[`wildobs_dp_download`](https://wildobs.github.io/WildObsR/reference/wildobs_dp_download.md)
for extracting data packages from WildObs' MongoDB.

## See also

- [`mongolite::mongo()`](https://jeroen.r-universe.dev/mongolite/reference/mongo.html)
  for database queries

## Examples

``` r
if (FALSE) { # \dontrun{
# Load API key from .Renviron
api_key <- Sys.getenv("WILDOBSR_API_KEY")

# Define spatial query: extract projects in a specific bounding box
spatial_query <- list(xmin = 145.0, xmax = 147.0, ymin = -20.0, ymax = -16.0)

# Define temporal query: select projects active in 2022-2025
temporal_query <- list(minDate = as.Date("2022-01-01"), maxDate = as.Date("2025-01-01"))

# Define taxonomic query: want all koalas and echidnas
taxa_query <- c("Phascolarctos cinereus", "Tachyglossus aculeatus")

# Define sampling design query: opportunistic and random datasets
sample_query <- c("simpleRandom", "opportunistic", "systematicRandom")

# Define contributor query: only want data from the WildObsR maintainer
contributor_query <- c("Zachary Amir")

# Query the WildObs database for matching projects
relevant_projects <- wildobs_mongo_query(api_key = api_key, spatial = spatial_query,
  temporal = temporal_query, taxonomic = taxa_query, samplingDesign = sample_query,
  contributors = contributor_query, tabularSharingPreference = "open")

# display the matching projects
print(relevant_projects)
} # }
```
