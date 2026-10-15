### make_api_fixture.R -- record a small slice of the public WildObs API for offline tests
##
## The download and query tests replay these records through a fake API, so the code
## that turns API responses into data packages runs in CI without a key. Everything here
## comes through the public API with a public key, so it is data any key holder can see.
## The open project is the package maintainer's own, trimmed to three deployments.
## A second, metadata-only copy of it is relabelled 'partial' under a made-up ID, so the
## embargoed path is tested without recording anyone else's project.
## Run from the package root with WILDOBSR_API_KEY set; not run by the test suite.

## Load libraries
library(here) # For project-relative paths so this runs on any machine

## load the package under development, so its API helper does the fetching
devtools::load_all(here(), quiet = TRUE)

## a public API key, exactly as a user would connect
api_key <- Sys.getenv("WILDOBSR_API_KEY")
# cant record anything without one
if (!nzchar(api_key)) {
  stop("WILDOBSR_API_KEY is not set, so make_api_fixture.R cannot reach the API.\n",
       "Add your key to ~/.Renviron and restart R.")
} # end API key check

## the open project to record, and the made-up ID its partial copy goes under
project <- "ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001"
partial_id <- "TEST_partial_copy_WildObsID_9999"
# the API endpoint every table is fetched from
url <- "https://camdbapi.wildobs.org.au/find"


#
##
### The metadata response, as the API sends it ----

## every project's metadata in one request, the way the package asks for it
resp <- httr::POST(url, httr::add_headers("X-API-Key" = api_key),
                   query = list(collection = "metadata"))
# keep the JSON unsimplified, so it can be written back out exactly as it came
parsed <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                             simplifyVector = FALSE)
# the documents sit in the results of the first element
docs <- parsed[[1]]$results
# keep only the open project's document
open_doc <- docs[[which(vapply(docs, function(d) identical(d$id, project), logical(1)))]]

## the partial copy: same document, made-up ID, embargoed
partial_doc <- open_doc
# a new ID, so it cannot be mistaken for a real project
partial_doc$id <- partial_id
partial_doc$name <- tolower(partial_id)
# and the sharing preference that makes it metadata-only
partial_doc$WildObsMetadata$tabularSharingPreference <- "partial"

## write the two documents back into the response shape the API uses
metadata_json <- jsonlite::toJSON(
  list(list(count = 2L, results = list(open_doc, partial_doc)), 200L),
  auto_unbox = TRUE, null = "null", digits = NA)


#
##
### The tables of three deployments ----

## the first three deployments, in key order
deps <- .wildobs_api_find(url, api_key,
                          list(collection = "deployments", filter = list(projectName = project),
                               sort = list(deploymentID = 1), limit = 3),
                          what = "the fixture deployments")
keep_deps <- deps$deploymentID

## a quick helper to fetch one table's rows for the kept deployments
fetch_rows <- function(collection) {
  .wildobs_api_find(url, api_key,
                    list(collection = collection,
                         filter = list(projectName = project,
                                       deploymentID = list(`$in` = keep_deps))),
                    what = sprintf("the fixture %s", collection))
} # end fetch helper

# covariates for those deployments
covs <- fetch_rows("covariates")
# up to 20 observations per deployment keeps the fixture small
obs <- fetch_rows("observations")
obs <- obs[stats::ave(seq_len(nrow(obs)), obs$deploymentID, FUN = seq_along) <= 20, ]
# the media behind those observations: each observation's representative image,
# plus any image that names the observation itself
med <- fetch_rows("media")
message(sprintf("media rows for the kept deployments: %d", nrow(med)))
med <- med[med$mediaID %in% obs$mediaID | med$observationID %in% obs$observationID, ]

## a few obscured observations of the project, so that path is exercised too
obscured <- .wildobs_api_find(url, api_key,
                              list(collection = "observations",
                                   filter = list(projectName = project,
                                                 deploymentID = list(`$regex` = "^obscured_for_")),
                                   limit = 3),
                              what = "the fixture obscured observations")
# they join the other observations; obscured rows lack some fields, which become NA
obs <- dplyr::bind_rows(obs, obscured)

## save it beside this script
saveRDS(list(metadata_json = metadata_json, project = project, partial_id = partial_id,
             deployments = deps, covariates = covs, observations = obs, media = med),
        here("tests", "testthat", "fixtures", "api_fixture.rds"), compress = "xz")
message(sprintf("api_fixture.rds: %d deployments, %d observations (%d obscured), %d media",
                nrow(deps), nrow(obs), nrow(obscured), nrow(med)))
