### make_as_camtrapdp_fixture.R -- build the small data package the as_camtrapdp() tests use
##
## A real, OPEN WildObs project (its tables are public through the API), trimmed to three
## deployments so the tests run offline and fast. Chosen because it carries every WildObs
## extension as_camtrapdp() handles: a non-standard taxonRank (subclass), a RAiD related
## identifier, English common names, media.observationID, and a covariates table.
## Run from the package root with LOCAL MongoDB access; not run by the test suite.

## the open project to trim
project <- "TAS_Quoin_Eavesdropping_Znidersic_2025-26_WildObsID_0054"

# download it with media, so all three Camtrap DP tables are present
devtools::load_all()
dp <- wildobs_dp_download(db_url = Sys.getenv("MONGODB_LOCAL_RO_URL"), project_ids = project,
                          media = TRUE)[[1]]

## keep three deployments, and only the rows that belong to them
keep_deps <- utils::head(dp$data$deployments$deploymentID, 3)
# a quick helper to trim one resource's in-memory table, keeping resources and dp$data in step
trim <- function(dp, name, rows) {
  i <- which(vapply(dp$resources, function(r) r$name, character(1)) == name)
  dp$resources[[i]]$data <- dp$resources[[i]]$data[rows(dp$resources[[i]]$data), , drop = FALSE]
  dp$data[[name]] <- dp$resources[[i]]$data
  dp
} # end trim helper
dp <- trim(dp, "deployments", function(d) d$deploymentID %in% keep_deps)
dp <- trim(dp, "covariates", function(d) d$deploymentID %in% keep_deps)
# up to 20 observations per deployment
dp <- trim(dp, "observations", function(d) d$deploymentID %in% keep_deps &
             stats::ave(seq_len(nrow(d)), d$deploymentID, FUN = seq_along) <= 20)
# the media for those observations
kept_obs <- dp$data$observations$observationID
dp <- trim(dp, "media", function(d) d$observationID %in% kept_obs)

## save it beside this script
saveRDS(dp, here::here("tests", "testthat", "fixtures", "as_camtrapdp_dp.rds"), compress = "xz")
