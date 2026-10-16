#' Example WildObs Data Packages from the Queensland Wet Tropics
#'
#' @description Reads the two WildObs projects shipped with the package, so examples,
#'   tests and tutorials run offline on real data. They come back in the same shape as
#'   [wildobs_dp_download()] returns: a named list of Camtrap DP data packages, each with
#'   its tables in `resources` and date-times in the project's local time zone. Read the
#'   tables with [frictionless::read_resource()]. A copy also sits in `dp$data`, because the
#'   `camtrapdp` class expects one, but other tools do not always keep it up to date.
#'
#'   The two projects sample the same rainforest with different designs, which is what
#'   makes pooling them a useful exercise:
#'   \describe{
#'     \item{`ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001`}{Systematic, unbaited cameras on
#'       roads, trails and off-trail across six national parks, 2022-23: 275 deployments.}
#'     \item{`QLD_Paluma_cassowary_Mclean_2023-24_WildObsID_0006`}{Targeted, baited
#'       cameras on game trails in Paluma Range National Park, 2023-24: 75 deployments.}
#'   }
#'   Both are open and licensed CC-BY-4.0; cite them with [cite_datapackages()]. They
#'   are copies of the public data at the time the package was built (see
#'   `versionControlWildObs`), with three differences from a live download: only animal
#'   observations are kept, there is no media table, and contributors' email addresses
#'   are left out. Threatened-species records stay
#'   obscured, exactly as the public API serves them. The copies are rebuilt by
#'   `dev/make_example_data.R` in the package source.
#'
#' @param project_ids Which projects to read. Defaults to both. Use
#'   `example_datapackages(project_ids = NULL)` to list the available IDs without
#'   reading anything.
#'
#' @return A named list of data packages of class `camtrapdp`, one per project, with
#'   `deployments`, `observations` and `covariates` tables. With `project_ids = NULL`,
#'   the character vector of available project IDs. Stops if an ID is not one of them.
#'
#' @examples
#' # the projects shipped with WildObsR
#' example_datapackages(project_ids = NULL)
#'
#' # read both, exactly as wildobs_dp_download() would return them
#' dp_list <- example_datapackages()
#' names(dp_list)
#' deps <- frictionless::read_resource(dp_list[[1]], "deployments")
#' head(deps[, c("deploymentID", "deploymentStart", "baitUse")])
#'
#' # and cite them
#' cite_datapackages(dp_list)
#'
#' @seealso [wildobs_dp_download()] for the full, current data from the WildObs database.
#' @author Zachary Amir
#' @importFrom frictionless read_package read_resource resource_names schema
#' @export
example_datapackages <- function(project_ids = c("ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001",
                                                 "QLD_Paluma_cassowary_Mclean_2023-24_WildObsID_0006")) {

  ## the folder of shipped packages, one sub-folder per project
  root <- system.file("extdata", "wet_tropics", package = "WildObsR")
  # each project's descriptor
  descriptors <- file.path(list.dirs(root, full.names = TRUE, recursive = FALSE), "datapackage.json")
  ## the folders carry short names, so read each project's ID from its descriptor
  available <- vapply(descriptors, function(f) jsonlite::read_json(f)$id, character(1))
  # keep the descriptor paths findable by project ID
  descriptors <- stats::setNames(descriptors, available)
  # asking for none lists them instead
  if (is.null(project_ids)) return(sort(unname(available)))

  ## an ID that is not shipped is most likely a typo or a live-only project
  unknown <- setdiff(project_ids, available)
  if (length(unknown) > 0) {
    stop(sprintf("example_datapackages() has no shipped copy of: %s.\n", paste(unknown, collapse = ", ")),
         sprintf("Choose from %s, or download other projects with wildobs_dp_download().",
                 paste(sort(available), collapse = ", ")),
         call. = FALSE)
  } # end unknown ID check

  # init an empty list to hold the packages
  dp_list <- list()
  # for each project asked for
  for (id in project_ids) {
    ## read the descriptor; the tables stay on disk until read below
    dp <- frictionless::read_package(descriptors[[id]])

    ## date-times are stored in UTC, so give them back in the project's own time zone
    tz <- dp$temporal$timeZone
    if (is.null(tz) || is.na(tz)) tz <- "UTC"

    # init the dp$data copy that wildobs_dp_download() also provides
    tables <- list()
    # for each table in the package
    for (r in frictionless::resource_names(dp)) {
      # read it from the gzipped CSV
      df <- as.data.frame(frictionless::read_resource(dp, r))
      ## frictionless turns constrained values into factors; a download keeps them as
      ## their schema type, so go back to text and type them the same way it does
      df[] <- lapply(df, function(x) if (is.factor(x)) as.character(x) else x)
      # type every column by its schema, date-times in the project's time zone
      df <- apply_schema_types(df, frictionless::schema(dp, r), timezone = tz)
      # every date-time column, shown in local time; the instant is unchanged
      for (cl in names(df)[vapply(df, inherits, logical(1), what = "POSIXct")]) {
        attr(df[[cl]], "tzone") <- tz
      } # end per date-time column
      # keep it for dp$data
      tables[[r]] <- df
      ## and hold it in the resource too, so read_resource() and frictionless
      ## functions work on it without the files on disk
      i <- which(frictionless::resource_names(dp) == r)
      dp$resources[[i]]$data <- df
      dp$resources[[i]]$path <- NULL
    } # end per table

    ## the tables, as wildobs_dp_download() also offers them
    dp$data <- tables
    # recognised as a Camtrap DP package
    class(dp) <- unique(c("camtrapdp", class(dp)))
    # store it under its project ID
    dp_list[[id]] <- dp
  } # end per project

  # return the packages
  return(dp_list)
} # end example_datapackages
