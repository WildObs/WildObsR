#' Recombine 100-Day Deployments Into Continuous Long-Term Deployments
#'
#' @description
#' The WildObs curation pipeline splits long camera deployments into pieces of at
#' most 100 days, so each piece fits inside a single-season survey
#' (\code{deploymentGroups}). Long-term analyses (multi-season occupancy models,
#' Living Planet Index style trends, the Threatened Species Index) want the
#' opposite: one deployment per camera for as long as it ran without a break.
#' This function stitches the pieces back together using the
#' \code{multiSeason_deploymentGroup} column, which already groups consecutive
#' surveys at a \code{locationName} that are separated by 30 days or less.
#'
#' A data package goes in and a data package comes out, so every resource that
#' carries \code{deploymentID} (deployments, covariates, observations and, if
#' present, media) is updated together and stays joinable.
#'
#' @param dp A WildObs camtrap data package, as returned by
#' \code{\link{wildobs_dp_download}}, holding at least the \code{deployments}
#' resource. The deployments must include \code{deploymentID}, \code{locationID},
#' \code{deploymentStart}, \code{deploymentEnd}, \code{deploymentGroups} and
#' \code{multiSeason_deploymentGroup}.
#'
#' @return The same data package with:
#' \itemize{
#'   \item one \code{deployments} row per continuous camera run, with
#'   \code{deploymentGroups} replaced by its \code{multiSeason_deploymentGroup};
#'   \item \code{covariates} collapsed to match the new deployments;
#'   \item \code{deploymentID} remapped in \code{observations} and \code{media};
#'   \item a new \code{deployment_crosswalk} resource with one row per original
#'   piece: \code{original_deploymentID}, \code{deploymentID} (the new one),
#'   \code{original_deploymentGroups}, \code{pieceStart} and \code{pieceEnd}.
#' }
#' Stops if a required resource or column is missing.
#'
#' @details
#' Pieces are chained in three steps:
#' \enumerate{
#'   \item Pieces are grouped by \code{locationID}, \code{multiSeason_deploymentGroup}
#'   and the \code{_CamN} suffix that the camDB pipeline appends to \code{deploymentID}
#'   when one station holds several cameras in the same survey (e.g. a lured and an
#'   unlured camera).
#'   \item Within a group, pieces are ordered by \code{deploymentStart}. A piece that
#'   starts before the previous one ended cannot be the same camera, so it starts a
#'   new chain and a warning names it.
#'   \item Each chain becomes one deployment named
#'   \code{<locationID>_ms<N>[_CamN][_partN]}, where \code{N} orders the
#'   multi-season groups at that station in time.
#' }
#'
#' Columns are merged across the pieces of a chain as follows. A value shared by
#' every piece is kept as is. Otherwise:
#' \itemize{
#'   \item \code{deploymentStart} takes the earliest piece start and
#'   \code{deploymentEnd} the latest piece end;
#'   \item numeric columns take a mean weighted by each piece's duration, rounded
#'   for integer columns;
#'   \item logical columns are \code{TRUE} if any piece is \code{TRUE};
#'   \item factor columns (controlled vocabularies such as \code{featureType}) take
#'   the value that covers the most time, so they stay inside their vocabulary;
#'   \item character columns list the distinct values, separated by \code{" | "}.
#' }
#'
#' The new \code{deploymentStart}/\code{deploymentEnd} span includes any gaps of up
#' to 30 days between pieces. Calculate sampling effort from \code{pieceStart} and
#' \code{pieceEnd} in \code{deployment_crosswalk}, not from the recombined span.
#' \code{observationID} and \code{eventID} are left unchanged; they remain unique.
#' Package-level metadata (e.g. the per-deploymentGroup \code{temporal} blocks) is
#' not rewritten.
#'
#' @examples
#' \dontrun{
#' dp_list <- wildobs_dp_download(api_key = my_key, project_ids = my_ids)
#' long_dp <- recombine_deployment_groups(dp_list[[1]])
#' frictionless::read_resource(long_dp, "deployment_crosswalk")
#' }
#'
#' @author Zachary Amir & Claude Opus 5.5
#'
#' @seealso \code{\link{survey_and_deployment_generator}}, which creates the pieces
#'
#' @export
recombine_deployment_groups <- function(dp) {
  #
  ##
  ### data validation -----

  ## the deployments resource is the one thing we cannot work without
  if (!"deployments" %in% frictionless::resource_names(dp)) {
    stop(dp$id, " has no deployments resource in recombine_deployment_groups().\n",
         "Download the data package with tabular data (not metadata_only) and try again.")
  } # end missing deployments condition

  ## grab the deployments
  deps <- frictionless::read_resource(dp, "deployments")

  ## Specify the columns the chaining relies on
  required_cols <- c("deploymentID", "locationID", "deploymentStart", "deploymentEnd",
                     "deploymentGroups", "multiSeason_deploymentGroup")
  # check if any are missing
  missing_cols <- setdiff(required_cols, names(deps))
  # and if so, hard stop naming them
  if (length(missing_cols) > 0) {
    stop(dp$id, " deployments are missing ", paste(missing_cols, collapse = ", "),
         " in recombine_deployment_groups().\n",
         "These come from Step 4 of the camDB pipeline; re-download the package from WildObs.")
  } # end missing columns condition

  #
  ##
  ### First, chain the 100-day pieces into continuous camera runs ----

  ## pull the camera number appended to deploymentID via camDB pipeline,
  ## which happens when a deployment holds several cameras without a suffix
  deps$..cam <- ifelse(grepl("_Cam[0-9]+$", deps$deploymentID),
                       sub("^.*_(Cam[0-9]+)$", "\\1", deps$deploymentID),
                       "")

  ## number each multi-season group at a location in time order, for short readable IDs
  ms_order <- deps %>%
    # one row per location and multi-season group
    dplyr::group_by(locationID, multiSeason_deploymentGroup) %>%
    # keep when that group started
    dplyr::summarise(..ms_start = min(deploymentStart), .groups = "drop") %>%
    # order the groups within each location
    dplyr::arrange(locationID, ..ms_start) %>%
    # and count them off 1, 2, 3...
    dplyr::group_by(locationID) %>%
    dplyr::mutate(..ms_n = seq_along(..ms_start)) %>%
    dplyr::ungroup() %>%
    # dropping the start time we only needed for ordering
    dplyr::select(-..ms_start)
  # attach that number to every piece
  deps <- dplyr::left_join(deps, ms_order, by = c("locationID", "multiSeason_deploymentGroup"))

  ## Build the chains
  deps <- deps %>%
    # group pieces of the same camera at the same station in the same multi-season group
    dplyr::group_by(locationID, multiSeason_deploymentGroup, ..cam) %>%
    # put them in time order
    dplyr::arrange(deploymentStart, .by_group = TRUE) %>%
    dplyr::mutate(
      # flag pieces that start before an earlier piece of this camera has ended
      ..overlap = as.numeric(deploymentStart) < dplyr::lag(cummax(as.numeric(deploymentEnd)),
                                                           default = -Inf),
      # every overlap opens a new part, so no chain ever runs two cameras at once
      ..part = cumsum(..overlap) + 1
    ) %>%
    dplyr::ungroup()

  ## say which pieces had to be split off, since the suffix should prevent this
  if (any(deps$..overlap)) {
    warning(dp$id, " has ", sum(deps$..overlap), " deployment(s) overlapping an earlier piece ",
            "of the same camera: ", paste(deps$deploymentID[deps$..overlap], collapse = ", "),
            ".\nThey were kept as separate long deployments (suffix _partN). ",
            "Check these deployments for duplicated or mislabelled cameras.")
  } # end overlap warning

  ## name each chain <locationID>_ms<N>, then the camera and part only where needed
  deps$..new_id <- paste0(deps$locationID, "_ms", deps$..ms_n,
                          ifelse(deps$..cam == "", "", paste0("_", deps$..cam)),
                          ifelse(deps$..part > 1, paste0("_part", deps$..part), ""))

  ## Keep a record of every original piece, which is the original measure of effort
  crosswalk <- data.frame(
    # the 100-day deployment
    original_deploymentID = deps$deploymentID,
    # the continuous run it now belongs to
    deploymentID = deps$..new_id,
    # its single-season survey
    original_deploymentGroups = deps$deploymentGroups,
    # and exactly when that original deployment was running
    pieceStart = deps$deploymentStart,
    pieceEnd = deps$deploymentEnd,
    stringsAsFactors = FALSE
  )

  #
  ##
  ### Then, collapse deployments and covariates onto the new IDs ----

  ## a lookup from old to new deploymentID, used for every resource
  id_map <- stats::setNames(crosswalk$deploymentID, crosswalk$original_deploymentID)
  ## and how long each original deployment ran, used to weight the merged values
  piece_days <- stats::setNames(
    as.numeric(difftime(crosswalk$pieceEnd, crosswalk$pieceStart, units = "days")),
    crosswalk$original_deploymentID)

  ## drop the helper columns before the deployments go back into the package
  deps <- deps[, !grepl("^\\.\\.", names(deps))]
  # survey labels now describe the long-term group
  deps$deploymentGroups <- deps$multiSeason_deploymentGroup

  ## collapse the deployments and, if present, the covariates the same way
  deps_long <- .collapse_pieces(deps, id_map, piece_days)
  # replace the deployments resource, keeping its schema
  dp <- .replace_resource(dp, "deployments", deps_long)

  ## covariates are one row per deployment, so they collapse in step
  if ("covariates" %in% frictionless::resource_names(dp)) {
    # grab the covariates
    covs <- frictionless::read_resource(dp, "covariates")
    # survey labels now describe the long-term group here too
    covs$deploymentGroups <- deps$multiSeason_deploymentGroup[match(covs$deploymentID,
                                                                      deps$deploymentID)]
    # collapse onto the new deploymentIDs and put them back
    dp <- .replace_resource(dp, "covariates", .collapse_pieces(covs, id_map, piece_days))
  } # end covariates condition

  ## observations and media keep every row; only their deploymentID changes
  for (r in intersect(c("observations", "media"), frictionless::resource_names(dp))) {
    # grab the resource
    x <- frictionless::read_resource(dp, r)
    # any deploymentID without a matching deployment would silently lose its records
    unknown <- setdiff(unique(x$deploymentID), names(id_map))
    if (length(unknown) > 0) {
      stop(dp$id, " ", r, " has ", length(unknown), " deploymentID(s) missing from deployments ",
           "in recombine_deployment_groups(): ", paste(utils::head(unknown, 5), collapse = ", "),
           ".\nThe package's resources do not join; re-download it before recombining.")
    } # end unknown deploymentID condition
    # point every record at its long deployment
    x$deploymentID <- unname(id_map[x$deploymentID])
    # and put the resource back
    dp <- .replace_resource(dp, r, x)
  } # end per resource

  ## add the crosswalk, so effort and the original IDs are never lost
  dp <- frictionless::add_resource(dp, "deployment_crosswalk", data = crosswalk)

  # return the recombined package
  return(dp)

} # end recombine_deployment_groups


#' Collapse the rows of one table onto new deploymentIDs
#'
#' @param x data frame with a \code{deploymentID} column holding the old IDs.
#' @param id_map named character vector, old deploymentID -> new deploymentID.
#' @param piece_days named numeric vector, old deploymentID -> days that piece ran.
#' @return data frame with one row per new deploymentID, columns in the input order.
#' @noRd
.collapse_pieces <- function(x, id_map, piece_days) {

  ## remember the column order so the schema still lines up
  col_order <- names(x)
  # weight each row by how long its piece ran (a zero-length piece still counts a little)
  x$..w <- pmax(unname(piece_days[x$deploymentID]), 1e-6)
  # and point it at its long deployment
  x$deploymentID <- unname(id_map[x$deploymentID])

  ## a single rule for merging one column across several deployments in a chain
  merge_values <- function(v, w, col) {
    # values every deployment agrees on are kept as they are
    if (length(unique(v)) == 1) return(v[1])
    # the long deployment starts when the first deployment started
    if (col == "deploymentStart") return(min(v))
    # and ends when the last deployment ended
    if (col == "deploymentEnd") return(max(v))
    # numbers are averaged by how long each deployment ran
    if (is.numeric(v)) {
      # skip missing values, but stay NA if every piece is missing
      m <- if (all(is.na(v))) NA else stats::weighted.mean(v, w, na.rm = TRUE)
      # integers stay whole numbers
      return(if (is.integer(v)) as.integer(round(m)) else m)
    } # end numeric condition
    # a TRUE in any deployment holds for the whole run
    if (is.logical(v)) return(any(v, na.rm = TRUE))
    # controlled vocabularies take the value that covers the most time
    if (is.factor(v)) return(v[which.max(tapply(w, v, sum)[as.character(v)])])
    # free text keeps every distinct value, collapsed together
    paste(sort(unique(stats::na.omit(as.character(v)))), collapse = " | ")
  } # end merge_values

  ## Collapse the table
  out <- x %>%
    # one group per long deployment
    dplyr::group_by(deploymentID) %>%
    # merge every other column with the rule above
    dplyr::summarise(dplyr::across(-..w, ~ merge_values(.x, ..w, dplyr::cur_column())),
                     .groups = "drop") %>%
    # keep the original column order
    dplyr::select(dplyr::all_of(col_order))

  # return a plain data frame
  return(as.data.frame(out))

} # end .collapse_pieces


#' Swap one resource of a data package for new data, keeping its schema
#'
#' @param dp data package.
#' @param resource_name name of the resource to replace.
#' @param data the new data frame.
#' @return the data package with the resource replaced.
#' @noRd
.replace_resource <- function(dp, resource_name, data) {

  # keep the schema the package already declares for this resource
  schema <- frictionless::schema(dp, resource_name)
  # take the old resource out
  dp <- frictionless::remove_resource(dp, resource_name)
  # and add the new data back under the same name and schema
  dp <- frictionless::add_resource(dp, resource_name, data = data, schema = schema)

  # return the updated package
  return(dp)

} # end .replace_resource
