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
#' present, media) is updated together and stays joinable, and the package's
#' \code{temporal} metadata describes the same survey groups as its data.
#'
#' @details
#' Pieces are chained in three steps:
#' \enumerate{
#'   \item Pieces are grouped by \code{locationID}, \code{multiSeason_deploymentGroup}
#'   and the \code{_CamN} suffix that the camDB pipeline appends to \code{deploymentID}
#'   when one station holds several cameras in the same survey (e.g. a lured and an
#'   unlured camera).
#'   \item Within a group, pieces are ordered by \code{deploymentStart}. A piece starts
#'   a new chain when it begins more than \code{max_gap_days} after the previous piece
#'   ended, because the 30-day rule behind \code{multiSeason_deploymentGroup} applies
#'   to the whole \code{locationName}, so a single station can sit idle for much
#'   longer inside one group. A piece that starts before the previous one ended
#'   cannot be the same camera, so it also starts a new chain and a warning names it.
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
#'   \item \code{deploymentTags} keeps each \code{key: value} pair once. Where pieces
#'   disagree on a key, placeholder values (\code{unspecified}, \code{unknown},
#'   \code{-}, \code{NA} or empty, ignoring case) are dropped if a real value exists,
#'   then the value that covers the most time is kept, so every key appears once;
#'   \item other character columns list the distinct values, separated by \code{" | "}.
#' }
#'
#' The new \code{deploymentStart}/\code{deploymentEnd} span includes any gaps of up
#' to \code{max_gap_days} between pieces. Calculate sampling effort from \code{pieceStart} and
#' \code{pieceEnd} in \code{deployment_crosswalk}, not from the recombined span.
#' \code{observationID} and \code{eventID} are left unchanged; they remain unique.
#'
#' In the \code{temporal} metadata, the per-\code{deploymentGroups} blocks are
#' replaced by one block per \code{multiSeason_deploymentGroup}. Each new block
#' starts at the earliest start and ends at the latest end of the single-season
#' blocks it absorbs. The package-level \code{start}, \code{end} and
#' \code{timeZone} are left unchanged.
#'
#' @param dp A WildObs camtrap data package, as returned by
#' \code{\link{wildobs_dp_download}}, holding at least the \code{deployments}
#' resource. The deployments must include \code{deploymentID}, \code{locationID},
#' \code{deploymentStart}, \code{deploymentEnd}, \code{deploymentGroups} and
#' \code{multiSeason_deploymentGroup}.
#' @param max_gap_days Numeric. The longest gap, in days, allowed between one piece
#' ending and the next starting at the same camera before the run is split into a
#' new \code{_partN} deployment. Defaults to 30, matching the gap that separates
#' multi-season groups.
#'
#' @return The same data package with:
#' \itemize{
#'   \item one \code{deployments} row per continuous camera run, with
#'   \code{deploymentGroups} replaced by its \code{multiSeason_deploymentGroup};
#'   \item \code{covariates} collapsed to match the new deployments;
#'   \item \code{deploymentID} remapped in \code{observations} and \code{media};
#'   \item \code{temporal} metadata holding one block per
#'   \code{multiSeason_deploymentGroup};
#'   \item a new \code{deployment_crosswalk} resource with one row per original
#'   piece: \code{original_deploymentID}, \code{deploymentID} (the new one),
#'   \code{original_deploymentGroups}, \code{pieceStart} and \code{pieceEnd}.
#' }
#' Stops if \code{max_gap_days} is not a single non-negative number, if a required
#' resource or column is missing, or if observations or media
#' hold a \code{deploymentID} that is not in the deployments.
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
#' @importFrom frictionless resource_names read_resource schema remove_resource add_resource
#' @importFrom dplyr group_by summarise arrange mutate ungroup select left_join filter lag across cur_column all_of
#' @importFrom stats setNames weighted.mean na.omit
#' @importFrom utils head
#' @export
recombine_deployment_groups <- function(dp, max_gap_days = 30) {


  #
  ##
  ### Make sure the package holds what the chaining relies on ----

  # the gap threshold must be one usable number of days
  if (!is.numeric(max_gap_days) || length(max_gap_days) != 1 || is.na(max_gap_days) ||
      max_gap_days < 0) {
    # so hard stop and say what it should look like
    stop(dp$id, " was given an invalid max_gap_days in recombine_deployment_groups().\n",
         "Provide a single non-negative number of days, e.g. max_gap_days = 30.")
  } # end max_gap_days condition

  # the deployments resource is the one thing we cannot work without
  if (!"deployments" %in% frictionless::resource_names(dp)) {
    # so hard stop and say how to get it
    stop(dp$id, " has no deployments resource in recombine_deployment_groups().\n",
         "Download the data package with tabular data (not metadata_only) and try again.")
  } # end missing deployments condition

  # grab the deployments
  deps <- frictionless::read_resource(dp, "deployments")

  ## the columns the chaining relies on
  required_cols <- c("deploymentID", "locationID", "deploymentStart", "deploymentEnd",
                     "deploymentGroups", "multiSeason_deploymentGroup")
  # check if any are missing
  missing_cols <- setdiff(required_cols, names(deps))
  # and if so, hard stop naming them
  if (length(missing_cols) > 0) {
    stop(sprintf("%s deployments are missing %s in recombine_deployment_groups().\n",
                 dp$id, paste(missing_cols, collapse = ", ")),
         "These come from Step 4 of the camDB pipeline; re-download the package from WildObs.")
  } # end missing columns condition


  #
  ##
  ### First, chain the 100-day pieces into continuous camera runs ----

  ## the camDB pipeline appends _CamN to deploymentID when a station holds
  ## several cameras in one survey, so pull that suffix (blank when absent)
  deps$..cam <- ifelse(grepl("_Cam[0-9]+$", deps$deploymentID),
                       sub("^.*_(Cam[0-9]+)$", "\\1", deps$deploymentID),
                       "")

  ## number each multi-season group at a location in time order, for short readable IDs
  ms_order <- deps |>
    # one row per location and multi-season group
    dplyr::group_by(locationID, multiSeason_deploymentGroup) |>
    # keep when that group started
    dplyr::summarise(..ms_start = min(deploymentStart), .groups = "drop") |>
    # order the groups within each location
    dplyr::arrange(locationID, ..ms_start) |>
    # restart the count at each location
    dplyr::group_by(locationID) |>
    # and count them off 1, 2, 3...
    dplyr::mutate(..ms_n = seq_along(..ms_start)) |>
    # drop the grouping
    dplyr::ungroup() |>
    # dropping the start time we only needed for ordering
    dplyr::select(-..ms_start)
  # attach that number to every piece
  deps <- dplyr::left_join(deps, ms_order, by = c("locationID", "multiSeason_deploymentGroup"))

  ## Build the chains
  deps <- deps |>
    # group pieces of the same camera at the same station in the same multi-season group
    dplyr::group_by(locationID, multiSeason_deploymentGroup, ..cam) |>
    # put them in time order
    dplyr::arrange(deploymentStart, .by_group = TRUE) |>
    dplyr::mutate(
      # the latest end among the earlier pieces of this camera (NA for the first piece)
      ..prev_end = dplyr::lag(cummax(as.numeric(deploymentEnd))),
      # flag pieces that start before an earlier piece of this camera has ended
      ..overlap = !is.na(..prev_end) & as.numeric(deploymentStart) < ..prev_end,
      # flag pieces that start too long after the camera last stopped
      ..gap = !is.na(..prev_end) &
        as.numeric(deploymentStart) - ..prev_end > max_gap_days * 86400,
      # every overlap or long gap opens a new part, so no chain runs two cameras at once
      # or spans a long idle period
      ..part = cumsum(..overlap | ..gap) + 1
    ) |>
    # drop the grouping
    dplyr::ungroup()

  # the _CamN suffix should prevent overlaps, so name any pieces that had to be split off
  if (any(deps$..overlap)) {
    warning(sprintf("%s has %d deployment(s) overlapping an earlier piece of the same camera: %s.\n",
                    dp$id, sum(deps$..overlap),
                    paste(deps$deploymentID[deps$..overlap], collapse = ", ")),
            "They were kept as separate long deployments (suffix _partN). ",
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

  # a lookup from old to new deploymentID, used for every resource
  id_map <- stats::setNames(crosswalk$deploymentID, crosswalk$original_deploymentID)
  # and how long each original deployment ran, used to weight the merged values
  piece_days <- stats::setNames(
    as.numeric(difftime(crosswalk$pieceEnd, crosswalk$pieceStart, units = "days")),
    crosswalk$original_deploymentID)

  # drop the helper columns before the deployments go back into the package
  deps <- deps[, !grepl("^\\.\\.", names(deps))]
  # which multi-season group each single-season survey now belongs to
  group_map <- unique(data.frame(original_deploymentGroups = deps$deploymentGroups,
                                 multiSeason_deploymentGroup = deps$multiSeason_deploymentGroup,
                                 stringsAsFactors = FALSE))
  # survey labels now describe the long-term group
  deps$deploymentGroups <- deps$multiSeason_deploymentGroup

  # collapse the deployments onto one row per continuous run
  deps_long <- .collapse_pieces(deps, id_map, piece_days)
  # and replace the deployments resource, keeping its schema
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
    # find any deploymentID without a matching deployment
    unknown <- setdiff(unique(x$deploymentID), names(id_map))
    # those records would silently lose their deployment, so hard stop
    if (length(unknown) > 0) {
      stop(sprintf("%s %s has %d deploymentID(s) missing from deployments in recombine_deployment_groups(): %s.\n",
                   dp$id, r, length(unknown), paste(utils::head(unknown, 5), collapse = ", ")),
           "The package's resources do not join; re-download it before recombining.")
    } # end unknown deploymentID condition
    # point every record at its long deployment
    x$deploymentID <- unname(id_map[x$deploymentID])
    # and put the resource back
    dp <- .replace_resource(dp, r, x)
  } # end per resource

  # add the crosswalk, so effort and the original IDs are never lost
  dp <- frictionless::add_resource(dp, "deployment_crosswalk", data = crosswalk)


  #
  ##
  ### Finally, match the temporal metadata to the new survey groups ----

  # packages without temporal metadata have nothing to update
  if (!is.null(dp$temporal)) {
    # swap the single-season blocks for multi-season ones
    dp$temporal <- .recombine_temporal(dp$temporal, group_map, dp$id)
  } # end temporal condition

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

  # remember the column order so the schema still lines up
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
    # tags are key: value pairs, so each key keeps a single value
    if (col == "deploymentTags") return(.merge_tags(as.character(v), w))
    # free text keeps every distinct value, collapsed together
    return(paste(sort(unique(stats::na.omit(as.character(v)))), collapse = " | "))
  } # end merge_values

  ## Collapse the table
  out <- x |>
    # one group per long deployment
    dplyr::group_by(deploymentID) |>
    # merge every other column with the rule above
    dplyr::summarise(dplyr::across(-..w, ~ merge_values(.x, ..w, dplyr::cur_column())),
                     .groups = "drop") |>
    # keep the original column order
    dplyr::select(dplyr::all_of(col_order))

  # return a plain data frame
  return(as.data.frame(out))

} # end .collapse_pieces


#' Merge the deploymentTags of several pieces into one tag string
#'
#' @param tags character vector, one pipe-delimited \code{key: value} string per piece.
#' @param w numeric vector, the days each piece ran.
#' @return a single tag string holding each key once, in the order keys first appear.
#'   Placeholder values only survive when a key has no real value; among real
#'   values the one covering the most time wins, ties going to the earliest piece.
#' @noRd
.merge_tags <- function(tags, w) {

  ## values that mean "not recorded", checked ignoring case
  ## agreed from a scan of PROD deploymentTags; none, nil, no and 0 are real values
  placeholders <- c("unspecified", "unknown", "-", "na", "")

  # init an empty table of pairs
  pairs <- NULL
  # for each piece's tag string
  for (i in seq_along(tags)) {
    # pieces without tags add nothing
    if (is.na(tags[i])) next
    # split into key: value pairs, the same way extract_tags() does
    p <- trimws(strsplit(tags[i], " | ", fixed = TRUE)[[1]])
    # drop blanks left by trailing delimiters
    p <- p[p != ""]
    # find the first colon, which separates key from value
    colon <- regexpr(":", p, fixed = TRUE)
    # and save one row per pair, carrying this piece's duration
    pairs <- rbind(pairs, data.frame(
      # malformed pairs with no colon act as their own key
      key = ifelse(colon > 0, trimws(substr(p, 1, colon - 1)), p),
      value = ifelse(colon > 0, trimws(substring(p, colon + 1)), NA_character_),
      # the pair exactly as written, which is what gets kept
      raw = p,
      w = rep(w[i], length(p)),
      stringsAsFactors = FALSE
    ))
  } # end per piece

  # if no piece had any tags, the run has none either
  if (is.null(pairs) || nrow(pairs) == 0) return(NA_character_)

  ## pick one pair per key
  kept <- vapply(unique(pairs$key), function(k) {
    # every pair for this key, in piece order
    kp <- pairs[pairs$key == k, ]
    # flag placeholder values
    is_ph <- is.na(kp$value) | tolower(kp$value) %in% placeholders
    # placeholders only survive when nothing real was recorded
    if (any(!is_ph)) kp <- kp[!is_ph, ]
    # compare values by their text, so "lure:none" and "lure: none" count together
    val <- ifelse(is.na(kp$value), kp$raw, kp$value)
    # total days each value covers
    days <- tapply(kp$w, val, sum)
    # the value covering the most time, ties going to the earliest piece
    best <- names(days)[days == max(days)]
    # and keep that pair as first written
    kp$raw[val %in% best][1]
  }, character(1))

  # rebuild a single tag string
  return(paste(kept, collapse = " | "))

} # end .merge_tags


#' Replace the deploymentGroups blocks of temporal metadata with multi-season blocks
#'
#' @param temporal list. The package's \code{temporal} metadata: package-level
#'   \code{start}, \code{end} and \code{timeZone} strings, plus one
#'   \code{list(start, end)} block per deploymentGroup.
#' @param group_map data frame with \code{original_deploymentGroups} and
#'   \code{multiSeason_deploymentGroup}, one row per pairing.
#' @param dp_id character. The package id, used in the warning.
#' @return the temporal list with the same package-level fields and one block per
#'   multi-season group, in time order. Warns if a survey has no block to take dates from.
#' @noRd
.recombine_temporal <- function(temporal, group_map, dp_id) {

  ### A deploymentGroup block is any entry whose value is a list
  ## the package-level start, end and timeZone are plain strings, so this
  ## keeps any package-level field we don't know about yet
  is_group <- vapply(temporal, is.list, logical(1))
  # split the blocks from the package-level fields
  old_blocks <- temporal[is_group]

  # surveys in the data that have no block in the metadata
  no_block <- setdiff(unique(group_map$original_deploymentGroups), names(old_blocks))
  # their dates can't be carried over, so say which ones
  if (length(no_block) > 0) {
    warning(sprintf("%s has %d deploymentGroups with no temporal block in recombine_deployment_groups(): %s.\n",
                    dp_id, length(no_block), paste(no_block, collapse = ", ")),
            "Their dates are left out of the multi-season blocks; check the package metadata.")
  } # end missing block warning

  ## one row per single-season block, holding its YYYY-MM-DD start and end
  old_dates <- data.frame(
    # the survey this block describes
    original_deploymentGroups = names(old_blocks),
    # [[ ]] rather than $, so a missing start can't partial-match another name
    start = vapply(old_blocks, function(b) {
      if (is.null(b[["start"]])) NA_character_ else as.character(b[["start"]])
    }, character(1)),
    # and the same for the end
    end = vapply(old_blocks, function(b) {
      if (is.null(b[["end"]])) NA_character_ else as.character(b[["end"]])
    }, character(1)),
    stringsAsFactors = FALSE
  )

  ## the span of each multi-season group, from the surveys it absorbs
  new_dates <- group_map |>
    # attach each survey's dates to its multi-season group
    dplyr::left_join(old_dates, by = "original_deploymentGroups") |>
    # surveys without a block were warned about above, so drop them
    dplyr::filter(!is.na(start), !is.na(end)) |>
    # one row per multi-season group
    dplyr::group_by(multiSeason_deploymentGroup) |>
    # YYYY-MM-DD strings sort in time order, so min and max give the span directly
    dplyr::summarise(start = min(start), end = max(end), .groups = "drop") |>
    # and list the groups in time order
    dplyr::arrange(start, multiSeason_deploymentGroup)

  # keep the package-level fields untouched, so the overall start and end never change
  new_temporal <- temporal[!is_group]
  # then add one block per multi-season group
  for (i in seq_len(nrow(new_dates))) {
    new_temporal[[new_dates$multiSeason_deploymentGroup[i]]] <- list(start = new_dates$start[i],
                                                                     end = new_dates$end[i])
  } # end per multi-season group

  # return the updated temporal metadata
  return(new_temporal)

} # end .recombine_temporal


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


## column names used inside the dplyr verbs above, declared so R CMD check
## doesn't mistake them for undefined variables
utils::globalVariables(c("locationID", "multiSeason_deploymentGroup", "deploymentID",
                         "deploymentStart", "deploymentEnd", "start", "end",
                         "..ms_start", "..cam", "..prev_end", "..overlap", "..gap", "..w"))
