#' Reclassify eventID Values Using a New Temporal Threshold
#'
#' @description
#' The WildObsR pipeline pre-calculates \code{deltaTime_event} (seconds since the
#' previous independent observation of the same species at the same deployment)
#' and groups records into events using a fixed independence threshold (default
#' 30 minutes). This function reuses that pre-calculated column to redraw event
#' boundaries at a different, user-supplied threshold, without re-parsing
#' \code{dateTime}, making it much faster than re-running the full event-grouping
#' step of the pipeline.
#'
#' This is useful for identifying "continuous" detections that exceed the default
#' independence threshold (e.g., an animal resting in front of a camera for hours):
#' rather than discarding or condensing these observations, users can reclassify
#' them under a longer threshold and decide for themselves whether to keep or
#' remove them.
#'
#' @param obs A data frame or tibble of observations (e.g., from a Camtrap DP
#' \code{observations} resource) that has already been processed by the WildObsR
#' pipeline. Must include the columns \code{deploymentID}, \code{scientificName},
#' \code{eventStart}, and \code{deltaTime_event}.
#' @param new_event_time A single positive numeric value giving the new
#' independence threshold, in minutes, used to group observations into events.
#'
#' @return The input \code{obs} data frame with \code{eventID} recalculated
#' using the new threshold. All other columns, including \code{eventStart} and
#' \code{eventEnd}, are left unchanged. The original row order is preserved.
#'
#' @details
#' For a given \code{new_event_time} threshold (in minutes), a row is flagged as
#' the start of a new event if \code{deltaTime_event == 0} (the first record of
#' a deploymentID/scientificName group) or \code{deltaTime_event} exceeds the new
#' threshold (converted to seconds). A cumulative sum of those boundary flags,
#' within each \code{deploymentID} + \code{scientificName} group and ordered by
#' \code{eventStart}, produces a sequential event counter that increments only at
#' genuine boundaries. Rows where \code{deltaTime_event} is \code{NA} (i.e.,
#' non-boundary rows) do not increment the counter.
#'
#' The new \code{eventID} is built as
#' \code{deploymentID_scientificName_<N>min_event_<counter>}, with spaces in
#' \code{scientificName} replaced by underscores. \code{scientificName} is
#' embedded in the eventID because the event counter calculated here resets per
#' species per deployment, unlike the sequential counter used to build eventID
#' in the WildObsR pipeline (which is unique across all species at a
#' deploymentID). Including \code{scientificName} keeps the new eventID unique
#' in the same way.
#'
#' \code{eventStart} and \code{eventEnd} are intentionally left untouched by this
#' function, as they reflect the temporal span of each observationID's media
#' files rather than the event-grouping threshold. Because a longer threshold
#' can combine multiple pre-existing events (and their \code{eventStart}/
#' \code{eventEnd} values) under a single new \code{eventID}, users who need
#' event-level start/end times consistent with the new threshold should
#' recompute them (e.g., by taking the min/max of \code{eventStart}/
#' \code{eventEnd} per new \code{eventID}) as a separate step.
#'
#' @examples
#' obs <- data.frame(
#'   deploymentID = "Cam01",
#'   scientificName = c("Canis lupus", "Canis lupus", "Canis lupus", "Canis lupus"),
#'   eventStart = as.POSIXct(c("2023-01-01 08:00:00", "2023-01-01 08:20:00",
#'                             "2023-01-01 09:10:00", "2023-01-02 08:00:00"),
#'                           tz = "UTC"),
#'   eventEnd = as.POSIXct(c("2023-01-01 08:00:10", "2023-01-01 08:20:10",
#'                           "2023-01-01 09:10:10", "2023-01-02 08:00:10"),
#'                         tz = "UTC"),
#'   # seconds since the previous independent (30-min) event boundary
#'   deltaTime_event = c(0, 1200, 3000, 82800),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Reclassify events using a 60-minute threshold
#' reclassify_eventID(obs, new_event_time = 60)
#'
#' @author Zachary Amir & Claude
#'
#' @export
reclassify_eventID <- function(obs, new_event_time) {

  ## Make sure the required columns are present
  required_cols <- c("deploymentID", "scientificName", "eventStart", "deltaTime_event")
  missing_cols <- setdiff(required_cols, names(obs))
  if (length(missing_cols) > 0) {
    stop(paste("The `obs` data frame is missing the following required column(s):",
               paste(missing_cols, collapse = ", "),
               ". These are needed to recalculate eventID with a new time threshold.\n"))
  }

  ## Make sure new_event_time is a single, positive number
  if (!is.numeric(new_event_time) || length(new_event_time) != 1 ||
      is.na(new_event_time) || new_event_time <= 0) {
    stop("`new_event_time` must be a single positive numeric value, given in minutes.\n")
  }

  ## Warn (but don't stop) if the new threshold is below the default obs_time window (5 min),
  ## since values below this are unlikely to represent a meaningful independence threshold.
  if (new_event_time < 5) {
    warning("`new_event_time` is less than 5 minutes, which is below the default observation window (obs_time) used by the WildObsR pipeline to define independent observations. Event boundaries finer than this may not be meaningful.\n")
  }

  ## convert threshold from minutes to seconds for comparison
  threshold_secs <- new_event_time * 60

  ## build new event tag string to embed in the new eventID (e.g., "30min" -> "60min")
  new_tag <- paste0(new_event_time, "min")

  ## preserve the original row order, since grouping/arranging below can reorder rows
  obs$..orig_row_order <- seq_len(nrow(obs))

  obs <- obs |>
    # group by deployment and species so event counters reset per species per camera
    dplyr::group_by(deploymentID, scientificName) |>
    # arrange by start date-time within each group, required for the boundary/cumsum logic below
    dplyr::arrange(eventStart, .by_group = TRUE) |>
    dplyr::mutate(
      # derive new event boundary flag:
      # TRUE  = first record ever (delta == 0) or gap exceeds new threshold
      # FALSE = gap exists but is below threshold (i.e., belongs to prior event)
      # NA    = non-boundary row (deltaTime_event is NA); treated as FALSE in cumsum
      independent_event_new = dplyr::case_when(
        deltaTime_event == 0              ~ TRUE,
        deltaTime_event >= threshold_secs ~ TRUE,
        !is.na(deltaTime_event)           ~ FALSE,
        TRUE                              ~ NA
      ),
      # increment event counter only at TRUE boundaries; NA rows treated as 0 (non-incrementing)
      event_counter = cumsum(ifelse(is.na(independent_event_new), FALSE, independent_event_new)),
      # reconstruct eventID, embedding scientificName since the counter above resets per
      # species (unlike the globally sequential counter used in the WildObsR pipeline)
      eventID = paste(deploymentID, gsub(" ", "_", scientificName), new_tag, "event", event_counter, sep = "_")
    ) |>
    # remove groupings and drop derived columns
    dplyr::ungroup() |>
    dplyr::select(-independent_event_new, -event_counter) |>
    # restore the original row order
    dplyr::arrange(..orig_row_order) |>
    dplyr::select(-..orig_row_order)

  ## return the updated data frame
  obs

} # end reclassify_eventID
