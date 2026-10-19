#' Build a Sampling Effort Matrix from Camera Deployments
#'
#' @description Calculates how long cameras were running, as camera-days, for every site
#' and sampling occasion, from the deployments table alone. Sampling effort happens 
#' regardless of what was detected, so it cannot depend on where or when animals were
#'  recorded, which is the requirement for using effort as a detection covariate or offset.
#'
#' The steps are:
#' \enumerate{
#'   \item Each deployment's running time is split into local calendar days (in
#'     `time_zone`), and each day gets the fraction of it the camera was running: a camera
#'     set at noon contributes 0.5 camera-days on its first day.
#'   \item Days are grouped into non-overlapping occasions of `w` days. Occasion 1 covers
#'     days 1 to `w`, occasion 2 days `w + 1` to `2w`, and so on; a final, shorter occasion
#'     is kept, and its length is reported in `occasions`.
#'   \item Camera-days are summed for every site, occasion and deployment. A site can be a
#'     single deployment (`site_col = "deploymentID"`), a camera station
#'     (`"locationID"`) with several deployments, or a grid cell from
#'     [spatial_hexagon_generator()].
#' }
#'
#' With `align = "site"` (default), each site's occasions start on the first day any of its
#' cameras ran, as in most single-season occupancy designs. With `align = "calendar"`,
#' every site's occasions start on the first day any camera in `deps` ran, so occasion `k`
#' covers the same dates everywhere, which multi-season and time-series models need.
#'
#' For long-term deployments rebuilt with [recombine_deployment_groups()], pass its
#' `deployment_crosswalk` as `crosswalk`: effort is then taken from the original cameras, so
#' the gaps between them (up to `max_gap_days`) count as no effort.
#'
#' @param deps A data frame of deployments, one row per deployment, with at least
#'   `deploymentID`, `deploymentStart` and `deploymentEnd` (POSIXct), and the column named
#'   in `site_col`. For grid cells, pass the output of [spatial_hexagon_generator()], never
#'   the resampled covariates, which have one row per cell and stop the function.
#' @param site_col Character. The column that defines a site. Defaults to
#'   `"deploymentID"`, one site per deployment. Sites with multiple camera deployments can
#'   be a station (`"locationID"`) or a grid cell from [spatial_hexagon_generator()], for
#'   example `"cellID_3km2"`; see "How the tables connect".
#' @param w Integer. Occasion length in days.
#' @param dur Integer or `NULL`. Number of days, counted from each site's (or the
#'   calendar) start, to keep. Defaults to `NULL`, which keeps every day with effort.
#' @param align `"site"` (default) or `"calendar"`; see Description.
#' @param split_by Character or `NULL`. A deployments column, such as `"featureType"` or
#'   `"baitUse"`, to split effort by. When given, the third dimension of `effort` holds its
#'   levels instead of individual deployments, so the share of effort under each level can
#'   be used as an observation covariate. One column only: to split by two, combine them
#'   into one column first, for example
#'   `deps$bait_trail <- paste(deps$baitUse, deps$trailStatus, sep = "_")`, then
#'   `split_by = "bait_trail"`. Missing values become their own level, `"NA"`, so decide
#'   what they mean before splitting.
#' @param crosswalk A data frame or `NULL`. The `deployment_crosswalk` from
#'   [recombine_deployment_groups()], with `deploymentID`, `pieceStart` and `pieceEnd`.
#' @param time_zone Character or `NULL`. The time zone that defines a calendar day, such as
#'   `dp$temporal$timeZone`. Defaults to the time zone of `deploymentStart`, then UTC.
#'   When the deployments span several time zones (several values given, or, with `NULL`,
#'   cameras whose coordinates fall in zones with different clocks, such as Queensland and
#'   New South Wales in summer), every camera uses UTC days, with a message. Effort and
#'   detections share the same day boundaries, so the matrices stay consistent; the day
#'   simply starts at 10 am in AEST, so an early-morning detection falls on the previous
#'   UTC day. That is harmless for occasions of several days. Zone names with the same
#'   clock, such as `Australia/Brisbane` and `Australia/Lindeman`, count as one.
#'
#' @return A list of class `wildobs_effort`:
#'   \describe{
#'     \item{`effort`}{Array of camera-days, site x occasion x deployment (or x level of
#'       `split_by`). A site's deployments fill the first slots of the third dimension;
#'       `deployments` says which deployment is in which slot.}
#'     \item{`effort_total`}{Matrix of camera-days, site x occasion. Zero means no camera
#'       was running.}
#'     \item{`occasions`}{One row per site and occasion: `site`, `occasion`, `start` and
#'       `end` dates, `days` in the occasion and `camera_days` of effort.}
#'     \item{`deployments`}{One row per deployment: `deploymentID`, `site`, `slot` (its
#'       position in the third dimension) and the `split_by` level.}
#'     \item{`daily`}{One row per deployment and active day: `deploymentID`, `site`,
#'       `date`, `occasion` and `camera_days`. [detection_matrix()] uses it to place
#'       detections.}
#'     \item{`settings`}{The arguments used, and each site's first day (`origin`).}
#'   }
#'   Stops if a required column is missing, a date-time is not POSIXct, a deployment ends
#'   before it starts, or `w` is not a positive whole number.
#'
#' @section How the tables connect:
#' The occupancy and abundance workflow uses three tables, each at the level its function
#' needs, joined by two keys: `deploymentID` links observations to deployments, and the
#' site column (for example `cellID_3km2`) links deployments to sites.
#'
#' | Table | One row per | Made by | Goes into |
#' |---|---|---|---|
#' | Deployments with sites (`covs_cellID`) | deployment | [frictionless::read_resource()], plus [spatial_hexagon_generator()] for cells | `effort_matrix()` |
#' | Observations | observation | [frictionless::read_resource()] | [detection_matrix()], which places each one in its site through the effort matrix |
#' | Site covariates | site | [resample_covariates_and_observations()] with `obs = NULL`, for cells | [model_matrix_input()] as `site_covs` |
#'
#' Effort needs each camera's own start and end, which only the deployment table keeps.
#' The resampled covariates have one row per cell, so they are for site covariates only.
#'
#' @examples
#' # two stations: one camera running 10 days, one starting at noon and running 4.5 days
#' deps <- data.frame(
#'   deploymentID = c("A", "B"),
#'   locationID = c("station_1", "station_2"),
#'   deploymentStart = as.POSIXct(c("2024-03-01 00:00", "2024-03-01 12:00"), tz = "UTC"),
#'   deploymentEnd = as.POSIXct(c("2024-03-11 00:00", "2024-03-06 00:00"), tz = "UTC")
#' )
#'
#' # camera-days per station in 5-day occasions
#' eff <- effort_matrix(deps, site_col = "locationID", w = 5)
#' eff$effort_total
#' eff$occasions
#'
#' @seealso [detection_matrix()] for the matching detections, and
#'   [recombine_deployment_groups()] for long-term deployments.
#' @author Zachary Amir
#' @export
effort_matrix <- function(deps, site_col = "deploymentID", w, dur = NULL,
                          align = c("site", "calendar"), split_by = NULL,
                          crosswalk = NULL, time_zone = NULL) {

  ## settle the choice of alignment
  align <- match.arg(align)

  #
  ##
  ### Verify the inputs ----

  ## deployments must be a table
  if (!is.data.frame(deps) || nrow(deps) == 0) {
    stop("effort_matrix() needs a data frame of deployments with at least one row.\n",
         "Pass frictionless::read_resource(dp, \"deployments\"), or a table with deploymentID, ",
         "deploymentStart and deploymentEnd.",
         call. = FALSE)
  } # end table check
  ## resampled covariates hold one row per cell, so each camera's start and end are gone
  # recognised by the tag resample_covariates_and_observations() adds, or by its cell-level columns
  resampled <- !is.null(attr(deps, "wildobs_resampled")) ||
    any(c("cellEffort", "samplingStart", "deploymentsIncluded") %in% names(deps))
  # if so, point to the table that still has one row per deployment
  if (resampled) {
    # hard stop, naming the table to pass instead
    stop("effort_matrix() was given resampled covariates (one row per cell), which have lost each camera's start and end.\n",
         sprintf("Pass the deployments with their cellIDs instead, the table you gave the resample: effort_matrix(covs_cellID, site_col = \"%s\", ...). ",
                 site_col),
         "Use the resampled covariates as site_covs in model_matrix_input().", call. = FALSE)
  } # end resampled check
  # the columns every calculation needs
  needed <- unique(c("deploymentID", "deploymentStart", "deploymentEnd", site_col, split_by))
  missing_cols <- setdiff(needed, names(deps))
  if (length(missing_cols) > 0) {
    stop(sprintf("effort_matrix() could not find these columns in deps: %s.\n",
                 paste(missing_cols, collapse = ", ")),
         "Check the spelling of site_col and split_by, or add the columns to deps.", call. = FALSE)
  } # end missing column check
  # date-times must be date-times, or day boundaries are meaningless
  if (!inherits(deps$deploymentStart, "POSIXct") || !inherits(deps$deploymentEnd, "POSIXct")) {
    stop("effort_matrix() needs deploymentStart and deploymentEnd as POSIXct date-times.\n",
         "Convert them with as.POSIXct(), giving the deployment's time zone.", call. = FALSE)
  } # end POSIXct check
  # every deployment needs a site
  if (any(is.na(deps[[site_col]]))) {
    stop(sprintf("effort_matrix() found %d deployment(s) with no value in site_col '%s'.\n",
                 sum(is.na(deps[[site_col]])), site_col),
         "Give every deployment a site, or remove those deployments.", call. = FALSE)
  } # end missing site check
  # a deployment ID used twice would double its effort
  if (any(duplicated(deps$deploymentID))) {
    stop(sprintf("effort_matrix() found repeated deploymentID values in deps: %s.\n",
                 paste(utils::head(unique(deps$deploymentID[duplicated(deps$deploymentID)]), 5),
                       collapse = ", ")),
         "Each deployment must appear once.", call. = FALSE)
  } # end duplicate check
  # occasions need a whole number of days
  if (length(w) != 1 || is.na(w) || w < 1 || w != round(w)) {
    stop("effort_matrix() needs w, the occasion length, as one whole number of days (1 or more).",
         call. = FALSE)
  } # end w check
  # the same for the number of days kept, when given
  if (!is.null(dur) && (length(dur) != 1 || is.na(dur) || dur < 1 || dur != round(dur))) {
    stop("effort_matrix() needs dur as one whole number of days (1 or more), or NULL to keep every day.",
         call. = FALSE)
  } # end dur check

  ## the time zone that defines a day: the argument, else the data's own, else UTC
  # without an argument, look up the cameras' own zones, since combined projects can span several
  if (is.null(time_zone) && all(c("latitude", "longitude") %in% names(deps))) {
    # the distinct zones the cameras sit in
    camera_zones <- .camera_time_zones(deps)
    # more than one is handled by the rule below; one keeps the data's own zone
    if (length(camera_zones) > 1) time_zone <- camera_zones
  } # end camera zone lookup
  if (is.null(time_zone)) time_zone <- attr(deps$deploymentStart, "tzone")
  if (is.null(time_zone) || length(time_zone) == 0 || !nzchar(time_zone[1])) {
    time_zone <- "UTC"
    message("effort_matrix(): no time zone given or found on deploymentStart, so days run midnight to midnight UTC.\n",
            "Pass time_zone = dp$temporal$timeZone to use local days.")
  } # end time zone fallback
  ## several zones share no single local day, so every camera uses UTC days
  if (length(unique(time_zone)) > 1) {
    # say which zones, and why this is still consistent
    message(sprintf("effort_matrix(): the deployments span %d time zones (%s), so days run midnight to midnight UTC for all of them.\n",
                    length(unique(time_zone)), paste(unique(time_zone), collapse = ", ")),
            "Effort and detections share these day boundaries, so the matrices stay consistent; ",
            "a day starts at 10 am in AEST, for example. Pass one time_zone to use local days instead.")
    time_zone <- "UTC"
  } # end several zones condition
  time_zone <- time_zone[1]


  #
  ##
  ### When was each deployment running? ----

  ## running periods: the recombined pieces when a crosswalk is given, else each deployment
  if (!is.null(crosswalk)) {
    # the crosswalk must carry the pieces
    if (!all(c("deploymentID", "pieceStart", "pieceEnd") %in% names(crosswalk))) {
      stop("effort_matrix() needs crosswalk to have deploymentID, pieceStart and pieceEnd.\n",
           "Pass the deployment_crosswalk resource from recombine_deployment_groups().", call. = FALSE)
    } # end crosswalk column check
    # keep the pieces of the deployments in deps
    periods <- data.frame(deploymentID = crosswalk$deploymentID,
                          start = crosswalk$pieceStart, end = crosswalk$pieceEnd)
    periods <- periods[periods$deploymentID %in% deps$deploymentID, , drop = FALSE]
  } else {
    # one period per deployment
    periods <- data.frame(deploymentID = deps$deploymentID,
                          start = deps$deploymentStart, end = deps$deploymentEnd)
  } # end crosswalk condition
  # a period must not run backwards
  backwards <- !is.na(periods$start) & !is.na(periods$end) & periods$end < periods$start
  if (any(backwards)) {
    stop(sprintf("effort_matrix() found %d deployment period(s) that end before they start, e.g. %s.\n",
                 sum(backwards), periods$deploymentID[which(backwards)[1]]),
         "Correct deploymentStart and deploymentEnd for those deployments.", call. = FALSE)
  } # end backwards check
  # a period with no start or end has no knowable effort
  incomplete <- is.na(periods$start) | is.na(periods$end)
  if (any(incomplete)) {
    # provide a warning if found
    warning(sprintf("effort_matrix() left out %d deployment period(s) with a missing start or end, e.g. %s.",
                    sum(incomplete), periods$deploymentID[which(incomplete)[1]]), call. = FALSE)
    # and drop the periods w/ no knowable effort; their deployments stay in the matrix as sites with no effort
    periods <- periods[!incomplete, , drop = FALSE]
  } # end incomplete condition

  ## split every period into local calendar days with the fraction of each day it covers
  daily <- .running_days(periods, time_zone)
  # pieces of one deployment should never overlap, but cap a day at one camera-day if they do
  daily <- stats::aggregate(camera_days ~ deploymentID + date, data = daily, FUN = sum)
  if (any(daily$camera_days > 1 + 1e-6)) {
    warning("effort_matrix() found deployment pieces that overlap in time; each camera counts at most one camera-day per day.",
            call. = FALSE)
    ## Roudn out camera days to a single day if present 
    daily$camera_days <- pmin(daily$camera_days, 1)
  } # end overlap condition


  #
  ##
  ### Place days in sites and occasions ----

  ## the site of every deployment, and the unit it fills in the third dimension
  dep_map <- data.frame(deploymentID = deps$deploymentID,
                        site = as.character(deps[[site_col]]), stringsAsFactors = FALSE)
  # the level of split_by, kept as text so logical and factor columns behave alike
  if (!is.null(split_by)) dep_map$level <- as.character(deps[[split_by]])
  # deployments in key order within each site
  dep_map <- dep_map[order(dep_map$site, dep_map$deploymentID), , drop = FALSE]
  # number each deployment in a site with a slot position
  dep_map$slot <- stats::ave(seq_len(nrow(dep_map)), dep_map$site, FUN = seq_along)
  # add the site to every day
  daily$site <- dep_map$site[match(daily$deploymentID, dep_map$deploymentID)]

  ## the first day of each site's occasions
  if (align == "site") {
    # each site starts on its own first active day
    origin <- tapply(daily$date, daily$site, min)
    origin <- stats::setNames(as.Date(origin, origin = "1970-01-01"), names(origin))
  } else {
    # every site starts on the first active day anywhere
    first <- min(daily$date)
    origin <- stats::setNames(rep(first, length(unique(dep_map$site))), sort(unique(dep_map$site)))
  } # end align condition
  # the day number of every active day, counted from its site's first day
  day_number <- as.integer(daily$date - origin[daily$site]) + 1L
  ## if a maximum duration was provided, 
  if (!is.null(dur)) {
    # days beyond dur are left out
    daily <- daily[day_number <= dur, , drop = FALSE]
    day_number <- day_number[day_number <= dur]
  } # end dur condition
  # and the occasion each day falls in
  daily$occasion <- (day_number - 1L) %/% as.integer(w) + 1L

  ## the dimensions of the result
  # number of rows
  sites <- sort(unique(dep_map$site))
  # number of columns 
  n_occ <- max(daily$occasion)
  # create a char name for columns 
  occ_names <- paste0("o", seq_len(n_occ))
  # the third dimension: deployment slots, or the levels of split_by
  # if there is nothing to split by, 
  if (is.null(split_by)) {
    # the third dimension is non-existent, so stick w/ deployment per site (i.e., slot)
    units <- paste0("deployment_", seq_len(max(dep_map$slot)))
    daily$unit <- units[dep_map$slot[match(daily$deploymentID, dep_map$deploymentID)]]
  } else {
    # but if we are splitting, grab that level 
    lv <- dep_map$level
    # make sure its all character
    lv[is.na(lv)] <- "NA"
    # then organize and attach via matching deployments
    units <- sort(unique(lv))
    daily$unit <- lv[match(daily$deploymentID, dep_map$deploymentID)]
  } # end third dimension condition


  #
  ##
  ### Sum camera-days into the arrays ----

  # camera-days for every site, occasion and unit that has any
  cells <- stats::aggregate(camera_days ~ site + occasion + unit, data = daily, FUN = sum)
  # init an empty array to fill
  effort <- array(0, dim = c(length(sites), n_occ, length(units)),
                  dimnames = list(site = sites, occasion = occ_names, unit = units))
  # fill it by position
  effort[cbind(match(cells$site, sites), cells$occasion, match(cells$unit, units))] <- cells$camera_days
  # the total over units
  effort_total <- apply(effort, c(1, 2), sum)
  # save the names 
  dimnames(effort_total) <- list(site = sites, occasion = occ_names)

  ## one row per site and occasion, with its dates
  occasions <- expand.grid(occasion = seq_len(n_occ), site = sites, stringsAsFactors = FALSE)
  occasions <- occasions[, c("site", "occasion")]
  # the first and last date of each occasion
  occasions$start <- origin[occasions$site] + (occasions$occasion - 1L) * w
  occasions$end <- occasions$start + (w - 1L)
  # the last occasion is cut short by dur, when given
  if (!is.null(dur)) occasions$end <- pmin(occasions$end, origin[occasions$site] + (dur - 1L))
  # its length in days, and its effort
  occasions$days <- as.integer(occasions$end - occasions$start) + 1L
  occasions$camera_days <- effort_total[cbind(match(occasions$site, sites), occasions$occasion)]
  rownames(occasions) <- NULL

  ## return everything detection_matrix() and model_matrix_input() need
  names(dep_map)[names(dep_map) == "level"] <- if (is.null(split_by)) "level" else split_by
  out <- list(
    effort = effort,
    effort_total = effort_total,
    occasions = occasions,
    deployments = dep_map[, setdiff(names(dep_map), "level"), drop = FALSE],
    daily = daily[, c("deploymentID", "site", "date", "occasion", "camera_days")],
    settings = list(site_col = site_col, w = w, dur = dur, align = align, split_by = split_by,
                    time_zone = time_zone, origin = origin)
  )
  class(out) <- c("wildobs_effort", "list")
  return(out)
} # end effort_matrix


#' Split running periods into local calendar days
#'
#' @param periods Data frame with `deploymentID`, `start` and `end` (POSIXct).
#' @param time_zone Character. The time zone that defines a day.
#' @return One row per deployment and day it ran: `deploymentID`, `date` and
#'   `camera_days`, the fraction of that day it was running.
#' @author Zachary Amir
#' @noRd
.running_days <- function(periods, time_zone) {
  # the first and last local day of every period
  first_day <- as.Date(periods$start, tz = time_zone)
  last_day <- as.Date(periods$end, tz = time_zone)
  # how many days each period touches
  n_days <- as.integer(last_day - first_day) + 1L
  # one row per period and day
  idx <- rep(seq_len(nrow(periods)), n_days)
  day <- first_day[idx] + (sequence(n_days) - 1L)
  # local midnight at the start and end of each day, so a 23- or 25-hour day is handled
  day_start <- as.POSIXct(format(day), tz = time_zone)
  day_end <- as.POSIXct(format(day + 1L), tz = time_zone)
  # the part of each day inside the period
  overlap_start <- pmax(as.numeric(periods$start[idx]), as.numeric(day_start))
  overlap_end <- pmin(as.numeric(periods$end[idx]), as.numeric(day_end))
  # as a fraction of that day's length
  frac <- pmax(overlap_end - overlap_start, 0) / (as.numeric(day_end) - as.numeric(day_start))
  # keep the days with any running time
  out <- data.frame(deploymentID = periods$deploymentID[idx], date = day, camera_days = frac,
                    stringsAsFactors = FALSE)
  return(out[out$camera_days > 0, , drop = FALSE])
} # end .running_days


#' Find the distinct time zones the cameras sit in
#'
#' Zones are looked up from each camera's coordinates. Zones with different names but the
#' same clock over the whole survey (for example `Australia/Brisbane` and
#' `Australia/Lindeman`) count as one, so only zones whose local days really differ are
#' returned.
#'
#' @param deps Data frame with `latitude`, `longitude`, `deploymentStart` and
#'   `deploymentEnd`.
#' @return A character vector of time zones with distinct clocks, possibly empty.
#' @author Zachary Amir
#' @noRd
.camera_time_zones <- function(deps) {
  # cameras with both coordinates
  has_xy <- !is.na(deps$latitude) & !is.na(deps$longitude)
  # nothing to look up
  if (!any(has_xy)) return(character(0))
  # each distinct camera position once, which keeps the lookup quick
  xy <- unique(data.frame(lat = deps$latitude[has_xy], lon = deps$longitude[has_xy]))
  # the zone of each position
  zones <- suppressWarnings(lutz::tz_lookup_coords(xy$lat, xy$lon, method = "fast", warn = FALSE))
  # the distinct zones that were found
  zones <- sort(unique(zones[!is.na(zones)]))
  # one zone or none needs no comparison
  if (length(zones) < 2) return(zones)
  ## compare the zones' clocks on every day of the survey
  # the survey's days, from the first start to the last end
  days <- seq(as.Date(min(deps$deploymentStart, na.rm = TRUE)),
              as.Date(max(deps$deploymentEnd, na.rm = TRUE)), by = "day")
  # midday UTC on each day, a moment that falls on that date everywhere in Australia
  noon <- as.POSIXct(paste(days, "12:00:00"), tz = "UTC")
  # each zone's offset from UTC on each of those days, joined into one fingerprint
  clocks <- vapply(zones, function(z) paste(as.POSIXlt(noon, tz = z)$gmtoff, collapse = ","),
                   character(1))
  # keep the first zone of each distinct clock
  zones[!duplicated(clocks)]
} # end .camera_time_zones
