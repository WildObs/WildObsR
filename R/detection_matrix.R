#' Build Detection Histories that Match a Sampling Effort Matrix
#'
#' @description Counts detections of each species for every site and occasion of an
#' [effort_matrix()], so detections and effort share exactly the same sites, occasions and
#' day boundaries. A cell is `NA` where no camera was running and 0 where cameras ran but
#' the species was not detected, so non-detections are never confused with no sampling.
#'
#' The steps are:
#' \enumerate{
#'   \item Obscured threatened-species records (`obscured_for_<category>_species`), which
#'     carry no camera, and observations that are not of animals, are set aside with a
#'     message saying how many.
#'   \item Each detection is placed on its deployment's site, and on the local calendar day
#'     of its `eventStart` in the effort matrix's time zone. A detection on a day its camera
#'     was not running stops the function, because it means the deployment dates and the
#'     observations disagree.
#'   \item With `type = "occupancy"`, a cell is 1 if the species was detected at all. With
#'     `type = "abundance"`, each event (`eventID`) counts the largest `count` among its
#'     observations, and events are then combined over the occasion with `individuals`:
#'     `"sum"` adds them, while `"max"` takes the largest observed value per event. 
#' }
#'
#' @param obs A data frame of observations with at least `deploymentID`, `eventStart`
#'   (POSIXct) and `scientificName`; `count` and `eventID` are used for abundance, and
#'   `observationType` to keep animals only.
#' @param effort The result of [effort_matrix()] for the same deployments.
#' @param scientificNames Character vector of species to build histories for. A species
#'   never detected is kept, with zeros wherever cameras ran.
#' @param type `"occupancy"` (default) or `"abundance"`.
#' @param individuals For abundance: `"sum"` (default) or `"max"`; see Description.
#' @param by_camera Logical. Keep each deployment separate, as a fourth dimension matching
#'   the third dimension of `effort$effort`? Defaults to `FALSE`.
#'
#' @return A numeric array, species x site x occasion (x deployment slot when
#'   `by_camera = TRUE`), with dimnames from `effort` and a `type` attribute
#'   (`"occupancy"` or `"abundance"`). `NA` marks cells with no effort.
#'   Stops if the observations name deployments that are not in `effort`, or fall on days
#'   their camera was not running.
#'
#' @examples
#' # one camera at each of two stations, running 10 days
#' deps <- data.frame(
#'   deploymentID = c("A", "B"),
#'   locationID = c("station_1", "station_2"),
#'   deploymentStart = as.POSIXct("2024-03-01 00:00", tz = "UTC"),
#'   deploymentEnd = as.POSIXct("2024-03-11 00:00", tz = "UTC")
#' )
#' eff <- effort_matrix(deps, site_col = "locationID", w = 5)
#'
#' # three detections of a pademelon, two of them at station 1
#' obs <- data.frame(
#'   deploymentID = c("A", "A", "B"),
#'   eventID = c("e1", "e2", "e3"),
#'   eventStart = as.POSIXct(c("2024-03-02 03:00", "2024-03-08 21:00", "2024-03-03 01:00"),
#'                           tz = "UTC"),
#'   scientificName = "Thylogale stigmatica",
#'   count = c(1, 2, 1),
#'   observationType = "animal"
#' )
#'
#' # detected or not, and the number of individuals, per station and occasion
#' detection_matrix(obs, eff, scientificNames = "Thylogale stigmatica")
#' detection_matrix(obs, eff, "Thylogale stigmatica", type = "abundance")
#'
#' @seealso [effort_matrix()] for the effort these detections match.
#' @author Zachary Amir
#' @export
detection_matrix <- function(obs, effort, scientificNames,
                             type = c("occupancy", "abundance"),
                             individuals = c("sum", "max"), by_camera = FALSE) {

  ## settle the choices the caller made
  type <- match.arg(type)
  individuals <- match.arg(individuals)


  #
  ##
  ### Verify the inputs ----

  ## the effort must come from effort_matrix(), which carries the day boundaries
  if (!inherits(effort, "wildobs_effort")) {
    stop("detection_matrix() needs effort from effort_matrix(), built from the same deployments.",
         call. = FALSE)
  } # end effort check
  ## resampled observations are summed per cell and day, and have no deploymentID to place them
  # recognised by the tag resample_covariates_and_observations() adds, or by its summary columns
  resampled <- !is.null(attr(obs, "wildobs_resampled")) ||
    (is.data.frame(obs) && !"deploymentID" %in% names(obs) &&
       any(c("independentEvents", "totalIndividuals") %in% names(obs)))
  # if so, point to the raw observations
  if (resampled) {
    # hard stop, naming the table to pass instead
    stop("detection_matrix() was given resampled observations, which have no deploymentID to place them on a camera and day.\n",
         "Pass the observations themselves, frictionless::read_resource(dp, \"observations\"); ",
         "detection_matrix() places each one in its cell through the effort matrix.", call. = FALSE)
  } # end resampled check
  # the observation columns every approach needs
  needed <- c("deploymentID", "eventStart", "scientificName")
  # if we are measuring abundance, we need counts too 
  if (type == "abundance") needed <- c(needed, "count")
  # check for missing cols
  missing_cols <- setdiff(needed, names(obs))
  # if so, 
  if (!is.data.frame(obs) || length(missing_cols) > 0) {
    # hard stop w/ which cols are missing 
    stop(sprintf("detection_matrix() needs obs as a data frame with: %s.\n",
                 paste(missing_cols, collapse = ", ")),
         "Pass frictionless::read_resource(dp, \"observations\"), or a table with those columns.",
         call. = FALSE)
  } # end observation column check
  # times must be date-times to find their day
  if (!inherits(obs$eventStart, "POSIXct")) {
    stop("detection_matrix() needs eventStart as a POSIXct date-time.\n",
         "Convert it with as.POSIXct(), giving the project's time zone.", call. = FALSE)
  } # end POSIXct check
  # at least one species to count
  if (length(scientificNames) == 0 || any(is.na(scientificNames))) {
    stop("detection_matrix() needs at least one species in scientificNames.", call. = FALSE)
  } # end species check


  #
  ##
  ### Keep the detections that can be placed ----

  ## obscured threatened-species records carry no camera
  obscured <- grepl("^obscured_for_", obs$deploymentID)
  # if we gathered any 
  if (any(obscured)) {
    # warn us about them, 
    message(sprintf("detection_matrix(): set aside %d obscured observation(s), which carry no camera.\n",
                    sum(obscured)),
            "Detection histories from public data under-count those species where they are listed.")
    # and then remove them 
    obs <- obs[!obscured, , drop = FALSE]
  } # end obscured condition
  ## only animals are detections
  if ("observationType" %in% names(obs)) {
    not_animal <- !(obs$observationType %in% "animal")
    if (any(not_animal)) {
      message(sprintf("detection_matrix(): set aside %d observation(s) that are not of animals (blank, human, vehicle, ...).",
                      sum(not_animal)))
      obs <- obs[!not_animal, , drop = FALSE]
    } # end not animal condition
  } # end observationType condition
  # only the species asked for
  obs <- obs[obs$scientificName %in% scientificNames, , drop = FALSE]
  # a species with no detections at all is real data, but worth saying
  never_seen <- setdiff(scientificNames, obs$scientificName)
  ## if there was a species supplied that is not in the data
  if (length(never_seen) > 0) {
    # give a warning but dont stop the show 
    message(sprintf("detection_matrix(): no detections of %s; kept as zeros wherever cameras ran.",
                    paste(never_seen, collapse = ", ")))
  } # end never seen condition

  ## every detection must belong to a deployment in the effort matrix
  dep_map <- effort$deployments
  unknown <- setdiff(unique(obs$deploymentID), dep_map$deploymentID)
  # if there are unaccounted for deployments
  if (length(unknown) > 0) {
    # hard stop and tell us who is at fault 
    stop(sprintf("detection_matrix() found observations at %d deployment(s) that are not in effort, e.g. %s.\n",
                 length(unknown), paste(utils::head(unknown, 3), collapse = ", ")),
         "Build effort_matrix() from the deployments these observations came from.", call. = FALSE)
  } # end unknown deployment check

  ## the local day of every detection, in the effort matrix's time zone
  obs$date <- as.Date(obs$eventStart, tz = effort$settings$time_zone)
  # join it to that camera's running time on that day
  key_obs <- paste(obs$deploymentID, obs$date)
  key_eff <- paste(effort$daily$deploymentID, effort$daily$date)
  hit <- match(key_obs, key_eff)

  ## a detection on a day the camera was not running means the dates disagree
  ## (detections beyond dur are left out on purpose, so check against the full days)
  ## Grab all sampling sites matching effort matrix  
  site <- dep_map$site[match(obs$deploymentID, dep_map$deploymentID)]
  # calculate the day the cameras were running 
  day_number <- as.integer(obs$date - effort$settings$origin[site]) + 1L
  # if there are values beyond the max duration,
  beyond_dur <- if (is.null(effort$settings$dur)){
    # set them to FALSE 
    rep(FALSE, nrow(obs))
  }else{
    # or ensure that we are greater than duration 
    day_number > effort$settings$dur
  }
  ## if there are detections that are beyond duration and our of scope
  off_effort <- is.na(hit) & !beyond_dur
  if (any(off_effort)) {
    # hard stop because the dates are disorganized 
    stop(sprintf("detection_matrix() found %d detection(s) on days their camera was not running, e.g. %s on %s.\n",
                 sum(off_effort), obs$deploymentID[which(off_effort)[1]],
                 format(obs$date[which(off_effort)[1]])),
         "Check deploymentStart and deploymentEnd against these observations before modelling.",
         call. = FALSE)
  } # end off effort check
  # detections beyond dur are left out of the histories
  if (any(beyond_dur)) {
    message(sprintf("detection_matrix(): left out %d detection(s) after the first %d days (dur).",
                    sum(beyond_dur), effort$settings$dur))
  } # end beyond dur condition
  ## thin the data to match 
  obs <- obs[!beyond_dur, , drop = FALSE]
  hit <- hit[!beyond_dur]
  # each detection's site and occasion
  obs$site <- effort$daily$site[hit]
  obs$occasion <- effort$daily$occasion[hit]
  obs$slot <- dep_map$slot[match(obs$deploymentID, dep_map$deploymentID)]


  #
  ##
  ### Count detections per species, site and occasion ----

  ## the value each detection contributes
  if (type == "occupancy") {
    # any detection marks presence
    obs$value <- rep(1, nrow(obs))
  } else {
    # a missing count is a detection of at least one animal
    no_count <- is.na(obs$count)
    # if there were NAs in count 
    if (any(no_count)) {
      # give us a warning that we assume this is one individual 
      message(sprintf("detection_matrix(): %d detection(s) have no count and are counted as 1 individual.",
                      sum(no_count)))
      # store the value 
      obs$count[no_count] <- 1
    } # end no count condition

    ## within an event, the largest count is the number of individuals seen

    ## Use eventID if its present in obs (camtrapDP), but if not, go one per row (assume independence)
    event <- if ("eventID" %in% names(obs)) obs$eventID else seq_len(nrow(obs))
    # make a fake event if there are NA values
    event[is.na(event)] <- paste0("row_", which(is.na(event)))
    # save the events in obs 
    obs$event <- event
    # now aggregate, within an event, the number of individuals seen as the count 
    if (nrow(obs) > 0) {
      obs <- stats::aggregate(count ~ scientificName + site + occasion + slot + event,
                              data = obs, FUN = max)
    } # end any detections condition
    # save this as the value we fill in the matrix 
    obs$value <- obs$count
  } # end type condition

  ## Determine how to combine detections within each cell
  # if occupancy, take the max. If abundance, use sum or max, based on input 
  fun <- if (type == "occupancy") max else if (individuals == "sum") sum else max
  # if we are splitting by camera, include the slot. Otherwise skip it 
  group <- if (by_camera) c("scientificName", "site", "occasion", "slot") else
    c("scientificName", "site", "occasion")

  ## init the empty array, NA wherever no camera was running
  sites <- dimnames(effort$effort)[[1]]
  occs <- dimnames(effort$effort)[[2]]
  # if we are splitting per slot, 
  if (by_camera) {
    # format the matrix as species x site x occasion x deployment slot
    slots <- dimnames(effort$effort)[[3]]
    # init with all zeros 
    y <- array(0, dim = c(length(scientificNames), length(sites), length(occs), length(slots)),
               dimnames = list(species = scientificNames, site = sites, occasion = occs, unit = slots))
    # no effort in that slot means no data
    no_effort <- effort$effort == 0
    # fill in NA pads when no effort 
    for (s in seq_along(scientificNames)) y[s, , , ][no_effort] <- NA
  } else {
    # species x site x occasion
    y <- array(0, dim = c(length(scientificNames), length(sites), length(occs)),
               dimnames = list(species = scientificNames, site = sites, occasion = occs))
    no_effort <- effort$effort_total == 0
    for (s in seq_along(scientificNames)) y[s, , ][no_effort] <- NA
  } # end by camera condition

  ## fill the cells with detections
  if (nrow(obs) > 0) {
    # aggregate based on pre-filled occu vs abund value, applying the grouping fun
    cells <- stats::aggregate(obs$value, by = obs[group], FUN = fun)
    # Determine the accurate positions in the array
    pos <- cbind(match(cells$scientificName, scientificNames), match(cells$site, sites),
                 cells$occasion)
    # find split positions if needed
    if (by_camera) pos <- cbind(pos, cells$slot)
    # then fill each position w/ the aggregated value 
    y[pos] <- cells$x
  } # end fill condition

  # record what the values mean, so model_matrix_input() can pick the matching model frame
  attr(y, "type") <- type
  # return the histories
  return(y)
} # end detection_matrix
