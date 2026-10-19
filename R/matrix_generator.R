#' Generate Detection History and Covariate Matrices from camtrapDP Formatted Data
#'
#' This function processes camera trapping data formatted using the camtrapDP data standard to generate detection history matrices along with site-level and observation-level covariate matrices. These outputs can then be used for hierarchical occupancy or abundance modeling (e.g., with the \code{unmarked} package).
#'
#' @param obs A \code{data.frame} of observations. This must include columns for date-time information
#'   (e.g., \code{eventStart}, \code{eventEnd},
#'   a species identifier (\code{scientificName}), and a unique identifier for sampling units
#'   (e.g., \code{cellID_10km} in spatially resampled data from \code{WildObsR::resample_covariates_and_observations()} or \code{deploymentID} in regular camtrap DP data).
#' @param covs A \code{data.frame} of covariates. This must include sampling start/end dates (e.g.,
#'   \code{samplingStart}, \code{samplingEnd} in spatially resampled data from \code{WildObsR::resample_covariates_and_observations()} or \code{deploymentStart}, \code{deploymentEnd} in regular camtrap DP data), spatial identifiers (matching \code{obs}), and additional site-level covariate columns.
#' @param dur A numeric value indicating the maximum sampling duration (in days) where a population is assumed to be closed to changes in size.
#' @param w A numeric value specifying the length (in days) of each sampling occasion window that is used to compress the detection history matrix to improve model performance by increasing detection probability.
#' @param site_covs A character vector of column names in \code{covs} to be used as site-level covariates.
#' @param obs_covs A character vector of column names in \code{obs} to be used as observation-level covariates.
#' @param all_locationNames A logical indicating whether all locationName values should be included (\code{TRUE})
#'   or only locationNames with species detections (\code{FALSE}) should be included.
#' @param scientificNames A character vector of species names to generate matrices for. Each species
#'   will be processed separately.
#' @param type A character string indicating the type of model. Use \code{"abundance"} to generate a count history matrix using count data
#'   or \code{"occupancy"} to generate a detection history matrix using presence/absence data.
#' @param individuals A character string specifying the method for aggregating individual counts used in the count history matrix.
#'   Use \code{"sum"} to sum detections across occasions or \code{"max"} to take the maximum value.
#' @param cap_count Logical. If \code{TRUE}, the function will automatically cap observed counts for each species
#'  based on its mean and maximum observed group size to prevent extremely large counts from negatively affecting model fit.
#'    Species with high mean or maximum individual counts will have their values capped (e.g., at 3, 5, or 7), while others retain full counts.
#'    Default is \code{FALSE}, meaning no capping is applied.
#'
#'@return A named \code{list} where each element corresponds to a species. Each element is itself a
#'   list with the following components:
#'   \describe{
#'     \item{\code{detection_matrix}}{A matrix of detection (or count) data with rows representing
#'       sampling units and columns representing sampling occasion windows.}
#'     \item{\code{site_level_covariates}}{A \code{data.frame} of site-level covariates used in the analysis.}
#'     \item{\code{observation_level_covariates}}{A \code{list} of matrices containing observation-level covariate, where each matrix is aligned with the detection history matrix.}
#'   }
#'
#' @details
#' The function performs several key steps:
#'
#' \enumerate{
#'   \item \strong{Data Validation:} Checks are performed to ensure that:
#'     \itemize{
#'       \item The identifier column (e.g., \code{cellID} or \code{deploymentID}) is consistent across
#'         \code{obs} and \code{covs}.
#'       \item Every sampling unit in \code{obs} has a row in \code{covs}, and every observation
#'         covariate is a column of \code{obs}; otherwise the function stops.
#'       \item NA values in site-level or observation-level covariates are kept, with a warning:
#'         unmarked drops the affected sampling units or occasions with its own warning.
#'       \item Date-time columns in both \code{obs} and \code{covs} are \code{POSIXct}. Text columns
#'         are converted, with a message; a value that cannot be read stops the function.
#'       \item No site-level covariate is dropped. One with the same value everywhere is kept,
#'         standardised to 0 if numeric.
#'       \item The provided species names (\code{scientificNames}) exist in the \code{obs} table.
#'     }
#'     Sampling units with covariates but no observations are kept, and enter every species'
#'     matrices as sampled zeros, with a message naming them.
#'   \item \strong{Timezone Adjustment:} Date-time values are put in the local time zone of the
#'     sampling units, looked up from their coordinates with \code{lutz::tz_lookup_coords}, so records fall
#'     on their local day. When the sampling units span time zones with different clocks, every
#'     unit uses UTC days instead, with a message.
#'   \item \strong{Date Sequence and Sampling Occasions:} A complete sequence of dates is generated for each
#'     sampling unit and an index is assigned to each sampling occasion.
#'   \item \strong{Matrix Generation:} For each species, a detection history matrix is created by aligning
#'     observations to their respective sampling occasions. The matrix can contain either count data (for
#'     abundance models) or binary presence/absence data (for occupancy models).
#'   \item \strong{Observation-level Covariate Formatting:} Observation covariates are organized into matrices
#'     that align with the detection history matrix and are compressed to match sampling occasion windows.
#' }
#'
#' @examples
#' \dontrun{
#' # Example usage with pre-processed camera trap data, accessed from WildObs:
#'
#' # Load the general use WildObs API key
#' api_key <- Sys.getenv("WILDOBSR_API_KEY")
#' # use WildObsR function to access data
#' dp <- WildObsR::wildobs_dp_download(api_key = api_key, project_ids = "ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001")
#' # extract first DP from the list
#' dp <- dp[[1]]
#'
#' # Extract resources from the data package
#' covs <- frictionless::read_resource(dp, "covariates")
#' obs  <- frictionless::read_resource(dp, "observations")
#' deps <- frictionless::read_resource(dp, "deployments")
#'
#' # Assign spatial scales
#' # 'scales' defines the area (in square meters) covered by the hexagonal cell
#' scales <- c(1000000, 3000000)
#'
#' # Generate spatial hexagons based on the provided scales
#' covs <- WildObsR::spatial_hexagon_generator(covs, scales)
#'
#' # Identify matching columns between deployments and covariates
#' cols <- names(deps)[names(deps) %in% names(covs)]
#' # Merge deployment information to the covariates
#' covs <- merge(deps, covs, by = cols)  # Note: This overwrites the original covs data
#'
#' # Define columns for mode aggregation (example: 'projectName' and 'habitat')
#' mode_cols_covs <- names(covs)[grepl("projectName|habitat", names(covs))]
#' # Set the method for aggregating the total number of individuals detected:
#' individuals <- "sum"  # Alternative: "max"
#' # Specify observation-level covariate variables derived from deployments.
#' # These variables capture information that varies in space and time.
#' obs_covs <- c("baitUse", "featureType", "setupBy", "cameraModel", "cameraDelay","cameraHeight", "cameraDepth", "cameraTilt", "cameraHeading", "detectionDistance", "deploymentTags")
#'
#' # now spatially resample the data, noting that this function may take a few minutes to run
#' resamp_data = WildObsR::resample_covariates_and_observations(covs, obs, individuals = "sum", mode_cols_covs, obs_covs)
#'
#' # Select the resampled observations and covs at the 1 km scale
#' resamp_obs = resamp_data$spatially_resampled_observations$cellID_1km
#' resamp_covs = resamp_data$spatially_resampled_covariates$cellID_1km
#'
#' # Run the matrix generator function
#' res <- matrix_generator(
#'   obs = resamp_obs,
#'   covs = resamp_covs,
#'   dur = 110,
#'   w = 3,
#'   site_covs = c("mode_habitat", "Avg_human_footprint_10km2", "locationName", "Avg_FLII_3km2"),
#'   obs_covs = c("cameraHeight", "featureType", "cameraModel"),
#'   all_locationNames = TRUE,
#'   scientificNames = c("Orthonyx spaldingii", "Uromys caudimaculatus"),
#'   type = "abundance",
#'   individuals = "sum"
#' )
#' # Access the detection matrix for one species:
#' res[["Orthonyx_spaldingii"]][["detection_matrix"]]
#' }
#'
#' @section Known limitations:
#' These come from how the inputs are built, and are why \code{\link{effort_matrix}},
#' \code{\link{detection_matrix}} and \code{\link{model_matrix_input}} exist. Prefer them for
#' new analyses.
#' \itemize{
#'   \item \strong{Effort depends on detections.} \code{numberDeploymentsActiveAtDate} from
#'     \code{\link{resample_covariates_and_observations}} counts the deployments with a record
#'     that day, and a day without any record counts as one camera. Used as an observation
#'     covariate, it understates effort where several cameras share a sampling unit and
#'     varies with what was detected. \code{matrix_generator()} warns when it is used.
#'     \code{\link{effort_matrix}} calculates camera-days from the deployments alone.
#'   \item \strong{Observation covariates come from the species' own records.} On a day the
#'     species was detected, a covariate takes the values of the cameras that detected it; on
#'     other days, every value recorded at the sampling unit. So the covariate differs between
#'     detection and non-detection occasions for reasons unrelated to detection.
#'     \code{\link{model_matrix_input}} builds observation covariates from the deployments,
#'     weighted by camera-days.
#'   \item \strong{Character observation covariates are pasted together} (for example
#'     \code{"none - trail"}) where several values meet, and summarised by their most common
#'     value per occasion, so they enter models as many-level factors.
#'   \item \strong{A sampling unit is assumed to run continuously} from its first start to its
#'     last end, so gaps between deployments in a cell count as sampled days.
#' }
#'
#' @seealso \code{\link{effort_matrix}}, \code{\link{detection_matrix}} and
#'   \code{\link{model_matrix_input}}, which build effort from deployments and format the
#'   result for unmarked, spOccupancy and spAbundance.
#'
#' @importFrom dplyr group_by summarise distinct select all_of
#' @importFrom tidyr unnest
#' @importFrom lubridate with_tz
#' @importFrom lutz tz_lookup_coords
#' @importFrom vegan decostand
#'
#' @author Zachary Amir
#'
#' @export
matrix_generator <- function(obs, covs, dur, w, site_covs, obs_covs, all_locationNames,
                             scientificNames, type, individuals, cap_count = FALSE) {


  #
  ##
  ### Data validation ----

  ## sampling units are cells if the observations were resampled, otherwise deployments
  if (any(grepl("cellID", names(obs)))) {
    # the cell column names the rows
    row_col <- names(obs)[grepl("cellID", names(obs))]
    # and resampled covariates carry sampling, not deployment, dates
    covs_date_cols <- c("samplingStart", "samplingEnd")
  } else {
    # otherwise each deployment is a row
    row_col <- names(obs)[grepl("deploymentID", names(obs))]
    # with its own start and end, always start first
    covs_date_cols <- c("deploymentStart", "deploymentEnd")
  } # end row column condition

  ## every sampling unit with observations must have covariates
  # the sampling units in the observations but not the covariates
  missing_units <- setdiff(obs[[row_col]], covs[[row_col]])
  if (length(missing_units) > 0) {
    # stop and name a few of them
    stop(sprintf("matrix_generator() found %d %s value(s) in obs that are not in covs, e.g. %s.\n",
                 length(missing_units), row_col, paste(utils::head(missing_units, 3), collapse = ", ")),
         "Give every sampling unit in the observations a row in the covariates, or drop those ",
         "observations (obscured records, for example, belong to no deployment).", call. = FALSE)
  } # end sampling unit check

  ## site covariates must be columns of the covariates table
  if (any(!site_covs %in% names(covs))) {
    # warn about the ones that are missing
    warning("You have provided site-level covariates that are not in the covs table:",
            setdiff(site_covs, names(covs)), "so these have been remove from the function.")
    # and carry on without them
    site_covs <- site_covs[site_covs %in% names(covs)]
  } # end missing site covariate check

  ## observation covariates must be columns of the observations table
  # the ones that are not
  missing_obs_covs <- setdiff(obs_covs, names(obs))
  if (length(missing_obs_covs) > 0) {
    # stop and name them
    stop(sprintf("matrix_generator() could not find the observation covariate(s) %s in obs.\n",
                 paste(missing_obs_covs, collapse = ", ")),
         "Choose obs_covs from the columns of the observations table.", call. = FALSE)
  } # end missing observation covariate check

  ## missing site covariate values are kept, since models can handle them, but worth saying
  # the site covariates with at least one NA
  na_site_covs <- site_covs[vapply(site_covs, function(v) anyNA(covs[[v]]), logical(1))]
  if (length(na_site_covs) > 0) {
    # warn and name them
    warning(sprintf("matrix_generator() found NA values in the site covariate(s) %s; they are kept as NA.\n",
                    paste(na_site_covs, collapse = ", ")),
            "unmarked drops the sampling units with missing site covariates, with its own warning, ",
            "while spOccupancy and spAbundance stop on them, so fill or drop them for those packages.",
            call. = FALSE)
  } # end site covariate NA check

  ## the same for observation covariates
  # the observation covariates with at least one NA
  na_obs_covs <- obs_covs[vapply(obs_covs, function(v) anyNA(obs[[v]]), logical(1))]
  if (length(na_obs_covs) > 0) {
    # warn and name them
    warning(sprintf("matrix_generator() found NA values in the observation covariate(s) %s; they are kept as NA.\n",
                    paste(na_obs_covs, collapse = ", ")),
            "unmarked treats the detections at those occasions as missing, with its own warning.",
            call. = FALSE)
  } # end observation covariate NA check

  ## cameras running is not effort, since it only counts deployments with a record that day
  if ("numberDeploymentsActiveAtDate" %in% obs_covs) {
    # warn and point to the deployment-based effort
    warning("numberDeploymentsActiveAtDate counts only the deployments with a record that day, ",
            "so as an observation covariate it understates effort and depends on detections.\n",
            "Use effort_matrix() for camera-days calculated from the deployments.", call. = FALSE)
  } # end cameras running warning

  ## every requested species must be in the observations
  if (any(!scientificNames %in% obs$scientificName)) {
    # stop and name the missing ones
    stop(paste("You have provided the following species names that are not present in the observations table:",
               paste(setdiff(scientificNames, obs$scientificName), collapse = " & "),
               "\nPlease choose species names that are present in the observations table."))
  } # end species check

  ## observation times and sampling dates must be date-times; text is converted, or stops
  # observation start and end
  for (x in c("eventStart", "eventEnd")) obs[[x]] <- .as_datetime_or_stop(obs[[x]], x, "obs")
  # sampling start and end
  for (x in covs_date_cols) covs[[x]] <- .as_datetime_or_stop(covs[[x]], x, "covs")
  ## TODO: add more checks as they come up


  #
  ##
  ### Prepare the data ----

  ## put every date-time in the local time zone, since frictionless reads them as UTC
  # resampled covariates carry each cell's average coordinates, deployments their own
  lat_col <- if ("Avg_latitude" %in% names(covs)) "Avg_latitude" else "latitude"
  lon_col <- if ("Avg_longitude" %in% names(covs)) "Avg_longitude" else "longitude"
  # the time zone needs coordinates
  if (!all(c(lat_col, lon_col) %in% names(covs))) {
    stop("matrix_generator() needs coordinates in covs to find the local time zone.\n",
         "Keep latitude and longitude (or Avg_latitude and Avg_longitude) in the covariates.",
         call. = FALSE)
  } # end coordinate check
  # the distinct time zones the sampling units sit in, as effort_matrix() finds them
  zones <- .camera_time_zones(data.frame(latitude = covs[[lat_col]], longitude = covs[[lon_col]],
                                         deploymentStart = covs[[covs_date_cols[1]]],
                                         deploymentEnd = covs[[covs_date_cols[2]]]))
  # one zone gives local days; several, or none found, give consistent UTC days
  tz <- if (length(zones) == 1) zones else "UTC"
  # say so when several zones force UTC
  if (length(zones) > 1) {
    message(sprintf("matrix_generator(): the sampling units span %d time zones (%s), so days run midnight to midnight UTC for all of them.",
                    length(zones), paste(zones, collapse = ", ")))
  } # end several zones message
  # observation start and end
  obs$eventEnd <- lubridate::with_tz(obs$eventEnd, tzone = tz)
  obs$eventStart <- lubridate::with_tz(obs$eventStart, tzone = tz)
  # sampling start and end
  covs[[covs_date_cols[1]]] <- lubridate::with_tz(covs[[covs_date_cols[1]]], tzone = tz)
  covs[[covs_date_cols[2]]] <- lubridate::with_tz(covs[[covs_date_cols[2]]], tzone = tz)
  # the local day of each observation; as.Date() would otherwise take the UTC day
  obs$date <- as.Date(obs$eventStart, tz = tz)

  ## the landscape of each sampling unit, for keeping only the landscapes a species was seen in
  if (!all_locationNames) {
    # which needs the landscapes
    if (!"locationName" %in% names(covs)) {
      stop("matrix_generator() needs a locationName column in covs when all_locationNames = FALSE.\n",
           "Keep locationName in the covariates, or set all_locationNames = TRUE.", call. = FALSE)
    } # end locationName check
    # one row per sampling unit and its landscape
    su_land <- dplyr::distinct(data.frame(covs[, c(row_col, "locationName")]))
  } # end landscape condition

  ### Clean and standardise the site covariates
  ## keep only the sampling unit, its dates and the site covariates
  covs_dat <- covs[, c(row_col, covs_date_cols, site_covs)]
  # the numeric covariates
  m_num <- data.frame(covs_dat[, vapply(covs_dat, is.numeric, logical(1)), drop = FALSE])
  # the date-times
  m_date <- covs_dat[, vapply(covs_dat, function(x) inherits(x, "POSIXct"), logical(1))]
  # standardise the numeric covariates so they are comparable across models
  m_std2 <- data.frame(suppressWarnings(vegan::decostand(m_num, method = "standardize", na.rm = TRUE)))
  # a covariate with one value everywhere has no spread to scale by, so it sits at its mean, 0
  constant <- vapply(m_num, function(v) length(unique(v[!is.na(v)])) == 1, logical(1))
  for (v in names(m_num)[constant]) m_std2[[v]] <- ifelse(is.na(m_num[[v]]), NA, 0)

  ## the character, logical and factor covariates, all kept whether or not they vary
  m_char <- data.frame(covs_dat[, vapply(covs_dat, function(x) is.character(x) || is.logical(x) || is.factor(x),
                                         logical(1)), drop = FALSE])
  ## put the character, date and standardised numeric covariates back together
  covs_dat <- data.frame(m_char, m_date, m_std2)

  ### Number every day each sampling unit ran
  ## one row per sampling unit with its start and end dates
  s <- dplyr::distinct(dplyr::select(covs_dat, all_of(row_col), all_of(covs_date_cols)))
  # as local dates rather than date-times
  s[[covs_date_cols[1]]] <- as.Date(s[[covs_date_cols[1]]], tz = tz)
  s[[covs_date_cols[2]]] <- as.Date(s[[covs_date_cols[2]]], tz = tz)

  ## one row per sampling unit per day, from its first start to its last end
  s2 <- covs_dat |>
    # per sampling unit
    dplyr::group_by(dplyr::across(dplyr::all_of(row_col))) |>
    # the list of local days it spans
    dplyr::summarise(
      date = list(seq.Date(
        from = as.Date(min(.data[[covs_date_cols[grepl("Start", covs_date_cols)]]]), tz = tz),
        to   = as.Date(max(.data[[covs_date_cols[grepl("End", covs_date_cols)]]]), tz = tz),
        by   = "day"
      )), .groups = "drop"
    ) |>
    # spread out to one row per day
    tidyr::unnest(cols = date)

  ## add the start and end dates to each day
  t <- merge(s2, s, by = row_col)

  ## number the days of each sampling unit from 1
  # an empty table to fill
  res <- t[0, ]
  res$seq <- numeric()
  # for each sampling unit
  for (i in unique(t[[row_col]])) {
    # grab its days
    d <- t[t[[row_col]] == i, ]
    # and number them 1 to the number of days it ran
    d$seq <- as.numeric(seq(from = 1,
                            # plus one, so the count starts at 1 rather than 0
                            to = unique(as.numeric(difftime(d[[covs_date_cols[grepl("End", covs_date_cols)]]] + 1,
                                                            d[[covs_date_cols[grepl("Start", covs_date_cols)]]],
                                                            units = "days"))),
                            by = 1))
    # add to the rest
    res <- rbind(res, d)
  } # end per sampling unit

  ## sampling units with covariates but no observations ran with nothing detected
  # which ones they are
  empty_units <- setdiff(unique(res[[row_col]]), obs[[row_col]])
  if (length(empty_units) > 0) {
    # say so, since they enter every species' matrix as zeros
    message(sprintf("%d sampling unit(s) have no observations and enter the matrices as zeros: %s",
                    length(empty_units), paste(utils::head(empty_units, 5), collapse = ", ")))
  } # end empty units message

  ## give each observation its day number
  obs_dat <- merge(obs, res, by = c(row_col, "date"))

  ## the merge must not lose or add observations
  if (nrow(obs) != nrow(obs_dat)) {
    # fewer rows: some observations fell outside the sampling dates
    if (nrow(obs_dat) < nrow(obs)) {
      # how many are left
      percent <- round(nrow(obs_dat) / nrow(obs) * 100, 2)
      # warn with the share
      warning("Adding the sampling occasion index to the observations has removed", percent,
              "percent of rows to the observations.")
    } else {
      # more rows means a sampling unit was duplicated
      percent <- round(nrow(obs) / nrow(obs_dat) * 100, 2)
      # which is a hard stop
      stop("Adding the sampling occasion index to the observations has added", percent,
           "percent of rows to the observations. This is not good, there should not be extra rows here.")
    } # end fewer or more condition
  } # end row check condition

  ## resampled observations count individuals in totalIndividuals, Camtrap DP in count
  count_col <- ifelse(any(grepl("Individuals", names(obs))), "totalIndividuals", "count")


  #
  ##
  ### Build the matrices for each species ----

  # store each species' results here
  sp_list <- list()
  # for each species
  for (y in 1:length(scientificNames)) {
    # select one species
    sp <- scientificNames[y]

    ## keep only the landscapes where it was detected, if asked to
    if (!all_locationNames) {
      # the sampling units with a detection
      locs <- unique(obs_dat[[row_col]][obs_dat$scientificName == sp])
      # the landscapes they sit in
      land <- unique(su_land$locationName[su_land[[row_col]] %in% locs])
      # and every sampling unit in those landscapes
      keep_units <- su_land[[row_col]][su_land$locationName %in% land]
    } else {
      # otherwise every sampling unit
      keep_units <- unique(covs_dat[[row_col]])
    } # end locationNames condition
    # the covariates, observations and days of those sampling units
    covs_land <- covs_dat[covs_dat[[row_col]] %in% keep_units, ]
    obs_land <- obs_dat[obs_dat[[row_col]] %in% keep_units, ]
    res_land <- res[res[[row_col]] %in% keep_units, ]

    ## the rows of every matrix for this species
    # the sampling units, in the order of the covariates
    su_ids <- as.character(unique(covs_land[[row_col]]))
    # the number of days each one ran, to its end rather than its last record
    su_days <- tapply(res_land$seq, as.character(res_land[[row_col]]), max)[su_ids]
    # and the longest of them
    n_days <- max(su_days)


    #
    ##
    ### Daily detection or count history ----

    ## an empty matrix: one row per sampling unit, one column per day
    mat <- matrix(NA,
                  # rows are the sampling units
                  nrow = length(su_ids),
                  # columns are the days
                  ncol = n_days,
                  # named by sampling unit and day
                  dimnames = list(su_ids, seq_len(n_days)))

    ## mark every day each sampling unit ran with a zero
    for (j in seq_along(su_ids)) {
      # are zeros until a detection fills them
      mat[j, seq_len(su_days[j])] <- 0
    } # end per sampling unit

    ## work out this species' count cap, if asked to
    if (!is.null(cap_count) && isTRUE(cap_count)) {
      # its counts
      sp_counts <- obs_land[[count_col]][obs_land$scientificName == sp]
      # leaving out missing and zero counts, which would skew the average
      sp_counts <- sp_counts[!is.na(sp_counts) & sp_counts > 0]
      # the mean and maximum group size
      mean_count <- mean(sp_counts)
      max_count <- max(sp_counts)
      # larger groups get a tighter cap
      cap_val <- if (mean_count > 10 || max_count > 20) {
        7
      } else if (mean_count > 5 || max_count > 10) {
        5
      } else if (mean_count > 3 || max_count > 5) {
        3
      } else {
        Inf
      } # end cap rules
    } # end cap condition

    ## fill in the days with a detection
    # for each sampling unit
    for (su in su_ids) {
      # its records of this species
      a <- obs_land[obs_land[[row_col]] == su & obs_land$scientificName == sp, ]

      ## counts, for abundance; sampling units with no detection stay zero
      if (nrow(a) > 0 & type == "abundance") {
        # for each day with a detection
        for (s in 1:length(a$date)) {
          # the day
          d <- a$date[s]
          # its day number
          indx <- a$seq[a[[row_col]] == su & a$date == d]
          # the count that day, the largest if there is more than one
          fill_count <- max(unique(a[[count_col]][a$date == d]))
          # capped, if asked to
          if (!is.null(cap_count) && isTRUE(cap_count)) {
            # the smaller of the count and the cap
            fill_count <- min(fill_count, cap_val)
            # goes in that sampling unit and day
            mat[su, indx] <- fill_count
          } else {
            # otherwise the full count goes in
            mat[su, indx] <- fill_count
          } # end cap condition
        } # end per day
      } # end abundance condition

      ## presence, for occupancy; sampling units with no detection stay zero
      if (nrow(a) > 0 & type == "occupancy") {
        # for each day with a detection
        for (s in 1:length(a$date)) {
          # the day
          d <- a$date[s]
          # its day number
          indx <- a$seq[a[[row_col]] == su & a$date == d]
          # mark the species present
          mat[su, indx] <- 1
        } # end per day
      } # end occupancy condition
    } # end per sampling unit


    #
    ##
    ### Compress the daily history into sampling occasions ----

    ## no longer than the longest-running sampling unit, for this species only
    dur_sp <- min(dur, n_days)
    ## non-overlapping windows of w days, shared by detections and observation covariates
    # the first day of each window
    starts <- seq.int(1L, dur_sp, by = w)
    # the days in each, with the last window cut at dur_sp
    sampling_windows <- lapply(starts, function(start) {
      seq.int(start, min(start + w - 1L, dur_sp))
    })
    # how many occasions that makes
    n_occasions <- length(sampling_windows)

    ## an empty matrix: one row per sampling unit, one column per occasion
    dh_mat <- matrix(nrow = nrow(mat),
                     # the final partial window is an occasion too
                     ncol = n_occasions,
                     # named by sampling unit and occasion number
                     dimnames = list(su_ids, seq_len(n_occasions)))
    # for each sampling unit
    for (u in 1:nrow(mat)) {
      # for each occasion
      for (p in seq_len(n_occasions)) {
        # the days in this occasion
        window_days <- sampling_windows[[p]]
        ## an occasion with no camera running stays NA
        if (all(is.na(mat[u, window_days]))) {
          dh_mat[u, p] <- NA
        } else if (type == "abundance" && individuals == "sum") {
          # otherwise sum the daily counts
          dh_mat[u, p] <- sum(as.numeric(mat[u, window_days]), na.rm = TRUE)
        } else if (type == "abundance" && individuals == "max") {
          # or take the largest daily count
          dh_mat[u, p] <- max(as.numeric(mat[u, window_days]), na.rm = TRUE)
        } else if (type == "occupancy") {
          # or record whether it was seen at all
          dh_mat[u, p] <- max(as.numeric(mat[u, window_days]), na.rm = TRUE)
        } # end occasion condition
      } # end per occasion
    } # end per sampling unit


    #
    ##
    ### Observation covariate matrices ----

    ## keep only the observation covariates that vary for this species
    obs_covs_thin <- character(0)
    if (length(obs_covs) > 0) {
      # count the distinct values of each one on this species' records
      n_levels <- vapply(obs_covs, function(v) {
        # its values on this species' records
        o_dat <- obs_land[[v]][obs_land$scientificName == sp]
        # and the number of distinct ones
        length(unique(o_dat[o_dat != "no_detection"]))
      }, integer(1))
      # the ones that vary
      obs_covs_thin <- obs_covs[n_levels > 1]
      # warn about the ones that do not
      if (length(obs_covs_thin) < length(obs_covs)) {
        warning(paste("There was no variation in the observation-level covariate:",
                      paste(setdiff(obs_covs, obs_covs_thin), collapse = " & "),
                      "for the species:", sp,
                      "so it has been removed from the matrix generation function for this species."))
      } # end invariant covariate warning
    } # end observation covariate condition

    ## no observation covariates left means an empty list, but the species is still returned
    if (length(obs_covs_thin) == 0) {
      comp_mat_list <- list()
    } else {

      ## the number of cameras running, which applies to every species
      obs_land_full <- dplyr::distinct(dplyr::select(obs_land, all_of(row_col),
                                                     all_of(obs_covs_thin[grepl("ActiveAtDate", obs_covs_thin)]),
                                                     seq))
      ## and the covariates of this species' own records
      sp_obs <- dplyr::distinct(dplyr::select(obs_land[obs_land$scientificName == sp, ], all_of(row_col), seq,
                                              all_of(obs_covs_thin[!grepl("ActiveAtDate", obs_covs_thin)])))
      ## joined, leaving NA on days without this species
      obs_land_full <- merge(sp_obs, obs_land_full, by = c(row_col, "seq"), all.y = TRUE)

      ## one daily matrix per observation covariate
      obs_mat_list <- list()
      # the covariate columns
      o_vars <- names(obs_land_full)[!names(obs_land_full) %in% c(row_col, "seq")]
      # for each covariate
      for (v in 1:length(o_vars)) {
        # its name
        o_var <- o_vars[v]
        ## an empty daily matrix with the same rows and days as the detections
        obs_mat <- matrix(NA, nrow = length(su_ids), ncol = n_days,
                          dimnames = list(su_ids, seq_len(n_days)))

        # for each sampling unit
        for (su in su_ids) {
          # its values of this covariate on any record, for days without this species
          su_values <- unique(obs_land[[o_var]][obs_land[[row_col]] == su])

          # for each day it ran, to its end rather than its last record
          for (indx in seq_len(su_days[[su]])) {
            # the covariate value(s) that day
            n <- unique(obs_land_full[obs_land_full[[row_col]] == su & obs_land_full$seq == indx, o_var])

            ## cameras running: a day without a record counts as one camera
            if (length(n) == 0 & grepl("ActiveAtDate", o_var)) {
              n <- 1
            } # end no record condition
            ## cameras running: several values that day are summed
            if (length(n) > 1 & grepl("ActiveAtDate", o_var)) {
              n <- sum(n)
            } # end several values condition

            ## other covariates: several values that day are pasted together
            if (length(n) > 1 & !grepl("ActiveAtDate", o_var)) {
              # split any already-pasted values, so no new combinations appear
              split_n <- unlist(strsplit(n, " - "))
              # and paste the distinct ones
              n <- paste(sort(unique(split_n)), collapse = " - ")
            } # end several values condition

            ## other covariates: a day without this species, or a missing value,
            ## takes every value recorded at the sampling unit
            if ((length(n) == 0 || any(is.na(n))) & !grepl("ActiveAtDate", o_var)) {
              # split any already-pasted values, so no new combinations appear
              split_n <- unlist(strsplit(su_values, " - "))
              # and paste the distinct ones, or NA where the sampling unit has no records at all
              n <- if (length(split_n) > 0) paste(sort(unique(split_n)), collapse = " - ") else NA
            } # end no record condition

            ## the value goes in that sampling unit and day
            obs_mat[su, indx] <- n
          } # end per day
        } # end per sampling unit

        ## save it under the covariate's name
        obs_mat_list[[v]] <- obs_mat
        names(obs_mat_list)[v] <- o_var
      } # end per observation covariate


      #
      ##
      ### Compress the covariate matrices into sampling occasions ----

      # store the compressed matrices here
      comp_mat_list <- list()
      # for each covariate
      for (v in 1:length(obs_mat_list)) {
        # its daily matrix
        obs_mat <- obs_mat_list[[v]]
        # and its name
        o_var <- names(obs_mat_list)[v]
        ## an empty matrix: one row per sampling unit, one column per occasion
        obs2 <- matrix(nrow = nrow(obs_mat),
                       # the final partial window is an occasion too
                       ncol = n_occasions,
                       # named by sampling unit and occasion number
                       dimnames = list(su_ids, seq_len(n_occasions)))

        # for each sampling unit
        for (u in 1:nrow(obs_mat)) {
          # for each occasion
          for (p in seq_len(n_occasions)) {
            # the days in this occasion
            window_days <- sampling_windows[[p]]

            ## an occasion with no values stays NA
            if (all(is.na(obs_mat[u, window_days]))) {
              obs2[u, p] <- NA
            } else if (grepl("ActiveAtDate", o_var)) {
              # cameras running are summed over the occasion
              obs2[u, p] <- sum(as.numeric(obs_mat[u, window_days]), na.rm = TRUE)
            } else {
              # other covariates take their most common value
              obs2[u, p] <- Mode(obs_mat[u, window_days])
            } # end occasion condition
          } # end per occasion
        } # end per sampling unit

        ## numeric covariates are zero where no camera ran, rather than NA
        if (is.numeric(obs_land_full[[o_var]])) {
          obs2[is.na(obs2)] <- 0
        } # end numeric condition

        ## save it under the covariate's name
        comp_mat_list[[v]] <- obs2
        names(comp_mat_list)[v] <- names(obs_mat_list)[v]
      } # end per covariate
    } # end observation covariate condition

    ## the site covariates of these sampling units, in the same order as the matrix rows
    site_level <- covs_land[match(su_ids, as.character(covs_land[[row_col]])), , drop = FALSE]
    rownames(site_level) <- NULL

    ## bundle the matrices for this species
    sp_bundle <- list("detection_matrix" = dh_mat,
                      "site_level_covariates" = site_level,
                      "observation_level_covariates" = comp_mat_list)
    # and store them under its name, with underscores for spaces
    sp_list[[y]] <- sp_bundle
    names(sp_list)[y] <- gsub(" ", "_", sp)
  } # end per species

  ## every species' matrices
  return(sp_list)
} # end function


#' Make a date-time column POSIXct, converting text, or stop
#'
#' @param x The column.
#' @param name Its name, for messages.
#' @param table `"obs"` or `"covs"`, for messages.
#' @return `x` as POSIXct. Text is read as ISO 8601 date-times; text without a UTC offset is
#'   read as UTC. Stops, naming an example, if any value cannot be read.
#' @author Zachary Amir
#' @keywords internal
#' @noRd
.as_datetime_or_stop <- function(x, name, table) {
  # already a date-time
  if (inherits(x, "POSIXct")) return(x)
  # read text such as 2023-02-01 10:00:00 or 2023-02-01T10:00:00+10:00, dates included
  out <- suppressWarnings(lubridate::ymd_hms(as.character(x), truncated = 3, quiet = TRUE, tz = "UTC"))
  # the values that were there but could not be read
  failed <- is.na(out) & !is.na(x)
  # if any, stop loudly rather than turn them into missing dates
  if (any(failed)) {
    stop(sprintf("matrix_generator() could not read %d value(s) of %s in %s as date-times, e.g. \"%s\".\n",
                 sum(failed), name, table, as.character(x)[which(failed)[1]]),
         "Read the tables with frictionless::read_resource(), or convert the column with as.POSIXct().",
         call. = FALSE)
  } # end failed conversion check
  # say that it was converted, and how
  message(sprintf("matrix_generator(): converted %s in %s from text to date-times; text without a UTC offset is read as UTC.",
                  name, table))
  out
} # end .as_datetime_or_stop
