#' Spatially resample the covariates and observations resources
#' Spatially Resample Covariates and Observations Across Spatial Scales
#'
#' @description
#' This function resamples covariate and observation data across spatial scales generated from the \code{WildObsR::spatial_hexagon_generator()} function. It validates the input data by ensuring that all necessary spatial, temporal, and deployment identifiers are present and correctly formatted. Numeric covariates are aggregated using means weighted by each deployment's camera-days (see `weights`), while categorical covariates are combined by concatenating unique values. Logical and factor columns in either table, such as `baitUse` and `featureType` from [frictionless::read_resource()], keep their class (and factor levels) when every cell holds one value; where some cell holds several, the column is pasted as text, for example `"FALSE - TRUE"`, with a message naming it. Modal columns always keep their class.
#'
#' With `obs = NULL`, only the covariates are resampled, which is all the occupancy and
#' abundance workflow needs, and is much faster (see "How the tables connect").
#'
#' In addition, the function computes several key metrics:
#' \itemize{
#'   \item Sampling effort per spatial unit (trap-nights),
#'   \item Resampled start and end dates for each sampling unit.
#' }
#' Users can optionally specify which covariate columns should be aggregated using the mode (i.e., the most frequent value) instead of concatenation, and which covariate variables that vary in space and time (i.e., observation covariates) should be incorporated into the resampled observations. Example values provided in the documentation
#' (\code{c("habitat")} for mode aggregation and \code{c("baitUse", "featureType")} for observation covariates) serve as examples that work with any camtrap DP formatted data.
#'
#' The function generates new columns in the output data as follows:
#'
#' \strong{Resampled Covariates} include:
#' \describe{
#'   \item{\code{deploymentsIncluded}}{A concatenated string of \code{deploymentID} values representing all deployments included within each spatial cell.}
#'   \item{\code{cellEffort}}{The total sampling effort per cell, calculated as trap-nights (i.e., the number of active deployments times the duration they were active, in days).}
#'   \item{\code{samplingStart}}{The earliest \code{deploymentStart} date-time among the deployments in a cell, marking the beginning of the sampling period per cellID.}
#'   \item{\code{samplingEnd}}{The latest \code{deploymentEnd} date-time among the deployments in a cell, marking the end of the sampling period per cellID.}
#' }
#'
#' \strong{Resampled Observations} include:
#' \describe{
#'   \item{\code{deploymentsActiveAtDate}}{A concatenated string of \code{deploymentID} values that were active in a given resampled cellID on each day.}
#'   \item{\code{numberDeploymentsActiveAtDate}}{The number of deployments active in a given resampled cell on each day.}
#'   \item{\code{independentEvents}}{The total count of unique \code{eventID} values per cell per day per species, representing independent events.}
#'   \item{\code{independentObservations}}{The total count of unique \code{observationID} values per cell per day per species.}
#'   \item{\code{totalIndividuals}}{The aggregated count of individuals per cell per day per species, computed based on the \code{individuals} parameter (either by summing counts or taking the maximum value).}
#' }
#'
#' @param covs A data frame or tibble of covariate data from a camtrapDP formatted frictionless data package, which can be accessed from the \code{WildObsR::wildobs_dp_download()} function. It must include columns defining spatial cells (with names starting with "cellID"), and date-time columns (e.g., \code{deploymentStart} and \code{deploymentEnd}) formatted as POSIXct.
#' @param obs A data frame or tibble of observation data from a camtrapDP formatted frictionless data package, which can be accessed from the \code{WildObsR::wildobs_dp_download()} function. It must include a \code{deploymentID} column and several date-time columns (e.g., \code{eventStart}, \code{eventEnd}, \code{classificationTimestamp} formatted as POSIXct. Or `NULL` (default) to resample the covariates only; `individuals` and `obs_covs` are then not used.
#' @param individuals A character string specifying how to aggregate the number of individuals counted per spatially re sampled cell per day. Options are \code{"sum"} to take the sum total number of counts or  \code{"max"} to use the maximum count. `"sum"` adds the count of every observation of the species in the cell that day, across all its cameras and repeat triggers, so one cat triggering a camera three times in 10 minutes counts 3, and a cat passing two cameras counts 2: an index of animal detections, not a number of individuals. `"max"` is the most individuals in a single observation, a conservative minimum present. This only affects the resampled observations; [detection_matrix()] counts from the observations themselves, with its own `individuals`.
#' @param mode_cols_covs (Optional) A character vector specifying the names of covariate columns for which the mode should be computed rather than concatenating all unique values. (Example: \code{c("habitat")}). With `weights = "camera_days"`, the mode is the value covering the most camera-days in the cell.
#' @param obs_covs (Optional) A character vector specifying observation-level covariate column names that may vary in space and time and will be combined with the resampled observations. (Example: \code{c("baitUse", "featureType")}). These are used only by [matrix_generator()]; for detection covariates with [effort_matrix()], use its `split_by` instead.
#' @param weights How each deployment counts towards its cell's averaged (`Avg_*`) and modal
#'   (`mode_*`) covariates. `"camera_days"` (default) weights each deployment by how long it
#'   ran, `deploymentEnd - deploymentStart` in days; `"none"` counts every deployment once,
#'   the plain mean and most frequent value. See "Weighting by camera-days".
#'
#' @return A nested list with two elements:
#' \describe{
#'   \item{`spatially_resampled_observations`}{A list of data frames corresponding to different spatial scales (e.g., "cellID_1km", "cellID_10km") containing all resampled observation data. Left out when `obs = NULL`.}
#'   \item{`spatially_resampled_covariates`}{A list of data frames corresponding to different spatial scales containing all resampled covariate data, one row per cell.}
#' }
#' Each table carries a `wildobs_resampled` attribute naming its scale, so [effort_matrix()]
#' and [detection_matrix()] can recognise them and say what to pass instead.
#'
#' @section How the tables connect:
#' The occupancy and abundance workflow uses three tables, each at the level its function
#' needs, joined by two keys: `deploymentID` links observations to deployments, and the
#' cellID column (for example `cellID_3km2`) links deployments to cells.
#'
#' | Table | One row per | Made by | Goes into |
#' |---|---|---|---|
#' | Deployments with cellIDs (`covs_cellID`) | deployment | [spatial_hexagon_generator()] | [effort_matrix()] with `site_col = "cellID_3km2"` |
#' | Observations | observation | [frictionless::read_resource()] | [detection_matrix()], which places each one in its cell through the effort matrix |
#' | Cell covariates | cell | this function, with `obs = NULL` | [model_matrix_input()] as `site_covs` |
#'
#' Effort needs each camera's own start and end, which only the deployment table keeps, so
#' the resampled covariates are used for site covariates only. The resampled observations
#' are for [matrix_generator()] and for summaries per cell and day.
#'
#' @section Weighting by camera-days:
#' A covariate that does not change over time, such as elevation, still differs between
#' the cameras in a cell, and the cell's value is an average over them. Weighting by
#' camera-days makes that average describe the habitat the cameras actually sampled, the
#' same camera-days the effort and detections come from. Two examples:
#' \itemize{
#'   \item Camera A at 200 m ran 90 days; camera B at 800 m failed after 2 days. The plain
#'     mean is 500 m; the weighted mean is (200 x 90 + 800 x 2) / 92 = 213 m, where nearly
#'     all the detections came from.
#'   \item Station X at 200 m was redeployed four times for 25 days each; station Y at
#'     800 m ran once for 100 days. The plain mean over the five rows is 320 m, counting X
#'     four times only because its cards were changed more often; the weighted mean is
#'     500 m, treating both stations' equal effort equally.
#' }
#' When the cameras in a cell ran for similar lengths the two agree closely: on three
#' public WildObs projects at 3 km² cells, weighting moved values by a median of under 5%
#' of the spread between cameras in the same cell. The difference matters for failed
#' cameras, redeployed stations and large cells.
#'
#' Coordinates (`latitude`, `longitude`) are always plain means, since they describe where
#' the cameras are rather than what they sampled. A deployment with a missing or negative
#' length has no weight; a cell where no deployment has any falls back to the plain mean,
#' with a warning. `cellEffort` is unaffected: it is always the total camera-days.
#'
#'@details
#' The function performs the following steps to resample the covariate and observation data:
#'
#' 1. **Data Validation**:
#'    - Verifies that the covariates data frame contains at least one spatial cellID column generated from the \code{WildObsR::spatial_hexagon_generator()} function (i.e., a column whose name starts with "cellID"). If not, the function stops with an error.
#'    - Checks that the \code{deploymentID} values match between the covariates and observation data. If there is a mismatch, the function stops with an error.
#'    - Ensures that the specified observation-level covariate columns (provided in \code{obs_covs}) exist in the covariates data. If some are missing, they are omitted from further processing with a warning.
#'    - Confirms that all required date-time columns in both datasets are formatted as \code{POSIXct}. This includes columns such as \code{eventStart}, \code{eventEnd}, and \code{classificationTimestamp} in the observations, and \code{deploymentStart} and \code{deploymentEnd} in the covariates.
#'
#' 2. **Data Preparation**:
#'    - Removes any columns that contain only \code{NA} values from both datasets.
#'    - For the covariates:
#'         - Extracts character columns (excluding those that will be processed by mode aggregation) and numeric columns that will be averaged.
#'         - Adds sequential Julian date values derived from \code{deploymentStart} and \code{deploymentEnd} to standardize the temporal resolution. These columns are removed before returning the final datasets.
#'    - For the observations:
#'         - Extracts date-time columns (identified by the \code{POSIXct} class), character/factor columns, and numeric columns (excluding those with names like "deltaTime" or "count").
#'         - Computes a sequential Julian date from \code{eventStart} to serve as the daily resolution for resampling. This column is removed before returning the final datasets.
#'
#' 3. **Resampling by Deployment Group and Spatial Scale**:
#'    - The function loops over each unique \code{deploymentGroup} present in the covariates data. This speeds up the process.
#'    - For each \code{deploymentGroup}:
#'         - Subsets the covariates and observations to include only the relevant deployments.
#'         - Merges spatial cell information (i.e., the \code{cellID} columns) from the covariates into the observations based on the \code{deploymentID}.
#'
#' 4. **Resampling of Covariates**:
#'    - Within each \code{deploymentGroup}, the function iterates over each spatial scale (columns containing "cellID_").
#'    - For each spatial scale, it loops through each unique sampling unit (i.e., each unique cell value):
#'         - A new row is created for each sampling unit, starting as an empty data frame with the same structure as the covariates data.
#'         - Numeric covariate values are aggregated by computing the mean, weighted by camera-days unless `weights = "none"` (or left as \code{NA} if all values are missing). The column names are updated to indicate that these are averaged values (e.g., \code{"Avg_<column>"}).
#'         - Sampling effort is calculated as the total active duration (in days) summed over all unique deployments in the cell.
#'         - The sampling period is defined by the minimum \code{deploymentStart} (as \code{samplingStart}) and maximum \code{deploymentEnd} (as \code{samplingEnd}) for the cell.
#'         - Character variables are processed by pasting together sorted unique values.
#'         - For any covariate specified in \code{mode_cols_covs}, the function calculates the mode (the value covering the most camera-days, or the most frequent value with `weights = "none"`) instead of concatenating all values.
#'         - Spatial information is preserved by keeping the cell identifier and any associated polygon geometry.
#'
#' 5. **Resampling of Observations**:
#'    - For each spatial scale, the function further processes the observations by iterating over each sampling unit, each day (based on the sequential Julian date), and each species (\code{scientificName}).
#'    - For each combination:
#'         - The function merges relevant observation-level covariate information from the covariates dataset.
#'         - Date-time columns are aggregated: start times are set to the minimum value and end times to the maximum.
#'         - Active deployment information is compiled, including a concatenated list of active deployments and the count of these deployments.
#'         - Event-level metrics are computed, such as the number of unique events and observations.
#'         - Individual counts are aggregated according to the user-specified method (either \code{"sum"} or \code{"max"}).
#'         - Species-level variables are aggregated by concatenating sorted unique values.
#'         - Numeric observation columns are averaged.
#'
#' 6. **Compilation of Resampled Data**:
#'    - After processing all \code{deploymentGroups} and spatial scales, a final verification is implemented to ensure all cellID values are present and matching in the resampled covariates and observations.
#'    - Finally, the function assembles the resampled data into two nested lists:
#'         - One list contains the resampled covariates, organized by spatial scale.
#'         - The other list contains the resampled observations, also organized by spatial scale.
#'
#' 7. **Output**:
#'    - The function returns a single list with two elements:
#'         - \code{"spatially resampled observations"}: a nested list of data frames for each spatial scale.
#'         - \code{"spatially resampled covariates"}: a nested list of data frames for each spatial scale.
#'
#' @examples
#' # load the baited Paluma project shipped with WildObsR
#' dp <- example_datapackages("QLD_Paluma_cassowary_Mclean_2023-24_WildObsID_0006")[[1]]
#'
#' # extract one table of deployments and their covariates
#' deps <- frictionless::read_resource(dp, "deployments")
#' covs <- frictionless::read_resource(dp, "covariates")
#' # merge both tables together based on several shared columns 
#' covs <- merge(deps, covs, by = intersect(names(deps), names(covs)))
#'
#' # assign every camera to a 1 km2 hexagonal cell (scales are cell areas in m2)
#' covs_cellID <- spatial_hexagon_generator(covs, scales = 1e6)
#'
#' # for occupancy and abundance models, resample the covariates only: one row per cell
#' cell <- resample_covariates_and_observations(covs_cellID, mode_cols_covs = "baitUse")
#' head(cell$spatially_resampled_covariates$cellID_1km2[, c("cellID_1km2", "cellEffort", "mode_baitUse")])
#'
#' # observations that can be placed on a camera (obscured records cannot)
#' obs <- frictionless::read_resource(dp, "observations")
#' obs <- obs[obs$deploymentID %in% covs_cellID$deploymentID, ]
#'
#' # or resample the observations to cells and days too, for matrix_generator() or summaries
#' resamp <- resample_covariates_and_observations(covs_cellID, obs, individuals = "sum",
#'                                                obs_covs = c("baitUse", "featureType"))
#' head(resamp$spatially_resampled_observations$cellID_1km2[, c("cellID_1km2", "scientificName",
#'                                                              "totalIndividuals")])
#'
#' @importFrom dplyr select bind_rows
#' @importFrom purrr map
#' @importFrom stringr str_split
#'
#' @author Zachary Amir
#'
#' @export
resample_covariates_and_observations <- function(covs, obs = NULL, individuals, mode_cols_covs, obs_covs,
                                                 weights = c("camera_days", "none")){

  ## settle how deployments are weighted within a cell
  weights <- match.arg(weights)
  # without observations, only the covariates are resampled
  covs_only <- is.null(obs)

  #
  ##
  ###
  #### Data validation ----

  ## Make sure the covariates has cellID values
  if(!any(grepl("cellID", names(covs)))){
    stop("The covariates you have provided in this function do not contain any spatially delimited cells defined in a column starting with 'cellID'. Please run the function WildObsR::spatial_hexagon_generator to generate cells based off your spatial scales of interest.\n")
  }
  ## Make sure deploymentID match between obs and covs
  if(!covs_only && length(setdiff(obs$deploymentID, covs$deploymentID)) != 0){
    stop("The covariates and observations tables you have provided in this function contain mis-matched deploymentID values! Please inspect your deploymentID values or reduce each table to only the matching values.\n")
  }
  # Check if mode_cols_covs was provided. If not, leave it as missing (or assign an empty vector if that makes sense).
  if (missing(mode_cols_covs)) {
    # assign it as a char vector with length 0
    mode_cols_covs <- character(0)
  }
  # same for obs_covs
  if (missing(obs_covs)) {
    obs_covs <- character(0)
  }
  ## observation covariates only go into the resampled observations, so without obs they do nothing
  if (covs_only && length(obs_covs) > 0) {
    # say so, then ignore them
    warning("resample_covariates_and_observations() ignored obs_covs, because obs = NULL resamples the covariates only.\n",
            "For detection covariates in the new workflow, use effort_matrix(split_by = ...).", call. = FALSE)
    obs_covs <- character(0)
  } # end ignored obs_covs condition
  ## Make sure obs_covs are actually present in the deployments, thin if not.
  if(length(obs_covs) > 0 & !any(obs_covs %in% names(covs))){
    # thin to match
    obs_covs = obs_covs[obs_covs %in% names(covs)]
    warning("You have provided observation-level covariates that are not present in the covariates resource. The observation-level covariates have been reduced to include only values present in the covariates resource.\n")
  }
  ## Make sure date columns are formatted as posixct
  if(!covs_only && !any(sapply(obs[, c("eventStart","eventEnd","classificationTimestamp")],
                 function(x) inherits(x, "POSIXct")))){
    stop("The observations table you provided in this function does not have date-time columns (i.e.,eventStart, eventEnd, &  classificationTimestamp) formatted in POSIXct class. Please format these date-time columns to a standard format (%Y-%m-%d %H:%M:%S) using the as.posixct() function before using this function.\n ")
  }
  # same for covs
  if(!any(sapply(covs[, c("deploymentStart", "deploymentEnd")],
                 function(x) inherits(x, "POSIXct")))){
    stop("The covariates table you provided in this function does not have date-time columns (i.e.,deploymentStart, & deploymentEnd) formatted in POSIXct class. Please format these date-time columns to a standard format (%Y-%m-%d %H:%M:%S) using the as.posixct() function before using this function.\n ")
  }


  #
  ##
  ###
  #### Prepare the covariates and observations  ----

  ## Right away, remove any columns in both tables that are all NA. Wont be useful anyway
  covs = Filter(function(x)!all(is.na(x)), covs)
  if (!covs_only) obs = Filter(function(x)!all(is.na(x)), obs)

  ## Treat logical and factor covariates (e.g. baitUse, featureType) as text, so their values get pasted together per cell
  # find the logical and factor columns
  text_like_covs <- names(covs)[vapply(covs, function(x) is.logical(x) || is.factor(x), logical(1))]
  # remember each one's class (and factor levels), so it can be given back after resampling
  class_templates <- lapply(covs[text_like_covs], function(x) x[0])
  # and store them as character
  covs[text_like_covs] <- lapply(covs[text_like_covs], as.character)
  ## the same for logical and factor observation columns, which are pasted per cell and day
  if (!covs_only) {
    # find them, keeping the covariates' templates for obs_covs taken from covs
    text_like_obs <- names(obs)[vapply(obs, function(x) is.logical(x) || is.factor(x), logical(1))]
    # remember their classes beside the covariates' ones
    class_templates <- c(class_templates, lapply(obs[setdiff(text_like_obs, names(class_templates))],
                                                 function(x) x[0]))
    # and store them as character
    obs[text_like_obs] <- lapply(obs[text_like_obs], as.character)
  } # end observation classes condition

  ## a requested column that is absent, or was just dropped for being all NA,
  ## has nothing to summarise, so leave it out with a warning naming it
  # mode columns first
  gone_mode <- setdiff(mode_cols_covs, names(covs))
  if (length(gone_mode) > 0) {
    # keep only the mode columns that still exist
    mode_cols_covs <- intersect(mode_cols_covs, names(covs))
    warning(sprintf("resample_covariates_and_observations() left out mode_cols_covs with no values in covs: %s.\n",
                    paste(gone_mode, collapse = ", ")),
            "They are absent or entirely NA, so there is no mode to compute.", call. = FALSE)
  } # end missing mode columns
  # then the observation-level covariates
  gone_obs <- setdiff(obs_covs, names(covs))
  if (length(gone_obs) > 0) {
    # keep only the observation-level covariates that still exist
    obs_covs <- intersect(obs_covs, names(covs))
    warning(sprintf("resample_covariates_and_observations() left out obs_covs with no values in covs: %s.\n",
                    paste(gone_obs, collapse = ", ")),
            "They are absent or entirely NA, so they cannot vary between days.", call. = FALSE)
  } # end missing observation covariates

  ## Extract relevant char/numeric/modal/etc cols from covaraites
  # select all character cols that wont get averaged
  char_cols_covs <- names(covs)[sapply(covs, is.character)]
  char_cols_covs = c(char_cols_covs, "deployment_seq_date","retrival_seq_date") # not char, but dont want to average julain dates
  # Verify modal cols are not in char
  char_cols_covs = char_cols_covs[! char_cols_covs %in% mode_cols_covs]
  # Extract numerical columns to average
  num_cols_covs = names(covs)[sapply(covs, is.numeric)]
  num_cols_covs = num_cols_covs[! num_cols_covs %in% c("deployment_seq_date","retrival_seq_date")] # make sure these are NOT averaged.
  # coordinates describe where the cameras are, not the habitat they sampled, so they are never weighted
  coord_cols <- c("latitude", "longitude")

  ## Each deployment's weight within its cell: the camera-days it ran
  # a missing or negative length gives the deployment no weight
  camera_days <- as.numeric(difftime(covs$deploymentEnd, covs$deploymentStart, units = "days"))
  camera_days[is.na(camera_days) | camera_days < 0] <- 0
  # with weights = "none" every deployment counts once, the plain mean
  covs$..weight <- if (weights == "camera_days") camera_days else rep(1, nrow(covs))
  # count the cells where no deployment ran long enough to weigh anything
  n_unweighted_cells <- 0

  ## Extract relevant cols from observations, when there are any
  if (!covs_only) {
    # date-level
    date_cols = names(obs)[sapply(obs, function(x) inherits(x, "POSIXct"))]  # grab all date-time columns
    # grab all char and factor variables from observations
    sp_cols = c(names(obs)[sapply(obs, is.character)], names(obs)[sapply(obs, is.factor)]) # grab all char and factor variables
    sp_cols = sp_cols[!sp_cols %in% c("deploymentID", "mediaID")] # no longer needed, in favor of cellID. and mediaID will get hectic and uselss, so ignore
    # grab numeric cols from obs
    num_cols_obs = names(obs)[sapply(obs, is.numeric)]
    # remove count, handled distinctly, and dont need deltaTimes anymore
    num_cols_obs = num_cols_obs[! grepl("deltaTime|count", num_cols_obs)]

    ## Add sequential Julian date to the observations
    obs$seq_date = julian(as.Date(obs$eventStart)) # observations lose within date resolution, day is the smallest temporal scale here.
  } # end observation columns condition
  ## and covs
  covs$deployment_seq_date = julian(covs$deploymentStart)
  covs$retrival_seq_date = julian(covs$deploymentEnd)

  #
  ##
  ###
  #### Ultra loop to resample data per deploymentGroup ----

  ## will iterate per deploymentGroup, then per spatial scale (cellID)

  ## create lists to store the data
  resamp_covs_list = list() # save covs here
  resamp_obs_list = list()  # save obs here
  for(i in 1:length(unique(covs$deploymentGroups))){

    # grab one deploymentGroup
    dg = unique(covs$deploymentGroups)[i]
    # and subset
    covs_dg = covs[covs$deploymentGroups == dg, ]

    ## merge the cellID values to observations, when there are any
    if (!covs_only) {
      obs_dg = obs[obs$deploymentID %in% covs_dg$deploymentID, ]
      cols = names(covs_dg)[grepl("cellID", names(covs_dg))]
      # and add deploymentID
      cols = c(cols, "deploymentID")
      # subset covs
      add = covs_dg[, cols]
      # and merge
      obs_dg = merge(add, obs_dg, by = "deploymentID")
    } # end merge condition

    ## create lists to store data per scale
    resamp_covs_list_scale = list()     ## covariates first
    resamp_obs_list_scale = list()      ## then observations

    ## resample per spatial scale
    for(l in 1:length(names(covs_dg[grepl("cellID_", names(covs_dg))]))){

      #
      ##
      ### Begin resampling the covarites ----

      # Select a single spatial scale
      s = names(covs_dg[grepl("cellID_", names(covs_dg))])[l]

      ## Create an empty list to fill in results per sampling unit per scale
      r = list()
      for(t in 1:length(unique(covs_dg[,s]))){ # Repeat for each sampling unit @ that scale

        # Select a single SU
        su_dat = covs_dg[covs_dg[[s]] == unique(covs_dg[,s])[t],]

        ## Create a new line to fill in the re-sampled metadata per cell
        new = as.data.frame(matrix(NA, nrow = 1, ncol = ncol(covs_dg)))
        colnames(new) = colnames(covs_dg)

        ## a cell where no deployment carries weight falls back to the plain mean, so count it
        if (sum(su_dat$..weight) <= 0) n_unweighted_cells <- n_unweighted_cells + 1

        ## Calculate means for specific covarites, weighting each deployment by its camera-days
        for(a in 1:length(num_cols_covs)){

          ## Add conditional if-else statement to retain NAs for cols that only have NA
          if(all(is.na(su_dat[,colnames(su_dat) == num_cols_covs[a]]))){

            new[1,colnames(new) == num_cols_covs[a]] = NA

            ## But still keep the column present for rbinding later
            colnames(new)[colnames(new) == num_cols_covs[a]] = paste("Avg", num_cols_covs[a], sep = "_")

          }else{

            ## if not all NA, calculate the mean: coordinates plainly, everything else weighted
            # coordinates get equal weight per deployment
            w_a <- if (num_cols_covs[a] %in% coord_cols) rep(1, nrow(su_dat)) else su_dat$..weight
            new[1,colnames(new) == num_cols_covs[a]] = .weighted_cell_mean(su_dat[[num_cols_covs[a]]], w_a)

            ## Change col name to reflect new avg value
            colnames(new)[colnames(new) == num_cols_covs[a]] = paste("Avg", num_cols_covs[a], sep = "_")
          } # end NA conditional
        } # end avg_cols

        #Calculate effort in trap-nights for all cams in cell
        effort = 0 # reset effort from prior iteration
        for(b in 1:length(unique(su_dat$deploymentID))){

          ## Use difftime() to calculate duration cam was active from start-end date.
          effort = effort + as.numeric(difftime(su_dat$deploymentEnd[su_dat$deploymentID == unique(su_dat$deploymentID)[b]],
                                                su_dat$deploymentStart[su_dat$deploymentID == unique(su_dat$deploymentID)[b]],
                                                units = "days"))
        } # end effort loop

        # Save total effort as cellEffort
        new$cellEffort = unique(effort)

        ### Calculate new start/stop dates to accommodate extra cams in single cells
        ## Sampling begins at the minimum start date for all cams included
        new$samplingStart = min(su_dat$deploymentStart[su_dat$deployment_seq_date == min(su_dat$deployment_seq_date)])

        ## And sampling ends at the maximum end date for all cams included
        new$samplingEnd = max(su_dat$deploymentEnd[su_dat$retrival_seq_date == max(su_dat$retrival_seq_date)])

        ## Make a direct copy of character variables
        for(c in 1:length(char_cols_covs)){

          # take a copy of the char value, but collapse when there are multiple values
          new[1,colnames(new) == char_cols_covs[c]] = paste(sort(unique(su_dat[,char_cols_covs[c]])), collapse = " - ")

        } # end char cols

        ## change col names
        names(new)[names(new) == "deploymentID"] = "deploymentsIncluded"

        ## verify we even have cols that need the mode!
        if(length(mode_cols_covs)>0){

          ## Calculate the mode for mode cols
          for(m in 1:length(mode_cols_covs)){

            # take the mode: the value covering the most camera-days (or deployments, with weights = "none")
            new[1,colnames(new) == mode_cols_covs[m]] = .weighted_cell_mode(su_dat[[mode_cols_covs[m]]], su_dat$..weight)

            ## Change col name to reflect new modal value
            colnames(new)[colnames(new) == mode_cols_covs[m]] = paste("mode", mode_cols_covs[m], sep = "_")

          } # end mod cols

        } # end mod_cols present conditon

        ## Save cell_id name, but variable style to accommodate different scales
        new[,s] = unique(su_dat[[s]])

        ## Specify variable polygon scale
        sid = stringr::str_split(s, "_")[[1]][2]
        p = colnames(su_dat[grepl("poly", colnames(su_dat))])
        p = p[endsWith(p, sid)]
        #and save it
        new[[p]] = unique(su_dat[[p]])

        ## create vector of averaged and modal cols
        avg = colnames(new[grepl("Avg", colnames(new))])
        mo = colnames(new[grepl("mode", colnames(new))])
        ## and character columns
        char = char_cols_covs[! grepl("cellID|polygon|seq_date", char_cols_covs)] # remove SU, polygons, and julian dates
        # and update char for the new deployments column
        char[char == "deploymentID"] = "deploymentsIncluded"

        ## Select only the relevant info
        new = dplyr::select(new,
                            ## Vectors of columns
                            all_of(s), all_of(p), all_of(avg),
                            all_of(mo), all_of(char),
                            ## newly calculated columns
                            cellEffort, samplingStart, samplingEnd) # anything else??

        ## save it!
        r[[t]] = new

      } # end per sampling unit

      ## combine list into one DF
      df_su_scale = do.call(rbind, r)
      # # and save in the final list
      # temp[[l]] = df_su_scale
      # names(temp)[l] = names(covs_dg[grepl("cellID_", names(covs_dg))])[l]
      resamp_covs_list_scale[[l]] = df_su_scale
      names(resamp_covs_list_scale)[l] = names(covs_dg[grepl("cellID_", names(covs_dg))])[l]
      # without observations, this scale is done
      if (covs_only) next

      #
      ##
      ### Begin resampling the observations ----

      ## Create an empty df to fill in results
      r = obs_dg[0,]

      for(y in unique(obs_dg[,s])){ # Repeat for each sampling unit @ that scale

        # Subset data for a single SU
        t = obs_dg[obs_dg[[s]] == y,]

        for(d in unique(t$seq_date)){ # Repeat for each day there was a detection

          # Subset data for a single day
          t2 = t[t$seq_date == d,]

          for(sp in unique(t2$scientificName)){ # Repeat for each different scientificName detected

            # Subset data for the specific scientificName
            t3 = t2[t2$scientificName == sp,]
            # And grab the relevant sampling unit from the covs_dg
            dep = covs_dg[covs_dg$deploymentID %in% unique(t3$deploymentID),]

            #create the new line to fill with re-sampled data
            new = as.data.frame(matrix(NA, nrow = 1, ncol = ncol(obs_dg)))
            colnames(new) = colnames(obs_dg)

            #
            ##
            ### Sampling Unit-level information (t)
            new[,s] = unique(t[[s]])                     # Cell_id that can change scales
            # grab the WRONG cellID values
            rm_cell = names(new)[grepl("cellID", names(new))][! names(new)[grepl("cellID", names(new))] %in% s]
            # remove them, along w/ depID
            new[, c("deploymentID", rm_cell)] = NULL
            # save all observation covariates here
            ## but first verify observation covariates are even provided!
            if(length(obs_covs) > 0){
              for(o in 1:length(obs_covs)){
                ob = obs_covs[o]
                new[[ob]] = paste(sort(unique(dep[[ob]])), collapse = " - ")      ## Simply collapse all values into one to fill one row
              } # end per obs col
            } # end obs_cov condition

            #
            ##
            ### Date-level information (t2)
            # first create new cols about how many deployments were active on this day
            new$deploymentsActiveAtDate = paste(sort(unique(t2$deploymentID)), collapse = " - ") # useful for referencing w/ OG captures alter
            new$numberDeploymentsActiveAtDate = length(unique(t2$deploymentID))
            # and save new obs cols
            new_obs_cols = c("deploymentsActiveAtDate", "numberDeploymentsActiveAtDate")
            # and loop thru all date cols
            for(x in 1:length(date_cols)){
              # one col
              d_col = date_cols[x]
              # update new date column, using min/max based on start/end date-time
              if(grepl("Start", d_col)){
                res = min(as.POSIXct(t2[[d_col]]))
              }else{
                res = max(as.POSIXct(t2[[d_col]]))
              } # end ifelse(), which couldnt keep posixct class
              # save it
              new[[d_col]] = res
            } # end per date_col

            #
            ##
            ### scientificName-level information (t3)
            # first create new cols for events and counts
            new$independentEvents = length(unique(t3$eventID))
            new$independentObservations = length(unique(t3$observationID))
            # total individuals, based on user input
            if(individuals == "sum"){
              new$totalIndividuals = sum(as.numeric(t3$count))
            }
            if(individuals == "max"){
              new$totalIndividuals = max(as.numeric(t3$count))
            }
            # and save new obs cols
            new_obs_cols = c(new_obs_cols, "independentEvents", "independentObservations", "totalIndividuals")
            # Loop thru all species-level columns to paste info together
            for(z in 1:length(sp_cols)){
              # one col
              s_col = sp_cols[z]
              # update info
              new[[s_col]] = paste(sort(unique(t3[[s_col]])), collapse = " - ")
            } # end per sp_cols

            # and average all numeric observations too
            # but only if present!
            if(length(num_cols_obs) > 0){
              for(n in 1:length(num_cols_obs)){
                # select one
                n_col = num_cols_obs[n]
                # update info
                new[[n_col]] = mean(t3[[n_col]])
              }
            }

            ## Select only the relevant info
            new = dplyr::select(new, all_of(s), all_of(sp_cols),
                                all_of(date_cols), all_of(num_cols_obs),
                                all_of(obs_covs), all_of(new_obs_cols))

            #save via rbind
            r = rbind(r, new)

          } # end per species
        } # end per date
      } # end per sampling unit

      ## Save per spatial scale
      resamp_obs_list_scale[[l]] = r
      names(resamp_obs_list_scale)[l] = names(obs_dg[grepl("cellID_", names(obs_dg))])[l]

    } # end per cellID scale
    # #testing clean up
    # rm(a,b,c,d,t,x,y,z,l,m,n,o,p,s, add, dep, df_su_scale, new, r, su_dat, mo,
    #    t2, t3, avg, char, char_cols_covs, cols, d_col, date_cols, dg, effort,
    #    n_col, new_obs_cols, num_cols_covs, num_cols_obs, ob, res, rm_cell,
    #    s_col, sid, sp, sp_cols)

    ### Combine into a nested list
    # observations
    resamp_obs_list[[i]] = resamp_obs_list_scale
    names(resamp_obs_list)[i] = dg

    # covariates
    resamp_covs_list[[i]] = resamp_covs_list_scale
    names(resamp_covs_list)[i] = dg

  } # end per depGroup

  ## say how many cells fell back to the plain mean because none of their deployments had a length
  if (weights == "camera_days" && n_unweighted_cells > 0) {
    warning(sprintf("resample_covariates_and_observations() used plain means in %d cell-scale combination(s) where no deployment had a positive length (deploymentEnd after deploymentStart).\n",
                    n_unweighted_cells),
            "Check deploymentStart and deploymentEnd for those deployments.", call. = FALSE)
  } # end unweighted cells warning

  ## Verify the cellID values agree between the resampled observations and covariates
  # collect one line per deploymentGroup and scale with cells that recorded no observations
  no_obs_notes <- character(0)
  # without observations there is nothing to compare
  if (covs_only) resamp_obs_list <- list()
  for (e in seq_along(resamp_obs_list)) {
    # go further in to the nested list
    list_covs <- resamp_covs_list[[e]]
    list_obs <- resamp_obs_list[[e]]
    for (f in seq_along(list_covs)) {
      # grab each DF
      dat_covs <- list_covs[[f]]
      dat_obs <- list_obs[[f]]
      # and the cellID col
      cell <- names(dat_covs)[grepl("cellID", names(dat_covs))]
      # observation cells come from the covariates, so a cell missing from them means the join failed
      unknown_cells <- setdiff(dat_obs[[cell]], dat_covs[[cell]])
      # if any, the observations can't be placed on a sampled cell
      if (length(unknown_cells) > 0) {
        # hard stop, naming the group and an example cell
        stop(sprintf("resample_covariates_and_observations() found %d %s value(s) in the observations with no covariates in deploymentGroup %s, e.g. %s.\n",
                     length(unknown_cells), cell, names(resamp_covs_list)[e], unknown_cells[1]),
             "Check that obs and covs come from the same deployments before resampling.", call. = FALSE)
      } # end unknown cells check
      # cells that were sampled but recorded none of the supplied observations
      empty_cells <- setdiff(dat_covs[[cell]], dat_obs[[cell]])
      # note how many, out of all sampled cells, for the warning below
      if (length(empty_cells) > 0) {
        no_obs_notes <- c(no_obs_notes, sprintf("  %s at %s: %d of %d cells",
                                                names(resamp_covs_list)[e], cell, length(empty_cells),
                                                length(unique(dat_covs[[cell]]))))
      } # end empty cells condition
    } # end per scale
  } # end per deploymentGroup
  ## warn once about every sampled cell without observations
  if (length(no_obs_notes) > 0) {
    warning(paste0("resample_covariates_and_observations() found sampled cells with no observations:\n",
                   paste(no_obs_notes, collapse = "\n"), "\n",
                   "This is expected when obs is filtered to a few species, or when a cell detected nothing: ",
                   "the cells stay in the resampled covariates, and effort_matrix() and matrix_generator() count them as sampled with no detections (zeros). ",
                   "It is a problem if obs was filtered by deployment, date or observationType in a way that removed real records, ",
                   "since those zeros would then be false absences. Check that obs covers every deployment in covs."),
            call. = FALSE)
  } # end no observations warning

  ## gather all relevant scales of cellID
  scales = names(resamp_covs_list[[1]]) # doesnt matter which number, all should be the same

  # extract the resampled covs per scale and combine
  resampled_covs <- purrr::map(scales, function(x) {
    dplyr::bind_rows(purrr::map(resamp_covs_list, ~ .x[[x]]))
  })
  # save the scale names for this list
  names(resampled_covs) = scales
  ## give logical and factor columns their class back, unless a cell holds several values
  # collect the columns that had to stay text, across scales
  kept_as_text <- character(0)
  for (x in scales) {
    restored <- .restore_classes(resampled_covs[[x]], class_templates)
    resampled_covs[[x]] <- restored$data
    kept_as_text <- c(kept_as_text, restored$kept_as_text)
  } # end per scale
  # tag each table with its scale, so effort_matrix() can tell it is one row per cell
  for (x in scales) attr(resampled_covs[[x]], "wildobs_resampled") <- x

  ## without observations, return the covariates alone
  if (covs_only) {
    # say which columns could not keep their class
    .message_kept_as_text(kept_as_text)
    return(list("spatially_resampled_covariates" = resampled_covs))
  } # end covariates only condition

  # do the same for observations
  resampled_obs <- purrr::map(scales, function(x) {
    dplyr::bind_rows(purrr::map(resamp_obs_list, ~ .x[[x]]))
  })
  # save the scale names for this list
  names(resampled_obs) = scales
  # give their logical and factor columns their class back too
  for (x in scales) {
    restored <- .restore_classes(resampled_obs[[x]], class_templates)
    resampled_obs[[x]] <- restored$data
    kept_as_text <- c(kept_as_text, restored$kept_as_text)
  } # end per scale
  # say which columns could not keep their class
  .message_kept_as_text(kept_as_text)
  # and tag them too, so detection_matrix() can tell they are summed per cell and day
  for (x in scales) attr(resampled_obs[[x]], "wildobs_resampled") <- x

  ## return a final product containg a nested list with two elements (obs and covs)
  ## and then each element in that list contains more versions of the data at the relevant scale.
  results = list("spatially_resampled_observations" = resampled_obs,
                 "spatially_resampled_covariates" = resampled_covs)

}# end function


#' Give resampled columns back their logical or factor class
#'
#' Logical and factor columns are pasted as text while resampling. A column whose values
#' are each a single original value (or empty) gets its class back; a column where some
#' cell or day holds several values, such as `"FALSE - TRUE"`, can only be text and stays
#' character. Modal columns (`mode_<name>`) always hold one value.
#'
#' @param df A resampled table.
#' @param templates A named list of zero-length vectors with each column's original class.
#' @return A list with `data`, the table, and `kept_as_text`, the columns left as text.
#' @author Zachary Amir
#' @keywords internal
#' @noRd
.restore_classes <- function(df, templates) {
  # the columns that had to stay text
  kept_as_text <- character(0)
  for (nm in names(templates)) {
    # the column under its own name, or as a modal column
    for (col in intersect(c(nm, paste0("mode_", nm)), names(df))) {
      # the values as text, with empty pastes treated as missing
      vals <- as.character(df[[col]])
      vals[!is.na(vals) & vals == ""] <- NA
      # the values a column of the original class can hold
      allowed <- if (is.factor(templates[[nm]])) levels(templates[[nm]]) else c("TRUE", "FALSE")
      # a pasted combination is not one of them, so the column stays text
      if (!all(is.na(vals) | vals %in% allowed)) {
        kept_as_text <- c(kept_as_text, col)
        next
      } # end mixed values condition
      # otherwise restore the original class
      df[[col]] <- if (is.factor(templates[[nm]])) factor(vals, levels = allowed) else as.logical(vals)
    } # end per column name
  } # end per template
  list(data = df, kept_as_text = kept_as_text)
} # end .restore_classes


#' Say which logical or factor columns had to stay text after resampling
#'
#' @param cols The column names, possibly repeated across scales and tables.
#' @return Nothing; prints one message when there are any.
#' @author Zachary Amir
#' @keywords internal
#' @noRd
.message_kept_as_text <- function(cols) {
  # each column once
  cols <- unique(cols)
  # nothing to report
  if (length(cols) == 0) return(invisible(NULL))
  # one message for all of them
  message(sprintf("resample_covariates_and_observations(): %s kept as text, because some cells hold more than one value (for example \"FALSE - TRUE\").\n",
                  paste(cols, collapse = ", ")),
          "For one value per cell, add the column to mode_cols_covs; to model detection by it, use effort_matrix(split_by = ...).")
  invisible(NULL)
} # end .message_kept_as_text


#' Camera-day weighted mean of one covariate within a cell
#'
#' @param x The covariate's values, one per deployment in the cell.
#' @param w Each deployment's weight, its camera-days (or 1 each for a plain mean).
#'
#' @return The weighted mean of the non-missing values, `NA` if there are none, or their
#'   plain mean if none of those deployments carries any weight.
#' @author Zachary Amir
#' @keywords internal
#' @noRd
.weighted_cell_mean <- function(x, w) {
  # only deployments with a value count
  keep <- !is.na(x)
  # nothing to average
  if (!any(keep)) return(NA_real_)
  # deployments with no length carry no weight, so fall back to the plain mean
  if (sum(w[keep]) <= 0) return(mean(x[keep]))
  # each value counts in proportion to the camera-days behind it
  sum(x[keep] * w[keep]) / sum(w[keep])
} # end .weighted_cell_mean


#' Camera-day weighted mode of one covariate within a cell
#'
#' @param x The covariate's values, one per deployment in the cell.
#' @param w Each deployment's weight, its camera-days (or 1 each for the most frequent value).
#'
#' @return The value with the most total weight, the first such value on a tie, `NA` if
#'   there are none, or the most frequent value if no deployment carries any weight.
#' @author Zachary Amir
#' @keywords internal
#' @noRd
.weighted_cell_mode <- function(x, w) {
  # only deployments with a value count
  keep <- !is.na(x)
  # nothing to choose from
  if (!any(keep)) return(NA)
  # deployments with no length carry no weight, so fall back to the most frequent value
  if (sum(w[keep]) <= 0) return(Mode(x[keep]))
  # the distinct values, in the order they first appear
  values <- unique(x[keep])
  # the total weight behind each value
  totals <- vapply(values, function(v) sum(w[keep][x[keep] == v]), numeric(1))
  # the value with the most weight
  values[which.max(totals)]
} # end .weighted_cell_mode
