#' Spatially resample the covariates and observations resources
#' Spatially Resample Covariates and Observations Across Spatial Scales
#'
#' @description
#' This function resamples covariate and observation data across spatial scales generated from the \code{WildObsR::spatial_hexagon_generator()} function. It validates the input data by ensuring that all necessary spatial, temporal, and deployment identifiers are present and correctly formatted. Numeric covariates are aggregated using means, while categorical covariates are combined by concatenating unique values.
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
#' @param obs A data frame or tibble of observation data from a camtrapDP formatted frictionless data package, which can be accessed from the \code{WildObsR::wildobs_dp_download()} function. It must include a \code{deploymentID} column and several date-time columns (e.g., \code{eventStart}, \code{eventEnd}, \code{classificationTimestamp}, \code{observationStart}, \code{observationEnd}) formatted as POSIXct.
#' @param individuals A character string specifying how to aggregate the number of individuals counted per spatially re sampled cell per day. Options are \code{"sum"} to take the sum total number of counts or  \code{"max"} to use the maximum count.
#' @param mode_cols_covs (Optional) A character vector specifying the names of covariate columns for which the mode should be computed rather than concatenating all unique values. (Example: \code{c("habitat")}).
#' @param obs_covs (Optional) A character vector specifying observation-level covariate column names that may vary in space and time and will be combined with the resampled observations. (Example: \code{c("baitUse", "featureType")}).
#'
#' @return A nested list with two elements:
#' \describe{
#'   \item{`spatially_resampled_observations`}{A list of data frames corresponding to different spatial scales (e.g., "cellID_1km", "cellID_10km") containing all resampled observation data.}
#'   \item{`spatially_resampled_covariates`}{A list of data frames corresponding to different spatial scales containing all resampled covariate data.}
#' }
#'
#'@details
#' The function performs the following steps to resample the covariate and observation data:
#'
#' 1. **Data Validation**:
#'    - Verifies that the covariates data frame contains at least one spatial cellID column generated from the \code{WildObsR::spatial_hexagon_generator()} function (i.e., a column whose name starts with "cellID"). If not, the function stops with an error.
#'    - Checks that the \code{deploymentID} values match between the covariates and observation data. If there is a mismatch, the function stops with an error.
#'    - Ensures that the specified observation-level covariate columns (provided in \code{obs_covs}) exist in the covariates data. If some are missing, they are omitted from further processing with a warning.
#'    - Confirms that all required date-time columns in both datasets are formatted as \code{POSIXct}. This includes columns such as \code{eventStart}, \code{eventEnd}, \code{classificationTimestamp}, \code{observationStart}, and \code{observationEnd} in the observations, and \code{deploymentStart} and \code{deploymentEnd} in the covariates.
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
#'    - The function loops over each unique deployment group present in the covariates data. This speeds up the process.
#'    - For each deployment group:
#'         - Subsets the covariates and observations to include only the relevant deployments.
#'         - Merges spatial cell information (i.e., the \code{cellID} columns) from the covariates into the observations based on the \code{deploymentID}.
#'
#' 4. **Resampling of Covariates**:
#'    - Within each deployment group, the function iterates over each spatial scale (columns containing "cellID_").
#'    - For each spatial scale, it loops through each unique sampling unit (i.e., each unique cell value):
#'         - A new row is created for each sampling unit, starting as an empty data frame with the same structure as the covariates data.
#'         - Numeric covariate values are aggregated by computing the mean (or left as \code{NA} if all values are missing). The column names are updated to indicate that these are averaged values (e.g., \code{"Avg_<column>"}).
#'         - Sampling effort is calculated as the total active duration (in days) summed over all unique deployments in the cell.
#'         - The sampling period is defined by the minimum \code{deploymentStart} (as \code{samplingStart}) and maximum \code{deploymentEnd} (as \code{samplingEnd}) for the cell.
#'         - Character variables are processed by pasting together sorted unique values.
#'         - For any covariate specified in \code{mode_cols_covs}, the function calculates the mode (i.e., the most frequent value) instead of concatenating all values.
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
#'    - After processing all deployment groups and spatial scales, a final verification is implemented to ensure all cellID values are present and matching in the resampled covariates and observations.
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
#' \dontrun{
#' # Download the data package
#' dp <- WildObsR::wildobs_dp_download("ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001")
#'
#' # Extract resources from the data package
#' covs <- frictionless::read_resource(dp, "covariates")
#' obs  <- frictionless::read_resource(dp, "observations")
#' deps <- frictionless::read_resource(dp, "deployments")
#'
#' ## Assign spatial scales
#' # 'scales' defines the apothem (in meters) of the hexagonal cell.
#' # The names (e.g., "1km", "10km") refer to the area covered by the cell.
#' scales <- c("1km" = 1074.6, "10km" = 3398)
#'
#' ## Add data source to the covariates
#' covs$source <- dp$contributors[[1]]$tag
#'
#' ## Generate spatial hexagons based on the provided scales
#' covs <- WildObsR::spatial_hexagon_generator(covs, scales)
#' rm(scales)
#'
#' ## Check that deploymentID values match between covariates and observations and deployments
#' verify_col_match(covs, obs, "deploymentID")
#'
#' # Identify matching columns between deployments and covariates
#' cols <- names(deps)[names(deps) %in% names(covs)]
#' # Merge deployment information to the covariates
#' covs <- merge(deps, covs, by = cols)  # Note: This overwrites the original covs data
#'
#' ## Define columns for mode aggregation (example: 'source' and 'habitat')
#' mode_cols_covs <- names(covs)[grepl("source|habitat", names(covs))]
#'
#' ## Set the method for aggregating the total number of individuals detected:
#' individuals <- "sum"  # Alternative: "max"
#'
#' ## Specify observation-level covariate variables derived from deployments.
#' # These variables capture information that varies in space and time.
#' obs_covs <- c("baitUse", "featureType", "setupBy", "cameraModel", "cameraDelay",
#'               "cameraHeight", "cameraDepth", "cameraTilt", "cameraHeading",
#'               "detectionDistance", "deploymentTags")
#' }
#' ## Run the function
#' resampled_data <- resample_covariates_and_observations(covs, obs, individuals, mode_cols_covs, obs_covs)
#'
#' @importFrom dplyr select bind_rows
#' @importFrom purrr map
#' @importFrom stringr str_split
#'
#' @author Zachary Amir
#'
#' @export
resample_covariates_and_observations <- function(covs, obs, individuals, mode_cols_covs, obs_covs){

  #
  ##
  ###
  #### Data validation ----

  ## Make sure the covariates has cellID values
  if(!any(grepl("cellID", names(covs)))){
    stop("The covariates you have provided in this function do not contain any spatially delimited cells defined in a column starting with 'cellID'. Please run the function WildObsR::spatial_hexagon_generator to generate cells based off your spatial scales of interest.\n")
  }
  ## Make sure deploymentID match between obs and covs
  if(length(setdiff(obs$deploymentID, covs$deploymentID)) +
     length(setdiff(obs$deploymentID, covs$deploymentID)) != 0){
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
  ## Make sure obs_covs are actually present in the deployments, thin if not.
  if(length(obs_covs) > 0 & !any(obs_covs %in% names(covs))){
    # thin to match
    obs_covs = obs_covs[obs_covs %in% names(covs)]
    warning("You have provided observation-level covariates that are not present in the covariates resource. The observation-level covariates have been reduced to include only values present in the covariates resource.\n")
  }
  ## Make sure date columns are formatted as posixct
  if(!any(sapply(obs[, c("eventStart","eventEnd","classificationTimestamp",
                         "observationStart","observationEnd")],
                 function(x) inherits(x, "POSIXct")))){
    stop("The observations table you provided in this function does not have date-time columns (i.e.,eventStart, eventEnd, classificationTimestamp, observationStart, & observationEnd) formatted in POSIXct class. Please format these date-time columns to a standard format (%Y-%m-%d %H:%M:%S) using the as.posixct() function before using this function.\n ")
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
  obs = Filter(function(x)!all(is.na(x)), obs)

  ## Extract relevant char/numeric/modal/etc cols from covaraites
  # select all character cols that wont get averaged
  char_cols_covs <- names(covs)[sapply(covs, is.character)]
  char_cols_covs = c(char_cols_covs, "deployment_seq_date","retrival_seq_date") # not char, but dont want to average julain dates
  # Verify modal cols are not in char
  char_cols_covs = char_cols_covs[! char_cols_covs %in% mode_cols_covs]
  # Extract numerical columns to average
  num_cols_covs = names(covs)[sapply(covs, is.numeric)]
  num_cols_covs = num_cols_covs[! num_cols_covs %in% c("deployment_seq_date","retrival_seq_date")] # make sure these are NOT averaged.

  ## Extract relevant cols from observations
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
    obs_dg = obs[obs$deploymentID %in% covs_dg$deploymentID, ]

    ## merge the cellID values to observations
    cols = names(covs_dg)[grepl("cellID", names(covs_dg))]
    # and add deploymentID
    cols = c(cols, "deploymentID")
    # subset covs
    add = covs_dg[, cols]
    # and merge
    obs_dg = merge(add, obs_dg, by = "deploymentID")

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

        ## Calculate means for specific covarites
        for(a in 1:length(num_cols_covs)){

          ## Add conditional if-else statement to retain NAs for cols that only have NA
          if(all(is.na(su_dat[,colnames(su_dat) == num_cols_covs[a]]))){

            new[1,colnames(new) == num_cols_covs[a]] = NA

            ## But still keep the column present for rbinding later
            colnames(new)[colnames(new) == num_cols_covs[a]] = paste("Avg", num_cols_covs[a], sep = "_")

          }else{

            ## if not all NA, calculate the mean
            new[1,colnames(new) == num_cols_covs[a]] = mean(su_dat[,colnames(su_dat) == num_cols_covs[a]], na.rm = T)

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

            # take the mode
            new[1,colnames(new) == mode_cols_covs[m]] = Mode(su_dat[,mode_cols_covs[m]])

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
            for(n in 1:length(num_cols_obs)){
              # select one
              n_col = num_cols_obs[n]
              # update info
              new[[n_col]] = mean(t3[[n_col]])
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

  ## Verify we didnt lose any cellID values through resampling
  for(e in 1:length(resamp_covs_list)){
    # go further in to the nested list
    list_covs = resamp_covs_list[[e]]
    list_obs = resamp_obs_list[[e]]
    leng = unique(length(list_covs), length(list_obs))
    for(f in 1:leng){
      # grab each DF
      dat_covs = list_covs[[f]]
      dat_obs = list_obs[[f]]
      # and the cellID col
      cell = names(dat_covs)[grepl("cellID", names(dat_covs))]
      # make sure were safe
      if(length(setdiff(dat_covs[[cell]], dat_obs[[cell]])) +
         length(setdiff(dat_obs[[cell]], dat_covs[[cell]])) != 0){
        stop(paste("The", cell, "values do not perfectly match in the resampled observations and covariates for the deploymentGroup", unique(dat_covs$deploymentGroups), "\nPlease inspect this dataset carefully before running this function!"))
      } # end condition
    } # end per leng
  } # end per list length

  ## gather all relevant scales of cellID
  scales = names(resamp_covs_list[[1]]) # doesnt matter which number, all should be the same

  # extract the resampled covs per scale and combine
  resampled_covs <- purrr::map(scales, function(x) {
    dplyr::bind_rows(purrr::map(resamp_covs_list, ~ .x[[x]]))
  })
  # save the scale names for this list
  names(resampled_covs) = scales

  # do the same for observations
  resampled_obs <- purrr::map(scales, function(x) {
    dplyr::bind_rows(purrr::map(resamp_obs_list, ~ .x[[x]]))
  })
  # save the scale names for this list
  names(resampled_obs) = scales

  ## return a final product containg a nested list with two elements (obs and covs)
  ## and then each element in that list contains more versions of the data at the relevant scale.
  results = list("spatially_resampled_observations" = resampled_obs,
                 "spatially_resampled_covariates" = resampled_covs)

}# end function
