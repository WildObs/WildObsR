#' Generate Detection History and Covariate Matrices from camtrapDP Formatted Data
#'
#' This function processes camera trapping data formatted using the camtrapDP data standard to generate detection history matrices along with site-level and observation-level covariate matrices. These outputs can then be used for hierarchical occupancy or abundance modeling (e.g., with the \code{unmarked} package).
#'
#' @param obs A \code{data.frame} of observations. This must include columns for date-time information
#'   (e.g., \code{eventStart}, \code{eventEnd}, \code{observationStart}, \code{observationEnd}),
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
#'       \item Specified site-level and observation-level covariate columns are present and contain no NA values.
#'       \item Date-time columns in both \code{obs} and \code{covs} are properly formatted as \code{POSIXct}.
#'       \item The provided species names (\code{scientificNames}) exist in the \code{obs} table.
#'     }
#'   \item \strong{Timezone Adjustment:} Date-time values are updated to the appropriate timezone based on
#'     the mean latitude and longitude of the sites using \code{lutz::tz_lookup_coords} and \code{lubridate::with_tz}.
#'   \item \strong{Date Sequence and Sampling Occasions:} A complete sequence of dates is generated for each
#'     sampling unit and an index is assigned to each sampling occasion.
#'   \item \strong{Matrix Generation:} For each species, a detection history matrix is created by aligning
#'     observations to their respective sampling occasions. The matrix can contain either count data (for
#'     abundance models) or binary presence/absence data (for occupancy models).
#'   \item \strong{Observation-level Covariate Formatting:} Observation covariates are organized into matrices
#'     that align with the detection history matrix and are compressed to match sampling occasion windows.
#' }
#'
#'  @examples
#' \dontrun{
#' # Example usage with pre-processed camera trap data:
#' dp <- WildObsR::wildobs_dp_download("ZAmir_QLD_Wet_Tropics_2022_WildObsID_0001")
#'
#' # Extract resources from the data package
#' covs <- frictionless::read_resource(dp, "covariates")
#' obs  <- frictionless::read_resource(dp, "observations")
#' deps <- frictionless::read_resource(dp, "deployments")
#'
#' ## Assign spatial scales
#' 'scales' defines the apothem (in meters) of the hexagonal cell, where the names (e.g., "1km", "10km") refer to the area covered by the cell.
#' scales <- c("1km" = 1074.6, "10km" = 3398)
#'
#' ## Add data source to the covariates
#' covs$source <- dp$contributors[[1]]$tag
#'
#' ## Generate spatial hexagons based on the provided scales
#' covs <- WildObsR::spatial_hexagon_generator(covs, scales)
#' # Identify matching columns between deployments and covariates
#' cols <- names(deps)[names(deps) %in% names(covs)]
#' # Merge deployment information to the covariates
#' covs <- merge(deps, covs, by = cols)  # Note: This overwrites the original covs data
#' ## Define columns for mode aggregation (example: 'source' and 'habitat')
#' mode_cols_covs <- names(covs)[grepl("source|habitat", names(covs))]
#' ## Set the method for aggregating the total number of individuals detected:
#' individuals <- "sum"  # Alternative: "max"
#' ## Specify observation-level covariate variables derived from deployments.
#' # These variables capture information that varies in space and time.
#' obs_covs <- c("baitUse", "featureType", "setupBy", "cameraModel", "cameraDelay","cameraHeight", "cameraDepth", "cameraTilt", "cameraHeading", "detectionDistance", "deploymentTags")
#'
#' ### now spatially resample the data
#' resamp_data = resample_covariates_and_observations(covs, obs, individuals = "sum", mode_cols_covs, obs_covs) # this function may take a few minutes to run
#'
#' ## Select the resampled observations and covs @ the 10 km scale
#' resamp_obs = resamp_data$spatially_resampled_observations$cellID_10km
#' resamp_covs = resamp_data$spatially_resampled_covariates$cellID_10km
#'
#' ## Run the matrix generator function
#' res <- matrix_generator(
#'   obs = resamp_obs,
#'   covs = resamp_covs,
#'   dur = 110,
#'   w = 3,
#'   site_covs = c("mode_habitat", "Avg_human_footprint_10km2", "locationName", "Avg_FLII_3km2"),
#'   obs_covs = c("numberDeploymentsActiveAtDate", "cameraHeight", "featureType", "cameraModel"),
#'   all_locationNames = TRUE,
#'   scientificNames = c("Hypsiprymnodon moschatus", "Orthonyx spaldingii", "Uromys caudimaculatus"),
#'   type = "abundance",
#'   individuals = "sum"
#' )
#'
#' # Access the detection matrix for one species:
#' res[["Hypsiprymnodon_moschatus"]][["detection_matrix"]]
#' }
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
matrix_generator = function(obs, covs, dur, w, site_covs, obs_covs,
                            all_locationNames, scientificNames, type, individuals){

  #
  ##
  ### Data validation ----
  ##
  #

  ## check if we are using cellID or deploymentID to be the rows, assuming to use cellID if its present
  if(any(grepl("cellID", names(obs)))){
    row_col = names(obs)[grepl("cellID", names(obs))]
    covs_date_cols = c("samplingStart","samplingEnd") # possibly update the resampling code to keep these consistent?
  }else{
    # assume regular deployment ID if not
    row_col = names(obs)[grepl("deploymentID", names(obs))]
    covs_date_cols = c("deploymentStart","deploymentEnd") ## always start, then end, for both!!
  }
  ## make sure the row col matches between obs and covs
  if(length(setdiff(obs[[row_col]], covs[[row_col]])) +
     length(setdiff(obs[[row_col]], covs[[row_col]])) != 0){
    stop(paste("The covariates and observations tables you have provided in this function contain mis-matched", row_col,
               "values! Please inspect your deploymentID values or reduce each table to only the matching values.\n"))
  }
  ## make sure site_covs are actually present in the covariates
  if(any(!site_covs %in% names(covs))){
    # make a warning
    warning("You have provided site-level covariates that are not in the covs table:", setdiff(site_covs, names(covs)), "so these have been remove from the function.")
    # thin
    site_covs = site_covs[site_covs %in% names(covs)]
  }
  ## Make sure there are no NA values in the site-level covariates, and if there are, remove them!
  if(any(covs[, is.na(site_covs)])){
    stop("You have provided site-level covariates that contain NA values in the covariates table and this is not allowed. Please choose different site-level covariates or inspect your data to correct the NA values.")
  }
  ## do the same for observation covs
  if(any(obs[, is.na(obs_covs)])){
    stop("You have provided observation-level covariates that contain NA values in the observations table and this is not allowed. Please choose different observation-level covariates or inspect your data to correct the NA values.")
  }
  ## make sure scientificNames are actually present in the observations
  if(any(!scientificNames %in% obs$scientificName)){
    stop(paste("You have provided the following species names that are not present in the observations table:", paste(setdiff(scientificNames, obs$scientificName), collapse = " & "), "\nPlease choose species names that are present in the observations table."))
  }
  ## make sure dates are properly formatted
  if(!any(sapply(obs[, c("eventStart","eventEnd","observationStart","observationEnd")],
                 function(x) inherits(x, "POSIXct")))){
    stop("The observations table you provided in this function does not have date-time columns (i.e.,eventStart, eventEnd, observationStart, & observationEnd) formatted in POSIXct class. Please format these date-time columns to a standard format (%Y-%m-%d %H:%M:%S) using the as.posixct() function before using this function.\n ")
  }
  # same for covs
  if(!any(sapply(covs[, covs_date_cols],
                 function(x) inherits(x, "POSIXct")))){
    stop("The covariates table you provided in this function does not have date-time columns (i.e.,deploymentStart, & deploymentEnd) formatted in POSIXct class. Please format these date-time columns to a standard format (%Y-%m-%d %H:%M:%S) using the as.posixct() function before using this function.\n ")
  }
  ### COME HERE and add more checks as you think of them.

  #
  ##
  ### Prepare the data ----
  ##
  #

  ## Make sure date-times are formatted in the right timezone, frictionless defaults to UTC
  #first grab the tx
  tz = suppressWarnings(lutz::tz_lookup_coords(mean(covs$Avg_latitude), mean(covs$Avg_longitude), method = "fast"))
  ## update obs
  obs$observationEnd = lubridate::with_tz(obs$observationEnd, tzone = tz)
  obs$observationStart = lubridate::with_tz(obs$observationStart, tzone = tz)
  ## update covs
  covs[[covs_date_cols[1]]] = lubridate::with_tz(covs[[covs_date_cols[1]]], tzone = tz)
  covs[[covs_date_cols[2]]] = lubridate::with_tz(covs[[covs_date_cols[2]]], tzone = tz)
  ## Create a date column in obs from the observationStart col
  obs$date = as.Date(obs$observationStart) # could change to eventStart??


  ### Clean covariates and standardize the values
  ## Thin the covariates to the most important information -> row col, dates, and site_covs
  covs_dat = covs[, c(row_col, covs_date_cols, site_covs)]
  ## standardize site covaraites to ensure variables are comparable across models later
  m_num = data.frame(covs_dat[,sapply(covs_dat, is.numeric), drop = FALSE])
  m_date = covs_dat[, sapply(covs_dat, function(x) inherits(x, "POSIXct"))]
  m_std = vegan::decostand(m_num, method = "standardize", na.rm = TRUE)
  m_std2 = data.frame(m_std[,colSums(is.na(m_std)) < nrow(m_std), drop = FALSE])#remove any columns with no data
  ## Make a statement if site-level cov was removed due to no data
  if(length(names(m_std2)) < length(names(m_std))){
    warning(paste("There was no data in the site-level covariate:", paste(setdiff(names(m_std), names(m_std2)), collapse = " & "), "so it has been removed from the matrix generation function."))
  }

  ## pull out all character site-covariates
  m_char = covs_dat[,sapply(covs_dat, is.character)]
  ## Verify m_char has variation
  site_sum = data.frame("site_cov" = names(m_char),
                       "num_levels" = NA )
  # loop thru each var
  for(o in 1:length(names(m_char))){
    s_dat = m_char[[names(m_char)[o]]]
    site_sum$num_levels[o] = length(unique(s_dat))
  }
  # thin to values w/ more than 1 level
  m_char_cols = site_sum$site_cov[site_sum$num_levels > 1]
  ## make a statement if we needed to remove site-level covs
  if(length(m_char_cols) < length(names(m_char))){
    warning(paste("There was no variation in the site-level covariate:", paste(setdiff(names(m_char), m_char_cols), collapse = " & "), "so it has been removed from the matrix generation function."))
  }
  ## Combine all standardized covs into one DF
  covs_dat = data.frame(m_char, m_date, m_std2)


  ### Add sampling occasion index to the observations
  ## Create a data frame with each sampling unit as a row, with start and stop dates
  s = dplyr::distinct(dplyr::select(covs_dat, all_of(row_col), all_of(covs_date_cols)))
  # and convert date-times to dates only
  s[[covs_date_cols[1]]] = as.Date(s[[covs_date_cols[1]]])
  s[[covs_date_cols[2]]] = as.Date(s[[covs_date_cols[2]]])

  # Create a new data frame 's2' that groups by the column specified in 'row_col' (e.g., "cellID_10km"), then creates a sequence of dates from the minimum of dates
  s2 <- covs_dat %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(row_col))) %>%
    dplyr::summarise(
      date = list(seq.Date(
        from = as.Date(min(.data[[covs_date_cols[grepl("Start", covs_date_cols)]]])),
        to   = as.Date(max(.data[[covs_date_cols[grepl("End", covs_date_cols)]]])),
        by   = "day"
      )), .groups = "drop"
    ) %>%
    tidyr::unnest(cols = date)

  ## Merge the sequence to the start and stop dates
  t = merge(s2, s, by = row_col)

  ## Add sequence from 1-n for each sampling unit
  res = t[0,]
  res$seq = numeric()

  for(i in unique(t[[row_col]])){
    ## select one SU
    d = t[t[[row_col]] == i,]
    ## generate the sequence
    d$seq = as.numeric(seq(from= 1,
                           to = unique(as.numeric(difftime(d[[covs_date_cols[grepl("End", covs_date_cols)]]] +1, # Add one to start seq on 1 instead of 0
                                                           d[[covs_date_cols[grepl("Start", covs_date_cols)]]],
                                                           units = "days"))), by = 1))
    ## Combine
    res = rbind(res, d)
  } # end per SU

  ## Make sure all cams got accounted for
  if(length(setdiff(res[[row_col]], unique(obs[[row_col]]))) +
     length(setdiff(unique(obs[[row_col]]), res[[row_col]])) != 0){
    stop(paste("There was a mis-match in the", row_col, "column when creating a sampling occasion index in the observations. Please ensure all date-time columns in the observations and covariates contain no errors before using this function.\n"))
  }

  ## merge the sequence with the captures
  obs_dat = merge(obs, res, by = c(row_col, "date"))

  ## check if we lost any rows
  if(nrow(obs) != nrow(obs_dat)){
    # are there less rows in c?
    if(nrow(obs_dat) < nrow(obs)){
      # how much was lost?
      percent = round(nrow(obs_dat)/nrow(obs)*100,2)
      warning("Adding the sampling occasion index to the observations has removed", percent, "percent of rows to the observations.")
    }else{
      # how much was gained?
      percent = round(nrow(obs)/nrow(obs_dat)*100,2)
      stop("Adding the sampling occasion index to the observations has added", percent, "percent of rows to the observations. This is not good, there should not be extra rows here.")
    }
  } # end row check condition


  #
  ##
  ### Begin iterating per species ----
  ##
  #
  sp_list = list() # store results here
  for(y in 1:length(scientificNames)){
    # select one species
    sp = scientificNames[y]

    ## first determine if we need to reduce the number of sampling units based off locationName
    if(!all_locationNames){
      # grab the relevant locations w/ detections
      locs = unique(obs_dat[[row_col]][obs_dat$scientificName == sp])
      # and match to the relevant locaitonName
      land = unique(covs_dat$locationName[covs_dat[[row_col]] %in% locs])
      # and thin to match
      covs_land = covs_dat[covs_dat$locationName %in% land, ]
      obs_land = obs_dat[obs_dat[[row_col]] %in% covs_dat[[row_col]], ]
    }else{
      # but if not, save all data and update names to make sure we dont overwrite covs_dat and obs_dat
      covs_land = covs_dat
      obs_land = obs_dat
    } # end locationNames condition

    #
    ##
    ### Begin Creating Detection/Count History Matrix ----
    ##
    #

    ## Outline the structure (i.e. no data) of the matrix and add col and row names
    mat = matrix(NA,
                 nrow = length(unique(obs_land[[row_col]])), # number of rows are our sampling locations
                 ncol = length(seq(from =1, to= max(obs_land$seq))), # number of columns are our sampling occasions
                 dimnames = list(as.character(unique(obs_land[[row_col]])), # row names, then column names
                                 seq(from =1, to= max(obs_land$seq))))

    ## Determine when each sampling unit was active-
    for(j in 1:length(unique(obs_land[[row_col]]))){
      a= obs_land[obs_land[[row_col]] == unique(obs_land[[row_col]])[j],] #subset for a specific sampling unit out of all possible options
      indx = seq(from = 1, to = max(a$seq)) #determine the sequence it was operational for
      mat[j,indx]=0 # at row J, across all sampling occasions, put a zero there
    }

    ## Fill in the matrix based on sampling unit and sampling occasion
    for(j in 1:length(unique(obs_land[[row_col]]))){ #repeat for each sampling unit
      su = unique(obs_land[[row_col]])[j] #specify the sampling unit
      a = obs_land[obs_land[[row_col]] == su & obs_land$scientificName == sp,] #subset captures data for specific sampling unit and for specific species

      # Fill in matrix w/ count data
      if(nrow(a)>0 & type == "abundance"){ #Bypass cameras without a detection and leave them as zero

        for(s in 1:length(a$date)){ #repeat for each Date detected at the sampling unit
          #specify the sampling date
          d = a$date[s]
          #specify the matching date index
          indx = a$seq[a[[row_col]] == su & a$date == d]
          ## fill in the matrix with the maximum value of individuals.
          mat[su,indx]= max(unique(a$totalIndividuals[a$date == d])) ## add max just to be safe in case there is more than one value, but there shouldnt be.
          #in the matrix where the row = sampling unit, and column = occasion,
          #use the total counts of the specific sampling occasion
        } # end per date loop & count fill
      } # end if-abundance statement

      # Fill in matrix w/ presence data
      if(nrow(a)>0 & type == "occupancy"){ #Bypass cameras without a detection and leave them as zero

        for(s in 1:length(a$date)){ #repeat for each Date detected at the sampling unit
          #specify the sampling date
          d = a$date[s]
          #specify the matching date index
          indx = a$seq[a[[row_col]] == su & a$date == d]
          #in the matrix where the row = sampling unit, and column = occasion,
          #mark the presence of the species with a 1
          mat[su,indx]= 1
        } # end presence fill
      } # end if-occu statement

    }# end matrix filling loop

    #
    ##
    ### Compress Detection/Count History Matrix ----
    ##
    #

    ## If all sampling units are active for less than 100 days, use the maximum sequence length for dur instead
    if(max(obs_land$seq) < dur){ dur = max(obs_land$seq)}
    ## Create a new and empty compressed matrix to fit sampling occasions
    dh_mat = matrix(nrow = nrow(mat), # number of rows are our sampling locations (same as above)
                    ncol = round(dur/w),  # number of cols is our maximum sampling duration (dur) divided by shink window (w)
                    dimnames = list(as.character(rownames(mat)), # row names are our sampling unit names
                                    seq(from = 1, to = round(dur/w)))) # col names are the number of shrunk observation windows.
    ### Sampling occasion loop-
    for(u in 1:nrow(mat)){ #Repeat for each row in the matrix
      for(p in 1:round(dur/w)){ # Repeat for each sampling occasion window

        # Outline the start dates of sampling occasion, using values provided in for-loop
        starts<-seq(from=1, to=dur, by=w)
        # Select a single start date
        l<-starts[p]
        ## by pass to limit matricies going out of bounds
        if(max(l:(l+w)) > dur){
          # create a new sequence that terminates at dur
          seq = l:(l+w)
          seq = seq[seq < dur]
        }else{
          #if its fine, save the name seq
          seq = l:(l+w)
        } # end out of bounds conditional
        if(type == "abundance"){
          ## conditional statement for how to summarize individuals
          if(individuals == "sum"){
            # Make if-else statement
            ifelse(all(is.na(mat[u,seq]), na.rm=FALSE) == "TRUE",
                   #if all values in matrix @ row u across the sampling occasion window are all NA,
                   dh_mat[u,p]<-NA, # then leave the sampling occasion as NA,
                   dh_mat[u,p]<- sum(as.numeric(mat[u,seq]), na.rm = TRUE))
            # But if FALSE, take the the sum of the detections
          }
          if(individuals == "max"){
            # Make if-else statement
            ifelse(all(is.na(mat[u,seq]), na.rm=FALSE) == "TRUE",
                   #if all values in matrix @ row u across the sampling occasion window are all NA,
                   dh_mat[u,p]<-NA, # then leave the sampling occasion as NA,
                   dh_mat[u,p]<- max(as.numeric(mat[u,seq]), na.rm = TRUE))
            # But if FALSE, take the the max of the detections
          }
        } # End conditional for abundance

        if(type == "occupancy"){
          # Make if-else statement
          ifelse(all(is.na(mat[u,seq]), na.rm=FALSE) == "TRUE",
                 #if all values in matrix @ row u across the sampling occasion window are all NA,
                 dh_mat[u,p] <- NA, # then leave the sampling occasion as NA,
                 dh_mat[u,p] <- max(as.numeric(mat[u,seq]), na.rm = TRUE))
          # But if FALSE, take the the maximum value (either zero or one)
        } # End conditional for occupancy
      } # End loop per sampling occasion
    } # End loop per row in matrix

    #
    ##
    ### Format Observation covariates Matrix ----
    ##
    #

    #### COME HERE and maybe add a conditional to bypass if there has been no obs_covs provided?

    ## first check that there is variation in observation covs for this species
    obs_sum = data.frame("obs_cov" = obs_covs,
                         "num_levels" = NA )
    # loop thru each var
    for(o in 1:length(obs_covs)){
      o_dat = obs_land[obs_land$scientificName == sp, obs_covs[o]]
      obs_sum$num_levels[o] = length(unique(o_dat[o_dat != "no_detection"]))
    }
    # thin to values w/ more than 1 level
    obs_covs_thin = obs_sum$obs_cov[obs_sum$num_levels > 1]

    # Make a warning statement if there were any observation covariates
    if(length(obs_covs_thin) < length(obs_covs)){
      warning(paste("There was no variation in the observation-level covariate:", paste(setdiff(obs_covs, obs_covs_thin), collapse = " & "), "for the species:", sp, "so it has been removed from the matrix generation function for this species."))
    }

    ## grab observation covs present across all cams (i.e. ActiveAtDate)
    obs_land_full = dplyr::distinct(dplyr::select(obs_land, all_of(row_col),
                                                  all_of(obs_covs_thin[grepl("ActiveAtDate", obs_covs_thin)]), seq))
    ## and grab observation covs specific to each species
    sp_obs = distinct(dplyr::select(obs_land[obs_land$scientificName == sp, ], all_of(row_col), seq,
                                    all_of(obs_covs_thin[!grepl("ActiveAtDate", obs_covs_thin)])))
    ## merge together, forcing NAs in the sp_obs version
    obs_land_full = merge(sp_obs, obs_land_full, by = c(row_col, "seq"), all.y = TRUE)
    ## store matricies in this list
    obs_mat_list = list()

    # run a loop for all observation-level covariates-
    for(v in 1:length(names(obs_land_full)[! names(obs_land_full) %in% c(row_col, "seq")])){
      # grab the name of one var
      o_var = names(obs_land_full)[! names(obs_land_full) %in% c(row_col, "seq")][v]
      ## create empty observation matrix dataframe
      obs_mat = (matrix(NA,
                        nrow = length(unique(obs_land_full[[row_col]])),
                        ncol = length(seq(from =1, to= max(obs_land_full$seq))),
                        dimnames = list(as.character(unique(obs_land_full[[row_col]])), # row names, then column names
                                        seq(from = 1, to= max(obs_land_full$seq)))))

      for(u in 1:length(unique(covs_land[[row_col]]))){ #repeat for each sampling unit
        # Select a single sampling unit (i.e. row)
        su = unique(covs_land[[row_col]])[u]
        # Select data from a single sampling unit
        o = obs_land[obs_land[[row_col]] == su,]

        ## but if not,
        for(x in 1:max(o$seq)){ #repeat for each sequence
          # Select the sequence (i.e. column)
          indx = seq(from = 1, to = max(o$seq), by = 1)[x]
          # select the obs cov
          n = obs_land_full[obs_land_full[[row_col]] == su & obs_land_full$seq == indx, o_var]
          # Add conditional to force n to be 1 if no active cams detected a species, but were active in the date sequence
          if(length(n) == 0 & indx <= max(o$seq) & grepl("ActiveAtDate", o_var)){ n = 1 }
          # Add a conditional if active cams has more than one value, take the sum
          if(length(n) > 1 & indx <= max(o$seq) & grepl("ActiveAtDate", o_var)){ n = sum(n)}

          # if zero, but still active in window, lebel no detection
          if(length(n) == 0 & indx <= max(o$seq) & !grepl("ActiveAtDate", o_var)){
            # instead of no_detection, use the site-level average
            n = paste(unique(obs_land[[o_var]][obs_land[[row_col]] == su]), collapse = " - ")
          }else{
            # if na, but still active in window, label as the site-level average
            if(any(is.na(n)) & indx <= max(o$seq) & !grepl("ActiveAtDate", o_var)){
              n = paste(unique(obs_land[[o_var]][obs_land[[row_col]] == su]), collapse = " - ")}
          }

          ## Fill in the obs dataframe, matching per row and column
          obs_mat[su,indx] = paste(unique(n), collapse = " - ") # adding unique to be safe

        } # End per sequence
      } # End per sampling unit

      ## save it!
      obs_mat_list[[v]] = obs_mat
      names(obs_mat_list)[v] = o_var

    } # end per obs var

    #
    ##
    ### Compress observation matricies to match sampling occasion window ----
    ##
    #

    ## store compressed matricies here
    comp_mat_list = list()
    for(v in 1:length(obs_mat_list)){
      ## select one var
      obs_mat = obs_mat_list[[v]]
      ## and grab the name
      o_var = names(obs_mat_list)[v]
      ## Create a new and empty compressed matrix to fit sampling occasions
      obs2 = matrix(nrow = nrow(obs_mat), # number of rows are our sampling locations (same as above)
                    ncol = round(dur/w),  # number of cols is our maximum sampling duration (dur) divided by shink window (w)
                    dimnames = list(as.character(rownames(obs_mat)), # row names are our sampling unit names
                                    seq(from = 1, to = round(dur/w)))) # col names are the number of shrunk observation windows.

      for(u in 1:nrow(obs_mat)){ #Repeat for each row in the matrix
        for(p in 1:round(dur/w)){ # Repeat for each sampling occasion window
          # Outline the start dates of sampling occasion, using values provided in for-loop
          starts<-seq(from=1, to=dur, by=w)
          # Select a single start date
          l<-starts[p]

          ## by pass to limit matricies going out of bounds
          if(max(l:(l+w)) > dur){
            # create a new sequence that terminates at dur
            seq = l:(l+w)
            seq = seq[seq < dur]

            ## bypass if seq is longer than the observation matrix
            if(max(seq) > dim(obs_mat)[2]){seq = seq[seq <= dim(obs_mat)[2]]}  # then thin seq to only include max values
          }else{
            #if its fine, save the name seq
            seq = l:(l+w)
          } # end out of bounds conditional

          ## we want the sum for effort
          if(grepl("ActiveAtDate", o_var)){
            # Make if-else statement
            ifelse(all(is.na(obs_mat[u,seq]), na.rm= TRUE) == "TRUE",
                   #if all values in matrix @ row u across the sampling occasion window are all NA,
                   obs2[u,p]<-NA, # then leave the sampling occasion as NA,
                   obs2[u,p]<- sum(as.numeric(obs_mat[u,seq]), na.rm = TRUE))
            # But if FALSE, take the the sum of observation covariate

            ## we want mode for character vars
            ### COME HERE, maybe there is an opportunity to format more robustly for character vs numeric cols
          }else{
            # Make if-else statement
            ifelse(all(is.na(obs_mat[u,seq]), na.rm= TRUE) == "TRUE",
                   #if all values in matrix @ row u across the sampling occasion window are all NA,
                   obs2[u,p]<-NA, # then leave the sampling occasion as NA,
                   obs2[u,p]<- Mode(obs_mat[u,seq]))
            # But if FALSE, take the the mode of observation covariate

          } # end condition for cams
        } # End loop per sampling occasion
      } # End loop per row in matrix

      ## dont forget to standardize numeric observation covars
      if(is.numeric(obs_land_full[, o_var])){

        #Standardize the observation covaraite
        me = mean(as.vector(obs2),na.rm=T)
        s = sd(as.vector(obs2),na.rm=T)
        cams1<-(obs2-me)/s

        ## Replace all NA values in obs cov w/ a zero.
        for (i in 1:dim(cams1)[1]) { #Repeat for each sampling unit
          # Select sampling occasions where any camera was active
          a <- which(obs2[i,]>0)
          # and make all other occasions (i.e. NA) equal to zero
          cams1[i,-a]=0
        } # end per SU
        ## rename to match
        obs2 = cams1
      } # end per numeric condition

      ## save it!
      comp_mat_list[[v]] = obs2
      names(comp_mat_list)[v] = names(obs_mat_list)[v]

    } # end per obs var

    ## bundle relevant info per species
    sp_bundle = list("detection_matrix" = dh_mat,
                     "site_level_covariates" = covs_dat,
                     "observation_level_covariates" = comp_mat_list)
    # and store in a list
    sp_list[[y]] = sp_bundle
    names(sp_list)[y] = gsub(" ", "_", sp)

  } # end per sp

  ### Return the final list
  return(sp_list)

} # end function
