#' Spatially resample the covariates and observations resources
#'
#'
#'
#'
#'
#
# #### EXAMPLE DATA GENERATION
# dp = frictionless::read_package("/Users/zachary_amir/Dropbox/WildObs master folder/WildObs GitHub Data Storage/data_clean/Step 4 DataPackages/ZAmir_QLD_Wet_Tropics_2022/datapackage.json")
# # extract covariates resource
# covs = frictionless::read_resource(dp, "covariates")
# obs = frictionless::read_resource(dp, "observations")
# deps = frictionless::read_resource(dp, "deployments")
#
# ## assign spatial scales
# scales = c("1km" = 1074.6, "10km" = 3398) # number refer to the apothem of the hexagonal cell, name refers to the area covered by the cell
# ## add dataSource to the covarites
# covs$source = dp$contributors[[1]]$tag
#
# ## Generate spatial hexagons
# covs = WildObsR::spatial_hexagon_generator(covs, scales)
# rm(scales)
# ## make sure were good.
# length(unique(covs$cellID_1km))
# length(unique(covs$cellID_10km))
# ## make sure were good.
# verify_col_match(covs, obs, "deploymentID")
#
# ## Merge deployments to the covaraites as well
# verify_col_match(covs, deps, "deploymentID") # safe to merge
# # grab matching cols
# cols = names(deps)[names(deps) %in% names(covs)]
# covs = merge(deps, covs, by = cols) ## Possibly problematic for overwriting og covs... but we will see.
#
# ## extract columns we want for the mode
# mode_cols_covs = names(covs)[grepl("source|habitat", names(covs))] # example
# #
# # ## how will we create the total number of individuals detected?
# individuals = "sum" # or "max"
# #
# # # What are the variables that would be moved from deployments to observations to inform observation covarites.
# obs_covs = c("baitUse", "featureType", "setupBy", "cameraModel", "cameraDelay",
#              "cameraHeight", "cameraDepth", "cameraTilt", "cameraHeading",
#              "detectionDistance", "deploymentTags") ## this is exhaustive, but could probably think of more??

resample_covariates_and_observations <- function(covs, obs, mode_cols_covs, obs_covs, individuals){

  #
  ##
  ###
  #### Data validation ----

  ## Make sure the covariates has cellID values
  ## Make sure the legit cols are legit (i.e. classes)
  ## Make sure deploymentID and deploymentGroups match between obs and covs
  ## Make sure obs_covs are actually present in the deployments, thin if not.
  ## Make sure the vectors provided contain enumerated values, particularly individuals.

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
        new$samplingBegin = min(su_dat$deploymentStart[su_dat$deployment_seq_date == min(su_dat$deployment_seq_date)])

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
                            cellEffort, samplingBegin, samplingEnd) # anything else??

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

      ##### COME HERE!!! Need to save per spatial scale, but cant Rbind different scales together
      ### ultimetly want a list containing the relevant scales, but also containing all sampling units.
      # and save in the final list


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
            for(o in 1:length(obs_covs)){
              ob = obs_covs[o]
              new[[ob]] = paste(sort(unique(dep[[ob]])), collapse = " - ")      ## Simply collapse all values into one to fill one row
            } # end per obs col

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
                                all_of(new_obs_cols))

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
  results = list("spatially resampled observations" = resampled_obs,
                 "spatially resampled covariates" = resampled_covs)


}# end function
