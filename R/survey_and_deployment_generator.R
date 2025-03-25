#' Determine Single-Season Sampling Efforts from Camera Trap Images
#'
#' This function analyzes date/time information from camera trap images to identify spatially and temporally discrete sampling efforts.
#' The function updates the input data with new survey and deployment IDs by analyzing spatial and temporal patterns in camera detections.
#' Users can specify the maximum duration of a survey and the maximum gap between detections before a new survey is triggered.
#' This is useful for organizing images into surveys and deployments for further analysis.
#'
#' @details
#' This function assumes that camera coordinates and date/time data have been verified for accuracy and are free of errors.
#' Spatially distinct groupings of cameras should be indicated in the `locationName` column in the deployments data frame.
#' The function will:
#' \enumerate{
#'   \item Group detections into surveys based on the `max_dur` (maximum survey duration) and `cam_long` (maximum gap between detections).
#'   \item Assign new deployment IDs and survey IDs, ensuring each deployment has unique identifiers based on the adjusted time periods.
#'   \item Handle cameras that are active over long periods and split their data based on natural breaks in detections or user-defined thresholds.
#'   \item Optionally filter out short survey periods based on the `szn_filter` parameter.
#' }
#'
#' @param caps A data frame of camera trap observations that contains date/time information for each image. It must include columns named `eventStart`, `deploymentID`, `dataSource`, and `locationName`.
#' @param deps A data frame of camera deployments that contains location information for each camera deployment. It must include columns named `deploymentID` and `locationName`.
#' @param cam_long A numeric value (in days) representing the maximum gap between detections before a new survey is started. Defaults to 20 days.
#' @param max_dur A numeric value (in days) representing the maximum duration a survey can last before starting a new one. Defaults to 100 days.
#' @param szn_filter A logical value indicating whether to filter out short survey periods. If `TRUE`, surveys with fewer detections within a defined time frame will be combined with adjacent surveys. Defaults to `TRUE`.
#' @param cam_filter A logical value indicating whether to use the cam_long argument to look for natural breaks in detections. This is best set to `FALSE` when there are manually created start/stop records when a data provider has not provided all records. Defaults to `TRUE`.

#'
#' @return A data frame based on the `caps` input, but with additional columns:
#' \describe{
#'   \item{new_deploymentID}{A new deployment identifier that reflects updated deployment groups.}
#'   \item{new_deploymentGroups}{A new survey identifier that represents the adjusted survey groupings based on the detection intervals and survey duration.}
#'   \item{new_locationName}{A new location identifier combining location and deployment information.}
#' }
#'
#' @importFrom dplyr select distinct mutate arrange
#' @importFrom stringr str_remove str_extract
#' @importFrom chron years
#' @importFrom plyr ddply
#'
#' @author Zachary Amir
#'
#' @export
survey_and_deployment_generator = function(caps, deps, cam_long = 20, max_dur = 100, szn_filter = TRUE, cam_filter = TRUE){

  ### Create flags to track if we need to rename columns back at the end

  #Deployment_id and placename, in caps and deps
  changed_deployment_id_caps = FALSE
  changed_deployment_id_deps = FALSE
  changed_placename_caps = FALSE
  changed_placename_deps = FALSE

  #And source
  changed_source_caps = FALSE

  #And landscape
  changed_landscape_caps = FALSE
  changed_landscape_deps = FALSE

  #And date column
  changed_photo_date_caps = FALSE

  #And notes
  changed_notes_deps = FALSE


  # Check and rename 5 columns if necessary
  if ("deployment_id" %in% colnames(caps)) {
    colnames(caps)[colnames(caps) == "deployment_id"] = "deploymentID"
    changed_deployment_id_caps = TRUE
  }

  if ("placename" %in% colnames(caps)) {
    colnames(caps)[colnames(caps) == "placename"] = "locationID"
    changed_placename_caps = TRUE
  }

  if ("source" %in% colnames(caps)) {
    colnames(caps)[colnames(caps) == "source"] = "dataSource"
    changed_source_caps = TRUE
  }

  if ("Landscape" %in% colnames(caps)) {
    colnames(caps)[colnames(caps) == "Landscape"] = "locationName"
    changed_landscape_caps = TRUE
  }

  if("Photo.Date" %in% colnames(caps)) {
    colnames(caps)[colnames(caps) == "Photo.Date"] = "eventStart"
    changed_photo_date_caps = TRUE
  }


  ## Same for deployments
  # Check and rename 4 columns if necessary
  if ("deployment_id" %in% colnames(deps)) {
    colnames(deps)[colnames(deps) == "deployment_id"] = "deploymentID"
    changed_deployment_id_deps = TRUE
  }

  # Check and rename columns if necessary
  if ("placename" %in% colnames(deps)) {
    colnames(deps)[colnames(deps) == "placename"] = "locationID"
    changed_placename_deps = TRUE
  }

  if ("Landscape" %in% colnames(deps)) {
    colnames(deps)[colnames(deps) == "Landscape"] = "locationName"
    changed_landscape_deps = TRUE
  }

  if ("notes" %in% colnames(deps)) {
    colnames(deps)[colnames(deps) == "notes"] = "deploymentComments"
    changed_notes_deps = TRUE
  }

  #### ensure data checks are valid to implement function

  # Make sure deployment is present in both the captures and deployments files
  if (!("deploymentID" %in% colnames(caps)) || !("deploymentID" %in% colnames(deps))) {
    stop("Input captures and deployments must have 'deploymentID' column, even if its just a copy of locationID.")
  }
  # Make sure Photo.Date is present in caps
  if (!("eventStart" %in% colnames(caps))) {
    stop("Input captures must have 'eventStart' column, even if its just a copy of eventEnd and/or Photo.Date.")
  }
  # Make sure data source is present in caps
  if (!("dataSource" %in% colnames(caps))) {
    stop("Input captures must have 'source' column for the relevant data source.")
  }
  # Make sure locationName is present in tboth the captures and deployments files
  if (!("locationName" %in% colnames(caps)) || !("locationName" %in% colnames(deps))) {
    stop("Input captures and deployments must have 'locationName' column.")
  }
  # Make sure deployments_ids match between deps and captures file
  if(length(setdiff(caps$deploymentID, deps$deploymentID)) +
     length(setdiff(deps$deploymentID, caps$deploymentID)) != 0){
    stop("The deployment_ids do not perfectly match between the captures and deployment files. Ensure a clean match before using this function")
  }
  # Make sure there are no NA values in eventStart
  if(any(is.na(caps$eventStart))){
    stop("The captures contains NA date-time values in the eventStart column which will cause the funciton to fail. Please correct or remove the NA values before proceeding.")
  }

  ## to store results here
  res = list()

  # loop per locationName to ensure seperate surveys
  for(l in 1:length(unique(caps$locationName))){

    # select a single locationName
    land = unique(caps$locationName)[l]

    # and thin data
    dl = caps[caps$locationName == land,]

    ## Extract the date value from dateTime object
    dl$date = as.Date(dl$eventStart)

    # select first and last date of the survey
    surv_start = min(dl$date)
    surv_end = max(dl$date)

    #create df with a row per day between the first and last record
    dates <- data.frame("date" =  rep(seq(surv_start, surv_end, by = 'days'),
                                            times = 1))

    # # thin dates to only include the dates where we have captures in this long survey
    dates = data.frame("date" = dates[dates$date %in% unique(dl$date),])

    # reorder data by chronological dates
    dates = data.frame("date" = dates[order(dates$date),])

    # add column to sep 90-day checks
    dates$check = "pt1"

    ## split apart surveys that are active for longer than pre-established date
    if(as.numeric(difftime(surv_end, surv_start)) >= max_dur){

      #### instead of making 90-day breaks,
      ### check cams for natural breaks (determined above in cam_long object),
      ## and then cut by 90 days
      for(p in 1:length(unique(dates$date))){

        # select an individual date and the next unique one
        date1 = unique(dates$date)[p]
        date2 = unique(dates$date)[p+1]

        # add conditional to bypass last unique date value and assign it the last value
        if(is.na(date2)){date2 = date1} # end max variable (p) value conditional

        ### Conditional statement to check if cam_filter is false
        ### and if there is evidence of manual start/stop records added to the captures (potentially remove this, but playing it extra safe RN)
        if(cam_filter == F &
           any(grepl("Manual_start_stop", deps$deploymentComments))){

          # skip to the next date without checking cam_long
          next

        } # end cam_filter condition

        # if dates are longer than pre-determined duration, assess 90 period seperately
        if(as.numeric(abs(difftime(date1, date2))) > cam_long){

          dates$check[dates$date >= date2] = paste("pt", p, sep = "")

        } # end conditional for long dates

      } # end loop per date

      ## assess max duration intervals based on our previous checks
      temp = list() # store results here
      for(p in 1:length(unique(dates$check))){

        #subset for a single chunk
        da = dates[dates$check == unique(dates$check)[p],]

        #split dates into groups based on max duration
        da<-da %>%
          dplyr::mutate(season = (cut.Date(date, breaks = paste(max_dur, "days"), labels = FALSE))) %>%
          dplyr::arrange(date)

        ### Make sure seasons are distinct between chunks by making a character var
        da$season = paste("pt",p,"_", da$season, sep = "")

        # save results
        temp[[p]] = da

      } # end loop per check

      # combine back into a df
      dates = do.call(rbind, temp)

      #Start szn_filter conditional
      if(szn_filter == TRUE){
        ## quick loop to avoid survey's w/ too few detections
        for(u in 1:length(unique(dates$season))){

          ## if the below code is true and we reduced the number of seasons less than var value,
          if(u <= length(unique(dates$season))){

            # if the first season has the same min and max dates (i.e. only one date),
            if(u == 1 &
               as.numeric(difftime(max(dates$date[dates$season == unique(dates$season)[u]]),
                                   min(dates$date[dates$season == unique(dates$season)[u]]))) == 0){

              # and if the difference between the next survey is less than a calendar week
              if(as.numeric(difftime(min(dates$date[dates$season == unique(dates$season)[u+1]]),
                                     (dates$date[dates$season == unique(dates$season)[u]]))) <= 8){

                # change that season to the NEXT one
                dates$season[dates$season == unique(dates$season)[u]] =
                  unique(dates$season)[u+1]

              }# end secondary conditional

            } # end first season conditional

            # if the last season has the same  min and max dates (i.e. only one date),
            if(u == length(unique(dates$season)) &
               as.numeric(difftime(max(dates$date[dates$season == unique(dates$season)[u]]),
                                   min(dates$date[dates$season == unique(dates$season)[u]]))) == 0){


              # and if the difference between the previous survey is less than a calendar week
              if(as.numeric(difftime(max(dates$date[dates$season == unique(dates$season)[u-1]]),
                                     (dates$date[dates$season == unique(dates$season)[u]]))) <= 8){

                # change that season to the PREVIOUS one
                dates$season[dates$season == unique(dates$season)[u]] =
                  unique(dates$season)[u-1]


              } # end secondary conditional

            } # end last season conditional

            # if there is a season w/ less than a calendar week of detections,
            if(u < length(unique(dates$season)) & u != 1 &
               as.numeric(difftime(max(dates$date[dates$season == unique(dates$season)[u]]),
                                   min(dates$date[dates$season == unique(dates$season)[u]]))) <= 8){

              # change that season to the PREVIOUS one
              dates$season[dates$season == unique(dates$season)[u]] =
                unique(dates$season)[u-1]

            }# end conditional for changing season to previous one

          }# end loop breaking conditional

        } # end quick loop for short survey splits

      } #End szn_filter conditional

      # summarize season's starting year via ddply
      szn = ddply(dates, .(season), summarize,
                  start_year = chron::years(min(date)))

      #rank each season from first to last
      szn$rank <- with(szn, ave(seq_along(szn$start_year),
                                szn$start_year,
                                FUN = seq_along))
      #recode values
      szn["rank"][szn["rank"] == "1"] <- "a"
      szn["rank"][szn["rank"] == "2"] <- "b"
      szn["rank"][szn["rank"] == "3"] <- "c"
      szn["rank"][szn["rank"] == "4"] <- "d"
      szn["rank"][szn["rank"] == "5"] <- "e"
      szn["rank"][szn["rank"] == "6"] <- "f"
      szn["rank"][szn["rank"] == "7"] <- "g"
      szn["rank"][szn["rank"] == "8"] <- "h"
      # only case w/ 8 values is continuous monitoring for 1 full year w/ no breaks

      # Generate the final ID,
      ## if there is only one ranking
      if(length(unique(szn$rank)) == 1){

        ## just use the year for the final ID
        szn$ID = paste(szn$start_year)
      }else{

        ## but if there is more than 1 ranking, make sure to include it!
        szn$ID = paste(szn$start_year, szn$rank, sep = "_")
      }

      # merge back w/ all dates
      dates = merge(dates, szn, by = 'season')

      ## for each ranking, assign a different new name for the survey
      for(id in 1:length(unique(dates$ID))){

        #subset the records from the single season (rank)
        r = unique(dates$ID)[id]
        ra = dates[dates$ID == r,]

        # and the same for the loop dataset for close inspection
        dl$add_to_survey_id[dl$locationName == land &
                              dl$date %in% ra$date] = r

      } # end loop per rank ID

    } # end conditional for cams that are active longer than max dur


    ## add something simple to ensure ALL add_to_survey_id rows have a temporal value
    if(as.numeric(difftime(surv_end, surv_start)) < max_dur){

      # simply take the start date of the survey and attach that to deploymentGroups
      dl$add_to_survey_id[dl$locationName == land &
                            dl$date >= surv_start &
                            dl$date <= surv_end] = as.character(chron::years(surv_start))

    } # end regular season conditional

    #### Create new deploymentGroups tags

    # create a new survey ID to each bin
    dl$new_deploymentGroups <- paste(dl$locationName,
                                     dl$add_to_survey_id,
                                     sub("^.*_", "", unique(dl$dataSource)), # extract only last name from dataSource
                                     sep = "_")

    # # Combine state and locationName to create a new deploymentGroups code
    # code = unique(paste0(dl$state, "_", dl$locationName))
    #
    # # add the new locationName tag w/ state
    # dl$new_locationName = paste(unique(code))
    ### Leave locationName alone for another function, too much is already happening here.

    ## Need to make sure camera names are split appropriately and appended w/ proper time tag as well
    ## and include relevant information from the metadata
    for(c in seq_along(unique(dl$locationID))){

      ## select a single locationID
      cam = unique(dl$locationID)[c]

      ## and subset caps for that place
      dlc = dl[dl$locationID == cam, ]

      ## extract distinct places, deployments, and surveys for sub-setted captures
      m = distinct(select(dlc, locationID, deploymentID, new_deploymentGroups, add_to_survey_id))

      #### First, make sure there is no year/letter info in the cam names
      ## extract year and letter separately
      yr = str_extract(cam, "_(?:20)(?:0[1-9]|[12][0-9]|22)")
      let = str_extract(cam, "(?<=_)\\w(?=$)")

      ## add conditional for missing letter
      if(!is.na(let)){
        # and combine
        combo = paste0(yr, "_", let)
      }else{
        # skip the letter if not present
        combo = yr
      } # end letter conditional


      ### Verify data from the deployments should or should not be included here.
      # if this place name has multiple deployments, and they arent a yearly/time thing
      if(nrow(m) > 1 &
         nrow(m) != length(unique(dlc$add_to_survey_id))){

        ## for each unique new_deploymentGroups value
        for(v in 1:length(unique(dlc$new_deploymentGroups))){

          ## for each row of deployments with the same locationID
          for(n in 1:nrow(m[m$new_deploymentGroups == unique(dlc$new_deploymentGroups)[v], ])){

            ## Grab the appropriate info
            info = dl$add_to_survey_id[dl$locationID == cam
                                       & dl$deploymentID == m$deploymentID[m$new_deploymentGroups == unique(dlc$new_deploymentGroups)[v]][n]
                                       & dl$new_deploymentGroups == unique(dlc$new_deploymentGroups)[v]]

            ## Make new cam name w/ cam, info, and individual cam number
            new_cam = paste(cam, info, paste("Cam", n, sep = ""), sep = "_")

            ### Verify the new cam name has not repeated the the combo created above
            check = gsub("_", "", combo) # remove underscore for easier checking
            if(!is.na(check) &
               grepl(paste(unique(info), check, sep = "_"), unique(new_cam))){

              ## reduce the new cam name to not include the dublicate
              new_cam = gsub(paste(unique(info), check, sep = "_"), unique(info), new_cam)

            } # end duplicate text conditional


            ## Finally, save new name in the data
            dl$new_deploymentID[dl$locationID == cam & #] = new_cam
                                  dl$deploymentID == m$deploymentID[m$new_deploymentGroups == unique(dlc$new_deploymentGroups)[v]][n] &
                                  dl$new_deploymentGroups == unique(dlc$new_deploymentGroups)[v]] = (new_cam)

          } # end per new_deploymentGroups

        } # end per row of m

      }else{

        ## but if there is only one deployment per cam,
        ## Make new cam name w/ regular code
        new_cam = paste(cam,
                        dl$add_to_survey_id[dl$locationID == unique(dl$locationID)[c]],
                        sep = "_")

        # Now add the updated time stamp to the deployment
        dl$new_deploymentID[dl$locationID == unique(dl$locationID)[c]] = (new_cam)

      } # end duplicate conditional

    } # end loop per cam

    ## save the final dataframe in the list
    res[[l]] <- dl

  } # end per locationName

  ## return the updated captures data frame w/ new columns
  results = do.call(rbind, res)

  #Rename our columns if needed
  # Change the column names back to original if they were changed
  if (changed_deployment_id_caps) {
    colnames(results)[colnames(results) == "deploymentID"] <- "deployment_id"
  }

  if (changed_source_caps) {
    colnames(results)[colnames(results) == "dataSource"] <- "source"
  }

  if (changed_placename_caps) {
    colnames(results)[colnames(results) == "locationID"] <- "placename"
  }

  if (changed_landscape_caps) {
    colnames(results)[colnames(results) == "locationName"] <- "Landscape"
  }

  if (changed_photo_date_caps) {
    colnames(results)[colnames(results) == "eventStart"] <- "Photo.Date"
  }

  #Return the results as the output
  return(results)

} # end function

# # for testing
# rm(l,p,c,u,r,cam, dl, yr, land, let, combo,
#    new_cam,v, m, dlc, surv_end, surv_start, da,
#    dates,ra,temp,szn,date1,date2,id, n, res,
#    changed_deployment_id_caps, changed_landscape_caps,
#    changed_deployment_id_deps, changed_landscape_deps,
#    changed_photo_date_caps, changed_placename_caps,
#    changed_placename_deps, changed_source_caps, check, info)
