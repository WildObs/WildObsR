#' Group Temporally Overlapping Deployment IDs
#'
#' This function checks for and consolidates temporally overlapping deployment IDs within the same location.
#' It is designed to group deployments that are active at overlapping or nearly adjacent times, ensuring that
#' each deploymentID represents a distinct time period. This function is recommended for use in step 1 to validate
#' deploymentID assignments and in step 3 to verify temporal and spatial distinctions between deploymentIDs.
#'
#' @details
#' This function assumes that the provided `locationID` values are already spatially distinct. Deployments at the
#' same location with overlapping or closely spaced start and end times are grouped together under a new
#' deploymentID, ensuring temporal and spatial distinctness. After updating deploymentIDs in `deps`,
#' the function also updates related deploymentIDs in `obs` and `media` to maintain consistency across data files.
#' The function includes checks to ensure matching deploymentIDs across the provided data frames before and after
#' processing, with detailed warnings if mismatches occur.
#'
#' @param deps A data frame of camera deployments, containing columns `deploymentID`, `locationID`,
#' `deploymentStart`, `deploymentEnd`, and `deploymentGroups`. The `deploymentStart` and `deploymentEnd`
#' columns should contain date-time information.
#' @param obs A data frame of observations with `deploymentID` and `locationID` columns that match with
#' the `deps` data frame.
#' @param media A data frame of media records with `deploymentID` column matching the `deps` data frame.
#'
#' @return A list with three updated data frames:
#' \describe{
#'   \item{deployments}{The updated `deps` data frame with consolidated deploymentIDs for temporally
#'   overlapping records.}
#'   \item{observations}{The updated `obs` data frame with deploymentID changes to match those in `deps`.}
#'   \item{media}{The updated `media` data frame with deploymentID changes to match those in `deps`.}
#' }
#'
#' @importFrom dplyr distinct
#' @importFrom stats setNames
#'
#' @examples
#' # Minimal toy example to demonstrate how overlapping deployments are merged
#'
#' # Create a deployments data frame with overlapping start and end dates
#' deps <- data.frame(
#'   deploymentID = c("Cam01_A", "Cam01_B"),
#'   locationID = c("Loc1", "Loc1"),
#'   deploymentStart = as.POSIXct(c("2020-01-01", "2020-01-05")),
#'   deploymentEnd   = as.POSIXct(c("2020-01-10", "2020-01-15")),
#'   deploymentGroups = NA_character_
#' )
#'
#' # Observations linked to those deployments
#' obs <- data.frame(
#'   observationID = 1:4,
#'   deploymentID = rep(c("Cam01_A", "Cam01_B"), each = 2),
#'   locationID = "Loc1"
#' )
#'
#' # Media linked to the same deployments
#' media <- data.frame(
#'   mediaID = paste0("m", 1:4),
#'   deploymentID = rep(c("Cam01_A", "Cam01_B"), each = 2)
#' )
#'
#' # Run the function — overlapping deployments will be consolidated
#' results <- update_temporally_overlapping_deployments(
#'   deps = deps,
#'   obs = obs,
#'   media = media
#' )
#'
#' # View updated deployments
#' results$deployments
#'
#' # Check updated observation and media deployment IDs
#' results$observations
#' results$media
#'
#' @author Zachary Amir
#'
#' @export
update_temporally_overlapping_deployments = function(deps, obs, media){

  ### Add a few warnings to make sure data is ready to go

  # Make sure deploymentStart is present in deps
  if (!("deploymentStart" %in% colnames(deps))) {
    stop("Input deployments must have 'deploymentStart' column.")
  }
  # Make sure deploymentEnd is present in deps
  if (!("deploymentEnd" %in% colnames(deps))) {
    stop("Input deployments must have 'deploymentEnd' column.")
  }
  # Make sure locationID is present in obs
  if (!("locationID" %in% colnames(deps))) {
    stop("Input deployments must have 'locationID' column.")
  }
  # Make sure deploymentID is present in obs
  if (!("deploymentID" %in% colnames(deps))) {
    stop("Input deployments must have 'deploymentID' column.")
  }
  # Make sure deploymentGroups is present in obs
  if (!("deploymentGroups" %in% colnames(deps))) {
    stop("Input deployments must have 'deploymentGroups' column, even if it is just all 'NA' values")
  }

  ### Ensure deploymentID is clear and matching between files!
  # deps and obs
  if(length(setdiff(deps$deploymentID, obs$deploymentID)) +
     length(setdiff(obs$deploymentID, deps$deploymentID)) != 0){
    stop("The deploymentID values between the deployments and observations data files do not match perfectly! This needs to be resolved before running this function.")
  }
  # deps and media
  if(length(setdiff(deps$deploymentID, media$deploymentID)) +
     length(setdiff(media$deploymentID, deps$deploymentID)) != 0){
    stop("The deploymentID values between the deployments and media data files do not match perfectly! This needs to be resolved before running this function.")
  }
  # media and obs
  if(length(setdiff(media$deploymentID, obs$deploymentID)) +
     length(setdiff(obs$deploymentID, media$deploymentID)) != 0){
    stop("The deploymentID values between the media and observations data files do not match perfectly! This needs to be resolved before running this function.")
  }
  # Check for repeated deploymentID values
  if(length(unique(deps$deploymentID)) < nrow(deps)){
    stop(paste("There are", (nrow(deps) - length(unique(deps$deploymentID))), "repeated deploymentID values in your deployments file. Please inspect the following deploymentIDs before proceeding:", paste(unique(deps$deploymentID[duplicated(deps$deploymentID)]), collapse = " & ")))
  }

  ## make a list to store results
  deps_list = list()

  ## and a dataframe to store all problematic and new deployments
  prob_deps_df = data.frame("new_deploymentID" = character(),
                            "old_deploymentID"= character(),
                            "locationID"= character(), stringsAsFactors = FALSE)
  ## check every locationID!
  for(i in 1:length(unique(deps$locationID))){

    ## grab the location + data
    loc = unique(deps$locationID)[i]
    loc_dat = deps[deps$locationID == loc,]
    ## and organize via start date/time
    loc_dat = loc_dat[order(loc_dat$deploymentStart),]

    ## only calculate time differences and groupings if there are multiple deploymentIDs!
    if(nrow(loc_dat) > 1){
      # Initialize a group column
      loc_dat$group <- 1

      # Loop through each deployment and check for overlaps
      for (l in 2:nrow(loc_dat)) {

        ## calculate time difference from the next start date/time w/ the previous end date/time.
        time_diff = as.numeric(difftime(loc_dat$deploymentStart[l],
                                        loc_dat$deploymentEnd[l - 1],
                                        units = "hours"))

        ### Check if the current deployment is active during a similar time as the previous one
        overlap_check = loc_dat$deploymentStart[l] <= loc_dat$deploymentEnd[l - 1]

        ## if theres an overlap,
        if (overlap_check) {
          # then assign the same group as the previous row
          loc_dat$group[l] <- loc_dat$group[l - 1]
        } else if (time_diff <= 24 && time_diff >= 0) {
          # If the time difference is less than 24 hours, group with the previous row as well
          loc_dat$group[l] <- loc_dat$group[l - 1]
        } else {
          # Otherwise, assign a new group
          loc_dat$group[l] <- loc_dat$group[l - 1] + 1
        } # end overlap check
      } # end per row

      ## now grab those groups w/ several cams
      groups = names(table(loc_dat$group)[table(loc_dat$group)>1])

      ## inspect for curious testing development
      # loc_dat[, c("deploymentID", "group", "deploymentStart", "deploymentEnd")]

      ## make a temporary list for binding w/in the nested loops
      temp = list()

      ## ONLY if there are multiple groups, then we inspect
      if(length(groups) > 0){
        ## for each group
        for(g in 1:length(groups)){

          ## grab the problematic deployments
          prob_deps = loc_dat$deploymentID[loc_dat$group == groups[g]]

          # create a new deploymentID name
          new_dep = paste(loc, "deployment", g, sep = "_")

          # find matching data in deps
          new_d = deps[deps$deploymentID %in% prob_deps, ]
          # grab the new min/max date/time
          new_start = min(new_d$deploymentStart)
          new_end = max(new_d$deploymentEnd)
          # and the first deploymentGroup
          new_dgroup = new_d$deploymentGroups[new_d$deploymentStart == new_start][1] # use 1 to make sure its the first!

          # and overwrite old data
          new_d$deploymentID = new_dep
          new_d$deploymentEnd = new_end
          new_d$deploymentStart = new_start
          new_d$deploymentGroups = new_dgroup

          # and make it distinct!
          new_d = dplyr::distinct(new_d)

          ## make sure we are only one row now
          if(nrow(new_d) > 1){
            # but if were not, dont combine and let us know
            # print(paste("There was an error combining deployment info from locationID:",
            #             loc, "group number:", paste(g, ".", sep = ""), "This could be because they do not share all deployment info (e.g. featureType). Inspect this data carefully!"))
            # and skip to the next iteration
            next
          }else{

            ## save info about which deployments had problems
            temp_df = data.frame("new_deploymentID" = rep(new_dep, length.out = length(prob_deps)),
                                 "old_deploymentID" = prob_deps,
                                 "locationID" = rep(loc, , length.out = length(prob_deps)))
            # and rbind w/ overal df
            prob_deps_df = dplyr::distinct(rbind(prob_deps_df, temp_df))

            ## and save the new deployments in the list!
            temp[[g]] = new_d

            ## and give us a statement
            print(paste("The locationID:", loc, "had", length(prob_deps),
                        "temporally overlapping deployments in group", paste(g, ".", sep = ""),
                        "These have now been condensed into one row with the updated deploymentID:",
                        unique(new_d$deploymentID)))

          } # end single row condition
        } # end loop per group.
      } # end zero length groups condition
    }else{
      next
    } # end condition for multiple rows in loc_dat

    ## combine temp into one dataframe
    temp_df = do.call(rbind, temp)

    ## and save this in the bigger list
    deps_list[[i]] = temp_df

  } # end loop per locationID

  # Remove NULL elements in the list
  deps_list <- Filter(Negate(is.null), deps_list)

  ## combine list into dataframe
  new_deps = do.call(rbind, deps_list)

  ## remove the problem deploymentIDs from og deployments
  deps = deps[! deps$deploymentID %in% unique(prob_deps_df$old_deploymentID), ]

  ## and combine w/ the updated data
  deps = rbind(new_deps, deps)

  ### but dont forget to update media and obs too!

  # Use match() to ensure one-to-one mapping between old and new deploymentID
  match_media = match(media$deploymentID, prob_deps_df$old_deploymentID)
  # Replace only the matched deploymentIDs
  media$deploymentID[!is.na(match_media)] = prob_deps_df$new_deploymentID[match_media[!is.na(match_media)]]

  ## and do the same for obs
  match_obs = match(obs$deploymentID, prob_deps_df$old_deploymentID)
  obs$deploymentID[!is.na(match_obs)] = prob_deps_df$new_deploymentID[match_obs[!is.na(match_obs)]]

  ### FINALLY, double check were still kosher for depID
  # deps and obs
  if(length(setdiff(deps$deploymentID, obs$deploymentID)) +
     length(setdiff(obs$deploymentID, deps$deploymentID)) != 0){
    stop("The deploymentID values between the deployments and observations data files do not match after preforming function calculations. This is really problematic, and Im sorry but I dont know what to do for you. Maybe go back to the drawing board and consider a new career far away from computers. You will be happier.")
  }
  # deps and media
  if(length(setdiff(deps$deploymentID, media$deploymentID)) +
     length(setdiff(media$deploymentID, deps$deploymentID)) != 0){
    stop("The deploymentID values between the deployments and media data files do not match after preforming function calculations. This is really problematic, and Im sorry but I dont know what to do for you. Maybe go back to the drawing board and consider a new career far away from computers. You will be happier.")
  }
  # media and obs
  if(length(setdiff(media$deploymentID, obs$deploymentID)) +
     length(setdiff(obs$deploymentID, media$deploymentID)) != 0){
    stop("The deploymentID values between the media and observations data files do not match after preforming function calculations. This is really problematic, and Im sorry but I dont know what to do for you. Maybe go back to the drawing board and consider a new career far away from computers. You will be happier.")
  }

  ### now save everything in a clear list
  results = list("deployments" = deps,
                 "observations" = obs,
                 "media" = media)


  ## Finally, return the updated list of results
  return(results)

} # end function

# testing clean up
# rm(i,l,g,loc,loc_dat, time_diff, groups, deps_list,
#    prob_deps_df, temp, temp_df, overlap_check,
#    new_dep, new_d, new_dgroup, new_end, new_start,
#    new_deps, prob_deps, match_media, match_obs)

