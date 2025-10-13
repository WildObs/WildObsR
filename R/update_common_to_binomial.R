#' Update common names to binomial nomenclature
#'
#' @description
#' This function is used to convert species names from common names into binomial nomenclature, based off the WildObs verified taxonomy database.
#' This is acheived by taking each missing species name, searching the common name column of binomial verifeid and suggesting replacements to the user from binomial_verified for any matches to the common name.
#' If a common_name is not found it will tell you.
#'
#'
#' @param obs_data A data frame containing observation data with a species column (either "scientificName" or "Species")
#' @param verified_data A data frame with verified species information containing columns: Common_name, user_provided_name, and binomial_verified
#' @param interactive Logical. If TRUE, prompts user to choose replacement when multiple matches exist (default: FALSE)
#'
#' @return The obs_data data frame with scientificName column updated with verified binomial nomenclature where matches were found
#'
#'
#' @details
#' The function can work interactivtly, allowing users to verify a name is correct or not.
#' If more than binomial name is found it, will present options to specify the correct name by entering numbers.
#' If nothing matches hit enter to skip that species.
#' It ignores capitals. A replacement will be done on the Species column of the caps frame with the user selected replacements.
#' Interactive defaults to FALSE, because knitting a Rmd document with an interactive funciton is not logical.
#' Any time there is more than one different binomial name to replace the common name,
#' it gets skipped for closer inspection later, but prints a message to let us know.
#'
#' @examples
#' \dontrun{
#' obs <- data.frame(Species = c("Red Fox", "Coyote", "Lace monitor"))
#' verified <- data.frame(
#'   Common_name = c("Red Fox", "Coyote", "goanna"),
#'   user_provided_name = c("fox", "", ""),
#'   binomial_verified = c("Vulpes vulpes", "Canis latrans", "Varanus varius")
#' )
#' update_common_to_binomial(obs, verified)
#' }
#' @author Tom Bruce & Zachary Amir & Shinyen Chiu
#' @export
update_common_to_binomial <- function(obs_data, verified_data, interactive = FALSE) {

  #First check our column names for what we need for this function to work that could differ between camtrapDP and OG
  #deploymentID = deployment_id
  #scientificName = Species

  # # Store the original column names
  # original_colnames = colnames(obs_data)
  #
  # # Create flags to track if we need to rename columns back at the end
  # changed_deployment_id = FALSE
  # changed_species = FALSE

  # Check and rename columns if necessary
  if ("deployment_id" %in% colnames(obs_data)) {
    colnames(obs_data)[colnames(obs_data) == "deployment_id"] = "deploymentID"
    changed_deployment_id = TRUE
  }

  if ("Species" %in% colnames(obs_data)) {
    colnames(obs_data)[colnames(obs_data) == "Species"] = "scientificName"
    changed_species = TRUE
  }

  # Lowercase for case-insensitive matching
  obs_lower <- tolower(obs_data$scientificName)
  verified_common_lower <- tolower(verified_data$Common_name)
  verified_user_lower <- tolower(verified_data$user_provided_name)

  # Create mapping from common/user names to binomial
  name_map <- setNames(verified_data$binomial_verified, verified_common_lower)
  name_map_user <- setNames(verified_data$binomial_verified, verified_user_lower)

  # First try matching common names
  match_common <- name_map[obs_lower]

  # If no match from common names, try user_provided_name
  no_match <- is.na(match_common)
  match_common[no_match] <- name_map_user[obs_lower[no_match]]

  # Get unique unmatched names to check for interactive resolution
  unmatched_names <- unique(obs_data$scientificName[is.na(match_common)])

  ### Apply logic to correcting names based on interactive or not
  if (interactive && length(unmatched_names) > 0) {
    for (missing_name in unmatched_names) {
      # Find all matching rows in verified_data (case-insensitive)
      matching_rows <- which(tolower(verified_data$Common_name) == tolower(missing_name))

      # if there are matching rows
      if (length(matching_rows) > 0) {
        # give us a message
        cat("Species:", missing_name, "\n")
        cat("Possible replacements based on Common_name:\n")
        # and print each option
        for (i in seq_along(matching_rows)) {
          cat("[", i, "] ", verified_data$binomial_verified[matching_rows[i]], "\n", sep = "")
        }
        # solicit feedback
        cat("Do you want to use a suggested replacement? (Enter the number, press Enter to skip): ")
        user_input <- as.numeric(readline())

        # if there was a replacement
        if (!is.na(user_input) && user_input >= 1 && user_input <= length(matching_rows)) {
          # make the replacement
          chosen_replacement <- verified_data$binomial_verified[matching_rows[user_input]]
          obs_data$scientificName[obs_data$scientificName == missing_name] <- chosen_replacement
        }
      } else {
        # No matching rows found - offer manual entry
        cat("No matching Common_name found for: '", missing_name, "'\n", sep = "")
        cat("Would you like to enter a new binomial name? (y/n): ")
        user_response <- tolower(readline())
        # take the new name if yes
        if (user_response == "y") {
          cat("Enter the binomial name for '", missing_name, "': ", sep = "")
          new_binomial <- readline()
          # save the updated name in scientificeName
          if (nchar(trimws(new_binomial)) > 0) {
            obs_data$scientificName[obs_data$scientificName == missing_name] <- trimws(new_binomial)
            cat("Updated '", missing_name, "' to '", trimws(new_binomial), "'\n", sep = "")
          } else {
            # or dont if they didnt provide one
            cat("No entry provided. Skipping '", missing_name, "'.\n", sep = "")
          } # end saving condition
        } # end yes/no condition
      } # end overall condition for manual entry
    } # end per missing name
  } else if (!interactive && length(unmatched_names) > 0) {
    # Non-interactive mode: check for ambiguous matches
    for (missing_name in unmatched_names) {
      matching_rows <- which(tolower(verified_data$Common_name) == tolower(missing_name))

      if (length(matching_rows) > 1) {
        chosen_replacement <- unique(verified_data$binomial_verified[matching_rows])

        if (length(chosen_replacement) > 1) {
          warning("Multiple verified binomials for '", missing_name,
                  "'. Please review or use interactive = TRUE.", call. = FALSE)
          next
        } # end greater than one condition
      } # end length matching_rows condition
    } # end missing_name in matching names loop
  } # end overall conditon

  # Replace scientificName where matches were found
  obs_data$scientificName[!is.na(match_common)] <- match_common[!is.na(match_common)]

  # Report any remaining unmatched names
  remaining_unmatched <- unique(obs_data$scientificName[is.na(match_common)])
  if (length(remaining_unmatched) > 0) {
    warning("No matching Common_name or user_provided_name found for: ",
            paste(remaining_unmatched, collapse = ", "), call. = FALSE)
  }

  return(obs_data)

} # end function
