#' Update common names to binomial nomenclature
#'
#' @description
#' This function is used to convert species names from common names into binomial nomenclature, based off the WildObs verified taxonomy database.
#' This is acheived by taking each missing species name, searching the common name column of binomial verifeid and suggesting replacements to the user from binomial_verified for any matches to the common name.
#' If a common_name is not found it will tell you.
#'
#' @param data_caps The observations tabular data that contains all observed species, with names stored in a column named either 'Species' or 'scientificName'
#' @param data_verified The WildObs verified taxonomy database, containing all user provided names (common and not) and the corresponding verified binomial nomenclature with links to GBIF & NCBI
#' @param interactive A logical value. If 'TRUE', the user will manually verify when there are multiple different binomial names. If 'FALSE', multiple binomial names are skipped.
#'
#' @return The function updates the species names in the observations tabular data (parameter 'data_caps') with the verified binomial nomenclature.
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
#' obs <- update_common_to_binomial(obs, verified, interactive = FALSE)
#' @author Tom Bruce & Zachary Amir
#' @export
update_common_to_binomial <- function(data_caps, data_verified, interactive = FALSE) {

  #First check our column names for what we need for this function to work that could differ between camtrapDP and OG
  #deploymentID = deployment_id
  #scientificName = Species

  # Store the original column names
  original_colnames = colnames(data_caps)

  # Create flags to track if we need to rename columns back at the end
  changed_deployment_id = FALSE
  changed_species = FALSE

  # Check and rename columns if necessary
  if ("deployment_id" %in% colnames(data_caps)) {
    colnames(data_caps)[colnames(data_caps) == "deployment_id"] = "deploymentID"
    changed_deployment_id = TRUE
  }

  if ("Species" %in% colnames(data_caps)) {
    colnames(data_caps)[colnames(data_caps) == "Species"] = "scientificName"
    changed_species = TRUE
  }

  #Then identify our missing species names that might need changing.

  missing_names <- setdiff(setdiff(data_caps$scientificName, data_verified$user_provided_name), data_verified$binomial_verified)#identify the missing names by comparing the Species from caps, to user_provided_name and binomial verified in verified.

  for (missing_name in missing_names) { #Then for each missing name
    matching_rows <- which(tolower(data_verified$Common_name) == tolower(missing_name)) #Search the common_names in verified for the missing name with both forced to be lower case and extract the row numbers of verified

    if (length(matching_rows) > 0) { #If there is a match

      if (interactive == TRUE) { # if we want an interactive session,

        #Print the missing name of the species
        cat("Species:", missing_name, "\n")
        #Tell the user the following could be possible replacements
        cat("Possible replacements based on Common_name:\n")

        for (i in seq_along(matching_rows)) { #For each row in match_rows,

          #Show the options using the index row numbers to extract each possible binomial_verified from the verified dataframe
          cat("[",i,"]", data_verified$binomial_verified[matching_rows[i]], "\n")
        }

        cat("Do you want to use a suggested replacement? (Enter the number, press Enter to skip): ") #Ask the user if they want to choose one of the options
        user_input <- as.numeric(readline()) #This is the interactive part where it will assign the number the user enters to user_input

        if (!is.na(user_input) && user_input >= 1 && user_input <= length(matching_rows))

          #IF statment checks as follows:
          # !is.na(user_input): Check the user input is not NA and  user has provided a number

          #user_input >= 1: Check the user has provided a positive numeric input

          #user_input <= length(matching_rows): Check the user provided number is less than or equal to the length of the availble options i.e. if there are only 3 options but the user enters 4 it cannot be possible and will not change anything and will move to the next species.

          #If all of these are met carry out chosen_replacement.

        {
          chosen_replacement <- data_verified$binomial_verified[matching_rows[user_input]] #Select the chosen replacement species based on the user's input. take the binomial name from the verified dataframe by extracting the matching row based on the users input

          # Check the number of records for the current species before replacement
          before_records <- sum(data_caps$scientificName == missing_name)

          data_caps$scientificName[data_caps$scientificName == missing_name] <- chosen_replacement #Replace the current species in the caps data frame with the chosen replacement.

          # Check the number of records for the current species after replacement
          after_records <- sum(data_caps$scientificName == chosen_replacement)

          if (before_records != after_records) {
            cat("Warning: The number of records for", missing_name, "changed after replacement. Please review.\n")
          } # end warning condition
        } # end user input condition

      }else{ # but if interactive == FALSE

        ## gather the replacement based only on matching rows and ensure there is only one value
        chosen_replacement <- unique(data_verified$binomial_verified[matching_rows])

        ## If there is more than one value,
        if(length(chosen_replacement) > 1){
          ## print a message noting of the original species name to investigate further.
          cat("Warning: The number of verified binomial nomenclature for",
              missing_name,
              "has more than one unique value. Please review outside the function, or turn the interactive argument to TRUE.\n")
          ## and skip this one and move to the next
          next
        }

        # Check the number of records for the current species before replacement
        before_records <- sum(data_caps$scientificName == missing_name)

        data_caps$scientificName[data_caps$scientificName == missing_name] <- chosen_replacement #Replace the current species in the caps data frame with the chosen replacement.

        # Check the number of records for the current species after replacement
        after_records <- sum(data_caps$scientificName == chosen_replacement)

        if (before_records != after_records) {
          cat("Warning: The number of records for", missing_name, "changed after replacement. Please review.\n")
        } # end warning condition

      } # end interactive condition

    } else {
      #If there are no matching rows, then there are no species found, so tell us which species.
      cat("No matching Common_name found for:", missing_name, "\n")

    } # end length of matching rows condition
  } # end per missing names loop

  # Change the column names back to original if they were changed
  if (changed_deployment_id) {
    colnames(data_caps)[colnames(data_caps) == "deploymentID"] <- "deployment_id"
  }

  if (changed_species) {
    colnames(data_caps)[colnames(data_caps) == "scientificName"] <- "Species"
  }

  return(data_caps)
} # end function
