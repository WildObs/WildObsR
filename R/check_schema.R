#' Validate Data Against CamTrap DP Schemas
#'
#' This function checks that each camtrapDP resource (i.e. `observations`, `deployments`, `media`, and `covariates`)
#' conforms to the camtrapDP specified schema by validating field names, data types, and constraints (e.g., required fields, numeric ranges).
#' It creates missing fields if necessary, ensures data types are aligned with the schema, and reports any discrepancies. The modified dataset is returned with updates applied.
#'
#' @details
#' The function follows these steps:
#' \enumerate{
#'   \item Loops through each field in the provided schema.
#'   \item If a field does not exist in the dataset, it is created and filled with `NA` values of the appropriate type.
#'   \item Checks whether the data type of each field matches the expected type from the schema (e.g., character, numeric, datetime).
#'   \item If the field is numeric, it checks whether values fall within specified constraints (e.g., minimum, maximum).
#'   \item Checks that required fields are present and non-missing.
#' }
#'
#' @param chosen_schema A list object representing the schema to check against (e.g., `obs_schema`, `deps_schema`, `media_schema`).
#' @param data A data frame containing the dataset to be validated.
#'
#' @return The modified tabular data resource with updated fields and any necessary corrections applied.
#'
#' @examples
#' # Example usage:
#' deps_schema <- list(
#'   fields = list(
#'     list(name = "locationID", type = "string", constraints = list(required = TRUE)),
#'     list(name = "longitude", type = "number", constraints = list(minimum = -180, maximum = 180)),
#'     list(name = "latitude", type = "number", constraints = list(minimum = -90, maximum = 90)),
#'     list(name = "timestamp", type = "datetime", constraints = list(required = TRUE))
#'   )
#' )
#'
#' deps <- data.frame(
#'   locationID = c("loc1", "loc2"),
#'   longitude = c(151.2093, 153.0251),
#'   latitude = c(-33.8688, NA),
#'   timestamp = as.POSIXct(c("2023-01-01 12:00", NA))
#' )
#'
#' # Check the data against the schema
#' validated_data <- check_schema(deps_schema, deps)
#' print(validated_data)
#'
#' @author Tom Bruce & Zachary Amir
#' @export
check_schema <- function(chosen_schema, data) {
  # Loop over all the fields in the schema
  for (i in seq_along(chosen_schema$fields)) {
    #1. Select a schema
    field <- chosen_schema$fields[[i]]
    field_name <- field$name #Identify it's name
    field_type <- field$type #Identify it's type, e.g. numeric character.

    # Print the current field being processed
    cat("Checking field:", field_name, "\n")

    #2. Check that that the field excists in your defined dataframe
    # Check if the field exists in the data
    if (!field_name %in% names(data)) { #IT DOES NOT EXIST
      cat("Field", field_name, "is missing from the data. Creating it with NA.\n")

      #IF it does not exist Create the missing field with the appropriate NA type
      if (field_type == "string") {
        data[[field_name]] <- as.character(NA)
      } else if (field_type == "number") {
        data[[field_name]] <- as.numeric(NA)
      } else if (field_type == "integer") {
        data[[field_name]] <- as.integer(NA)
      } else if (field_type == "boolean") {
        data[[field_name]] <- as.logical(NA)
      } else if (field_type == "datetime") {
        data[[field_name]] <- as.POSIXct(NA)
      }

      #Move onto the next schema field

      next
    }

    #3. Extract the data type from your chosen column

    # Get the data type
    data_type <- class(data[[field_name]])

    # Handle POSIXct and POSIXt - important for date/time columns
    if ("POSIXct" %in% data_type || "POSIXt" %in% data_type) {
      data_type <- "POSIXct"  # Standardize to POSIXct for comparison
    }

    # Convert factor to character for type comparison
    if ("factor" %in% data_type) {
      data[[field_name]] <- as.character(data[[field_name]])
      data_type <- "character"
    }

    # Map schema types to R types
    expected_type <- switch(field_type,
                            "string" = "character",
                            "number" = "numeric",
                            "integer" = "integer",
                            "boolean" = "logical",
                            "datetime" = "POSIXct",  # Add datetime type
                            field_type)  # Default case

    # Ensure expected_type is a character vector
    if (is.list(expected_type)) {
      expected_type <- unlist(expected_type)
    }

    ## Make a quick exception for numeric and integers being almost equivalent in R
    if(data_type == "numeric" & expected_type == "integer"){
      # convert data type to integer
      data_type = "integer"
    }
    # now reverse it!
    if(data_type == "integer" & expected_type == "numeric"){
      # convert data type to numeric
      data_type = "numeric"
    }

    ### Now add another quick exception for ISO8601 dates in character format
    if(data_type == "character" & expected_type == "POSIXct"){
      # establish the regex for ISO8601 date-times
      iso_regex <- "^\\d{4}-\\d{2}-\\d{2}([ T]([0-2]\\d:[0-5]\\d(:[0-5]\\d)?(\\.\\d+)?(Z|[+-][0-2]\\d:[0-5]\\d)?))?$"
      # if the iso format is present in the data
      if(any(grepl(iso_regex, data[[field_name]]))){
        # update datatype to be date-time
        data_type = "POSIXct"
      } # end iso check
    } # end class check


    # Check if the actual data type in our environment matches the expected type in camtrapDP
    if (!data_type %in% expected_type) {
      cat("Error: Field", field_name, "has type", data_type, "but expected", paste(expected_type, collapse = ", "), ".\n")
    }

    #4. if it is numeric and there are constraints defined e.g. coordinates check that it falls within them if not print a warning.

    # If it's a number, check constraints like min and max if they exist
    if (field_type == "number" && is.numeric(data[[field_name]])) {
      if (!is.null(field$constraints$minimum) && any(data[[field_name]] < field$constraints$minimum, na.rm = TRUE)) {
        cat("Error: Field", field_name, "has values below the minimum constraint.\n")
      }
      if (!is.null(field$constraints$maximum) && any(data[[field_name]] > field$constraints$maximum, na.rm = TRUE)) {
        cat("Error: Field", field_name, "has values above the maximum constraint.\n")
      }
    }

    ##5. Check the dates and times are all complete and there is no missing values where appropriate e.g. obs!

    # If it's a datetime, check for valid date formats
    if (field_type == "datetime" && inherits(data[[field_name]], "POSIXct")) {
      if (any(is.na(data[[field_name]]))) {
        cat("Error: Field", field_name, "is required but contains missing values.\n")
      }
    }

    ##6. If the field is required but it has missing values this could be a problem let us know!

    # Check if the field is required and if there are missing values
    if (!is.null(field$constraints$required) && field$constraints$required) {
      if (any(is.na(data[[field_name]]))) {
        cat("Error: Field", field_name, "is required but contains missing values.\n")
      }
    }
  }

  # Return the modified data
  return(data)
}
