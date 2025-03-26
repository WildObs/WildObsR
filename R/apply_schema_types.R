#' Apply camtrapDP Schemas to Data
#'
#' This function applies the camtrapDP schema to a frictionless data package by converting column types according to the schema definition. It includes flexible datetime parsing to accommodate multiple common formats.
#'
#' @param data A dataframe representing a resource (e.g., observations, deployments).
#' @param schema A list representing the schema that defines the expected column types
#'   and constraints for the dataset.
#' @return A dataframe with columns transformed according to the schema.
#' @details
#' The function converts columns based on the `type` field in the schema:
#' \itemize{
#'   \item \code{"datetime"}: Attempts to parse using multiple common formats and converts to ISO 8601.
#'   \item \code{"date"}: Converts to `Date` class.
#'   \item \code{"integer"}: Converts to integer.
#'   \item \code{"number"}: Converts to numeric.
#'   \item \code{"boolean"}: Converts logical-like strings (e.g., 'TRUE', 'FALSE', 'T', 'F') to logical.
#'   \item \code{"string"}: Converts to character, optionally factoring if an enum constraint is present.
#'   \item \code{"factor"}: Converts to factor.
#' }
#'
#' If an unknown field type is encountered, a warning is issued.
#'
#' @author Zachary Amir & ChatGPT
#'
#' @export
apply_schema_types <- function(data, schema) {
  for (field in schema$fields) {
    # first, gather information about this field in particular.
    col_name <- field$name
    col_type <- field$type
    col_format <- field$format
    col_enum <- field$constraints$enum

    if (col_name %in% names(data)) {

      if (col_type == "datetime") {
        # Parse datetime with flexible formats
        parsed <- tryCatch(
          as.POSIXct(data[[col_name]], format = col_format, tz = "UTC"),
          error = function(e) NULL
        )

        ## check for common formats if there are NA values in the conversion
        if (any(is.na(parsed))) {
          common_formats <- c("%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%S%z")
          for (fmt in common_formats) {
            parsed <- tryCatch(as.POSIXct(data[[col_name]], format = fmt, tz = "UTC"), error = function(e) NULL)
            if (!any(is.na(parsed))) break
          }
        }
        if (any(is.na(parsed))) {
          warning(paste("Failed to parse datetime for column:", col_name, "Please convert to common format (e.g., %Y-%m-%d %H:%M:%S")))
        } else {
          # Format the datetime as ISO 8601 with T separator
          data[[col_name]] <- format(parsed, "%Y-%m-%dT%H:%M:%S%z")
        }

      } else if (col_type == "date") {
        # Try parsing date
        data[[col_name]] <- tryCatch(
          as.Date(data[[col_name]], format = col_format),
          error = function(e) {warning(paste("Failed to parse date for column:", col_name)); data[[col_name]]}
        )

      } else if (col_type == "integer") {
        data[[col_name]] <- suppressWarnings(as.integer(data[[col_name]]))

      } else if (col_type == "number") {
        data[[col_name]] <- suppressWarnings(as.numeric(data[[col_name]]))

      } else if (col_type == "boolean") {
        # Convert boolean-like strings (e.g., 'TRUE', 'FALSE', 'T', 'F') to logical
        data[[col_name]] <- as.logical(tolower(as.character(data[[col_name]])))

        # Handle strings with enum constraints as factors
      }  else if (col_type %in% c("character","string") & !is.null(col_enum)){
        # Convert to factor with levels from enum
        data[[col_name]] <- factor(data[[col_name]], levels = unlist(col_enum))

        # or handle regular strings
      } else if (col_type %in% c("character","string")) {
        # Ensure the column is character
        data[[col_name]] <- as.character(data[[col_name]])

      } else if (col_type == "factor") {
        # Convert to factor
        data[[col_name]] <- as.factor(data[[col_name]])

      } else {
        warning(paste("Unknown field type:", col_type, "for column:", col_name))
      }
    }
  }
  data
}# end function
