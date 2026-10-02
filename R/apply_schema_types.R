#' Apply camtrapDP Schemas to Data
#'
#' This function applies the camtrapDP schema to a frictionless data package by converting column types according to the schema definition. It includes flexible datetime parsing to accommodate multiple common formats.
#'
#' @param data A dataframe representing a resource (e.g., observations, deployments).
#' @param schema A list representing the schema that defines the expected column types
#'   and constraints for the dataset.
#' @param timezone A character string specifying the timezone for datetime conversions. Defaults to "UTC".
#'   This parameter is particularly important for camtrap data to ensure temporal alignment across deployments.
#' @return A dataframe with columns transformed according to the schema.
#' @details
#' The function converts columns based on the `type` field in the schema:
#' \itemize{
#'   \item \code{"datetime"}: Attempts to parse using multiple common formats and maintains as POSIXct with specified timezone.
#'   \item \code{"date"}: Converts to `Date` class.
#'   \item \code{"integer"}: Converts to integer.
#'   \item \code{"number"}: Converts to numeric.
#'   \item \code{"boolean"}: Converts logical-like strings (e.g., 'TRUE', 'FALSE', 'T', 'F') to logical.
#'   \item \code{"string"}: Converts to character, optionally factoring if an enum constraint is present.
#'   \item \code{"factor"}: Converts to factor.
#'   \item \code{"any"}: Left unchanged, since Frictionless allows any type (e.g. Camtrap DP's `exifData`).
#' }
#'
#' If an unknown field type is encountered, a warning is issued.
#'
#' @author Zachary Amir & ChatGPT
#'
#' @export
apply_schema_types <- function(data, schema, timezone = "UTC") {
  for (field in schema$fields) {
    # first, gather information about this field in particular.
    col_name <- field$name
    col_type <- field$type
    col_format <- field$format
    col_enum <- field$constraints$enum

    if (col_name %in% names(data)) {

      if (col_type == "datetime") {
        # Use provided timezone parameter (from temporal metadata) for proper timezone handling
        tz <- timezone
        # the cells that hold a value; empty cells stay NA whatever the format
        has_value <- !is.na(data[[col_name]])

        ## an empty column has nothing to parse, so give it the right empty type
        if (!any(has_value)) {
          data[[col_name]] <- as.POSIXct(rep(NA, nrow(data)), tz = tz)
          next
        } # end empty column condition

        ## a missing format is not a parse instruction, so fall back to ISO 8601
        if (is.null(col_format)) col_format <- "%Y-%m-%dT%H:%M:%S%z"

        ## a parse has failed if nothing came back, or a real value came back NA
        parse_failed <- function(p) is.null(p) || any(is.na(p[has_value]))

        # parse the date to posixct safely
        parsed <- tryCatch(
          as.POSIXct(data[[col_name]], format = col_format, tz = tz),
          error = function(e) NULL
        )

        ## check for common formats if the declared format did not fit
        if (parse_failed(parsed)) {
          common_formats <- c("%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%S%z")
          for (fmt in common_formats) {
            parsed <- tryCatch(as.POSIXct(data[[col_name]], format = fmt, tz = tz), error = function(e) NULL)
            if (!parse_failed(parsed)) break
          } # end per format
        } # end fallback condition

        ## leave the column untouched rather than half-convert it
        if (parse_failed(parsed)) {
          warning(paste("Failed to parse datetime for column:", col_name, "Please convert to common format (e.g., %Y-%m-%d %H:%M:%S)"))
        } else {
          # store as POSIXct so date-times keep their timezone information
          data[[col_name]] <- parsed
        } # end parse result condition

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

      } else if (col_type == "any") {
        # frictionless "any" means the values can be of any type, so leave them as they are


      } else {
        warning(paste("Unknown field type:", col_type, "for column:", col_name))
      }
    } # end per col name
  } # end per field
  data
}# end function
