#' Convert a Data Frame to a Nested List
#'
#' This function is used internally in the wildobs_dp_download() function to convert a data frame into a nested list, ensuring that vector fields are flattened while preserving list-like structures.
#' @seealso \code{\link{wildobs_dp_download}} for downloading and bundling data packages.
#'
#' @param df A data frame or a list containing a data frame.
#' @return A list representation of the input data frame. If the input is a single-row data frame, it returns a named list. If the input is a list containing a data frame, it returns a list where each row is represented as a nested list.
#'
#' @details
#' The function performs the following transformations:
#' - Converts the first row of a data frame into a named list.
#' - If the input is a list containing a data frame, it processes each row separately and returns a list of lists.
#' - Ensures that vector fields (e.g., `path`, `deploymentGroups`) are flattened to prevent unnecessary nested lists.
#' - Stops execution if the input is neither a data frame nor a list containing a data frame.
#'
#' @examples
#' # Example with a single-row data frame
#' df <- data.frame(a = 1, b = "text", c = list(c(1, 2, 3)))
#' convert_df_to_list(df)
#'
#' # Example with a list containing a data frame
#' df_list <- list(data.frame(a = 1:2, b = c("x", "y")))
#' convert_df_to_list(df_list)
#'
#' @importFrom purrr map
#'
#' @author Zachary Amir & ChatGPT
#'
#' @keywords internal
convert_df_to_list <- function(df) {
  if (is.data.frame(df)) {
    # Convert first row to list and flatten any list-like columns
    row_list <- as.list(df[1, , drop = FALSE])

    # Ensure no nested lists (i.e., flatten vector fields like `path` and `deploymentGroups`)
    row_list <- purrr::map(row_list, function(x) {
      if (is.list(x) && length(x) == 1) unlist(x, recursive = FALSE) else x
    })

    return(row_list)

  } else if (is.list(df) && is.data.frame(df[[1]])) {
    # If input is a list containing a dataframe, process row-wise
    return(purrr::map(seq_len(nrow(df[[1]])), function(i) {
      row_list <- as.list(df[[1]][i, , drop = FALSE])

      # Flatten list-like fields
      row_list <- purrr::map(row_list, function(x) {
        if (is.list(x) && length(x) == 1) unlist(x, recursive = FALSE) else x
      })

      return(row_list)
    }))

  } else {
    stop("Input must be a dataframe or a list containing a dataframe.")
  }
}


#' Check if a Spatial Value is Empty
#'
#' This function is used internally in the wildobs_dp_download() function to check whether a given value is NULL, entirely NA, or the string `"NULL"`.
#' @seealso \code{\link{wildobs_dp_download}} for downloading and bundling data packages.
#'
#' @param x A value to check.
#' @return `TRUE` if the value is NULL, entirely NA, or the string `"NULL"`, otherwise `FALSE`.
#'
#' @examples
#' is_empty_spatial(NULL)        # TRUE
#' is_empty_spatial(NA)          # TRUE
#' is_empty_spatial("NULL")      # TRUE
#' is_empty_spatial("polygon")   # FALSE
#'
#' @author Zachary Amir & ChatGPT
#'
#' @keywords internal
is_empty_spatial <- function(x) {
  is.null(x) || all(is.na(x)) || x == "NULL" #|| !is.character(x)
}


#' Check if Temporal Data is Empty
#'
#' This function is used internally in the wildobs_dp_download() function to check whether a temporal dataset (e.g., a dataframe or vector) contains only `NA` values, indicating it is empty.
#' @seealso \code{\link{wildobs_dp_download}} for downloading and bundling data packages.
#'
#' @param x A dataframe or atomic vector representing temporal data.
#' @return `TRUE` if the temporal data is entirely `NA`, otherwise `FALSE`.
#'
#' @examples
#' df <- data.frame(start = NA, end = NA)
#' is_empty_temporal(df)  # TRUE
#'
#' vec <- c(NA, NA, NA)
#' is_empty_temporal(vec)  # TRUE
#'
#' df2 <- data.frame(start = "2023-01-01", end = NA)
#' is_empty_temporal(df2)  # FALSE
#'
#' @author Zachary Amir & ChatGPT
#'
#' @keywords internal
is_empty_temporal = function(x) {
  if (is.data.frame(x) && all(is.na(x[1, ]))) return(TRUE)  # Check if all values in the first row are NA
  if (is.atomic(x) && all(is.na(x))) return(TRUE)  # Handle direct NA values like `timeZone`
  return(FALSE)
}


#' Recursively Clean a List by Removing NULL, NA, and Empty Lists
#'
#' This function is used internally in the wildobs_dp_download() function to recursively traverses a list and removes any elements that are:
#' - `NULL`
#' - `NA`
#' - Empty lists (`list()`)
#' @seealso \code{\link{wildobs_dp_download}} for downloading and bundling data packages.
#'
#' @param x A nested list containing various data types.
#' @return A cleaned version of the list with all `NULL`, `NA`, and empty lists removed.
#'
#' @examples
#' example_list <- list(a = 1, b = NULL, c = list(d = NA, e = list()))
#' clean_list_recursive(example_list)
#' # Returns: list(a = 1)
#'
#' nested_list <- list(a = 1, b = list(c = NULL, d = list(e = NA)))
#' clean_list_recursive(nested_list)
#' # Returns: list(a = 1)
#'
#' @author Zachary Amir & ChatGPT
#'
#' @keywords internal
clean_list_recursive <- function(x) {
  ## Remember! resources > schema > fields
  if (is.list(x)) {
    # Apply the function recursively to each element
    x <- lapply(x, clean_list_recursive)
    # Remove elements that are NULL, NA, or empty lists
    x <- x[!sapply(x, function(el) {
      is.null(el) || (is.atomic(el) && all(is.na(el))) || (is.list(el) && length(el) == 0)
    })]
  }
  x
} # end function


#' Reformat Schema Fields for Frictionless Data Package
#'
#' This function is used internally in the wildobs_dp_download() function to take a set of schema fields and reformats them to ensure proper structure for use in a Frictionless Data Package.
#' It processes constraints dynamically, removing `NULL` and `NA` values, and retains relevant metadata such as descriptions, units, and formats.
#' @seealso \code{\link{wildobs_dp_download}} for downloading and bundling data packages.
#'
#' @param fields A dataframe containing schema field information, including constraints,
#'   descriptions, and other metadata.
#' @return A list of formatted schema fields, with cleaned constraints and metadata.
#'
#' @examples
#' fields <- data.frame(
#'   name = c("scientificName", "individualID"),
#'   description = c("Species name", "Identification of individual animals"),
#'   constraints = list(list(required = TRUE, unique = FALSE), list(required = FALSE)),
#'   type = c("string", "string"),
#'   example = c("Wallabia bicolor", "NA"),
#'   format = c(NA, "default")
#' )
#' formatted_fields <- reformat_fields(fields)
#'
#' @seealso \code{\link{clean_list_recursive}} for removing empty list elements.
#'
#' @author Zachary Amir & ChatGPT
#'
#' @keywords internal
reformat_fields <- function(fields) {
  ## can load data for testing.
  # fields = resources[resources$name == "deployments",]
  # fields = fields$schema$fields[[1]]
  purrr::map(seq_along(fields$name), function(l) {

    # Build constraints dynamically (only include non-empty constraints)
    constraints <- list(
      required = if (!is.null(fields$constraints$required[l]) && !is.na(fields$constraints$required[l])) fields$constraints$required[l],
      unique = if (!is.null(fields$constraints$unique[l]) && !is.na(fields$constraints$unique[l])) fields$constraints$unique[l],
      minimum = if (!is.null(fields$constraints$minimum[l]) && !is.na(fields$constraints$minimum[l])) fields$constraints$minimum[l],
      maximum = if (!is.null(fields$constraints$maximum[l]) && !is.na(fields$constraints$maximum[l])) fields$constraints$maximum[l],
      enum = if (!is.null(fields$constraints$enum[l]) && !is.na(fields$constraints$enum[l])) fields$constraints$enum[l]
    )

    # Remove empty (NULL & NA) constraints
    constraints <- clean_list_recursive(constraints)

    # Build the final field list
    field_list =  list(
      name = fields$name[l],
      description = fields$description[l],
      `skos:broadMatch` = if(length(fields$`skos:broadMatch`[l]) > 0)  fields$`skos:broadMatch`[l] else NULL,
      `skos:exactMatch` = if(length(fields$`skos:exactMatch`[l]) > 0) fields$`skos:exactMatch`[l] else NULL,
      `skos:narrowMatch` = if(length(fields$`skos:narrowMatch`[l]) > 0) fields$`skos:narrowMatch`[l] else NULL,
      unit =if(length(fields$unit[l]) > 0) fields$unit[l] else NULL,
      constraints = if (length(constraints) > 0) constraints else NULL,  # Only include constraints if non-empty
      example = fields$example[l],
      format = if(length(fields$format[l]) > 0) fields$format[l] else NULL,
      type = if(length(fields$type[l]) > 0) fields$type[l] else NULL
    )
    # Clean recursively to remove NULL and NA values at all levels
    clean_list_recursive(field_list)
  })
} # end function


#' Reformat a Schema for a Frictionless Data Package
#'
#' This function is used internally in the wildobs_dp_download() function to transform a schema into a properly structured list, ensuring compatibility with the Frictionless Data Package format.
#' It processes fields using \code{\link{reformat_fields}} to maintain correct metadata structure.
#'
#' @seealso \code{\link{wildobs_dp_download}} for downloading and bundling data packages.
#'
#' @param schema A list containing schema metadata, typically extracted from a Frictionless Data Package.
#' @return A formatted list representing the schema, including name, title, description, fields, missing values, primary key, and foreign keys.
#' @seealso
#'   \code{\link{reformat_fields}} for structuring field metadata,
#'   \code{\link{wildobs_dp_download}} for downloading and bundling data packages.
#'
#' @author Zachary Amir & ChatGPT
#'
#' @keywords internal
reformat_schema <- function(schema) {
  ## can load data for testing:
  # schema = resources[resources$name == "deployments", "schema"]
  list(
    name = schema$name,
    title = schema$title,
    description = schema$description,
    fields = reformat_fields(schema$fields[[1]]),  # Use custom function for properly formatted fields
    missingValues = schema$missingValues,
    primaryKey = schema$primaryKey,
    foreignKeys = schema$foreignKeys
  )
} # end function



#' Calculate Mode of a Vector
#'
#' Returns the most frequently occurring value in a vector.
#'
#' @param x A vector of values
#'
#' @return The mode (most frequent value) in the vector, or NA if the input
#'   is empty or all values are NA
#'
#' @examples
#' Mode(c(1, 2, 2, 3, 3, 3))
#' Mode(c("a", "b", "b", "c"))
#'
#' @keywords internal
Mode <- function(x) {
  x <- x[!is.na(x)]  # Remove NA values
  if (length(x) == 0) {
    return(NA)  # Return NA if all values were NA
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#' Convert Area to Hexagon Apothem
#'
#' Calculates the apothem (distance from center to midpoint of side) for
#' a regular hexagon given its area in square meters.
#'
#' @param area_m2 Numeric vector of hexagon areas in square meters.
#'   Must be positive values.
#'
#' @return A numeric vector of apothem values (in meters) with names
#'   indicating the original area (formatted as "km2" or "m2")
#'
#' @details
#' Calculates hexagon dimensions using standard formulas:
#' side length = sqrt((2 * area) / (3 * sqrt(3)))
#' apothem = (side * sqrt(3)) / 2
#'
#' @examples
#' area_to_apothem(1e6)  # 1 km²
#' area_to_apothem(c(1e6, 5e6))  # Multiple areas
#'
#' @keywords internal
area_to_apothem <- function(area_m2) {
  if (!is.numeric(area_m2) || any(area_m2 <= 0)) {
    stop("`scales` must be numeric and positive, representing area in square meters (m2).")
  }

  # Step 1: Calculate side length from area
  side <- sqrt((2 * area_m2) / (3 * sqrt(3)))

  # Step 2: Calculate apothem from side length
  apothem <- (side * sqrt(3)) / 2

  # Step 3: Create user-friendly names
  # Name the vector based on whether area is divisible by 1000^2 (i.e., km2)
  name_labels <- ifelse(
    area_m2 %% 1e6 == 0,
    paste0(area_m2 / 1e6, "km2"),
    paste0(area_m2, "m2")
  )
  # save the names
  names(apothem) <- name_labels
  # return the vector
  return(apothem)
}

#
##
### Data mob blank template functions
##
#

#' Rename or Add a Column
#'
#' Renames an existing column or creates a new column with NA values if the
#' old column name is not provided.
#'
#' @param df A data frame
#' @param new_name Character string of the new column name
#' @param old_name Character string of the old column name. If empty, NA, or
#'   length 0, a new column is created instead.
#'
#' @return A data frame with the renamed or new column
#'
#' @examples
#' df <- data.frame(x = 1:3, y = 4:6)
#' rename_or_add_column(df, "new_col", "x")
#' rename_or_add_column(df, "blank_col", "")
#'
#' @keywords internal
rename_or_add_column <- function(df, new_name, old_name) {
  if (length(old_name) == 0 || is.na(old_name) || old_name == "") {
    df[[new_name]] <- NA  # Create new column with NA
  } else {
    names(df)[names(df) == old_name] <- new_name  # Rename column
  }
  return(df)
}

#' Get Decimal Places from Numeric Values
#'
#' Extracts the number of decimal places from a numeric value, removing
#' trailing zeros.
#'
#' @param x A numeric value
#'
#' @return An integer representing the number of significant decimal places
#'
#' @examples
#' get_decimal_places(12.345)
#' get_decimal_places(10.5)
#' get_decimal_places(5.00)
#'
#' @keywords internal
get_decimal_places <- function(x) {
  x_str <- as.character(x)
  # Remove trailing zeroes after the decimal
  decimals <- sub("^[^.]*\\.?", "", x_str)
  decimals <- gsub("0+$", "", decimals)
  nchar(decimals)
}


#
##
### camDB send data to MongoDB functions
##
#

#' Clean Data Frame for MongoDB
#'
#' Converts character "NA" strings and NaN values to proper NA values,
#' preparing the data frame for MongoDB insertion.
#'
#' @param df A data frame to clean
#'
#' @return A data frame with "NA" character strings and NaN values converted to NA
#'
#' @examples
#' df <- data.frame(x = c(1, NaN, 3), y = c("a", "NA", "c"))
#' mongo_clean_df(df)
#'
#' @keywords internal
mongo_clean_df <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.character(x)) x[x == "NA"] <- NA
    if (is.numeric(x)) x[is.nan(x)] <- NA
    return(x)
  })
  return(df)
}

#' Prepare Document for MongoDB
#'
#' Converts date columns to MongoDB extended JSON format and handles
#' collection-specific formatting requirements.
#'
#' @param doc A list or data frame representing a document
#' @param date_cols Character vector of column names containing dates
#' @param collection Character string specifying the collection type
#'   (e.g., "observations")
#'
#' @return A list with date columns formatted as MongoDB extended JSON
#'   (`$date` format) and collection-specific fields processed
#'
#' @details
#' Character date strings are converted to POSIXct format (UTC) before
#' formatting. POSIXt objects are converted directly to ISO 8601 format.
#' For the "observations" collection, missing classificationTimestamp
#' values are set to NA (converted to JSON null).
#'
#' @keywords internal
mongo_prepare_doc <- function(doc, date_cols, collection) {
  # Process date columns: if x is character, convert it to POSIXct first;
  # if it’s a valid POSIXt value then convert to MongoDB extended JSON.
  doc[date_cols] <- lapply(doc[date_cols], function(x) {
    # If date-time is stored as a character value, try converting it.
    if (is.character(x)) {
      dt <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
      if (!is.na(dt)) {
        return(list(`$date` = format(dt, "%Y-%m-%dT%H:%M:%OS3Z")))
      } else {
        return(NA)
      }
      # but if stored as proper date-time, convert to mongo format
    } else if (inherits(x, "POSIXt") && !is.na(x)) {
      return(list(`$date` = format(x, "%Y-%m-%dT%H:%M:%OS3Z")))
    } else {
      return(NA)
    }
  })

  # For the observations collection: if classificationTimestamp is missing,
  # set it to NA so that jsonlite converts it to JSON null.
  if (collection == "observations" && (is.null(doc[["classificationTimestamp"]]) || is.na(doc[["classificationTimestamp"]]))) {
    doc[["classificationTimestamp"]] <- NA
  }
  # return the cleaned up list
  return(doc)
}

# Convert a single row (data frame slice) to JSON using our preparation function.
convert_row_to_json <- function(row, date_cols, collection) {
  doc <- as.list(row)
  doc <- mongo_prepare_doc(doc, date_cols, collection)
  jsonlite::toJSON(doc, auto_unbox = TRUE, na = "null", pretty = FALSE)
}

#' Convert Data Frame Row to JSON
#'
#' Converts a single row to a JSON string with proper MongoDB formatting
#' for date fields.
#'
#' @param row A single row from a data frame (typically obtained via subsetting)
#' @param date_cols Character vector of column names containing dates
#' @param collection Character string specifying the collection type
#'   (e.g., "observations")
#'
#' @return A character string containing the JSON representation of the row
#'
#' @details
#' This is a wrapper function that converts a row to a list, applies
#' MongoDB-specific formatting via `mongo_prepare_doc()`, and then
#' serializes to JSON with `jsonlite::toJSON()`.
#'
#' @keywords internal
mongo_format_dates <- function(x) {
  # Check if the value is a character. Adjust the format string if needed.
  if (is.character(x)) {
    dt <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    if (!is.na(dt)) {
      return(list(`$date` = format(dt, "%Y-%m-%dT%H:%M:%OS3Z")))
    } else {
      return(NA)
    }
  } else if (inherits(x, "POSIXt") && !is.na(x)) {
    return(list(`$date` = format(x, "%Y-%m-%dT%H:%M:%OS3Z")))
  } else {
    return(NA)
  }
}

#' Convert to Standard Date Format
#'
#' Converts various date formats (including Unix epoch timestamps) to
#' standard ISO 8601 format.
#'
#' @param x A date value: character string, POSIXct, or Date object.
#'   Can be in multiple formats including ISO 8601, common date-time formats,
#'   or Unix epoch (seconds since 1970-01-01).
#'
#' @return A character string in "YYYY-MM-DD HH:MM:SS" format (UTC),
#'   or empty string if input is NA or empty
#'
#' @details
#' Attempts to parse input in the following order:
#' 1. If already a POSIXt or Date object, format directly
#' 2. Try common date-time string formats
#' 3. If all digits, treat as Unix epoch timestamp
#' 4. Return input as-is if parsing fails
#'
#' All output times are in UTC timezone.
#'
#' @examples
#' convert_to_standard_date("2022-06-01 12:30:45")
#' convert_to_standard_date("1654070445")  # Unix epoch
#' convert_to_standard_date(as.POSIXct("2022-06-01"))
#'
#' @keywords internal
convert_to_standard_date <- function(x) {
  ## Assuming x here is a single element

  # Remove extraneous whitespace:
  x <- trimws(x)

  # If x is NA or an empty string, return an empty string.
  if (is.na(x) || x == "") return("")

  # If x is already a date-time (POSIXct), simply format it:
  if(inherits(x, "POSIXt") || inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  }

  # Otherwise, assume x is a character string.
  # First, try to parse it using common formats.
  dt <- tryCatch(
    as.POSIXct(x, tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%OS")),
    error = function(e) NA
  )

  if (!is.na(dt)) {
    return(format(dt, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  }

  # If that fails, check if the string is only digits (i.e. a Unix epoch)
  if (grepl("^[0-9]+$", x)) {
    dt_epoch <- tryCatch(
      as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "UTC"),
      error = function(e) NA
    )
    if (!is.na(dt_epoch)) {
      return(format(dt_epoch, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    }
  }

  # Otherwise, return x as is (or NA)
  return(x)
}

#' Canonicalize Data Frame for Comparison
#'
#' Standardizes a data frame for consistent comparison by converting dates,
#' rounding numerics, and normalizing missing values.
#'
#' @param df A data frame to canonicalize
#' @param date_cols Character vector of column names containing dates
#' @param numeric_round Integer specifying decimal places for rounding
#'   numeric columns (default: 2)
#'
#' @return A data frame with all columns as character strings, dates
#'   standardized, numerics rounded and NaN converted to NA, columns
#'   sorted alphabetically, and missing values normalized to empty strings
#'
#' @details
#' This function prepares data frames for comparison by:
#' 1. Converting specified date columns using `convert_to_standard_date()`
#' 2. Rounding numeric columns and replacing NaN with NA
#' 3. Converting all columns to character type
#' 4. Standardizing missing values (NA and "" treated as equivalent)
#' 5. Sorting columns alphabetically
#'
#' @examples
#' df <- data.frame(
#'   name = c("Alice", "Bob"),
#'   date = c("2022-06-01", "2022-07-15"),
#'   value = c(1.234, 5.678)
#' )
#' canonicalize_df(df, date_cols = "date", numeric_round = 2)
#'
#' @keywords internal
canonicalize_df <- function(df, date_cols, numeric_round = 2) {
  df_copy <- as.data.frame(df, stringsAsFactors = FALSE)  # Convert input to data.frame

  # Process date columns using our helper function.
  for (col in date_cols) {
    if (col %in% names(df_copy)) {
      # Use sapply to apply the function elementwise.
      df_copy[[col]] <- sapply(df_copy[[col]], convert_to_standard_date)
    }
  }

  # Identify numeric columns and round them to 1 decimal places (or adjust as needed).
  numeric_cols <- sapply(df_copy, is.numeric)
  # For these columns, replace NaN with NA, then round to the value provided in the function
  df_copy[numeric_cols] <- lapply(df_copy[numeric_cols], function(x) {
    x[is.nan(x)] <- NA
    round(x, numeric_round)
  })

  # Convert all columns to character to have a consistent basis for comparison.
  df_char <- data.frame(lapply(df_copy, as.character), stringsAsFactors = FALSE)

  # For all character columns, standardize missing values:
  # Replace NA with "" so that both NA and "" are treated equally.
  for (col in names(df_char)) {
    if (is.character(df_char[[col]])) {
      df_char[[col]][is.na(df_char[[col]])] <- ""
    }
  }

  # Order the columns alphabetically for consistency.
  df_char <- df_char[, sort(names(df_char))]

  return(df_char)
}

#' Compute Row Hashes
#'
#' Generates a hash for each row in a data frame to enable row-level
#' comparison and duplicate detection.
#'
#' @param df A data frame
#'
#' @return A character vector of hash values (one per row), computed using
#'   MD5 algorithm via `digest::digest()`
#'
#' @details
#' Columns are sorted alphabetically before hashing to ensure consistent
#' hash values across data frames with the same data in different column orders.
#' Row values are concatenated with "||" separator before hashing.
#'
#' @examples
#' df <- data.frame(x = 1:3, y = c("a", "b", "c"))
#' compute_row_hashes(df)
#'
#' @keywords internal
compute_row_hashes <- function(df) {
  # Ensure the columns are in a consistent order.
  df <- df[, sort(names(df)), drop = FALSE]
  # for each row,
  apply(df, 1, function(row) {
    # Create the hashes using digest and concatenate all values with a separator.
    digest(paste(row, collapse = "||"))
  })
}

