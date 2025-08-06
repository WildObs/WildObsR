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
#' @export
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
#' @export
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
#' @export
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
#' @export
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
#' @export
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
#' @export
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



# Quick function for mode, since R doesnt have a built in one
Mode <- function(x) {
  x <- x[!is.na(x)]  # Remove NA values
  if (length(x) == 0) {
    return(NA)  # Return NA if all values were NA
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}



## Quick function for converting vector of km2 areas to a named vector list for spatial_hexagon_generator()
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
