#' Extract Tag Key:Value Pairs into Individual Columns
#'
#' Parses pipe-delimited \code{key:value} tag columns (such as
#' \code{deploymentTags} or \code{observationTags}) in a Camtrap DP-formatted
#' dataframe and expands each unique key into its own column, appended to the
#' right-hand side of the input dataframe.
#'
#' @param df A dataframe containing a tags column. Typically a Camtrap DP
#'   \code{deployments} or \code{observations} table.
#' @param col Optional character string naming the tags column to parse. If
#'   \code{NULL} (the default), the function auto-detects by looking for a
#'   column named \code{deploymentTags} or \code{observationTags}. If neither
#'   or both are found and \code{col} is \code{NULL}, an informative error is
#'   thrown asking the user to specify \code{col} explicitly. A dataframe
#'   containing both columns is non-standard Camtrap DP; a warning is issued
#'   even when \code{col} is specified.
#' @param keep_original Logical. Default \code{TRUE}. If \code{TRUE}, the
#'   original tags column is retained in the output. If \code{FALSE}, it is
#'   dropped.
#'
#' @return The input dataframe with new tag columns appended at the right-hand
#'   side. The original column order is preserved. Each unique key found across
#'   all tag strings becomes a new column. Rows with no tags (NA or empty
#'   string) receive \code{NA} in all new tag columns. Literal \code{"NA"}
#'   values within tag strings are converted to true \code{NA}. Each new column
#'   is auto-coerced to the most specific type possible (numeric → logical →
#'   date-time); if coercion would introduce new \code{NA} values beyond those
#'   already present, the column is retained as character and a
#'   \code{warning()} is emitted identifying the column name and the values
#'   that prevented coercion.
#'
#' @details
#' Tag strings are expected to follow the pipe-delimited key:value format used
#' by Camtrap DP, for example:
#' \preformatted{"temperature:13 | moonphase:New_Crescent"}
#'
#' Pairs are split first on \code{" | "} (space-pipe-space) and then on the
#' first \code{":"} to separate key from value. Only the first colon is used
#' as the delimiter, so values may themselves contain colons (e.g. datetime
#' strings).
#'
#' Type coercion is attempted in order: numeric, then logical, then date-time
#' (ISO 8601 formats). Coercion is only applied if it does not introduce new
#' \code{NA} values beyond those already present in the column.
#'
#' @examples
#' \dontrun{
#' # Expand deployment tag columns onto the deployments dataframe
#' dep_expanded <- extract_tags(dep)
#'
#' # Expand observation tag columns onto the observations dataframe
#' obs_expanded <- extract_tags(obs)
#'
#' # Explicitly name the column to parse
#' dep_expanded <- extract_tags(dep, col = "deploymentTags")
#'
#' # Drop the original tags column from output
#' dep_expanded <- extract_tags(dep, keep_original = FALSE)
#' }
#'
#' @author Zachary Amir & Claude Opus 4.5
#'
#' @importFrom dplyr bind_cols
#' @importFrom purrr map map_chr
#' @importFrom stringr str_split str_trim
#'
#' @export
extract_tags <- function(df, col = NULL, keep_original = TRUE) {
  #
  ##
  ### Validate data input validation ----

  # Confirm the input is a dataframe before doing anything else
  if (!is.data.frame(df)) {
    stop("`df` must be a dataframe.")
  }

  # Confirm keep_original is a single logical value
  if (!is.logical(keep_original) || length(keep_original) != 1) {
    stop("`keep_original` must be a single logical value (TRUE or FALSE).")
  }

  # Define the two standard Camtrap DP tag column names
  standard_tag_cols <- c("deploymentTags", "observationTags")
  # and determine which tag is present in the df
  present_tag_cols  <- intersect(standard_tag_cols, names(df))

  # Warn if both standard tag columns are present (non-standard Camtrap DP),
  # regardless of whether the user supplied `col` or not
  if (length(present_tag_cols) == 2) {
    warning(
      "Both `deploymentTags` and `observationTags` are present in `df`. ",
      "This is non-standard Camtrap DP. ",
      "Please specify `col` explicitly to indicate which column to parse.",
      call. = FALSE
    )
  } # end double tag condition

  ### Resolve which column to parse ------

  ## if the user specified a column,
  if (!is.null(col)) {
    # validate that it actually exists in the dataframe
    if (!col %in% names(df)) {
      # and hard stop if it dopes not
      stop(sprintf("Column '%s' not found in `df`.", col))
    }
    # but if present, save the tag column
    tags_col <- col

    ## but if the user did not specify,
  } else {
    ## Auto-detect exactly one standard tag column
    # if there were no tags found in the data,
    if (length(present_tag_cols) == 0) {
      # hard stop with a clear error
      stop(
        "No tag column found in `df`. ",
        "Expected a column named `deploymentTags` or `observationTags`. ",
        "Please specify `col` explicitly."
      )
      # But if both tags were found in the data,
    } else if (length(present_tag_cols) == 2) {
      # hard stop with a clear error
      stop(
        "Both `deploymentTags` and `observationTags` found in `df`. ",
        "Please specify `col` explicitly."
      )
      ## but if there is exactly one tag
    } else {
      # use it automatically
      tags_col <- present_tag_cols
    }
  }

  ### Parse the tag strings -------------

  # Pull out the raw tag strings as a plain character vector
  raw_tags <- as.character(df[[tags_col]])

  # For each row, split the pipe-delimited string into named key:value pairs.
  # The result is a list of named character lists, one element per row.
  parsed <- purrr::map(raw_tags, function(tag_str) {

    # Treat missing values, empty strings, and literal "NA" as no tags present
    if (is.na(tag_str) || tag_str == "" || tag_str == "NA") {
      return(list())
    }

    # Step 1: split on " | " (space-pipe-space) to get individual key:value pairs
    pairs <- stringr::str_split(tag_str, " \\| ")[[1]]

    # Step 2: trim surrounding whitespace that may exist around each pair
    pairs <- stringr::str_trim(pairs)

    # Step 3: drop any empty strings that result from trailing delimiters
    pairs <- pairs[nchar(pairs) > 0]

    # Step 4: for each pair, split on the FIRST colon only.
    # This allows values to contain colons (e.g. datetime strings or URLs).
    kv <- lapply(pairs, function(pair) {
      # Find position of the first colon in this pair
      colon_pos <- regexpr(":", pair, fixed = TRUE)
      # Skip malformed pairs that contain no colon
      if (colon_pos == -1) return(NULL)
      key   <- str_trim(substring(pair, 1, colon_pos - 1))
      value <- str_trim(substring(pair, colon_pos + 1))
      list(key = key, value = value)
    })

    # Drop any NULL entries that arose from malformed (no-colon) pairs
    kv <- Filter(Negate(is.null), kv)

    # Assemble a named list of key = value for this row
    result <- stats::setNames(
      lapply(kv, `[[`, "value"),
      sapply(kv, `[[`, "key")
    )

    return(result)
  })

  ### Collect all unique keys found across all rows ----------

  all_keys <- unique(unlist(lapply(parsed, names)))

  # If no keys were found (all rows had empty/missing tags), return df unchanged
  if (length(all_keys) == 0) {
    message("No tag key:value pairs found in column '", tags_col,
            "'. Returning `df` unchanged.")
    return(df)
  }

  ### Build the tag-expanded dataframe ------------

  # For each unique key, extract the corresponding value from each row.
  # Rows that do not have a given key receive NA.
  tag_df <- as.data.frame(
    lapply(all_keys, function(key) {
      # Extract the value for this key from every row's parsed list
      vals <- purrr::map_chr(parsed, function(row_tags) {
        val <- row_tags[[key]]
        # If this row has no entry for the key, return NA
        if (is.null(val)) NA_character_ else val
      })
      # Convert any literal string "NA" to a true NA
      vals[vals == "NA"] <- NA_character_
      vals
    }),
    stringsAsFactors = FALSE
  )
  # Assign the key names as column names of the new tag dataframe
  names(tag_df) <- all_keys

  ### Attempt type coercion on each new tag column -------

  # Try to coerce each character column to a more informative type.
  # This modifies tag_df in-place and may emit warnings for columns that
  # cannot be safely coerced.
  tag_df <- .coerce_tag_columns(tag_df)

  #### Assemble the output dataframe ---------

  # Start from the original dataframe (preserving all original columns and order)
  out_df <- df

  # Optionally remove the original tags column if the user does not want it
  if (!keep_original) {
    out_df <- out_df[, setdiff(names(out_df), tags_col), drop = FALSE]
  }

  # Append the new tag columns to the right-hand side of the dataframe
  out_df <- dplyr::bind_cols(out_df, tag_df)

  return(out_df)
}


#### Define a couple internal helpers ---------------------

#' Attempt Type Coercion for Expanded Tag Columns
#'
#' Internal helper for \code{\link{extract_tags}}. Iterates over each character
#' column and attempts to coerce it to a more specific type in priority order:
#' numeric → logical → date-time (ISO 8601). Coercion is only applied when it
#' does not introduce new \code{NA} values. If all coercion attempts fail, the
#' column is retained as character and a \code{warning()} is emitted identifying
#' the column name and the values that prevented coercion.
#'
#' @param df A dataframe of character columns (the expanded tag columns).
#' @return The same dataframe with columns coerced where it is safe to do so.
#' @keywords internal
.coerce_tag_columns <- function(df) {

  # ISO 8601 / common date-time formats to try in order
  datetime_formats <- c(
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%dT%H:%M",
    "%Y-%m-%d"
  )

  for (col_name in names(df)) {
    original   <- df[[col_name]]
    # Record which positions are NA before any coercion attempt
    already_na <- is.na(original)

    # ---- Attempt 1: numeric coercion -----------------------------------------
    numeric_vals <- suppressWarnings(as.numeric(original))
    # Coercion is safe when it does NOT produce new NAs beyond those already present
    new_na_from_numeric <- is.na(numeric_vals) & !already_na
    if (!any(new_na_from_numeric)) {
      df[[col_name]] <- numeric_vals
      next  # Move on to the next column
    }

    # ---- Attempt 2: logical coercion -----------------------------------------
    # Only attempt if every non-NA value is a recognised boolean string
    non_na_vals  <- original[!already_na]
    lower_non_na <- tolower(non_na_vals)
    if (length(lower_non_na) > 0 && all(lower_non_na %in% c("true", "false"))) {
      logical_vals <- suppressWarnings(as.logical(original))
      new_na_from_logical <- is.na(logical_vals) & !already_na
      if (!any(new_na_from_logical)) {
        df[[col_name]] <- logical_vals
        next  # Move on to the next column
      }
    }

    # ---- Attempt 3: date-time coercion ---------------------------------------
    dt_result <- .try_parse_datetime(original, datetime_formats)
    if (!is.null(dt_result)) {
      new_na_from_dt <- is.na(dt_result) & !already_na
      if (!any(new_na_from_dt)) {
        df[[col_name]] <- dt_result
        next  # Move on to the next column
      }
    }

    # ---- All coercion attempts failed: keep as character and warn ------------
    # Report the values that blocked numeric coercion (the first-priority type)
    failed_vals <- unique(original[new_na_from_numeric])
    warning(
      sprintf(
        "Column '%s' could not be safely coerced from character; retaining as character. ",
        col_name
      ),
      sprintf(
        "Values that prevented numeric coercion: %s",
        paste(failed_vals, collapse = ", ")
      ),
      call. = FALSE
    )
    # Column is left unchanged (already character)
  }

  return(df)
}


#' Try to Parse a Character Vector as Date-Time
#'
#' Internal helper that attempts to parse a character vector using a list of
#' date-time format strings. Returns a \code{POSIXct} vector if one of the
#' formats parses all non-NA values cleanly, or \code{NULL} if no format
#' succeeds.
#'
#' @param x A character vector.
#' @param formats A character vector of format strings (as accepted by
#'   \code{\link{strptime}}).
#' @return A \code{POSIXct} vector (UTC) if parsing succeeds, or \code{NULL}.
#' @keywords internal
.try_parse_datetime <- function(x, formats) {
  for (fmt in formats) {
    # Attempt to parse the entire vector with this format
    parsed <- suppressWarnings(as.POSIXct(x, format = fmt, tz = "UTC"))
    # Check that every non-NA input produced a non-NA output
    input_not_na <- !is.na(x)
    if (all(!is.na(parsed[input_not_na]))) {
      return(parsed)
    }
  }
  # None of the formats produced a clean parse
  return(NULL)
}
