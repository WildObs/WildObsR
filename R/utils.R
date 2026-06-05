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
#'
#' @keywords internal
area_to_apothem <- function(area_m2) {
  if (!is.numeric(area_m2) || any(area_m2 <= 0)) {
    stop("`scales` must be numeric and positive, representing area in square meters (m2).")
  }

  # Step 1: Calculate side length from area
  side <- sqrt((2 / (3 * sqrt(3))) * area_m2)

  # Step 2: Calculate apothem from side length
  apothem <- (side * sqrt(3)) / 2

  # Step 3: Create user-friendly names
  # Name the vector based on whether area is divisible by 1000^2 (i.e., km2)
  name_labels <- ifelse(
    abs(area_m2 %% 1e6) < 1e-6,
    paste0(formatC(area_m2 / 1e6, format = "f", drop0trailing = TRUE), "km2"),
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
#'
#' @keywords internal
get_decimal_places <- function(x) {
  x_str <- as.character(x)
  # Remove trailing zeroes after the decimal
  decimals <- sub("^[^.]*\\.?", "", x_str)
  decimals <- gsub("0+$", "", decimals)
  nchar(decimals)
}

#' Format Spatial Bounding Boxes into GeoJSON-Style List
#'
#' This internal helper reformats a `spatial` object’s bounding box (`bbox`)
#' column into a lightweight, GeoJSON-like list structure. It is used within
#' WildObsR functions that serialize spatial metadata for database or JSON export.
#'
#' The function handles several possible input formats:
#' \itemize{
#'   \item A nested data frame already containing \code{xmin}, \code{ymin},
#'         \code{xmax}, and \code{ymax} columns.
#'   \item A character string of four comma-separated coordinates
#'         (e.g., \code{"152.0796, -27.7047, 152.0934, -27.6972"}).
#'   \item A numeric vector of length four giving bounding box coordinates.
#' }
#'
#' @param spatial_obj A data frame containing a \code{bbox} column, where each
#'   element corresponds to one or more location bounding boxes. Each entry may
#'   be a data frame, a character string, or a numeric vector.
#'
#' @return A named list in simplified GeoJSON style:
#' \preformatted{
#' list(
#'   type = "polygon",
#'   bbox = list(
#'     Location1 = list(list(xmin = ..., ymin = ..., xmax = ..., ymax = ...)),
#'     Location2 = list(list(xmin = ..., ymin = ..., xmax = ..., ymax = ...))
#'   )
#' )
#' }
#' or \code{NULL} if no valid bounding boxes are found.
#'
#' @details
#' The resulting object mimics a minimal GeoJSON \emph{Polygon} structure but
#' preserves location-wise bounding boxes rather than coordinate rings.
#' This internal utility is primarily used when converting spatial metadata
#' to JSON for MongoDB insertion or validation.
#'
#' @examples
#' \dontrun{
#' # Example input mimicking a CamtrapDP-style spatial object
#' spatial_obj <- data.frame(
#'   bbox = I(list(data.frame(
#'     Site_A = list(list(xmin = 152.0, ymin = -27.7, xmax = 152.1, ymax = -27.6)),
#'     Site_B = list(list(xmin = 151.9, ymin = -27.8, xmax = 152.0, ymax = -27.7))
#'   )))
#' )
#'
#' # Format to GeoJSON-style list
#' geojson_list <- format_spatial_to_geojson(spatial_obj)
#' str(geojson_list)
#' }
#'
#' @keywords internal
#' @noRd
format_spatial_to_geojson <- function(spatial_obj) {
  # Return NULL if not a data.frame
  if (!is.data.frame(spatial_obj)) return(NULL)

  #--- CASE 1: Features-based spatial object
  if ("features" %in% names(spatial_obj)) {
    # Extract the data frame that contains all features
    features_df <- spatial_obj$features[[1]]
    # If missing or invalid geometry → fall back to bbox instead of returning NULL
    if (!is.null(features_df) && "geometry" %in% names(features_df)){
      # extract geometry and property info
      geometries <- features_df$geometry
      props <- features_df$properties

      # assemble GeoJSON-style list of features
      features_out <- vector("list", nrow(features_df))
      # Loop over each feature and reformat it into GeoJSON-style structure
      for (i in seq_len(nrow(features_df))) {
        features_out[[i]] <- list(
          type = "Feature",
          properties = as.list(props[i, , drop = FALSE]),
          geometry = list(
            type = geometries$type[i],
            coordinates = geometries$coordinates[[i]]
          )
        )
      } # end per nrow features
      # Return the final GeoJSON-style FeatureCollection
      return(list(
        type = "FeatureCollection",
        features = features_out
      ))
    } # end null and geom condition
  }# end feature search condition

  # Handle the case where the spatial object only has a "bbox" element
  # This matches your earlier schema where bounding boxes are stored per location
  # check that bbox is in the names
  if (!"bbox" %in% names(spatial_obj)) return(NULL)
  # if so, return the DF
  bbox_df <- spatial_obj$bbox
  # but make sure its a DF and return NULL if not
  if (!is.data.frame(bbox_df)) return(NULL)

  # Prepare an empty list to store formatted bounding boxes
  bbox_out <- list()
  # Loop through each column in the bbox data frame (each corresponds to a location)
  for (loc_name in names(bbox_df)) {
    # Extract the first element from each list cell
    val <- bbox_df[[loc_name]][[1]]
    # Skip if missing or explicitly NULL
    if (is.null(val) || (is.character(val) && val == "NULL")) next

    # Case 1: already a data frame with xmin, ymin, xmax, ymax
    if (is.data.frame(val) && all(c("xmin", "ymin", "xmax", "ymax") %in% names(val))) {
      bbox_out[[loc_name]] <- list(list(
        xmin = val$xmin[1], ymin = val$ymin[1],
        xmax = val$xmax[1], ymax = val$ymax[1]
      ))
      next
    }
    # Case 2: character string formatted as four comma-separated coordinates
    if (is.character(val)) {
      nums <- suppressWarnings(as.numeric(strsplit(val, "\\s*,\\s*")[[1]]))
      if (length(nums) == 4 && all(is.finite(nums))) {
        bbox_out[[loc_name]] <- list(list(
          xmin = nums[1], ymin = nums[2], xmax = nums[3], ymax = nums[4]
        ))
      }
      next
    }
    # Case 3: numeric vector of length 4
    if (is.numeric(val) && length(val) == 4) {
      bbox_out[[loc_name]] <- list(list(
        xmin = val[1], ymin = val[2], xmax = val[3], ymax = val[4]
      ))
      next
    }
  }
  # Return NULL if no valid bounding boxes were found
  if (length(bbox_out) == 0) return(NULL)
  # Construct and return a simple GeoJSON-style object
  list(type = "polygon", bbox = bbox_out)
}


#' Extract bounding boxes from spatial metadata
#'
#' This internal helper function extracts and normalizes bounding boxes
#' from the `spatial` slot of the metadata collection returned by MongoDB
#' This function supports both the flat spatial bounding boxes (`spatial$bbox`)
#' and the nested geoJSON-style (`spatial$features`) formats.
#'
#' The output is a standardized data frame containing one row per
#' locationName with the corresponding bounding box coordinates and project ID.
#'
#' @param metadata A list-like dataframe returned by MongoDB when reading the
#' metadata collection into R
#'   Must include a `spatial` element with either:
#'   \itemize{
#'     \item `spatial$bbox` — a data frame of bounding boxes in legacy format.
#'     \item `spatial$features` — a list of GeoJSON-style features with
#'       `geometry$coordinates` entries.
#'   }
#'
#' @return A tibble (data frame) with columns:
#'   \describe{
#'     \item{locationName}{Character string indicating the name of the location.}
#'     \item{xmin, ymin, xmax, ymax}{Numeric bounding box coordinates.}
#'     \item{id}{Character string identifying the data package ID associated with each row.}
#'   }
#'
#' @details
#' The function automatically detects whether the metadata object uses
#' the flat bbox or nested geoJSON spatial format.
#' If both are present, results are merged and duplicates removed.
#'
#' Invalid, empty, or malformed bounding boxes are skipped with NA values returned.
#'
#' @importFrom tibble tibble
#' @importFrom purrr map map_dbl map_chr map_lgl imap_dfr
#' @importFrom dplyr mutate filter select distinct bind_rows row_number
#' @importFrom tidyr pivot_longer
#'
#' @keywords internal
extract_spatial_bboxes <- function(metadata) {
  # ---- required namespaces ----
  requireNamespace("tibble")
  requireNamespace("purrr")
  requireNamespace("dplyr")
  requireNamespace("tidyr")

  # ---- helper to compute bbox from coordinate list ----
  parse_coords_to_bbox <- function(coords) {
    if (is.null(coords)) return(NULL)
    if (is.list(coords)) coords <- unlist(coords, use.names = FALSE)
    coords <- suppressWarnings(as.numeric(coords))
    if (length(coords) < 4 || any(!is.finite(coords))) return(NULL)
    half <- length(coords) / 2
    if (half %% 1 != 0) return(NULL)
    xs <- coords[seq_len(half)]
    ys <- coords[(half + 1):length(coords)]
    list(
      xmin = min(xs, na.rm = TRUE),
      xmax = max(xs, na.rm = TRUE),
      ymin = min(ys, na.rm = TRUE),
      ymax = max(ys, na.rm = TRUE)
    )
  }

  # helper: safely turn a bbox cell into xmin/ymin/xmax/ymax
  parse_old_bbox <- function(cell) {
    # already a data.frame with named cols
    if (is.data.frame(cell) && all(c("xmin","ymin","xmax","ymax") %in% names(cell))) {
      return(list(
        xmin = as.numeric(cell$xmin[1]),
        ymin = as.numeric(cell$ymin[1]),
        xmax = as.numeric(cell$xmax[1]),
        ymax = as.numeric(cell$ymax[1])
      ))
    }
    # named list with xmin/ymin/xmax/ymax
    if (is.list(cell) && all(c("xmin","ymin","xmax","ymax") %in% names(cell))) {
      return(list(
        xmin = as.numeric(cell[["xmin"]][1]),
        ymin = as.numeric(cell[["ymin"]][1]),
        xmax = as.numeric(cell[["xmax"]][1]),
        ymax = as.numeric(cell[["ymax"]][1])
      ))
    }
    # character like "152.08, -27.70, 152.09, -27.69"
    if (is.character(cell) && length(cell) == 1L) {
      nums <- suppressWarnings(as.numeric(strsplit(cell, "\\s*,\\s*")[[1]]))
      if (length(nums) == 4 && all(is.finite(nums))) {
        return(list(xmin = nums[1], ymin = nums[2], xmax = nums[3], ymax = nums[4]))
      }
    }
    # numeric vector c(xmin, ymin, xmax, ymax)
    if (is.numeric(cell) && length(cell) == 4L) {
      return(list(xmin = cell[1], ymin = cell[2], xmax = cell[3], ymax = cell[4]))
    }
    # anything else -> NULL (to be filtered out)
    NULL
  }


  # ---- 1. OLD FORMAT: metadata$spatial$bbox ----
  out_old <- tibble::tibble()
  # check if the bbox is saved as its own dataframe
  if (is.data.frame(metadata$spatial$bbox)) {
    # keep the row id BEFORE pivoting so we can map back to metadata$id
    out_old <- metadata$spatial$bbox |>
      dplyr::mutate(row_id = dplyr::row_number()) |>
      tidyr::pivot_longer(
        cols = -row_id,
        names_to = "locationName",
        values_to = "bbox"
      )
    # drop NULL / empty cells
    out_old = dplyr::filter(out_old, purrr::map_lgl(bbox, ~ !is.null(.x)))
    # parse each cell into xmin/ymin/xmax/ymax
    out_old = dplyr::mutate(out_old, parsed = purrr::map(bbox, parse_old_bbox))
    # keep only successfully parsed cells
    out_old = dplyr::filter(out_old, purrr::map_lgl(parsed, ~ !is.null(.x)))
    # expand parsed list-cols into numeric columns
    out_old = dplyr::mutate(out_old,
                            xmin = purrr::map_dbl(parsed, "xmin"),
                            ymin = purrr::map_dbl(parsed, "ymin"),
                            xmax = purrr::map_dbl(parsed, "xmax"),
                            ymax = purrr::map_dbl(parsed, "ymax")
    )
    # attach the correct project id using the preserved row_id
    out_old <- dplyr::mutate(
      out_old,
      id = if ("id" %in% names(metadata)) {
        # Match each bbox row to its project ID via row_id
        purrr::map_chr(row_id, ~ {
          if (!is.na(.x) && .x <= length(metadata$id)) metadata$id[[.x]] else NA_character_
        })
      } else {
        # but default to NA if not availible
        NA_character_
      }
    )
    # Now thin to the relevant columns
    out_old = dplyr::select(out_old, locationName, xmin, ymin, xmax, ymax, id)
  }

  # ---- 2. NEW FORMAT: metadata$spatial$features ----
  out_new <- tibble::tibble()
  if (is.list(metadata$spatial$features)) {
    out_new <- purrr::imap_dfr(metadata$spatial$features, function(feature_set, idx) {

      if (is.null(feature_set) || !is.data.frame(feature_set)) return(tibble::tibble())
      if (!"geometry" %in% names(feature_set)) return(tibble::tibble())

      rows <- vector("list", nrow(feature_set))

      for (i in seq_len(nrow(feature_set))) {
        coords <- NULL
        if (is.data.frame(feature_set$geometry) &&
            "coordinates" %in% names(feature_set$geometry)) {
          coords <- feature_set$geometry$coordinates[[i]]
        }
        bbox <- parse_coords_to_bbox(coords)
        if (is.null(bbox)) next

        loc_name <- NA_character_
        if ("properties" %in% names(feature_set) &&
            "locationName" %in% names(feature_set$properties)) {
          loc_name <- feature_set$properties$locationName[i]
          # catch old legacy name
        } else if ("properties" %in% names(feature_set) &&
                   "name" %in% names(feature_set$properties)) {
          loc_name <- feature_set$properties$name[i]
        }

        rows[[i]] <- tibble::tibble(
          locationName = loc_name,
          xmin = bbox$xmin,
          ymin = bbox$ymin,
          xmax = bbox$xmax,
          ymax = bbox$ymax,
          id   = metadata$id[[idx]]
        )
      }

      rows <- rows[!vapply(rows, is.null, logical(1))]
      if (length(rows) == 0) return(tibble::tibble())
      do.call(dplyr::bind_rows, rows)
    })
  }

  # ---- 3. Combine both results ----
  out_combined <- dplyr::bind_rows(out_old, out_new)
  out_combined <- dplyr::distinct(out_combined)
  out_combined
}


# Helper: Convert dp$spatial (GeoJSON-shaped list) back to sf for mapping
geojson_list_to_sf <- function(geojson_list) {
  geojson_string <- jsonlite::toJSON(geojson_list, auto_unbox = TRUE)
  geojsonsf::geojson_sf(geojson_string)
}
