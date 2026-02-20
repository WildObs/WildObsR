#' Extract Metadata Elements from Data Packages
#'
#' Extracts specified metadata elements from one or more Camera Trap Data Packages
#' (camtrap DP) and returns them as tidy data frames. Handles flexible list
#' structures and automatically replaces NULL values with NA to ensure
#' consistent data types.
#'
#' @param dp_list A single data package (list) or a list of data packages to
#' extract metadata from. Each data package should be a named list with an \code{id} field.
#' @param elements A character vector specifying which metadata elements to extract.
#'   Supported elements are: \code{"contributors"}, \code{"sources"},
#'   \code{"licenses"}, \code{"relatedIdentifiers"}, \code{"references"},
#'   \code{"project"}, \code{"WildObsMetadata"}, \code{"spatial"}, \code{"temporal"},
#'   \code{"taxonomic"}.
#'
#' @return
#' If a single element is requested: A data frame with one row per record
#' (or one row for flat objects) and columns for each field in that element.
#' Includes a \code{DPID} column with the source data package ID.
#'
#' If multiple elements are requested: A named list of data frames, one for
#' each requested element. Each data frame includes a \code{DPID} column to
#' distinguish different data packages
#'
#' When multiple data packages are provided, results are accumulated across all
#' DPs using \code{dplyr::bind_rows()}, so each data frame in the result
#' contains records from all input data packages.
#'
#' @details
#' This function is designed for extracting and standardizing metadata from camtrap DP
#' packages. It handles:
#' \itemize{
#'   \item Single or multiple data packages
#'   \item Flexible list structures (lists of objects vs. flat objects)
#'   \item NULL values (replaced with NA for consistency)
#'   \item Multiple data packages (rows combined via \code{dplyr::bind_rows()})
#' }
#'
#' @section Spatial data:
#' The \code{"spatial"} element returns a dataframe of bounding box coordinates
#' (locationName, xmin, ymin, xmax, ymax) for convenient inspection. Users who
#' need the full GeoJSON geometry for spatial analysis or mapping can access it
#' directly via \code{dp$spatial} and convert to an sf object using
#' \code{geojson_list_to_sf(dp$spatial)}.
#'
#' @examples
#' \dontrun{
#' # Load a single data package
#' dp <- frictionless::read_package("path/to/datapackage.json")
#'
#' # Extract a single element
#' contributors_df <- extract_metadata(dp, "contributors")
#'
#' # Extract multiple elements
#' metadata_list <- extract_metadata(dp, c("contributors", "sources", "project"))
#'
#' # Load multiple data packages
#' dp_list <- lapply(
#'   list.files("path/to/dps", pattern = "datapackage.json", recursive = TRUE),
#'   frictionless::read_package
#' )
#'
#' # Extract across multiple packages
#' all_contributors <- extract_metadata(dp_list, "contributors")
#' }
#'
#' @author Zachary Amir
#'
#' @importFrom purrr pluck map
#' @importFrom dplyr bind_rows
#'
#' @export
extract_metadata <- function(dp_list, elements = c("contributors", "sources", "licenses", "relatedIdentifiers", "references", "project", "WildObsMetadata", "spatial", "temporal", "taxonomic")){
  # ## testing!!!
  # elements = c("sources","contributors", "WildObsMetadata", "spatial")

  ## current supported elements
  supp_el = c("contributors", "sources", "licenses", "relatedIdentifiers", "references", "project", "WildObsMetadata", "spatial", "temporal", "taxonomic")

  #
  ##
  ### Input validation

  ## first check if elements contains enumerated values
  if(!any(elements %in% supp_el)){
    stop(paste("You have not provided metadata elements that match this functions supported elements.\n The elements you can include are:\n", paste(supp_el, collapse = ", ")))
  }

  # Ensure dp_list is a list of data packages (handle single DP)
  if (!is.null(dp_list$id) || is.null(dp_list[[1]]$id)) {
    # Single data package provided, but save the list in one big list!
    dp_list <- list(dp_list)
  }

  #
  ##
  ### Extract values

  # Initialize list to accumulate results across all DPs
  accumulated_results <- setNames(
    lapply(elements, function(x) NULL), # a list w/ NULL positions for all elements
    elements
  )

  # load a helper function to normalize values
  normalize_values <- function(x) {
    # if null, return NA
    if (is.null(x)) return(NA)
    # collapse vectors if longer than 1
    if (length(x) > 1) x <- paste(x, collapse = ";")
    # convert logical to character for uniformity.
    if (is.logical(x)) x <- tolower(as.character(x))
    x
  }

  # loop thru each dp
  for(d in 1:length(dp_list)){
    # select one dp
    dp = dp_list[[d]]

    # loop thru all elements
    for(i in 1:length(elements)){
      ## first, check if the element is present in the dp
      if(!elements[i] %in% names(dp)){
        # if missing, let us know!
        warning(paste("The element", elements[i], "is missing from data package",
                    dp$id, "so this element is getting skipped."))
        # and skip to to the next
        next
      }
      # extract relevant metadata list matching supplied element
      el_list = purrr::pluck(dp, elements[i])

      # Skip if element doesn't exist
      if (is.null(el_list) || length(el_list) == 0) {
        next
      }

      ## Make a special exception for spatial information formatted as a geoJSON
      if (elements[i] == "spatial") {
        # if were extracting spatial, first check if the data is formatted as a geoJSON
        if (!is.null(el_list$type) && tolower(el_list$type) == "featurecollection") {
          # extract the coordinates of the bounding boxes and save as a dataframe
          res <- purrr::map_dfr(el_list$features, function(f) {
            ## grab coordinates and then infer structure
            coords <- f$geometry$coordinates

            # Structure depends on how the dp was read from JSON:
            # - 3D array [1, n_points, 2]: fromJSON() with simplifyVector = TRUE (default)
            # - Nested list: fromJSON() with simplifyVector = FALSE
            if (is.array(coords)) {
              # coords[1, , 1] = all longitudes, coords[1, , 2] = all latitudes
              lon <- coords[1, , 1]
              lat <- coords[1, , 2]
            } else if (is.list(coords)) {
              # coords[[1]] = outer ring, each element = [lon, lat] pair
              ring <- coords[[1]]
              lon  <- sapply(ring, `[[`, 1)
              lat  <- sapply(ring, `[[`, 2)
            } else {
              stop(paste("Unexpected coordinate structure for feature:",
                         f$properties$name %||% "unknown"))
            }
            ## save into a dataframe
            data.frame(
              locationName = f$properties$name %||% NA_character_,
              xmin = min(lon, na.rm = TRUE),
              ymin = min(lat, na.rm = TRUE),
              xmax = max(lon, na.rm = TRUE),
              ymax = max(lat, na.rm = TRUE)
            )
          }) # end map_dfr
          # Anything reaching here is either NULL spatial, or a legacy format
          # that should have been corrected by the QAQC pipeline before reaching users
        } else {
          # warn the users and return an empty DF
          warning(paste0(
            "Spatial data for ", dp$id, " is not a GeoJSON FeatureCollection. ",
            "Expected $type = 'FeatureCollection' but got: '", el_list$type %||% "NULL", "'. ",
            "Re-running the QAQC pipeline should resolve this."
          ))
          res <- data.frame(
            locationName = NA_character_,
            xmin = NA_real_, ymin = NA_real_,
            xmax = NA_real_, ymax = NA_real_,
            stringsAsFactors = FALSE
          )
        } # end else
        # and dont forget the id
        res$DPID <- dp$id
      } # end spatial conditon
      ## Add a special condition for temporal to accommodate timezones
      else if (elements[i] == "temporal") {

        # Extract timeZone if present
        tz <- el_list$timeZone %||% NA
        el_list$timeZone <- NULL  # remove before flattening

        # Each remaining element should represent a deployment group
        res <- purrr::map_dfr(names(el_list), function(nm) {
          val <- el_list[[nm]]
          data.frame(
            deploymentGroup = nm,
            start = val$start %||% NA,
            end = val$end %||% NA
          )
        })

        # Add timezone and DP ID
        res$timeZone <- tz
        res$DPID <- dp$id
      }
      ## but if the element is NOT temporal or spatial,
      else{
        # handle all other elements normally!

        # if nothing was found, make res null here
        if (length(el_list) == 0){ res = NULL} # end zero condition

        # Check if this is a list of objects (each element is itself a list)
        # or a single flat object (list of scalars)
        if (is.list(el_list[[1]]) && !is.data.frame(el_list[[1]])) {
          # STEP 1 — Clean each element in the list safely
          el_list_clean <- purrr::map(el_list, ~{
            # Replace any NULLs and emtpy lists with NA to keep consistent structure
            .x <- lapply(.x, function(y) {
              if (is.null(y) || (is.list(y) && length(y) == 0) || length(y) == 0) {
                return(NA)
              } else {
                return(y)
              }
            })

            # Normalize values (your custom function)
            .x <- lapply(.x, normalize_values)

            # If this element becomes empty (e.g., {}), create a 1-row NA placeholder
            if (length(.x) == 0) {
              return(list(dummy_placeholder = NA))
            }

            # Return cleaned element
            .x
          }) # end el_list_clean

          # STEP 2 — Handle variable-length field names safely
          # Collect all possible field names across all list elements
          all_fields <- unique(unlist(lapply(el_list_clean, names)))

          # STEP 3 — Coerce each list element to a 1-row data frame
          # Fill missing fields with NA so all rows have equal length
          el_list_aligned <- purrr::map(el_list_clean, function(x) {

            # Ensure all expected fields exist
            missing <- setdiff(all_fields, names(x))
            if (length(missing) > 0) x[missing] <- NA

            # Convert to data.frame safely
            tryCatch(
              as.data.frame(as.list(x), stringsAsFactors = FALSE),
              error = function(e) {
                # Fallback NA-filled row if conversion fails
                data.frame(as.list(setNames(rep(NA, length(all_fields)), all_fields)),
                           stringsAsFactors = FALSE)
              }
            )
          })

          # STEP 4 — Combine all rows into one data frame
          # purrr::list_rbind is faster and safer than map_df()
          res <- purrr::list_rbind(el_list_aligned)
        } else {
          ## before proceeding, just make totally sure each element is flat (i.e., not a nested list)
          el_list <- lapply(el_list, function(x) {
            # If element is a list but not a data.frame, try to flatten
            if (is.list(x) && !is.data.frame(x)) {
              # Repeatedly unlist one level until it's atomic or NULL
              while (is.list(x) && length(x) == 1 && !is.data.frame(x[[1]])) {
                x <- x[[1]]
              }
            }
            # Replace completely empty or NULL with NA
            if (is.null(x) || length(x) == 0) x <- NA
            return(x)
          })

          # normalize values in the flat list directly
          el_list <- lapply(el_list, normalize_values)
          # Single flat object or mixed: convert to single-row data frame
          res = as.data.frame(as.list(el_list))
        } # end list structure condition

        ## save the ID of the DP in the data frame
        res$DPID = dp$id
      } # end else conditon for spatial

      # Save the results by searching for null elements
      if (is.null(accumulated_results[[elements[i]]])) {
        # if null, add the new results
        accumulated_results[[elements[i]]] <- res
      } else {
        # But if present, bind rows with multiple DPs
        accumulated_results[[elements[i]]] <- dplyr::bind_rows(
          accumulated_results[[elements[i]]],
          res
        )
      } # end else saving conditional
    } # end per i element
  } # end per DP

  # Remove NULL entries (elements that weren't found in any DP)
  accumulated_results <- Filter(Negate(is.null), accumulated_results)

  # Return based on number of elements
  if (length(accumulated_results) == 1) {
    # Single element: return as data frame
    return(accumulated_results[[1]])
  } else {
    # Multiple elements: return as named list
    return(accumulated_results)
  } # end else return condition

} # end function
