#' Extract Metadata Elements from Data Packages
#'
#' Extracts specified metadata elements from one or more Camera Trap Data Packages (camtrap DP) and returns them as tidy data frames. Handles flexible list structures and automatically replaces NULL values with NA to ensure consistent data types.
#'
#' @param dp_list A single data package (list) or a list of data packages to extract metadata from. Each data package should be a named list with an \code{id} field.
#' @param elements A character vector specifying which metadata elements to extract.
#'   Supported elements are: \code{"contributors"}, \code{"sources"},
#'   \code{"licenses"}, \code{"relatedIdentifiers"}, \code{"references"},
#'   \code{"project"}, \code{"WildObsMetadata"}, \code{"spatial"}, \code{"temporal"},
#'   \code{"taxonomic"}.
#'
#' @return
#' If a single element is requested: A data frame with one row per record (or one row for flat objects) and columns for each field in that element. Includes a \code{DPID} column with the source data package ID.
#'
#' If multiple elements are requested: A named list of data frames, one for each requested element. Each data frame includes a \code{DPID} column to distinguish different data packages
#'
#' When multiple data packages are provided, results are accumulated across all DPs using \code{dplyr::bind_rows()}, so each data frame in the result contains records from all input data packages.
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
#' Before using this function, it is recommended to validate your data packages using
#' a QAQC function to ensure consistency in data types and structure across packages.
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
  # elements = c("sources","contributors", "WildObsMetadata")

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

  # loop thru each dp
  for(d in 1:length(dp_list)){
    # select one dp
    dp = dp_list[[d]]

    # loop thru all elements
    for(i in 1:length(elements)){
      # extract relevant metadata list matching supplied element
      el_list = purrr::pluck(dp, elements[i])

      # Skip if element doesn't exist
      if (is.null(el_list) || length(el_list) == 0) {
        next
      }

      ## Make a special exception for spatial information formatted as a geoJSON
      if (elements[i] == "spatial") {
        # if were extracting spatial, first check if the data is formatted as a geoJSON
        if (!is.null(el_list$type) && el_list$type == "FeatureCollection") {
          # extract the coordinates of the bounding boxes and save as a dataframe
          res <- purrr::map_dfr(el_list$features, function(f) {
            coords <- f$geometry$coordinates[[1]]
            data.frame(
              name = f$properties$name,
              xmin = coords[[1]][1],
              ymin = coords[[1]][2],
              xmax = coords[[3]][1],
              ymax = coords[[3]][2]
            )
          })
        } else if (!is.null(el_list$type) && el_list$type == "Polygon") {
          # this handles extraacting coordinates from only one polygon rather than a collection of many
          coords <- el_list$coordinates[[1]]
          res <- data.frame(
            name = NA,
            xmin = coords[[1]][1],
            ymin = coords[[1]][2],
            xmax = coords[[3]][1],
            ymax = coords[[3]][2]
          )
        } else {
          warning("Spatial format not recognized; skipping.")
          next
        }
        # save the type, either polygon (one locationName) or featureCollection (many locaitons)
        res$type <- el_list$type
        # and dont forget the id
        res$DPID <- dp$id
      } # end spatial


      # if nothing was found, make res null here
      if (length(el_list) == 0){ res = NULL} # end zero condition

      # Check if this is a list of objects (each element is itself a list)
      # or a single flat object (list of scalars)
      if (is.list(el_list[[1]]) && !is.data.frame(el_list[[1]])) {
        # first safely replace NULL values w/ NA to prevent dimension mis-match
        el_list_clean <- purrr::map(el_list, ~{
          .x[sapply(.x, is.null)] <- NA
          .x
        })
        # then normally convert the list of objects
        res <- purrr::map_df(el_list_clean, ~as.data.frame(as.list(.x)))

      } else {
        # Single flat object or mixed: convert to single-row data frame
        res = as.data.frame(as.list(el_list))
      } # end list structure condition

      ## save the ID of the DP in the data frame
      res$DPID = dp$id

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
