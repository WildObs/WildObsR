#' Query WildObs MongoDB for Relevant Project IDs
#'
#' This function queries the WildObs MongoDB database for projects matching specified spatial, temporal, and data-sharing criteria.
#' It extracts metadata from the database and filters projects based on bounding box overlaps, temporal overlaps, and sharing preferences..
#' This function is planned to be continually developed to allow more information to query eventually.
#'
#' The character vector of project IDs that is returned from this function is then used in the function @seealso \code{\link{wildobs_dp_download}} for extracting data packages from WildObs' MongoDB.
#'
#' @param spatial A named list specifying spatial query parameters, including:
#'   \describe{
#'     \item{xmin}{Minimum longitude value.}
#'     \item{xmax}{Maximum longitude value.}
#'     \item{ymin}{Minimum latitude value.}
#'     \item{ymax}{Maximum latitude value.}
#'   }
#' @param temporal A named list specifying temporal query parameters, including:
#'   \describe{
#'     \item{minDate}{Earliest allowable date as a `Date` object.}
#'     \item{maxDate}{Latest allowable date as a `Date` object.}
#'   }
#' @param tabularSharingPreference A character vector specifying accepted sharing preferences.
#'   Defaults to `c("open", "partial")`. Only projects with these preferences are returned.
#' @return A character vector of project IDs matching the specified criteria.
#' @examples
#' \dontrun{
#' # Define spatial query: extract projects in a specific bounding box
#' spatial_query <- list(xmin = 145.0, xmax = 147.0, ymin = -16.0, ymax = -20.0)
#'
#' # Define temporal query: select projects active in 2022-2025
#' temporal_query <- list(minDate = as.Date("2022-01-01"), maxDate = as.Date("2025-01-01"))
#'
#' # Query the WildObs database for matching projects
#' relevant_projects <- wildobs_mongo_query(spatial = spatial_query, temporal = temporal_query)
#' print(relevant_projects)
#' }
#' @seealso
#' - [mongolite::mongo()] for database queries
#' @importFrom magrittr %>%
#' @export
wildobs_mongo_query = function(spatial = NULL, temporal = NULL,
                               #taxonomic, samplingDesign, contributors, # TODO:  Fill in later!
                               tabularSharingPreference = c("open", "partial")){
  # create an empty vector to store project IDs
  proj_ids = c()

  # load rel library
  # library(magrittr) # for the %>%

  ## first, establish access for MongoDB
  # will only ever be read only!
  USER = "woro"
  PASS = "woroPa55w0rd"
  # HOST = "203.101.228.237"
  HOST = "localhost" # for testing
  PORT = "29017"
  DATABASE = "wildobs_camdb"

  ## combine all the information into a database-url to enable access
  db_url <- sprintf("mongodb://%s:%s@%s:%s/%s", USER, PASS, HOST, PORT, DATABASE)
  # rm(USER, PASS, HOST, PORT, DATABASE)

  ## access the metadata from the DB
  metadata = mongolite::mongo(db = "wildobs_camdb", collection = "metadata", url = db_url)$find()

  ## and immediately thin metadata to include the specific sharing preferences
  metadata = metadata[metadata$WildObsMetadata$tabularSharingPreference %in% tabularSharingPreference, ]


  #
  ##
  ### Spatial query ----

  # for testing
  # spatial = list(xmin = 145.0, xmax = 147.0, ymin = -16.0,  ymax = -20.0)

  ## Make sure there is spatial information!
  if(exists("spatial", inherits = FALSE) && !is.null(spatial) && length(spatial) > 0){
    # Extract the bounding box data frame
    bbox_df <- metadata$spatial$bbox %>%
      tidyr::pivot_longer(cols = everything(), names_to = "locationName", values_to = "bbox") %>%
      dplyr::mutate(row_id = rep(seq_len(nrow(metadata)), each = ncol(metadata$spatial$bbox))) %>%  # Track original row ID
      dplyr::filter(purrr::map_lgl(bbox, ~ !is.null(.x) && !all(is.na(.x)))) %>%  # Remove NULL or all-NA values
      dplyr::mutate(
        xmin = purrr::map_dbl(bbox, ~ .x$xmin[1]),
        ymin = purrr::map_dbl(bbox, ~ .x$ymin[1]),
        xmax = purrr::map_dbl(bbox, ~ .x$xmax[1]),
        ymax = purrr::map_dbl(bbox, ~ .x$ymax[1])
      ) %>%
      dplyr::select(locationName, row_id, xmin, ymin, xmax, ymax)  # Keep only relevant columns

    # grab relevant project IDs
    bbox_df <- bbox_df %>% dplyr::mutate(id = metadata$id[row_id])

    # Filter bbox_df to only rows that **overlap** the spatial extent
    bbox_df_filtered = bbox_df[bbox_df$xmin >= spatial$xmin &      # At least some part of bbox must be within xmin
                                 bbox_df$xmax <= spatial$xmax &    # At least some part of bbox must be within xmax
                                 bbox_df$ymax >= spatial$ymax &    # At least some part of bbox must be within ymin
                                 bbox_df$ymin <= spatial$ymin, ]   # At least some part of bbox must be within ymax

    # save the relevant project ids
    proj_ids = unique(c(proj_ids, bbox_df_filtered$id))

    ## clean up for testing
    # rm(bbox_df, bbox_df_filtered, spatial)
  } # end spatial condition

  #
  ##
  ### Temporal query ----

  # for testing
  # temporal = list(minDate = as.Date("2022-01-01"), maxDate = as.Date("2025-01-01"))

  if(!missing(temporal) && !is.null(temporal) && length(temporal) > 0){
    # extract the data frame from the meta
    temporal_df = metadata$temporal
    temporal_df$timeZone = NULL # dont want this rn

    # Track row indices to associate with project ID
    temporal_df <- temporal_df %>%
      dplyr::mutate(row_id = seq_len(nrow(.))) %>%  # Create row IDs before pivoting
      tidyr::pivot_longer(cols = -row_id, names_to = "name", values_to = "dates") %>%
      tidyr::unnest_wider(dates) %>%
      dplyr::filter(!is.na(start) & !is.na(end)) %>% # Keep only non-NA values
      dplyr::mutate(start = as.Date(start), end = as.Date(end)) %>% # make sure dates are dates.
      dplyr::mutate(id = metadata$id[row_id]) %>%
      dplyr::select(id, name, start, end)

    # Filter projects that fall within the temporal range
    temporal_df_filtered <- temporal_df %>%
      dplyr::filter(
        (start >= temporal$minDate & start <= temporal$maxDate) |    # Start is within the range
          (end >= temporal$minDate & end <= temporal$maxDate) |      # End is within the range
          (start <= temporal$minDate & end >= temporal$maxDate)      # Project fully overlaps the range
      )

    # save the relevant project ids
    proj_ids = unique(c(proj_ids, temporal_df_filtered$id))
  } # end temporal condition

  #
  ##
  ### Taxonomic

  #
  ##
  ### Contributors

  #
  ##
  ### samplingDesign

  #
  ##
  ### Return the updated vector of IDs.
  # but if there are no conditions met, provide all open and partial options
  if(length(proj_ids) == 0){
    # print a message
    print(paste("There were no matches in our database of the specific parameters provided in your function. This will return all project IDs that match the tabular data sharing preference of", paste0(tabularSharingPreference, collapse = " & ")))
    # grab em all
    proj_ids = metadata$id
  }

  # return the vector
  proj_ids

} # end function
