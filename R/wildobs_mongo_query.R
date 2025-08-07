#' Query WildObs MongoDB for Relevant Project IDs
#'
#' This function queries the WildObs MongoDB database for projects matching specified spatial, temporal, and data-sharing criteria.
#' It extracts metadata from the database and filters projects based on bounding box overlaps, temporal overlaps, and sharing preferences..
#' This function is planned to be continually developed to allow more information to query eventually.
#'
#' The character vector of project IDs that is returned from this function is then used in the function @seealso \code{\link{wildobs_dp_download}} for extracting data packages from WildObs' MongoDB.
#'
#' @param db_url A character string specifying the MongoDB connection URI. This should follow the format:
#'   `'mongodb://username:password@host:port/database'`. If `NULL`, the function will stop with an error.
#'   This parameter allows users to specify their own connection string to the WildObs MongoDB instance.
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
#'   Defaults to `c("open")`, but the user can also specify 'partial' for limited metadata of the project. Only projects with these preferences are returned.
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
wildobs_mongo_query = function(db_url = NULL, spatial = NULL, temporal = NULL,
                               #taxonomic, samplingDesign, contributors, # TODO:  Fill in later!
                               tabularSharingPreference = c("open")){
  # create an empty vector to store project IDs
  proj_ids = c()

  ## read in environment file with confidential DB access info
  # readRenviron("inst/config/.Renviron.local.ro") # local version
  # readRenviron("inst/config/.Renviron.prod.ro") # remote version

  # ## load information from enviromnet
  # HOST <- Sys.getenv("HOST")
  # PORT <- Sys.getenv("PORT")
  # DATABASE <- Sys.getenv("DATABASE")
  # USER <- Sys.getenv("USER")
  # PASS <- Sys.getenv("PASS")
  #
  # ## combine all the information into a database-url to enable access
  # db_url <- sprintf("mongodb://%s:%s@%s:%s/%s", USER, PASS, HOST, PORT, DATABASE)
  # rm(USER, PASS, HOST, PORT, DATABASE)

  #### UPDATE: until we can create a public facing log-in information to access the DB,
  ### I will leave db_url as an option that users have to input to the function
  ## so they can access the database. Ideally we will have a better solution in the future!
  if(is.null(db_url)){
    stop("You have not provided a URL to access MongoDB.\nPlease provide an appropriate URL if you want to access the database.")
  }
  ## Make sure the db URL they provide matches the basic pattern
  pattern <- "^mongodb:\\/\\/[^:@]+:[^:@]+@[^\\/]+:\\d+\\/[a-zA-Z0-9._-]+$"
  if (!grepl(pattern, db_url)) {
    stop("The URL to access the database must be a valid MongoDB URI of the follwoing format: \n'mongodb://user:password@host:port/dbname'")
  }

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

    # Filter bbox_df to remove any non-overlapping boxes from the spatial extent
    bbox_df_filtered <- bbox_df[
      !(bbox_df$xmax < spatial$xmin |   # bbox entirely west of xmin
          bbox_df$xmin > spatial$xmax |   # bbox entirely east of xmax
          bbox_df$ymax < spatial$ymin |   # bbox entirely south of ymin
          bbox_df$ymin > spatial$ymax),   # bbox entirely north of ymax
    ]

    # save the relevant project ids
    proj_ids_spatial <- bbox_df_filtered$id

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
    proj_ids_temporal <- temporal_df_filtered$id
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

  ## make a list of all relevant project IDs
  id_lists <- list(proj_ids_spatial, proj_ids_temporal)  # come here and add more as they arise!
  ## Take the intersection of all queries
  proj_ids <- Reduce(intersect, id_lists)

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
